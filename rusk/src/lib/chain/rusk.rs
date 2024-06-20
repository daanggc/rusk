// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) DUSK NETWORK. All rights reserved.

use std::path::Path;
use std::sync::{mpsc, Arc, LazyLock};
use std::time::{Duration, Instant};
use std::{fs, io};

use parking_lot::RwLock;
use sha3::{Digest, Sha3_256};
use tokio::task;
use tracing::{debug, info, warn};

use dusk_bytes::DeserializableSlice;
use dusk_consensus::operations::{CallParams, VerificationOutput};
use execution_core::{
    stake::StakeData, BlsScalar, StakePublicKey,
    Transaction as PhoenixTransaction,
};
use node::database::DBViewer;
use node_data::ledger::{SpentTransaction, Transaction};
use rusk_abi::dusk::Dusk;
use rusk_abi::{
    CallReceipt, ContractError, ContractId, Error as PiecrustError, Event,
    Session, STAKE_CONTRACT, TRANSFER_CONTRACT, VM,
};
use rusk_profile::to_rusk_state_id_path;
use tokio::sync::broadcast;

use super::{coinbase_value, emission_amount, Rusk, RuskTip};
use crate::free_tx_verifier::FreeTxVerifier;
use crate::http::RuesEvent;
use crate::{Error, Result};

pub static DUSK_KEY: LazyLock<StakePublicKey> = LazyLock::new(|| {
    let dusk_cpk_bytes = include_bytes!("../../assets/dusk.cpk");
    StakePublicKey::from_slice(dusk_cpk_bytes)
        .expect("Dusk consensus public key to be valid")
});

impl Rusk {
    pub fn new<P: AsRef<Path>>(
        dir: P,
        generation_timeout: Option<Duration>,
        feeder_gas_limit: u64,
        event_sender: broadcast::Sender<RuesEvent>,
    ) -> Result<Self> {
        let dir = dir.as_ref();
        let commit_id_path = to_rusk_state_id_path(dir);

        let base_commit_bytes = fs::read(commit_id_path)?;
        if base_commit_bytes.len() != 32 {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                format!(
                    "Expected commit id to have 32 bytes, got {}",
                    base_commit_bytes.len()
                ),
            )
            .into());
        }
        let mut base_commit = [0u8; 32];
        base_commit.copy_from_slice(&base_commit_bytes);

        let vm = Arc::new(rusk_abi::new_vm(dir)?);

        let tip = Arc::new(RwLock::new(RuskTip {
            current: base_commit,
            base: base_commit,
        }));

        Ok(Self {
            tip,
            vm,
            dir: dir.into(),
            generation_timeout,
            feeder_gas_limit,
            event_sender,
            db_viewer: None,
        })
    }

    pub fn set_db_viewer(&mut self, db_viewer: impl DBViewer) {
        self.db_viewer = Some(Arc::new(db_viewer))
    }

    pub fn db_viewer(&self) -> &Arc<dyn DBViewer> {
        self.db_viewer.as_ref().expect("DBViewer must be set")
    }

    pub fn execute_transactions<I: Iterator<Item = Transaction>>(
        &self,
        params: &CallParams,
        txs: I,
    ) -> Result<(Vec<SpentTransaction>, Vec<Transaction>, VerificationOutput)>
    {
        let started = Instant::now();

        let block_height = params.round;
        let block_gas_limit = params.block_gas_limit;
        let generator = params.generator_pubkey.inner();
        let missed_generators = &params.missed_generators[..];

        let mut session = self.session(block_height, None)?;

        let mut block_gas_left = block_gas_limit;

        let mut spent_txs = Vec::<SpentTransaction>::new();
        let mut discarded_txs = vec![];

        let mut dusk_spent = 0;

        let mut event_hasher = Sha3_256::new();

        let free_tx_verifier = FreeTxVerifier::new(self.db_viewer().as_ref());

        for unspent_tx in txs {
            if let Some(timeout) = self.generation_timeout {
                if started.elapsed() > timeout {
                    info!("execute_transactions timeout triggered {timeout:?}");
                    break;
                }
            }
            let tx_id = hex::encode(unspent_tx.id());
            if unspent_tx.inner.fee().gas_limit > block_gas_left {
                info!("Skipping {tx_id} due gas_limit greater than left: {block_gas_left}");
                continue;
            }

            // check free transaction
            if unspent_tx.inner.nullifiers.len() == 0 {
                if let Some(e) = free_tx_verifier.verify(&unspent_tx).err() {
                    info!(
                        "Skipping {tx_id} due to unverified proof of work: {e}"
                    );
                    continue;
                }
            }

            match execute(&mut session, &unspent_tx.inner) {
                Ok(receipt) => {
                    let gas_spent = receipt.gas_spent;

                    // If the transaction went over the block gas limit we
                    // re-execute all spent transactions. We don't discard the
                    // transaction, since it is technically valid.
                    if gas_spent > block_gas_left {
                        warn!("This is not supposed to happen with conservative tx inclusion");
                        session = self.session(block_height, None)?;

                        for spent_tx in &spent_txs {
                            // We know these transactions were correctly
                            // executed before, so we don't bother checking.
                            let _ =
                                execute(&mut session, &spent_tx.inner.inner);
                        }

                        continue;
                    }

                    // We're currently ignoring the result of successful calls
                    let err = receipt.data.err().map(|e| format!("{e}"));
                    info!("Tx {tx_id} executed with {gas_spent} gas and err {err:?}");

                    update_hasher(&mut event_hasher, &receipt.events);

                    block_gas_left -= gas_spent;
                    let gas_price = unspent_tx.inner.fee.gas_price;
                    dusk_spent += gas_spent * gas_price;
                    spent_txs.push(SpentTransaction {
                        inner: unspent_tx,
                        gas_spent,
                        economic_mode: receipt.economic_mode,
                        block_height,
                        err,
                    });
                }
                Err(e) => {
                    info!("discard tx {tx_id} due to {e:?}");
                    // An unspendable transaction should be discarded
                    discarded_txs.push(unspent_tx);
                    continue;
                }
            }
        }

        let coinbase_events = reward_slash_and_update_root(
            &mut session,
            block_height,
            dusk_spent,
            generator,
            missed_generators,
        )?;
        update_hasher(&mut event_hasher, &coinbase_events);

        let state_root = session.root();
        let event_hash = event_hasher.finalize().into();

        Ok((
            spent_txs,
            discarded_txs,
            VerificationOutput {
                state_root,
                event_hash,
            },
        ))
    }

    /// Verify the given transactions are ok.
    pub fn verify_transactions(
        &self,
        block_height: u64,
        block_gas_limit: u64,
        generator: &StakePublicKey,
        txs: &[Transaction],
        missed_generators: &[StakePublicKey],
    ) -> Result<(Vec<SpentTransaction>, VerificationOutput)> {
        let session = self.session(block_height, None)?;

        accept(
            session,
            block_height,
            block_gas_limit,
            generator,
            txs,
            missed_generators,
        )
        .map(|(a, b, _, _)| (a, b))
    }

    /// Accept the given transactions.
    ///
    ///   * `consistency_check` - represents a state_root, the caller expects to
    ///   be returned on successful transactions execution. Passing a None
    ///   value disables the check.
    #[allow(clippy::too_many_arguments)]
    pub fn accept_transactions(
        &self,
        block_height: u64,
        block_gas_limit: u64,
        generator: StakePublicKey,
        txs: Vec<Transaction>,
        consistency_check: Option<VerificationOutput>,
        missed_generators: &[StakePublicKey],
    ) -> Result<(Vec<SpentTransaction>, VerificationOutput)> {
        let session = self.session(block_height, None)?;

        let (spent_txs, verification_output, session, events) = accept(
            session,
            block_height,
            block_gas_limit,
            &generator,
            &txs[..],
            missed_generators,
        )?;

        if let Some(expected_verification) = consistency_check {
            if expected_verification != verification_output {
                // Drop the session if the resulting is inconsistent
                // with the callers one.
                return Err(Error::InconsistentState(verification_output));
            }
        }

        self.set_current_commit(session.commit()?);

        for event in events {
            let _ = self.event_sender.send(event.into());
        }

        Ok((spent_txs, verification_output))
    }

    pub fn finalize_state(&self, commit: [u8; 32]) -> Result<()> {
        let commit_id_path = to_rusk_state_id_path(&self.dir);
        fs::write(commit_id_path, commit)?;

        self.set_base_and_delete(commit);
        Ok(())
    }

    pub fn revert(&self, state_hash: [u8; 32]) -> Result<[u8; 32]> {
        let mut tip = self.tip.write();

        let commits = self.vm.commits();
        if !commits.contains(&state_hash) {
            return Err(Error::CommitNotFound(state_hash));
        }

        tip.current = state_hash;
        Ok(tip.current)
    }

    pub fn revert_to_base_root(&self) -> Result<[u8; 32]> {
        self.revert(self.base_root())
    }

    /// Get the base root.
    pub fn base_root(&self) -> [u8; 32] {
        self.tip.read().base
    }

    /// Get the current state root.
    pub fn state_root(&self) -> [u8; 32] {
        self.tip.read().current
    }

    /// Returns the nullifiers that already exist from a list of given
    /// `nullifiers`.
    pub fn existing_nullifiers(
        &self,
        nullifiers: &Vec<BlsScalar>,
    ) -> Result<Vec<BlsScalar>> {
        self.query(TRANSFER_CONTRACT, "existing_nullifiers", nullifiers)
    }

    /// Returns the stakes.
    pub fn provisioners(
        &self,
        base_commit: Option<[u8; 32]>,
    ) -> Result<impl Iterator<Item = (StakePublicKey, StakeData)>> {
        let (sender, receiver) = mpsc::channel();
        self.feeder_query(STAKE_CONTRACT, "stakes", &(), sender, base_commit)?;
        Ok(receiver.into_iter().map(|bytes| {
            rkyv::from_bytes::<(StakePublicKey, StakeData)>(&bytes).expect(
                "The contract should only return (pk, stake_data) tuples",
            )
        }))
    }

    /// Fetches the previous state data for stake changes in the contract.
    ///
    /// Communicates with the stake contract to obtain information about the
    /// state data before the last changes. Optionally takes a base commit
    /// hash to query changes since a specific point in time.
    ///
    /// # Arguments
    ///
    /// - `base_commit`: An optional base commit hash indicating the starting
    ///   point for querying changes.
    ///
    /// # Returns
    ///
    /// Returns a Result containing an iterator over tuples. Each tuple consists
    /// of a `StakePublicKey` and an optional `StakeData`, representing the
    /// state data before the last changes in the stake contract.
    pub fn last_provisioners_change(
        &self,
        base_commit: Option<[u8; 32]>,
    ) -> Result<Vec<(StakePublicKey, Option<StakeData>)>> {
        let (sender, receiver) = mpsc::channel();
        self.feeder_query(
            STAKE_CONTRACT,
            "prev_state_changes",
            &(),
            sender,
            base_commit,
        )?;
        Ok(receiver.into_iter().map(|bytes| {
            rkyv::from_bytes::<(StakePublicKey, Option<StakeData>)>(&bytes).expect(
                "The contract should only return (pk, Option<stake_data>) tuples",
            )
        }).collect())
    }

    pub fn provisioner(
        &self,
        pk: &StakePublicKey,
    ) -> Result<Option<StakeData>> {
        self.query(STAKE_CONTRACT, "get_stake", pk)
    }

    pub(crate) fn session(
        &self,
        block_height: u64,
        commit: Option<[u8; 32]>,
    ) -> Result<Session> {
        let commit = commit.unwrap_or_else(|| {
            let tip = self.tip.read();
            tip.current
        });

        let session = rusk_abi::new_session(&self.vm, commit, block_height)?;

        Ok(session)
    }

    pub(crate) fn set_current_commit(&self, commit: [u8; 32]) {
        let mut tip = self.tip.write();
        tip.current = commit;
    }

    pub(crate) fn set_base_and_delete(&self, commit: [u8; 32]) {
        let mut tip = self.tip.write();

        tip.current = commit;
        tip.base = commit;

        // We will delete all commits except the new commit.
        let mut commits_to_delete = self.vm.commits();
        commits_to_delete.retain(|c| c != &commit);

        // Delete all commits except the new commit.
        // Deleting commits is blocking, meaning it will wait until any process
        // using the commit is done. This includes any queries that are
        // currently executing.
        // Since we do want commits to be deleted, but don't want block
        // finalization to wait, we spawn a new task to delete the commits.
        task::spawn(delete_commits(self.vm.clone(), commits_to_delete));
    }
}

async fn delete_commits(vm: Arc<VM>, commits: Vec<[u8; 32]>) {
    for commit in commits {
        if let Err(err) = vm.delete_commit(commit) {
            debug!("failed deleting commit {}: {err}", hex::encode(commit));
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn accept(
    session: Session,
    block_height: u64,
    block_gas_limit: u64,
    generator: &StakePublicKey,
    txs: &[Transaction],
    missed_generators: &[StakePublicKey],
) -> Result<(
    Vec<SpentTransaction>,
    VerificationOutput,
    Session,
    Vec<Event>,
)> {
    let mut session = session;

    let mut block_gas_left = block_gas_limit;

    let mut spent_txs = Vec::with_capacity(txs.len());
    let mut dusk_spent = 0;

    let mut events = Vec::new();
    let mut event_hasher = Sha3_256::new();

    for unspent_tx in txs {
        let tx = &unspent_tx.inner;
        let receipt = execute(&mut session, tx)?;

        update_hasher(&mut event_hasher, &receipt.events);
        events.extend(receipt.events);

        let gas_spent = receipt.gas_spent;

        dusk_spent += gas_spent * tx.fee.gas_price;
        block_gas_left = block_gas_left
            .checked_sub(gas_spent)
            .ok_or(Error::OutOfGas)?;

        spent_txs.push(SpentTransaction {
            inner: unspent_tx.clone(),
            gas_spent,
            economic_mode: receipt.economic_mode,
            block_height,
            // We're currently ignoring the result of successful calls
            err: receipt.data.err().map(|e| format!("{e}")),
        });
    }

    let coinbase_events = reward_slash_and_update_root(
        &mut session,
        block_height,
        dusk_spent,
        generator,
        missed_generators,
    )?;

    update_hasher(&mut event_hasher, &coinbase_events);
    events.extend(coinbase_events);

    let state_root = session.root();
    let event_hash = event_hasher.finalize().into();

    Ok((
        spent_txs,
        VerificationOutput {
            state_root,
            event_hash,
        },
        session,
        events,
    ))
}

/// Executes a transaction, returning the receipt of the call and the gas spent.
/// The following steps are performed:
///
/// 1. Call the "spend_and_execute" function on the transfer contract with
///    unlimited gas. If this fails, an error is returned. If an error is
///    returned the transaction should be considered unspendable/invalid, but no
///    re-execution of previous transactions is required.
///
/// 2. Call the "refund" function on the transfer contract with unlimited gas.
///    The amount charged depends on the gas spent by the transaction, and the
///    optional contract call in step 1.
fn execute(
    session: &mut Session,
    tx: &PhoenixTransaction,
) -> Result<CallReceipt<Result<Vec<u8>, ContractError>>, PiecrustError> {
    // Spend the inputs and execute the call. If this errors the transaction is
    // unspendable.
    let mut receipt = session.call::<_, Result<Vec<u8>, ContractError>>(
        TRANSFER_CONTRACT,
        "spend_and_execute",
        tx,
        tx.fee.gas_limit,
    )?;

    // Ensure all gas is consumed if there's an error in the contract call
    if receipt.data.is_err() {
        receipt.gas_spent = receipt.gas_limit;
    }

    let contract_id = tx
        .call
        .clone()
        .map(|(module_id, _, _)| ContractId::from_bytes(module_id));

    // Refund the appropriate amount to the transaction. This call is guaranteed
    // to never error. If it does, then a programming error has occurred. As
    // such, the call to `Result::expect` is warranted.
    let refund_receipt = session
        .call::<_, ()>(
            TRANSFER_CONTRACT,
            "refund",
            &(
                tx.fee,
                receipt.gas_spent,
                receipt.economic_mode.clone(),
                contract_id,
            ),
            u64::MAX,
        )
        .expect("Refunding must succeed");

    receipt.events.extend(refund_receipt.events);

    Ok(receipt)
}

fn update_hasher(hasher: &mut Sha3_256, events: &[Event]) {
    for event in events {
        hasher.update(event.source.as_bytes());
        hasher.update(event.topic.as_bytes());
        hasher.update(&event.data);
    }
}

fn reward_slash_and_update_root(
    session: &mut Session,
    block_height: u64,
    dusk_spent: Dusk,
    generator: &StakePublicKey,
    slashing: &[StakePublicKey],
) -> Result<Vec<Event>> {
    let (dusk_value, generator_value) =
        coinbase_value(block_height, dusk_spent);

    let r = session.call::<_, ()>(
        STAKE_CONTRACT,
        "reward",
        &(*DUSK_KEY, dusk_value),
        u64::MAX,
    )?;

    let mut events = r.events;

    let r = session.call::<_, ()>(
        STAKE_CONTRACT,
        "reward",
        &(*generator, generator_value),
        u64::MAX,
    )?;
    events.extend(r.events);

    let slash_amount = emission_amount(block_height);

    for to_slash in slashing {
        let r = session.call::<_, ()>(
            STAKE_CONTRACT,
            "slash",
            &(*to_slash, slash_amount),
            u64::MAX,
        )?;
        events.extend(r.events);
    }

    let r = session.call::<_, ()>(
        TRANSFER_CONTRACT,
        "update_root",
        &(),
        u64::MAX,
    )?;
    events.extend(r.events);

    Ok(events)
}
