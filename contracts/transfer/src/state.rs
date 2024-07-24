// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) DUSK NETWORK. All rights reserved.

use crate::error::Error;
use crate::tree::Tree;
use crate::verifier_data::*;

use alloc::collections::btree_map::Entry;
use alloc::collections::{BTreeMap, BTreeSet};
use alloc::vec::Vec;

use poseidon_merkle::Opening as PoseidonOpening;
use ringbuffer::{ConstGenericRingBuffer, RingBuffer};
use rusk_abi::{ContractError, ContractId, PublicInput, STAKE_CONTRACT};

use execution_core::{
    transfer::{
        AccountData, MoonlightTransaction, PhoenixTransaction, Transaction,
        TreeLeaf, Withdraw, WithdrawReceiver, WithdrawReplayToken,
        WithdrawSignature, TRANSFER_TREE_DEPTH,
    },
    BlsPublicKey, BlsScalar, Note, Sender,
};

use crate::transitory;
use transitory::Deposit;

/// Number of roots stored
pub const MAX_ROOTS: usize = 5000;

/// An empty account, used as the default return and for instantiating new
/// entries.
const EMPTY_ACCOUNT: AccountData = AccountData {
    nonce: 0,
    balance: 0,
};

fn contract_fn_sender(fn_name: &str, contract: ContractId) -> Sender {
    let mut bytes = [0u8; 128];

    let mut offset = 0;

    bytes[offset..offset + fn_name.len()].copy_from_slice(fn_name.as_bytes());
    offset += fn_name.len();

    bytes[offset..offset + 32].copy_from_slice(&contract.to_bytes());

    Sender::ContractInfo(bytes)
}

pub struct TransferState {
    tree: Tree,
    nullifiers: BTreeSet<BlsScalar>,
    roots: ConstGenericRingBuffer<BlsScalar, MAX_ROOTS>,
    // NOTE: we should never remove entries from this list, since the entries
    //       contain the nonce of the given account. Doing so opens the account
    //       up to replay attacks.
    accounts: BTreeMap<[u8; 193], AccountData>,
    contract_balances: BTreeMap<ContractId, u64>,
}

impl TransferState {
    pub const fn new() -> TransferState {
        TransferState {
            tree: Tree::new(),
            nullifiers: BTreeSet::new(),
            roots: ConstGenericRingBuffer::new(),
            accounts: BTreeMap::new(),
            contract_balances: BTreeMap::new(),
        }
    }

    /// Checks the [`Withdraw`] is correct, and mints the amount of the
    /// withdrawal.
    fn mint_withdrawal(&mut self, fn_name: &str, withdraw: Withdraw) {
        let contract = withdraw.contract();
        let value = withdraw.value();

        let msg = withdraw.signature_message();
        let signature = withdraw.signature();

        match withdraw.token() {
            WithdrawReplayToken::Phoenix(nullifiers) => {
                let tx_payload = transitory::unwrap_phoenix_tx().payload();

                for n in &tx_payload.tx_skeleton.nullifiers {
                    if !nullifiers.contains(n) {
                        panic!("Incorrect nullifiers signed");
                    }
                }
            }
            WithdrawReplayToken::Moonlight(nonce) => {
                let tx_payload = transitory::unwrap_moonlight_tx().payload();

                if nonce != &tx_payload.nonce {
                    panic!("Incorrect nonce signed");
                }
            }
        }

        match withdraw.receiver() {
            WithdrawReceiver::Phoenix(address) => {
                let signature = match signature {
                    WithdrawSignature::Phoenix(s) => s,
                    _ => panic!(
                        "Withdrawal to Phoenix must be signed with Schnorr"
                    ),
                };

                let hash = rusk_abi::hash(msg);
                let pk = address.note_pk();

                if !rusk_abi::verify_schnorr(hash, *pk, *signature) {
                    panic!("Invalid signature");
                }

                let sender = contract_fn_sender(
                    fn_name,
                    ContractId::from_bytes(*contract),
                );

                let note = Note::transparent_stealth(*address, value, sender);
                self.push_note_current_height(note);
            }
            WithdrawReceiver::Moonlight(account) => {
                let signature = match signature {
                    WithdrawSignature::Moonlight(s) => s,
                    _ => panic!(
                        "Withdrawal to Moonlight must be signed with BLS"
                    ),
                };

                if !rusk_abi::verify_bls(msg, *account, *signature) {
                    panic!("Invalid signature");
                }

                let account_bytes = account.to_raw_bytes();
                let account =
                    self.accounts.entry(account_bytes).or_insert(EMPTY_ACCOUNT);

                account.balance += value;
            }
        }
    }

    /// Mint more Dusk.
    ///
    /// This can only be called by the stake contract, and will increase the
    /// total amount of circulating Dusk. It is intended to be called during the
    /// execution of the `withdraw` function, and the amount minted should
    /// conform to the consensus emission schedule.
    ///
    /// # Safety
    /// We assume on trust that the value sent by the stake contract is
    /// according to consensus rules.
    pub fn mint(&mut self, mint: Withdraw) {
        if rusk_abi::caller() != STAKE_CONTRACT {
            panic!("Can only be called by the stake contract!")
        }

        let contract = mint.contract();

        if mint.contract() != contract {
            panic!("Withdrawal should from the stake contract");
        }

        self.mint_withdrawal("MINT", mint);
    }

    /// Withdraw from a contract's balance to a Phoenix note or a Moonlight
    /// account.
    ///
    /// Users sign the `Withdraw` data, which the contract being called
    /// (withdrawn from) is then responsible for making available to this
    /// contract via a call to this function. The function allows for
    /// withdrawals to both Phoenix notes and Moonlight accounts.
    ///
    /// # Panics
    /// This can only be called by the contract specified, and only if said
    /// contract has enough balance.
    pub fn withdraw(&mut self, withdraw: Withdraw) {
        let contract = ContractId::from_bytes(*withdraw.contract());

        if contract != rusk_abi::caller() {
            panic!("The \"withdraw\" function can only be called by the contract specified in the payload");
        }

        let value = withdraw.value();

        if self.contract_balance(&contract) < value {
            panic!("The contract doesn't have enough balance");
        }

        self.sub_contract_balance(&contract, value)
            .expect("Subtracting balance from contract should succeed");

        self.mint_withdrawal("WITHDRAW", withdraw);
    }

    /// Deposit funds to a contract's balance.
    ///
    /// This function checks whether a deposit has been placed earlier on the
    /// state. If so and the contract-id matches the caller, the deposit will be
    /// added to the contract's balance.
    ///
    /// # Panics
    /// This function will panic if there is no deposit on the state or the
    /// caller-id doesn't match the contract-id stored for the deposit.
    pub fn deposit(&mut self, value: u64) {
        // check is the request comes from a contract
        let caller = rusk_abi::caller();
        if caller.is_uninitialized() {
            panic!("Only a contract is authorized to claim a deposit.")
        }

        let deposit = transitory::deposit_info_mut();
        match deposit {
            Deposit::Available(deposit_contract, deposit_value) => {
                let deposit_contract = *deposit_contract;
                let deposit_value = *deposit_value;

                if deposit_value != value {
                    panic!(
                        "The value to deposit doesn't match the value in the transaction"
                    );
                }

                if deposit_contract != caller {
                    panic!("The calling contract doesn't match the contract in the transaction");
                }

                // add to the contract's balance and set the deposit as taken
                self.add_contract_balance(deposit_contract, deposit_value);
                *deposit = Deposit::Taken(deposit_contract, deposit_value);
            }
            Deposit::Taken(_, _) => {
                panic!("The deposit has already been taken")
            }
            Deposit::None => panic!("There is no deposit in the transaction"),
        }
    }

    /// The top level transaction execution function.
    ///
    /// Delegates to [`Self::spend_and_execute_phoenix`] and
    /// [`Self::spend_and_execute_moonlight`], depending on if the transaction
    /// uses the Phoenix or the Moonlight models, respectively.
    pub fn spend_and_execute(
        &mut self,
        tx: Transaction,
    ) -> Result<Vec<u8>, ContractError> {
        match tx {
            Transaction::Phoenix(tx) => self.spend_and_execute_phoenix(tx),
            Transaction::Moonlight(tx) => self.spend_and_execute_moonlight(tx),
        }
    }

    /// Spends the inputs and creates the given UTXO within the given phoenix
    /// transaction, and executes the contract call if present. It performs
    /// all checks necessary to ensure the transaction is valid - hash
    /// matches, anchor has been a root of the tree, proof checks out,
    /// etc...
    ///
    /// This will emplace the deposit in the state, if it exists - making it
    /// available for any contracts called.
    ///
    /// [`refund`] **must** be called if this function succeeds, otherwise we
    /// will have an inconsistent state.
    ///
    /// # Panics
    /// Any failure in the checks performed in processing the transaction will
    /// result in a panic. The contract expects the environment to roll back any
    /// change in state.
    ///
    /// [`refund`]: [`TransferState::refund`]
    fn spend_and_execute_phoenix(
        &mut self,
        tx: PhoenixTransaction,
    ) -> Result<Vec<u8>, ContractError> {
        transitory::put_transaction(tx);
        let tx = transitory::unwrap_phoenix_tx();

        let tx_skeleton = &tx.payload().tx_skeleton;

        // panic if the root is invalid
        if !self.root_exists(&tx_skeleton.root) {
            panic!("Root not found in the state!");
        }

        // panic if any of the given nullifiers already exist
        if self.any_nullifier_exists(&tx_skeleton.nullifiers) {
            panic!("A provided nullifier already exists!");
        }

        // append the nullifiers to the nullifiers set
        self.nullifiers.extend(&tx_skeleton.nullifiers);

        // verify the phoenix-circuit
        if !verify_tx_proof(tx) {
            panic!("Invalid transaction proof!");
        }

        // append the output notes to the phoenix-notes tree
        let block_height = rusk_abi::block_height();
        self.tree
            .extend_notes(block_height, tx_skeleton.outputs.clone());

        // perform contract call if present
        let mut result = Ok(Vec::new());
        if let Some(call) = tx.call() {
            result = rusk_abi::call_raw(
                ContractId::from_bytes(call.contract),
                &call.fn_name,
                &call.fn_args,
            );
        }

        result
    }

    /// Spends the amount available to the moonlight transaction, and executes
    /// the contract call if present. It performs all checks necessary to ensure
    /// the transaction is valid - signature check, available funds, etc...
    ///
    /// This will emplace the deposit in the state, if it exists - making it
    /// available for any contracts called.
    ///
    /// [`refund`] **must** be called if this function succeeds, otherwise we
    /// will have an inconsistent state.
    ///
    /// # Panics
    /// Any failure in the checks performed in processing the transaction will
    /// result in a panic. The contract expects the environment to roll back any
    /// change in state.
    ///
    /// [`refund`]: [`TransferState::refund`]
    fn spend_and_execute_moonlight(
        &mut self,
        tx: MoonlightTransaction,
    ) -> Result<Vec<u8>, ContractError> {
        transitory::put_transaction(tx);
        let tx = transitory::unwrap_moonlight_tx();

        // check the signature is valid and made by `from`
        let payload = tx.payload();
        let signature = *tx.signature();

        let from = payload.from;
        let digest = tx.signature_message();

        if !rusk_abi::verify_bls(digest, from, signature) {
            panic!("Invalid signature!");
        }

        // check `from` has the funds necessary to suppress the total value
        // available in this transaction, and that the `nonce` is higher than
        // the currently held number. If these conditions are violated we panic
        // since the transaction is invalid - either because the account doesn't
        // have (enough) funds, or because they're possibly trying to reuse a
        // previously used signature (i.e. a replay attack).
        //
        // Afterwards, we simply deduct the total amount of the transaction from
        // the balance, increment the nonce, and rely on `refund` to be called
        // after a successful exit.
        let from_bytes = from.to_raw_bytes();

        // the total value carried by a transaction is the sum of the value, the
        // deposit, and gas_limit * gas_price.
        let total_value = payload.value
            + payload.deposit
            + payload.gas_limit * payload.gas_price;

        match self.accounts.get_mut(&from_bytes) {
            Some(account) => {
                if total_value > account.balance {
                    panic!("Account doesn't have enough funds");
                }
                if payload.nonce <= account.nonce {
                    panic!("Replayed nonce");
                }

                account.balance -= total_value;
                account.nonce = payload.nonce;
            }
            None => panic!("Account has no funds"),
        }

        // if there is a value carried by the transaction but no key specified
        // in the `to` field, we just give the value back to `from`.
        if payload.value > 0 {
            let key = match payload.to {
                Some(to) => to.to_raw_bytes(),
                None => from_bytes,
            };

            // if the key has no entry, we simply instantiate a new one with a
            // zero nonce and balance.
            let account = self.accounts.entry(key).or_insert(EMPTY_ACCOUNT);
            account.balance += payload.value;
        }

        // perform contract call if present
        let mut result = Ok(Vec::new());
        if let Some(call) = tx.call() {
            result = rusk_abi::call_raw(
                ContractId::from_bytes(call.contract),
                &call.fn_name,
                &call.fn_args,
            );
        }

        result
    }

    /// Refund the previously performed transaction, taking into account the
    /// given gas spent and a potential deposit that hasn't been picked up by
    /// the contract. The note produced will be refunded to the address present
    /// in the fee structure.
    ///
    /// This function guarantees that it will not panic.
    pub fn refund(&mut self, gas_spent: u64) {
        let tx = transitory::unwrap_tx();

        // If there is a deposit still available on the call to this function,
        // we refund it to the called.
        let deposit = match transitory::deposit_info() {
            Deposit::Available(_, deposit) => Some(*deposit),
            _ => None,
        };

        // in phoenix, a refund note is with the unspent amount to the stealth
        // address in the `Fee` structure, while in moonlight we simply refund
        // the `from` account for what it didn't spend
        //
        // any eventual deposit that failed to be "picked up" is refunded in the
        // same way - in phoenix the same note is reused, in moonlight the
        // 'key's balance gets increased.
        match tx {
            Transaction::Phoenix(tx) => {
                let fee = &tx.payload().fee;

                let remainder_note = fee.gen_remainder_note(gas_spent, deposit);

                let remainder_value = remainder_note
                    .value(None)
                    .expect("Should always succeed for a transparent note");

                if remainder_value > 0 {
                    self.push_note_current_height(remainder_note);
                }
            }
            Transaction::Moonlight(tx) => {
                let payload = tx.payload();

                let from_bytes = payload.from.to_raw_bytes();

                let remaining_gas = payload.gas_limit - gas_spent;
                let remaining = remaining_gas * payload.gas_price
                    + deposit.unwrap_or_default();

                let account = self.accounts.get_mut(&from_bytes).expect(
                    "The account that just transacted must have an entry",
                );

                account.balance += remaining;
            }
        }
    }

    /// Push a note to the contract's state with the given block height
    ///
    /// Note: the method `update_root` needs to be called after the last note is
    /// pushed.
    pub fn push_note(&mut self, block_height: u64, note: Note) -> Note {
        let tree_leaf = TreeLeaf { block_height, note };
        let pos = self.tree.push(tree_leaf.clone());
        rusk_abi::emit("TREE_LEAF", (pos, tree_leaf));
        self.get_note(pos)
            .expect("There should be a note that was just inserted")
    }

    /// Feeds the host with the leaves in the tree, starting from the given
    /// height.
    pub fn leaves_from_height(&self, height: u64) {
        for leaf in self.tree.leaves(height) {
            rusk_abi::feed(leaf.clone());
        }
    }

    /// Feeds the host with the leaves in the tree, starting from the given
    /// position.
    pub fn leaves_from_pos(&self, pos: u64) {
        for leaf in self.tree.leaves_pos(pos) {
            rusk_abi::feed(leaf.clone());
        }
    }

    /// Update the root for of the tree.
    pub fn update_root(&mut self) {
        let root = self.tree.root();
        self.roots.push(root);
    }

    /// Get the root of the tree.
    pub fn root(&self) -> BlsScalar {
        self.tree.root()
    }

    /// Get the count of the notes in the tree.
    pub fn num_notes(&self) -> u64 {
        self.tree.leaves_len()
    }

    /// Get the opening
    pub fn opening(
        &self,
        pos: u64,
    ) -> Option<PoseidonOpening<(), TRANSFER_TREE_DEPTH>> {
        self.tree.opening(pos)
    }

    /// Takes some nullifiers and returns a vector containing the ones that
    /// already exists in the contract
    pub fn existing_nullifiers(
        &self,
        nullifiers: Vec<BlsScalar>,
    ) -> Vec<BlsScalar> {
        nullifiers
            .into_iter()
            .filter_map(|n| self.nullifiers.get(&n).map(|_| n))
            .collect()
    }

    pub fn account(&self, key: &BlsPublicKey) -> AccountData {
        let key_bytes = key.to_raw_bytes();
        self.accounts
            .get(&key_bytes)
            .cloned()
            .unwrap_or(EMPTY_ACCOUNT)
    }

    pub fn add_account_balance(&mut self, key: &BlsPublicKey, value: u64) {
        let key_bytes = key.to_raw_bytes();
        let account = self.accounts.entry(key_bytes).or_insert(EMPTY_ACCOUNT);
        account.balance = account.balance.saturating_add(value);
    }

    pub fn sub_account_balance(&mut self, key: &BlsPublicKey, value: u64) {
        let key_bytes = key.to_raw_bytes();
        if let Some(account) = self.accounts.get_mut(&key_bytes) {
            account.balance = account.balance.saturating_sub(value);
        }
    }

    /// Return the balance of a given contract.
    pub fn contract_balance(&self, contract_id: &ContractId) -> u64 {
        self.contract_balances
            .get(contract_id)
            .copied()
            .unwrap_or_default()
    }

    /// Add balance to the given contract
    pub fn add_contract_balance(&mut self, contract: ContractId, value: u64) {
        match self.contract_balances.entry(contract) {
            Entry::Vacant(ve) => {
                ve.insert(value);
            }
            Entry::Occupied(mut oe) => {
                let v = oe.get_mut();
                *v += value
            }
        }
    }

    pub(crate) fn sub_contract_balance(
        &mut self,
        address: &ContractId,
        value: u64,
    ) -> Result<(), Error> {
        match self.contract_balances.get_mut(address) {
            Some(balance) => {
                let (bal, underflow) = balance.overflowing_sub(value);

                if underflow {
                    Err(Error::NotEnoughBalance)
                } else {
                    *balance = bal;

                    Ok(())
                }
            }

            _ => Err(Error::NotEnoughBalance),
        }
    }

    fn get_note(&self, pos: u64) -> Option<Note> {
        self.tree.get(pos).map(|l| l.note)
    }

    fn any_nullifier_exists(&self, nullifiers: &[BlsScalar]) -> bool {
        for nullifier in nullifiers {
            if self.nullifiers.contains(nullifier) {
                return true;
            }
        }

        false
    }

    fn root_exists(&self, root: &BlsScalar) -> bool {
        self.roots.contains(root)
    }

    fn push_note_current_height(&mut self, note: Note) -> Note {
        let block_height = rusk_abi::block_height();
        self.push_note(block_height, note)
    }
}

fn verify_tx_proof(tx: &PhoenixTransaction) -> bool {
    let pis: Vec<PublicInput> =
        tx.public_inputs().iter().map(|pi| pi.into()).collect();

    // fetch the verifier data
    let num_inputs = tx.payload().tx_skeleton.nullifiers.len();
    let vd = verifier_data_execute(num_inputs)
        .expect("No circuit available for given number of inputs!")
        .to_vec();

    // verify the proof
    rusk_abi::verify_proof(vd, tx.proof().to_vec(), pis)
}

#[cfg(test)]
mod test_transfer {
    use super::*;

    #[test]
    fn find_existing_nullifiers() {
        let mut transfer = TransferState::new();

        let (zero, one, two, three, ten, eleven) = (
            BlsScalar::from(0),
            BlsScalar::from(1),
            BlsScalar::from(2),
            BlsScalar::from(3),
            BlsScalar::from(10),
            BlsScalar::from(11),
        );

        let existing = transfer
            .existing_nullifiers(vec![zero, one, two, three, ten, eleven]);

        assert_eq!(existing.len(), 0);

        for i in 1..10 {
            transfer.nullifiers.insert(BlsScalar::from(i));
        }

        let existing = transfer
            .existing_nullifiers(vec![zero, one, two, three, ten, eleven]);

        assert_eq!(existing.len(), 3);

        assert!(existing.contains(&one));
        assert!(existing.contains(&two));
        assert!(existing.contains(&three));
    }
}
