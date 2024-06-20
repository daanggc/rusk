// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) DUSK NETWORK. All rights reserved.

pub mod common;

use crate::common::utils::*;

use dusk_bytes::{ParseHexStr, Serializable};
use execution_core::{
    stake::Stake, BlsPublicKey, BlsSecretKey, BlsSignature, Fee, JubJubScalar,
    Note, Ownable, PublicKey, SecretKey, Transaction, GENERATOR_NUMS_EXTENDED,
};
use ff::Field;
use rand::rngs::StdRng;
use rand::{CryptoRng, RngCore, SeedableRng};
use rkyv::{Archive, Deserialize, Serialize};
use rusk_abi::dusk::{dusk, LUX};
use rusk_abi::{
    ContractData, ContractId, EconomicMode, Error, Session, TRANSFER_CONTRACT,
    VM,
};
use transfer_circuits::{
    CircuitInput, CircuitInputSignature, ExecuteCircuitOneTwo,
    SendToContractTransparentCircuit,
};

const GENESIS_VALUE: u64 = dusk(1_000.0);
const SUBSIDY_VALUE: u64 = GENESIS_VALUE / 2;
const POINT_LIMIT: u64 = 0x10_000_000;

const CHARLIE_CONTRACT_ID: ContractId = {
    let mut bytes = [0u8; 32];
    bytes[0] = 0xFC;
    ContractId::from_bytes(bytes)
};
const ALICE_CONTRACT_ID: ContractId = {
    let mut bytes = [0u8; 32];
    bytes[0] = 0xFA;
    ContractId::from_bytes(bytes)
};

const CHARLIE_FREE_LIMIT: u64 = 20_000_000;
const CHARLIE_FREE_PRICE_HINT: (u64, u64) = (200, 1);

const OWNER: [u8; 32] = [0; 32];

/// Subsidy a contract with a value.
#[derive(Debug, Clone, PartialEq, Eq, Archive, Serialize, Deserialize)]
#[archive_attr(derive(bytecheck::CheckBytes))]
pub struct Subsidy {
    /// Public key to which the subsidy will belong.
    pub public_key: BlsPublicKey,
    /// Signature belonging to the given public key.
    pub signature: BlsSignature,
    /// Value of the subsidy.
    pub value: u64,
    /// Proof of the `STCT` circuit.
    pub proof: Vec<u8>,
}

fn instantiate<Rng: RngCore + CryptoRng>(
    rng: &mut Rng,
    vm: &VM,
    psk: Option<PublicKey>,
    charlie_owner: Option<PublicKey>,
) -> Session {
    let transfer_bytecode = include_bytes!(
        "../../../target/dusk/wasm64-unknown-unknown/release/transfer_contract.wasm"
    );
    let alice_bytecode = include_bytes!(
        "../../../target/dusk/wasm32-unknown-unknown/release/alice.wasm"
    );
    let charlie_bytecode = include_bytes!(
        "../../../target/dusk/wasm32-unknown-unknown/release/charlie.wasm"
    );

    let mut session = rusk_abi::new_genesis_session(vm);

    session
        .deploy(
            transfer_bytecode,
            ContractData::builder()
                .owner(OWNER)
                .contract_id(TRANSFER_CONTRACT),
            POINT_LIMIT,
        )
        .expect("Deploying the transfer contract should succeed");

    if let Some(charlie_owner) = charlie_owner {
        session
            .deploy(
                charlie_bytecode,
                ContractData::builder()
                    .owner(charlie_owner.to_bytes())
                    .contract_id(CHARLIE_CONTRACT_ID)
                    .free_limit(CHARLIE_FREE_LIMIT)
                    .free_price_hint(CHARLIE_FREE_PRICE_HINT),
                POINT_LIMIT,
            )
            .expect("Deploying the charlie contract should succeed");
    }

    session
        .deploy(
            alice_bytecode,
            ContractData::builder()
                .owner(OWNER)
                .contract_id(ALICE_CONTRACT_ID),
            POINT_LIMIT,
        )
        .expect("Deploying the alice contract should succeed");

    if let Some(psk) = psk {
        let genesis_note = Note::transparent(rng, &psk, GENESIS_VALUE);

        // push genesis note to the contract
        session
            .call::<_, Note>(
                TRANSFER_CONTRACT,
                "push_note",
                &(0u64, genesis_note),
                POINT_LIMIT,
            )
            .expect("Pushing genesis note should succeed");
    }

    update_root(&mut session).expect("Updating the root should succeed");

    // sets the block height for all subsequent operations to 1
    let base = session.commit().expect("Committing should succeed");

    rusk_abi::new_session(vm, base, 1)
        .expect("Instantiating new session should succeed")
}

/// Transfers value from given note into contract's account.
/// Expects transparent note which will fund the subsidy and a subsidy value
/// which is smaller or equal to the value of the note.
/// Returns the gas spent on the operation.
fn subsidize_contract<R: RngCore + CryptoRng>(
    rng: &mut R,
    mut session: &mut Session,
    contract_id: ContractId,
    subsidy_keeper_pk: BlsPublicKey,
    subsidy_keeper_sk: BlsSecretKey,
    subsidizer_psk: PublicKey,
    subsidizer_ssk: SecretKey,
    input_note: Note,
    subsidy_value: u64,
) -> ExecutionResult {
    let input_note_value = input_note
        .value(None)
        .expect("The value should be transparent");
    let input_blinder = input_note
        .blinding_factor(None)
        .expect("The blinder should be transparent");
    let input_nullifier = input_note.gen_nullifier(&subsidizer_ssk);

    let gas_limit = dusk(1.0);
    let gas_price = LUX;

    assert!(subsidy_value <= input_note_value);
    let crossover_blinder = JubJubScalar::random(&mut *rng);

    let (mut fee, crossover) = Note::obfuscated(
        rng,
        &subsidizer_psk,
        subsidy_value,
        crossover_blinder,
    )
    .try_into()
    .expect("Getting a fee and a crossover should succeed");

    fee.gas_limit = gas_limit;
    fee.gas_price = gas_price;

    let change_value = input_note_value - subsidy_value - gas_price * gas_limit;
    let change_blinder = JubJubScalar::random(&mut *rng);
    let change_note =
        Note::obfuscated(rng, &subsidizer_psk, change_value, change_blinder);

    let stct_address = rusk_abi::contract_to_scalar(&CHARLIE_CONTRACT_ID);
    let stct_signature = SendToContractTransparentCircuit::sign(
        rng,
        &subsidizer_ssk,
        &fee,
        &crossover,
        subsidy_value,
        &stct_address,
    );

    let stct_circuit = SendToContractTransparentCircuit::new(
        &fee,
        &crossover,
        subsidy_value,
        crossover_blinder,
        stct_address,
        stct_signature,
    );

    let (prover, _) = prover_verifier("SendToContractTransparentCircuit");
    let (stct_proof, _) = prover
        .prove(rng, &stct_circuit)
        .expect("Proving STCT circuit should succeed");

    let stake_digest = Stake::signature_message(0, subsidy_value);
    let sig = subsidy_keeper_sk.sign(&subsidy_keeper_pk, &stake_digest);

    let subsidy = Subsidy {
        public_key: subsidy_keeper_pk,
        signature: sig,
        value: subsidy_value,
        proof: stct_proof.to_bytes().to_vec(),
    };
    let subsidy_bytes = rkyv::to_bytes::<_, 4096>(&subsidy)
        .expect("Subsidy should be correctly serialized")
        .to_vec();

    let call = Some((
        contract_id.to_bytes(),
        String::from("subsidize"),
        subsidy_bytes,
    ));

    let mut execute_circuit = ExecuteCircuitOneTwo::new();

    execute_circuit.set_fee_crossover(
        &fee,
        &crossover,
        subsidy_value,
        crossover_blinder,
    );

    execute_circuit
        .add_output_with_data(change_note, change_value, change_blinder)
        .expect("Appending output should succeed");

    let input_opening = opening(&mut session, *input_note.pos())
        .expect("Querying the opening for the given position should succeed")
        .expect("An opening should exist for a note in the tree");

    let sk_r = subsidizer_ssk.sk_r(input_note.stealth_address());
    let pk_r_p = GENERATOR_NUMS_EXTENDED * sk_r.as_ref();

    let anchor =
        root(&mut session).expect("Getting the anchor should be successful");

    let tx_hash_input_bytes = Transaction::hash_input_bytes_from_components(
        &[input_nullifier],
        &[change_note],
        &anchor,
        &fee,
        &Some(crossover),
        &call,
    );
    let tx_hash = rusk_abi::hash(tx_hash_input_bytes);

    execute_circuit.set_tx_hash(tx_hash);

    let circuit_input_signature =
        CircuitInputSignature::sign(rng, &subsidizer_ssk, &input_note, tx_hash);
    let circuit_input = CircuitInput::new(
        input_opening,
        input_note,
        pk_r_p.into(),
        input_note_value,
        input_blinder,
        input_nullifier,
        circuit_input_signature,
    );

    execute_circuit
        .add_input(circuit_input)
        .expect("Appending input should succeed");

    let (prover_key, _) = prover_verifier("ExecuteCircuitOneTwo");
    let (execute_proof, _) = prover_key
        .prove(rng, &execute_circuit)
        .expect("Proving should be successful");

    let tx = Transaction {
        anchor,
        nullifiers: vec![input_nullifier],
        outputs: vec![change_note],
        fee,
        crossover: Some(crossover),
        proof: execute_proof.to_bytes().to_vec(),
        call,
    };

    let execution_result =
        execute(&mut session, tx).expect("Executing TX should succeed");
    update_root(&mut session).expect("Updating the root should succeed");
    execution_result
}

fn instantiate_and_subsidize_contract(
    vm: &mut VM,
    contract_id: ContractId,
    subsidy_value: u64,
) -> (Session, SecretKey) {
    let rng = &mut StdRng::seed_from_u64(0xfeeb);

    let subsidizer_ssk = SecretKey::random(rng); // money giver to subsidize the sponsor
    let subsidizer_psk = PublicKey::from(&subsidizer_ssk);

    let test_sponsor_ssk = SecretKey::random(rng);
    let test_sponsor_psk = PublicKey::from(&test_sponsor_ssk); // sponsor is Charlie's owner

    let subsidy_keeper_sk = BlsSecretKey::random(rng);
    let subsidy_keeper_pk = BlsPublicKey::from(&subsidy_keeper_sk);

    let mut session =
        instantiate(rng, vm, Some(subsidizer_psk), Some(test_sponsor_psk));

    let leaves = leaves_from_height(&mut session, 0)
        .expect("Getting leaves in the given range should succeed");

    assert_eq!(leaves.len(), 1, "There should be one note in the state");

    let note = leaves[0].note;

    assert_eq!(
        module_balance(&mut session, contract_id)
            .expect("Module balance should succeed"),
        0u64
    );

    subsidize_contract(
        rng,
        &mut session,
        contract_id,
        subsidy_keeper_pk,
        subsidy_keeper_sk,
        subsidizer_psk,
        subsidizer_ssk,
        note,
        subsidy_value,
    );

    assert_eq!(
        module_balance(&mut session, contract_id)
            .expect("Module balance should succeed"),
        subsidy_value
    );

    println!("contract has been subsidized with amount={SUBSIDY_VALUE}");

    (session, test_sponsor_ssk)
}

/// Creates and executes a transaction which calls a given method
/// of a given contract. The transaction will contain input and
/// output notes. The contract will pay for all the gas costs
/// so that the call will effectively be free for the caller.
fn call_contract_method_with_deposit(
    mut session: &mut Session,
    contract_id: ContractId,
    method: impl AsRef<str>,
    sponsor_ssk: SecretKey,
    gas_price: u64,
) -> Result<(ExecutionResult, u64, u64), Error> {
    const SPONSORING_NOTE_VALUE: u64 = 100_000_000_000;

    let rng = &mut StdRng::seed_from_u64(0xfeeb);
    let test_sponsor_psk = PublicKey::from(&sponsor_ssk); // sponsor is Charlie's owner

    // make sure the sponsoring contract is properly subsidized (has funds)
    let balance_before = module_balance(&mut session, contract_id)
        .expect("Module balance should succeed");
    println!(
        "current balance of contract '{:X?}' is {}",
        contract_id.to_bytes()[0],
        balance_before
    );
    assert!(balance_before > 0);

    let note = Note::transparent(rng, &test_sponsor_psk, SPONSORING_NOTE_VALUE);

    let note = session
        .call::<_, Note>(
            TRANSFER_CONTRACT,
            "push_note",
            &(0u64, note),
            POINT_LIMIT,
        )
        .expect("Pushing genesis note should succeed")
        .data;

    update_root(&mut session).expect("Updating the root should succeed");

    let input_value =
        note.value(None).expect("The value should be transparent");
    println!(
        "sponsoring note has been obtained, note value={}",
        input_value
    );
    let input_blinder = note
        .blinding_factor(None)
        .expect("The blinder should be transparent");

    let input_nullifier = note.gen_nullifier(&sponsor_ssk);

    let fee = Fee::new(rng, POINT_LIMIT, gas_price, &test_sponsor_psk);

    // The change note should have the value of the input note, minus what is
    // maximally spent.
    let change_value = input_value - gas_price * POINT_LIMIT;
    let change_blinder = JubJubScalar::random(&mut *rng);
    println!("prepared change note with change value={}", change_value);
    let change_note =
        Note::obfuscated(rng, &test_sponsor_psk, change_value, change_blinder);

    let call = Some((
        contract_id.to_bytes(),
        String::from(method.as_ref()),
        vec![],
    ));

    // Compose the circuit. In this case we're using one input and one output.
    let mut circuit = ExecuteCircuitOneTwo::new();

    circuit.set_fee(&fee);
    circuit
        .add_output_with_data(change_note, change_value, change_blinder)
        .expect("appending input or output should succeed");

    let opening = opening(session, *note.pos())
        .expect("Querying the opening for the given position should succeed")
        .expect("An opening should exist for a note in the tree");

    // Generate pk_r_p
    let sk_r = sponsor_ssk.sk_r(note.stealth_address());
    let pk_r_p = GENERATOR_NUMS_EXTENDED * sk_r.as_ref();

    let anchor =
        root(session).expect("Getting the anchor should be successful");

    let tx_hash_input_bytes = Transaction::hash_input_bytes_from_components(
        &[input_nullifier],
        &[change_note],
        &anchor,
        &fee,
        &None,
        &call,
    );
    let tx_hash = rusk_abi::hash(tx_hash_input_bytes);

    circuit.set_tx_hash(tx_hash);

    let circuit_input_signature =
        CircuitInputSignature::sign(rng, &sponsor_ssk, &note, tx_hash);
    let circuit_input = CircuitInput::new(
        opening,
        note,
        pk_r_p.into(),
        input_value,
        input_blinder,
        input_nullifier,
        circuit_input_signature,
    );

    circuit
        .add_input(circuit_input)
        .expect("appending input or output should succeed");

    let (prover, _) = prover_verifier("ExecuteCircuitOneTwo");
    let (proof, _) = prover
        .prove(rng, &circuit)
        .expect("creating a proof should succeed");

    let tx = Transaction {
        anchor,
        nullifiers: vec![input_nullifier],
        outputs: vec![change_note],
        fee,
        crossover: None,
        proof: proof.to_bytes().to_vec(),
        call,
    };

    println!(
        "executing method '{}' - contract '{:X?}' is paying",
        method.as_ref(),
        contract_id.to_bytes()[0]
    );
    let execution_result = execute(session, tx)?;
    update_root(session).expect("Updating the root should succeed");

    println!(
        "gas spent for the execution of method '{}' is {}",
        method.as_ref(),
        execution_result.gas_spent
    );

    let balance_after = module_balance(&mut session, contract_id)
        .expect("Module balance should succeed");

    println!(
        "contract's '{:X?}' balance before the call: {}",
        contract_id.as_bytes()[0],
        balance_before
    );
    println!(
        "contract's '{:X?}' balance after the call: {}",
        contract_id.as_bytes()[0],
        balance_after
    );

    Ok((execution_result, balance_before, balance_after))
}

/// Creates and executes a transaction which calls a given method
/// of a given contract. The transaction won't contain any notes
/// and all gas costs will be paid by the called contract.
/// The contract is expected to have funds in its wallet yet the
/// caller does not need to have a wallet at all - it is a free
/// call for the caller, except for the provided PoW.
fn call_contract_method_no_deposit(
    mut session: &mut Session,
    contract_id: ContractId,
    method: impl AsRef<str>,
) -> Result<(ExecutionResult, u64, u64), Error> {
    // make sure the sponsoring contract is properly subsidized (has funds)
    let balance_before = module_balance(&mut session, contract_id)
        .expect("Module balance should succeed");
    println!(
        "current balance of contract '{:X?}' is {}",
        contract_id.to_bytes()[0],
        balance_before
    );

    // we just need any psk, it won't be used as there won't be any refund
    const DUMMY_PSK: &str = "8ebcaed21b0dd87eb7ca0b1cc1cd3e2e3df85a737037f475f9f7c65176f9ad3f8ebcaed21b0dd87eb7ca0b1cc1cd3e2e3df85a737037f475f9f7c65176f9ad3f";
    let dummy_refund_psk: PublicKey = PublicKey::from_hex_str(DUMMY_PSK)
        .expect("public key creation should succeed");

    let mut rng = StdRng::seed_from_u64(0xcafe);

    // note: gas price zero means that this is will be a free call
    // we use dummy psk until we change the format of the Fee struct
    // to accommodate for the free calls
    let fee = Fee::new(&mut rng, POINT_LIMIT, 0, &dummy_refund_psk);

    let call = Some((
        contract_id.to_bytes(),
        String::from(method.as_ref()),
        vec![],
    ));

    let anchor =
        root(session).expect("Getting the anchor should be successful");

    let tx = Transaction {
        anchor,
        nullifiers: vec![],
        outputs: vec![],
        fee,
        crossover: None,
        proof: vec![],
        call,
    };

    println!(
        "executing method '{}' - contract '{:X?}' is paying",
        method.as_ref(),
        contract_id.to_bytes()[0]
    );
    let execution_result = execute(session, tx)?;
    update_root(session).expect("Updating the root should succeed");

    println!(
        "gas spent for the execution of method '{}' is {}",
        method.as_ref(),
        execution_result.gas_spent
    );

    let balance_after = module_balance(&mut session, contract_id)
        .expect("Module balance should succeed");

    println!(
        "contract's '{:X?}' balance before the call: {}",
        contract_id.as_bytes()[0],
        balance_before
    );
    println!(
        "contract's '{:X?}' balance after the call: {}",
        contract_id.as_bytes()[0],
        balance_after
    );

    Ok((execution_result, balance_before, balance_after))
}

#[test]
fn contract_pays_with_deposit() {
    const GAS_PRICE: u64 = 2;
    let vm = &mut rusk_abi::new_ephemeral_vm()
        .expect("Creating ephemeral VM should work");

    let (mut session, sponsor_ssk) = instantiate_and_subsidize_contract(
        vm,
        CHARLIE_CONTRACT_ID,
        SUBSIDY_VALUE,
    );
    let (execution_result, balance_before, balance_after) =
        call_contract_method_with_deposit(
            &mut session,
            CHARLIE_CONTRACT_ID,
            "pay",
            sponsor_ssk,
            GAS_PRICE,
        )
        .expect("Contract call should succeed");
    assert!(balance_after < balance_before);
    let balance_delta = balance_before - balance_after;
    if let EconomicMode::Allowance(allowance) = execution_result.economic_mode {
        assert!(allowance >= balance_delta)
    } else {
        assert!(false);
    }
    assert!(balance_delta >= execution_result.gas_spent);
}

#[test]
fn contract_pays_no_deposit() {
    let vm = &mut rusk_abi::new_ephemeral_vm()
        .expect("Creating ephemeral VM should work");

    let (mut session, _) = instantiate_and_subsidize_contract(
        vm,
        CHARLIE_CONTRACT_ID,
        SUBSIDY_VALUE,
    );
    let (execution_result, balance_before, balance_after) =
        call_contract_method_no_deposit(
            &mut session,
            CHARLIE_CONTRACT_ID,
            "pay",
        )
        .expect("Contract call should succeed");
    assert!(balance_after < balance_before);
    let balance_delta = balance_before - balance_after;
    if let EconomicMode::Allowance(allowance) = execution_result.economic_mode {
        println!("balance_delta={} allowance={}", balance_delta, allowance);
        assert!(
            allowance
                >= (balance_delta * CHARLIE_FREE_PRICE_HINT.1
                    / CHARLIE_FREE_PRICE_HINT.0) /* we need to convert
                                                  * balance delta, which is
                                                  * in Dusk,
                                                  * into gas points, here we assume that during test
                                                  * the average gas price is
                                                  * 1 */
        )
    } else {
        assert!(false);
    }
    assert!(balance_delta >= execution_result.gas_spent);
}

#[test]
fn contract_pays_not_enough_allowance_with_deposit() {
    const GAS_PRICE: u64 = 2;
    let vm = &mut rusk_abi::new_ephemeral_vm()
        .expect("Creating ephemeral VM should work");

    let (mut session, sponsor_ssk) = instantiate_and_subsidize_contract(
        vm,
        CHARLIE_CONTRACT_ID,
        SUBSIDY_VALUE,
    );
    let result = call_contract_method_with_deposit(
        &mut session,
        CHARLIE_CONTRACT_ID,
        "pay_and_fail",
        sponsor_ssk,
        GAS_PRICE,
    );
    assert!(result.is_err());
}

#[test]
fn contract_pays_not_enough_allowance_no_deposit() {
    let vm = &mut rusk_abi::new_ephemeral_vm()
        .expect("Creating ephemeral VM should work");

    let (mut session, _) = instantiate_and_subsidize_contract(
        vm,
        CHARLIE_CONTRACT_ID,
        SUBSIDY_VALUE,
    );
    let result = call_contract_method_no_deposit(
        &mut session,
        CHARLIE_CONTRACT_ID,
        "pay_and_fail",
    );
    assert!(result.is_err());
}

#[test]
fn contract_pays_not_enough_balance_with_deposit() {
    const INSUFFICIENT_SUBSIDY: u64 = 1000;
    const GAS_PRICE: u64 = 2;
    let vm = &mut rusk_abi::new_ephemeral_vm()
        .expect("Creating ephemeral VM should work");

    let (mut session, sponsor_ssk) = instantiate_and_subsidize_contract(
        vm,
        CHARLIE_CONTRACT_ID,
        INSUFFICIENT_SUBSIDY,
    );
    let result = call_contract_method_with_deposit(
        &mut session,
        CHARLIE_CONTRACT_ID,
        "pay",
        sponsor_ssk,
        GAS_PRICE,
    );
    assert!(result.is_err());
}

#[test]
fn contract_pays_not_enough_balance_no_deposit() {
    const INSUFFICIENT_SUBSIDY: u64 = 1000;
    let vm = &mut rusk_abi::new_ephemeral_vm()
        .expect("Creating ephemeral VM should work");

    let (mut session, _) = instantiate_and_subsidize_contract(
        vm,
        CHARLIE_CONTRACT_ID,
        INSUFFICIENT_SUBSIDY,
    );
    let result = call_contract_method_no_deposit(
        &mut session,
        CHARLIE_CONTRACT_ID,
        "pay",
    );
    assert!(result.is_err());
}

#[test]
fn contract_does_not_pay_indirectly_with_deposit() {
    const GAS_PRICE: u64 = 2;
    let vm = &mut rusk_abi::new_ephemeral_vm()
        .expect("Creating ephemeral VM should work");

    let (mut session, sponsor_ssk) = instantiate_and_subsidize_contract(
        vm,
        CHARLIE_CONTRACT_ID,
        SUBSIDY_VALUE,
    );
    let (execution_result, balance_before, balance_after) =
        call_contract_method_with_deposit(
            &mut session,
            CHARLIE_CONTRACT_ID,
            "pay_indirectly_and_fail",
            sponsor_ssk,
            GAS_PRICE,
        )
        .expect("Contract call should succeed");
    assert_eq!(balance_after, balance_before);
    assert_eq!(execution_result.economic_mode, EconomicMode::None);
}

#[test]
fn contract_does_not_pay_indirectly_no_deposit() {
    let vm = &mut rusk_abi::new_ephemeral_vm()
        .expect("Creating ephemeral VM should work");

    let (mut session, _) = instantiate_and_subsidize_contract(
        vm,
        CHARLIE_CONTRACT_ID,
        SUBSIDY_VALUE,
    );
    let (execution_result, balance_before, balance_after) =
        call_contract_method_no_deposit(
            &mut session,
            CHARLIE_CONTRACT_ID,
            "pay_indirectly_and_fail",
        )
        .expect("Contract call should succeed");
    assert_eq!(balance_after, balance_before);
    assert_eq!(execution_result.economic_mode, EconomicMode::None);
}

#[test]
fn free_tx_calls_not_paying_contract() {
    let vm = &mut rusk_abi::new_ephemeral_vm()
        .expect("Creating ephemeral VM should work");

    let (mut session, _) = instantiate_and_subsidize_contract(
        vm,
        CHARLIE_CONTRACT_ID,
        SUBSIDY_VALUE,
    );
    let result = call_contract_method_no_deposit(
        &mut session,
        ALICE_CONTRACT_ID,
        "ping",
    );
    assert!(result.is_err())
}
