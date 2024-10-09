// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) DUSK NETWORK. All rights reserved.

pub mod common;

use crate::common::utils::{
    account, chain_id, contract_balance, create_phoenix_transaction, execute,
    filter_notes_owned_by, leaves_from_height, leaves_from_pos, num_notes,
    owned_notes_value, update_root,
};

use dusk_bytes::Serializable;
use ff::Field;
use rand::rngs::StdRng;
use rand::{CryptoRng, RngCore, SeedableRng};

use execution_core::{
    dusk,
    signatures::bls::{
        PublicKey as AccountPublicKey, SecretKey as AccountSecretKey,
    },
    transfer::{
        data::{ContractCall, TransactionData},
        moonlight::Transaction as MoonlightTransaction,
        phoenix::{
            Note, PublicKey as PhoenixPublicKey, SecretKey as PhoenixSecretKey,
            ViewKey as PhoenixViewKey,
        },
        withdraw::{Withdraw, WithdrawReceiver, WithdrawReplayToken},
        ContractToAccount, ContractToContract, TRANSFER_CONTRACT,
    },
    ContractError, ContractId, JubJubScalar, LUX,
};
use rusk_abi::{ContractData, Session, VM};

const PHOENIX_GENESIS_VALUE: u64 = dusk(1_000.0);
const MOONLIGHT_GENESIS_VALUE: u64 = dusk(1_000.0);

const GAS_LIMIT: u64 = 0x10000000;

const ALICE_ID: ContractId = {
    let mut bytes = [0u8; 32];
    bytes[0] = 0xFA;
    ContractId::from_bytes(bytes)
};
const BOB_ID: ContractId = {
    let mut bytes = [0u8; 32];
    bytes[0] = 0xFB;
    ContractId::from_bytes(bytes)
};

const OWNER: [u8; 32] = [0; 32];
const CHAIN_ID: u8 = 0xFA;

/// Instantiate the virtual machine with the transfer contract deployed, with a
/// single note carrying the `GENESIS_VALUE` owned by the given public key.
fn instantiate<Rng: RngCore + CryptoRng>(
    rng: &mut Rng,
    vm: &VM,
    phoenix_pk: &PhoenixPublicKey,
    moonlight_pk: &AccountPublicKey,
) -> Session {
    let transfer_bytecode = include_bytes!(
        "../../../target/dusk/wasm64-unknown-unknown/release/transfer_contract.wasm"
    );
    let alice_bytecode = include_bytes!(
        "../../../target/dusk/wasm32-unknown-unknown/release/alice.wasm"
    );
    let bob_bytecode = include_bytes!(
        "../../../target/dusk/wasm32-unknown-unknown/release/bob.wasm"
    );

    let mut session = rusk_abi::new_genesis_session(vm, CHAIN_ID);

    session
        .deploy(
            transfer_bytecode,
            ContractData::builder()
                .owner(OWNER)
                .contract_id(TRANSFER_CONTRACT),
            GAS_LIMIT,
        )
        .expect("Deploying the transfer contract should succeed");

    session
        .deploy(
            alice_bytecode,
            ContractData::builder().owner(OWNER).contract_id(ALICE_ID),
            GAS_LIMIT,
        )
        .expect("Deploying the alice contract should succeed");

    session
        .deploy(
            bob_bytecode,
            ContractData::builder()
                .owner(OWNER)
                .contract_id(BOB_ID)
                .init_arg(&1u8),
            GAS_LIMIT,
        )
        .expect("Deploying the bob contract should succeed");

    let sender_blinder = [
        JubJubScalar::random(&mut *rng),
        JubJubScalar::random(&mut *rng),
    ];
    let genesis_note = Note::transparent(
        rng,
        phoenix_pk,
        phoenix_pk,
        PHOENIX_GENESIS_VALUE,
        sender_blinder,
    );

    // push genesis phoenix note to the contract
    session
        .call::<_, Note>(
            TRANSFER_CONTRACT,
            "push_note",
            &(0u64, genesis_note),
            GAS_LIMIT,
        )
        .expect("Pushing genesis note should succeed");

    update_root(&mut session).expect("Updating the root should succeed");

    // insert genesis moonlight account
    session
        .call::<_, ()>(
            TRANSFER_CONTRACT,
            "add_account_balance",
            &(*moonlight_pk, MOONLIGHT_GENESIS_VALUE),
            GAS_LIMIT,
        )
        .expect("Inserting genesis account should succeed");

    // sets the block height for all subsequent operations to 1
    let base = session.commit().expect("Committing should succeed");

    rusk_abi::new_session(vm, base, CHAIN_ID, 1)
        .expect("Instantiating new session should succeed")
}

#[test]
fn phoenix_transfer() {
    const TRANSFER_FEE: u64 = dusk(1.0);

    let rng = &mut StdRng::seed_from_u64(0xfeeb);

    let vm = &mut rusk_abi::new_ephemeral_vm()
        .expect("Creating ephemeral VM should work");

    let phoenix_sender_sk = PhoenixSecretKey::random(rng);
    let phoenix_sender_pk = PhoenixPublicKey::from(&phoenix_sender_sk);

    let phoenix_change_pk = phoenix_sender_pk.clone();

    let phoenix_receiver_pk =
        PhoenixPublicKey::from(&PhoenixSecretKey::random(rng));

    let moonlight_sk = AccountSecretKey::random(rng);
    let moonlight_pk = AccountPublicKey::from(&moonlight_sk);

    let session = &mut instantiate(rng, vm, &phoenix_sender_pk, &moonlight_pk);

    let leaves = leaves_from_height(session, 0)
        .expect("Getting leaves in the given range should succeed");

    assert_eq!(leaves.len(), 1, "There should be one note in the state");

    let total_num_notes =
        num_notes(session).expect("Getting num_notes should succeed");
    assert_eq!(
        total_num_notes,
        leaves.last().expect("note to exists").note.pos() + 1,
        "num_notes should match position of last note + 1"
    );

    // create the transaction
    let gas_limit = TRANSFER_FEE;
    let gas_price = LUX;
    let input_note_pos = 0;
    let transfer_value = 42;
    let is_obfuscated = true;
    let deposit = 0;
    let contract_call: Option<ContractCall> = None;

    let tx = create_phoenix_transaction(
        rng,
        session,
        &phoenix_sender_sk,
        &phoenix_change_pk,
        &phoenix_receiver_pk,
        gas_limit,
        gas_price,
        [input_note_pos],
        transfer_value,
        is_obfuscated,
        deposit,
        contract_call,
    );

    let gas_spent = execute(session, tx)
        .expect("Executing TX should succeed")
        .gas_spent;
    update_root(session).expect("Updating the root should succeed");

    println!("EXECUTE_1_2 : {} gas", gas_spent);

    let leaves = leaves_from_height(session, 1)
        .expect("Getting the notes should succeed");
    assert_eq!(
        leaves.len(),
        3,
        "There should be three notes in the tree at this block height"
    );

    let amount_notes =
        num_notes(session).expect("Getting num_notes should succeed");
    assert_eq!(
        amount_notes,
        leaves.last().expect("note to exists").note.pos() + 1,
        "num_notes should match position of last note + 1"
    );

    let leaves = leaves_from_pos(session, input_note_pos + 1)
        .expect("Getting the notes should succeed");
    assert_eq!(
        leaves.len(),
        3,
        "There should be three notes in the tree at this block height"
    );
}

#[test]
fn phoenix_transfer_gas_fails() {
    const TRANSFER_FEE: u64 = dusk(1.0);

    let rng = &mut StdRng::seed_from_u64(0xfeeb);

    let vm = &mut rusk_abi::new_ephemeral_vm()
        .expect("Creating ephemeral VM should work");

    let phoenix_sender_sk = PhoenixSecretKey::random(rng);
    let phoenix_sender_pk = PhoenixPublicKey::from(&phoenix_sender_sk);

    let phoenix_change_pk = phoenix_sender_pk.clone();

    let phoenix_receiver_pk =
        PhoenixPublicKey::from(&PhoenixSecretKey::random(rng));

    let moonlight_sk = AccountSecretKey::random(rng);
    let moonlight_pk = AccountPublicKey::from(&moonlight_sk);

    let session = &mut instantiate(rng, vm, &phoenix_sender_pk, &moonlight_pk);

    let leaves = leaves_from_height(session, 0)
        .expect("Getting leaves in the given range should succeed");

    assert_eq!(leaves.len(), 1, "There should be one note in the state");

    let total_num_notes =
        num_notes(session).expect("Getting num_notes should succeed");
    assert_eq!(
        total_num_notes,
        leaves.last().expect("note to exist").note.pos() + 1,
        "num_notes should match position of last note + 1"
    );

    let gas_limit = TRANSFER_FEE;
    let gas_price = 0;
    let input_note_pos = 0;
    let transfer_value = 42;
    let is_obfuscated = true;
    let deposit = 0;
    let contract_call: Option<ContractCall> = None;

    let tx = create_phoenix_transaction(
        rng,
        session,
        &phoenix_sender_sk,
        &phoenix_change_pk,
        &phoenix_receiver_pk,
        gas_limit,
        gas_price,
        [input_note_pos],
        transfer_value,
        is_obfuscated,
        deposit,
        contract_call,
    );

    let result = execute(session, tx);

    assert!(
        result.is_err(),
        "Transaction should fail due to zero gas price"
    );

    // After the failed transaction, verify the state is unchanged
    let leaves_after_fail = leaves_from_height(session, 0)
        .expect("Getting the leaves should succeed after failed transaction");

    assert_eq!(
        leaves_after_fail.len(),
        1, // The number of leaves should remain the same
        "There should still be one note after the failed transaction"
    );

    let total_num_notes_after_fail =
        num_notes(session).expect("Getting num_notes should succeed");

    assert_eq!(
        total_num_notes_after_fail,
        total_num_notes, // The total number of notes should not have increased
        "num_notes should not increase due to the failed transaction"
    );
}

#[test]
fn moonlight_transfer() {
    const TRANSFER_VALUE: u64 = dusk(1.0);

    let rng = &mut StdRng::seed_from_u64(0xfeeb);

    let vm = &mut rusk_abi::new_ephemeral_vm()
        .expect("Creating ephemeral VM should work");

    let phoenix_pk = PhoenixPublicKey::from(&PhoenixSecretKey::random(rng));

    let moonlight_sender_sk = AccountSecretKey::random(rng);
    let moonlight_sender_pk = AccountPublicKey::from(&moonlight_sender_sk);

    let moonlight_receiver_pk =
        AccountPublicKey::from(&AccountSecretKey::random(rng));

    let session = &mut instantiate(rng, vm, &phoenix_pk, &moonlight_sender_pk);

    let sender_account = account(session, &moonlight_sender_pk)
        .expect("Getting the sender account should succeed");
    let receiver_account = account(session, &moonlight_receiver_pk)
        .expect("Getting the receiver account should succeed");

    assert_eq!(
        sender_account.balance, MOONLIGHT_GENESIS_VALUE,
        "The sender account should have the genesis value"
    );
    assert_eq!(
        receiver_account.balance, 0,
        "The receiver account should be empty"
    );

    let chain_id =
        chain_id(session).expect("Getting the chain ID should succeed");

    let transaction = MoonlightTransaction::new(
        &moonlight_sender_sk,
        Some(moonlight_receiver_pk),
        TRANSFER_VALUE,
        0,
        GAS_LIMIT,
        LUX,
        sender_account.nonce + 1,
        chain_id,
        None::<TransactionData>,
    )
    .expect("Creating moonlight transaction should succeed");

    let gas_spent = execute(session, transaction)
        .expect("Transaction should succeed")
        .gas_spent;

    println!("MOONLIGHT TRANSFER: {} gas", gas_spent);

    let sender_account = account(session, &moonlight_sender_pk)
        .expect("Getting the sender account should succeed");
    let receiver_account = account(session, &moonlight_receiver_pk)
        .expect("Getting the receiver account should succeed");

    assert_eq!(
        sender_account.balance,
        MOONLIGHT_GENESIS_VALUE - gas_spent - TRANSFER_VALUE,
        "The sender account should decrease by the amount spent"
    );
    assert_eq!(
        receiver_account.balance, TRANSFER_VALUE,
        "The receiver account should have the transferred value"
    );
}

#[test]
fn moonlight_transfer_with_refund() {
    const TRANSFER_VALUE: u64 = dusk(1.0);

    let rng = &mut StdRng::seed_from_u64(0xfeeb);

    let vm = &mut rusk_abi::new_ephemeral_vm()
        .expect("Creating ephemeral VM should work");

    let phoenix_pk = PhoenixPublicKey::from(&PhoenixSecretKey::random(rng));

    let moonlight_sender_sk = AccountSecretKey::random(rng);
    let moonlight_sender_pk = AccountPublicKey::from(&moonlight_sender_sk);

    let moonlight_refund_pk =
        AccountPublicKey::from(&AccountSecretKey::random(rng));

    let moonlight_receiver_pk =
        AccountPublicKey::from(&AccountSecretKey::random(rng));

    let session = &mut instantiate(rng, vm, &phoenix_pk, &moonlight_sender_pk);

    let sender_account = account(session, &moonlight_sender_pk)
        .expect("Getting the sender account should succeed");
    let receiver_account = account(session, &moonlight_receiver_pk)
        .expect("Getting the receiver account should succeed");

    assert_eq!(
        sender_account.balance, MOONLIGHT_GENESIS_VALUE,
        "The sender account should have the genesis value"
    );
    assert_eq!(
        receiver_account.balance, 0,
        "The receiver account should be empty"
    );

    let chain_id =
        chain_id(session).expect("Getting the chain ID should succeed");

    let transaction = MoonlightTransaction::new_with_refund(
        &moonlight_sender_sk,
        &moonlight_refund_pk,
        Some(moonlight_receiver_pk),
        TRANSFER_VALUE,
        0,
        GAS_LIMIT,
        LUX,
        sender_account.nonce + 1,
        chain_id,
        None::<TransactionData>,
    )
    .expect("Creating moonlight transaction should succeed");

    let max_gas = GAS_LIMIT * LUX;
    let gas_spent = execute(session, transaction)
        .expect("Transaction should succeed")
        .gas_spent;
    let gas_refund = max_gas - gas_spent;

    println!("MOONLIGHT TRANSFER: {} gas", gas_spent);

    let sender_account = account(session, &moonlight_sender_pk)
        .expect("Getting the sender account should succeed");
    let refund_account = account(session, &moonlight_refund_pk)
        .expect("Getting the refund account should succeed");
    let receiver_account = account(session, &moonlight_receiver_pk)
        .expect("Getting the receiver account should succeed");

    assert_eq!(
        sender_account.balance,
        MOONLIGHT_GENESIS_VALUE - max_gas - TRANSFER_VALUE,
        "The sender account should decrease by the amount spent"
    );
    assert_eq!(
        refund_account.balance, gas_refund,
        "The sender account should decrease by the amount spent"
    );
    assert_eq!(
        receiver_account.balance, TRANSFER_VALUE,
        "The receiver account should have the transferred value"
    );
}

#[test]
fn moonlight_transfer_gas_fails() {
    const TRANSFER_VALUE: u64 = dusk(1.0);

    let rng = &mut StdRng::seed_from_u64(0xfeeb);

    let vm = &mut rusk_abi::new_ephemeral_vm()
        .expect("Creating ephemeral VM should work");

    let phoenix_pk = PhoenixPublicKey::from(&PhoenixSecretKey::random(rng));

    let moonlight_sender_sk = AccountSecretKey::random(rng);
    let moonlight_sender_pk = AccountPublicKey::from(&moonlight_sender_sk);

    let moonlight_receiver_pk =
        AccountPublicKey::from(&AccountSecretKey::random(rng));

    let session = &mut instantiate(rng, vm, &phoenix_pk, &moonlight_sender_pk);

    let sender_account = account(session, &moonlight_sender_pk)
        .expect("Getting the sender account should succeed");
    let receiver_account = account(session, &moonlight_receiver_pk)
        .expect("Getting the receiver account should succeed");

    assert_eq!(
        sender_account.balance, MOONLIGHT_GENESIS_VALUE,
        "The sender account should have the genesis value"
    );
    assert_eq!(
        receiver_account.balance, 0,
        "The receiver account should be empty"
    );

    let chain_id =
        chain_id(session).expect("Getting the chain ID should succeed");

    let transaction = MoonlightTransaction::new(
        &moonlight_sender_sk,
        Some(moonlight_receiver_pk),
        TRANSFER_VALUE,
        0,
        GAS_LIMIT,
        0,
        sender_account.nonce + 1,
        chain_id,
        None::<TransactionData>,
    )
    .expect("Creating moonlight transaction should succeed");

    let result = execute(session, transaction);

    assert!(
        result.is_err(),
        "Transaction should fail due to zero gas price"
    );

    // Since the transaction failed, balances should remain the same
    let sender_account = account(session, &moonlight_sender_pk)
        .expect("Getting the sender account should succeed");
    let receiver_account = account(session, &moonlight_receiver_pk)
        .expect("Getting the receiver account should succeed");

    assert_eq!(
        sender_account.balance, MOONLIGHT_GENESIS_VALUE,
        "The sender account should still have the genesis value"
    );
    assert_eq!(
        receiver_account.balance, 0,
        "The receiver account should still be empty"
    );
}

#[test]
fn phoenix_alice_ping() {
    const PING_FEE: u64 = dusk(1.0);

    let rng = &mut StdRng::seed_from_u64(0xfeeb);

    let vm = &mut rusk_abi::new_ephemeral_vm()
        .expect("Creating ephemeral VM should work");

    let phoenix_sender_sk = PhoenixSecretKey::random(rng);
    let phoenix_sender_pk = PhoenixPublicKey::from(&phoenix_sender_sk);

    let phoenix_change_pk = phoenix_sender_pk.clone();

    let moonlight_sk = AccountSecretKey::random(rng);
    let moonlight_pk = AccountPublicKey::from(&moonlight_sk);

    let session = &mut instantiate(rng, vm, &phoenix_sender_pk, &moonlight_pk);

    let leaves = leaves_from_height(session, 0)
        .expect("Getting leaves in the given range should succeed");

    assert_eq!(leaves.len(), 1, "There should be one note in the state");

    // create the transaction
    let gas_limit = PING_FEE;
    let gas_price = LUX;
    let input_note_pos = 0;
    let transfer_value = 0;
    let is_obfuscated = false;
    let deposit = 0;
    let contract_call = Some(ContractCall {
        contract: ALICE_ID,
        fn_name: String::from("ping"),
        fn_args: vec![],
    });

    let tx = create_phoenix_transaction(
        rng,
        session,
        &phoenix_sender_sk,
        &phoenix_change_pk,
        &phoenix_sender_pk,
        gas_limit,
        gas_price,
        [input_note_pos],
        transfer_value,
        is_obfuscated,
        deposit,
        contract_call,
    );

    let gas_spent = execute(session, tx)
        .expect("Executing TX should succeed")
        .gas_spent;
    update_root(session).expect("Updating the root should succeed");

    println!("EXECUTE_PING: {} gas", gas_spent);

    let leaves = leaves_from_height(session, 1)
        .expect("Getting the notes should succeed");
    assert_eq!(
        leaves.len(),
        // since the transfer value is a transparent note with value 0 there is
        // only the change note added to the tree
        2,
        "There should be two notes in the tree after the transaction"
    );
}

#[test]
fn moonlight_alice_ping() {
    let rng = &mut StdRng::seed_from_u64(0xfeeb);

    let vm = &mut rusk_abi::new_ephemeral_vm()
        .expect("Creating ephemeral VM should work");

    let phoenix_pk = PhoenixPublicKey::from(&PhoenixSecretKey::random(rng));

    let moonlight_sk = AccountSecretKey::random(rng);
    let moonlight_pk = AccountPublicKey::from(&moonlight_sk);

    let session = &mut instantiate(rng, vm, &phoenix_pk, &moonlight_pk);

    let acc = account(session, &moonlight_pk)
        .expect("Getting the sender account should succeed");

    let contract_call = Some(ContractCall {
        contract: ALICE_ID,
        fn_name: String::from("ping"),
        fn_args: vec![],
    });

    assert_eq!(
        acc.balance, MOONLIGHT_GENESIS_VALUE,
        "The account should have the genesis value"
    );

    let chain_id =
        chain_id(session).expect("Getting the chain ID should succeed");

    let transaction = MoonlightTransaction::new(
        &moonlight_sk,
        None,
        0,
        0,
        GAS_LIMIT,
        LUX,
        acc.nonce + 1,
        chain_id,
        contract_call,
    )
    .expect("Creating moonlight transaction should succeed");

    let gas_spent = execute(session, transaction)
        .expect("Transaction should succeed")
        .gas_spent;

    println!("MOONLIGHT PING: {} gas", gas_spent);

    let acc = account(session, &moonlight_pk)
        .expect("Getting the account should succeed");

    assert_eq!(
        acc.balance,
        MOONLIGHT_GENESIS_VALUE - gas_spent,
        "The account should decrease by the amount spent"
    );
}

#[test]
fn phoenix_deposit_and_withdraw() {
    const DEPOSIT_FEE: u64 = dusk(1.0);
    const WITHDRAW_FEE: u64 = dusk(1.0);

    let rng = &mut StdRng::seed_from_u64(0xfeeb);

    let vm = &mut rusk_abi::new_ephemeral_vm()
        .expect("Creating ephemeral VM should work");

    let phoenix_sender_sk = PhoenixSecretKey::random(rng);
    let phoenix_sender_vk = PhoenixViewKey::from(&phoenix_sender_sk);
    let phoenix_sender_pk = PhoenixPublicKey::from(&phoenix_sender_sk);

    let phoenix_change_pk = phoenix_sender_pk.clone();

    let moonlight_sk = AccountSecretKey::random(rng);
    let moonlight_pk = AccountPublicKey::from(&moonlight_sk);

    let session = &mut instantiate(rng, vm, &phoenix_sender_pk, &moonlight_pk);

    let leaves = leaves_from_height(session, 0)
        .expect("Getting leaves in the given range should succeed");

    assert_eq!(leaves.len(), 1, "There should be one note in the state");

    // create the deposit transaction
    let gas_limit = DEPOSIT_FEE;
    let gas_price = LUX;
    let input_note_pos = 0;
    let transfer_value = 0;
    let is_obfuscated = false;
    let deposit_value = PHOENIX_GENESIS_VALUE / 2;
    let contract_call = Some(ContractCall {
        contract: ALICE_ID,
        fn_name: String::from("deposit"),
        fn_args: deposit_value.to_bytes().into(),
    });

    let tx = create_phoenix_transaction(
        rng,
        session,
        &phoenix_sender_sk,
        &phoenix_change_pk,
        &phoenix_sender_pk,
        gas_limit,
        gas_price,
        [input_note_pos],
        transfer_value,
        is_obfuscated,
        deposit_value,
        contract_call,
    );

    let gas_spent = execute(session, tx.clone())
        .expect("Executing TX should succeed")
        .gas_spent;
    update_root(session).expect("Updating the root should succeed");

    println!("EXECUTE_DEPOSIT: {} gas", gas_spent);

    let leaves = leaves_from_height(session, 1)
        .expect("getting the notes should succeed");
    assert_eq!(
        PHOENIX_GENESIS_VALUE,
        transfer_value
            + tx.deposit()
            + tx.max_fee()
            + tx.outputs()[1]
                .value(Some(&PhoenixViewKey::from(&phoenix_sender_sk)))
                .unwrap()
    );
    assert_eq!(
        leaves.len(),
        // since the transfer value is a transparent note with value 0 there is
        // only the change note added to the tree
        2,
        "There should be two notes in the tree at this block height"
    );

    // the alice contract has the correct balance

    let alice_balance = contract_balance(session, ALICE_ID)
        .expect("Querying the contract balance should succeed");
    assert_eq!(
        alice_balance, deposit_value,
        "Alice should have the value of the input crossover"
    );

    // start withdrawing the amount just transferred to the alice contract
    // this is done by calling the alice contract directly, which then calls the
    // transfer contract

    let input_notes = filter_notes_owned_by(
        phoenix_sender_vk,
        leaves.into_iter().map(|leaf| leaf.note),
    );

    assert_eq!(
        input_notes.len(),
        2,
        "All new notes should be owned by our view key"
    );

    let address =
        phoenix_sender_pk.gen_stealth_address(&JubJubScalar::random(&mut *rng));
    let note_sk = phoenix_sender_sk.gen_note_sk(&address);

    let withdraw = Withdraw::new(
        rng,
        &note_sk,
        ALICE_ID,
        PHOENIX_GENESIS_VALUE / 2,
        WithdrawReceiver::Phoenix(address),
        WithdrawReplayToken::Phoenix(vec![
            input_notes[0].gen_nullifier(&phoenix_sender_sk),
            input_notes[1].gen_nullifier(&phoenix_sender_sk),
        ]),
    );

    let gas_limit = WITHDRAW_FEE;
    let gas_price = LUX;
    let input_notes_pos = [*input_notes[0].pos(), *input_notes[1].pos()];
    let transfer_value = 0;
    let is_obfuscated = false;
    let deposit_value = 0;
    let contract_call = Some(ContractCall {
        contract: ALICE_ID,
        fn_name: String::from("withdraw"),
        fn_args: rkyv::to_bytes::<_, 1024>(&withdraw)
            .expect("should serialize Mint correctly")
            .to_vec(),
    });

    let tx = create_phoenix_transaction(
        rng,
        session,
        &phoenix_sender_sk,
        &phoenix_change_pk,
        &phoenix_sender_pk,
        gas_limit,
        gas_price,
        input_notes_pos.try_into().unwrap(),
        transfer_value,
        is_obfuscated,
        deposit_value,
        contract_call,
    );

    let gas_spent = execute(session, tx)
        .expect("Executing TX should succeed")
        .gas_spent;
    update_root(session).expect("Updating the root should succeed");

    println!("EXECUTE_WITHDRAW: {} gas", gas_spent);

    let alice_balance = contract_balance(session, ALICE_ID)
        .expect("Querying the contract balance should succeed");
    assert_eq!(
        alice_balance, 0,
        "Alice should have no balance after it is withdrawn"
    );
}

#[test]
fn phoenix_to_moonlight_swap() {
    const SWAP_VALUE: u64 = dusk(1.0);

    let rng = &mut StdRng::seed_from_u64(0xfeeb);

    let phoenix_sender_sk = PhoenixSecretKey::random(rng);
    let phoenix_sender_vk = PhoenixViewKey::from(&phoenix_sender_sk);
    let phoenix_sender_pk = PhoenixPublicKey::from(&phoenix_sender_sk);

    let phoenix_change_pk = phoenix_sender_pk.clone();

    let moonlight_sk = AccountSecretKey::random(rng);
    let moonlight_pk = AccountPublicKey::from(&moonlight_sk);

    let vm = &mut rusk_abi::new_ephemeral_vm()
        .expect("Creating ephemeral VM should work");
    let mut session =
        &mut instantiate(rng, vm, &phoenix_sender_pk, &moonlight_pk);

    let swapper_account = account(&mut session, &moonlight_pk)
        .expect("Getting account should succeed");

    assert_eq!(
        swapper_account.balance, MOONLIGHT_GENESIS_VALUE,
        "The swapper's account should have the genesis value"
    );

    let leaves = leaves_from_height(session, 0)
        .expect("getting the notes should succeed");
    let notes = filter_notes_owned_by(
        phoenix_sender_vk,
        leaves.into_iter().map(|leaf| leaf.note),
    );

    assert_eq!(notes.len(), 1, "There should be one note at this height");

    let convert = Withdraw::new(
        rng,
        &moonlight_sk,
        TRANSFER_CONTRACT,
        SWAP_VALUE,
        WithdrawReceiver::Moonlight(moonlight_pk),
        WithdrawReplayToken::Phoenix(vec![
            notes[0].gen_nullifier(&phoenix_sender_sk)
        ]),
    );

    let contract_call = ContractCall {
        contract: TRANSFER_CONTRACT,
        fn_name: String::from("convert"),
        fn_args: rkyv::to_bytes::<_, 1024>(&convert)
            .expect("should serialize conversion correctly")
            .to_vec(),
    };

    let tx = create_phoenix_transaction(
        rng,
        session,
        &phoenix_sender_sk,
        &phoenix_change_pk,
        &phoenix_sender_pk,
        GAS_LIMIT,
        LUX,
        [0],
        0,
        false,
        SWAP_VALUE,
        Some(contract_call),
    );

    let gas_spent = execute(session, tx)
        .expect("Executing TX should succeed")
        .gas_spent;
    update_root(session).expect("Updating the root should succeed");

    println!("CONVERT phoenix to moonlight: {} gas", gas_spent);

    let swapper_account = account(&mut session, &moonlight_pk)
        .expect("Getting account should succeed");

    assert_eq!(
        swapper_account.balance,
        MOONLIGHT_GENESIS_VALUE + SWAP_VALUE,
        "The swapper's account should have swap value added"
    );

    let leaves = leaves_from_height(session, 1)
        .expect("getting the notes should succeed");
    let notes = filter_notes_owned_by(
        phoenix_sender_vk,
        leaves.into_iter().map(|leaf| leaf.note),
    );
    let notes_value = owned_notes_value(phoenix_sender_vk, &notes);

    assert_eq!(
        notes.len(),
        2,
        "New notes should have been created as change and refund (transparent notes with the value 0 are not appended to the tree)"
    );
    assert_eq!(
        notes_value,
        PHOENIX_GENESIS_VALUE - gas_spent - SWAP_VALUE,
        "The new notes should have the original value minus the swapped value and gas spent"
    );
}

#[test]
fn moonlight_to_phoenix_swap() {
    const SWAP_VALUE: u64 = dusk(1.0);

    let rng = &mut StdRng::seed_from_u64(0xfeeb);

    let phoenix_sk = PhoenixSecretKey::random(rng);
    let phoenix_vk = PhoenixViewKey::from(&phoenix_sk);
    let phoenix_pk = PhoenixPublicKey::from(&phoenix_sk);

    let moonlight_sk = AccountSecretKey::random(rng);
    let moonlight_pk = AccountPublicKey::from(&moonlight_sk);

    let vm = &mut rusk_abi::new_ephemeral_vm()
        .expect("Creating ephemeral VM should work");
    let mut session = &mut instantiate(rng, vm, &phoenix_pk, &moonlight_pk);

    let swapper_account = account(&mut session, &moonlight_pk)
        .expect("Getting account should succeed");
    let nonce = swapper_account.nonce + 1;

    assert_eq!(
        swapper_account.balance, MOONLIGHT_GENESIS_VALUE,
        "The swapper's account should have the genesis value"
    );

    let leaves = leaves_from_height(session, 1)
        .expect("getting the notes should succeed");
    let notes = filter_notes_owned_by(
        phoenix_vk,
        leaves.into_iter().map(|leaf| leaf.note),
    );

    assert_eq!(notes.len(), 0, "There should be no notes at this height");

    let address =
        phoenix_pk.gen_stealth_address(&JubJubScalar::random(&mut *rng));
    let note_sk = phoenix_sk.gen_note_sk(&address);

    let convert = Withdraw::new(
        rng,
        &note_sk,
        TRANSFER_CONTRACT,
        SWAP_VALUE,
        WithdrawReceiver::Phoenix(address),
        WithdrawReplayToken::Moonlight(nonce),
    );

    let contract_call = ContractCall {
        contract: TRANSFER_CONTRACT,
        fn_name: String::from("convert"),
        fn_args: rkyv::to_bytes::<_, 1024>(&convert)
            .expect("should serialize conversion correctly")
            .to_vec(),
    };

    let chain_id =
        chain_id(session).expect("Getting the chain ID should succeed");

    let tx = MoonlightTransaction::new(
        &moonlight_sk,
        None,
        0,
        SWAP_VALUE,
        GAS_LIMIT,
        LUX,
        nonce,
        chain_id,
        Some(contract_call),
    )
    .expect("Creating moonlight transaction should succeed");

    let gas_spent = execute(&mut session, tx)
        .expect("Executing transaction should succeed")
        .gas_spent;
    update_root(session).expect("Updating the root should succeed");

    println!("CONVERT moonlight to phoenix: {} gas", gas_spent);

    let swapper_account = account(&mut session, &moonlight_pk)
        .expect("Getting account should succeed");

    assert_eq!(
        swapper_account.balance,
        MOONLIGHT_GENESIS_VALUE - gas_spent - SWAP_VALUE,
        "The swapper's account should have had the swap value subtracted along with gas spent"
    );

    let leaves = leaves_from_height(session, 1)
        .expect("getting the notes should succeed");
    let notes = filter_notes_owned_by(
        phoenix_vk,
        leaves.into_iter().map(|leaf| leaf.note),
    );
    let notes_value = owned_notes_value(phoenix_vk, &notes);

    assert_eq!(notes.len(), 1, "A new note should have been created");
    assert_eq!(
        notes_value, SWAP_VALUE,
        "The new note should have the swapped value",
    );
}

#[test]
fn swap_wrong_contract_targeted() {
    const SWAP_VALUE: u64 = dusk(1.0);

    let rng = &mut StdRng::seed_from_u64(0xfeeb);

    let phoenix_sk = PhoenixSecretKey::random(rng);
    let phoenix_vk = PhoenixViewKey::from(&phoenix_sk);
    let phoenix_pk = PhoenixPublicKey::from(&phoenix_sk);

    let moonlight_sk = AccountSecretKey::random(rng);
    let moonlight_pk = AccountPublicKey::from(&moonlight_sk);

    let vm = &mut rusk_abi::new_ephemeral_vm()
        .expect("Creating ephemeral VM should work");
    let mut session = &mut instantiate(rng, vm, &phoenix_pk, &moonlight_pk);

    let swapper_account = account(&mut session, &moonlight_pk)
        .expect("Getting account should succeed");
    let nonce = swapper_account.nonce + 1;

    assert_eq!(
        swapper_account.balance, MOONLIGHT_GENESIS_VALUE,
        "The swapper's account should have the genesis value"
    );

    let leaves = leaves_from_height(session, 1)
        .expect("getting the notes should succeed");
    let notes = filter_notes_owned_by(
        phoenix_vk,
        leaves.into_iter().map(|leaf| leaf.note),
    );

    assert_eq!(notes.len(), 0, "There should be no notes at this height");

    let address =
        phoenix_pk.gen_stealth_address(&JubJubScalar::random(&mut *rng));
    let note_sk = phoenix_sk.gen_note_sk(&address);

    let convert = Withdraw::new(
        rng,
        &note_sk,
        ALICE_ID, /* this should be the transfer contract, but
                   * we're testing the "wrong target" case */
        SWAP_VALUE,
        WithdrawReceiver::Phoenix(address),
        WithdrawReplayToken::Moonlight(nonce),
    );

    let contract_call = ContractCall {
        contract: TRANSFER_CONTRACT,
        fn_name: String::from("convert"),
        fn_args: rkyv::to_bytes::<_, 1024>(&convert)
            .expect("should serialize conversion correctly")
            .to_vec(),
    };

    let chain_id =
        chain_id(session).expect("Getting the chain ID should succeed");

    let tx = MoonlightTransaction::new(
        &moonlight_sk,
        None,
        0,
        SWAP_VALUE,
        GAS_LIMIT,
        LUX,
        nonce,
        chain_id,
        Some(contract_call),
    )
    .expect("Creating moonlight transaction should succeed");

    let receipt = execute(&mut session, tx)
        .expect("Executing transaction should succeed");
    update_root(session).expect("Updating the root should succeed");

    let res = receipt.data;
    let gas_spent = receipt.gas_spent;

    assert!(matches!(res, Err(_)), "The contract call should error");

    let swapper_account = account(&mut session, &moonlight_pk)
        .expect("Getting account should succeed");

    assert_eq!(
        swapper_account.balance,
        MOONLIGHT_GENESIS_VALUE - gas_spent,
        "The swapper's account should have only the gas spent subtracted"
    );

    let leaves = leaves_from_height(session, 1)
        .expect("getting the notes should succeed");
    let notes = filter_notes_owned_by(
        phoenix_vk,
        leaves.into_iter().map(|leaf| leaf.note),
    );

    assert!(notes.is_empty(), "A new note should not been created");
}

/// In this test we deposit some Dusk to the Alice contract, and subsequently
/// proceed to call Alice's `contract_to_contract` function, targeting Bob as
/// the receiver of the transfer.
#[test]
fn contract_to_contract() {
    const DEPOSIT_VALUE: u64 = MOONLIGHT_GENESIS_VALUE / 2;
    const TRANSFER_VALUE: u64 = DEPOSIT_VALUE / 2;

    let rng = &mut StdRng::seed_from_u64(0xfeeb);

    let vm = &mut rusk_abi::new_ephemeral_vm()
        .expect("Creating ephemeral VM should work");

    let phoenix_pk = PhoenixPublicKey::from(&PhoenixSecretKey::random(rng));

    let moonlight_sk = AccountSecretKey::random(rng);
    let moonlight_pk = AccountPublicKey::from(&moonlight_sk);

    let session = &mut instantiate(rng, vm, &phoenix_pk, &moonlight_pk);

    let acc = account(session, &moonlight_pk)
        .expect("Getting the account should succeed");
    let alice_balance = contract_balance(session, ALICE_ID)
        .expect("Querying the contract balance should succeed");
    let bob_balance = contract_balance(session, BOB_ID)
        .expect("Querying the contract balance should succeed");

    assert_eq!(
        acc.balance, MOONLIGHT_GENESIS_VALUE,
        "The depositer account should have the genesis value"
    );
    assert_eq!(
        alice_balance, 0,
        "Alice must have an initial balance of zero"
    );
    assert_eq!(bob_balance, 0, "Bob must have an initial balance of zero");

    let fn_args = rkyv::to_bytes::<_, 256>(&DEPOSIT_VALUE)
        .expect("Serializing should succeed")
        .to_vec();
    let contract_call = Some(ContractCall {
        contract: ALICE_ID,
        fn_name: String::from("deposit"),
        fn_args,
    });

    let chain_id =
        chain_id(session).expect("Getting the chain ID should succeed");

    let transaction = MoonlightTransaction::new(
        &moonlight_sk,
        None,
        0,
        DEPOSIT_VALUE,
        GAS_LIMIT,
        LUX,
        acc.nonce + 1,
        chain_id,
        contract_call,
    )
    .expect("Creating moonlight transaction should succeed");

    let receipt =
        execute(session, transaction).expect("Transaction should succeed");
    let gas_spent_deposit = receipt.gas_spent;

    println!("MOONLIGHT DEPOSIT: {:?}", receipt.data);
    println!("MOONLIGHT DEPOSIT: {gas_spent_deposit} gas");

    let acc = account(session, &moonlight_pk)
        .expect("Getting the account should succeed");
    let alice_balance = contract_balance(session, ALICE_ID)
        .expect("Querying the contract balance should succeed");
    let bob_balance = contract_balance(session, BOB_ID)
        .expect("Querying the contract balance should succeed");

    assert_eq!(
        acc.balance,
        MOONLIGHT_GENESIS_VALUE - gas_spent_deposit - DEPOSIT_VALUE,
        "The account should decrease by the amount spent and the deposit sent"
    );
    assert_eq!(
        alice_balance, DEPOSIT_VALUE,
        "Alice must have the deposit in their balance"
    );
    assert_eq!(bob_balance, 0, "Bob must have a balance of zero");

    let transfer = ContractToContract {
        contract: BOB_ID,
        value: TRANSFER_VALUE,
        fn_name: String::from("recv_transfer"),
        data: vec![],
    };
    let fn_args = rkyv::to_bytes::<_, 256>(&transfer)
        .expect("Serializing should succeed")
        .to_vec();
    let contract_call = Some(ContractCall {
        contract: ALICE_ID,
        fn_name: String::from("contract_to_contract"),
        fn_args,
    });

    let transaction = MoonlightTransaction::new(
        &moonlight_sk,
        None,
        0,
        0,
        GAS_LIMIT,
        LUX,
        acc.nonce + 1,
        chain_id,
        contract_call,
    )
    .expect("Creating moonlight transaction should succeed");

    let receipt =
        execute(session, transaction).expect("Transaction should succeed");
    let gas_spent_send = receipt.gas_spent;

    println!("MOONLIGHT SEND_TO_CONTRACT: {:?}", receipt.data);
    println!("MOONLIGHT SEND_TO_CONTRACT: {gas_spent_send} gas");

    let acc = account(session, &moonlight_pk)
        .expect("Getting the account should succeed");
    let alice_balance = contract_balance(session, ALICE_ID)
        .expect("Querying the contract balance should succeed");
    let bob_balance = contract_balance(session, BOB_ID)
        .expect("Querying the contract balance should succeed");

    assert_eq!(
        acc.balance,
        MOONLIGHT_GENESIS_VALUE
            - gas_spent_deposit
            - gas_spent_send
            - DEPOSIT_VALUE,
        "The account should decrease by the amount spent and the deposit sent"
    );
    assert_eq!(
        alice_balance, DEPOSIT_VALUE - TRANSFER_VALUE,
        "Alice must have the deposit minus the transferred amount in their balance"
    );
    assert_eq!(
        bob_balance, TRANSFER_VALUE,
        "Bob must have the transfer value as balance"
    );
}

/// In this test we deposit some Dusk from a moonlight account to the Alice
/// contract, and subsequently call the Alice contract to trigger a transfer
/// back to the same account.
#[test]
fn contract_to_account() {
    const DEPOSIT_VALUE: u64 = MOONLIGHT_GENESIS_VALUE / 2;
    const TRANSFER_VALUE: u64 = DEPOSIT_VALUE / 2;

    let rng = &mut StdRng::seed_from_u64(0xfeeb);

    let vm = &mut rusk_abi::new_ephemeral_vm()
        .expect("Creating ephemeral VM should work");

    let phoenix_pk = PhoenixPublicKey::from(&PhoenixSecretKey::random(rng));

    let moonlight_sk = AccountSecretKey::random(rng);
    let moonlight_pk = AccountPublicKey::from(&moonlight_sk);

    let session = &mut instantiate(rng, vm, &phoenix_pk, &moonlight_pk);

    let acc = account(session, &moonlight_pk)
        .expect("Getting the account should succeed");
    let alice_balance = contract_balance(session, ALICE_ID)
        .expect("Querying the contract balance should succeed");

    assert_eq!(
        acc.balance, MOONLIGHT_GENESIS_VALUE,
        "The depositer account should have the genesis value"
    );
    assert_eq!(
        alice_balance, 0,
        "Alice must have an initial balance of zero"
    );

    let fn_args = rkyv::to_bytes::<_, 256>(&DEPOSIT_VALUE)
        .expect("Serializing should succeed")
        .to_vec();
    let contract_call = Some(ContractCall {
        contract: ALICE_ID,
        fn_name: String::from("deposit"),
        fn_args,
    });

    let chain_id =
        chain_id(session).expect("Getting the chain ID should succeed");

    let transaction = MoonlightTransaction::new(
        &moonlight_sk,
        None,
        0,
        DEPOSIT_VALUE,
        GAS_LIMIT,
        LUX,
        acc.nonce + 1,
        chain_id,
        contract_call,
    )
    .expect("Creating moonlight transaction should succeed");

    let receipt =
        execute(session, transaction).expect("Transaction should succeed");
    let gas_spent_deposit = receipt.gas_spent;

    println!("MOONLIGHT DEPOSIT: {:?}", receipt.data);
    println!("MOONLIGHT DEPOSIT: {gas_spent_deposit} gas");

    let acc = account(session, &moonlight_pk)
        .expect("Getting the account should succeed");
    let alice_balance = contract_balance(session, ALICE_ID)
        .expect("Querying the contract balance should succeed");

    assert_eq!(
        acc.balance,
        MOONLIGHT_GENESIS_VALUE - gas_spent_deposit - DEPOSIT_VALUE,
        "The account should decrease by the amount spent and the deposit sent"
    );
    assert_eq!(
        alice_balance, DEPOSIT_VALUE,
        "Alice must have the deposit in their balance"
    );

    let transfer = ContractToAccount {
        account: moonlight_pk,
        value: TRANSFER_VALUE,
    };
    let fn_args = rkyv::to_bytes::<_, 256>(&transfer)
        .expect("Serializing should succeed")
        .to_vec();
    let contract_call = Some(ContractCall {
        contract: ALICE_ID,
        fn_name: String::from("contract_to_account"),
        fn_args,
    });

    let transaction = MoonlightTransaction::new(
        &moonlight_sk,
        None,
        0,
        0,
        GAS_LIMIT,
        LUX,
        acc.nonce + 1,
        chain_id,
        contract_call,
    )
    .expect("Creating moonlight transaction should succeed");

    let receipt =
        execute(session, transaction).expect("Transaction should succeed");
    let gas_spent_send = receipt.gas_spent;

    println!("MOONLIGHT SEND_TO_ACCOUNT: {:?}", receipt.data);
    println!("MOONLIGHT SEND_TO_ACCOUNT: {gas_spent_send} gas");

    let acc = account(session, &moonlight_pk)
        .expect("Getting the account should succeed");
    let alice_balance = contract_balance(session, ALICE_ID)
        .expect("Querying the contract balance should succeed");

    assert_eq!(
        acc.balance,
        MOONLIGHT_GENESIS_VALUE
            - gas_spent_deposit
            - gas_spent_send
            - DEPOSIT_VALUE
            + TRANSFER_VALUE,
        "The account should decrease by the amount spent and the deposit sent, \
         and increase by the transfer"
    );
    assert_eq!(
        alice_balance, DEPOSIT_VALUE - TRANSFER_VALUE,
        "Alice must have the deposit minus the transferred amount in their balance"
    );
}

/// In this test we try to transfer some Dusk from a contract to an account,
/// when the contract doesn't have sufficient funds.
#[test]
fn contract_to_account_insufficient_funds() {
    // Transfer value larger than DEPOSIT
    const DEPOSIT_VALUE: u64 = MOONLIGHT_GENESIS_VALUE / 2;
    const TRANSFER_VALUE: u64 = 2 * DEPOSIT_VALUE;

    let rng = &mut StdRng::seed_from_u64(0xfeeb);

    let vm = &mut rusk_abi::new_ephemeral_vm()
        .expect("Creating ephemeral VM should work");

    let phoenix_pk = PhoenixPublicKey::from(&PhoenixSecretKey::random(rng));

    let moonlight_sk = AccountSecretKey::random(rng);
    let moonlight_pk = AccountPublicKey::from(&moonlight_sk);

    let session = &mut instantiate(rng, vm, &phoenix_pk, &moonlight_pk);

    let acc = account(session, &moonlight_pk)
        .expect("Getting the account should succeed");
    let alice_balance = contract_balance(session, ALICE_ID)
        .expect("Querying the contract balance should succeed");

    assert_eq!(
        acc.balance, MOONLIGHT_GENESIS_VALUE,
        "The depositer account should have the genesis value"
    );
    assert_eq!(
        alice_balance, 0,
        "Alice must have an initial balance of zero"
    );

    let fn_args = rkyv::to_bytes::<_, 256>(&DEPOSIT_VALUE)
        .expect("Serializing should succeed")
        .to_vec();
    let contract_call = Some(ContractCall {
        contract: ALICE_ID,
        fn_name: String::from("deposit"),
        fn_args,
    });

    let chain_id =
        chain_id(session).expect("Getting the chain ID should succeed");

    let transaction = MoonlightTransaction::new(
        &moonlight_sk,
        None,
        0,
        DEPOSIT_VALUE,
        GAS_LIMIT,
        LUX,
        acc.nonce + 1,
        chain_id,
        contract_call,
    )
    .expect("Creating moonlight transaction should succeed");

    let receipt =
        execute(session, transaction).expect("Transaction should succeed");
    let gas_spent_deposit = receipt.gas_spent;

    println!("MOONLIGHT DEPOSIT: {:?}", receipt.data);
    println!("MOONLIGHT DEPOSIT: {gas_spent_deposit} gas");

    let acc = account(session, &moonlight_pk)
        .expect("Getting the account should succeed");
    let alice_balance = contract_balance(session, ALICE_ID)
        .expect("Querying the contract balance should succeed");

    assert_eq!(
        acc.balance,
        MOONLIGHT_GENESIS_VALUE - gas_spent_deposit - DEPOSIT_VALUE,
        "The account should decrease by the amount spent and the deposit sent"
    );
    assert_eq!(
        alice_balance, DEPOSIT_VALUE,
        "Alice must have the deposit in their balance"
    );

    let transfer = ContractToAccount {
        account: moonlight_pk,
        value: TRANSFER_VALUE,
    };
    let fn_args = rkyv::to_bytes::<_, 256>(&transfer)
        .expect("Serializing should succeed")
        .to_vec();
    let contract_call = Some(ContractCall {
        contract: ALICE_ID,
        fn_name: String::from("contract_to_account"),
        fn_args,
    });

    let transaction = MoonlightTransaction::new(
        &moonlight_sk,
        None,
        0,
        0,
        GAS_LIMIT,
        LUX,
        acc.nonce + 1,
        chain_id,
        contract_call,
    )
    .expect("Creating moonlight transaction should succeed");

    let receipt =
        execute(session, transaction).expect("Transaction should succeed");
    let gas_spent_send = receipt.gas_spent;

    println!(
        "MOONLIGHT SEND_TO_ACCOUNT INSUFFICIENT FUNDS: {:?}",
        receipt.data
    );
    println!(
        "MOONLIGHT SEND_TO_ACCOUNT INSUFFICIENT_FUNDS: {gas_spent_send} gas"
    );

    let acc = account(session, &moonlight_pk)
        .expect("Getting the account should succeed");
    let alice_balance = contract_balance(session, ALICE_ID)
        .expect("Querying the contract balance should succeed");

    assert!(
        matches!(receipt.data, Err(_)),
        "Alice should error because the transfer contract panics"
    );
    assert_eq!(
        acc.balance,
        MOONLIGHT_GENESIS_VALUE
            - gas_spent_deposit
            - gas_spent_send
            - DEPOSIT_VALUE,
        "The account should decrease by the amount spent and the deposit sent"
    );
    assert_eq!(
        alice_balance, DEPOSIT_VALUE,
        "Alice must have the deposit amount still in their balance"
    );
}

/// In this test we try to call the function directly - i.e. not initiated by a
/// contract, but by the transaction itself.
#[test]
fn contract_to_account_direct_call() {
    const TRANSFER_VALUE: u64 = MOONLIGHT_GENESIS_VALUE / 2;

    let rng = &mut StdRng::seed_from_u64(0xfeeb);

    let vm = &mut rusk_abi::new_ephemeral_vm()
        .expect("Creating ephemeral VM should work");

    let phoenix_pk = PhoenixPublicKey::from(&PhoenixSecretKey::random(rng));

    let moonlight_sk = AccountSecretKey::random(rng);
    let moonlight_pk = AccountPublicKey::from(&moonlight_sk);

    let session = &mut instantiate(rng, vm, &phoenix_pk, &moonlight_pk);

    let acc = account(session, &moonlight_pk)
        .expect("Getting the account should succeed");

    assert_eq!(
        acc.balance, MOONLIGHT_GENESIS_VALUE,
        "The depositer account should have the genesis value"
    );

    let transfer = ContractToAccount {
        account: moonlight_pk,
        value: TRANSFER_VALUE,
    };
    let fn_args = rkyv::to_bytes::<_, 256>(&transfer)
        .expect("Serializing should succeed")
        .to_vec();
    let contract_call = Some(ContractCall {
        contract: TRANSFER_CONTRACT,
        fn_name: String::from("contract_to_account"),
        fn_args,
    });

    let chain_id =
        chain_id(session).expect("Getting the chain ID should succeed");

    let transaction = MoonlightTransaction::new(
        &moonlight_sk,
        None,
        0,
        0,
        GAS_LIMIT,
        LUX,
        acc.nonce + 1,
        chain_id,
        contract_call,
    )
    .expect("Creating moonlight transaction should succeed");

    let receipt =
        execute(session, transaction).expect("Transaction should succeed");
    let gas_spent_send = receipt.gas_spent;

    println!("MOONLIGHT SEND_TO_ACCOUNT DIRECTLY: {:?}", receipt.data);
    println!("MOONLIGHT SEND_TO_ACCOUNT DIRECTLY: {gas_spent_send} gas");

    let acc = account(session, &moonlight_pk)
        .expect("Getting the account should succeed");

    assert!(
        matches!(receipt.data, Err(ContractError::Panic(_))),
        "The transfer contract should panic on a direct call"
    );
    assert_eq!(
        acc.balance,
        MOONLIGHT_GENESIS_VALUE - gas_spent_send,
        "The account should decrease by the amount spent"
    );
}
