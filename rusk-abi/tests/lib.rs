// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) DUSK NETWORK. All rights reserved.

#![deny(clippy::all)]
#![cfg(feature = "host")]

use std::sync::OnceLock;

use rand_core::OsRng;

use dusk_bytes::{ParseHexStr, Serializable};
use dusk_plonk::prelude::*;
use execution_core::{
    BlsScalar, NotePublicKey, NoteSecretKey, PublicKey, SecretKey,
    StakePublicKey, StakeSecretKey,
};
use ff::Field;
use rusk_abi::hash::Hasher;
use rusk_abi::PublicInput;
use rusk_abi::{ContractData, ContractId, Session, VM};

const POINT_LIMIT: u64 = 0xc00000;

#[test]
fn hash_host() {
    let test_inputs = [
        "bb67ed265bf1db490ded2e1ede55c0d14c55521509dc73f9c354e98ab76c9625",
        "7e74220084d75e10c89e9435d47bb5b8075991b2e29be3b84421dac3b1ee6007",
        "5ce5481a4d78cca03498f72761da1b9f1d2aa8fb300be39f0e4fe2534f9d4308",
    ];

    let test_inputs: Vec<BlsScalar> = test_inputs
        .iter()
        .map(|input| BlsScalar::from_hex_str(input).unwrap())
        .collect();

    let mut input = Vec::with_capacity(3 * BlsScalar::SIZE);
    for scalar in test_inputs {
        input.extend(scalar.to_bytes());
    }

    assert_eq!(
        "0x0e17c56704c3ec2523d206e2e06e08b336e0079bb4c4c5b850d496125f73cdb9",
        format!("{:?}", Hasher::digest(input))
    );
}

fn instantiate(vm: &VM, height: u64) -> (Session, ContractId) {
    let bytecode = include_bytes!(
        "../../target/dusk/wasm32-unknown-unknown/release/host_fn.wasm"
    );

    let mut session = rusk_abi::new_genesis_session(vm);

    let contract_id = session
        .deploy(
            bytecode,
            ContractData::builder().owner(get_owner().to_bytes()),
            POINT_LIMIT,
        )
        .expect("Deploying module should succeed");

    let base = session.commit().expect("Committing should succeed");

    let session = rusk_abi::new_session(vm, base, height)
        .expect("Instantiating new session should succeed");

    (session, contract_id)
}

#[test]
fn hash() {
    let vm =
        rusk_abi::new_ephemeral_vm().expect("Instantiating VM should succeed");
    let (mut session, contract_id) = instantiate(&vm, 0);

    let test_inputs = [
        "bb67ed265bf1db490ded2e1ede55c0d14c55521509dc73f9c354e98ab76c9625",
        "7e74220084d75e10c89e9435d47bb5b8075991b2e29be3b84421dac3b1ee6007",
        "5ce5481a4d78cca03498f72761da1b9f1d2aa8fb300be39f0e4fe2534f9d4308",
    ];

    let test_inputs: Vec<BlsScalar> = test_inputs
        .iter()
        .map(|input| BlsScalar::from_hex_str(input).unwrap())
        .collect();

    let mut input = Vec::with_capacity(3 * BlsScalar::SIZE);
    for scalar in test_inputs {
        input.extend(scalar.to_bytes())
    }

    let scalar: BlsScalar = session
        .call(contract_id, "hash", &input, POINT_LIMIT)
        .expect("Querying should succeed")
        .data;

    assert_eq!(
        "0x0e17c56704c3ec2523d206e2e06e08b336e0079bb4c4c5b850d496125f73cdb9",
        format!("{scalar:?}")
    );
}

#[test]
fn poseidon_hash() {
    let vm =
        rusk_abi::new_ephemeral_vm().expect("Instantiating VM should succeed");
    let (mut session, contract_id) = instantiate(&vm, 0);

    let test_inputs = [
        "bb67ed265bf1db490ded2e1ede55c0d14c55521509dc73f9c354e98ab76c9625",
        "7e74220084d75e10c89e9435d47bb5b8075991b2e29be3b84421dac3b1ee6007",
        "5ce5481a4d78cca03498f72761da1b9f1d2aa8fb300be39f0e4fe2534f9d4308",
    ];

    let test_inputs: Vec<BlsScalar> = test_inputs
        .iter()
        .map(|input| BlsScalar::from_hex_str(input).unwrap())
        .collect();

    let scalar: BlsScalar = session
        .call(contract_id, "poseidon_hash", &test_inputs, POINT_LIMIT)
        .expect("Querying should succeed")
        .data;

    assert_eq!(
        "0x2885ca6d908b34ca83f2177d78283c25d8c5c7230877025bc8d558b8a94e6fe3",
        format!("{scalar:?}")
    );
}

#[test]
fn schnorr_signature() {
    let vm =
        rusk_abi::new_ephemeral_vm().expect("Instantiating VM should succeed");
    let (mut session, contract_id) = instantiate(&vm, 0);

    let note_sk = NoteSecretKey::random(&mut OsRng);
    let message = BlsScalar::random(&mut OsRng);
    let note_pk = NotePublicKey::from(&note_sk);

    let note_sig = note_sk.sign(&mut OsRng, message);

    assert!(note_pk.verify(&note_sig, message));

    let valid: bool = session
        .call(
            contract_id,
            "verify_schnorr",
            &(message, note_pk, note_sig),
            POINT_LIMIT,
        )
        .expect("Querying should succeed")
        .data;

    assert!(valid, "Signature verification expected to succeed");

    let wrong_sk = NoteSecretKey::random(&mut OsRng);
    let note_pk = NotePublicKey::from(&wrong_sk);

    let valid: bool = session
        .call(
            contract_id,
            "verify_schnorr",
            &(message, note_pk, note_sig),
            POINT_LIMIT,
        )
        .expect("Querying should succeed")
        .data;

    assert!(!valid, "Signature verification expected to fail");
}

#[test]
fn stake_signature() {
    let vm =
        rusk_abi::new_ephemeral_vm().expect("Instantiating VM should succeed");
    let (mut session, contract_id) = instantiate(&vm, 0);

    let message = b"some-message".to_vec();

    let stake_sk = StakeSecretKey::random(&mut OsRng);
    let stake_pk = StakePublicKey::from(&stake_sk);

    let stake_sig = stake_sk.sign(&stake_pk, &message);

    let arg = (message, stake_pk, stake_sig);
    let valid: bool = session
        .call(contract_id, "verify_bls", &arg, POINT_LIMIT)
        .expect("Query should succeed")
        .data;

    assert!(valid, "Stake Signature verification expected to succeed");

    let wrong_sk = StakeSecretKey::random(&mut OsRng);
    let wrong_pk = StakePublicKey::from(&wrong_sk);

    let arg = (arg.0, wrong_pk, arg.2);
    let valid: bool = session
        .call(contract_id, "verify_bls", &arg, POINT_LIMIT)
        .expect("Query should succeed")
        .data;

    assert!(!valid, "Stake Signature verification expected to fail");
}

#[derive(Debug, Default)]
pub struct TestCircuit {
    pub a: BlsScalar,
    pub b: BlsScalar,
    pub c: BlsScalar,
}

impl TestCircuit {
    pub fn new(a: u64, b: u64) -> Self {
        let a = a.into();
        let b = b.into();
        let c = a + b;

        Self { a, b, c }
    }
}

impl Circuit for TestCircuit {
    fn circuit(&self, composer: &mut Composer) -> Result<(), Error> {
        // append 3 gates that always evaluate to true

        let a = composer.append_witness(self.a);
        let b = composer.append_witness(self.b);
        let six = composer.append_witness(BlsScalar::from(6));
        let one = composer.append_witness(BlsScalar::from(1));
        let seven = composer.append_witness(BlsScalar::from(7));
        let min_twenty = composer.append_witness(-BlsScalar::from(20));

        let constraint = Constraint::new()
            .left(-BlsScalar::one())
            .a(a)
            .right(-BlsScalar::one())
            .b(b)
            .public(self.c);
        composer.append_gate(constraint);

        let constraint = Constraint::new()
            .mult(1)
            .left(2)
            .right(3)
            .fourth(1)
            .constant(4)
            .output(4)
            .a(six)
            .b(seven)
            .d(one)
            .o(min_twenty);
        composer.append_gate(constraint);

        let constraint = Constraint::new()
            .mult(1)
            .left(1)
            .right(1)
            .constant(127)
            .output(1)
            .a(min_twenty)
            .b(six)
            .o(seven);
        composer.append_gate(constraint);

        Ok(())
    }
}

#[test]
fn plonk_proof() {
    let vm =
        rusk_abi::new_ephemeral_vm().expect("Instantiating VM should succeed");
    let (mut session, contract_id) = instantiate(&vm, 0);

    let pp = include_bytes!("./pp_test.bin");
    let pp = unsafe { PublicParameters::from_slice_unchecked(&pp[..]) };

    let label = b"dusk-network";

    let (prover, verifier) = Compiler::compile::<TestCircuit>(&pp, label)
        .expect("Circuit should compile successfully");

    let a = 1u64;
    let b = 2u64;
    let expected_pi = vec![BlsScalar::from(a) + BlsScalar::from(b)];
    let circuit = TestCircuit::new(a, b);

    let (proof, prover_pi) = prover
        .prove(&mut OsRng, &circuit)
        .expect("Proving circuit should succeed");

    // Check public inputs
    assert_eq!(
        expected_pi, prover_pi,
        "Prover generates different pi than expected"
    );

    // Integrity check
    verifier
        .verify(&proof, &expected_pi)
        .expect("Proof should verify successfully");
    let verifier = verifier.to_bytes();

    let public_inputs: Vec<PublicInput> =
        expected_pi.into_iter().map(|pi| From::from(pi)).collect();

    let proof = proof.to_bytes().to_vec();

    let arg = (verifier, proof, public_inputs);
    let valid: bool = session
        .call(contract_id, "verify_proof", &arg, POINT_LIMIT)
        .expect("Query should succeed")
        .data;

    assert!(valid, "The proof should be valid");

    let wrong_public_inputs = vec![BlsScalar::from(0)];
    let wrong_public_inputs: Vec<PublicInput> =
        wrong_public_inputs.into_iter().map(From::from).collect();

    let arg = (arg.0, arg.1, wrong_public_inputs);
    let valid: bool = session
        .call(contract_id, "verify_proof", &arg, POINT_LIMIT)
        .expect("Query should succeed")
        .data;

    assert!(!valid, "The proof should be invalid");
}

#[test]
fn block_height() {
    const HEIGHT: u64 = 123;

    let vm =
        rusk_abi::new_ephemeral_vm().expect("Instantiating VM should succeed");
    let (mut session, contract_id) = instantiate(&vm, HEIGHT);

    let height: u64 = session
        .call(contract_id, "block_height", &(), POINT_LIMIT)
        .expect("Query should succeed")
        .data;

    assert_eq!(height, HEIGHT);
}

fn get_owner() -> &'static PublicKey {
    static OWNER: OnceLock<PublicKey> = OnceLock::new();
    OWNER.get_or_init(|| {
        let sk = SecretKey::random(&mut OsRng);
        PublicKey::from(&sk)
    })
}

#[test]
fn owner_raw() {
    let vm =
        rusk_abi::new_ephemeral_vm().expect("Instantiating VM should succeed");
    let (mut session, contract_id) = instantiate(&vm, 0);

    let owner: [u8; 64] = session
        .call(contract_id, "contract_owner_raw", get_owner(), POINT_LIMIT)
        .expect("Query should succeed")
        .data;

    assert_eq!(owner, get_owner().to_bytes());
}

#[test]
fn owner() {
    let vm =
        rusk_abi::new_ephemeral_vm().expect("Instantiating VM should succeed");
    let (mut session, contract_id) = instantiate(&vm, 0);

    let owner: PublicKey = session
        .call(contract_id, "contract_owner", get_owner(), POINT_LIMIT)
        .expect("Query should succeed")
        .data;

    assert_eq!(owner, get_owner().to_owned());
}
