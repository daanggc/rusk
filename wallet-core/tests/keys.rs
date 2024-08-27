// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) DUSK NETWORK. All rights reserved.

use dusk_bytes::Serializable;

use wallet_core::keys::{
    derive_bls_sk, derive_multiple_phoenix_sk, derive_phoenix_pk,
    derive_phoenix_sk, derive_phoenix_vk,
};

const SEED: [u8; 64] = [0; 64];
const INDEX: u8 = 42;

#[test]
fn test_derive_phoenix_sk() {
    // it is important that we always derive the same key from a fixed seed
    let sk_bytes = [
        160, 210, 234, 8, 94, 23, 76, 60, 130, 143, 137, 225, 37, 83, 68, 218,
        207, 192, 171, 235, 252, 130, 133, 62, 18, 232, 6, 49, 245, 123, 220,
        12, 250, 111, 39, 88, 24, 41, 156, 174, 241, 14, 118, 173, 11, 53, 192,
        126, 7, 119, 70, 69, 212, 230, 124, 79, 223, 140, 93, 153, 33, 147,
        163, 0,
    ];
    assert_eq!(derive_phoenix_sk(&SEED, INDEX).to_bytes(), sk_bytes);
}

#[test]
fn test_derive_multiple_phoenix_sk() {
    // it is important that we always derive the same key from a fixed seed
    let sk_bytes_0 = [
        184, 26, 20, 142, 31, 215, 134, 183, 100, 232, 94, 101, 206, 208, 164,
        59, 72, 52, 42, 195, 106, 148, 124, 219, 47, 13, 145, 73, 227, 209,
        140, 2, 27, 66, 207, 96, 97, 88, 228, 153, 4, 43, 36, 106, 159, 80, 80,
        173, 145, 49, 116, 46, 8, 8, 95, 220, 115, 6, 30, 4, 100, 15, 89, 4,
    ];
    let sk_bytes_1 = [
        39, 29, 134, 8, 24, 45, 211, 176, 210, 104, 110, 168, 65, 57, 41, 187,
        77, 165, 30, 222, 130, 179, 204, 85, 252, 152, 73, 218, 50, 97, 245, 8,
        252, 26, 233, 156, 237, 162, 138, 31, 83, 196, 188, 60, 69, 76, 44, 59,
        251, 111, 182, 35, 247, 210, 66, 108, 46, 117, 92, 183, 16, 104, 162,
        9,
    ];

    let keys = derive_multiple_phoenix_sk(&SEED, 0..2);
    assert_eq!(keys[0].to_bytes(), sk_bytes_0,);
    assert_eq!(keys[1].to_bytes(), sk_bytes_1,);
}

#[test]
fn test_derive_phoenix_pk() {
    // it is important that we always derive the same key from a fixed seed
    let pk_bytes = [
        59, 192, 170, 209, 99, 97, 60, 124, 218, 81, 61, 102, 25, 235, 14, 87,
        219, 234, 56, 102, 10, 111, 22, 189, 171, 101, 180, 168, 17, 70, 72,
        101, 135, 243, 55, 243, 138, 103, 185, 26, 196, 219, 84, 126, 33, 115,
        84, 60, 38, 41, 79, 104, 232, 222, 105, 2, 60, 185, 149, 50, 207, 43,
        89, 100,
    ];
    assert_eq!(derive_phoenix_pk(&SEED, INDEX).to_bytes(), pk_bytes);
}

#[test]
fn test_derive_phoenix_vk() {
    // it is important that we always derive the same key from a fixed seed
    let vk_bytes = [
        160, 210, 234, 8, 94, 23, 76, 60, 130, 143, 137, 225, 37, 83, 68, 218,
        207, 192, 171, 235, 252, 130, 133, 62, 18, 232, 6, 49, 245, 123, 220,
        12, 135, 243, 55, 243, 138, 103, 185, 26, 196, 219, 84, 126, 33, 115,
        84, 60, 38, 41, 79, 104, 232, 222, 105, 2, 60, 185, 149, 50, 207, 43,
        89, 100,
    ];
    assert_eq!(derive_phoenix_vk(&SEED, INDEX).to_bytes(), vk_bytes);
}

#[test]
fn test_derive_bls_sk() {
    // it is important that we always derive the same key from a fixed seed
    let sk_bytes = [
        130, 180, 24, 224, 131, 143, 97, 18, 120, 53, 37, 39, 251, 44, 121,
        168, 4, 248, 29, 176, 142, 136, 224, 188, 159, 246, 73, 6, 112, 174, 6,
        7,
    ];
    assert_eq!(derive_bls_sk(&SEED, INDEX).to_bytes(), sk_bytes);
}
