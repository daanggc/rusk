// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) DUSK NETWORK. All rights reserved.

//! Wrapper for a strip-able bytecode that we want to keep the integrity of.

extern crate alloc;
use crate::reader::{read_arr, read_vec};
use alloc::vec::Vec;
use bytecheck::CheckBytes;
use core::mem;
use dusk_bytes::Error::InvalidData;
use dusk_bytes::{Error as BytesError, Serializable};
use rkyv::{Archive, Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Eq, Archive, Serialize, Deserialize)]
#[archive_attr(derive(CheckBytes))]
/// Holds bytes of bytecode and its hash.
pub struct Bytecode {
    /// Hash of the bytecode bytes.
    pub hash: [u8; 32],
    /// Bytecode bytes.
    pub bytes: Vec<u8>,
}

impl Bytecode {
    /// Provides contribution bytes for an external hash.
    #[must_use]
    pub fn to_hash_input_bytes(&self) -> Vec<u8> {
        self.hash.to_vec()
    }

    /// Serializes this object into a variable length buffer
    #[must_use]
    pub fn to_var_bytes(&self) -> Vec<u8> {
        let mut bytes = Vec::new();
        bytes.extend(self.hash);
        bytes.extend((self.bytes.len() as u64).to_bytes());
        bytes.extend(&self.bytes);
        bytes
    }

    /// Deserialize from a bytes buffer.
    ///
    /// # Errors
    /// Errors when the bytes are not available.
    pub fn from_buf(buf: &[u8]) -> Result<(Self, usize), BytesError> {
        let mut buf = buf;
        let hash = read_arr::<32>(&mut buf)?;
        let bytes = read_vec(&mut buf)?;
        let bytes_len = bytes.len();
        Ok((Self { hash, bytes }, 32 + bytes_len + mem::size_of::<u64>()))
    }
}
