// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) DUSK NETWORK. All rights reserved.

//! Extra data that may be sent with the `data` field of either transaction
//! type.

use alloc::format;
use alloc::string::String;
use alloc::vec::Vec;

use bytecheck::CheckBytes;
use dusk_bytes::{DeserializableSlice, Error as BytesError, Serializable};
use rkyv::{
    ser::serializers::AllocSerializer, Archive, Deserialize, Serialize,
};

use crate::{ContractId, Error, ARGBUF_LEN};

/// The maximum size of a memo.
pub const MAX_MEMO_SIZE: usize = 512;

/// Data for either contract call or contract deployment.
#[derive(Debug, Clone, PartialEq, Eq, Archive, Serialize, Deserialize)]
#[archive_attr(derive(CheckBytes))]
#[allow(clippy::large_enum_variant)]
pub enum TransactionData {
    /// Data for a contract call.
    Call(ContractCall),
    /// Data for a contract deployment.
    Deploy(ContractDeploy),
    /// Additional data added to a transaction, that is not a deployment or a
    /// call.
    Memo(Vec<u8>),
}

impl From<ContractCall> for TransactionData {
    fn from(c: ContractCall) -> Self {
        TransactionData::Call(c)
    }
}

impl From<ContractDeploy> for TransactionData {
    fn from(d: ContractDeploy) -> Self {
        TransactionData::Deploy(d)
    }
}

/// Data for performing a contract deployment
#[derive(Debug, Clone, PartialEq, Eq, Archive, Serialize, Deserialize)]
#[archive_attr(derive(CheckBytes))]
pub struct ContractDeploy {
    /// Bytecode of the contract to be deployed.
    pub bytecode: ContractBytecode,
    /// Owner of the contract to be deployed.
    pub owner: Vec<u8>,
    /// Init method arguments of the deployed contract.
    pub init_args: Option<Vec<u8>>,
    /// Nonce for contract id uniqueness and vanity
    pub nonce: u64,
}

/// All the data the transfer-contract needs to perform a contract-call.
#[derive(Debug, Clone, PartialEq, Eq, Archive, Serialize, Deserialize)]
#[archive_attr(derive(CheckBytes))]
pub struct ContractCall {
    /// The unique ID of the contract to be called.
    pub contract: ContractId,
    /// The function of the contract that should be called.
    pub fn_name: String,
    /// The function arguments for the contract call, in bytes.
    pub fn_args: Vec<u8>,
}

impl ContractDeploy {
    /// Serialize a `ContractDeploy` into a variable length byte buffer.
    #[must_use]
    pub fn to_var_bytes(&self) -> Vec<u8> {
        let mut bytes = Vec::new();

        bytes.extend(&self.bytecode.to_var_bytes());

        bytes.extend((self.owner.len() as u64).to_bytes());
        bytes.extend(&self.owner);

        match &self.init_args {
            Some(init_args) => {
                bytes.push(1);
                bytes.extend((init_args.len() as u64).to_bytes());
                bytes.extend(init_args);
            }
            None => bytes.push(0),
        }

        bytes.extend(self.nonce.to_bytes());

        bytes
    }

    /// Deserialize a `ContractDeploy` from a byte buffer.
    ///
    /// # Errors
    /// Errors when the bytes are not canonical.
    pub fn from_slice(buf: &[u8]) -> Result<Self, BytesError> {
        let mut buf = buf;

        let bytecode = ContractBytecode::from_buf(&mut buf)?;

        let owner = crate::read_vec(&mut buf)?;

        let init_args = match u8::from_reader(&mut buf)? {
            0 => None,
            1 => Some(crate::read_vec(&mut buf)?),
            _ => return Err(BytesError::InvalidData),
        };

        let nonce = u64::from_reader(&mut buf)?;

        Ok(Self {
            bytecode,
            owner,
            init_args,
            nonce,
        })
    }
}

impl ContractCall {
    /// Creates a new contract call.
    ///
    /// # Errors
    /// Errors if rkyv serialization fails.
    pub fn new(
        contract: impl Into<ContractId>,
        fn_name: impl Into<String>,
        fn_args: &impl Serialize<AllocSerializer<ARGBUF_LEN>>,
    ) -> Result<Self, Error> {
        Ok(Self {
            contract: contract.into(),
            fn_name: fn_name.into(),
            fn_args: rkyv::to_bytes::<_, ARGBUF_LEN>(fn_args)
                .map_err(|e| Error::Rkyv(format!("{e:?}")))?
                .to_vec(),
        })
    }

    /// Serialize a `ContractCall` into a variable length byte buffer.
    #[must_use]
    pub fn to_var_bytes(&self) -> Vec<u8> {
        let mut bytes = Vec::new();

        bytes.extend(self.contract.as_bytes());

        let fn_name_bytes = self.fn_name.as_bytes();
        bytes.extend((fn_name_bytes.len() as u64).to_bytes());
        bytes.extend(fn_name_bytes);

        bytes.extend((self.fn_args.len() as u64).to_bytes());
        bytes.extend(&self.fn_args);

        bytes
    }

    /// Deserialize a `ContractCall` from a byte buffer.
    ///
    /// # Errors
    /// Errors when the bytes are not canonical.
    pub fn from_slice(buf: &[u8]) -> Result<Self, BytesError> {
        let mut buf = buf;

        let contract = crate::read_arr::<32>(&mut buf)?;

        let fn_name = crate::read_str(&mut buf)?;

        let fn_args = crate::read_vec(&mut buf)?;

        Ok(Self {
            contract: contract.into(),
            fn_name,
            fn_args,
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Archive, Serialize, Deserialize)]
#[archive_attr(derive(CheckBytes))]
/// Holds bytes of bytecode and its hash.
pub struct ContractBytecode {
    /// Hash of the bytecode bytes.
    pub hash: [u8; 32],
    /// Bytecode bytes.
    pub bytes: Vec<u8>,
}

impl ContractBytecode {
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
    /// Resets buffer to a position after the bytes read.
    ///
    /// # Errors
    /// Errors when the bytes are not available.
    pub fn from_buf(buf: &mut &[u8]) -> Result<Self, BytesError> {
        let hash = crate::read_arr::<32>(buf)?;
        let bytes = crate::read_vec(buf)?;
        Ok(Self { hash, bytes })
    }
}
