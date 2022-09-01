// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) DUSK NETWORK. All rights reserved.
use crate::user::provisioners::PublicKey;

pub enum Status {
    Past,
    Present,
    Future,
}

pub trait MessageTrait {
    fn compare(&self, round: u64, step: u8) -> Status;
    fn get_pubkey_bls(&self) -> PublicKey;
}

/// Message is a data unit that consensus phase can process.
#[derive(Debug, Default, Clone)]
pub struct Message {
    pub header: Header,
    pub payload: Payload,
}

impl MessageTrait for Message {
    fn compare(&self, round: u64, step: u8) -> Status {
        self.header.compare(round, step)
    }

    fn get_pubkey_bls(&self) -> PublicKey {
        self.header.pubkey_bls
    }
}

#[derive(Debug, Default, Clone)]
pub struct Header {
    pub pubkey_bls: PublicKey,
    pub round: u64,
    pub step: u8,
    pub block_hash: [u8; 32],
}

impl Header {
    pub fn compare(&self, round: u64, step: u8) -> Status {
        if self.round == round {
            if self.step == step {
                return Status::Present;
            }

            if self.step > step {
                return Status::Past;
            }

            if self.step < step {
                return Status::Future;
            }
        }

        if self.round > round {
            return Status::Past;
        }

        if self.round < round {
            return Status::Future;
        }

        Status::Past
    }
}

#[derive(Debug, Clone)]
pub enum Payload {
    Reduction(payload::Reduction),
    NewBlock(Box<payload::NewBlock>),
    Empty,
}

impl Default for Payload {
    fn default() -> Self {
        Payload::Empty
    }
}

pub mod payload {
    use crate::commons::Block;

    #[derive(Default, Debug, Clone)]
    pub struct Reduction {
        pub signed_hash: [u8; 32],
    }

    #[derive(Default, Debug, Clone)]
    pub struct NewBlock {
        pub prev_hash: [u8; 32],
        pub candidate: Block,
        pub signed_hash: [u8; 32],
    }
}
