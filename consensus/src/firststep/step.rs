// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) DUSK NETWORK. All rights reserved.
use crate::commons::{RoundUpdate, SelectError};

use crate::consensus::Context;
use crate::event_loop::event_loop;
use crate::firststep::handler;
use crate::messages::MsgReduction;

use crate::frame;
use crate::frame::Frame;
use crate::user::committee::Committee;
use tokio::sync::mpsc::Receiver;
use tokio::sync::oneshot;

pub const COMMITTEE_SIZE: usize = 64;

#[allow(unused)]
pub struct Reduction {
    msg_rx: Receiver<MsgReduction>,

    pub timeout: u16,
    handler: handler::Reduction,
}

impl Reduction {
    pub fn new(msg_rx: Receiver<MsgReduction>) -> Self {
        Self {
            msg_rx,
            timeout: 0,
            handler: handler::Reduction {},
        }
    }

    pub fn initialize(&mut self, frame: &Frame) {
        let empty = frame::NewBlock::default();

        let mut _new_block = match frame {
            Frame::NewBlock(f) => f,
            Frame::StepVotes(_) => panic!("invalid frame"),
            Frame::Empty => &empty,
        };
    }

    pub async fn run(
        &mut self,
        ctx_recv: &mut oneshot::Receiver<Context>,
        committee: Committee,
        ru: RoundUpdate,
        step: u8,
    ) -> Result<Frame, SelectError> {
        if committee.am_member() {
            // TODO: SendReduction async
            // TODO: Register my reduction locally
        }

        // TODO: drain queued messages

        event_loop(
            &mut self.handler,
            &mut self.msg_rx,
            ctx_recv,
            ru,
            step,
            &committee,
        )
        .await
    }

    pub fn name(&self) -> &'static str {
        "1th_reduction"
    }

    pub fn get_committee_size(&self) -> usize {
        COMMITTEE_SIZE
    }
}
