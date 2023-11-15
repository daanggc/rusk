// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) DUSK NETWORK. All rights reserved.

use crate::commons::{ConsensusError, RoundUpdate};
use crate::msg_handler::{HandleMsgOutput, MsgHandler};
use crate::step_votes_reg::{SafeStepVotesRegistry, SvType};
use async_trait::async_trait;
use node_data::ledger;
use node_data::ledger::{Hash, Signature, StepVotes};
use tracing::error;

use crate::aggregator::Aggregator;
use node_data::message::{payload, Message, Payload, Topics};

use crate::user::committee::Committee;

pub struct Reduction {
    pub(crate) sv_registry: SafeStepVotesRegistry,

    pub(crate) aggregator: Aggregator,
    pub(crate) first_step_votes: StepVotes,
    pub(crate) curr_step: u8,
}

#[async_trait]
impl MsgHandler<Message> for Reduction {
    fn verify(
        &mut self,
        msg: Message,
        _ru: &RoundUpdate,
        _step: u8,
        _committee: &Committee,
    ) -> Result<Message, ConsensusError> {
        let signed_hash = match &msg.payload {
            Payload::Reduction(p) => Ok(p.signature),
            Payload::Empty => Ok(Signature::default().0),
            _ => Err(ConsensusError::InvalidMsgType),
        }?;

        if let Err(e) = msg.header.verify_signature(&signed_hash) {
            error!("verify_signature err: {}", e);
            return Err(ConsensusError::InvalidSignature);
        }

        Ok(msg)
    }

    /// Collect the reduction message.
    async fn collect(
        &mut self,
        msg: Message,
        ru: &RoundUpdate,
        step: u8,
        committee: &Committee,
    ) -> Result<HandleMsgOutput, ConsensusError> {
        assert_eq!(step, self.curr_step, "msg does not belong to curr step");

        let signature = match &msg.payload {
            Payload::Reduction(p) => Ok(p.signature),
            Payload::Empty => Ok(Signature::default().0),
            _ => Err(ConsensusError::InvalidMsgType),
        }?;

        // Collect vote, if msg payload is of reduction type
        if let Some((block_hash, second_step_votes)) = self
            .aggregator
            .collect_vote(committee, &msg.header, &signature)
        {
            // Record result in global round results registry
            _ = self.sv_registry.lock().await.add_step_votes(
                step,
                block_hash,
                second_step_votes,
                SvType::SecondReduction,
            );

            return Ok(HandleMsgOutput::Ready(self.build_agreement_msg(
                ru,
                step,
                block_hash,
                second_step_votes,
            )));
        }

        Ok(HandleMsgOutput::Pending(msg))
    }

    /// Collects the reduction message from former iteration.
    async fn collect_from_past(
        &mut self,
        msg: Message,
        _ru: &RoundUpdate,
        step: u8,
        committee: &Committee,
    ) -> Result<HandleMsgOutput, ConsensusError> {
        let signature = match &msg.payload {
            Payload::Reduction(p) => Ok(p.signature),
            Payload::Empty => Ok(Signature::default().0),
            _ => Err(ConsensusError::InvalidMsgType),
        }?;

        // Collect vote, if msg payload is reduction type
        if let Some((hash, sv)) =
            self.aggregator
                .collect_vote(committee, &msg.header, &signature)
        {
            // Record result in global round registry
            if let Some(m) = self.sv_registry.lock().await.add_step_votes(
                step,
                hash,
                sv,
                SvType::SecondReduction,
            ) {
                // TODO: Send an agreement
            }
        }

        Ok(HandleMsgOutput::Pending(msg))
    }

    /// Handle of an event of step execution timeout
    fn handle_timeout(
        &mut self,
        _ru: &RoundUpdate,
        _step: u8,
    ) -> Result<HandleMsgOutput, ConsensusError> {
        Ok(HandleMsgOutput::Ready(Message::empty()))
    }
}

impl Reduction {
    pub(crate) fn new(sv_registry: SafeStepVotesRegistry) -> Self {
        Self {
            sv_registry,
            aggregator: Default::default(),
            first_step_votes: Default::default(),
            curr_step: 0,
        }
    }

    fn build_agreement_msg(
        &self,
        ru: &RoundUpdate,
        step: u8,
        block_hash: Hash,
        second_step_votes: ledger::StepVotes,
    ) -> Message {
        let hdr = node_data::message::Header {
            pubkey_bls: ru.pubkey_bls.clone(),
            round: ru.round,
            step,
            block_hash,
            topic: Topics::Agreement as u8,
        };

        let signature = hdr.sign(&ru.secret_key, ru.pubkey_bls.inner());
        let payload = payload::Agreement {
            signature,
            first_step: self.first_step_votes,
            second_step: second_step_votes,
        };

        Message::new_agreement(hdr, payload)
    }

    pub(crate) fn reset(&mut self, step: u8) {
        self.first_step_votes = StepVotes::default();
        self.curr_step = step;
    }
}
