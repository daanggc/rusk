// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) DUSK NETWORK. All rights reserved.

use crate::commons::{ConsensusError, RoundUpdate};
use crate::config;
use crate::messages::Message;
use crate::msg_handler::MsgHandler;
use crate::queue::Queue;
use crate::user::committee::Committee;
use crate::user::provisioners::Provisioners;
use crate::user::sortition;
use crate::util::pending_queue::PendingQueue;
use std::time::Duration;
use tokio::sync::oneshot;
use tokio::time;
use tokio::time::Instant;
use tracing::error;

/// ExecutionCtx encapsulates all data needed by a single step to be fully executed.
pub struct ExecutionCtx<'a> {
    pub cancel_chan: &'a mut oneshot::Receiver<bool>,

    /// Messaging-related fields
    pub inbound: PendingQueue,
    pub outbound: PendingQueue,
    pub future_msgs: &'a mut Queue<Message>,

    /// State-related fields
    pub provisioners: &'a mut Provisioners,

    // Round/Step parameters
    pub round_update: RoundUpdate,
    pub step: u8,
}

impl<'a> ExecutionCtx<'a> {
    pub fn new(
        cancel_chan: &'a mut oneshot::Receiver<bool>,
        inbound: PendingQueue,
        outbound: PendingQueue,
        future_msgs: &'a mut Queue<Message>,
        provisioners: &'a mut Provisioners,
        round_update: RoundUpdate,
        step: u8,
    ) -> Self {
        Self {
            cancel_chan,
            inbound,
            outbound,
            future_msgs,
            provisioners,
            round_update,
            step,
        }
    }

    // event_loop collects multiple events - inbound messages, cancel event or timeout event.
    pub async fn event_loop<C: MsgHandler<Message>>(
        &mut self,
        committee: &Committee,
        phase: &mut C,
    ) -> Result<Message, ConsensusError> {
        self.trace("run event_loop");

        // Calculate timeout
        let deadline = Instant::now()
            .checked_add(Duration::from_millis(config::CONSENSUS_TIMEOUT_MS))
            .unwrap();

        let inbound = self.inbound.clone();

        loop {
            tokio::select! {
                biased;
                // Handle both timeout and cancel events
                result = time::timeout_at(deadline, &mut *self.cancel_chan) => {
                    if result.is_ok() {
                        // cancel chan triggered
                        return Err(ConsensusError::Cancelled);
                    } else {
                        // Timeout-ed step should proceed to next step with zero-ed message.
                          return self.process_timeout_event(committee, phase);
                    }
                 },
                // Handle inbound message
                res = inbound.recv() => {
                    match res {
                        Ok(msg) => {
                            if let Some(step_result) = self.process_inbound_msg(committee, phase, msg).await {
                                return Ok(step_result);
                            }
                        },
                        Err(e) => {
                            error!("recv inbound message error: {}",e);
                        },
                    }
                },
            }
        }
    }

    /// process_inbound_msg delegates the message to the Phase handler for further processing.
    async fn process_inbound_msg<C: MsgHandler<Message>>(
        &mut self,
        committee: &Committee,
        phase: &mut C,
        msg: Message,
    ) -> Option<Message> {
        match phase.handle(msg.clone(), self.round_update, self.step, committee) {
            // Fully valid state reached on this step. Return it as an output.
            // Populate next step with it.
            Ok(result) => {
                let (msg, is_final_msg) = result;

                // Re-publish the returned message
                self.outbound.send(msg.clone()).await.unwrap_or_else(|err| {
                    tracing::error!("unable to re-publish a handled msg {:?}", err)
                });

                if is_final_msg {
                    return Some(msg);
                }

                None
            }
            // An error here means an phase considers this message as invalid.
            // This could be due to failed verification, bad round/step.
            Err(e) => {
                match e {
                    ConsensusError::FutureEvent => {
                        // This is a message from future round or step.
                        // Save it in future_msgs to be processed when we reach same round/step.
                        self.future_msgs
                            .put_event(msg.header.round, msg.header.step, msg);
                    }
                    ConsensusError::PastEvent => {
                        tracing::trace!("past event");
                    }
                    _ => {
                        self.error("phase handler", e);
                    }
                }

                None
            }
        }
    }

    fn process_timeout_event<C: MsgHandler<Message>>(
        &mut self,
        _committee: &Committee,
        _phase: &mut C,
    ) -> Result<Message, ConsensusError> {
        Err(ConsensusError::Timeout)
    }

    pub fn get_sortition_config(&self, size: usize) -> sortition::Config {
        sortition::Config::new(
            self.round_update.seed,
            self.round_update.round,
            self.step,
            size,
        )
    }

    pub fn handle_future_msgs<C: MsgHandler<Message>>(
        &self,
        committee: &Committee,
        phase: &mut C,
    ) -> Option<Message> {
        let ru = &self.round_update;

        if let Ok(messages) = self.future_msgs.get_events(ru.round, self.step) {
            for msg in messages {
                if let Ok(f) = phase.handle(msg, *ru, self.step, committee) {
                    return Some(f.0);
                }
            }
        }

        None
    }

    pub fn trace(&self, event_name: &'static str) {
        tracing::trace!(
            "event={} round={}, step={}, bls_key={}",
            event_name,
            self.round_update.round,
            self.step,
            self.round_update.pubkey_bls.encode_short_hex()
        );
    }

    pub fn error(&self, event_name: &'static str, err: ConsensusError) {
        tracing::trace!(
            "event={} round={}, step={}, bls_key={} err={:#?}",
            event_name,
            self.round_update.round,
            self.step,
            self.round_update.pubkey_bls.encode_short_hex(),
            err,
        );
    }
}
