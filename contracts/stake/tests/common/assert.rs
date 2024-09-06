// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) DUSK NETWORK. All rights reserved.

use dusk_bytes::Serializable;
use rkyv::{check_archived_root, Deserialize, Infallible};

use execution_core::{
    signatures::bls::PublicKey as BlsPublicKey,
    stake::{StakeEvent, StakeWithReceiverEvent},
    Event,
};

pub fn assert_event<S>(
    events: &Vec<Event>,
    topic: S,
    should_pk: &BlsPublicKey,
    should_amount: u64,
) where
    S: AsRef<str>,
{
    let topic = topic.as_ref();
    let event = events
        .iter()
        .find(|e| e.topic == topic)
        .expect(&format!("event: {topic} should exist in the event list",));

    if topic == "unstake" || topic == "withdraw" {
        let staking_event_data = check_archived_root::<StakeWithReceiverEvent>(
            event.data.as_slice(),
        )
        .expect("Stake event data should deserialize correctly");
        let staking_event_data: StakeWithReceiverEvent = staking_event_data
            .deserialize(&mut Infallible)
            .expect("Infallible");
        assert_eq!(staking_event_data.value, should_amount);
        assert_eq!(staking_event_data.account.to_bytes(), should_pk.to_bytes());
    } else {
        let staking_event_data: StakeEvent =
            serde_json::from_slice(&event.data)
                .expect("Stake event data should deserialize correctly");
        assert_eq!(staking_event_data.value, should_amount);
        assert_eq!(staking_event_data.account.to_bytes(), should_pk.to_bytes());
    }
}
