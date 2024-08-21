// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) DUSK NETWORK. All rights reserved.

use std::time::Duration;

/// Maximum number of iterations Consensus runs per a single round.
pub const CONSENSUS_MAX_ITER: u8 = 50;

/// Total credits of steps committees
pub const PROPOSAL_COMMITTEE_CREDITS: usize = 1;
pub const VALIDATION_COMMITTEE_CREDITS: usize = 64;
pub const RATIFICATION_COMMITTEE_CREDITS: usize = 64;

pub const RELAX_ITERATION_THRESHOLD: u8 = 8;

/// Emergency mode is enabled after 16 iterations
pub const EMERGENCY_MODE_ITERATION_THRESHOLD: u8 = 16;

pub const MIN_STEP_TIMEOUT: Duration = Duration::from_secs(7);
pub const MAX_STEP_TIMEOUT: Duration = Duration::from_secs(40);
pub const TIMEOUT_INCREASE: Duration = Duration::from_secs(2);
pub const MINIMUM_BLOCK_TIME: u64 = 10;

// Returns `floor(value/2) + 1`
pub fn majority(value: usize) -> usize {
    value / 2 + 1
}

// Returns `ceil( value/3*2 )`
pub fn supermajority(value: usize) -> usize {
    let sm = value as f32 / 3.0 * 2.0;
    sm.ceil() as usize
}

/// Returns the quorum of a Ratification committee
pub fn ratification_quorum() -> usize {
    supermajority(RATIFICATION_COMMITTEE_CREDITS)
}

/// Returns the quorum of a Validation committee
pub fn validation_quorum() -> usize {
    supermajority(VALIDATION_COMMITTEE_CREDITS)
}

/// Returns the number of credits beyond the quorum for a Validation committee
pub fn validation_extra() -> usize {
    VALIDATION_COMMITTEE_CREDITS - validation_quorum()
}

/// Returns the number of credits beyond the quorum for a Ratification committee
pub fn ratification_extra() -> usize {
    RATIFICATION_COMMITTEE_CREDITS - ratification_quorum()
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_quorums() {
        assert_eq!(majority(VALIDATION_COMMITTEE_CREDITS), 33);
        assert_eq!(validation_quorum(), 43);
        assert_eq!(ratification_quorum(), 43);
        assert_eq!(validation_extra(), 21);
        assert_eq!(ratification_extra(), 21);
    }
}
