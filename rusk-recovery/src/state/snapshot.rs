// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) DUSK NETWORK. All rights reserved.

use std::fmt::Debug;

use dusk_bytes::Serializable;
use execution_core::signatures::bls::PublicKey as AccountPublicKey;
use execution_core::transfer::phoenix::PublicKey as PhoenixPublicKey;
use execution_core::Dusk;
use serde_derive::{Deserialize, Serialize};

mod stake;
mod wrapper;

pub use stake::GenesisStake;
use wrapper::Wrapper;

use crate::state;

#[derive(Serialize, Deserialize, PartialEq, Eq)]
pub struct PhoenixBalance {
    address: Wrapper<PhoenixPublicKey, { PhoenixPublicKey::SIZE }>,
    pub seed: Option<u64>,
    #[serde(skip_serializing_if = "Vec::is_empty", default = "Vec::new")]
    pub notes: Vec<Dusk>,
}

impl PhoenixBalance {
    pub fn address(&self) -> &PhoenixPublicKey {
        &self.address
    }
}

#[derive(Serialize, Deserialize, PartialEq, Eq)]
pub struct MoonlightAccount {
    address: Wrapper<AccountPublicKey, { AccountPublicKey::SIZE }>,
    pub balance: Dusk,
}

impl MoonlightAccount {
    pub fn address(&self) -> &AccountPublicKey {
        &self.address
    }
}

#[derive(Serialize, Deserialize, Default, PartialEq, Eq)]
pub struct Snapshot {
    base_state: Option<String>,
    owner: Option<Wrapper<PhoenixPublicKey, { PhoenixPublicKey::SIZE }>>,

    // This "serde skip" workaround seems needed as per https://github.com/toml-rs/toml-rs/issues/384
    #[serde(skip_serializing_if = "Vec::is_empty", default = "Vec::new")]
    phoenix_balance: Vec<PhoenixBalance>,
    #[serde(skip_serializing_if = "Vec::is_empty", default = "Vec::new")]
    moonlight_account: Vec<MoonlightAccount>,
    #[serde(skip_serializing_if = "Vec::is_empty", default = "Vec::new")]
    stake: Vec<GenesisStake>,
}

impl Debug for Snapshot {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let toml = toml::to_string(self).map_err(|e| {
            let _ = writeln!(f, "{e}");
            std::fmt::Error
        })?;
        f.write_str(&toml)
    }
}

impl Snapshot {
    /// Returns an iterator over the phoenix balances included in this snapshot
    pub fn phoenix_balances(&self) -> impl Iterator<Item = &PhoenixBalance> {
        self.phoenix_balance.iter()
    }

    /// Returns an iterator of the moonlight accounts included in this snapshot
    pub fn moonlight_accounts(
        &self,
    ) -> impl Iterator<Item = &MoonlightAccount> {
        self.moonlight_account.iter()
    }

    /// Returns an iterator of the stakes included in this snapshot.
    pub fn stakes(&self) -> impl Iterator<Item = &GenesisStake> {
        self.stake.iter()
    }

    /// Return the owner of the smart contract.
    pub fn owner(&self) -> [u8; PhoenixPublicKey::SIZE] {
        let dusk = Wrapper::from(*state::DUSK_KEY);
        self.owner.as_ref().unwrap_or(&dusk).to_bytes()
    }

    pub fn base_state(&self) -> Option<&str> {
        self.base_state.as_deref()
    }
}

#[cfg(test)]
mod tests {

    use std::error::Error;

    use super::*;
    use crate::state;

    pub(crate) fn testnet_from_file() -> Result<Snapshot, Box<dyn Error>> {
        let toml = include_str!("../../config/testnet.toml");
        let snapshot = toml::from_str(toml)?;
        Ok(snapshot)
    }

    #[test]
    fn testnet_toml() -> Result<(), Box<dyn Error>> {
        let testnet = testnet_from_file()?;

        testnet
            .phoenix_balance
            .iter()
            .find(|b| b.address().eq(&*state::FAUCET_KEY))
            .expect("Testnet must have faucet configured");

        testnet
            .stakes()
            .next()
            .expect("Testnet must have at least a provisioner configured");

        Ok(())
    }

    #[test]
    fn empty_toml() -> Result<(), Box<dyn Error>> {
        let str = toml::to_string_pretty(&Snapshot::default())?;
        let deserialized: Snapshot = toml::from_str(&str)?;

        // `Snapshot` is too big to be compared with assert_eq
        assert_eq!(
            Snapshot::default(),
            deserialized,
            "Deserialized struct differs from the serialized one"
        );
        Ok(())
    }
}
