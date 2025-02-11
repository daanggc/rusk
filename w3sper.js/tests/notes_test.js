// This Source Code Form is subject to the terms of the Mozilla Public
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) DUSK NETWORK. All rights reserved.

import { test, assert, getLocalWasmBuffer } from "./harness.js";
import { ProfileGenerator, Bookkeeper } from "@dusk/w3sper";

const hex = (bytes) =>
  Array.from(bytes)
    .map((byte) => byte.toString(16).padStart(2, "0"))
    .join("");

// Define a seed for deterministic profile generation
const SEED = new Uint8Array(64).fill(1);
const seeder = async () => SEED;

const NOTES_RKYV = "./tests/assets/notes.rkyv";
const notesBuffer = await Deno.readFile(NOTES_RKYV);

// NOTE: This tests helps to check some of the protocol driver internals
import * as ProtocolDriver from "../src/protocol-driver/mod.js";

// Test case for default profile
test("owened notes balance", async () => {
  ProtocolDriver.load(await getLocalWasmBuffer());

  const profiles = new ProfileGenerator(seeder);

  const owner1 = await Promise.all([
    profiles.default,
    profiles.next(),
    profiles.next(),
  ]);

  const owner2 = await Promise.all([profiles.next(), profiles.next()]);
  const owner3 = [await profiles.next()];
  const owner4 = [await profiles.next()];

  let [notes1] = await ProtocolDriver.mapOwned(owner1, notesBuffer);
  assert.equal(notes1.length, 3);

  assert.equal(notes1[0].size, 12);
  assert.equal([...notes1[0].keys()].map(hex), [
    "b3063d50864e5e138db87447e0bdaf4cf345c17dd2da0b7ac7abbc08b090e62b",
    "f628a8d5bafffe9943fa61672d00019a136efc418bbbb157df3f08bd964fc13c",
    "275f94a9b14a4b87e9b1921246a55f6095972fefb12a246554d6c276dd8a0f68",
    "e3aa198c485f2c874b8592eb1accf96bddae24787df3c4f300b660d1be97c55b",
    "e6b6da2109e2668b98a01005c90a489fa54493bc5487afe50b8ec1d5f560685d",
    "15230b3e429007eba2c85072bcef3db76f5721203f589de10d24a5c319f0386b",
    "f8a3b6b8466ccd11684398f72abb7ab5c6c1b94891eeca4e100da202a6519317",
    "11e70166fee323718a1b55a950d5701315a509043e26d5591d0a069110c12b12",
    "b76449a3ad7cc5f86b4f318c20273344291aeeb624cfb9580d960ffef4c4926d",
    "65e08a957c08c32aa955f3b5edd13ead24f8d63408679cae381d1f6e2412e914",
    "3abfda24337c08c8da2cb28369a48b55916329038c1287521749cdb9f1efce02",
    "7cd032f0160e0a54ca1de4d86ee8c9981289121982c28a916752ffc06c9fc72a",
  ]);

  assert.equal(notes1[1].size, 1);
  assert.equal([...notes1[1].keys()].map(hex), [
    "1afa317b0d0c8bcf2c08890380954adabb19ffd4bf721422179b6f08b664394a",
  ]);

  assert.equal(notes1[2].size, 1);
  assert.equal([...notes1[2].keys()].map(hex), [
    "7a70d19b83c4722a6b27e2f5712e9ad3b7573656d32ccf847ec95d3ae942955b",
  ]);

  let [notes2] = await ProtocolDriver.mapOwned(owner2, notesBuffer);

  assert.equal(notes2.length, 2);
  assert.equal([...notes2[0].keys()].map(hex), [
    "cc7a09800474fc6668fba1f1b631e940f00309d88a9424fe1c84ca93d627b518",
  ]);

  assert.equal([...notes2[1].keys()].map(hex), [
    "96c24f81d2587017726ec7bbcbbfa80d5f300002c5d8dabe53870e97379d873f",
  ]);

  let [notes3] = await ProtocolDriver.mapOwned(owner3, notesBuffer);

  assert.equal(notes3.length, 1);
  assert.equal([...notes3[0].keys()].map(hex), [
    "81d45c11c5e9b20c2ebba17eaa4f720c669bfd4876cf620280c296225b721c18",
  ]);

  let [notes4] = await ProtocolDriver.mapOwned(owner4, notesBuffer);
  assert.equal(notes4.length, 1);
  assert.equal(notes4[0].size, 0);

  // Create a treasury object for testing
  let treasury = {
    data: {
      "62b5giMnKSpczFSdeLAouS76DZRB6Ny755WTUbJ7sp9dXMJptfe3gknP3XRubWkT1apSZ4YPanSVFjBJBP2SV6wU":
        notes1[0],
      "2cjjDfHEqP3nBQNXqukyKRCJ466VoWGyJmiCbgWuinnK6JEmftzuoHBNjs1gej19A8dgZN8XfGLvKDam2AxJuhya":
        notes2[0],
      "5LGVg71BfjmqV6GEB5pov1ZFNaaVUNsqmmBj1uGCTEND6kbh4w2aq13vXfYtDjNM4VpvpWZdagm7b4XbnxVUJZfU":
        notes3[0],
      "2BqT2oxcE56deFGjKxEpPy3E9NapkiFjzEzDoQAjcmhss4pmYGrfAgRuTrAPe3feGvysymjgP8QFD9M7GcbS2qKi":
        notes4[0],
    },

    address(profile) {
      return this.data[profile];
    },
  };

  let bookkeeper = new Bookkeeper(treasury);

  assert.equal(await bookkeeper.balance(owner1[0].address), {
    value: 67n,
    spendable: 39n,
  });

  assert.equal(await bookkeeper.balance(owner2[0].address), {
    value: 3n,
    spendable: 3n,
  });

  assert.equal(await bookkeeper.balance(owner3[0].address), {
    value: 42n,
    spendable: 42n,
  });

  assert.equal(await bookkeeper.balance(owner4[0].address), {
    value: 0n,
    spendable: 0n,
  });

  let picked = await ProtocolDriver.pickNotes(
    owner1[0].address,
    notes1[0],
    10n,
  );
  assert.equal(picked.size, 4);

  assert.equal(
    await ProtocolDriver.balance(await owner1[0].seed, +owner1[0], picked),
    {
      value: 10n,
      spendable: 10n,
    },
  );

  picked = await ProtocolDriver.pickNotes(owner1[0], notes1[0], 14n);
  assert.equal(picked.size, 4);

  assert.equal(
    await ProtocolDriver.balance(await owner1[0].seed, +owner1[0], picked),
    {
      value: 15n,
      spendable: 15n,
    },
  );

  picked = await ProtocolDriver.pickNotes(owner3[0], notes3[0], 14n);
  assert.equal(picked.size, 1);

  assert.equal(
    await ProtocolDriver.balance(await owner3[0].seed, +owner3[0], picked),
    {
      value: 42n,
      spendable: 42n,
    },
  );

  picked = await ProtocolDriver.pickNotes(owner4, notes4[0], 1n);
  assert.equal(picked.size, 0);

  await ProtocolDriver.unload();
});
