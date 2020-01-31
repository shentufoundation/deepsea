#!/usr/bin/env node

// TODO: test withdrawals

const fs     = require('fs');
const ethers = require('ethers');
const assert = require('assert');

const bn = ethers.utils.bigNumberify;
const pe = ethers.utils.parseEther;

if (process.argv.length != 3) {
    console.log("invalid args");
    process.exit(1);
}

const endpoint = "http://localhost:8545";
const provider = new ethers.providers.JsonRpcProvider(endpoint);

const abi = [
  "constructor()",
  "function initialize(uint) public",
  "function getbid() public view returns (uint)",
  "function getbidder() public view returns (address)",
  "function getchair() public view returns (address)",
  "function getdeadline() public view returns (uint)",
  "function bid() public returns (bool)"
];

const bytecode = fs.readFileSync(process.argv[2]).toString().replace(/\n|\t|\r| /g, "");
const DEADLINE = 1000;
const CREATOR  = provider.getSigner(0);

async function deploy(deadline) {
  let factory = new ethers.ContractFactory(abi, bytecode, CREATOR);
  let auction = await factory.deploy();
  await auction.deployed();
  let tx = await auction.initialize(deadline);
  await tx.wait();
  return auction.address;
}

async function read(auction) {
  let bid      = await auction.getbid      ();
  let bidder   = await auction.getbidder   ();
  let chair    = await auction.getchair    ();
  let deadline = await auction.getdeadline ();
  return [bid, bidder, chair, deadline];
}

async function show(auction) {
  let bid, bidder, chair, deadline;
  [bid, bidder, chair, deadline] = await read(auction);
  console.log("bid      : " + bid     );
  console.log("bidder   : " + bidder  );
  console.log("chair    : " + chair   );
  console.log("deadline : " + deadline);
}

async function testConstructor() {
  let addr    = await deploy(DEADLINE);
  let signer  = provider.getSigner(1);
  let auction = new ethers.Contract(addr, abi, signer);
  let bid, bidder, chair, deadline;

  [bid, bidder, chair, deadline] = await read(auction);
  assert(bid.eq(bn(0)));             // bid == 0
  assert(deadline.eq(bn(DEADLINE))); // deadline == DEADLINE
  assert.strictEqual(bidder, await CREATOR.getAddress());
  assert.strictEqual(chair , await CREATOR.getAddress());
  console.log("constructor: passing");
}

async function testBids () {
  let alice = provider.getSigner(1);
  let bob   = provider.getSigner(2);
  let bid, bidder, chair, deadline;
  let addr, auction, tx, overrides;
  const BID0 = pe('0.1');
  const BID1 = pe('0.2');

  // deploy
  addr  = await deploy(DEADLINE);
  auction = new ethers.Contract(addr, abi, alice);

  // valid bid
  tx = await auction.bid({ value: BID0 });
  [bid, bidder, chair, deadline] = await read(auction);
  assert(bid.eq(BID0));
  assert(deadline.eq(bn (DEADLINE)));
  assert.strictEqual(bidder, await alice.getAddress()  );
  assert.strictEqual(chair , await CREATOR.getAddress());
  console.log("valid bid: passing");

  // invalid bid (same bidder)
  tx = await auction.bid({ value: BID1 });
  [bid, bidder, chair, deadline] = await read(auction);
  assert(bid.eq(BID0));
  assert(deadline.eq(bn(DEADLINE)));
  assert.strictEqual(bidder, await alice.getAddress()  );
  assert.strictEqual(chair , await CREATOR.getAddress());
  console.log("invalid bid (bidder): passing");

  // invalid bid (insufficient value)
  auction  = new ethers.Contract (addr, abi, bob);
  tx = await auction.bid({ value: pe ('0.0') });
  [bid, bidder, chair, deadline] = await read(auction);
  assert(bid.eq(BID0));
  assert(deadline.eq(bn(DEADLINE)));
  assert.strictEqual(bidder, await alice.getAddress()  );
  assert.strictEqual(chair , await CREATOR.getAddress());
  console.log("invalid bid (value): passing");

  // invalid bid (deadline)
  addr = await deploy(0);
  auction = new ethers.Contract(addr, abi, alice);
  tx = await auction.bid({ value: BID0 });
  [bid, bidder, chair, deadline] = await read(auction);
  assert(bid.eq(pe('0.0')));
  assert(deadline.eq(bn(0)));
  assert.strictEqual(bidder, await CREATOR.getAddress());
  assert.strictEqual(chair , await CREATOR.getAddress());
  console.log("invalid bid (deadline): passing");
}

async function main() {
  await testConstructor();
  await testBids();
}

main();
