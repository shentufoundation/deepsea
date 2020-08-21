#!/usr/bin/env node

// const net = require('net');
// const solc = require('solc');
const fs = require('fs');
const ethers = require("ethers");

const endpoint = "http://localhost:8545";
const provider = new ethers.providers.JsonRpcProvider(endpoint);

const bytecodeFilename = "builtins.bytecode";
const abiFilename = "builtins.abi";

const bytecode = fs.readFileSync(bytecodeFilename).toString().replace(/\n|\t|\r| /g, "");
const abi = JSON.parse(fs.readFileSync(abiFilename).toString());

const signer = provider.getSigner(0);

function printTest(name, success) {
    console.log(name +": " + (success? "pass✅" : "fail❌"));
}

async function deploy() {
  let factory = new ethers.ContractFactory(abi, bytecode, signer);
  let creator = await signer.getAddress();
  let contract = await factory.deploy();
  let tx = await contract.deployed();

  let VALUE = ethers.utils.parseEther('0.1');

  tx = await contract.f({value: VALUE});

  let _address = await contract.get_address();
  let _origin = await contract.get_origin();
  let _caller = await contract.get_caller();
  let _callvalue = await contract.get_callvalue();
  let _coinbase = await contract.get_coinbase();
  let _timestamp = await contract.get_timestamp();
  let _number = await contract.get_number();
  let _balance_this = await contract.get_balance_this();
  let _blockhash_prev = await contract.get_blockhash_prev();  


  blockNumber = await provider.getBlockNumber();
  block = await provider.getBlock (blockNumber)

  printTest("address", _address == contract.address);
  printTest("origin", _origin == creator);
  printTest("caller", _caller == creator);
  printTest("callvalue", _callvalue.eq(VALUE));
  printTest("timestamp", _timestamp.eq(block.timestamp));
  printTest("number", _number.eq(blockNumber));
  printTest("balance(this_address)", _balance_this.eq(VALUE));
  printTest("blockhash(parent)", _blockhash_prev.eq(block.parentHash)); // the hash of the current block is not available, you just get 0x0 if you try, so we query the parent
}

deploy();
