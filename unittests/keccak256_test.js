#!/usr/bin/env node

const bytecodeFilename = "keccak256.bytecode";
const abiFilename = "keccak256.abi";

// const net = require('net');
// const solc = require('solc');
const fs = require('fs');
const ethers = require("ethers");

const endpoint = "http://localhost:8545";
const provider = new ethers.providers.JsonRpcProvider(endpoint);

const bytecode = fs.readFileSync(bytecodeFilename).toString().replace(/\n|\t|\r| /g, "");
const abi = JSON.parse(fs.readFileSync(abiFilename).toString());
const signer = provider.getSigner(0);

function printTest(name, success) {
    console.log(name +": " + (success? "pass✅" : "fail❌"));
}

async function deploy() {
  let factory = new ethers.ContractFactory(abi, bytecode, signer);
  let contract = await factory.deploy();
  let tx = await contract.deployed();

  let x = "0x000000000000000000000000000000000000000000000000000000000000002a";
  let x_hash = await contract.hash1(x);
  printTest("hash1", x_hash.eq(ethers.utils.keccak256(x)));

  let y = "0x000000000000000000000000000000000000000000000000000000000000002b";
  let xy_hash = await contract.hash2(x,y);
  printTest("hash2", xy_hash.eq(ethers.utils.keccak256(ethers.utils.concat([x,y]))));

  let xy_hash_1 = await contract.hash2_1(x,y);
  printTest("hash2_1", xy_hash_1.eq(ethers.utils.keccak256(ethers.utils.concat([x,y]))));  

  let xy_hash_2 = await contract.hash2_2(x,y);
  printTest("hash2_2", xy_hash_2.eq(ethers.utils.keccak256(ethers.utils.concat([x,y]))));  
  
  let xy_hash_3 = await contract.hash2_3(x,y);
    printTest("hash2_3", xy_hash_3.eq(ethers.utils.keccak256(ethers.utils.concat([x,y]))));

  // Note that below, we get different results for the hashes of addresses than Solidity would give,
  // becuase we have more zero-padding.
    
  let xa = ethers.utils.getAddress("0x000000000000000000000000000000000000002a");
  let xa_hash = await contract.hash1a(xa);
  printTest("hash1a", xa_hash.eq(ethers.utils.keccak256(x))); //note: x, not xa.

  let xy_hash_1a = await contract.hash2_1a(xa,y);
  printTest("hash2_1a", xy_hash_1.eq(ethers.utils.keccak256(ethers.utils.concat([x,y])))); //note: x, not xa
    
}

deploy();
