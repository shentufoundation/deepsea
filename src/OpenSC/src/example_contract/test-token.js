#!/usr/bin/env node

// const net = require('net');
// const solc = require('solc');
const fs = require('fs');
const ethers = require("ethers");

if (process.argv.length != 3) {
    console.log("invalid args");
    process.exit(1);
}

const endpoint = "http://localhost:8545";
const provider = new ethers.providers.JsonRpcProvider(endpoint);

// const abi = [
//   "constructor()",
//   "function initialize () public returns (uint)",
//   "function totalSupply() view returns (uint)",
//   "function balanceOf(uint) view returns (uint)",
//   "function transfer(uint, int) returns (bool)"
// ];

const abi = [ 
 {"type":"function",
   "name":"totalSupply",
   "inputs":[],
   "outputs":[{"name":"", "type":"uint256"}],
   "payable":false,
   "constant":true,
   "stateMutability":"view"},
 {"type":"function",
   "name":"balanceOf",
   "inputs":[{"name":"a", "type":"uint256"}],
   "outputs":[{"name":"", "type":"uint256"}],
   "payable":false,
   "constant":true,
   "stateMutability":"view"},
   {"type":"function",
   "name":"allowance",
   "inputs":[{"name":"owner", "type":"uint256"},{"name":"spender", "type":"uint256"}],
   "outputs":[{"name":"", "type":"uint256"}],
   "payable":false,
   "constant":true,
   "stateMutability":"view"},
 {"type":"function",
   "name":"transfer",
   "inputs":[{"name":"a", "type":"uint256"},{"name":"v", "type":"uint256"}],
   "outputs":[{"name":"", "type":"bool"}],
   "payable":true,
   "constant":false,
   "stateMutability":"payable"},
   {"type":"function",
   "name":"approve",
   "inputs":[{"name":"spender", "type":"uint256"},{"name":"v", "type":"uint256"}],
   "outputs":[{"name":"", "type":"bool"}],
   "payable":true,
   "constant":false,
   "stateMutability":"payable"},
   {"type":"function",
   "name":"transferFrom",
   "inputs":[{"name":"from", "type":"uint256"},{"name":"to", "type":"uint256"},{"name":"v", "type":"uint256"}],
   "outputs":[{"name":"", "type":"bool"}],
   "payable":true,
   "constant":false,
   "stateMutability":"payable"},
  ];

const bytecode = fs.readFileSync(process.argv[2]).toString().replace(/\n|\t|\r| /g, "");
const signer = provider.getSigner(0);
const creator = signer.getAddress();

async function deploy() {
  console.log("sending creation transaction...")
  let factory = new ethers.ContractFactory(abi, bytecode, signer);
  let contract = await factory.deploy();
  await contract.deployed();
  console.log("contract address: " + contract.address);
  console.log("transaction hash: " + contract.deployTransaction.hash);
  let deployedBytecode = await provider.getCode(contract.address);
  // console.log("deployed bytecode: " + deployedBytecode);

  let alice = 24; // arbitrary address
  console.log("calling transfer...");
  tx = await contract.transfer(alice, 100);
  console.log("transaction hash: " + tx.hash);
  let supply = await contract.totalSupply();
  let aliceBalance = await contract.balanceOf(alice);
  let creatorBalance = await contract.balanceOf(creator);
  console.log("total supply: " + supply);
  console.log("creator balance: " + creatorBalance);
  console.log("alice balance: " + aliceBalance);
}

deploy();
