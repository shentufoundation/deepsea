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
  "name":"get",
  "inputs":[],
  "outputs":[{"name":"", "type":"uint256"}],
  "payable":false,
  "constant":true,
  "stateMutability":"view"},
 {"type":"function",
   "name":"set",
   "inputs":[{"name":"x", "type":"uint256"}],
   "outputs":[{"name":"", "type":"uint256"}],
   "payable":true,
   "constant":false,
   "stateMutability":"payable"}];

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

  console.log("calling get...");
  let getval = await contract.get();
  console.log("value get as: " + getval);

  let setv = 240; // arbitrary uint
  console.log("calling set...");
  setval = await contract.set(setv);
  console.log("value set as: " + setval);

  console.log("calling get...");
  getval = await contract.get();
  console.log("value get as: " + getval);
}

deploy();
