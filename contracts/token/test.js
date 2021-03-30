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

const abi =
[ {"type":"constructor",
   "name":"constructor",
   "inputs":[],
   "outputs":[],
   "payable":false,
   "constant":false,
   "stateMutability":"nonpayable"},
 {"type":"function",
   "name":"totalSupply",
   "inputs":[],
   "outputs":[{"name":"", "type":"uint256"}],
   "payable":false,
   "constant":true,
   "stateMutability":"view"},
 {"type":"function",
   "name":"balanceOf",
   "inputs":[{"name":"tokenOwner", "type":"address"}],
   "outputs":[{"name":"", "type":"uint256"}],
   "payable":false,
   "constant":true,
   "stateMutability":"view"},
 {"type":"function",
   "name":"transfer",
   "inputs":[{"name":"toA", "type":"address"},{"name":"tokens", "type":"uint256"}],
   "outputs":[{"name":"", "type":"bool"}],
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

  // console.log("calling initalize...");
  // let tx = await contract.initialize();
  // console.log("transaction hash: " + tx.hash);
  let alice = ethers.utils.getAddress("0x0000000000000000000000000000000000000024"); // arbitrary address
  console.log("calling transfer...");
  let tx = await contract.transfer(alice, 100);
  console.log("transaction hash: " + tx.hash);
  let supply = await contract.totalSupply();
  let aliceBalance = await contract.balanceOf(alice);
  let creatorBalance = await contract.balanceOf(creator);
  console.log("total supply: " + supply);
  console.log("creator balance: " + creatorBalance);
  console.log("alice balance: " + aliceBalance);
}

deploy();
