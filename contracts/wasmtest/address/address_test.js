#!/usr/bin/env node

// const net = require('net');
// const solc = require('solc');

const bytecodeFilename = "address.bytecode";

const fs = require('fs');
const ethers = require("ethers");

const endpoint = "http://localhost:8545";
const provider = new ethers.providers.JsonRpcProvider(endpoint);

// const abi = [
//   "constructor()",
//   "function initialize () public returns (uint)",
//   "function totalSupply() view returns (uint)",
//   "function balanceOf(uint) view returns (uint)",
//   "function transfer(uint, int) returns (bool)"
// ];

const abi = [ {"type":"function",
  "name":"init",
  "inputs":[],
  "outputs":[],
  "payable":true,
  "constant":false,
  "stateMutability":"payable"},
  {"type":"function",
  "name":"get",
  "inputs":[],
  "outputs":[{"name":"", "type":"address"}],
  "payable":true,
  "constant":true,
  "stateMutability":"payable"},
  {"type":"function",
  "name":"set",
  "inputs":[{"name":"x", "type":"address"}],
  "outputs":[],
  "payable":true,
  "constant":false,
  "stateMutability":"payable"}];

const bytecode = fs.readFileSync(bytecodeFilename).toString().replace(/\n|\t|\r| /g, "");
const signer = provider.getSigner(0);
const creator = signer.getAddress();

function printTest(name, success) {
  console.log(name +": " + (success? "pass" : "fail"));
}

async function deploy() {
  //console.log("sending creation transaction...")
  let factory = new ethers.ContractFactory(abi, bytecode, signer);
  let contract = await factory.deploy();
  await contract.deployed();
  // console.log("contract address: " + contract.address);
  // console.log("transaction hash: " + contract.deployTransaction.hash);
  // let deployedBytecode = await provider.getCode(contract.address);
  // console.log("deployed bytecode: " + deployedBytecode);

  
  await contract.init();

  let getval = await contract.get();
  printTest("init value: ", getval == "0x000000000000000000000000000000000000efca");

  let setv = "0xd115bffabbdd893a6f7cea402e7338643ced44a6"; // 160-bit address format
  await contract.set(setv);

  getval = await contract.get();
  printTest("set value: ", getval == ethers.utils.getAddress(setv));

}

deploy();
