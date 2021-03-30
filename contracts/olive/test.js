#!/usr/bin/env node

const fs = require('fs');
const ethers = require("ethers");

if (process.argv.length != 3) {
    console.log("invalid args");
    process.exit(1);
}

const endpoint = "http://localhost:8545";
const provider = new ethers.providers.JsonRpcProvider(endpoint);

const abi = [ {"type":"constructor",
   "name":"constructor",
   "inputs":[],
   "outputs":[],
   "payable":false,
   "constant":false,
   "stateMutability":"nonpayable"},
 {"type":"function",
   "name":"balanceOf",
   "inputs":[{"name":"addr", "type":"address"}],
   "outputs":[{"name":"", "type":"uint256"}],
   "payable":false,
   "constant":true,
   "stateMutability":"view"},
 {"type":"function",
   "name":"transfer",
   "inputs":[{"name":"to_", "type":"address"},{"name":"value", "type":"uint256"},{"name":"now", "type":"uint256"}],
   "outputs":[{"name":"", "type":"bool"}],
   "payable":true,
   "constant":false,
   "stateMutability":"payable"},
 {"type":"function",
   "name":"allowance",
   "inputs":[{"name":"owner", "type":"address"},{"name":"spender", "type":"address"}],
   "outputs":[{"name":"", "type":"uint256"}],
   "payable":true,
   "constant":false,
   "stateMutability":"payable"},
 {"type":"function",
   "name":"transferFrom",
   "inputs":[{"name":"from_", "type":"address"},{"name":"to_", "type":"address"},{"name":"value", "type":"uint256"},{"name":"now", "type":"uint256"}],
   "outputs":[{"name":"", "type":"bool"}],
   "payable":true,
   "constant":false,
   "stateMutability":"payable"},
 {"type":"function",
   "name":"approve",
   "inputs":[{"name":"spender", "type":"address"},{"name":"value", "type":"uint256"}],
   "outputs":[{"name":"", "type":"bool"}],
   "payable":true,
   "constant":false,
   "stateMutability":"payable"},
 {"type":"function",
   "name":"increaseApproval",
   "inputs":[{"name":"spender", "type":"address"},{"name":"addedValue", "type":"uint256"}],
   "outputs":[{"name":"", "type":"bool"}],
   "payable":true,
   "constant":false,
   "stateMutability":"payable"},
 {"type":"function",
   "name":"decreaseApproval",
   "inputs":[{"name":"_spender", "type":"address"},{"name":"_subtractedValue", "type":"uint256"}],
   "outputs":[{"name":"", "type":"bool"}],
   "payable":true,
   "constant":false,
   "stateMutability":"payable"},
 {"type":"event",
   "name":"OwnershipTransferred",
   "inputs":[{"name":"previousOwner", "type":"address", "internalType": "address", "indexed": true}, {"name":"newOwner", "type":"address", "internalType": "address", "indexed": true}]},
 {"type":"event",
   "name":"Pause",
   "inputs":[{"name":"v", "type":"bool", "internalType": "bool", "indexed": false}]},
 {"type":"event",
   "name":"Unpause",
   "inputs":[{"name":"v", "type":"bool", "internalType": "bool", "indexed": false}]},
 {"type":"event",
   "name":"Transfer",
   "inputs":[{"name":"from", "type":"address", "internalType": "address", "indexed": true}, {"name":"to_", "type":"address", "internalType": "address", "indexed": true}, {"name":"value", "type":"uint256", "internalType": "uint256", "indexed": false}]},
 {"type":"event",
   "name":"Approval",
   "inputs":[{"name":"owner", "type":"address", "internalType": "address", "indexed": true}, {"name":"spender", "type":"address", "internalType": "address", "indexed": true}, {"name":"value", "type":"uint256", "internalType": "uint256", "indexed": false}]}]

      
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
  console.log("deployed bytecode: " + deployedBytecode);

  // contract.on("Transfer", (from, to, val) => {
  //     console.log("Transfer event");
  //     console.log(from, to, val);
  // });

  console.log("calling balanceOf creator...");
  let balanceOf = await contract.balanceOf(creator);
  console.log("value get as: " + balanceOf);
  // console.log(balanceOf);
  let alice = ethers.utils.getAddress("0x0000000000000000000000000000000000000024"); // arbitrary address

  // console.log("calling approve...");
  // let approve_res = await contract.approve(creator, 1000);
  // console.log("approve result is " + approve_res);


  // console.log("calling allowance");
  // let allowance_res = await contract.allowance(creator, alice);
  // console.log("allowance result is " + allowance_res);

  console.log("calling approve... ");
  let approve_res2 = await contract.approve(alice, 100);
  console.log("approve result is " + approve_res2);

  console.log("calling allowance ");
  let allowance_res2 = await contract.allowance(creator, alice);
  console.log("allowance result is " + allowance_res2);

  console.log("calling transfer...");
  // to_, value, now
  let transfer_res = await contract.transfer(alice, 100, 100);
  console.log("transfer result is: " + transfer_res);
  


  console.log("calling balanceOf alice... after receive the token");
  let balanceOfreceiver = await contract.balanceOf(alice);
  console.log("value get as after receiving: " + balanceOfreceiver);

  console.log("calling balanceOf creator... after receive the token");
  let balanceOfcreator = await contract.balanceOf(creator);
  console.log("value get as after receiving: " + balanceOfcreator);
  //console.log(balanceOf);

}

deploy();
