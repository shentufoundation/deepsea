#!/usr/bin/env node

const bytecodeFilename = "events.bytecode";
const abiFilename = "events.abi";

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
  let iface = new ethers.utils.Interface(abi);
  let contract = await factory.deploy();
  let tx = await contract.deployed();

  retval = await contract.f();
  //console.log(x, retval, x_hash, x_hash2);

  let filter = {
      address: contract.address
  };

  const logs = await provider.getLogs(filter);
  let events = logs.map((log) => iface.parseLog(log))
  //console.log(retval);
  //console.log(logs);
  //console.log(events);

  printTest("E1 signature", events[0].signature == "E1(uint256)");
  printTest("E1 values", events[0].args['v'] == 42);
  printTest("E2 signature", events[1].signature == "E2(uint256,bool)");
  printTest("E2 values",   events[1].args['v1'] == 1 
                        && events[1].args['v2'] == false);
  printTest("IE1 signature", events[2].signature == "IE1(uint256)");
  printTest("IE1 values", events[2].args['v'] == 13);
  printTest("IE2 signature", events[3].signature == "IE2(uint256,bool)");
  printTest("IE2 values",    events[3].args['v1'] == 2 
                          && events[3].args['v2'] == false);
  printTest("Transfer signature", events[4].signature == "Transfer(uint256,uint256,uint256)");
  printTest("Transfer values",    events[4].args['fromA'] == 1
                          && events[4].args['toA'] == 2
	                  && events[4].args['value'] == 42);
  printTest("Approval signature", events[5].signature == "Approval(uint256,uint256,uint256)");
  printTest("Approval values",    events[5].args['owner'] == 1
                          && events[5].args['spender'] == 2
	                  && events[5].args['value'] == 42);
}

deploy();
