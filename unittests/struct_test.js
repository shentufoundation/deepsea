#!/usr/bin/env node

unittest = require("./test-unittest");

const bytecodeFilename = "struct.bytecode";

const abi = [ {"type":"function",
   "name":"initialize",
   "inputs":[{"name":"_totalSupply", "type":"uint256"},{"name":"_owner", "type":"uint256"}],
   "outputs":[{"name":"", "type":"UNIT"}],
   "payable":"true",
   "constant":false,
   "stateMutability":"payable"},
 {"type":"function",
   "name":"hasOwner",
   "inputs":[],
   "outputs":[{"name":"", "type":"UNIT"}],
   "payable":"true",
   "constant":false,
   "stateMutability":"payable"},
 {"type":"function",
   "name":"hasSupply",
   "inputs":[{"name":"amount", "type":"uint256"}],
   "outputs":[{"name":"", "type":"UNIT"}],
   "payable":"true",
   "constant":false,
   "stateMutability":"payable"}];


const testFunctions = [{name: "initialize", args: [0, 0], expectSuccess: true},
                       {name: "hasOwner", args: [], expectSuccess: false},
                       {name: "hasSupply", args: [5], expectSuccess: false},
                       {name: "initialize", args: [5, 17], expectSuccess: true},
                       {name: "hasSupply", args: [4], expectSuccess: true},
                       {name: "hasSupply", args: [5], expectSuccess: true},
                       {name: "hasSupply", args: [6], expectSuccess: false},
                       {name: "hasOwner", args: [], expectSuccess: true}]; 

unittest.deploy(bytecodeFilename, abi, testFunctions);
