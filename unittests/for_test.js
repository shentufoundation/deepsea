#!/usr/bin/env node

unittest = require("./test-unittest");

const bytecodeFilename = "for.bytecode";

const abi = [ {"type":"function",
   "name":"multiply",
   "inputs":[{"name":"a", "type":"uint256"},{"name":"b", "type":"uint256"}],
   "outputs":[],
   "payable":true,
   "constant":false,
   "stateMutability":"payable"}];

const testFunctions = [{name: "multiply", args: [3, 5], expectSuccess: true},
                       {name: "multiply", args: [1000, 5], expectSuccess: true},
/* Negative values will underflow to huge 256-bit values, so this would take a while... Also, ethers will complain about passing a negative value to a uint arg.
                       {name: "multiply", args: [1000, -5], expectSuccess: true},
                       {name: "multiply", args: [-3, 7], expectSuccess: true}*/];

unittest.deploy(bytecodeFilename, abi, testFunctions);
