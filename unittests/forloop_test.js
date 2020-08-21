#!/usr/bin/env node

unittest = require("./test-unittest");

const bytecodeFilename = "forloop.bytecode";
const abiFilename = "forloop.abi";

const testFunctions = [{name: "multiply", args: [3, 5], expectSuccess: true},
                       {name: "multiply", args: [100, 5], expectSuccess: true},
/* Negative values will underflow to huge 256-bit values, so this would take a while... Also, ethers will complain about passing a negative value to a uint arg.
                       {name: "multiply", args: [1000, -5], expectSuccess: true},
                       {name: "multiply", args: [-3, 7], expectSuccess: true}*/];

unittest.deploy(bytecodeFilename, abiFilename, testFunctions);
