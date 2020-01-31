#!/usr/bin/env node

unittest = require("./test-unittest");

const bytecodeFilename = "for.bytecode";

const abi = [
  "function multiply (int, int)"
];

const testFunctions = [{name: "multiply", args: [3, 5], expectSuccess: true},
                       {name: "multiply", args: [1000, 5], expectSuccess: true},
/* Negative values will underflow to huge 256-bit values, so this would take a while...
                       {name: "multiply", args: [1000, -5], expectSuccess: true}, */
                       {name: "multiply", args: [-3, 7], expectSuccess: true}];

unittest.deploy(bytecodeFilename, abi, testFunctions);
