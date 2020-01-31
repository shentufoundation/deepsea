#!/usr/bin/env node

unittest = require("./test-unittest");

const bytecodeFilename = "match-int.bytecode";

const abi = [
  "function f () returns (int)"
];

const testFunctions = [{name: "f", args: [], expectSuccess: true}];

unittest.deploy(bytecodeFilename, abi, testFunctions);
