#!/usr/bin/env node

unittest = require("./test-unittest");

const bytecodeFilename = "layers1.bytecode";
const abiFilename = "layers1.abi";

const testFunctions = [{name: "f", args: [], expectSuccess: true}];

unittest.deploy(bytecodeFilename, abiFilename, testFunctions);
