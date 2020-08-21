#!/usr/bin/env node

unittest = require("./test-unittest");

const testFunctions = [{name: "get", args: [], expectSuccess: true}];

unittest.deploy("constructor.bytecode", "constructor.abi", testFunctions, [100, 321]);
