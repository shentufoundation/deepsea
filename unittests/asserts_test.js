#!/usr/bin/env node

unittest = require("./test-unittest");

const testFunctions = [{name: "f", args: [], expectSuccess: true},
                       {name: "g", args: [], expectSuccess: false},
                       {name: "h", args: [], expectSuccess: false},
                       {name: "i", args: [], expectSuccess: true},
                       {name: "j", args: [], expectSuccess: false}];

unittest.deploy("asserts.bytecode", "asserts.abi", testFunctions);
