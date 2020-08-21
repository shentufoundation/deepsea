#!/usr/bin/env node

unittest = require("./test-unittest");

const bytecodeFilename = "struct.bytecode";
const abiFilename = "struct.abi";

const testFunctions = [{name: "initialize", args: [0, 0], expectSuccess: true},
                       {name: "hasOwner", args: [], expectSuccess: false},
                       {name: "hasSupply", args: [5], expectSuccess: false},
                       {name: "initialize", args: [5, 17], expectSuccess: true},
                       {name: "hasSupply", args: [4], expectSuccess: true},
                       {name: "hasSupply", args: [5], expectSuccess: true},
                       {name: "hasSupply", args: [6], expectSuccess: false},
                       {name: "hasOwner", args: [], expectSuccess: true}]; 

unittest.deploy(bytecodeFilename, abiFilename, testFunctions);
