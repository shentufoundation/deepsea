#!/usr/bin/env node

unittest = require("./test-unittest");

const bytecodeFilename = "asserts.bytecode";

const abi = [
  "function f()",
  "function g()",
  "function h()",
  "function i()",
  "function j()",
];

const testFunctions = [{name: "f", args: [], expectSuccess: true},
                       {name: "g", args: [], expectSuccess: false},
                       {name: "h", args: [], expectSuccess: false},
                       {name: "i", args: [], expectSuccess: true},
                       {name: "j", args: [], expectSuccess: false}];

unittest.deploy(bytecodeFilename, abi, testFunctions);
