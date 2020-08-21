#!/usr/bin/env node

unittest = require("./test-unittest");

const testFunctions = [{name: "test_upper", args: [], expectSuccess: true},
		       {name: "get_w", args: [], expectValue: 9}];

unittest.deploy("constructor2.bytecode", "constructor2.abi", testFunctions, [6, 7, 8, 9]);
