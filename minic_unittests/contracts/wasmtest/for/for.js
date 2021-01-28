#!/usr/bin/env node
'strict';
let main = require('../index');

if (process.argv.length < 4) {
    console.log(`usage: ${process.argv[1]} ewasm-file func  [args..]`);
    process.exit(0);
}

function generate(funcSig, arg1, arg2) {
    let callData = ''
    switch (funcSig) {
        case 'multiply':
            callData += '165c4a16';
            break;
        default:
            console.log(`unknown func: ${funcSig}`);
            process.exit(0);
    }
    arg1 = BigInt(arg1);
    callData += arg1.toString(16).padStart(64, '0');
    if (arg2 !== undefined) {
        arg2 = BigInt(arg2);
        callData += arg2.toString(16).padStart(64, '0');
    }
    return callData;
}
let callData = generate(...process.argv.slice(3, 6));
main(process.argv[2], callData).then(console.log).catch(console.log);
