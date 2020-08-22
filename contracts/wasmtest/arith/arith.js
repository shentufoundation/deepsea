#!/usr/bin/env node
'strict';
let main = require('../index');

if (process.argv.length != 6) {
    console.log(`usage: ${process.argv[1]} ewasm-file func arg1 arg2`);
    process.exit(0);
}

function generate(funcSig, arg1, arg2) {
    let callData = ''
    switch (funcSig) {
        case 'add':
            callData += '771602f7';
            break;
        case 'sub':
            callData += 'b67d77c5';
            break;
        case 'mul':
            callData += 'c8a4ac9c';
            break;
        case 'div':
            callData += 'a391c15b';
            break;
        // case 'mod':
        //     callData += 'f43f523a';
        //     break;
        // case 'shl':
        //     callData += '9da760ef';
        //     break;
        // case 'shr':
        //     callData += '75f4479a';
        //     break;
        default:
            console.log(`unknown func: ${funcSig}`);
            process.exit(0);
    }
    arg1 = BigInt(arg1);
    callData += arg1.toString(16).padStart(64, '0');
    arg2 = BigInt(arg2);
    callData += arg2.toString(16).padStart(64, '0');
    return callData;
}
let callData = generate(...process.argv.slice(3, 6));
main(process.argv[2], callData).then(console.log).catch(console.log);
