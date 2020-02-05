#!/bin/bash

EDSGER=../binaries/MacOS/dsc

function runtest {
    input=$1
    echo "$input" ------------------------------
    ${EDSGER} ${input}.ds bytecode > ${input}.bytecode
    ./${input}_test.js
}

tests=`ls *_test.js`

for i in $tests; do runtest ${i%_test.js}; done
