#!/bin/bash

EDSGER=../binaries/MacOS/dsc

# Delete the generated files before running a test again.
function rmtest {
    input=$1
    if [ -f ${input}.bytecode ]; then
	rm ${input}.bytecode
    fi
    if [ -f ${input}.abi ]; then
	rm ${input}.abi
    fi
    if [ -d ${input} ]; then
	rm -r ${input}
    fi
}

function coq_runtest {
     input=$1
     echo "$input" ------------------------------    
     ${EDSGER} ${input}.ds coq  >/dev/null ||  echo edsger faild
     (cd "$input"; coq_makefile -f _CoqProject -o Makefile) >/dev/null 2>&1 || echo coq_makefile failed 
     (cd "$input"; make) > /dev/null 2>&1 || echo make failed 
     cp ../backend/extraction/Asm.ml $input/extraction/
     cp ../backend/extraction/*Ext.ml $input/extraction/
     (cd $input/extraction; ocamlbuild Tester.byte) || echo ocamlbuild failed 
     $input/extraction/Tester.byte bytecode > ${input}.bytecode || echo bytecode compilation failed 
     ${EDSGER} ${input}.ds abi > ${input}.abi
     ./${input}_test.js
}

function runtest {
    input=$1
    echo "$input" ------------------------------
    ${EDSGER} ${input}.ds bytecode > ${input}.bytecode
    ${EDSGER} ${input}.ds abi > ${input}.abi
    ./${input}_test.js
}


if [ "$#" -eq 0 ]; then
    tests=(*_test.js)
else
    tests=("$@")
fi

# You can replace "runtest" with "coq_runtest" below to use the
# Coq compilation path.
for i in "${tests[@]}"; do rmtest ${i%_test.js}; done
for i in "${tests[@]}"; do runtest ${i%_test.js}; done
