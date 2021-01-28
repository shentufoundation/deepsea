#!/bin/bash

dsc=../binaries/MacOS/dsc

if [[ ! -d  build ]]; then mkdir build; fi

./cut_amm.sh

for deepsea in ./contracts/*.ds; do
	basename=${deepsea##*/}
  echo Compiling $basename
  $dsc $deepsea combined-json > build/${basename%.ds}.json
done

npm run tsc
node build/tests.js
