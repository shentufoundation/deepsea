#!/bin/bash

edsger=../binaries/MacOS/dsc

if [[ ! -d  build ]]; then mkdir build; fi

for deepsea in *.ds; do
  $edsger $deepsea combined-json | python3 clean-json.py > build/${deepsea%.ds}.json
done

npm run tsc
node build/tests.js
