#!/bin/bash

# Add deepsea (.ds) or minic (.mc) files to this directory to add new unit
# tests. Deepsea files are first prett-printed to minic source. Then, all minic
# files are parsed and printed twice and checked for any differences.

minicc=../binaries/MacOS/minicc
dsc=../binaries/MacOS/dsc

skip_files="token_ant.ds"

deepsea_compiled=$(mktemp -d)
tmp1=$(mktemp)
tmp2=$(mktemp)

on_exit () {
  echo -e '\nCleaning up...'
  rm -r "$deepsea_compiled"
  rm $tmp1 $tmp2
  echo $pass_count passed, $fail_count failed
}
trap on_exit EXIT

for f in ./unittests/*.ds ./contracts/*/*.ds; do
  bname=$(basename $f)
  if echo $skip_files | grep -q -w $bname; then
    continue
  fi
  echo Compiling $bname

  $dsc $f minic > $deepsea_compiled/${bname%.*}.mc
done

echo -e '\nRunning tests'

fail_count=0
pass_count=0

for mc in ./*.mc $deepsea_compiled/*; do
  echo -n $(basename $mc)

  $minicc $mc > $tmp1
  $minicc $tmp1 > $tmp2

  if diff -u $tmp1 $tmp2; then
    echo
    ((pass_count++))
  else
    echo : FAIL
    ((fail_count++))
  fi
done
