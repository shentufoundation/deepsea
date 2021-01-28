#!/bin/bash

fail_count=0
pass_count=0

minicc=../binaries/MacOS/minicc
dsc=../binaries/MacOS/dsc

skip_files="token_ant.ds"

edsger_out_mc=$(mktemp)
edsger_out_asmbl=$(mktemp)
minicc_out_mc=$(mktemp)
minicc_out_asmbl=$(mktemp)

on_exit() {
  echo -e '\nCleaning up...'
  rm $edsger_out_mc $edsger_out_asmbl \
    $minicc_out_mc $minicc_out_asmbl
  echo $pass_count passed, $fail_count failed
}
trap on_exit EXIT

for f in ./unittests/*.ds ./contracts/*/*.ds; do
  bname=$(basename $f)
  if echo $skip_files | grep -q -w $bname; then
    continue
  fi

  echo -n $f

  $dsc $f minic > $edsger_out_mc
  $dsc $f assembly > $edsger_out_asmbl
  $minicc $edsger_out_mc print > $minicc_out_mc
  $minicc $edsger_out_mc assembly > $minicc_out_asmbl

  if cmp -s $edsger_out_asmbl $minicc_out_asmbl; then
    echo
    ((pass_count++))
  else
    echo : FAIL
    ((fail_count++))
  fi
done
