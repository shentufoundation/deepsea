#!/bin/bash

# Add deepsea (.ds) or minic (.mc) files to this directory to add new unit
# tests. Deepsea files are first prett-printed to minic source. Then, all minic
# files are parsed and printed twice and checked for any differences.

set -e

root=$(git rev-parse --show-toplevel)
test_dir=$root/minic/tests
minicc=$root/minicc.bc
deepsea_compiled=$(mktemp -d)

skip_files="swaps_threeway.ds swaps_general.ds spblind.ds fpblind.ds defi.ds"

for deepsea in $(find -L $test_dir -name '*.ds'); do
	bname=$(basename $deepsea)
	if echo $skip_files | grep -q -w $bname; then
		continue
	fi
	echo Compiling $bname

	$root/edsger.bc $deepsea minic > $deepsea_compiled/${bname%.*}.mc
	# $root/edsger.bc $deepsea bytecode > $deepsea_compiled/${bname%.*}.bc
done

echo -e '\nRunning tests'

for mc in $test_dir/*.mc $deepsea_compiled/*; do
	echo $(basename $mc)
	tmp1=$(mktemp)
	tmp2=$(mktemp)
	trap "diff -u $tmp1 $tmp2 | diff-so-fancy" ERR

	$minicc $mc > $tmp1
	$minicc $tmp1 > $tmp2
	diff -u $tmp1 $tmp2 | diff-so-fancy


	# $minicc $mc bytecode > $tmp1
	# diff -u ${mc%.*}.bc $tmp1 | diff-so-fancy

	rm $tmp1 $tmp2
done

rm -r $deepsea_compiled
