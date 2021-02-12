#!/bin/bash

root=$(git rev-parse --show-toplevel)
Edsger=$root/src/Edsger

source_file=$(mktemp)
processed=$(mktemp)
parser=$Edsger/parser.mly
cp $parser $source_file
cppo -n $parser > $processed
cp $processed $parser
menhir $parser
cp $source_file $parser
rm $source_file $processed
