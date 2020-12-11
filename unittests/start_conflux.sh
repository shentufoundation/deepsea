#!/bin/bash

# Set this to the location of the conflux repo.
conflux=~/Documents/conflux-rust

cp ./tethys.toml ./genesis_secrets.txt $conflux/run
cd $conflux/run
./clear_state.sh
$conflux/target/release/conflux --config tethys.toml
