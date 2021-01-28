#!/bin/bash

# Set this to the location of the conflux repo.
conflux=~/Documents/conflux-rust

cp ./tethys.toml $conflux/run
genPrivateKey=aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
genesis_secrets=$conflux/run/genesis_secrets.txt
if [[ -f $genesis_secrets ]]; then rm $genesis_secrets; fi
echo $genPrivateKey > $genesis_secrets
cd $conflux/run
./clear_state.sh
$conflux/target/release/conflux --config tethys.toml
