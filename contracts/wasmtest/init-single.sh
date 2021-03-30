#!/bin/bash

set -xe

certikd unsafe-reset-all
rm -rf ~/.certikd
rm -rf ~/.certikcli

# mkdir -p ~/.certikcli/config
# cp toolsets/oracle-operator/oracle-operator.toml ~/.certikcli/config/

certikd init testnode0 --chain-id shentu

certikcli config chain-id shentu
certikcli config keyring-backend test
certikcli keys add node0 --keyring-backend test

certikd add-genesis-account $(certikcli keys show node0 --keyring-backend test -a) 2000000000000uctk
certikd add-genesis-certifier $(certikcli keys show node0 --keyring-backend test -a)

#echo jkljkljlk | certikd gentx --name node0 --amount 1000000000000uctk
certikd gentx --name node0 --amount 1000000000000uctk --keyring-backend test
certikd collect-gentxs

certikd start
