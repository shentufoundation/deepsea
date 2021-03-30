set -xe

#caddr=$(certikcli tx cvm deploy --ewasm ewasm/foobar.sol --from node0 --gas-prices 0.025uctk --gas-adjustment 2.0 --gas auto -y -b block | grep -A 1 "key: new-contract-address" | grep -o 'certik\w*')
#certikcli tx cvm call "$caddr" setFoo 17 --from node0 --fees 5000uctk -y -b block
#certikcli tx cvm call "$caddr" getFoo --from node0 --fees 5000uctk -y -b block

#caddr=$(certikcli tx cvm deploy --ewasm ewasm/foobar.wasm --abi ewasm/foobar.abi --from node0 --gas-prices 0.025uctk --gas-adjustment 2.0 --gas auto -y -b block | grep -A 1 "key: new-contract-address" | grep -o 'certik\w*')
#certikcli tx cvm call "$caddr" setFoo 17 --from node0 --fees 5000uctk -y -b block
#certikcli tx cvm call "$caddr" getFoo --from node0 --fees 5000uctk -y -b block

caddr=$(certikcli tx cvm deploy --ewasm --runtime tests/arith.wasm --abi tests/arith.abi --from node0 --gas-prices 0.025uctk --gas-adjustment 2.0 --gas auto -y -b block | grep -A 1 "key: new-contract-address" | grep -o 'certik\w*')
certikcli tx cvm call "$caddr" add 17 2 --from node0 --fees 5000uctk -y -b block
