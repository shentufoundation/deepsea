#!/bin/bash

amm="contracts/amm.ds"

fixed_supply_token='
layer L = {
  o = FixedSupplyToken
}'

liquidity_token='
layer L = {
  o = LiquidityToken 
}'

amm_cut=$( gsed '/\(\* CUT \*\)/Q' "$amm")

echo "$amm_cut" "$fixed_supply_token" | tee erc20Token0.ds erc20Token1.ds >/dev/null
echo "$amm_cut" "$liquidity_token" > liquidityToken.ds
