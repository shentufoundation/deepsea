import { Chain } from './chain'
import { ethers } from 'ethers'
import { execSync } from 'child_process';

export async function runTest(chains: Chain[]) {
  const [
    liquidity_chain,
    erc0_chain,
    erc1_chain,
    amm_chain
  ] = chains

  await Promise.all([
    erc0_chain.deployContract('./erc20Token0'),
    erc1_chain.deployContract('./erc20Token1'),
    liquidity_chain.deployContract('./liquidityToken'),
  ])

  const addresses = [
    erc0_chain.getContractAddress(),
    erc1_chain.getContractAddress(),
    liquidity_chain.getContractAddress(),
  ]

  console.log(addresses.join(" "))
  execSync('echo ' + addresses.join(" ") + ' | ./replace_addresses.sh')
  await amm_chain.deployContract('./amm_with_addresses')

  const amm_address = amm_chain.getContractAddress()
  const liquidity_provider = ethers.Wallet.createRandom()
  const user = ethers.Wallet.createRandom()

  try {
    let response = await erc0_chain.callMethod("balanceOf", [await erc0_chain.getAccountAddress()])
    console.log("response (balanceOf):", response.toString())

    response = await erc0_chain.callMethod("transfer", [liquidity_provider.address, 60000])
    console.log("response (transfer erc0 -> liq_prov):", response)

    response = await erc0_chain.callMethod("balanceOf", [liquidity_provider.address])
    console.log("response (balanceOf liq_prov in erc0):", response.toString())

    response = await erc1_chain.callMethod("transfer", [liquidity_provider.address, 40000])
    console.log("response (transfer erc1 -> liq_prov):", response)

    response = await erc0_chain.callMethod("transfer", [amm_address, 500])
    console.log("response (transfer erc0 -> AMM):", response)

    response = await erc1_chain.callMethod("transfer", [amm_address, 500])
    console.log("response (transfer erc1 -> AMM):", response)

    response = await amm_chain.callMethod("mint", [liquidity_provider.address])
    console.log("response (mint):", response)

    // Pretend the user has some erc0 that they want to transfer to AMM
    // (actually going straight from erc0 contract to AMM)
    response = await erc0_chain.callMethod("transfer", [amm_address, 200])
    console.log("response (transfer erc0 -> AMM):", response)

    response = await amm_chain.callMethod("getReserve0")
    console.log("response (getReserve0):", response.toString())

    response = await amm_chain.callMethod("getBalance0")
    console.log("response (getBalance0):", response.toString())

    response = await erc0_chain.callMethod("balanceOf", [user.address])
    console.log("response (balanceOf user in erc0):", response.toString())
    response = await erc1_chain.callMethod("balanceOf", [user.address])
    console.log("response (balanceOf user in erc1):", response.toString())

    response = await amm_chain.callMethod("simpleSwap0", [user.address])
    console.log("response (simpleSwap0):", response)

    response = await erc0_chain.callMethod("balanceOf", [user.address])
    console.log("response (balanceOf user in erc0):", response.toString())
    response = await erc1_chain.callMethod("balanceOf", [user.address])
    console.log("response (balanceOf user in erc1):", response.toString())

  }
  catch (e) {
    console.log(e)
  }
}

export default { runTest }
