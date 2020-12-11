import { ethers } from 'ethers'
import { Chain } from './chain'
import { printTest } from './utils'

function checkHash(chain: Chain, hash1, vals: string[]) {
  return chain.hashToString(hash1) == ethers.utils.keccak256(ethers.utils.concat(vals))
}

export async function runTest(chain: Chain) {
  await chain.deployContract('keccak256')

  let x = "0x000000000000000000000000000000000000000000000000000000000000002a";
  let x_hash = await chain.callMethod("hash1", [x])
  printTest("hash1", checkHash(chain, x_hash, [x]))

  let y = "0x000000000000000000000000000000000000000000000000000000000000002b";
  let xy_hash = await chain.callMethod("hash2", [x,y]);
  printTest("hash2", checkHash(chain, xy_hash, [x,y]))

  let xy_hash_1 = await chain.callMethod("hash2_1", [x,y]);
  printTest("hash2_1", checkHash(chain, xy_hash_1, [x,y]))

  let xy_hash_2 = await chain.callMethod("hash2_2", [x,y]);
  printTest("hash2_2", checkHash(chain, xy_hash_2, [x,y]))

  let xy_hash_3 = await chain.callMethod("hash2_3", [x,y]);
  printTest("hash2_3", checkHash(chain, xy_hash_3, [x,y]))

  // Note that below, we get different results for the hashes of addresses than Solidity would give,
  // becuase we have more zero-padding.

  let xa = ethers.utils.getAddress("0x000000000000000000000000000000000000002a");
  let xa_hash = await chain.callMethod("hash1a", [xa]);
  printTest("hash1a", checkHash(chain, xa_hash, [x])) //note: x, not xa.

  let xy_hash_1a = await chain.callMethod("hash2_1a", [xa,y]);
  printTest("hash2_1a", checkHash(chain, xy_hash_1, [x,y])) //note: x, not xa.

}

export default { runTest }
