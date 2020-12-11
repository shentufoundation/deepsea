import { Chain } from './chain'
import { printTest } from './utils'

export async function runTest(chain: Chain) {
  await chain.deployContract('builtins')

  const _value = 5;

  let blockNumber = await chain.callMethod('f', [], {value: _value})

  const [
    _address,
    _origin,
    _caller,
    _callvalue,
    _coinbase,
    _timestamp,
    _number,
    _balance_this,
    _blockhash_prev
  ] = await Promise.all([
    chain.callMethod('get_address'),
    chain.callMethod('get_origin'),
    chain.callMethod('get_caller'),
    chain.callMethod('get_callvalue'),
    chain.callMethod('get_coinbase'),
    chain.callMethod('get_timestamp'),
    chain.callMethod('get_number'),
    chain.callMethod('get_balance_this'),
    chain.callMethod('get_blockhash_prev')
  ])

  printTest("address", _address == chain.getContractAddress());
  printTest("origin", _origin == chain.getAccountAddress());
  printTest("caller", _caller == chain.getAccountAddress());
  printTest("callvalue", _callvalue == _value);
  printTest("timestamp", _timestamp == await chain.getBlockTimestamp(blockNumber));
  printTest("number", _number == blockNumber);
  printTest("balance(this_address)", _balance_this == _value);
  printTest("blockhash(parent)", chain.hashToString(_blockhash_prev)
                                 == await chain.getBlockParentHash(blockNumber));
  // the hash of the current block is not available, you just get 0x0 if you try, so we query the parent
}

export default { runTest }
