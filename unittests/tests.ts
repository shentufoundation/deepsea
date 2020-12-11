import { ConfluxChain } from './conflux-chain'
import { EthereumChain } from './ethereum-chain'
import { runTest } from './test-unittest'
import yaml from 'js-yaml'
import fs from 'fs'

import builtin from './builtins_test'
import keccak256 from './keccak256_test'
import events from './events_test'

async function main() {
  const tests = yaml.safeLoad(fs.readFileSync('./tests.yml'))

  const chains = [
    new EthereumChain,
    new ConfluxChain
  ]

  const specialTests = {
    builtin: builtin,
    keccak256: keccak256,
    events: events,
  }

  for (const chain of chains) {
    console.log(chain.constructor.name.padEnd(37, '*'))

    // Special tests each have their own file, because they have results that
    // need to be checked against non-constant values, or they look at event
    // logs.
    for (const t in specialTests) {
      console.log(t.padEnd(30, '-'))
      await specialTests[t].runTest(chain)
    }

    // These are tests that can be described in the `tests.json` file. They
    // either check whether a method call passes/reverts or the result of a
    // method's return value against a constant expected value.
    for (const t in tests) {
      console.log(t.padEnd(30, '-'))
      await runTest(chain, t, tests[t].calls, tests[t].constructorArgs);
    }
  }
}

main()
