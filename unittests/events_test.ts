import { Chain } from './chain'
import { printTest } from './utils'

export async function runTest(chain: Chain) {
  await chain.deployContract('events')

  await chain.callMethod("f");

  let events = await chain.getEvents();

  printTest("E0 signature",       events[0].signature == "E0()");
  printTest("E1 signature",       events[1].signature == "E1(uint256)");
  printTest("E1 values",          events[1].args['v'] == 42);
  printTest("E2 signature",       events[2].signature == "E2(uint256,bool)");
  printTest("E2 values",          events[2].args['v1'] == 1
                               && events[2].args['v2'] == false);
  printTest("IE1 signature",      events[3].signature == "IE1(uint256)");
  printTest("IE1 values",         events[3].args['v'] == 13);
  printTest("IE2 signature",      events[4].signature == "IE2(uint256,bool)");
  printTest("IE2 values",         events[4].args['v1'] == 2
                               && events[4].args['v2'] == false);
  printTest("Transfer signature", events[5].signature == "Transfer(uint256,uint256,uint256)");
  printTest("Transfer values",    events[5].args['fromA'] == 1
                               && events[5].args['toA'] == 2
                               && events[5].args['value'] == 42);
  printTest("Approval signature", events[6].signature == "Approval(uint256,uint256,uint256)");
  printTest("Approval values",    events[6].args['owner'] == 1
                               && events[6].args['spender'] == 2
                               && events[6].args['value'] == 42);
}

export default { runTest }
