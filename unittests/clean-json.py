import sys
import json

s = sys.stdin.read()
obj = json.loads(s)
contracts = obj[list(obj)[0]]
first_contract = contracts[list(contracts)[0]]
first_contract['bytecode'] = '0x' + first_contract['bin']
del first_contract['bin']
first_contract['abi'] = json.loads(first_contract['abi'])
print(json.dumps(first_contract))
