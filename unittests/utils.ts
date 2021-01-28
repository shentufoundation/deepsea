export function getJsonFilename(filename: string): string {
  if (!filename.startsWith('./'))
    filename = './' + filename
  if (!filename.endsWith('.json'))
    filename = filename + '.json'
  return filename
}


export function padName(name: string): string {
  return ("  " + name + ":").padEnd(30, ' ')
}

export function printTest(name: string, success: boolean) {
  console.log(padName(name), (success? "pass✅" : "fail❌"));
}

export function cleanCombined(combined: {}): abiBytecode {
	const contracts = combined["contracts"]
	const name = Object.keys(contracts)[0]
	const contract = contracts[name]
	return {
		abi: JSON.parse(contract["abi"]),
		bytecode: '0x' + contract["bin"]
	}
}

interface abiBytecode {
	abi
	bytecode: string
}
