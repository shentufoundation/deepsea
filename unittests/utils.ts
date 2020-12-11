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
