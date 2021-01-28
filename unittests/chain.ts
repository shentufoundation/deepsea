export interface Chain {
  deployContract(jsonFilename: string, constructorArgs?: any[]): Promise<void>
  callMethod(func: string, args?: any[], options?: {}): any

  getContractAddress(): string
  getAccountAddress(): Promise<string>
  getBlockTimestamp(blockNumber: number): Promise<number>
  getBlockParentHash(blockNumber: number): Promise<string>
  getEvents(): Promise<Event[]>

  hashToString(hash: any): string
}

export interface Event {
  signature: string 
  args: {}
}
