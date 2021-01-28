import { ethers } from 'ethers'
import { Chain, Event } from './chain'
import { cleanCombined, getJsonFilename } from './utils'
import _ from 'lodash'


export class EthereumChain implements Chain {
  private _provider: ethers.providers.JsonRpcProvider
  private _contract: ethers.Contract
  private _contracts: ethers.Contract[]
  private _abi: any[]
  private _signer: ethers.providers.JsonRpcSigner
  private _creator: string

  constructor() {
    const endpoint = "http://localhost:8545";
    this._provider = new ethers.providers.JsonRpcProvider(endpoint);

    this._signer = this._provider.getSigner(0);
    this._contracts = []
  }

  async deployContract(jsonFilename: string, constructorArgs=[]) {
    const combined = require(getJsonFilename(jsonFilename))
    const {abi, bytecode} = cleanCombined(combined)
    this._abi = abi

    let factory = new ethers.ContractFactory(abi, bytecode, this._signer);
    this._contract = await (factory.deploy.apply(factory, constructorArgs));
    await this._contract.deployed();
  }

  async deployContracts(jsonFilenames: string[], constructorArgs: any[][]) {
    for (const i in jsonFilenames) {
      const combined = require(getJsonFilename(jsonFilenames[i]))
      const {abi, bytecode} = cleanCombined(combined)

      let factory = new ethers.ContractFactory(abi, bytecode, this._signer);
      let contract = await (factory.deploy.apply(factory, constructorArgs[i]))
      this._contracts.push(contract);
      await this._contracts[i].deployed(); // TODO: parallelize this loop
    }
  }

  async callRead(func: string, args=[]) {
    return await this._contract[func](...args)
  }

  async callWrite(func: string, args=[], options={}) {
    let receipt = await this._contract[func](...args, options)
    return receipt.blockNumber
  }

  isRead(func: string): boolean {
    const f = this._abi.find(element => element.name == func)
    let readStates = [ "view", "pure" ]
    return readStates.includes(f.stateMutability)
  }

  async callMethod(func: string, args=[], options={}) {
    if (this.isRead(func)) {
      if (!_.isEmpty(options))
        throw Error("Options specified but ignored.")
      return this.callRead(func, args) // returns the read value
    } else
    return this.callWrite(func, args, options) // returns the block number
  }

  getContractAddress(): string {
    return this._contract.address
  }

  getContractAddresses(): string[] {
    return this._contracts.map(c => c.address)
  }

  async getAccountAddress(): Promise<string> {
    if (typeof this._creator === 'undefined' || this._creator === null) {
      this._creator = await this._signer.getAddress();
    }

    return this._creator
  }

  async getBlockTimestamp(blockNumber: number): Promise<number> {
    let block = await this._provider.getBlock(blockNumber)
    return block.timestamp
  }

  async getBlockParentHash(blockNumber: number): Promise<string> {
    let block = await this._provider.getBlock(blockNumber)
    return block.parentHash
  }

  hashToString(hash: any): string {
    return hash.toHexString()
  }

  async getEvents(): Promise<Event[]> {
    const logs = await this._provider.getLogs({address: this._contract.address});
    let iface = new ethers.utils.Interface(this._abi);
    return logs.map((log) => iface.parseLog(log))
  }
}
