import { Conflux, Account } from 'js-conflux-sdk'
import { Chain, Event } from './chain'
import { cleanCombined, getJsonFilename } from './utils'
import _ from 'lodash'


export class ConfluxChain implements Chain {
  private _conflux: Conflux
  private _contract: any
  private _genesisAccount: Account
  private _abi: any[]

  constructor(verbose=false) {
    let logger = this.getLogger(verbose)

    this._conflux = new Conflux({
      url: 'http://localhost:12537',
      // defaultGasPrice: 1000,
      // defaultGas: 1000,
      // logger: this._logger,
      logger: logger,
    })

    const genesisPrivateKey =
      '0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'
    this._genesisAccount = this._conflux.wallet.addPrivateKey(genesisPrivateKey)
  }
  private getLogger(verbose: boolean): Logger {
    if (verbose)
      return console

    let logger: Logger = {
      info: function() {}, // Output nothing.
      error: function() {}
    }
    return logger
  }

  async deployContract(jsonFilename: string, constructorArgs=[]) {
    const combined = require(getJsonFilename(jsonFilename))
    const {abi, bytecode} = cleanCombined(combined)
    this._contract = this._conflux.Contract({ abi, bytecode })
    this._abi = abi

    const receipt = await this._contract
      .constructor(...constructorArgs)
      .sendTransaction({ from: this._genesisAccount })
      .executed();
    this._contract.address = receipt.contractCreated;
  }

  async callRead(func: string, args=[]){
    return await this._contract[func](...args)
  }

  async callWrite(func: string, args=[], options={}) {
    // console.log("func: " + func)
    // console.log("args: " + args)
    // console.log("options: " + JSON.stringify(options))
    let receipt = await this._contract[func](...args)
      .sendTransaction({ from: this._genesisAccount, ...options })
      .executed();
    return receipt.epochNumber
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

  // Getter functions
  getContractAddress(): string {
    return this._contract.address
  }

  async getAccountAddress(): Promise<string> {
    return this._genesisAccount.toString()
  }

  async getBlockTimestamp(blockNumber: number): Promise<number> {
    let receipt: any = await this._conflux.getBlockByEpochNumber(blockNumber)
    return receipt.timestamp
  }

  async getBlockParentHash(blockNumber: number): Promise<string> {
    let receipt: any = await this._conflux.getBlockByEpochNumber(blockNumber)
    return receipt.parentHash
  }

  hashToString(hash: any): string {
    return hash
  }

  async getEvents(): Promise<Event[]> {
    const logs = await this._conflux.getLogs({address: this._contract.address})
    let decoded = logs.map(l => this._contract.abi.decodeLog(l))
    return decoded.map(o => {
      if (!o) return o
        let substitutions = {type: "signature", object: "args"}
        for (const old_key in substitutions) {
          const new_key = substitutions[old_key]
            Object.defineProperty(o, new_key,
              Object.getOwnPropertyDescriptor(o, old_key))
            delete o[old_key];
          }
        return o
      }
    )
  }
}

interface Logger {
  info(): any
  error(): any
}
