#!/usr/bin/env node

'strict';
let fs = require('fs');
let utils = require('./utils');
let precompiled = {
    keccak256: null
};
// const ethers = require("ethers");

class Environment {
    constructor() {
        this.storage = {};
        this.transactionReceipt = {};
        this.gasLeft = 65536;
        this.callData = new Uint8Array(0);
        this.returnData = new Uint8Array(0);
        this.caller = '1234567890123456789012345678901234567890';
        this.callValue = '00000000000000000000000000000000';
        this.txGasPrice = '1f3f33ff5fabc5fdff67890feff12345';
        this.txOrigin = '1234567890123456789012345678901234567890';
        this.blockCoinbase = '1234567890123456789012345678901234567890';
        this.blockDifficulty = '0000000000000000000000000000000000000000000000000000405BBD86CA28';
        this.blockGasLimit = 21000;
        this.blockNumber = 3456;
        this.blockTimestamp = 6666;
        this.blockhash = '1234567890123456789012345678901234567890123456789012345678901234';
        this.address_balance = '12345678901234567890123456789012';
        this.this_address = '5E72914535f202659083Db3a02C984188Fa26e9f';
    }
    setCallData(callData) {
        if (!/(^[0-9a-f]+)?/i.test(callData)) {
            console.log(`except hex encoded calldata, got: ${callData}`);
            return false;
        }
        if (callData.length > 0) {
            if (callData.length % 2)
                callData = "0" + callData;
            this.callData = new Uint8Array(callData.length / 2).fill(0x00);;
            callData.match(/.{2}/g).forEach((value, i) => {
                this.callData[i] = parseInt(value, 16);
            });
        }
        return true;
    }
    getStorage(storage) {
        return JSON.stringify(this.storage);
    }
    setStorage(storage) {
        this.storage = JSON.parse(storage);
        return true;
    }
}

class Interface {
    get exports() {
        let ret = {
            ethereum: {},
            debug: {},
        };

        // binding EEI functions
        this.eei.forEach((method) => {
            ret.ethereum[method] = this[method].bind(this);
        });

        // javascript doesn't support EEI with i64 parameter/return value, so binding to wrapper wasm function
        this.hooks.forEach((method) => {
            ret.ethereum[method] = this.eei_wrapper.instance.exports[method];
        });

        [
            'print32',
        ].forEach((method) => {
            ret.debug[method] = this[method].bind(this);
        });

        return ret;
    }
    constructor(env) {
        this.env = env;
        this.eei = [
            'useGas',
            'getCallDataSize',
            'callDataCopy',
            'storageStore',
            'storageLoad',
            'log',
            'finish',
            'revert',
            'returnDataCopy',
            'getCaller',
            'getCallValue',
            'getTxGasPrice',
            'getTxOrigin',
            'getBlockCoinbase',
            'getBlockDifficulty',
            'getReturnDataSize',
            'getExternalBalance',
            'getAddress',
        ];
        this.hooks = [
            'getGasLeft',
            'callStatic',
            'call',
            'callDelegate',
            'getBlockGasLimit',
            'getBlockNumber',
            'getBlockHash',
            'getBlockTimestamp',
        ];
    }
    async connect() {
        let hooks = {};
        this.hooks.forEach((method) => {
            hooks[method] = this[method].bind(this);
        });
        // EEI hook function injection
        this.eei_wrapper = await WebAssembly.instantiate(fs.readFileSync("lib/wrapper.wasm"), {
            ethereum: hooks
        });
    }
    getMemory(offset, length) {
        return new Uint8Array(this.mem.buffer, offset, length);
    }
    setMemory(offset, length, value) {
        const memory = new Uint8Array(this.mem.buffer, offset, length);
        memory.set(value);
    }
    takeGas(amount) {
        if (this.env.gasLeft < amount) {
            throw new Error('Ran out of gas')
        }
        this.env.gasLeft -= amount
    }
    useGas(gas) {
        console.log(`useGas(${gas})`);
        //takeGas(gas);
    }
    getCallDataSize() {
        console.log(`getCallDataSize()`);
        this.takeGas(2);
        console.log(`{ size: ${this.env.callData.length} }`);
        return this.env.callData.length;
    }
    callDataCopy(resultOffset, dataOffset, length) {
        console.log(`callDataCopy(${resultOffset}, ${dataOffset}, ${length})`);
        this.takeGas(3 + Math.ceil(length / 32) * 3)
        if (length) {
            const callData = this.env.callData.slice(dataOffset, dataOffset + length);
            this.setMemory(resultOffset, length, callData);
            console.log(`{ data: ${utils.toHex(callData)} }`);
        }
    }
    storageStore(pathOffset, valueOffset) {
        console.log(`storageStore(${pathOffset}, ${valueOffset})`);
        const path = utils.toBigInt('0x' + utils.toHex(this.getMemory(pathOffset, 32))).toString(16);
        const value = utils.toBigInt('0x' + utils.toHex(this.getMemory(valueOffset, 32))).toString(16);
        this.env.storage[path] = value;
        console.log(`{ key: ${path}, value: ${value} }`);
    }
    storageLoad(pathOffset, valueOffset) {
        console.log(`storageLoad(${pathOffset}, ${valueOffset})`);
        const path = utils.toBigInt('0x' + utils.toHex(this.getMemory(pathOffset, 32))).toString(16);
        if (path in this.env.storage) {
            let value = this.env.storage[path];
            const data = value.padStart(64, '0').match(/.{2}/g).map(value => parseInt(value, 16));
            this.setMemory(valueOffset, 32, data);
            console.log(`{ key: ${path}, value: ${value} }`);
        } else {
            const data = Array(32).fill(0);
            this.setMemory(valueOffset, 32, data);
            console.log(`{ key: ${path}, value: 0 }`);
        }
    }
    log(dataOffset, dataLength, numberOfTopics, topic1, topic2, topic3, topic4) {
        console.log(`log(${dataOffset}, ${dataLength}, ${numberOfTopics}, ${topic1}, ${topic2}, ${topic3}, ${topic4})`);
        this.takeGas(375 + (375 * numberOfTopics) + (8 * dataLength));
        if (dataLength >= 1) {
            const data = utils.toHex(this.getMemory(dataOffset, dataLength));
            this.env.transactionReceipt.data = data;
            console.log(`{ data: ${data} }`);
        }
        if (numberOfTopics >= 1) {
            this.env.transactionReceipt.topics = [];
            const t1 = utils.toHex(this.getMemory(topic1, 32));
            this.env.transactionReceipt.topics.push(t1);
            console.log(`{ t1/signature: ${t1} }`);
        }
        if (numberOfTopics >= 2) {
            const t2 = utils.toHex(this.getMemory(topic2, 32));
            this.env.transactionReceipt.topics.push(t2);
            console.log(`{ t2: ${t2} }`);
        }
        if (numberOfTopics >= 3) {
            const t3 = utils.toHex(this.getMemory(topic3, 32));
            this.env.transactionReceipt.topics.push(t3);
            console.log(`{ t3: ${t3} }`);
        }
        if (numberOfTopics >= 4) {
            const t4 = utils.toHex(this.getMemory(topic4, 32));
            this.env.transactionReceipt.topics.push(t4);
            console.log(`{ t4: ${t4} }`);
        }
    }
    finish(dataOffset, dataLength) {
        console.log(`finish(${dataOffset}, ${dataLength})`);
        const data = this.getMemory(dataOffset, dataLength)
        this.env.returnData = data;
        throw new Error('finish');
    }
    revert(dataOffset, dataLength) {
        console.log(`revert(${dataOffset}, ${dataLength})`);
        const data = this.getMemory(dataOffset, dataLength)
        this.env.returnData = data;
        const message = String.fromCharCode.apply(null, data);
        this.env.transactionReceipt.revertReason = message;
        console.log(`{ message: ${message} }`);
        throw new Error('revert');
        process.exit(0);
    }
    callStatic(gas, addressOffset, dataOffset, dataLength) {
        console.log(`callStatic(${gas}, ${addressOffset}, ${dataOffset}, ${dataLength})`);
        const addressInHex = '0x' + utils.toHex(this.getMemory(addressOffset, 20));
        const address = utils.toBigInt(addressInHex);
        const data = this.getMemory(dataOffset, dataLength);
        console.log(`{ address: ${addressInHex}, data: ${utils.toHex(data)} }`);

        let vm;
        switch (address) {
            case BigInt(2): // Sha256
            case BigInt(9): // Keccak256
                vm = precompiled.keccak256;
                break;
            default:
                return 1;
        }
        vm.run(utils.toHex(data));

        this.env.returnData = vm.env.returnData;
        return 0;
    }
    call(gas, addressOffset, valueOffset, dataOffset, dataLength) {
        console.log(`call(${gas}, ${addressOffset}, ${valueOffset}, ${dataOffset}, ${dataLength})`);
        const addressInHex = '0x' + utils.toLEHex(this.getMemory(addressOffset, 20));
        const address = utils.toBigInt(addressInHex);
        const value = utils.toBigInt('0x' + utils.toHex(this.getMemory(valueOffset, 16))).toString(16);
        const data = this.getMemory(dataOffset, dataLength);
        console.log(`{ address: ${addressInHex}, value: ${value}, data: ${utils.toHex(data)} }`);

        let vm;
        switch (address) {
            case BigInt(2): // Sha256
            case BigInt(9): // Keccak256
                // console.log(`{ address: ${addressInHex}, yayyyyy`);
                vm = precompiled.keccak256;
                // ethers.utils.keccak256()
                break;
            default:
                return 1;
        }
        vm.run(utils.toHex(data));

        this.env.returnData = vm.env.returnData;
        return 0;
    }
    callDelegate(gas, addressOffset, dataOffset, dataLength) {
        console.log(`callDelegate(${gas}, ${addressOffset}, ${dataOffset}, ${dataLength})`);
        const addressInHex = '0x' + utils.toHex(this.getMemory(addressOffset, 20));
        const address = utils.toBigInt(addressInHex);
        const data = this.getMemory(dataOffset, dataLength);
        console.log(`{ address: ${addressInHex}, data: ${utils.toHex(data)} }`);

        let vm;
        switch (address) {
            case BigInt(2): // Sha256
            case BigInt(9): // Keccak256
                vm = precompiled.keccak256;
                break;
            default:
                return 1;
        }
        vm.run(utils.toHex(data));

        this.env.returnData = vm.env.returnData;
        return 0;
    }
    getReturnDataSize() {
        console.log(`getReturnDataSize()`);
        this.takeGas(2);
        console.log(`{ size: ${this.env.returnData.length} }`);
        return this.env.returnData.length;
    }
    returnDataCopy(resultOffset, dataOffset, length) {
        console.log(`returnDataCopy(${resultOffset}, ${dataOffset}, ${length})`);
        if (length) {
            const callData = this.env.returnData.slice(dataOffset, dataOffset + length);
            this.setMemory(resultOffset, length, callData);
            console.log(`{ data: ${utils.toHex(callData)} }`)
        }
    }
    getCaller(resultOffset) {
        console.log(`getCaller(${resultOffset})`);
        const data = this.env.caller.padStart(40, '0').match(/.{2}/g).map(value => parseInt(value, 16));
        this.setMemory(resultOffset, 20, data);
        console.log(`{ caller: ${utils.toHex(data)} }`);
    }
    getCallValue(resultOffset) {
        console.log(`getCallValue(${resultOffset})`);
        const data = this.env.callValue.padStart(32, '0').match(/.{2}/g).map(value => parseInt(value, 16));
        this.setMemory(resultOffset, 16, data);
        console.log(`{ value: ${utils.toHex(data)} }`);
    }
    getGasLeft() {
        console.log(`getGasLeft()`);
        console.log(`{ gas: ${this.env.gasLeft} }`);
        return this.env.gasLeft;
    }

    getTxGasPrice(valueOffset) {
        console.log(`getTxGasPrice(${valueOffset})`);
        const data = this.env.txGasPrice.padStart(32, '0').match(/.{2}/g).reverse().map(value => parseInt(value, 16));
        this.setMemory(valueOffset, 16, data);
        console.log(`{ price: ${utils.toHex(data)} }`);
    }

    getTxOrigin(resultOffset) {
        console.log(`getTxOrigin(${resultOffset})`);
        const data = this.env.txOrigin.padStart(40, '0').match(/.{2}/g).map(value => parseInt(value, 16));
        this.setMemory(resultOffset, 20, data);
        console.log(`{ orig: ${utils.toHex(data)} }`);
    }

    getBlockCoinbase(resultOffset) {
        console.log(`getBlockCoinbase(${resultOffset})`);
        const data = this.env.blockCoinbase.padStart(40, '0').match(/.{2}/g).map(value => parseInt(value, 16));
        this.setMemory(resultOffset, 20, data);
        console.log(`{ coinbase: ${utils.toHex(data)} }`);
    }

    getBlockDifficulty(resultOffset) {
        console.log(`getBlockDifficulty(${resultOffset})`);
        const data = this.env.blockDifficulty.padStart(64, '0').match(/.{2}/g).reverse().map(value => parseInt(value, 16));
        this.setMemory(resultOffset, 32, data);
        console.log(`{ difficulty: ${utils.toHex(data)} }`);
    }

    getBlockGasLimit() {
        console.log(`getBlockGasLimit()`);
        console.log(`{ gas: ${this.env.blockGasLimit} }`);
        return this.env.blockGasLimit;
    }

    getBlockNumber() {
        console.log(`getBlockNumber()`);
        console.log(`{ block: ${this.env.blockNumber} }`);
        return this.env.blockNumber;
    }

    getBlockTimestamp() {
        console.log(`getBlockTimestamp()`);
        console.log(`{ timestamp: ${this.env.blockTimestamp} }`);
        return this.env.blockTimestamp;
    }

    getBlockHash(number, resultOffset) {
        console.log(`getBlockHash(${resultOffset})`);
        const data = this.env.blockhash.padStart(64, '0').match(/.{2}/g).map(value => parseInt(value, 16));
        this.setMemory(resultOffset, 32, data);
        console.log(`{ blockhash: ${utils.toHex(data)} }`);
        return 0;
    }

    getExternalBalance(addressOffset, resultOffset) {
        console.log(`getExternalBalance(${addressOffset}, ${resultOffset})`);
        const addressInHex = '0x' + utils.toHex(this.getMemory(addressOffset, 20));
        const address = utils.toBigInt(addressInHex);
        const data = this.env.address_balance.padStart(32, '0').match(/.{2}/g).reverse().map(value => parseInt(value, 16));
        this.setMemory(resultOffset, 16, data);
        console.log(`{ address: ${addressInHex}, value: ${utils.toHex(data)} }`);
    }

    getAddress(resultOffset) {
        console.log(`getAddress(${resultOffset})`);
        const data = this.env.this_address.padStart(40, '0').match(/.{2}/g).map(value => parseInt(value, 16));
        this.setMemory(resultOffset, 20, data);
        console.log(`{ orig: ${utils.toHex(data)} }`);
    }

    print32(value) {
        console.log(`print32(${(new Uint32Array([value]))[0]})`);
    }
}

class VM {
    constructor(path) {
        this.env = new Environment();
        this.int = new Interface(this.env);
        this.path = path;
    }
    async instantiate() {
        await this.int.connect();
        this.vm = await WebAssembly.instantiate(fs.readFileSync(this.path), this.int.exports);
        this.int.mem = this.vm.instance.exports.memory;
    }
    run(callData, storage, env) {
        if (!this.env.setCallData(callData)) {
            return false;
        }
        if (storage && !this.env.setStorage(storage)) {
            return false;
        }
        if (env) {
            this.env = Object.assign(this.env, env);
        }
        try {
            this.vm.instance.exports.main();
        } catch (error) {
            if (error.message !== "finish") {
                console.log(error);
            }
        }
        return true;
    }
}

async function main(path, callData, storage, env) {
    for (let name in precompiled) {
        let vm = new VM(`lib/${name}.wasm`);
        await vm.instantiate();
        precompiled[name] = vm;
    }
    let vm = new VM(path);
    await vm.instantiate();
    vm.run(callData, storage, env);
    return {
        returnData: utils.toLEHex(vm.env.returnData),
        storage: vm.env.getStorage(),
        transactionReceipt: vm.env.transactionReceipt
    };
};

if (require.main === module) {
    if (4 > process.argv.length || process.argv.length > 5) {
        console.log(`usage: ${process.argv[1]} ewasm-file calldata [storage]`);
        process.exit(0);
    }
    main(process.argv[2], process.argv[3], process.argv[4]).then(console.log).catch(console.log);
} else {
    module.exports = main;
}