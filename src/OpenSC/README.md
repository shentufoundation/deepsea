# OpenSC
A toy project of programming language for smart contract written in `Ocaml`. 
This is final project for COMS W4115 programming language & translator @ Columbia.

## Introduction

[`OpenSC`](https://github.com/JackSnowWolf/OpenSC) 
is a functional programming language which has similar functionality 
compared to [`Scilla`](https://scilla.readthedocs.io/en/latest/) 
and [`Pact`](https://github.com/kadena-io/pact). 
It is statically typed and will support several features. 
It is a high-level language that will be primarily used to implement smart contracts,
 which are programs that provide protocol for handling account behavior in Ethereum.

Compared to other languages, we model contracts as some simple transition systems, 
with the transitions being pure functions of the contract state. These functions 
are expressed from one state to another state in a list of storage mutations.

Inspired by the `MiniC` language, part of the 
[`DeepSEA`](https://certik.io/blog/technology/an-introduction-to-deepsea) compiler, 
we aim to develop a language which allows interactive formal verification of 
smart contracts with security guarantees. From a specific input program, the 
compiler generates executable bytecode, as well as a model of the program that 
can be loaded into the [`Coq`](https://coq.inria.fr/)
proof assistant. Our eventual goal is that smart contracts like storage, 
auction and token can be written by `OpenSC`, and that these contracts 
can be compiled via the translator into binary codes that can be executed on `EVM`.

## Usage

### Environment Dependencies

- Install `ocaml`, which is what our translator is written in.
- Install `opam`, the ocaml package manager
- `opam install cryptokit`,  which is used in the `Minic` (IR code) generation phase of the compiler front-end for cryptographic hashing


### Synopsis

```bash
# at root directory of OpenSC
# in the OpenSC/src directory
% dune build opensc.bc
% ../../_build/default/OpenSC/src/opensc.bc [source.sc] [mode]
```

### Modes

- `ast`
    - generate raw AST and print its structure
- `sast`
    - generate SAST (semantically checked AST) and print its structure
- `bytecode`
    - generate EVM bytecode and print it

### How to test
#### command
```bash
% opensc.bc [path to source.sc] [mode] 
# mode : ast | sast | minic | bytecode
```
#### example program sources for test
* example_contract/simpleStorage.sc
* example_contract/simpleToken.sc

#### run bytecode on EVM
##### install dependencies
* install and use node 12
* install ganache-cli, ethers
* open ganache-cli
##### run bytecode on EVM
```bash
# at root directory of OpenSC
% cd src/example_contract
./test-simpleStorage.js simpleStorage.bytecode
./test-simpleToken.js simpleToken.bytecode
```


### Reference Manual

Check our [Language Reference Manual](OpenSC_Reference_Manual.pdf) for
 detailed usage

> WARNING: We are not supporting multi-key mapping, events and log functions.


## Acknowledgements

Thanks to Professor Ronghui Gu, the instructor of our course, who brought us to the PLT world and let us realize the charm of functional programming and formal verification, both of which are what our project is based on. 

Thanks to River Dillon Keefer and Amanda Liu, TAs of our course, who introduced the DeepSEA project to us and provided very inspiring and helpful ideas on the OpenSC language syntax among other project details. 

Thanks to Vilhelm Sjöberg, our project advisor, researcher at Yale and the primary creator of the DeepSEA project, who provided us with great information on everything about the DeepSEA project, and answered our many questions, which has been super helpful. 

## Contact

- Jun Sha `js5506`: [email](mailto:js5506@columbia.edu.com)
- Linghan Kong `lk2811`: [email](mailto:lk2811@columbia.edu)
- Ruibin Ma `rm3708`: [email](mailto:rm3708@columbia.edu)
- Chong Hu `ch3467`: [email](mailto:ch3467@columbia.edu)

## License

Copyright (c) 2020, `OpenSC`. All rights reserved.

The code is distributed under a MIT license. See [`LICENSE`](LICENSE) for information.
