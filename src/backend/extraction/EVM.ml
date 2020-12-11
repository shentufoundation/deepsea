open AST
open Datatypes
open Integers

type evm =
| Coq_evm_stop
| Coq_evm_add
| Coq_evm_mul
| Coq_evm_sub
| Coq_evm_div
| Coq_evm_sdiv
| Coq_evm_mod
| Coq_evm_smod
| Coq_evm_addmod
| Coq_evm_mulmod
| Coq_evm_exp
| Coq_evm_signextend
| Coq_evm_lt
| Coq_evm_gt
| Coq_evm_slt
| Coq_evm_sgt
| Coq_evm_eq
| Coq_evm_iszero
| Coq_evm_and
| Coq_evm_or
| Coq_evm_xor
| Coq_evm_not
| Coq_evm_byte
| Coq_evm_sha3
| Coq_evm_address
| Coq_evm_balance
| Coq_evm_origin
| Coq_evm_caller
| Coq_evm_callvalue
| Coq_evm_calldataload
| Coq_evm_calldatasize
| Coq_evm_codesize
| Coq_evm_gasprice
| Coq_evm_extcodesize
| Coq_evm_blockhash
| Coq_evm_coinbase
| Coq_evm_timestamp
| Coq_evm_number
| Coq_evm_chainid
| Coq_evm_selfbalance
| Coq_evm_difficulty
| Coq_evm_gaslimit
| Coq_evm_gas
| Coq_evm_codecopy
| Coq_evm_pop
| Coq_evm_mload
| Coq_evm_mstore
| Coq_evm_mstore8
| Coq_evm_sload
| Coq_evm_sstore
| Coq_evm_jump
| Coq_evm_jumpi
| Coq_evm_totallength of nat
| Coq_evm_label of label
| Coq_evm_push of Int256.int
| Coq_evm_push_label of label
| Coq_evm_dup of nat
| Coq_evm_swap of nat
| Coq_evm_log of nat
| Coq_evm_call
| Coq_evm_revert
| Coq_evm_return
