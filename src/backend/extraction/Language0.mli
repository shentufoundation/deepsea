open AST
open Ascii
open Ctypes
open Datatypes
open Decimal
open Globalenvs
open MachineModel
open Nat0
open OptErrMonad
open String0
open Structure
open Values

type instrs = instr list

val eei_revert_output_data_length : nat

val pre_compiled_data_size : nat

val default_gas_limit : nat

type eei_mem_offset =
| Coq_addressOffset
| Coq_dataOffset
| Coq_valueOffset
| Coq_resultOffset
| Coq_pathOffset
| Coq_topic1Offset
| Coq_topic2Offset
| Coq_topic3Offset
| Coq_topic4Offset

type eei_wasm =
| Coq_eei_getAddress
| Coq_eei_getBalance
| Coq_eei_getBlockHash
| Coq_eei_call
| Coq_eei_callDataCopy
| Coq_eei_getCallDataSize
| Coq_eei_callCode
| Coq_eei_callDelegate
| Coq_eei_storageStore
| Coq_eei_storageLoad
| Coq_eei_getCaller
| Coq_eei_getCallValue
| Coq_eei_codeCopy
| Coq_eei_getCodeSize
| Coq_eei_getBlockCoinbase
| Coq_eei_create
| Coq_eei_getBlockDifficulty
| Coq_eei_externalCodeCopy
| Coq_eei_getExternalCodeSize
| Coq_eei_getGasLeft
| Coq_eei_getBlockGasLimit
| Coq_eei_getTxGasPrice
| Coq_eei_log
| Coq_eei_getBlockNumber
| Coq_eei_getTxOrigin
| Coq_eei_useGas
| Coq_eei_selfDestruct
| Coq_eei_getBlockTimestamp
| Coq_eei_revert
| Coq_eei_getReturnDataSize
| Coq_eei_returnDataCopy

type aux_func =
| Coq_aux_pow
| Coq_aux_sha1
| Coq_aux_sha2
| Coq_aux_sha3
| Coq_aux_notint

val scratch_global_idx : nat

val scratch_global_idx1 : nat

val scratch_global_idx2 : nat

val system_contract_keccak256 : nat

val get_idx_eei_mem_offset : eei_mem_offset -> nat

val get_idx_aux : aux_func -> nat

val get_idx_eei : eei_wasm -> nat

val set_len : nat -> nat -> instr list

val set_len_hash : nat -> instr list

val store_len : instr list -> nat -> instr list

val set_eeiOffset : instr list -> eei_mem_offset -> nat -> instr list

val load_len : nat -> nat -> instr list

val load_len_hash : nat -> instr list

val load_resultOffset : nat -> instr list

val wasm_builtin0 : builtin0 -> instr list optErr

val wasm_builtin1 : builtin1 -> instr list

val callExternalSha256TwoParam : instr list

val storeLogArgs : nat -> instrs

type coq_function = { fn_return : coq_type;
                      fn_params : (ident, coq_type) prod list;
                      fn_temps : (ident, coq_type) prod list;
                      fn_locals : (ident, coq_type) prod list;
                      fn_body : instrs }

val fn_return : coq_function -> coq_type

val fn_params : coq_function -> (ident, coq_type) prod list

val fn_temps : coq_function -> (ident, coq_type) prod list

val fn_body : coq_function -> instrs

type genv = (coq_function, coq_type) Genv.t
