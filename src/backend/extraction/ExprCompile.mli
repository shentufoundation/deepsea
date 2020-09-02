open AST
open Ascii
open BinNums
open Compiled
open Cop
open Datatypes
open EVM
open Integers
open MachineModel
open Maps0
open Monad
open OptErrMonad
open PeanoNat
open String0

val dup_ident : nat -> compiled

val global_address : coq_Z PTree.t -> ident -> compiled

val sha_base : Int256.int

val sha_arg2 : Int256.int

val sha_size1 : Int256.int

val sha_size2 : Int256.int

val sha_1_compiled : compiled

val sha_2_compiled : compiled

val binop_compiled : binary_operation -> bool -> compiled

val unop_compiled : unary_operation -> compiled

val builtin0_compiled : builtin0 -> evm

val builtin1_compiled : builtin1 -> evm
