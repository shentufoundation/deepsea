open Ascii
open BinNums
open Compiled
open Cop
open Datatypes
open EVM
open Integers
open MachineModel
open Monad
open OptErrMonad
open PeanoNat
open String0

val dup_ident : nat -> compiled

val binop_compiled : binary_operation -> bool -> compiled

val unop_compiled : unary_operation -> compiled

val builtin0_compiled : builtin0 -> evm

val builtin1_compiled : builtin1 -> evm
