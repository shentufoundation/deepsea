open AST
open Ascii
open BinInt
open BinNums
open Compiled
open Datatypes
open EVM
open ExprCompile
open Integers
open Language5
open Maps0
open MemoryModel
open Nat0
open OptErrMonad
open PeanoNat
open String0
open Values

val assign_stack_compiled : nat -> compiled

val push_public_args : nat -> nat -> nat -> Int.int -> compiled

val code_return : label -> compiled

val cleanup : ret_type -> label -> compiled

val push_event_args : nat -> compiled

val stm_compiled : statement -> coq_Z PTree.t -> label -> compiled

val code_compiled : code -> coq_Z PTree.t -> label -> compiled
