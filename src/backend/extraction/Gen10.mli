open AST
open Ascii
open BinNums
open BinPos
open Cop
open Ctypes
open Datatypes
open ExpCintptr
open GlobalenvCompile
open Globalenvs
open Int0
open Integers
open Language0
open List0
open Maps0
open Monad
open Nat0
open OptErrMonad
open Options
open PeanoNat
open StackEnv
open StmtCintptr
open String0
open Structure
open Trees
open Values

val wasm_expr : nat PTree.t -> ExpCintptr.expr -> bool -> instr list optErr

val wasm_exprs : nat PTree.t -> ExpCintptr.expr list -> instr list optErr

val optident : nat PTree.t -> ident option -> instrs optErr

val wasm_statement :
  (positive, nat) prod list -> nat PTree.t -> coq_type -> statement -> nat ->
  instrs optErr

val allocate_temps : (ident, coq_type) prod list -> nat -> nat PTree.t

val allocate_all_temps : (ident, coq_type) prod list -> nat PTree.t

val allocate_fn_temps : coq_function -> nat PTree.t

val wasm_function :
  coq_Z PTree.t -> (positive, nat) prod list -> coq_function ->
  Language0.coq_function optErr

val wasm_constructor :
  coq_function option -> coq_Z PTree.t -> (positive, nat) prod list ->
  Language0.coq_function option optErr

val wasm_functions :
  coq_function PTree.t -> coq_Z PTree.t -> (positive, nat) prod list ->
  Language0.coq_function PTree.t optErr

val wasm_methoddefs :
  coq_function option IntMap.t -> coq_Z PTree.t -> (positive, nat) prod list
  -> Language0.coq_function option IntMap.t optErr

val wasm_func_identifier :
  (positive, 'a1) prod list -> nat -> nat -> (positive, nat) prod list

val wasm_genv : genv -> Language0.genv optErr
