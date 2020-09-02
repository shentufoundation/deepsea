open AST
open Ascii
open BinNums
open BinPos
open Cop
open Ctypes
open Datatypes
open GlobalenvCompile
open Globalenvs
open Int0
open Integers
open Language
open Language7
open List0
open Maps0
open Monad
open Nat0
open OptErrMonad
open Options
open PeanoNat
open String0
open Structure
open Trees
open Values0

val global_address : coq_Z PTree.t -> ident -> instr list optErr

val wasm_expr :
  nat PTree.t -> coq_Z PTree.t -> Language.expr -> bool -> instr list optErr

val wasm_exprs :
  nat PTree.t -> coq_Z PTree.t -> Language.expr list -> instr list optErr

val wasm_statement :
  nat PTree.t -> coq_Z PTree.t -> statement -> nat -> instrs optErr

val allocate_locals : (ident, coq_type) prod list -> nat -> nat PTree.t

val allocate_all_locals : (ident, coq_type) prod list -> nat PTree.t

val allocate_fn_locals : Language.coq_function -> nat PTree.t

val wasm_function :
  coq_Z PTree.t -> Language.coq_function -> coq_function optErr

val wasm_constructor :
  Language.coq_function option -> coq_Z PTree.t -> coq_function option optErr

val wasm_functions :
  Language.coq_function PTree.t -> coq_Z PTree.t -> coq_function PTree.t
  optErr

val wasm_methoddefs :
  Language.coq_function option IntMap.t -> coq_Z PTree.t -> coq_function
  option IntMap.t optErr

val wasm_genv : Language.genv -> genv optErr
