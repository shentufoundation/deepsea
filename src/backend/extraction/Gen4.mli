open AST
open Ascii
open BinNums
open Cop
open Ctypes
open Datatypes
open Globalenvs
open Integers
open Language2
open Language
open Language0
open List0
open Maps0
open MemoryModel
open Monad
open Nat0
open OptErrMonad
open Options
open Semantics
open String0
open TempModel
open Trees

val stacked_expr :
  nat PTree.t -> Language.expr -> nat -> bool -> statement list optErr

val stacked_exprs :
  nat PTree.t -> Language.expr list -> nat -> statement list optErr

val stacked_optexpr :
  nat PTree.t -> Language.expr option -> statement list optErr

val ident_indices : nat PTree.t -> ident list -> nat -> nat list optErr

val set_indices : nat list -> statement list

val optident : nat PTree.t -> ident option -> statement optErr

val return_type : Language2.coq_function -> coq_type

val toreturn : Language2.coq_function -> bool option -> ret_type

val zero_stm : statement

val xzero_stms : nat -> statement list

val zero_stms : nat -> statement list

val z_stm : coq_Z -> statement

val stacked_code :
  nat PTree.t -> Language2.coq_function -> Language2.code -> code optErr

val allocate_locals : (ident, coq_type) prod list -> nat -> nat PTree.t

val allocate_all_locals : (ident, coq_type) prod list -> nat PTree.t

val allocate_fn_locals : Language2.coq_function -> nat PTree.t

val stacked_function : Language2.coq_function -> Language0.coq_function optErr

val stacked_fundef : Language2.coq_function -> Language0.coq_function optErr

val stacked_fundefs :
  Language2.coq_function PTree.t -> Language0.coq_function PTree.t optErr

val stacked_methods :
  Language2.coq_function option IntMap.t -> Language0.coq_function option
  IntMap.t optErr

val stacked_constructor :
  Language2.coq_function option -> Language0.coq_function optErr

val stacked_genv : Language2.genv -> genv optErr

val stacked_program : Language2.program -> program optErr
