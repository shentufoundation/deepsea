open AST
open Ascii
open Datatypes
open Globalenvs
open Language5
open Language0
open Maps0
open MemoryModel
open Monad
open OptErrMonad
open Semantics0
open String0
open Trees
open Values

val expressionless_expr : expr -> Language5.statement

val pops : nat -> Language5.statement list

val cleanup : nat -> Language5.statement list

val expressionless_rt : ret_type -> Language5.ret_type optErr

val fetch_args : nat -> nat -> Language5.statement list

val extract_lbl : typed_label -> label

val fetch_constructor_args : nat -> nat -> Language5.statement list

val expressionless_stm : statement -> Language5.statement list optErr

val expressionless_stm_constructor :
  statement -> Language5.statement list optErr

val expressionless_code : code -> Language5.code optErr

val expressionless_Constructor_code : code -> Language5.code optErr

val expressionless_constructor_code : code -> Language5.code optErr

val expressionless_function : coq_function -> Language5.coq_function optErr

val expressionless_constructor_function :
  coq_function -> Language5.coq_function optErr

val expressionless_fundefs :
  coq_function PTree.t -> Language5.coq_function PTree.t optErr

val expressionless_methods :
  coq_function option IntMap.t -> Language5.coq_function option IntMap.t
  optErr

val expressionless_constructor :
  coq_function option -> Language5.coq_function optErr

val expressionless_genv : genv -> Language5.genv optErr

val expressionless_program : program -> Language5.program optErr
