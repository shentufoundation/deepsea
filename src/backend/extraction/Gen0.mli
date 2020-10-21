open AST
open Ascii
open BinPos
open Cop
open Ctypes
open Datatypes
open ExpMiniC
open Globalenvs
open Maps0
open Monad
open Nat0
open OptErrMonad
open PeanoNat
open StmtClocal
open StmtMiniC
open String0
open Trees

val sequentialize : StmtClocal.statement list -> StmtClocal.statement

val clocal_rvalue :
  (nat -> expr) -> (expr -> nat -> (StmtClocal.statement, nat) prod optErr)
  -> expr -> nat -> (StmtClocal.statement, nat) prod optErr

val clocal_lvalue :
  (nat -> expr) -> (expr -> nat -> (StmtClocal.statement, nat) prod optErr)
  -> expr -> nat -> (StmtClocal.statement, nat) prod optErr

val clocal_expr_list :
  (nat -> expr) -> (expr list -> nat -> (StmtClocal.statement, nat) prod
  optErr) -> expr list -> nat -> (StmtClocal.statement, nat) prod optErr

val split_exps : nat -> expr list -> (expr list, expr list) prod optErr

val clocal_stm :
  (nat -> expr) -> statement -> (StmtClocal.statement, nat) prod optErr

val max_id : (ident, coq_type) prod list -> nat

val make_scrmap : nat -> nat -> expr

val ptr_type : coq_type

val extend_locs :
  nat -> nat -> (ident, coq_type) prod list -> (ident, coq_type) prod list

val clocal_function : coq_function -> StmtClocal.coq_function optErr

val clocal_constructor :
  coq_function option -> StmtClocal.coq_function option optErr

val clocal_functions :
  coq_function PTree.t -> StmtClocal.coq_function PTree.t optErr

val clocal_methoddefs :
  coq_function option IntMap.t -> StmtClocal.coq_function option IntMap.t
  optErr

val clocal_genv : genv -> StmtClocal.genv optErr
