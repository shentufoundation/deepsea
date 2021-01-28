open AST
open Ascii
open BinInt
open BinNums
open Cop
open Ctypes
open Datatypes
open ExpCintptr
open ExpStacked
open FramesLabelsCintptr
open Globalenvs
open Integers
open List0
open Maps0
open MemoryModel
open Monad
open Nat0
open OptErrMonad
open Options
open Semantics1
open StmtClinear
open StmtStacked
open String0
open TempModelLow
open Trees

val stacked_expr :
  nat PTree.t -> ExpCintptr.expr -> nat -> bool -> statement list optErr

val stacked_exprs :
  nat PTree.t -> ExpCintptr.expr list -> nat -> statement list optErr

val ident_indices : nat PTree.t -> ident list -> nat -> nat list optErr

val set_indices : nat list -> statement list

val load_returns : nat -> statement list

val optident : nat PTree.t -> ident option -> statement optErr

val return_type : StmtClinear.coq_function -> coq_type

val toreturn : StmtClinear.coq_function -> bool option -> ret_type

val zero_stm : statement

val xzero_stms : nat -> statement list

val zero_stms : nat -> statement list

val z_stm : coq_Z -> statement

val stacked_code :
  nat PTree.t -> StmtClinear.coq_function -> StmtClinear.code -> code optErr

val allocate_temps : (ident, coq_type) prod list -> nat -> nat PTree.t

val allocate_all_temps : (ident, coq_type) prod list -> nat PTree.t

val allocate_fn_temps : StmtClinear.coq_function -> nat PTree.t

val stacked_function :
  StmtClinear.coq_function -> StmtStacked.coq_function optErr

val stacked_fundef :
  StmtClinear.coq_function -> StmtStacked.coq_function optErr

val stacked_fundefs :
  StmtClinear.coq_function PTree.t -> StmtStacked.coq_function PTree.t optErr

val stacked_methods :
  StmtClinear.coq_function option IntMap.t -> StmtStacked.coq_function option
  IntMap.t optErr

val stacked_constructor :
  StmtClinear.coq_function option -> StmtStacked.coq_function optErr

val stacked_genv : StmtClinear.genv -> genv optErr

val stacked_program : StmtClinear.program -> program optErr
