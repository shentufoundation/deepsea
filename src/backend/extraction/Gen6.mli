open Ascii
open BinNums
open Cop
open Ctypes
open Datatypes
open ExpCintptr
open ExpMiniC
open Globalenvs
open Maps0
open Monad
open OptErrMonad
open StackEnv
open StmtCintptr
open StmtClocal
open String0
open Trees

val spE : ExpCintptr.expr

val offsetE : coq_function -> ExpCintptr.expr

val pushS : coq_function -> StmtCintptr.statement

val popS : coq_function -> StmtCintptr.statement

val cintptr_expr : coq_function -> expr -> ExpCintptr.expr optErr

val cintptr_exprs : coq_function -> expr list -> ExpCintptr.expr list optErr

val cintptr_expr_opt :
  coq_function -> expr option -> ExpCintptr.expr option optErr

val cintptr_stmt : coq_function -> statement -> StmtCintptr.statement optErr

val cintptr_function : coq_function -> StmtCintptr.coq_function optErr

val cintptr_fundefs :
  coq_function PTree.t -> StmtCintptr.coq_function PTree.t optErr

val cintptr_methods :
  coq_function option IntMap.t -> StmtCintptr.coq_function option IntMap.t
  optErr

val cintptr_constructor :
  coq_function option -> StmtCintptr.coq_function optErr

val cintptr_genv : genv -> StmtCintptr.genv optErr
