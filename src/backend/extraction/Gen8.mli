open AST
open Ascii
open BinNums
open Datatypes
open ExpStacked
open Globalenvs
open Integers
open LowValues
open Maps0
open MemoryModel
open Monad
open OptErrMonad
open Semantics2
open StmtExpressionless
open StmtStacked
open String0
open Trees

val expressionless_expr : expr -> StmtExpressionless.statement

val pops : nat -> StmtExpressionless.statement list

val cleanup : nat -> StmtExpressionless.statement list

val expressionless_rt : ExpStacked.ret_type -> ret_type optErr

val fetch_args :
  function_kind -> nat -> nat -> StmtExpressionless.statement list

val extract_lbl : typed_label -> label

val expressionless_stm :
  statement -> function_kind -> StmtExpressionless.statement list optErr

val expressionless_code :
  code -> function_kind -> StmtExpressionless.code optErr

val expressionless_function :
  function_kind -> coq_function -> StmtExpressionless.coq_function optErr

val expressionless_fundefs :
  coq_function PTree.t -> StmtExpressionless.coq_function PTree.t optErr

val expressionless_methods :
  coq_function option IntMap.t -> StmtExpressionless.coq_function option
  IntMap.t optErr

val expressionless_constructor :
  coq_function option -> StmtExpressionless.coq_function optErr

val expressionless_genv : genv -> StmtExpressionless.genv optErr

val expressionless_program : program -> StmtExpressionless.program optErr
