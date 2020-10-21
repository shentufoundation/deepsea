open AST
open Ctypes
open Datatypes
open Maps0
open StmtExpressionless

type genv = { genv_vars : ident list; genv_defs : coq_type PTree.t;
              genv_main : code; genv_main_entrypoint : label }

val genv_vars : genv -> ident list

val genv_defs : genv -> coq_type PTree.t

val genv_main : genv -> code

val genv_main_entrypoint : genv -> label
