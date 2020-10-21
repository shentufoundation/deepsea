open AST
open Ctypes
open Datatypes
open Maps0
open StmtExpressionless

type genv = { genv_vars : ident list; genv_defs : coq_type PTree.t;
              genv_main : code; genv_main_entrypoint : label }

(** val genv_vars : genv -> ident list **)

let genv_vars x = x.genv_vars

(** val genv_defs : genv -> coq_type PTree.t **)

let genv_defs x = x.genv_defs

(** val genv_main : genv -> code **)

let genv_main x = x.genv_main

(** val genv_main_entrypoint : genv -> label **)

let genv_main_entrypoint x = x.genv_main_entrypoint
