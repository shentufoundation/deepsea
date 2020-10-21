open AST
open Ctypes
open Datatypes
open Globalenvs
open Maps0
open StmtClinear

type bblock = statement list

type code = bblock PTree.t

type coq_function = { fn_return : coq_type;
                      fn_params : (ident, coq_type) prod list;
                      fn_temps : (ident, coq_type) prod list;
                      fn_locals : (ident, coq_type) prod list;
                      fn_code : code; fn_entrypoint : label }

(** val fn_return : coq_function -> coq_type **)

let fn_return x = x.fn_return

(** val fn_params : coq_function -> (ident, coq_type) prod list **)

let fn_params x = x.fn_params

(** val fn_temps : coq_function -> (ident, coq_type) prod list **)

let fn_temps x = x.fn_temps

(** val fn_locals : coq_function -> (ident, coq_type) prod list **)

let fn_locals x = x.fn_locals

(** val fn_code : coq_function -> code **)

let fn_code x = x.fn_code

(** val fn_entrypoint : coq_function -> label **)

let fn_entrypoint x = x.fn_entrypoint

type genv = (coq_function, coq_type) Genv.t
