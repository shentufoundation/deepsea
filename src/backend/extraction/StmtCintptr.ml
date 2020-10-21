open AST
open Ctypes
open Datatypes
open ExpCintptr
open Globalenvs
open Integers

type statement =
| Sskip
| Ssassign of expr * expr
| Smassign of expr * expr
| Sset of ident * expr
| Scall of ident option * label * expr list
| Ssequence of statement * statement
| Sifthenelse of expr * statement * statement
| Sloop of statement
| Sbreak
| Sreturn of ident option
| Shash of expr * expr * expr option
| Stransfer of expr * expr
| Scallmethod of expr * ident list * Int.int * expr * expr list
| Slog of expr list * expr list
| Srevert

type coq_function = { fn_return : coq_type;
                      fn_params : (ident, coq_type) prod list;
                      fn_temps : (ident, coq_type) prod list;
                      fn_locals : (ident, coq_type) prod list;
                      fn_body : statement }

(** val fn_return : coq_function -> coq_type **)

let fn_return x = x.fn_return

(** val fn_params : coq_function -> (ident, coq_type) prod list **)

let fn_params x = x.fn_params

(** val fn_temps : coq_function -> (ident, coq_type) prod list **)

let fn_temps x = x.fn_temps

(** val fn_locals : coq_function -> (ident, coq_type) prod list **)

let fn_locals x = x.fn_locals

(** val fn_body : coq_function -> statement **)

let fn_body x = x.fn_body

type genv = (coq_function, coq_type) Genv.t
