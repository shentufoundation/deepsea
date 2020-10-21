open AST
open Ctypes
open Datatypes
open ExpMiniC
open Globalenvs
open Integers

type statement =
| Sskip
| Sassign of expr * expr
| Sset of ident * expr
| Scall of ident option * label * expr list
| Ssequence of statement * statement
| Sifthenelse of expr * statement * statement
| Sloop of statement
| Sbreak
| Sreturn of ident option
| Stransfer of expr * expr
| Scallmethod of expr * ident list * Int.int * expr * expr list
| Slog of expr list * expr list
| Srevert

(** val coq_Sfor :
    statement -> expr -> statement -> statement -> statement **)

let coq_Sfor s1 e2 s3 s4 =
  Ssequence (s1, (Sloop (Sifthenelse (e2, (Ssequence (s3, s4)), Sbreak))))

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
