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

val coq_Sfor : statement -> expr -> statement -> statement -> statement

type coq_function = { fn_return : coq_type;
                      fn_params : (ident, coq_type) prod list;
                      fn_temps : (ident, coq_type) prod list;
                      fn_locals : (ident, coq_type) prod list;
                      fn_body : statement }

val fn_return : coq_function -> coq_type

val fn_params : coq_function -> (ident, coq_type) prod list

val fn_temps : coq_function -> (ident, coq_type) prod list

val fn_locals : coq_function -> (ident, coq_type) prod list

val fn_body : coq_function -> statement

type genv = (coq_function, coq_type) Genv.t
