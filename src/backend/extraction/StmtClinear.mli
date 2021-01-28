open AST
open Ctypes
open Datatypes
open ExpCintptr
open Globalenvs
open Integers

type node = ident

type statement =
| Ssassign of expr * expr
| Smassign of expr * expr
| Sset of ident * expr
| Scall of ident option * label * expr list * label
| Sreturn of ident option
| Sdone of bool option
| Slabel of label
| Sjump of label
| Sjumpi of expr * label
| Shash of expr * expr * expr option
| Stransfer of expr * expr * label
| Scallmethod of expr * ident list * Int.int * expr * expr option * expr list
   * label
| Slog of expr list * expr list
| Srevert
| Sfetchargs of bool
| Sintro

type code = statement list

type coq_function = { fn_return : coq_type;
                      fn_params : (ident, coq_type) prod list;
                      fn_temps : (ident, coq_type) prod list;
                      fn_locals : (ident, coq_type) prod list; fn_code : 
                      code }

val fn_return : coq_function -> coq_type

val fn_params : coq_function -> (ident, coq_type) prod list

val fn_temps : coq_function -> (ident, coq_type) prod list

val fn_locals : coq_function -> (ident, coq_type) prod list

val fn_code : coq_function -> code

type genv = (coq_function, coq_type) Genv.t

type program = (genv, label) prod
