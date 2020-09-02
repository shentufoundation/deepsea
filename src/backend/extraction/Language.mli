open AST
open Cop
open Ctypes
open Datatypes
open Globalenvs
open Integers
open MachineModel

type expr =
| Econst_int of Int.int * coq_type
| Econst_int256 of Int256.int * coq_type
| Evar of ident * coq_type
| Etempvar of ident * coq_type
| Ederef of expr * coq_type
| Eunop of unary_operation * expr * coq_type
| Ebinop of binary_operation * expr * expr * coq_type
| Efield of expr * ident * coq_type
| Earrayderef of expr * expr * coq_type
| Ehashderef of expr * expr * coq_type
| Ecall0 of builtin0 * coq_type
| Ecall1 of builtin1 * expr * coq_type

val typeof : expr -> coq_type

type statement =
| Sskip
| Sassign of expr * expr
| Sset of ident * expr
| Scall of ident option * label * expr list
| Ssequence of statement * statement
| Sifthenelse of expr * statement * statement
| Sloop of statement
| Sbreak
| Sreturn of expr option
| Stransfer of expr * expr
| Scallmethod of expr * ident list * Int.int * expr * expr list
| Slog of expr list * expr list
| Srevert

val coq_Sfor : statement -> expr -> statement -> statement -> statement

type coq_function = { fn_return : coq_type;
                      fn_params : (ident, coq_type) prod list;
                      fn_temps : (ident, coq_type) prod list;
                      fn_body : statement }

val fn_return : coq_function -> coq_type

val fn_params : coq_function -> (ident, coq_type) prod list

val fn_temps : coq_function -> (ident, coq_type) prod list

val fn_body : coq_function -> statement

type genv = (coq_function, coq_type) Genv.t
