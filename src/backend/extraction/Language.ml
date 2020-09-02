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

(** val typeof : expr -> coq_type **)

let typeof = function
| Econst_int (_, ty) -> ty
| Econst_int256 (_, ty) -> ty
| Evar (_, ty) -> ty
| Etempvar (_, ty) -> ty
| Ederef (_, ty) -> ty
| Eunop (_, _, ty) -> ty
| Ebinop (_, _, _, ty) -> ty
| Efield (_, _, ty) -> ty
| Earrayderef (_, _, ty) -> ty
| Ehashderef (_, _, ty) -> ty
| Ecall0 (_, ty) -> ty
| Ecall1 (_, _, ty) -> ty

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

(** val coq_Sfor :
    statement -> expr -> statement -> statement -> statement **)

let coq_Sfor s1 e2 s3 s4 =
  Ssequence (s1, (Sloop (Ssequence ((Ssequence ((Sifthenelse (e2, Sskip,
    Sbreak)), s3)), s4))))

type coq_function = { fn_return : coq_type;
                      fn_params : (ident, coq_type) prod list;
                      fn_temps : (ident, coq_type) prod list;
                      fn_body : statement }

(** val fn_return : coq_function -> coq_type **)

let fn_return x = x.fn_return

(** val fn_params : coq_function -> (ident, coq_type) prod list **)

let fn_params x = x.fn_params

(** val fn_temps : coq_function -> (ident, coq_type) prod list **)

let fn_temps x = x.fn_temps

(** val fn_body : coq_function -> statement **)

let fn_body x = x.fn_body

type genv = (coq_function, coq_type) Genv.t
