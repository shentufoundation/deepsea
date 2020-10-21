open AST
open Cop
open Ctypes
open Integers
open MachineModel

type expr =
| Econst_int of Int.int * coq_type
| Econst_int256 of Int256.int * coq_type
| Etempvar of ident * coq_type
| Esload of expr * coq_type
| Emload of expr * coq_type
| Eaddr of expr * coq_type
| Eunop of unary_operation * expr * coq_type
| Ebinop of binary_operation * expr * expr * coq_type
| Ecall0 of builtin0 * coq_type
| Ecall1 of builtin1 * expr * coq_type

(** val typeof : expr -> coq_type **)

let typeof = function
| Econst_int (_, ty) -> ty
| Econst_int256 (_, ty) -> ty
| Etempvar (_, ty) -> ty
| Esload (_, ty) -> ty
| Emload (_, ty) -> ty
| Eaddr (_, ty) -> ty
| Eunop (_, _, ty) -> ty
| Ebinop (_, _, _, ty) -> ty
| Ecall0 (_, ty) -> ty
| Ecall1 (_, _, ty) -> ty
