open AST
open Cop
open Ctypes
open Integers
open MachineModel

type expr =
| Econst_int of Int.int * coq_type
| Econst_int256 of Int256.int * coq_type
| Evar of ident * coq_type
| Eglob of ident * coq_type
| Etempvar of ident * coq_type
| Ederef of expr * coq_type
| Eaddr of expr * coq_type
| Eunop of unary_operation * expr * coq_type
| Ebinop of binary_operation * expr * expr * coq_type
| Efield of expr * ident * coq_type
| Eindex of expr * expr * coq_type
| Ecall0 of builtin0 * coq_type
| Ecall1 of builtin1 * expr * coq_type

val typeof : expr -> coq_type
