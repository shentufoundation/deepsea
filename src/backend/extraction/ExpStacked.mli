open AST
open Cop
open Datatypes
open Integers
open MachineModel

type expr =
| Econst_int256 of Int256.int
| Eglob of ident
| Etempvar of nat
| Emload
| Esload
| Eunop of unary_operation
| Ebinop of binary_operation * bool
| Ecall0 of builtin0
| Ecall1 of builtin1

type ret_type =
| Tvoid_fun
| Tvoid_method
| Tvoid_constructor
| Terror
| Tsome_fun
| Tsome_method
| Tsome_constructor

type typed_label =
| Linternal of label
| Lcall of label
| Lreturn of label
