open AST
open Cop
open Ctypes
open Datatypes
open Globalenvs
open Integers
open MachineModel

type expr =
| Econst_int256 of Int256.int
| Evar of ident
| Etempvar of nat
| Ederef
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

type statement =
| Sskip
| Srvalue of expr
| Slvalue of expr
| Spushvoid
| Spop
| Sassign
| Sset of nat
| Sdone of nat * ret_type
| Spushlabel of typed_label
| Slabel of label
| Sjump
| Sjumpi
| Stransfer
| Scallmethod of Int.int * nat * nat
| Slog of nat * nat
| Srevert
| Sfetchargs of nat

type code = statement list

type coq_function =
  code
  (* singleton inductive, whose constructor was mkfunction *)

val fn_code : coq_function -> code

type genv = (coq_function, coq_type) Genv.t

type program = (genv, label) prod
