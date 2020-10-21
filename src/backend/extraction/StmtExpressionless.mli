open AST
open Cop
open Ctypes
open Datatypes
open Globalenvs
open Integers
open LowValues
open MachineModel

type function_kind =
| Coq_normalFunction
| Coq_constructorFunction

type ret_type =
| Tvoid_method
| Tconstructor
| Tfun
| Tsome_method

type statement =
| Spush of (coq_val, label) sum
| Sdup of nat
| Ssload
| Smload
| Sunop of unary_operation
| Sbinop of binary_operation * bool
| Scall0 of builtin0
| Scall1 of builtin1
| Sskip
| Spop
| Ssstore
| Smstore
| Sswap of nat
| Sdone of ret_type
| Slabel of label
| Sjump
| Sjumpi
| Shash
| Stransfer
| Scallmethod of Int.int * nat * nat
| Slog of nat * nat
| Srevert
| Scalldataload
| Sconstructordataload of nat

type code = statement list

type coq_function =
  code
  (* singleton inductive, whose constructor was mkfunction *)

val fn_code : coq_function -> code

type genv = (coq_function, coq_type) Genv.t

type program = (genv, label) prod
