open AST
open Cop
open Ctypes
open Datatypes
open Globalenvs
open Integers
open MachineModel
open Values

type ret_type =
| Tvoid_method
| Tconstructor
| Tfun
| Tsome_method

type statement =
| Spush of (coq_val, label) sum
| Sdup of nat
| Ssload
| Sunop of unary_operation
| Sbinop of binary_operation * bool
| Scall0 of builtin0
| Scall1 of builtin1
| Sskip
| Spop
| Ssstore
| Sswap of nat
| Sdone of ret_type
| Slabel of label
| Sjump
| Sjumpi
| Stransfer
| Scallmethod of Int.int * nat * nat
| Slog of nat * nat
| Srevert
| Scalldataload
| Smload
| Scodecopy
| TotalLength

type code = statement list

type coq_function =
  code
  (* singleton inductive, whose constructor was mkfunction *)

(** val fn_code : coq_function -> code **)

let fn_code f =
  f

type genv = (coq_function, coq_type) Genv.t

type program = (genv, label) prod
