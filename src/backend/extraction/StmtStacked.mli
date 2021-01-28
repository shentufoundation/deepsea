open AST
open Ctypes
open Datatypes
open ExpStacked
open Globalenvs
open Integers

type statement =
| Sskip
| Srvalue of expr
| Slvalue of expr
| Spushvoid
| Spop
| Ssassign
| Smassign
| Sset of nat
| Sdone of nat * ret_type
| Spushlabel of typed_label
| Slabel of label
| Sjump_call
| Sjump_internal
| Sjumpi
| Shash
| Stransfer
| Scallargs of Int.int * nat * nat
| Scallmethod of bool
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
