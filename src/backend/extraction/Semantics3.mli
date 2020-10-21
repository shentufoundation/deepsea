open AST
open Datatypes
open Integers
open Labels
open LowValues
open StmtExpressionless

val sg_shift : nat

val z0 : (coq_val, label) sum

val code_labels : code -> label list

val label_verify : code -> bool
