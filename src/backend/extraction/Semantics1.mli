open AST
open Datatypes
open Integers
open Labels
open Language5
open Values

val sg_shift : nat

val z0 : (coq_val, label) sum

val code_labels : code -> label list

val label_verify : code -> bool
