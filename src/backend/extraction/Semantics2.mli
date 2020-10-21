open AST
open BinNums
open Cop
open Coqlib
open Datatypes
open Globalenvs
open Integers
open Labels
open List0
open LowValues
open MachineModel
open Maps0
open PeanoNat
open Specif
open StmtExpressionless

val code_labels : code -> label list

val function_labels : coq_function -> label list

val genv_list_labels : genv -> label list

val stm_eq_dec : statement -> statement -> sumbool

val function_starts_with_label : (label, coq_function) prod -> bool

val all_true : bool list -> bool

val functions_start_with_labels : (label, coq_function) prod list -> bool

val label_verify : genv -> bool
