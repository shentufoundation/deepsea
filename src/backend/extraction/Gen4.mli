open AST
open Ascii
open Datatypes
open Globalenvs
open Integers
open Labels
open List0
open Maps0
open Monad
open OptErrMonad
open Options
open Semantics0
open StmtClinear
open String0
open Trees

val nodes_in_code : code -> label list

val allocate_labels : fd_label -> code -> label_map -> label_map

val allocate_labels_fundef :
  fd_label -> coq_function -> label_map -> label_map

val xallocate_labels_fun :
  (label, coq_function) prod list -> label_map -> label_map

val allocate_labels_fun : coq_function PTree.t -> label_map -> label_map

val allocate_labels_methods :
  Int.int list -> coq_function option IntMap.t -> label_map -> label_map

val allocate_labels_constructor :
  coq_function option -> label_map -> label_map

val allocate_labels_ge : genv -> label_map

val clabeled_stm : fd_label -> label_map -> statement -> statement optErr

val clabeled_code : fd_label -> label_map -> code -> code optErr

val fn_is_method : fd_label -> bool

val clabeled_function_body : label -> fd_label -> code -> code

val clabeled_function :
  label_map -> fd_label -> coq_function -> coq_function optErr

val clabeled_fundef :
  label_map -> fd_label -> coq_function -> coq_function optErr

val clabeled_fundef_f :
  label_map -> label -> coq_function -> (label, coq_function) prod optErr

val clabeled_fundefs :
  label_map -> coq_function PTree.t -> coq_function PTree.t optErr

val xclabeled_methods :
  label_map -> Int.int list -> coq_function option IntMap.t -> coq_function
  option IntMap.t optErr

val clabeled_methods :
  label_map -> Int.int list -> coq_function option IntMap.t -> coq_function
  option IntMap.t optErr

val clabeled_constructor :
  label_map -> coq_function option -> coq_function optErr

val clabeled_genv : label_map -> genv -> genv optErr

val clabeled_program : genv -> program optErr
