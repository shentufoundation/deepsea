open AST
open Ascii
open BinNums
open BinPos
open Datatypes
open IndexLib
open Integers
open Maps0
open Monad
open OptErrMonad
open OptionMonad
open Options
open Specif
open String0

type label_map = { lm_map : label PTree.t; lm_max : label }

val lm_map : label_map -> label PTree.t

val lm_max : label_map -> label

val allocate_label : positive -> label_map -> label_map

val xallocate_labels : label list -> label_map -> label_map

val empty_label_map : label_map

type fd_label =
| Lfun of label
| Lconstructor
| Lmethod of Int.int
| Lmultiplexer of Int.int
| Lbody

type node_label =
| Lfun_node of label * label * positive
| Lconstructor_node of label * positive
| Lmethod_node of Int.int * label * positive

type label_type = (fd_label, node_label) sum

val node_within' : fd_label -> label -> positive -> label_type

val node_within : fd_label -> label -> label_type

val call_node_within : fd_label -> label -> label_type

val key : label_type -> positive

val label_key : label_map -> label_type -> label optErr

val elt_key : fd_label -> label -> positive

val call_elt_key : fd_label -> label -> positive

val labels_no_dups : label list -> coq_unit PTree.t option

val label_norepet_dec : label list -> sumbool

val decide_label_norepet : label list -> bool
