open AST
open Datatypes
open Globalenvs
open Labels
open List0
open Maps0
open Specif
open StmtClinear

val label_code : code -> label list

val label_function : coq_function -> bool

val xlabel_functions : coq_function option list -> bool

val label_functions : coq_function PTree.t -> bool

val label_methods : coq_function option IntMap.t -> bool
