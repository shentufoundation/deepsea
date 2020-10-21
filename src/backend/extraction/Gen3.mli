open AST
open Ascii
open BinNums
open Coqlib
open Datatypes
open Globalenvs
open Maps0
open Monad
open OptErrMonad
open Semantics
open Specif
open StmtClinear
open String0
open Trees

val enumerate_rest : Semantics.code -> node list

val labels_of_bblock : bblock -> label list

val enumerate'_func : (node list, Semantics.code) sigT -> node list

val enumerate' : node list -> Semantics.code -> node list

val enumerate : Semantics.coq_function -> node list

val starts_with : label -> code -> bool

val add_branch : label -> code -> code

val clinear_block : bblock -> code -> code

val clinear_node : Semantics.coq_function -> node -> code -> code

val clinear_body : Semantics.coq_function -> node list -> code

val clinear_function : Semantics.coq_function -> coq_function optErr

val clinear_fundef : Semantics.coq_function -> coq_function optErr

val clinear_fundefs :
  Semantics.coq_function PTree.t -> coq_function PTree.t optErr

val clinear_methoddefs :
  Semantics.coq_function option IntMap.t -> coq_function option IntMap.t
  optErr

val clinear_constructor : Semantics.coq_function option -> coq_function optErr

val clinear_genv : Semantics.genv -> genv optErr
