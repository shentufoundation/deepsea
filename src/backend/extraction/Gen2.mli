open AST
open Ascii
open BinNums
open Coqlib
open Datatypes
open Globalenvs
open Language3
open Language1
open Language2
open Maps0
open Monad
open OptErrMonad
open Specif
open String0
open Trees

val enumerate_rest : Language3.code -> node list

val labels_of_bblock : bblock -> label list

val enumerate'_func : (node list, Language3.code) sigT -> node list

val enumerate' : node list -> Language3.code -> node list

val enumerate : Language3.coq_function -> node list

val starts_with : label -> code -> bool

val add_branch : label -> code -> code

val clinear_block : bblock -> code -> code

val clinear_node : Language3.coq_function -> node -> code -> code

val clinear_body : Language3.coq_function -> node list -> code

val clinear_function : Language3.coq_function -> coq_function optErr

val clinear_fundef : Language3.coq_function -> coq_function optErr

val clinear_fundefs :
  Language3.coq_function PTree.t -> coq_function PTree.t optErr

val clinear_methoddefs :
  Language3.coq_function option IntMap.t -> coq_function option IntMap.t
  optErr

val clinear_constructor : Language3.coq_function option -> coq_function optErr

val clinear_genv : Language3.genv -> genv optErr
