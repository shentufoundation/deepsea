open Ascii
open BinNums
open Cop
open Ctypes
open Datatypes
open ExpMiniC
open Globalenvs
open Integers
open Maps0
open Monad
open OptErrMonad
open StmtMiniC
open String0
open Trees

val constofpos : positive -> expr

val clike_rvalue : expr -> expr optErr

val clike_lvalue : expr -> expr optErr

val clike_rvalue_list : expr list -> expr list optErr

val clike_stm : statement -> statement optErr

val clike_function : coq_function -> coq_function optErr

val clike_constructor : coq_function option -> coq_function option optErr

val clike_functions : coq_function PTree.t -> coq_function PTree.t optErr

val clike_methoddefs :
  coq_function option IntMap.t -> coq_function option IntMap.t optErr

val clike_genv : genv -> genv optErr
