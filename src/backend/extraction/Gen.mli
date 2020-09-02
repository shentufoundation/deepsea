open AST
open Ascii
open BinInt
open Cop
open Ctypes
open Datatypes
open Globalenvs
open Integers
open Language
open Maps0
open Monad
open OptErrMonad
open Options
open String0
open Trees

val int_t : coq_type

val struct_lvalue : expr -> coq_type -> ident -> expr optErr

val array_lvalue : expr -> expr -> expr optErr

val hash_lvalue : expr -> expr -> expr optErr

val clike_rvalue : expr -> expr optErr

val clike_lvalue : expr -> expr optErr

val clike_rvalue_list : expr list -> expr list optErr

val clike_optvalue : expr option -> expr option optErr

val clike_stm : statement -> statement optErr

val clike_function : coq_function -> coq_function optErr

val clike_constructor : coq_function option -> coq_function option optErr

val clike_functions : coq_function PTree.t -> coq_function PTree.t optErr

val clike_methoddefs :
  coq_function option IntMap.t -> coq_function option IntMap.t optErr

val clike_genv : genv -> genv optErr
