open Ascii
open BinNums
open BinPos
open Coqlib
open Ctypes
open Datatypes
open Globalenvs
open Language1
open Language
open Maps0
open Monad
open OptErrMonad
open Specif
open String0
open Trees

type __ = Obj.t

type state = { st_nextnode : positive; st_code : code }

val st_nextnode : state -> positive

val st_code : state -> code

val init_state : state

type 'a res =
| Fail
| OK of 'a * state

type 'a mon = state -> 'a res

val coq_Monad_mon : __ mon coq_Monad

val error : 'a1 mon

val add_instr : Language1.statement -> node mon

val reserve_instr : node mon

val check_empty_node : state -> node -> sumbool

val update_instr : node -> Language1.statement -> coq_unit mon

val cgraph_statement :
  statement -> node -> node -> node -> node option -> node mon

val cgraph_function : coq_function -> Language1.coq_function optErr

val empty_constructor : coq_function

val cgraph_constructor :
  coq_function option -> Language1.coq_function option optErr

val cgraph_functions :
  coq_function PTree.t -> Language1.coq_function PTree.t optErr

val cgraph_methoddefs :
  coq_function option IntMap.t -> Language1.coq_function option IntMap.t
  optErr

val cgraph_genv : genv -> Language1.genv optErr
