open Ascii
open BinNums
open BinPos
open Coqlib
open Ctypes
open Datatypes
open Globalenvs
open Maps0
open Monad
open OptErrMonad
open Specif
open StmtCGraph
open StmtCintptr
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

val add_instr : StmtCGraph.statement -> node mon

val reserve_instr : node mon

val check_empty_node : state -> node -> sumbool

val update_instr : node -> StmtCGraph.statement -> coq_unit mon

val cgraph_statement :
  statement -> node -> node -> node -> node option -> node mon

val cgraph_function : coq_function -> StmtCGraph.coq_function optErr

val empty_constructor : coq_function

val cgraph_constructor :
  coq_function option -> StmtCGraph.coq_function option optErr

val cgraph_functions :
  coq_function PTree.t -> StmtCGraph.coq_function PTree.t optErr

val cgraph_methoddefs :
  coq_function option IntMap.t -> StmtCGraph.coq_function option IntMap.t
  optErr

val cgraph_genv : genv -> StmtCGraph.genv optErr
