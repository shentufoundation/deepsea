open AST
open Ascii
open BinNums
open BinPos
open Coqlib
open Ctypes
open Datatypes
open ExpCintptr
open Globalenvs
open List0
open Maps0
open Monad
open Nat0
open OptErrMonad
open PeanoNat
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

type set = bool PTree.t

type coq_LVDomain = set

val set_empty : set

val set_union : positive list -> set -> set -> set -> set

val set_minus : positive list -> set -> set -> set -> set

val set_add : ident -> set -> set

val set_in : ident -> set -> bool

val set_card : set -> nat

val set_eq : set -> set -> bool

val set_fold_left : ('a1 -> ident -> 'a1) -> set -> 'a1 -> 'a1

val is_exit : node -> node -> node -> bool

val get_use_expr : expr -> coq_LVDomain optErr

val get_use_exprs : expr list -> coq_LVDomain optErr

val get_use : StmtCGraph.statement -> coq_LVDomain optErr

val get_successor : StmtCGraph.statement -> set

val use : code -> node -> coq_LVDomain optErr

val def : code -> node -> coq_LVDomain optErr

val f_b : code -> node -> coq_LVDomain -> coq_LVDomain optErr

type coq_CD = coq_LVDomain PTree.t

val initialize : nat -> coq_CD -> coq_CD

val empty_domain : set

val get_lv :
  code -> node -> node -> nat -> coq_CD -> coq_CD -> (coq_CD, coq_CD) prod
  optErr

val get_lv_mfp :
  code -> nat -> node -> node -> nat -> coq_CD -> coq_CD -> coq_CD optErr

val compute_livevar : code -> nat -> node -> node -> nat -> coq_CD optErr

type tvs = set

type clashg = tvs PTree.t

val create_clash_graph : coq_CD -> clashg optErr

val remove_node_graph : ident -> clashg -> clashg

val least_degree : clashg -> ident

val pop_graph : nat -> clashg -> ident list -> ident list optErr

type colorMap = nat PTree.t

val get_assigned_colors : colorMap -> tvs -> set

val colorMapFull : set

val max_reg : nat

val default_reg : nat

val assign_color : ident list -> clashg -> colorMap -> colorMap optErr

val recolor_expr : expr -> colorMap -> expr optErr

val recolor_exprs : expr list -> colorMap -> expr list optErr

val recolor_regs : ident list -> colorMap -> ident list -> ident list optErr

val recolor_reg : ident -> colorMap -> ident

val recolor :
  code -> set -> nat -> node -> colorMap -> (code -> code) -> (code -> code)
  optErr

val color_graph :
  code -> nat -> node -> clashg -> (code, colorMap) prod optErr

val variable_coalescing :
  code -> nat -> node -> node -> node -> nat -> (code, colorMap) prod optErr

val get_clash_graph :
  code -> nat -> node -> node -> nat -> (positive, positive list) prod list
  optErr

val max_iteration : nat

val cgraph_function : coq_function -> StmtCGraph.coq_function optErr

val clash_function :
  coq_function -> (positive, positive list) prod list optErr

val empty_constructor : coq_function

val cgraph_constructor :
  coq_function option -> StmtCGraph.coq_function option optErr

val cgraph_functions :
  coq_function PTree.t -> StmtCGraph.coq_function PTree.t optErr

val cgraph_methoddefs :
  coq_function option IntMap.t -> StmtCGraph.coq_function option IntMap.t
  optErr

val graphviz_helper :
  StmtCGraph.coq_function -> ((positive, StmtCGraph.statement) prod list,
  positive) prod

val clash_viz : genv -> (positive, positive list) prod list list optErr

val cgraph_viz :
  genv -> ((positive, StmtCGraph.statement) prod list, positive) prod list
  optErr

val cgraph_genv : genv -> StmtCGraph.genv optErr
