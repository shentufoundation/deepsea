open AST
open Ascii
open BinNums
open BinPos
open Datatypes
open ExpCintptr
open Globalenvs
open List0
open Maps0
open Monad
open Nat0
open OptErrMonad
open PeanoNat
open StmtCGraph
open String0
open Trees

type set = bool PTree.t

type coq_LVDomain = set

val set_empty : set

val set_union' : positive list -> set -> set -> set -> set

val set_union : set -> set -> set

val set_minus' : positive list -> set -> set -> set -> set

val set_minus : bool PTree.t -> set -> set

val set_add : ident -> set -> set

val set_in : ident -> set -> bool

val set_card : set -> nat

val set_eq : set -> set -> bool

val set_fold_left : ('a1 -> ident -> 'a1) -> set -> 'a1 -> 'a1

val get_use_expr : expr -> coq_LVDomain

val get_use_exprs : expr list -> coq_LVDomain

val get_use : statement -> coq_LVDomain

val get_successor : statement -> node list

val get_def : statement -> coq_LVDomain

type coq_CD = coq_LVDomain PTree.t

val f_b : code -> node -> coq_CD -> coq_LVDomain

val update_at : code -> coq_CD -> node -> coq_CD

val lv_top : code -> coq_LVDomain

val cd_top : code -> coq_CD

val get_lv_once : code -> node list -> coq_CD -> coq_CD

val get_lv_mfp : code -> node list -> nat -> coq_CD -> coq_CD

val compute_livevar : code -> nat -> coq_CD optErr

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

val recolor_expr_opt : expr option -> colorMap -> expr option optErr

val recolor :
  code -> set -> nat -> node -> colorMap -> (code -> code) -> (code -> code)
  optErr

val color_graph :
  code -> nat -> node -> clashg -> (code, colorMap) prod optErr

val variable_coalescing :
  code -> nat -> node -> nat -> (code, colorMap) prod optErr

val get_clash_graph :
  code -> nat -> (positive, positive list) prod list optErr

val max_iteration : nat

val copt_function : coq_function -> coq_function optErr

val copt_constructor : coq_function option -> coq_function option optErr

val clash_function :
  coq_function -> (positive, positive list) prod list optErr

val copt_functions : coq_function PTree.t -> coq_function PTree.t optErr

val copt_methoddefs :
  coq_function option IntMap.t -> coq_function option IntMap.t optErr

val graphviz_helper :
  coq_function -> ((positive, statement) prod list, positive) prod

val clash_viz : genv -> (positive, positive list) prod list list optErr

val cgraph_viz :
  genv -> ((positive, statement) prod list, positive) prod list optErr

val copt_genv : genv -> genv optErr
