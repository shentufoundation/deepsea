open Ascii
open Datatypes
open Globalenvs
open Maps0
open Monad
open OptErrMonad
open Semantics
open StmtCGraph
open StmtClinear
open String0
open Trees

val cbasic_stm : bool option -> node -> StmtCGraph.statement -> bblock

val cbasic_code : bool option -> StmtCGraph.code -> Semantics.code

val cbasic_function :
  bool option -> StmtCGraph.coq_function -> Semantics.coq_function

val cbasic_fundef :
  bool option -> StmtCGraph.coq_function -> Semantics.coq_function optErr

val cbasic_fundefs :
  StmtCGraph.coq_function PTree.t -> Semantics.coq_function PTree.t optErr

val cbasic_methoddefs :
  StmtCGraph.coq_function option IntMap.t -> Semantics.coq_function option
  IntMap.t optErr

val cbasic_constructor :
  StmtCGraph.coq_function option -> Semantics.coq_function optErr

val cbasic_genv : StmtCGraph.genv -> Semantics.genv optErr
