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

(** val cbasic_stm :
    bool option -> node -> StmtCGraph.statement -> bblock **)

let cbasic_stm ismethod nthis = function
| Sskip n -> Coq_cons ((Sjump n), Coq_nil)
| StmtCGraph.Smassign (lv, rv, n) ->
  Coq_cons ((Smassign (lv, rv)), (Coq_cons ((Sjump n), Coq_nil)))
| StmtCGraph.Ssassign (lv, rv, n) ->
  Coq_cons ((Ssassign (lv, rv)), (Coq_cons ((Sjump n), Coq_nil)))
| StmtCGraph.Sset (id, rv, n) ->
  Coq_cons ((Sset (id, rv)), (Coq_cons ((Sjump n), Coq_nil)))
| StmtCGraph.Scall (retval, lab, args, n) ->
  Coq_cons ((Scall (retval, lab, args, nthis)), (Coq_cons ((Sjump n),
    Coq_nil)))
| Scond (cond, ntrue, nfalse) ->
  Coq_cons ((Sjumpi (cond, ntrue)), (Coq_cons ((Sjump nfalse), Coq_nil)))
| StmtCGraph.Sreturn (var, n) ->
  Coq_cons ((Sreturn var), (Coq_cons ((Sjump n), Coq_nil)))
| StmtCGraph.Sdone -> Coq_cons ((Sdone ismethod), Coq_nil)
| StmtCGraph.Shash (ex1, ex2, exo, n) ->
  Coq_cons ((Shash (ex1, ex2, exo)), (Coq_cons ((Sjump n), Coq_nil)))
| StmtCGraph.Stransfer (a, v, nfail, n) ->
  Coq_cons ((Stransfer (a, v, nfail)), (Coq_cons ((Sjump n), Coq_nil)))
| StmtCGraph.Scallmethod (a, rvs, sig0, v, gas, args, nfail, n) ->
  Coq_cons ((Scallmethod (a, rvs, sig0, v, gas, args, nfail)), (Coq_cons
    ((Sjump n), Coq_nil)))
| StmtCGraph.Slog (topics, args, n) ->
  Coq_cons ((Slog (topics, args)), (Coq_cons ((Sjump n), Coq_nil)))
| StmtCGraph.Srevert -> Coq_cons (Srevert, Coq_nil)

(** val cbasic_code : bool option -> StmtCGraph.code -> Semantics.code **)

let cbasic_code ismethod c =
  PTree.map (cbasic_stm ismethod) c

(** val cbasic_function :
    bool option -> StmtCGraph.coq_function -> Semantics.coq_function **)

let cbasic_function ismethod f =
  { Semantics.fn_return = f.StmtCGraph.fn_return; Semantics.fn_params =
    f.StmtCGraph.fn_params; Semantics.fn_temps = f.StmtCGraph.fn_temps;
    Semantics.fn_locals = f.StmtCGraph.fn_locals; Semantics.fn_code =
    (cbasic_code ismethod f.StmtCGraph.fn_code); Semantics.fn_entrypoint =
    f.fn_entrypoint }

(** val cbasic_fundef :
    bool option -> StmtCGraph.coq_function -> Semantics.coq_function optErr **)

let cbasic_fundef ismethod f =
  ret (Obj.magic coq_Monad_optErr) (cbasic_function ismethod f)

(** val cbasic_fundefs :
    StmtCGraph.coq_function PTree.t -> Semantics.coq_function PTree.t optErr **)

let cbasic_fundefs t0 =
  transl_tree (cbasic_fundef (Some Coq_false)) t0

(** val cbasic_methoddefs :
    StmtCGraph.coq_function option IntMap.t -> Semantics.coq_function option
    IntMap.t optErr **)

let cbasic_methoddefs methods =
  transl_map (cbasic_fundef (Some Coq_true)) methods

(** val cbasic_constructor :
    StmtCGraph.coq_function option -> Semantics.coq_function optErr **)

let cbasic_constructor = function
| Some c ->
  bind (Obj.magic coq_Monad_optErr) (cbasic_fundef None c) (fun f ->
    ret (Obj.magic coq_Monad_optErr) f)
| None ->
  Error (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false,
    Coq_false, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
    Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
    ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
    Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
    Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
    (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
    Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
    Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_false,
    Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_false,
    Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
    Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true,
    Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
    Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_false,
    Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_false,
    Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
    Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
    Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
    Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true,
    Coq_true, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true,
    Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
    Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
    (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true,
    Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
    Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
    ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
    Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
    Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
    (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false,
    Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
    Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
    (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
    Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
    Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
    (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
    Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
    Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
    ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
    Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
    Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
    (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
    Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
    Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
    Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
    Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
    Coq_false)),
    EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

(** val cbasic_genv : StmtCGraph.genv -> Semantics.genv optErr **)

let cbasic_genv ge =
  let vars = ge.Genv.genv_vars in
  let funcs = ge.Genv.genv_funcs in
  let methods = ge.Genv.genv_methods in
  let defs = ge.Genv.genv_defs in
  let fundefs = ge.Genv.genv_fundefs in
  let methoddefs = ge.Genv.genv_methoddefs in
  let constructor = ge.Genv.genv_constructor in
  bind (Obj.magic coq_Monad_optErr) (Obj.magic cbasic_fundefs fundefs)
    (fun fundefs0 ->
    bind (Obj.magic coq_Monad_optErr)
      (Obj.magic cbasic_methoddefs methoddefs) (fun methoddefs0 ->
      bind (Obj.magic coq_Monad_optErr)
        (Obj.magic cbasic_constructor constructor) (fun constructor0 ->
        ret (Obj.magic coq_Monad_optErr) { Genv.genv_vars = vars;
          Genv.genv_funcs = funcs; Genv.genv_methods = methods;
          Genv.genv_defs = defs; Genv.genv_fundefs = fundefs0;
          Genv.genv_methoddefs = methoddefs0; Genv.genv_constructor = (Some
          constructor0) })))
