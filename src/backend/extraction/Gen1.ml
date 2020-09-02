open Ascii
open Datatypes
open Globalenvs
open Language3
open Language1
open Language2
open Maps0
open Monad
open OptErrMonad
open String0
open Trees

(** val cbasic_stm : bool option -> node -> Language1.statement -> bblock **)

let cbasic_stm ismethod nthis = function
| Sskip n -> Coq_cons ((Sjump n), Coq_nil)
| Language1.Sassign (lv, rv, n) ->
  Coq_cons ((Sassign (lv, rv)), (Coq_cons ((Sjump n), Coq_nil)))
| Language1.Sset (id, rv, n) ->
  Coq_cons ((Sset (id, rv)), (Coq_cons ((Sjump n), Coq_nil)))
| Language1.Scall (retval, lab, args, n) ->
  Coq_cons ((Scall (retval, lab, args, nthis)), (Coq_cons ((Sjump n),
    Coq_nil)))
| Scond (cond, ntrue, nfalse) ->
  Coq_cons ((Sjumpi (cond, ntrue)), (Coq_cons ((Sjump nfalse), Coq_nil)))
| Language1.Sreturn (val0, n) ->
  Coq_cons ((Sreturn val0), (Coq_cons ((Sjump n), Coq_nil)))
| Language1.Sdone -> Coq_cons ((Sdone ismethod), Coq_nil)
| Language1.Stransfer (a, v, nfail, n) ->
  Coq_cons ((Stransfer (a, v, nfail)), (Coq_cons ((Sjump n), Coq_nil)))
| Language1.Scallmethod (a, rvs, sig0, v, args, nfail, n) ->
  Coq_cons ((Scallmethod (a, rvs, sig0, v, args, nfail)), (Coq_cons ((Sjump
    n), Coq_nil)))
| Language1.Slog (topics, args, n) ->
  Coq_cons ((Slog (topics, args)), (Coq_cons ((Sjump n), Coq_nil)))
| Language1.Srevert -> Coq_cons (Srevert, Coq_nil)

(** val cbasic_code : bool option -> Language1.code -> Language3.code **)

let cbasic_code ismethod c =
  PTree.map (cbasic_stm ismethod) c

(** val cbasic_function :
    bool option -> Language1.coq_function -> Language3.coq_function **)

let cbasic_function ismethod f =
  { Language3.fn_return = f.Language1.fn_return; Language3.fn_params =
    f.Language1.fn_params; Language3.fn_temps = f.Language1.fn_temps;
    Language3.fn_code = (cbasic_code ismethod f.Language1.fn_code);
    Language3.fn_entrypoint = f.fn_entrypoint }

(** val cbasic_fundef :
    bool option -> Language1.coq_function -> Language3.coq_function optErr **)

let cbasic_fundef ismethod f =
  ret (Obj.magic coq_Monad_optErr) (cbasic_function ismethod f)

(** val cbasic_fundefs :
    Language1.coq_function PTree.t -> Language3.coq_function PTree.t optErr **)

let cbasic_fundefs t0 =
  transl_tree (cbasic_fundef (Some Coq_false)) t0

(** val cbasic_methoddefs :
    Language1.coq_function option IntMap.t -> Language3.coq_function option
    IntMap.t optErr **)

let cbasic_methoddefs methods =
  transl_map (cbasic_fundef (Some Coq_true)) methods

(** val cbasic_constructor :
    Language1.coq_function option -> Language3.coq_function optErr **)

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

(** val cbasic_genv : Language1.genv -> Language3.genv optErr **)

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
