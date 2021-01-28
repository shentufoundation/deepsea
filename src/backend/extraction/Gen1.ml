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

(** val st_nextnode : state -> positive **)

let st_nextnode x = x.st_nextnode

(** val st_code : state -> code **)

let st_code x = x.st_code

(** val init_state : state **)

let init_state =
  { st_nextnode = Coq_xH; st_code = PTree.empty }

type 'a res =
| Fail
| OK of 'a * state

type 'a mon = state -> 'a res

(** val coq_Monad_mon : __ mon coq_Monad **)

let coq_Monad_mon =
  { ret = (fun _ x s -> OK (x, s)); bind = (fun _ _ f g s ->
    match f s with
    | Fail -> Fail
    | OK (a, s') -> g a s') }

(** val error : 'a1 mon **)

let error _ =
  Fail

(** val add_instr : StmtCGraph.statement -> node mon **)

let add_instr i s =
  let n = s.st_nextnode in
  OK (n, { st_nextnode = (Pos.succ n); st_code = (PTree.set n i s.st_code) })

(** val reserve_instr : node mon **)

let reserve_instr s =
  let n = s.st_nextnode in
  OK (n, { st_nextnode = (Pos.succ n); st_code = s.st_code })

(** val check_empty_node : state -> node -> sumbool **)

let check_empty_node s n =
  match PTree.get n s.st_code with
  | Some _ -> Coq_right
  | None -> Coq_left

(** val update_instr : node -> StmtCGraph.statement -> coq_unit mon **)

let update_instr n i s =
  match plt n s.st_nextnode with
  | Coq_left ->
    (match check_empty_node s n with
     | Coq_left ->
       OK (Coq_tt, { st_nextnode = s.st_nextnode; st_code =
         (PTree.set n i s.st_code) })
     | Coq_right -> Fail)
  | Coq_right -> Fail

(** val cgraph_statement :
    statement -> node -> node -> node -> node option -> node mon **)

let rec cgraph_statement s nd nret nrev nbrk =
  match s with
  | Sskip -> ret (Obj.magic coq_Monad_mon) nd
  | Ssassign (lv, rv) -> add_instr (StmtCGraph.Ssassign (lv, rv, nd))
  | Smassign (lv, rv) -> add_instr (StmtCGraph.Smassign (lv, rv, nd))
  | Sset (id, rv) -> add_instr (StmtCGraph.Sset (id, rv, nd))
  | Scall (retval, lab, args) ->
    add_instr (StmtCGraph.Scall (retval, lab, args, nd))
  | Ssequence (s1, s2) ->
    bind (Obj.magic coq_Monad_mon) (cgraph_statement s2 nd nret nrev nbrk)
      (fun ns -> cgraph_statement s1 ns nret nrev nbrk)
  | Sifthenelse (c, strue, sfalse) ->
    bind (Obj.magic coq_Monad_mon)
      (cgraph_statement sfalse nd nret nrev nbrk) (fun nfalse ->
      bind (Obj.magic coq_Monad_mon)
        (cgraph_statement strue nd nret nrev nbrk) (fun ntrue ->
        add_instr (Scond (c, ntrue, nfalse))))
  | Sloop sbody ->
    bind (Obj.magic coq_Monad_mon) reserve_instr (fun n1 ->
      bind (Obj.magic coq_Monad_mon)
        (cgraph_statement sbody n1 nret nrev (Some nd)) (fun n2 ->
        bind (Obj.magic coq_Monad_mon)
          (Obj.magic update_instr n1 (StmtCGraph.Sskip n2)) (fun _ ->
          ret (Obj.magic coq_Monad_mon) n1)))
  | Sbreak ->
    (match nbrk with
     | Some nbrk0 -> ret (Obj.magic coq_Monad_mon) nbrk0
     | None -> error)
  | Sreturn retvar -> add_instr (StmtCGraph.Sreturn (retvar, nret))
  | Shash (ex1, ex2, exo) -> add_instr (StmtCGraph.Shash (ex1, ex2, exo, nd))
  | Stransfer (a, v) -> add_instr (StmtCGraph.Stransfer (a, v, nrev, nd))
  | Scallmethod (a, rvs, sig0, v, g, args) ->
    add_instr (StmtCGraph.Scallmethod (a, rvs, sig0, v, g, args, nrev, nd))
  | Slog (topics, args) -> add_instr (StmtCGraph.Slog (topics, args, nd))
  | Srevert -> ret (Obj.magic coq_Monad_mon) nrev

(** val cgraph_function : coq_function -> StmtCGraph.coq_function optErr **)

let cgraph_function f =
  let cgraph_fun =
    bind (Obj.magic coq_Monad_mon) (add_instr Sdone) (fun nret ->
      bind (Obj.magic coq_Monad_mon) (add_instr StmtCGraph.Srevert)
        (fun nrev -> cgraph_statement f.fn_body nret nret nrev None))
  in
  (match cgraph_fun init_state with
   | Fail ->
     Error (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
       Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii
       (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
       Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
       Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
       (Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
       Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
       Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
       (Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
       Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
       Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
       (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
       Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
       Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
       (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
       Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
       Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
       (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
       Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
       Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
       (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
       Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
       (Coq_false, Coq_true, Coq_false, Coq_false, Coq_false, Coq_true,
       Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
       Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
       (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
       Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
       Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
       (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_true, Coq_false,
       Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
       Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
       (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
       Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
       (Coq_false, Coq_true, Coq_true, Coq_false, Coq_false, Coq_true,
       Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
       Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
       (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
       Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
       Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
       Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
       Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
       (Coq_false, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
       Coq_true, Coq_false)),
       EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))
   | OK (nentry, s) ->
     ret (Obj.magic coq_Monad_optErr) { StmtCGraph.fn_return = f.fn_return;
       StmtCGraph.fn_params = f.fn_params; StmtCGraph.fn_temps = f.fn_temps;
       StmtCGraph.fn_locals = f.fn_locals; fn_code = s.st_code;
       fn_entrypoint = nentry; fn_nextnode = s.st_nextnode })

(** val empty_constructor : coq_function **)

let empty_constructor =
  { fn_return = Tvoid; fn_params = Coq_nil; fn_temps = Coq_nil; fn_locals =
    Coq_nil; fn_body = Sskip }

(** val cgraph_constructor :
    coq_function option -> StmtCGraph.coq_function option optErr **)

let cgraph_constructor = function
| Some f0 ->
  bind (Obj.magic coq_Monad_optErr) (Obj.magic cgraph_function f0) (fun cf ->
    ret (Obj.magic coq_Monad_optErr) (Some cf))
| None ->
  bind (Obj.magic coq_Monad_optErr)
    (Obj.magic cgraph_function empty_constructor) (fun cf ->
    ret (Obj.magic coq_Monad_optErr) (Some cf))

(** val cgraph_functions :
    coq_function PTree.t -> StmtCGraph.coq_function PTree.t optErr **)

let cgraph_functions defs =
  transl_tree cgraph_function defs

(** val cgraph_methoddefs :
    coq_function option IntMap.t -> StmtCGraph.coq_function option IntMap.t
    optErr **)

let cgraph_methoddefs defs =
  transl_map cgraph_function defs

(** val cgraph_genv : genv -> StmtCGraph.genv optErr **)

let cgraph_genv ge =
  let vars = ge.Genv.genv_vars in
  let names = ge.Genv.genv_funcs in
  let fundefs = ge.Genv.genv_fundefs in
  let sigs = ge.Genv.genv_methods in
  let defs = ge.Genv.genv_defs in
  let methoddefs = ge.Genv.genv_methoddefs in
  let constructor = ge.Genv.genv_constructor in
  bind (Obj.magic coq_Monad_optErr) (Obj.magic cgraph_functions fundefs)
    (fun fundefs0 ->
    bind (Obj.magic coq_Monad_optErr)
      (Obj.magic cgraph_methoddefs methoddefs) (fun methoddefs0 ->
      bind (Obj.magic coq_Monad_optErr)
        (Obj.magic cgraph_constructor constructor) (fun constructor0 ->
        ret (Obj.magic coq_Monad_optErr) { Genv.genv_vars = vars;
          Genv.genv_funcs = names; Genv.genv_methods = sigs; Genv.genv_defs =
          defs; Genv.genv_fundefs = fundefs0; Genv.genv_methoddefs =
          methoddefs0; Genv.genv_constructor = constructor0 })))
