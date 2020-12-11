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
  | Scallmethod (a, rvs, sig0, v, args) ->
    add_instr (StmtCGraph.Scallmethod (a, rvs, sig0, v, args, nrev, nd))
  | Slog (topics, args) -> add_instr (StmtCGraph.Slog (topics, args, nd))
  | Srevert -> ret (Obj.magic coq_Monad_mon) nrev

type set = bool PTree.t

type coq_LVDomain = set

(** val set_empty : set **)

let set_empty =
  PTree.empty

(** val set_union : positive list -> set -> set -> set -> set **)

let rec set_union keys x y r =
  match keys with
  | Coq_nil -> r
  | Coq_cons (hd, rest) ->
    set_union rest x y
      (PTree.set hd
        (match PTree.get_default Coq_false hd x with
         | Coq_true -> Coq_true
         | Coq_false -> PTree.get_default Coq_false hd y) r)

(** val set_minus : positive list -> set -> set -> set -> set **)

let rec set_minus keys x y r =
  match keys with
  | Coq_nil -> r
  | Coq_cons (hd, rest) ->
    set_minus rest x y
      (PTree.set hd
        (match PTree.get_default Coq_false hd x with
         | Coq_true -> negb (PTree.get_default Coq_false hd y)
         | Coq_false -> Coq_false) r)

(** val set_add : ident -> set -> set **)

let rec set_add i r =
  PTree.set i Coq_true r

(** val set_in : ident -> set -> bool **)

let rec set_in i r =
  PTree.get_default Coq_false i r

(** val set_card : set -> nat **)

let rec set_card x =
  PTree.fold1 (fun acc e ->
    match e with
    | Coq_true -> add acc (S O)
    | Coq_false -> acc) x O

(** val set_eq : set -> set -> bool **)

let rec set_eq x y =
  match Nat.eqb (set_card (set_minus (PTree.xkeys x Coq_xH) x y PTree.empty))
          O with
  | Coq_true ->
    Nat.eqb (set_card (set_minus (PTree.xkeys y Coq_xH) y x PTree.empty)) O
  | Coq_false -> Coq_false

(** val set_fold_left : ('a1 -> ident -> 'a1) -> set -> 'a1 -> 'a1 **)

let set_fold_left f m v =
  PTree.fold (fun acc i e ->
    match e with
    | Coq_true -> f acc i
    | Coq_false -> acc) m v

(** val is_exit : node -> node -> node -> bool **)

let is_exit nret nrev n =
  match Pos.eqb n nrev with
  | Coq_true -> Coq_true
  | Coq_false -> Pos.eqb n nret

(** val get_use_expr : expr -> coq_LVDomain optErr **)

let rec get_use_expr = function
| Etempvar (i, _) -> ret (Obj.magic coq_Monad_optErr) (set_add i set_empty)
| Esload (e0, _) ->
  bind (Obj.magic coq_Monad_optErr) (get_use_expr e0) (fun e' ->
    ret (Obj.magic coq_Monad_optErr) e')
| Emload (e0, _) ->
  bind (Obj.magic coq_Monad_optErr) (get_use_expr e0) (fun e' ->
    ret (Obj.magic coq_Monad_optErr) e')
| Eaddr (e0, _) ->
  bind (Obj.magic coq_Monad_optErr) (get_use_expr e0) (fun e' ->
    ret (Obj.magic coq_Monad_optErr) e')
| Eunop (_, e0, _) ->
  bind (Obj.magic coq_Monad_optErr) (get_use_expr e0) (fun e' ->
    ret (Obj.magic coq_Monad_optErr) e')
| Ebinop (_, e1, e2, _) ->
  bind (Obj.magic coq_Monad_optErr) (get_use_expr e1) (fun e1' ->
    bind (Obj.magic coq_Monad_optErr) (get_use_expr e2) (fun e2' ->
      ret (Obj.magic coq_Monad_optErr)
        (set_union (app (PTree.xkeys e1' Coq_xH) (PTree.xkeys e2' Coq_xH))
          e1' e2' PTree.empty)))
| Ecall1 (_, e0, _) ->
  bind (Obj.magic coq_Monad_optErr) (get_use_expr e0) (fun e' ->
    ret (Obj.magic coq_Monad_optErr) e')
| _ -> ret (Obj.magic coq_Monad_optErr) set_empty

(** val get_use_exprs : expr list -> coq_LVDomain optErr **)

let rec get_use_exprs = function
| Coq_nil -> ret (Obj.magic coq_Monad_optErr) set_empty
| Coq_cons (e, rest) ->
  bind (Obj.magic coq_Monad_optErr) (get_use_expr e) (fun e' ->
    bind (Obj.magic coq_Monad_optErr) (get_use_exprs rest) (fun rest' ->
      ret (Obj.magic coq_Monad_optErr)
        (set_union (app (PTree.xkeys rest' Coq_xH) (PTree.xkeys e' Coq_xH))
          rest' e' PTree.empty)))

(** val get_use : StmtCGraph.statement -> coq_LVDomain optErr **)

let get_use = function
| StmtCGraph.Smassign (lv, rv, _) ->
  bind (Obj.magic coq_Monad_optErr) (get_use_expr lv) (fun lv' ->
    bind (Obj.magic coq_Monad_optErr) (get_use_expr rv) (fun rv' ->
      ret (Obj.magic coq_Monad_optErr)
        (set_union (app (PTree.xkeys lv' Coq_xH) (PTree.xkeys rv' Coq_xH))
          lv' rv' PTree.empty)))
| StmtCGraph.Ssassign (lv, rv, _) ->
  bind (Obj.magic coq_Monad_optErr) (get_use_expr lv) (fun lv' ->
    bind (Obj.magic coq_Monad_optErr) (get_use_expr rv) (fun rv' ->
      ret (Obj.magic coq_Monad_optErr)
        (set_union (app (PTree.xkeys lv' Coq_xH) (PTree.xkeys rv' Coq_xH))
          lv' rv' PTree.empty)))
| StmtCGraph.Sset (_, rv, _) ->
  bind (Obj.magic coq_Monad_optErr) (get_use_expr rv) (fun rv' ->
    ret (Obj.magic coq_Monad_optErr) rv')
| StmtCGraph.Scall (_, _, args, _) ->
  bind (Obj.magic coq_Monad_optErr) (get_use_exprs args) (fun args' ->
    ret (Obj.magic coq_Monad_optErr) args')
| Scond (cond, _, _) ->
  bind (Obj.magic coq_Monad_optErr) (get_use_expr cond) (fun cond' ->
    ret (Obj.magic coq_Monad_optErr) cond')
| StmtCGraph.Sreturn (val0, _) ->
  (match val0 with
   | Some e -> ret (Obj.magic coq_Monad_optErr) (set_add e set_empty)
   | None -> ret (Obj.magic coq_Monad_optErr) set_empty)
| StmtCGraph.Shash (e1, e2, eo, _) ->
  bind (Obj.magic coq_Monad_optErr) (get_use_expr e1) (fun e1' ->
    bind (Obj.magic coq_Monad_optErr) (get_use_expr e2) (fun e2' ->
      match eo with
      | Some eoo ->
        bind (Obj.magic coq_Monad_optErr) (get_use_expr eoo) (fun eoo' ->
          ret (Obj.magic coq_Monad_optErr)
            (set_union
              (app
                (PTree.xkeys
                  (set_union
                    (app (PTree.xkeys e1' Coq_xH) (PTree.xkeys e2' Coq_xH))
                    e1' e2' PTree.empty) Coq_xH) (PTree.xkeys eoo' Coq_xH))
              (set_union
                (app (PTree.xkeys e1' Coq_xH) (PTree.xkeys e2' Coq_xH)) e1'
                e2' PTree.empty) eoo' PTree.empty))
      | None ->
        ret (Obj.magic coq_Monad_optErr)
          (set_union (app (PTree.xkeys e1' Coq_xH) (PTree.xkeys e2' Coq_xH))
            e1' e2' PTree.empty)))
| StmtCGraph.Stransfer (a, v, _, _) ->
  bind (Obj.magic coq_Monad_optErr) (get_use_expr a) (fun a' ->
    bind (Obj.magic coq_Monad_optErr) (get_use_expr v) (fun v' ->
      ret (Obj.magic coq_Monad_optErr)
        (set_union (app (PTree.xkeys a' Coq_xH) (PTree.xkeys v' Coq_xH)) a'
          v' PTree.empty)))
| StmtCGraph.Scallmethod (a, _, _, v, args, _, _) ->
  bind (Obj.magic coq_Monad_optErr) (get_use_expr a) (fun a' ->
    bind (Obj.magic coq_Monad_optErr) (get_use_expr v) (fun v' ->
      bind (Obj.magic coq_Monad_optErr) (get_use_exprs args) (fun args' ->
        ret (Obj.magic coq_Monad_optErr)
          (set_union
            (app
              (PTree.xkeys
                (set_union
                  (app (PTree.xkeys a' Coq_xH) (PTree.xkeys v' Coq_xH)) a' v'
                  PTree.empty) Coq_xH) (PTree.xkeys args' Coq_xH))
            (set_union (app (PTree.xkeys a' Coq_xH) (PTree.xkeys v' Coq_xH))
              a' v' PTree.empty) args' PTree.empty))))
| StmtCGraph.Slog (topics, args, _) ->
  bind (Obj.magic coq_Monad_optErr) (get_use_exprs topics) (fun topics' ->
    bind (Obj.magic coq_Monad_optErr) (get_use_exprs args) (fun args' ->
      ret (Obj.magic coq_Monad_optErr)
        (set_union
          (app (PTree.xkeys topics' Coq_xH) (PTree.xkeys args' Coq_xH))
          topics' args' PTree.empty)))
| _ -> ret (Obj.magic coq_Monad_optErr) set_empty

(** val get_successor : StmtCGraph.statement -> set **)

let get_successor = function
| StmtCGraph.Sskip n -> set_add n set_empty
| StmtCGraph.Smassign (_, _, n) -> set_add n set_empty
| StmtCGraph.Ssassign (_, _, n) -> set_add n set_empty
| StmtCGraph.Sset (_, _, n) -> set_add n set_empty
| StmtCGraph.Scall (_, _, _, n) -> set_add n set_empty
| Scond (_, ntrue, nfalse) ->
  set_union
    (app (PTree.xkeys (set_add ntrue set_empty) Coq_xH)
      (PTree.xkeys (set_add nfalse set_empty) Coq_xH))
    (set_add ntrue set_empty) (set_add nfalse set_empty) PTree.empty
| StmtCGraph.Sreturn (_, n) -> set_add n set_empty
| StmtCGraph.Shash (_, _, _, n) -> set_add n set_empty
| StmtCGraph.Stransfer (_, _, nfail, n) ->
  set_union
    (app (PTree.xkeys (set_add nfail set_empty) Coq_xH)
      (PTree.xkeys (set_add n set_empty) Coq_xH)) (set_add nfail set_empty)
    (set_add n set_empty) PTree.empty
| StmtCGraph.Scallmethod (_, _, _, _, _, nfail, n) ->
  set_union
    (app (PTree.xkeys (set_add nfail set_empty) Coq_xH)
      (PTree.xkeys (set_add n set_empty) Coq_xH)) (set_add nfail set_empty)
    (set_add n set_empty) PTree.empty
| StmtCGraph.Slog (_, _, n) -> set_add n set_empty
| _ -> set_empty

(** val use : code -> node -> coq_LVDomain optErr **)

let use cfg block =
  match PTree.get block cfg with
  | Some s ->
    bind (Obj.magic coq_Monad_optErr) (get_use s) (fun s' ->
      ret (Obj.magic coq_Monad_optErr) s')
  | None ->
    Error (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false,
      Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii
      (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
      Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
      Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
      Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
      Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
      Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
      Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
      Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
      Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
      Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
      Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
      Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
      Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
      Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
      Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
      Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
      (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
      Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
      (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_true,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
      Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
      (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
      Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
      (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_false, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
      Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
      ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
      Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
      Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
      Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true,
      Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
      Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
      Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_false,
      Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
      Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
      Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
      Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
      Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
      Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
      Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
      (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
      Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
      Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
      Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
      Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_false,
      Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
      Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_false, Coq_true, Coq_false, Coq_false, Coq_false, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
      Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
      Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
      (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
      Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
      ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
      Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
      ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
      Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
      Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
      EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

(** val def : code -> node -> coq_LVDomain optErr **)

let def cfg block =
  match PTree.get block cfg with
  | Some s ->
    (match s with
     | StmtCGraph.Sset (id, _, _) ->
       ret (Obj.magic coq_Monad_optErr) (set_add id set_empty)
     | StmtCGraph.Scall (retval, _, _, _) ->
       (match retval with
        | Some rv -> ret (Obj.magic coq_Monad_optErr) (set_add rv set_empty)
        | None -> ret (Obj.magic coq_Monad_optErr) set_empty)
     | StmtCGraph.Scallmethod (_, rvs, _, _, _, _, _) ->
       ret (Obj.magic coq_Monad_optErr)
         (fold_left (fun acc e -> set_add e acc) rvs set_empty)
     | _ -> ret (Obj.magic coq_Monad_optErr) set_empty)
  | None ->
    Error (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false,
      Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii
      (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
      Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
      Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
      Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
      Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
      Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
      Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
      Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
      Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
      Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
      Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
      Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
      Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
      Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
      Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
      Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
      (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
      Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
      (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
      Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
      (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_false, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
      Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
      (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_false, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
      Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
      ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
      Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
      Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
      Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true,
      Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
      Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
      Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_false,
      Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
      Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
      Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
      Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
      Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
      Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
      Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
      (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
      Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
      Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
      Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
      Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_false,
      Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
      Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_false, Coq_true, Coq_false, Coq_false, Coq_false, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
      Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
      Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
      (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
      Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
      ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
      Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
      ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
      Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
      Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
      EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

(** val f_b : code -> node -> coq_LVDomain -> coq_LVDomain optErr **)

let f_b cfg block x =
  bind (Obj.magic coq_Monad_optErr) (use cfg block) (fun u ->
    bind (Obj.magic coq_Monad_optErr) (def cfg block) (fun d ->
      ret (Obj.magic coq_Monad_optErr)
        (set_union
          (app (PTree.xkeys u Coq_xH)
            (PTree.xkeys (set_minus (PTree.xkeys x Coq_xH) x d PTree.empty)
              Coq_xH)) u (set_minus (PTree.xkeys x Coq_xH) x d PTree.empty)
          PTree.empty)))

type coq_CD = coq_LVDomain PTree.t

(** val initialize : nat -> coq_CD -> coq_CD **)

let rec initialize i s =
  match i with
  | O -> s
  | S x -> PTree.set (Pos.of_nat x) set_empty s

(** val empty_domain : set **)

let empty_domain =
  set_empty

(** val get_lv :
    code -> node -> node -> nat -> coq_CD -> coq_CD -> (coq_CD, coq_CD) prod
    optErr **)

let rec get_lv cfg nret nrev i iN oUT =
  let id = Pos.of_nat i in
  (match is_exit nret nrev id with
   | Coq_true ->
     (match i with
      | O -> ret (Obj.magic coq_Monad_optErr) (Coq_pair (iN, oUT))
      | S x -> get_lv cfg nret nrev x iN oUT)
   | Coq_false ->
     (match PTree.get id cfg with
      | Some stmt ->
        let oUT0 =
          PTree.set id
            (set_fold_left (fun acc e ->
              set_union
                (app (PTree.xkeys acc Coq_xH)
                  (PTree.xkeys (PTree.get_default set_empty e iN) Coq_xH))
                acc (PTree.get_default set_empty e iN) PTree.empty)
              (get_successor stmt) empty_domain) oUT
        in
        bind (Obj.magic coq_Monad_optErr)
          (Obj.magic f_b cfg id (PTree.get_default set_empty id oUT0))
          (fun v ->
          let iN0 = PTree.set id v iN in
          (match i with
           | O -> ret (Obj.magic coq_Monad_optErr) (Coq_pair (iN0, oUT0))
           | S x -> get_lv cfg nret nrev x iN0 oUT0))
      | None ->
        Error (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
          Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
          (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
          Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
          Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
          (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
          Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
          Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
          Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
          Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
          ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_false,
          Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
          Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
          Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
          Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
          ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
          Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
          Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
          Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
          Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
          ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
          Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true,
          Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
          Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
          Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
          ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
          Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true,
          Coq_true, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
          Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
          Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
          ((Ascii (Coq_true, Coq_true, Coq_true, Coq_false, Coq_false,
          Coq_true, Coq_true, Coq_false)),
          EmptyString))))))))))))))))))))))))))))))))))))))))

(** val get_lv_mfp :
    code -> nat -> node -> node -> nat -> coq_CD -> coq_CD -> coq_CD optErr **)

let rec get_lv_mfp cfg maxnode nret nrev iteration iN oUT =
  match iteration with
  | O -> ret (Obj.magic coq_Monad_optErr) oUT
  | S x ->
    (match get_lv cfg nret nrev maxnode iN oUT with
     | Success p ->
       let Coq_pair (nIN, nOUT) = p in
       (match PTree.beq set_eq iN nIN with
        | Coq_true -> ret (Obj.magic coq_Monad_optErr) oUT
        | Coq_false -> get_lv_mfp cfg maxnode nret nrev x nIN nOUT)
     | Error msg -> Error msg)

(** val compute_livevar :
    code -> nat -> node -> node -> nat -> coq_CD optErr **)

let compute_livevar cfg maxnode nret nrev precision =
  let iN = initialize maxnode PTree.empty in
  let oUT = initialize maxnode PTree.empty in
  get_lv_mfp cfg maxnode nret nrev precision iN oUT

type tvs = set

type clashg = tvs PTree.t

(** val create_clash_graph : coq_CD -> clashg optErr **)

let rec create_clash_graph lv =
  ret (Obj.magic coq_Monad_optErr)
    (PTree.fold1 (fun acc e ->
      set_fold_left (fun acc' e' ->
        PTree.set e'
          (set_union
            (app (PTree.xkeys (PTree.get_default set_empty e' acc') Coq_xH)
              (PTree.xkeys e Coq_xH)) (PTree.get_default set_empty e' acc') e
            PTree.empty) acc') e acc) lv PTree.empty)

(** val remove_node_graph : ident -> clashg -> clashg **)

let rec remove_node_graph t0 g =
  let g' = PTree.remove t0 g in
  PTree.fold (fun acc i e ->
    PTree.set i
      (set_minus (PTree.xkeys e Coq_xH) e (set_add t0 set_empty) PTree.empty)
      acc) g' g'

(** val least_degree : clashg -> ident **)

let rec least_degree g =
  let Coq_pair (id, _) =
    PTree.fold (fun acc i e ->
      let Coq_pair (_, mn) = acc in
      let c = set_card e in
      (match Nat.leb c mn with
       | Coq_true -> Coq_pair (i, c)
       | Coq_false -> acc)) g (Coq_pair ((Pos.of_nat O), (S (S (S (S (S (S (S
      (S (S (S (S (S (S (S (S (S O))))))))))))))))))
  in
  id

(** val pop_graph : nat -> clashg -> ident list -> ident list optErr **)

let rec pop_graph i g stack =
  match i with
  | O ->
    (match Nat.eqb (length (PTree.elements g)) O with
     | Coq_true -> ret (Obj.magic coq_Monad_optErr) stack
     | Coq_false ->
       Error (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
         Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
         (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
         Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
         Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
         (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
         ((Ascii (Coq_true, Coq_true, Coq_true, Coq_false, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
         ((Ascii (Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
         Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
         Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
         Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true,
         Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
         Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
         Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
         ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
         Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
         Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
         Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
         ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
         Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
         Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_true,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
         Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
         ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_true, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
         ((Ascii (Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
         Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_false,
         Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
         Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
         Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_false,
         Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
         Coq_true, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
         Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
         Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
         Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
         EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
  | S n ->
    let ldn = least_degree g in
    pop_graph n (remove_node_graph ldn g) (Coq_cons (ldn, stack))

type colorMap = nat PTree.t

(** val get_assigned_colors : colorMap -> tvs -> set **)

let get_assigned_colors c s =
  PTree.fold (fun acc i e ->
    match e with
    | Coq_true ->
      (match PTree.get i c with
       | Some cl -> set_add (Pos.of_nat cl) acc
       | None -> acc)
    | Coq_false -> acc) s set_empty

(** val colorMapFull : set **)

let colorMapFull =
  set_add (Pos.of_nat (S O))
    (set_add (Pos.of_nat (S (S O)))
      (set_add (Pos.of_nat (S (S (S O))))
        (set_add (Pos.of_nat (S (S (S (S O)))))
          (set_add (Pos.of_nat (S (S (S (S (S O))))))
            (set_add (Pos.of_nat (S (S (S (S (S (S O)))))))
              (set_add (Pos.of_nat (S (S (S (S (S (S (S O))))))))
                (set_add (Pos.of_nat (S (S (S (S (S (S (S (S O)))))))))
                  (set_add (Pos.of_nat (S (S (S (S (S (S (S (S (S O))))))))))
                    (set_add
                      (Pos.of_nat (S (S (S (S (S (S (S (S (S (S O)))))))))))
                      (set_add
                        (Pos.of_nat (S (S (S (S (S (S (S (S (S (S (S
                          O))))))))))))
                        (set_add
                          (Pos.of_nat (S (S (S (S (S (S (S (S (S (S (S (S
                            O)))))))))))))
                          (set_add
                            (Pos.of_nat (S (S (S (S (S (S (S (S (S (S (S (S
                              (S O))))))))))))))
                            (set_add
                              (Pos.of_nat (S (S (S (S (S (S (S (S (S (S (S (S
                                (S (S O)))))))))))))))
                              (set_add
                                (Pos.of_nat (S (S (S (S (S (S (S (S (S (S (S
                                  (S (S (S (S O)))))))))))))))) set_empty))))))))))))))

(** val max_reg : nat **)

let max_reg =
  S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S O)))))))))))))))

(** val default_reg : nat **)

let default_reg =
  S O

(** val assign_color : ident list -> clashg -> colorMap -> colorMap optErr **)

let rec assign_color stack g c =
  match stack with
  | Coq_nil -> ret (Obj.magic coq_Monad_optErr) c
  | Coq_cons (x, xs) ->
    (match PTree.get x g with
     | Some lk ->
       let assignable =
         set_minus (PTree.xkeys colorMapFull Coq_xH) colorMapFull
           (get_assigned_colors c lk) PTree.empty
       in
       (match Nat.ltb O (set_card assignable) with
        | Coq_true ->
          (match PTree.fold (fun acc i e ->
                   match e with
                   | Coq_true -> Some i
                   | Coq_false -> acc) assignable None with
           | Some reg -> assign_color xs g (PTree.set x (Pos.to_nat reg) c)
           | None ->
             Error (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true,
               Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
               (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
               Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
               Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
               Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
               Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
               ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true,
               Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
               Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
               Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
               Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
               ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
               Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
               Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
               Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
               Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
               Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
               Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
               ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
               Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
               Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
               Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
               Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
               Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
               Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
               ((Ascii (Coq_true, Coq_true, Coq_true, Coq_false, Coq_false,
               Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
               Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
               Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
               Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
               ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
               Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
               Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
               Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_false,
               Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
               ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true,
               Coq_true, Coq_true, Coq_false)),
               EmptyString)))))))))))))))))))))))))))))))))))))))))))))
        | Coq_false ->
          Error (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_true,
            Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
            (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
            Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
            Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
            (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
            Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
            (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
            Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
            Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
            (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
            Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
            (Coq_true, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
            Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
            Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
            (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
            Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
            (Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
            Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
            Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
            (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false,
            Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
            (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
            Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
            Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
            (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false,
            Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
            (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
            Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
            Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
            (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
            Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
            (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
            Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
            Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
            (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true,
            Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
            (Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
            Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
            Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
            (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
            Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
            (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
            Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
            Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
            (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
            Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
            (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
            Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
            Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
            (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_true,
            Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
            (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
            Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
            Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
            Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
            Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
            ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
            Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
            Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
            Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
            Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
            ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
            Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_false,
            Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
            Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
            Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
            ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
            Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
            Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
            Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
            Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
            ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
            Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
            Coq_true, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
            Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
            Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
            ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_true,
            Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
            Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
            Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
            Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
            ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
            Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_false,
            Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
            Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
            Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
            ((Ascii (Coq_true, Coq_true, Coq_true, Coq_false, Coq_false,
            Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
            Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
            Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
            Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
            ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
            Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
            Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
            Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_false,
            Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
            ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true,
            Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
            Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
            Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
            Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
            ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
            Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_false,
            Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_false,
            Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
            Coq_false, Coq_true, Coq_true, Coq_false, Coq_false)), (String
            ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_true,
            Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true,
            Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_false,
            Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
            Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
            ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
            Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
            Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
            Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
            Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
            ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
            Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
            Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
            Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
            Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
            ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
            Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
            Coq_true, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
            Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
            Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
            ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_true,
            Coq_false, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
            Coq_true, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
            Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
            Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
            ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false,
            Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
            Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
            Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_false,
            Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
            EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
     | None ->
       Error (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false,
         Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
         (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
         Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
         Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
         (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
         Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
         (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
         Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
         Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
         (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false,
         Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
         (Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
         Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
         Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
         (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false,
         Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
         (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
         Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
         Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
         (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
         Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
         Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
         ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
         Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
         ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
         ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
         ((Ascii (Coq_true, Coq_true, Coq_true, Coq_false, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
         Coq_false)),
         EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

(** val recolor_expr : expr -> colorMap -> expr optErr **)

let rec recolor_expr e c =
  match e with
  | Etempvar (i, t0) ->
    ret (Obj.magic coq_Monad_optErr) (Etempvar
      ((Pos.of_nat (PTree.get_default default_reg i c)), t0))
  | Esload (e0, t0) ->
    bind (Obj.magic coq_Monad_optErr) (recolor_expr e0 c) (fun e' ->
      ret (Obj.magic coq_Monad_optErr) (Esload (e', t0)))
  | Emload (e0, t0) ->
    bind (Obj.magic coq_Monad_optErr) (recolor_expr e0 c) (fun e' ->
      ret (Obj.magic coq_Monad_optErr) (Emload (e', t0)))
  | Eaddr (e0, t0) ->
    bind (Obj.magic coq_Monad_optErr) (recolor_expr e0 c) (fun e' ->
      ret (Obj.magic coq_Monad_optErr) (Eaddr (e', t0)))
  | Eunop (o, e0, t0) ->
    bind (Obj.magic coq_Monad_optErr) (recolor_expr e0 c) (fun e' ->
      ret (Obj.magic coq_Monad_optErr) (Eunop (o, e', t0)))
  | Ebinop (o, e1, e2, t0) ->
    bind (Obj.magic coq_Monad_optErr) (recolor_expr e1 c) (fun e1' ->
      bind (Obj.magic coq_Monad_optErr) (recolor_expr e2 c) (fun e2' ->
        ret (Obj.magic coq_Monad_optErr) (Ebinop (o, e1', e2', t0))))
  | Ecall1 (b, e0, t0) ->
    bind (Obj.magic coq_Monad_optErr) (recolor_expr e0 c) (fun e' ->
      ret (Obj.magic coq_Monad_optErr) (Ecall1 (b, e', t0)))
  | _ -> ret (Obj.magic coq_Monad_optErr) e

(** val recolor_exprs : expr list -> colorMap -> expr list optErr **)

let rec recolor_exprs es c =
  match es with
  | Coq_nil -> ret (Obj.magic coq_Monad_optErr) Coq_nil
  | Coq_cons (e, rest) ->
    bind (Obj.magic coq_Monad_optErr) (Obj.magic recolor_expr e c) (fun e' ->
      bind (Obj.magic coq_Monad_optErr) (recolor_exprs rest c) (fun rest' ->
        ret (Obj.magic coq_Monad_optErr) (Coq_cons (e', rest'))))

(** val recolor_regs :
    ident list -> colorMap -> ident list -> ident list optErr **)

let rec recolor_regs l c r =
  match l with
  | Coq_nil -> ret (Obj.magic coq_Monad_optErr) r
  | Coq_cons (x, xs) ->
    recolor_regs xs c (Coq_cons
      ((Pos.of_nat (PTree.get_default default_reg x c)), r))

(** val recolor_reg : ident -> colorMap -> ident **)

let recolor_reg i c =
  Pos.of_nat (PTree.get_default default_reg i c)

(** val recolor :
    code -> set -> nat -> node -> colorMap -> (code -> code) -> (code ->
    code) optErr **)

let rec recolor cfg t0 rd tn c cont =
  match rd with
  | O ->
    Error (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true,
      Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii
      (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
      Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
      Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
      Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
      Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
      Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
      (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
      Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
      ((Ascii (Coq_true, Coq_true, Coq_false, Coq_true, Coq_false, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
      Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
      Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
      Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
      Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
      (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
      Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
      (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
      Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
      (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
      Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
      (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
      Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
      Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
      ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
      Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
      (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
      Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
      (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
      Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
      ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
      Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
      (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
      Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
      (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_false, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
      Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
      (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
      Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
      (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
      Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
      ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
      Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
      Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
      Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
      Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
      Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
      (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
      Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
      (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
      Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
      ((Ascii (Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
      Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
      (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_true, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
      Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
      ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
      Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
      Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
      Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
      ((Ascii (Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
      Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
      (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
      Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
      (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
      Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
      (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
      Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
      (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
      Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
      ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
      Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
      ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
      Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
      Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
      (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
      Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
      (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
      Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
      EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
  | S x ->
    (match PTree.get tn cfg with
     | Some s ->
       (match set_in tn t0 with
        | Coq_true -> ret (Obj.magic coq_Monad_optErr) cont
        | Coq_false ->
          let t' =
            set_union
              (app (PTree.xkeys t0 Coq_xH)
                (PTree.xkeys (set_add tn set_empty) Coq_xH)) t0
              (set_add tn set_empty) PTree.empty
          in
          let recolor' = recolor cfg t' x in
          (match s with
           | StmtCGraph.Sskip n -> recolor' n c cont
           | StmtCGraph.Smassign (lv, rv, n) ->
             bind (Obj.magic coq_Monad_optErr) (Obj.magic recolor_expr lv c)
               (fun lv' ->
               bind (Obj.magic coq_Monad_optErr)
                 (Obj.magic recolor_expr rv c) (fun rv' ->
                 recolor' n c (fun g ->
                   PTree.set tn (StmtCGraph.Smassign (lv', rv', n)) (cont g))))
           | StmtCGraph.Ssassign (lv, rv, n) ->
             bind (Obj.magic coq_Monad_optErr) (Obj.magic recolor_expr lv c)
               (fun lv' ->
               bind (Obj.magic coq_Monad_optErr)
                 (Obj.magic recolor_expr rv c) (fun rv' ->
                 recolor' n c (fun g ->
                   PTree.set tn (StmtCGraph.Ssassign (lv', rv', n)) (cont g))))
           | StmtCGraph.Sset (id, rv, n) ->
             bind (Obj.magic coq_Monad_optErr) (Obj.magic recolor_expr rv c)
               (fun rv' ->
               recolor' n c (fun g ->
                 PTree.set tn (StmtCGraph.Sset ((recolor_reg id c), rv', n))
                   (cont g)))
           | StmtCGraph.Scall (retval, lab, args, n) ->
             bind (Obj.magic coq_Monad_optErr)
               (Obj.magic recolor_exprs args c) (fun args' ->
               match retval with
               | Some rv ->
                 recolor' n c (fun g ->
                   PTree.set tn (StmtCGraph.Scall ((Some (recolor_reg rv c)),
                     lab, args', n)) (cont g))
               | None ->
                 recolor' n c (fun g ->
                   PTree.set tn (StmtCGraph.Scall (retval, lab, args', n))
                     (cont g)))
           | Scond (cond, ntrue, nfalse) ->
             bind (Obj.magic coq_Monad_optErr)
               (Obj.magic recolor_expr cond c) (fun cond' ->
               bind (Obj.magic coq_Monad_optErr)
                 (recolor' ntrue c (fun g -> g)) (fun cont' ->
                 recolor' nfalse c (fun g ->
                   PTree.set tn (Scond (cond', ntrue, nfalse))
                     (cont' (cont g)))))
           | StmtCGraph.Sreturn (val0, n) ->
             (match val0 with
              | Some v ->
                recolor' n c (fun g ->
                  PTree.set tn (StmtCGraph.Sreturn ((Some (recolor_reg v c)),
                    n)) (cont g))
              | None -> recolor' n c cont)
           | StmtCGraph.Shash (e1, e2, eo, n) ->
             bind (Obj.magic coq_Monad_optErr) (Obj.magic recolor_expr e1 c)
               (fun e1' ->
               bind (Obj.magic coq_Monad_optErr)
                 (Obj.magic recolor_expr e2 c) (fun e2' ->
                 match eo with
                 | Some eoo ->
                   bind (Obj.magic coq_Monad_optErr)
                     (Obj.magic recolor_expr eoo c) (fun eoo' ->
                     recolor' n c (fun g ->
                       PTree.set tn (StmtCGraph.Shash (e1', e2', (Some eoo'),
                         n)) (cont g)))
                 | None ->
                   recolor' n c (fun g ->
                     PTree.set tn (StmtCGraph.Shash (e1', e2', None, n))
                       (cont g))))
           | StmtCGraph.Stransfer (a, v, nfail, n) ->
             bind (Obj.magic coq_Monad_optErr) (Obj.magic recolor_expr a c)
               (fun a' ->
               bind (Obj.magic coq_Monad_optErr) (Obj.magic recolor_expr v c)
                 (fun v' ->
                 bind (Obj.magic coq_Monad_optErr)
                   (recolor' nfail c (fun g -> g)) (fun cont' ->
                   recolor' n c (fun g ->
                     PTree.set tn (StmtCGraph.Stransfer (a', v', nfail, n))
                       (cont' (cont g))))))
           | StmtCGraph.Scallmethod (a, rvs, sig0, v, args, nfail, n) ->
             bind (Obj.magic coq_Monad_optErr)
               (Obj.magic recolor_regs rvs c Coq_nil) (fun rvs' ->
               bind (Obj.magic coq_Monad_optErr) (Obj.magic recolor_expr a c)
                 (fun a' ->
                 bind (Obj.magic coq_Monad_optErr)
                   (Obj.magic recolor_expr v c) (fun v' ->
                   bind (Obj.magic coq_Monad_optErr)
                     (Obj.magic recolor_exprs args c) (fun args' ->
                     bind (Obj.magic coq_Monad_optErr)
                       (recolor' nfail c (fun g -> g)) (fun cont' ->
                       recolor' n c (fun g ->
                         PTree.set tn (StmtCGraph.Scallmethod (a', rvs',
                           sig0, v', args', nfail, n)) (cont' (cont g))))))))
           | StmtCGraph.Slog (topics, args, n) ->
             bind (Obj.magic coq_Monad_optErr)
               (Obj.magic recolor_exprs topics c) (fun topics' ->
               bind (Obj.magic coq_Monad_optErr)
                 (Obj.magic recolor_exprs args c) (fun args' ->
                 recolor' n c (fun g ->
                   PTree.set tn (StmtCGraph.Slog (topics', args', n)) (cont g))))
           | _ -> ret (Obj.magic coq_Monad_optErr) cont))
     | None ->
       Error (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true,
         Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
         (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
         Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
         Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
         (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
         Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
         (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
         Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
         Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
         (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
         ((Ascii (Coq_false, Coq_true, Coq_true, Coq_false, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
         Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
         Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
         ((Ascii (Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
         Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
         Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_false,
         Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
         Coq_true, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
         Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
         Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_false,
         Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
         Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
         Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true,
         Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
         Coq_false, Coq_false, Coq_false, Coq_true, Coq_false)), (String
         ((Ascii (Coq_true, Coq_true, Coq_true, Coq_false, Coq_false,
         Coq_false, Coq_true, Coq_false)),
         EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

(** val color_graph :
    code -> nat -> node -> clashg -> (code, colorMap) prod optErr **)

let color_graph cfg maxnode nentry cg =
  match PTree.fold (fun acc _ e ->
          match acc with
          | Coq_true -> Nat.ltb (set_card e) max_reg
          | Coq_false -> Coq_false) cg Coq_true with
  | Coq_true ->
    let num_tv = length (PTree.elements cg) in
    bind (Obj.magic coq_Monad_optErr)
      (Obj.magic pop_graph (length (PTree.elements cg)) cg Coq_nil)
      (fun stack ->
      let unique_nodes =
        Nat.eqb
          (set_card (fold_left (fun acc e -> set_add e acc) stack set_empty))
          num_tv
      in
      (match unique_nodes with
       | Coq_true ->
         bind (Obj.magic coq_Monad_optErr)
           (Obj.magic assign_color stack cg PTree.empty) (fun colors ->
           match Nat.eqb (length (PTree.elements colors)) num_tv with
           | Coq_true ->
             bind (Obj.magic coq_Monad_optErr)
               (Obj.magic recolor cfg set_empty maxnode nentry colors
                 (fun g -> g)) (fun f ->
               ret (Obj.magic coq_Monad_optErr) (Coq_pair ((f cfg), colors)))
           | Coq_false ->
             Error (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
               Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
               (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
               Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
               Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
               Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
               Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
               ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
               Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
               Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
               Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
               Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
               ((Ascii (Coq_true, Coq_true, Coq_true, Coq_false, Coq_false,
               Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
               Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
               Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
               Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
               Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
               Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
               ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
               Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
               Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
               Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
               Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
               Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
               Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
               ((Ascii (Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
               Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
               Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
               Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
               Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
               (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false,
               Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
               (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
               Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
               Coq_true, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
               Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
               Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
               ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
               Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
               Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
               Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
               Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
               ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
               Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
               Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
               Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
               Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
               (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true,
               Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
               (Coq_true, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
               Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
               Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
               Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_false,
               Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
               ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
               Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
               Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
               Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
               Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
               (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true,
               Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
               (Coq_false, Coq_true, Coq_true, Coq_false, Coq_false,
               Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
               Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
               Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
               Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
               Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
               Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
               ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
               Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
               Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
               Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
               Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
               ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
               Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
               Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
               Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
               Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
               ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
               Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
               (Coq_false, Coq_true, Coq_true, Coq_false, Coq_false,
               Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
               Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
               Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
               Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
               ((Ascii (Coq_true, Coq_false, Coq_true, Coq_true, Coq_false,
               Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
               Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
               Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
               Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
               Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
               Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
               ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
               Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
               Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
               Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
               Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
               ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
               Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
               (Coq_true, Coq_true, Coq_true, Coq_false, Coq_false, Coq_true,
               Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
               Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
               Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
               Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
               ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
               Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
               Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
               Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
               Coq_true, Coq_false, Coq_true, Coq_false, Coq_false)), (String
               ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
               Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
               (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
               Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
               Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
               Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
               Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
               ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
               Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
               (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
               Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
               Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
               Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
               Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
               ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
               Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
               (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
               Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
               Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
               Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
               Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
               ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
               Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
               Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
               Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
               Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
               Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
               Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
               ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
               Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
               Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
               Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
               Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
               (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false,
               Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
               (Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
               Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
               Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
               Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
               Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
               (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
               Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
               (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
               Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
               Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
               Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
               Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
               ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
               Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
               Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
               Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
               Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
               EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
       | Coq_false ->
         Error (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
           Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
           Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
           (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
           Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_false,
           Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
           Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
           Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
           (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_true,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_true, Coq_false, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false,
           Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
           Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
           Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
           Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
           Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
           Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
           Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false,
           Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
           (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
           (Coq_true, Coq_true, Coq_true, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
           Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
  | Coq_false ->
    Error (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_true,
      Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
      Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
      Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_false,
      Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
      Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
      (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
      Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
      (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_true, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
      Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
      (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
      Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
      Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
      ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
      Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
      (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
      Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
      (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
      Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
      ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
      Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
      Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
      Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
      ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
      Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
      Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
      (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
      Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
      ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
      Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
      Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
      (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
      Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
      ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
      Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
      (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
      Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
      (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_true, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
      Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
      (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
      Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_false,
      Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
      Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
      Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
      Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
      Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
      Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
      (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
      Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
      ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
      Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
      Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
      (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
      Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
      (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_false, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
      Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
      (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
      Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
      ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_false,
      Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
      Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
      (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false,
      Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_false,
      Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
      Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
      Coq_true, Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
      (Coq_true, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
      Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
      Coq_false, Coq_true, Coq_true, Coq_false, Coq_false)), (String ((Ascii
      (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
      Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
      Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
      (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
      Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
      ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
      Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
      Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
      (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
      Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
      ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_false,
      Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_true, Coq_true, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
      Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_false,
      Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
      Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
      Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
      Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
      Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
      EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

(** val variable_coalescing :
    code -> nat -> node -> node -> node -> nat -> (code, colorMap) prod optErr **)

let variable_coalescing cfg maxnode nentry nret nrev precision =
  bind (Obj.magic coq_Monad_optErr)
    (Obj.magic compute_livevar cfg maxnode nret nrev precision) (fun lvs ->
    bind (Obj.magic coq_Monad_optErr) (Obj.magic create_clash_graph lvs)
      (fun cg -> color_graph cfg maxnode nentry cg))

(** val get_clash_graph :
    code -> nat -> node -> node -> nat -> (positive, positive list) prod list
    optErr **)

let get_clash_graph cfg maxnode nret nrev precision =
  bind (Obj.magic coq_Monad_optErr)
    (Obj.magic compute_livevar cfg maxnode nret nrev precision) (fun lvs ->
    bind (Obj.magic coq_Monad_optErr) (Obj.magic create_clash_graph lvs)
      (fun cg ->
      ret (Obj.magic coq_Monad_optErr)
        (map (fun x ->
          let Coq_pair (id, tvss) = x in
          Coq_pair (id,
          (PTree.fold (fun acc i ind ->
            match ind with
            | Coq_true -> Coq_cons (i, acc)
            | Coq_false -> acc) tvss Coq_nil))) (PTree.elements cg))))

(** val max_iteration : nat **)

let max_iteration =
  S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S
    O))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

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
     (match variable_coalescing s.st_code
              (sub (Pos.to_nat s.st_nextnode) (S O)) nentry
              (Pos.of_nat (S O)) (Pos.of_nat (S (S O))) max_iteration with
      | Success p ->
        let Coq_pair (ncfg, cmap) = p in
        ret (Obj.magic coq_Monad_optErr) { StmtCGraph.fn_return =
          f.fn_return; StmtCGraph.fn_params =
          (map (fun e ->
            let Coq_pair (id, ty) = e in
            Coq_pair ((Pos.of_nat (PTree.get_default (S O) id cmap)), ty))
            f.fn_params); StmtCGraph.fn_temps =
          (map (fun e ->
            let Coq_pair (id, ty) = e in
            Coq_pair ((Pos.of_nat (PTree.get_default (S O) id cmap)), ty))
            f.fn_temps); StmtCGraph.fn_locals = f.fn_locals; fn_code = ncfg;
          fn_entrypoint = nentry }
      | Error msg -> Error msg))

(** val clash_function :
    coq_function -> (positive, positive list) prod list optErr **)

let clash_function f =
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
   | OK (_, s) ->
     get_clash_graph s.st_code (sub (Pos.to_nat s.st_nextnode) (S O))
       (Pos.of_nat (S O)) (Pos.of_nat (S (S O))) max_iteration)

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

(** val graphviz_helper :
    StmtCGraph.coq_function -> ((positive, StmtCGraph.statement) prod list,
    positive) prod **)

let graphviz_helper f =
  Coq_pair ((PTree.elements f.fn_code), f.fn_entrypoint)

(** val clash_viz :
    genv -> (positive, positive list) prod list list optErr **)

let clash_viz ge =
  fold_left (fun acc x ->
    match acc with
    | Success acc' ->
      bind (Obj.magic coq_Monad_optErr) (Obj.magic clash_function x)
        (fun x' -> ret (Obj.magic coq_Monad_optErr) (Coq_cons (x', acc')))
    | Error msg -> Error msg) (Genv.all_functions ge) (Success Coq_nil)

(** val cgraph_viz :
    genv -> ((positive, StmtCGraph.statement) prod list, positive) prod list
    optErr **)

let cgraph_viz ge =
  fold_left (fun acc x ->
    match acc with
    | Success acc' ->
      bind (Obj.magic coq_Monad_optErr) (Obj.magic cgraph_function x)
        (fun x' ->
        ret (Obj.magic coq_Monad_optErr) (Coq_cons ((graphviz_helper x'),
          acc')))
    | Error msg -> Error msg) (Genv.all_functions ge) (Success Coq_nil)

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
