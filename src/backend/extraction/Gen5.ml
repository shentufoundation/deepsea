open Ascii
open BinNums
open Cop
open Ctypes
open Datatypes
open ExpCintptr
open ExpMiniC
open Globalenvs
open Maps0
open Monad
open OptErrMonad
open StackEnv
open StmtCintptr
open StmtClocal
open String0
open Trees

(** val spE : ExpCintptr.expr **)

let spE =
  Emload ((ExpCintptr.Econst_int256 (sp, (Tpointer (Coq_mem, (Tpointer
    (Coq_mem, Tvoid)))))), (Tpointer (Coq_mem, Tvoid)))

(** val offsetE : coq_function -> ExpCintptr.expr **)

let offsetE f =
  ExpCintptr.Econst_int256 ((frame_size f.fn_locals), (Tint (I256, Unsigned)))

(** val pushS : coq_function -> StmtCintptr.statement **)

let pushS f =
  StmtCintptr.Smassign ((ExpCintptr.Econst_int256 (sp, (Tpointer (Coq_mem,
    (Tpointer (Coq_mem, Tvoid)))))), (ExpCintptr.Ebinop (Oadd, spE,
    (offsetE f), (Tpointer (Coq_mem, Tvoid)))))

(** val popS : coq_function -> StmtCintptr.statement **)

let popS f =
  StmtCintptr.Smassign ((ExpCintptr.Econst_int256 (sp, (Tpointer (Coq_mem,
    (Tpointer (Coq_mem, Tvoid)))))), (ExpCintptr.Ebinop (Osub, spE,
    (offsetE f), (Tpointer (Coq_mem, Tvoid)))))

(** val cintptr_expr : coq_function -> expr -> ExpCintptr.expr optErr **)

let rec cintptr_expr f = function
| Econst_int (_, _) ->
  Error (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false,
    Coq_false, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
    Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
    ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
    Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
    Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
    (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false,
    Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
    Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
    Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
    Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
    Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
    (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_true, Coq_false,
    Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
    Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
    ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
    Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
    Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
    (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
    Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true,
    Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false,
    Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false,
    Coq_true, Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true,
    Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_false,
    Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false,
    Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
    Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false,
    Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
    Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
    Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true,
    Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
    Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
    (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
    Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
    Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
    (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false,
    Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
    Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
    ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
    Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
    Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
    (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false,
    Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
    Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
    (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
    Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
    Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
    EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
| Econst_int256 (i, t0) ->
  ret (Obj.magic coq_Monad_optErr) (ExpCintptr.Econst_int256 (i, t0))
| Evar (id, t0) ->
  let locals = f.fn_locals in
  let fl = mkfieldlist locals in
  (match offset fl id with
   | Some off ->
     let foE = ExpCintptr.Econst_int256 ((frame_size locals), (Tint (I256,
       Unsigned)))
     in
     let baseE = ExpCintptr.Ebinop (Osub, spE, foE, (Tpointer (Coq_mem,
       (Tstruct (Coq_xH, fl)))))
     in
     let offE = ExpCintptr.Econst_int256 (off, (Tint (I256, Unsigned))) in
     ret (Obj.magic coq_Monad_optErr) (ExpCintptr.Ebinop (Oadd, baseE, offE,
       t0))
   | None ->
     Error (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
       Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii
       (Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
       Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
       Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
       (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
       Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
       Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
       Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
       Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
       (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
       Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
       Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
       (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false,
       Coq_false, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
       Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
       Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true,
       Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
       (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
       Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
       Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
       (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
       Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
       Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
       (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
       Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
       Coq_true, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false)),
       (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
       Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
       Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
       (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
       Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_false,
       Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
       Coq_false)), EmptyString)))))))))))))))))))))))))))))))))))))))))))
| Eglob (_, _) ->
  Error (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
    Coq_false, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
    Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
    ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
    Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
    Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
    (Coq_false, Coq_true, Coq_false, Coq_false, Coq_false, Coq_true,
    Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
    Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
    (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_true,
    Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
    Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
    (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_true,
    Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
    Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
    ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
    Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
    Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
    (Coq_false, Coq_true, Coq_false, Coq_false, Coq_false, Coq_true,
    Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
    Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
    (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
    Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
    Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
    (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_true,
    Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
    Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true,
    Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
    Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
    Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
    Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
    Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true,
    Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_false,
    Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
    Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true,
    Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
    Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
    Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_true,
    Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
    Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_false,
    Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
    Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
    Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
    Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
    Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true,
    Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
    Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
    Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_false,
    Coq_false)),
    EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
| Etempvar (i, t0) ->
  ret (Obj.magic coq_Monad_optErr) (ExpCintptr.Etempvar (i, t0))
| Ederef (e0, t0) ->
  bind (Obj.magic coq_Monad_optErr) (cintptr_expr f e0) (fun e' ->
    match ExpCintptr.typeof e' with
    | Tpointer (p, _) ->
      (match p with
       | Coq_mem -> ret (Obj.magic coq_Monad_optErr) (Emload (e', t0))
       | Coq_stor -> ret (Obj.magic coq_Monad_optErr) (Esload (e', t0))
       | Coq_call ->
         Error (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
           Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false,
           Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
           Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
           Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
           Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
           Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
           Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
           Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
           Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false,
           Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true,
           Coq_false, Coq_true, Coq_false, Coq_false)),
           EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))
    | _ ->
      Error (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii
        (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
        Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
        Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
        (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
        Coq_false, Coq_true, Coq_true, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
        Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
        Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
        ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
        ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
        Coq_false, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
        Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
        ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
        Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
        Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
        ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
        Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true,
        Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
        Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
        Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false)),
        (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
        Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
        Coq_true, Coq_true, Coq_false, Coq_true, Coq_false, Coq_false)),
        (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_false, Coq_false,
        Coq_false, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
        Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
        Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
        EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
| Eaddr (e0, t0) ->
  bind (Obj.magic coq_Monad_optErr) (cintptr_expr f e0) (fun e' ->
    ret (Obj.magic coq_Monad_optErr) (ExpCintptr.Eaddr (e', t0)))
| Eunop (o, e0, t0) ->
  (match o with
   | Osha_1 ->
     Error (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
       Coq_true, Coq_false, Coq_true, Coq_false)), (String ((Ascii
       (Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
       Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
       Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
       (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_true,
       Coq_false, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
       Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false,
       Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
       Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
       ((Ascii (Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
       Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
       Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
       (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true,
       Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
       Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
       Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
       Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
       (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
       Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
       Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
       (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true,
       Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
       Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
       Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true,
       Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
       Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
       Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
       Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
       (Coq_false, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
       Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
       Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
       (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false, Coq_false,
       Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
       Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
       (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
       Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
       (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false, Coq_true,
       Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
       Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
       (Coq_true, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
       Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
       Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
       (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
       Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
       Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
       Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
       Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
       (Coq_false, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
       Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
       Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
       (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false, Coq_false,
       Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
       Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
       (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_false, Coq_false,
       Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
       Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
       (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
       Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
       Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
       (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
       Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
       (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
       Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
       Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
       (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
       Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
       Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
       Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
       Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
       (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
       Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
       Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
       (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
       Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
       Coq_true, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false)),
       (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
       Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
       Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
       (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
       Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_false,
       Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
       Coq_false)),
       EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
   | _ ->
     bind (Obj.magic coq_Monad_optErr) (cintptr_expr f e0) (fun e' ->
       ret (Obj.magic coq_Monad_optErr) (ExpCintptr.Eunop (o, e', t0))))
| Ebinop (o, e1, e2, t0) ->
  (match o with
   | Osha_2 ->
     Error (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
       Coq_true, Coq_false, Coq_true, Coq_false)), (String ((Ascii
       (Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
       Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
       Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
       (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_true,
       Coq_false, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
       Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false,
       Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
       Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
       ((Ascii (Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
       Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
       Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
       (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true,
       Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
       Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
       Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
       Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
       (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
       Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
       Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
       (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true,
       Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
       Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
       Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true,
       Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
       Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
       Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
       Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
       (Coq_false, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
       Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
       Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
       (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false, Coq_false,
       Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
       Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
       (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
       Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
       (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false, Coq_true,
       Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
       Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
       (Coq_true, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
       Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
       Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
       (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
       Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
       Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
       Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
       Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
       (Coq_false, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
       Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
       Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
       (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false, Coq_false,
       Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
       Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
       (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_false, Coq_false,
       Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
       Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
       (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
       Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
       Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
       (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
       Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
       (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
       Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
       Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
       (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
       Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
       Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
       Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
       Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
       (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
       Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
       Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
       (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
       Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
       Coq_true, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false)),
       (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
       Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
       Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
       (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
       Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_false,
       Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
       Coq_false)),
       EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
   | _ ->
     bind (Obj.magic coq_Monad_optErr) (cintptr_expr f e1) (fun e1' ->
       bind (Obj.magic coq_Monad_optErr) (cintptr_expr f e2) (fun e2' ->
         ret (Obj.magic coq_Monad_optErr) (ExpCintptr.Ebinop (o, e1', e2',
           t0)))))
| Efield (e0, id, t0) ->
  bind (Obj.magic coq_Monad_optErr) (cintptr_expr f e0) (fun e' ->
    match ExpCintptr.typeof e' with
    | Tpointer (p, t1) ->
      (match p with
       | Coq_mem ->
         (match t1 with
          | Tstruct (_, fl) ->
            (match offset fl id with
             | Some off ->
               let offE = ExpCintptr.Econst_int256 (off, (Tint (I256,
                 Unsigned)))
               in
               ret (Obj.magic coq_Monad_optErr) (ExpCintptr.Ebinop (Oadd, e',
                 offE, t0))
             | None ->
               Error (String ((Ascii (Coq_false, Coq_true, Coq_false,
                 Coq_false, Coq_false, Coq_false, Coq_true, Coq_false)),
                 (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false,
                 Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
                 (Coq_false, Coq_false, Coq_true, Coq_false, Coq_false,
                 Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
                 Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
                 Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
                 Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
                 Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
                 Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
                 (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_false,
                 Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
                 (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true,
                 Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
                 Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
                 Coq_true, Coq_false)), (String ((Ascii (Coq_false,
                 Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
                 Coq_true, Coq_false)), (String ((Ascii (Coq_false,
                 Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
                 Coq_false, Coq_false)), (String ((Ascii (Coq_true,
                 Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
                 Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
                 Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
                 Coq_false)), (String ((Ascii (Coq_false, Coq_false,
                 Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
                 Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
                 Coq_false, Coq_false, Coq_false, Coq_true, Coq_false)),
                 (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_false,
                 Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
                 (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
                 Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
                 Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
                 Coq_true, Coq_false)), (String ((Ascii (Coq_false,
                 Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
                 Coq_true, Coq_false)), (String ((Ascii (Coq_false,
                 Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
                 Coq_true, Coq_false)), (String ((Ascii (Coq_false,
                 Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
                 Coq_false, Coq_false)), (String ((Ascii (Coq_true,
                 Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
                 Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
                 Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
                 Coq_false)), (String ((Ascii (Coq_false, Coq_false,
                 Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
                 Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
                 Coq_false, Coq_false, Coq_false, Coq_true, Coq_false)),
                 (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true,
                 Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
                 (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
                 Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
                 Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
                 Coq_true, Coq_false)), (String ((Ascii (Coq_false,
                 Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
                 Coq_true, Coq_false)), (String ((Ascii (Coq_false,
                 Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
                 Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
                 Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
                 Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
                 Coq_true, Coq_false, Coq_true, Coq_false, Coq_false)),
                 (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_false,
                 Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii
                 (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
                 Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
                 Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
                 Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
                 Coq_true, Coq_false, Coq_true, Coq_false, Coq_false)),
                 (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_false,
                 Coq_true, Coq_true, Coq_true, Coq_false)),
                 EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
          | _ ->
            Error (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii
              (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_false,
              Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
              Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true,
              Coq_false, Coq_true, Coq_false, Coq_false, Coq_false, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
              Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
              Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
              Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true,
              Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
              Coq_true, Coq_false, Coq_true, Coq_false, Coq_false)), (String
              ((Ascii (Coq_true, Coq_true, Coq_true, Coq_false, Coq_false,
              Coq_false, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
              Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_false,
              Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
              Coq_false)),
              EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
       | _ ->
         Error (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
           Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
           Coq_true, Coq_true, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
           Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
           Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
           Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_true, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
           Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false,
           Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false,
           Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
           Coq_true, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
           Coq_true, Coq_true, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_false,
           Coq_true, Coq_true, Coq_true, Coq_false)),
           EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
    | _ ->
      Error (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii
        (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
        Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
        Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
        (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
        Coq_false, Coq_true, Coq_true, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
        Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
        Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true,
        Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
        (Coq_true, Coq_true, Coq_false, Coq_true, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
        Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
        ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
        ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
        Coq_false, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_true, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
        Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false,
        Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
        Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
        Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
        Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
        Coq_false, Coq_false, Coq_false, Coq_true, Coq_false)), (String
        ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
        Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
        Coq_true, Coq_true, Coq_false, Coq_true, Coq_false, Coq_false)),
        (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_false, Coq_false,
        Coq_false, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
        Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
        Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
        EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
| Eindex (e1, e2, t0) ->
  bind (Obj.magic coq_Monad_optErr) (cintptr_expr f e1) (fun e1' ->
    bind (Obj.magic coq_Monad_optErr) (cintptr_expr f e2) (fun e2' ->
      match ExpCintptr.typeof e1' with
      | Tpointer (p, t1) ->
        (match p with
         | Coq_mem ->
           (match t1 with
            | Tarray (t', _) ->
              let sizeE = ExpCintptr.Econst_int256 ((sizeof t'), (Tint (I256,
                Unsigned)))
              in
              let offE = ExpCintptr.Ebinop (Omul, e2', sizeE, (Tint (I256,
                Unsigned)))
              in
              ret (Obj.magic coq_Monad_optErr) (ExpCintptr.Ebinop (Oadd, e1',
                offE, t0))
            | _ ->
              Error (String ((Ascii (Coq_false, Coq_true, Coq_false,
                Coq_false, Coq_false, Coq_false, Coq_true, Coq_false)),
                (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false,
                Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
                (Coq_false, Coq_false, Coq_true, Coq_false, Coq_false,
                Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
                Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
                Coq_false, Coq_false)), (String ((Ascii (Coq_false,
                Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
                Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
                Coq_true, Coq_true, Coq_true, Coq_true, Coq_false)), (String
                ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
                Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
                (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
                Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
                Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_false,
                Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
                Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
                ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
                Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
                Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
                Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
                Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
                (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
                Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
                (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
                Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
                Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
                Coq_false)), (String ((Ascii (Coq_false, Coq_false,
                Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
                Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
                Coq_false, Coq_false, Coq_false, Coq_true, Coq_false)),
                (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true,
                Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
                (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
                Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
                Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
                Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
                Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
                Coq_false)), (String ((Ascii (Coq_false, Coq_false,
                Coq_false, Coq_true, Coq_true, Coq_true, Coq_true,
                Coq_false)), (String ((Ascii (Coq_false, Coq_false,
                Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
                Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
                Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
                ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
                Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
                Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
                Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
                Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
                Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
                Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
                ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
                Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
                Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
                Coq_false)), (String ((Ascii (Coq_false, Coq_false,
                Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
                Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
                Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
                ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
                Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
                Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_false,
                Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
                Coq_false, Coq_false, Coq_false, Coq_true, Coq_false)),
                (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
                Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
                (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
                Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
                Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_false,
                Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
                Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
                EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
         | _ ->
           Error (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false,
             Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii
             (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
             Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
             Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
             (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
             Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
             (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
             Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
             Coq_false, Coq_true, Coq_true, Coq_true, Coq_true, Coq_false)),
             (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
             Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
             (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
             Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
             Coq_true, Coq_true, Coq_false, Coq_true, Coq_false, Coq_false)),
             (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_true,
             Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
             (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
             Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
             Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
             (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false,
             Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
             (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
             Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true,
             Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
             Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
             Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
             ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
             Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true,
             Coq_false, Coq_true, Coq_false, Coq_false, Coq_false, Coq_true,
             Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
             Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
             ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
             Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
             Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
             Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
             Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
             ((Ascii (Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
             Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
             Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
             Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
             Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
             ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
             Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
             Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
             Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
             Coq_false, Coq_false, Coq_false, Coq_true, Coq_false)), (String
             ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
             Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
             Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
             Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
             Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
             ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
             Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
             Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
             Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_false,
             Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
             ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
             Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true,
             Coq_true, Coq_true, Coq_false, Coq_false, Coq_false, Coq_true,
             Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
             Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
             ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
             Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
             Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_false,
             Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
             Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
             EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
      | _ ->
        Error (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false,
          Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii
          (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
          Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
          Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
          (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
          Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
          (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
          Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
          Coq_false, Coq_true, Coq_true, Coq_true, Coq_true, Coq_false)),
          (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
          Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
          (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
          Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
          Coq_true, Coq_true, Coq_false, Coq_true, Coq_false, Coq_false)),
          (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_true,
          Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
          (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
          Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
          Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
          (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false,
          Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
          (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
          Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
          Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
          (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true,
          Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
          (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
          Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
          Coq_true, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false)),
          (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true,
          Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
          (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
          Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
          Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
          (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
          Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
          (Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
          Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
          Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
          (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true,
          Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
          (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
          Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
          Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
          (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
          Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii
          (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
          Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
          Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
          (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false,
          Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
          (Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
          Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
          Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
          (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false,
          Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
          (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
          Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
          Coq_true, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false)),
          (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
          Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
          (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
          Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
          Coq_true, Coq_true, Coq_false, Coq_true, Coq_false, Coq_false)),
          (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_false,
          Coq_true, Coq_true, Coq_true, Coq_false)),
          EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
| Ecall0 (b, t0) ->
  ret (Obj.magic coq_Monad_optErr) (ExpCintptr.Ecall0 (b, t0))
| Ecall1 (b, e0, t0) ->
  bind (Obj.magic coq_Monad_optErr) (cintptr_expr f e0) (fun e' ->
    ret (Obj.magic coq_Monad_optErr) (ExpCintptr.Ecall1 (b, e', t0)))

(** val cintptr_exprs :
    coq_function -> expr list -> ExpCintptr.expr list optErr **)

let rec cintptr_exprs f = function
| Coq_nil -> ret (Obj.magic coq_Monad_optErr) Coq_nil
| Coq_cons (e, es0) ->
  bind (Obj.magic coq_Monad_optErr) (cintptr_exprs f es0) (fun es' ->
    bind (Obj.magic coq_Monad_optErr) (Obj.magic cintptr_expr f e) (fun e' ->
      ret (Obj.magic coq_Monad_optErr) (Coq_cons (e', es'))))

(** val cintptr_expr_opt :
    coq_function -> expr option -> ExpCintptr.expr option optErr **)

let cintptr_expr_opt f = function
| Some e ->
  bind (Obj.magic coq_Monad_optErr) (Obj.magic cintptr_expr f e) (fun e' ->
    ret (Obj.magic coq_Monad_optErr) (Some e'))
| None -> ret (Obj.magic coq_Monad_optErr) None

(** val cintptr_stmt :
    coq_function -> statement -> StmtCintptr.statement optErr **)

let rec cintptr_stmt f = function
| Sskip -> ret (Obj.magic coq_Monad_optErr) StmtCintptr.Sskip
| Smassign (e1, e2) ->
  bind (Obj.magic coq_Monad_optErr) (Obj.magic cintptr_expr f e1) (fun e1' ->
    bind (Obj.magic coq_Monad_optErr) (Obj.magic cintptr_expr f e2)
      (fun e2' ->
      ret (Obj.magic coq_Monad_optErr) (StmtCintptr.Smassign (e1', e2'))))
| Ssassign (e1, e2) ->
  bind (Obj.magic coq_Monad_optErr) (Obj.magic cintptr_expr f e1) (fun e1' ->
    bind (Obj.magic coq_Monad_optErr) (Obj.magic cintptr_expr f e2)
      (fun e2' ->
      ret (Obj.magic coq_Monad_optErr) (StmtCintptr.Ssassign (e1', e2'))))
| Sset (i, e) ->
  bind (Obj.magic coq_Monad_optErr) (Obj.magic cintptr_expr f e) (fun e' ->
    ret (Obj.magic coq_Monad_optErr) (StmtCintptr.Sset (i, e')))
| Scall (rv, dest, args) ->
  bind (Obj.magic coq_Monad_optErr) (Obj.magic cintptr_exprs f args)
    (fun args' ->
    ret (Obj.magic coq_Monad_optErr) (StmtCintptr.Scall (rv, dest, args')))
| Ssequence (s1, s2) ->
  bind (Obj.magic coq_Monad_optErr) (cintptr_stmt f s1) (fun s1' ->
    bind (Obj.magic coq_Monad_optErr) (cintptr_stmt f s2) (fun s2' ->
      ret (Obj.magic coq_Monad_optErr) (StmtCintptr.Ssequence (s1', s2'))))
| Sifthenelse (e, s1, s2) ->
  bind (Obj.magic coq_Monad_optErr) (Obj.magic cintptr_expr f e) (fun e' ->
    bind (Obj.magic coq_Monad_optErr) (cintptr_stmt f s1) (fun s1' ->
      bind (Obj.magic coq_Monad_optErr) (cintptr_stmt f s2) (fun s2' ->
        ret (Obj.magic coq_Monad_optErr) (StmtCintptr.Sifthenelse (e', s1',
          s2')))))
| Sloop s0 ->
  bind (Obj.magic coq_Monad_optErr) (cintptr_stmt f s0) (fun s' ->
    ret (Obj.magic coq_Monad_optErr) (StmtCintptr.Sloop s'))
| Sbreak -> ret (Obj.magic coq_Monad_optErr) StmtCintptr.Sbreak
| Sreturn idopt ->
  ret (Obj.magic coq_Monad_optErr) (StmtCintptr.Ssequence ((popS f),
    (StmtCintptr.Sreturn idopt)))
| Shash (e1, e2, eopt) ->
  bind (Obj.magic coq_Monad_optErr) (Obj.magic cintptr_expr f e1) (fun e1' ->
    bind (Obj.magic coq_Monad_optErr) (Obj.magic cintptr_expr f e2)
      (fun e2' ->
      bind (Obj.magic coq_Monad_optErr) (Obj.magic cintptr_expr_opt f eopt)
        (fun eopt0 ->
        ret (Obj.magic coq_Monad_optErr) (StmtCintptr.Shash (e1', e2', eopt0)))))
| Stransfer (a, v) ->
  bind (Obj.magic coq_Monad_optErr) (Obj.magic cintptr_expr f a) (fun a' ->
    bind (Obj.magic coq_Monad_optErr) (Obj.magic cintptr_expr f v) (fun v' ->
      ret (Obj.magic coq_Monad_optErr) (StmtCintptr.Stransfer (a', v'))))
| Scallmethod (a, rvs, sg, v, args) ->
  bind (Obj.magic coq_Monad_optErr) (Obj.magic cintptr_expr f a) (fun a' ->
    bind (Obj.magic coq_Monad_optErr) (Obj.magic cintptr_expr f v) (fun v' ->
      bind (Obj.magic coq_Monad_optErr) (Obj.magic cintptr_exprs f args)
        (fun args' ->
        ret (Obj.magic coq_Monad_optErr) (StmtCintptr.Scallmethod (a', rvs,
          sg, v', args')))))
| Slog (topics, args) ->
  bind (Obj.magic coq_Monad_optErr) (Obj.magic cintptr_exprs f topics)
    (fun topics' ->
    bind (Obj.magic coq_Monad_optErr) (Obj.magic cintptr_exprs f args)
      (fun args' ->
      ret (Obj.magic coq_Monad_optErr) (StmtCintptr.Slog (topics', args'))))
| Srevert -> ret (Obj.magic coq_Monad_optErr) StmtCintptr.Srevert

(** val cintptr_function : coq_function -> StmtCintptr.coq_function optErr **)

let cintptr_function f =
  bind (Obj.magic coq_Monad_optErr) (Obj.magic cintptr_stmt f f.fn_body)
    (fun s ->
    ret (Obj.magic coq_Monad_optErr) { StmtCintptr.fn_return = f.fn_return;
      StmtCintptr.fn_params = f.fn_params; StmtCintptr.fn_temps = f.fn_temps;
      StmtCintptr.fn_locals = f.fn_locals; StmtCintptr.fn_body =
      (StmtCintptr.Ssequence ((pushS f), s)) })

(** val cintptr_fundefs :
    coq_function PTree.t -> StmtCintptr.coq_function PTree.t optErr **)

let rec cintptr_fundefs t0 =
  transl_tree cintptr_function t0

(** val cintptr_methods :
    coq_function option IntMap.t -> StmtCintptr.coq_function option IntMap.t
    optErr **)

let rec cintptr_methods methods =
  transl_map cintptr_function methods

(** val cintptr_constructor :
    coq_function option -> StmtCintptr.coq_function optErr **)

let cintptr_constructor = function
| Some c ->
  bind (Obj.magic coq_Monad_optErr) (cintptr_function c) (fun f ->
    ret (Obj.magic coq_Monad_optErr) f)
| None ->
  Error (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false,
    Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
    Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
    ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
    Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
    Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
    (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false,
    Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
    Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
    Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
    Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true,
    Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
    Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
    Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true,
    Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
    Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
    Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
    Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true,
    Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
    Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
    Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
    Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
    Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
    Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
    Coq_false)), EmptyString))))))))))))))))))))))))))))))))))))))))))

(** val cintptr_genv : genv -> StmtCintptr.genv optErr **)

let cintptr_genv ge =
  let vars = ge.Genv.genv_vars in
  let funcs = ge.Genv.genv_funcs in
  let methods = ge.Genv.genv_methods in
  let defs = ge.Genv.genv_defs in
  let fundefs = ge.Genv.genv_fundefs in
  let methoddefs = ge.Genv.genv_methoddefs in
  let constructor = ge.Genv.genv_constructor in
  bind (Obj.magic coq_Monad_optErr) (Obj.magic cintptr_fundefs fundefs)
    (fun fundefs0 ->
    bind (Obj.magic coq_Monad_optErr) (Obj.magic cintptr_methods methoddefs)
      (fun methoddefs0 ->
      bind (Obj.magic coq_Monad_optErr)
        (Obj.magic cintptr_constructor constructor) (fun constructor0 ->
        ret (Obj.magic coq_Monad_optErr) { Genv.genv_vars = vars;
          Genv.genv_funcs = funcs; Genv.genv_methods = methods;
          Genv.genv_defs = defs; Genv.genv_fundefs = fundefs0;
          Genv.genv_methoddefs = methoddefs0; Genv.genv_constructor = (Some
          constructor0) })))
