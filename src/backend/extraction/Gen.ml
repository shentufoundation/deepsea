open Ascii
open BinNums
open Cop
open Ctypes
open Datatypes
open ExpMiniC
open Globalenvs
open Integers
open Maps0
open Monad
open OptErrMonad
open StmtMiniC
open String0
open Trees

(** val constofpos : positive -> expr **)

let constofpos p =
  Econst_int256 ((Int256.repr (Zpos p)), (Tpointer (Coq_stor, Tvoid)))

(** val clike_rvalue : expr -> expr optErr **)

let rec clike_rvalue = function
| Econst_int (_, _) ->
  Error (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false,
    Coq_false, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
    Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
    ((Ascii (Coq_true, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
    Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
    Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
    (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true,
    Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
    Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false,
    Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
    Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true,
    Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
    Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
    (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
    Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
    Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
    ((Ascii (Coq_false, Coq_true, Coq_true, Coq_false, Coq_false, Coq_true,
    Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
    Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
    (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true,
    Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
    Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
    Coq_true, Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_false,
    Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false,
    Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
    Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true,
    Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
    Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
    Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
    Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true,
    Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
    Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
    (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
    Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
    Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
    (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
    Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
    Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
    ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
    Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
    Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
    (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_true,
    Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
    Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true,
    Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
    Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
    Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
    Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true,
    Coq_false, Coq_true, Coq_false, Coq_false)),
    EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
| Evar (id, ty) ->
  ret (Obj.magic coq_Monad_optErr) (Ederef ((Evar (id, (Tpointer (Coq_mem,
    ty)))), ty))
| Eglob (id, ty) ->
  ret (Obj.magic coq_Monad_optErr) (Ederef ((Eunop (Osha_1, (constofpos id),
    (Tpointer (Coq_stor, ty)))), ty))
| Ederef (ex0, ty) ->
  bind (Obj.magic coq_Monad_optErr) (clike_lvalue ex0) (fun ex' ->
    ret (Obj.magic coq_Monad_optErr) (Ederef (ex', ty)))
| Eaddr (ex0, _) -> clike_lvalue ex0
| Eunop (op, ex0, ty) ->
  bind (Obj.magic coq_Monad_optErr) (clike_rvalue ex0) (fun ex' ->
    ret (Obj.magic coq_Monad_optErr) (Eunop (op, ex', ty)))
| Ebinop (op, ex1, ex2, ty) ->
  bind (Obj.magic coq_Monad_optErr) (clike_rvalue ex1) (fun ex1' ->
    bind (Obj.magic coq_Monad_optErr) (clike_rvalue ex2) (fun ex2' ->
      ret (Obj.magic coq_Monad_optErr) (Ebinop (op, ex1', ex2', ty))))
| Efield (ex0, id, ty) ->
  bind (Obj.magic coq_Monad_optErr) (clike_lvalue ex0) (fun ex' ->
    match typeof ex' with
    | Tpointer (p, _) ->
      (match p with
       | Coq_mem ->
         ret (Obj.magic coq_Monad_optErr) (Ederef ((Efield (ex', id,
           (Tpointer (Coq_mem, ty)))), ty))
       | Coq_stor ->
         ret (Obj.magic coq_Monad_optErr) (Ederef ((Ebinop (Osha_2, ex',
           (constofpos id), (Tpointer (Coq_stor, ty)))), ty))
       | Coq_call ->
         Error (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true,
           Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false,
           Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
           Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
           Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
           Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_false,
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
           (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
           Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
           Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
           Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
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
           (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
           Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false,
           Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
           Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
           Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
           Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
           Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
           Coq_true, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
           Coq_true, Coq_true, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_false,
           Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
           Coq_false, Coq_false)),
           EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
    | _ ->
      Error (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true,
        Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
        Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
        Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
        (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_true, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
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
        Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
        Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_true,
        Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
        ((Ascii (Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
        Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
        Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
        (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
        Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
        Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
        ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
        Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
        Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
        (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_true,
        Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
        (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
        Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
        Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
        Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
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
        (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
        Coq_true, Coq_false, Coq_false)),
        EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
| Eindex (ex1, ex2, ty) ->
  bind (Obj.magic coq_Monad_optErr) (clike_lvalue ex1) (fun ex1' ->
    bind (Obj.magic coq_Monad_optErr) (clike_rvalue ex2) (fun ex2' ->
      match typeof ex1' with
      | Tpointer (p, _) ->
        (match p with
         | Coq_mem ->
           ret (Obj.magic coq_Monad_optErr) (Ederef ((Eindex (ex1', ex2',
             (Tpointer (Coq_mem, ty)))), ty))
         | Coq_stor ->
           ret (Obj.magic coq_Monad_optErr) (Ederef ((Ebinop (Osha_2, ex1',
             ex2', (Tpointer (Coq_stor, ty)))), ty))
         | Coq_call ->
           Error (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true,
             Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii
             (Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
             Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
             Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
             (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false,
             Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
             (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
             Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
             Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
             (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false,
             Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
             (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
             Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true,
             Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
             Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
             Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
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
             Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
             Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
             ((Ascii (Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
             Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
             Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
             Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
             Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
             ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false,
             Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
             Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
             Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
             Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
             ((Ascii (Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
             Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
             Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
             Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
             Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
             ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
             Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
             Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
             Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
             Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
             ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
             Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
             Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
             Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
             Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
             ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
             Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
             Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
             Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_false,
             Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
             ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
             Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_false,
             Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
             Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
             Coq_true, Coq_true, Coq_true, Coq_true, Coq_false)), (String
             ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
             Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
             Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
             Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
             Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
             ((Ascii (Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
             Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true,
             Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
             Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
             Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
             ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
             Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
             Coq_true, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
             Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
             Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
             ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
             Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true,
             Coq_true, Coq_true, Coq_false, Coq_false, Coq_false, Coq_true,
             Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
             Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
             ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
             Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
             Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_false,
             Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
             Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
             ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
             Coq_true, Coq_false, Coq_false)),
             EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
      | _ ->
        Error (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true,
          Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii
          (Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
          Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
          Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
          (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false,
          Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
          (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
          Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
          Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
          (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false,
          Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
          (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
          Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
          Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
          (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_false,
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
          (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
          Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
          (Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
          Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
          Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
          (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
          Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
          (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
          Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
          Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
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
          (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
          Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
          (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
          Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
          Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
          (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true,
          Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
          (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
          Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
          Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
          (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false,
          Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
          (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
          Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
          Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
          (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true,
          Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
          (Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
          Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
          Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
          (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
          Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
          (Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
          Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
          Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false)),
          (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true,
          Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
          (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
          Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
          Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
          (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
          Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
          (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
          Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
          Coq_true, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false)),
          (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
          Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
          (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
          Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
          Coq_true, Coq_true, Coq_false, Coq_true, Coq_false, Coq_false)),
          (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_false,
          Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
          (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
          Coq_false, Coq_false)),
          EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
| Ecall1 (b, ex0, ty) ->
  bind (Obj.magic coq_Monad_optErr) (clike_rvalue ex0) (fun ex' ->
    ret (Obj.magic coq_Monad_optErr) (Ecall1 (b, ex', ty)))
| x -> ret (Obj.magic coq_Monad_optErr) x

(** val clike_lvalue : expr -> expr optErr **)

and clike_lvalue = function
| Evar (id, ty) ->
  ret (Obj.magic coq_Monad_optErr) (Evar (id, (Tpointer (Coq_mem, ty))))
| Eglob (id, ty) ->
  ret (Obj.magic coq_Monad_optErr) (Eunop (Osha_1, (constofpos id), (Tpointer
    (Coq_stor, ty))))
| Ederef (ex0, _) -> clike_rvalue ex0
| Efield (ex0, id, ty) ->
  bind (Obj.magic coq_Monad_optErr) (clike_lvalue ex0) (fun ex' ->
    match typeof ex' with
    | Tpointer (p, _) ->
      (match p with
       | Coq_mem ->
         ret (Obj.magic coq_Monad_optErr) (Efield (ex', id, (Tpointer
           (Coq_mem, ty))))
       | Coq_stor ->
         ret (Obj.magic coq_Monad_optErr) (Ebinop (Osha_2, ex',
           (constofpos id), (Tpointer (Coq_stor, ty))))
       | Coq_call ->
         Error (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true,
           Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false,
           Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
           Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
           Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
           Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_false,
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
           Coq_false, Coq_true, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true,
           Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
           Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false,
           Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
           Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
           Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
           Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
           Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
           Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_true,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_true,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
           Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
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
           (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
           Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_true,
           Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
           (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_true, Coq_false, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true,
           Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
           (Coq_true, Coq_true, Coq_true, Coq_false, Coq_false, Coq_false,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
           Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true,
           Coq_false, Coq_true, Coq_false, Coq_false)),
           EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
    | _ ->
      Error (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true,
        Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
        Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
        Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
        (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_true, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
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
        (Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
        Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
        Coq_true, Coq_true, Coq_false, Coq_false, Coq_true, Coq_false)),
        (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
        Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_false,
        Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
        Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
        Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
        ((Ascii (Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_false,
        Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
        Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
        Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true,
        Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
        (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true,
        Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_true, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
        Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false,
        Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
        Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
        Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false,
        Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
        Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
        Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
        (Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
        Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
        Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_false,
        Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
        Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
        Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
        ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
        Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_false,
        Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_false,
        Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
        Coq_false, Coq_false, Coq_false, Coq_true, Coq_false)), (String
        ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
        Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
        Coq_true, Coq_false, Coq_true, Coq_false, Coq_false)), (String
        ((Ascii (Coq_true, Coq_true, Coq_true, Coq_false, Coq_false,
        Coq_false, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
        Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
        Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
        Coq_true, Coq_false, Coq_false)),
        EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
| Eindex (ex1, ex2, ty) ->
  bind (Obj.magic coq_Monad_optErr) (clike_lvalue ex1) (fun ex1' ->
    bind (Obj.magic coq_Monad_optErr) (clike_rvalue ex2) (fun ex2' ->
      match typeof ex1' with
      | Tpointer (p, _) ->
        (match p with
         | Coq_mem ->
           ret (Obj.magic coq_Monad_optErr) (Eindex (ex1', ex2', (Tpointer
             (Coq_mem, ty))))
         | Coq_stor ->
           ret (Obj.magic coq_Monad_optErr) (Ebinop (Osha_2, ex1', ex2',
             (Tpointer (Coq_stor, ty))))
         | Coq_call ->
           Error (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true,
             Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii
             (Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
             Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
             Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
             (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false,
             Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
             (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
             Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
             Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
             (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false,
             Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
             (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
             Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true,
             Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
             Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
             Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
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
             Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_false,
             Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
             Coq_true, Coq_false, Coq_false, Coq_true, Coq_false)), (String
             ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
             Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_false,
             Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
             Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
             Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
             ((Ascii (Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
             Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
             Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
             Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_false,
             Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
             ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
             Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
             Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
             Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
             Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
             ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
             Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
             Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
             Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
             Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
             ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
             Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true,
             Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
             Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
             Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
             ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
             Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
             Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
             Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
             Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
             ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_false,
             Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
             Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
             Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
             Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
             ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
             Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
             Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
             Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
             Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
             ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
             Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_false,
             Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
             Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
             Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
             ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
             Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
             Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
             Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
             Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
             ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
             Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
             Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
             Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
             Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
             ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
             Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
             Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_true,
             Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
             Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
             ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
             Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
             Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
             Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
             Coq_true, Coq_false, Coq_true, Coq_false, Coq_false)), (String
             ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false,
             Coq_false, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
             Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
             Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
             Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
             ((Ascii (Coq_true, Coq_true, Coq_false, Coq_true, Coq_false,
             Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
             Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
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
             Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
             Coq_true, Coq_false, Coq_true, Coq_false, Coq_false)),
             EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
      | _ ->
        Error (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true,
          Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii
          (Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
          Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
          Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
          (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false,
          Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
          (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
          Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
          Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
          (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false,
          Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
          (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
          Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
          Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
          (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_false,
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
          Coq_false, Coq_true, Coq_false, Coq_true, Coq_false, Coq_false)),
          (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true,
          Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii
          (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
          Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
          Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
          (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
          Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
          (Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
          Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
          Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
          (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false,
          Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
          (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
          Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
          Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
          (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
          Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
          (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
          Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
          Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
          (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true,
          Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
          (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
          Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
          Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
          (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_true,
          Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
          (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
          Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
          Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
          (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true,
          Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
          (Coq_false, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
          Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
          Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
          (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_true,
          Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
          (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
          Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
          Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
          (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
          Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
          (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
          Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
          Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
          (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
          Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
          Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
          Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
          Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
          ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
          Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
          Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
          Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_false,
          Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
          ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
          Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_false,
          Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
          Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
          Coq_true, Coq_true, Coq_true, Coq_true, Coq_false)), (String
          ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
          Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
          Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
          Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
          Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
          ((Ascii (Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
          Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true,
          Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
          Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
          Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
          ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
          Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
          Coq_true, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
          Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
          Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
          ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
          Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true,
          Coq_true, Coq_true, Coq_false, Coq_false, Coq_false, Coq_true,
          Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
          Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
          ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
          Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
          Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_false,
          Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
          Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
          ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
          Coq_true, Coq_false, Coq_false)),
          EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
| _ ->
  Error (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_true, Coq_false,
    Coq_false, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
    Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
    ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
    Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
    Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
    (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
    Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
    Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
    (Coq_true, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_false,
    Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true,
    Coq_true, Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_false,
    Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
    Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
    Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
    Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true,
    Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
    Coq_true, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
    Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
    Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
    Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
    Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_false,
    Coq_false, Coq_true, Coq_false, Coq_true, Coq_false, Coq_true,
    Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false,
    Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
    Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
    Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
    Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
    Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_false,
    Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true,
    Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
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
    Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true,
    Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
    Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
    Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
    Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
    Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
    Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
    Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
    Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
    Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
    Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
    Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true,
    Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
    Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
    (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
    Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
    Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
    ((Ascii (Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
    Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
    Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
    (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_true,
    Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
    Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
    Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
    Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
    Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true,
    Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
    Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
    (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
    Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
    Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
    ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
    Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_false,
    Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
    (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
    Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true,
    Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
    Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
    Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false,
    Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
    Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
    Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false,
    Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
    Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
    Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true,
    Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true,
    Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
    Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
    Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true,
    Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
    Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_false,
    Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_false,
    Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
    Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true,
    Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
    Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
    Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
    Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_false,
    Coq_false)),
    EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

(** val clike_rvalue_list : expr list -> expr list optErr **)

let rec clike_rvalue_list = function
| Coq_nil -> ret (Obj.magic coq_Monad_optErr) Coq_nil
| Coq_cons (hd, tl) ->
  bind (Obj.magic coq_Monad_optErr) (Obj.magic clike_rvalue hd) (fun first ->
    bind (Obj.magic coq_Monad_optErr) (clike_rvalue_list tl) (fun rest ->
      ret (Obj.magic coq_Monad_optErr) (Coq_cons (first, rest))))

(** val clike_rvalue_option : expr option -> expr option optErr **)

let rec clike_rvalue_option = function
| Some e ->
  let e' = clike_rvalue e in
  (match e' with
   | Success s -> ret (Obj.magic coq_Monad_optErr) (Some s)
   | Error msg -> Error msg)
| None -> ret (Obj.magic coq_Monad_optErr) None

(** val clike_stm : statement -> statement optErr **)

let rec clike_stm = function
| Sassign (lv, rv) ->
  bind (Obj.magic coq_Monad_optErr) (Obj.magic clike_lvalue lv) (fun lv' ->
    bind (Obj.magic coq_Monad_optErr) (Obj.magic clike_rvalue rv) (fun rv' ->
      ret (Obj.magic coq_Monad_optErr) (Sassign (lv', rv'))))
| Sset (id, rv) ->
  bind (Obj.magic coq_Monad_optErr) (Obj.magic clike_rvalue rv) (fun rv' ->
    ret (Obj.magic coq_Monad_optErr) (Sset (id, rv')))
| Scall (id, label, args) ->
  bind (Obj.magic coq_Monad_optErr) (Obj.magic clike_rvalue_list args)
    (fun rv_list ->
    ret (Obj.magic coq_Monad_optErr) (Scall (id, label, rv_list)))
| Ssequence (stm1, stm2) ->
  bind (Obj.magic coq_Monad_optErr) (clike_stm stm1) (fun seq1 ->
    bind (Obj.magic coq_Monad_optErr) (clike_stm stm2) (fun seq2 ->
      ret (Obj.magic coq_Monad_optErr) (Ssequence (seq1, seq2))))
| Sifthenelse (ex, stm1, stm2) ->
  bind (Obj.magic coq_Monad_optErr) (Obj.magic clike_rvalue ex) (fun ex0 ->
    bind (Obj.magic coq_Monad_optErr) (clike_stm stm1) (fun true_stm ->
      bind (Obj.magic coq_Monad_optErr) (clike_stm stm2) (fun false_stm ->
        ret (Obj.magic coq_Monad_optErr) (Sifthenelse (ex0, true_stm,
          false_stm)))))
| Sloop loop ->
  bind (Obj.magic coq_Monad_optErr) (clike_stm loop) (fun loop0 ->
    ret (Obj.magic coq_Monad_optErr) (Sloop loop0))
| Stransfer (addr, val0) ->
  bind (Obj.magic coq_Monad_optErr) (Obj.magic clike_rvalue addr)
    (fun addr0 ->
    bind (Obj.magic coq_Monad_optErr) (Obj.magic clike_rvalue val0)
      (fun val1 -> ret (Obj.magic coq_Monad_optErr) (Stransfer (addr0, val1))))
| Scallmethod (addr, retvals, funsig, val0, gas, args) ->
  bind (Obj.magic coq_Monad_optErr) (Obj.magic clike_rvalue addr)
    (fun addr0 ->
    bind (Obj.magic coq_Monad_optErr) (Obj.magic clike_rvalue val0)
      (fun val1 ->
      bind (Obj.magic coq_Monad_optErr) (Obj.magic clike_rvalue_option gas)
        (fun gas0 ->
        bind (Obj.magic coq_Monad_optErr) (Obj.magic clike_rvalue_list args)
          (fun args0 ->
          ret (Obj.magic coq_Monad_optErr) (Scallmethod (addr0, retvals,
            funsig, val1, gas0, args0))))))
| Slog (topics, args) ->
  bind (Obj.magic coq_Monad_optErr) (Obj.magic clike_rvalue_list topics)
    (fun topics0 ->
    bind (Obj.magic coq_Monad_optErr) (Obj.magic clike_rvalue_list args)
      (fun args0 -> ret (Obj.magic coq_Monad_optErr) (Slog (topics0, args0))))
| x -> ret (Obj.magic coq_Monad_optErr) x

(** val clike_function : coq_function -> coq_function optErr **)

let clike_function f =
  bind (Obj.magic coq_Monad_optErr) (Obj.magic clike_stm f.fn_body)
    (fun stm ->
    ret (Obj.magic coq_Monad_optErr) { fn_return = f.fn_return; fn_params =
      f.fn_params; fn_temps = f.fn_temps; fn_locals = f.fn_locals; fn_body =
      stm })

(** val clike_constructor :
    coq_function option -> coq_function option optErr **)

let clike_constructor = function
| Some f0 ->
  bind (Obj.magic coq_Monad_optErr) (Obj.magic clike_function f0) (fun f1 ->
    ret (Obj.magic coq_Monad_optErr) (Some f1))
| None -> ret (Obj.magic coq_Monad_optErr) None

(** val clike_functions :
    coq_function PTree.t -> coq_function PTree.t optErr **)

let clike_functions defs =
  transl_tree clike_function defs

(** val clike_methoddefs :
    coq_function option IntMap.t -> coq_function option IntMap.t optErr **)

let clike_methoddefs defs =
  transl_map clike_function defs

(** val clike_genv : genv -> genv optErr **)

let clike_genv ge =
  let vars = ge.Genv.genv_vars in
  let defs = ge.Genv.genv_defs in
  let names = ge.Genv.genv_funcs in
  let functions = ge.Genv.genv_fundefs in
  let sigs = ge.Genv.genv_methods in
  let methoddefs = ge.Genv.genv_methoddefs in
  let constructor = ge.Genv.genv_constructor in
  bind (Obj.magic coq_Monad_optErr) (Obj.magic clike_functions functions)
    (fun functions0 ->
    bind (Obj.magic coq_Monad_optErr) (Obj.magic clike_methoddefs methoddefs)
      (fun methoddefs0 ->
      bind (Obj.magic coq_Monad_optErr)
        (Obj.magic clike_constructor constructor) (fun constructor0 ->
        ret (Obj.magic coq_Monad_optErr) { Genv.genv_vars = vars;
          Genv.genv_funcs = names; Genv.genv_methods = sigs; Genv.genv_defs =
          defs; Genv.genv_fundefs = functions0; Genv.genv_methoddefs =
          methoddefs0; Genv.genv_constructor = constructor0 })))
