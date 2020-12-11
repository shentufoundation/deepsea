open AST
open Ascii
open BinInt
open BinNums
open Cop
open Ctypes
open Datatypes
open ExpCintptr
open ExpStacked
open FramesLabelsCintptr
open Globalenvs
open Integers
open List0
open Maps0
open MemoryModel
open Monad
open Nat0
open OptErrMonad
open Options
open Semantics1
open StmtClinear
open StmtStacked
open String0
open TempModelLow
open Trees

(** val stacked_expr :
    nat PTree.t -> ExpCintptr.expr -> nat -> bool -> statement list optErr **)

let rec stacked_expr temps e s_extra rvalue =
  match e with
  | Econst_int (_, _) ->
    Error (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
      Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
      Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
      Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true,
      Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
      Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
      Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true,
      Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
      Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
      Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
      Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
      (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
      Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_true, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
      Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
      (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
      Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
      ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
      Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
      (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true,
      Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
      Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_false)),
      (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_true, Coq_false,
      Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
      Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
      (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
      Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
      ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
      Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
      Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
      (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
      Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
      ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
      Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
      Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
      (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
      Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
      ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
      Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
      Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
      Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
      EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
  | ExpCintptr.Econst_int256 (i, _) ->
    ret (Obj.magic coq_Monad_optErr) (Coq_cons ((Srvalue (Econst_int256 i)),
      Coq_nil))
  | ExpCintptr.Etempvar (i, _) ->
    bind (Obj.magic coq_Monad_optErr)
      (fromOption (PTree.get i (Obj.magic temps)) (String ((Ascii (Coq_true,
        Coq_true, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true,
        Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_true, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
        Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false,
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
        Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
        (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
        Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false,
        Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
        (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
        Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
        ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
        Coq_false)),
        EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))
      (fun ind ->
      ret (Obj.magic coq_Monad_optErr) (Coq_cons ((Srvalue (Etempvar
        (add s_extra ind))), Coq_nil)))
  | ExpCintptr.Esload (e0, _) ->
    bind (Obj.magic coq_Monad_optErr)
      (stacked_expr temps e0 s_extra Coq_true) (fun e' ->
      ret (Obj.magic coq_Monad_optErr)
        (app e' (Coq_cons ((Srvalue Esload), Coq_nil))))
  | ExpCintptr.Emload (e0, _) ->
    bind (Obj.magic coq_Monad_optErr)
      (stacked_expr temps e0 s_extra Coq_true) (fun e' ->
      ret (Obj.magic coq_Monad_optErr)
        (app e' (Coq_cons ((Srvalue Emload), Coq_nil))))
  | Eaddr (e0, _) ->
    (match rvalue with
     | Coq_true ->
       bind (Obj.magic coq_Monad_optErr)
         (stacked_expr temps e0 s_extra Coq_false) (fun e' ->
         ret (Obj.magic coq_Monad_optErr) e')
     | Coq_false ->
       Error (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii
         (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
         Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
         Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
         (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
         Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
         (Coq_true, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
         Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
         (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false,
         Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
         (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
         Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
         Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
         (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
         Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
         Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
         Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
         Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
         Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true,
         Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
         Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
         Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_false,
         Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
         Coq_true, Coq_false, Coq_true, Coq_false, Coq_false)), (String
         ((Ascii (Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
         Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
         Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_true,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
         Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
         ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
         Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
         ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
         Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_false,
         Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
         Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
         Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
         Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
         Coq_true, Coq_false, Coq_true, Coq_false, Coq_false)), (String
         ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
         Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
         Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_false,
         Coq_false)),
         EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
  | ExpCintptr.Eunop (o, e0, _) ->
    (match rvalue with
     | Coq_true ->
       (match o with
        | Onotbool ->
          bind (Obj.magic coq_Monad_optErr)
            (stacked_expr temps e0 s_extra Coq_true) (fun e' ->
            ret (Obj.magic coq_Monad_optErr)
              (app e' (Coq_cons ((Srvalue (Eunop o)), Coq_nil))))
        | Onotint ->
          bind (Obj.magic coq_Monad_optErr)
            (stacked_expr temps e0 s_extra Coq_true) (fun e' ->
            ret (Obj.magic coq_Monad_optErr)
              (app e' (Coq_cons ((Srvalue (Eunop o)), Coq_nil))))
        | Oneg ->
          bind (Obj.magic coq_Monad_optErr)
            (stacked_expr temps e0 s_extra Coq_true) (fun e' ->
            ret (Obj.magic coq_Monad_optErr)
              (app e' (Coq_cons ((Srvalue (Eunop o)), Coq_nil))))
        | Osha_1 ->
          Error (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_true,
            Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii
            (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
            Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
            Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
            (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_true,
            Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
            (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
            Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
            Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
            (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
            Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
            (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
            Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
            Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
            (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true,
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
            (Coq_false, Coq_true, Coq_false, Coq_false, Coq_false, Coq_true,
            Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
            Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
            (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
            Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
            (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
            Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
            Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
            Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
            Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
            ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
            Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
            Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
            Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
            Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
            ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
            Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
            Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
            Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
            Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
            ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_false,
            Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
            Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
            Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
            Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
            ((Ascii (Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
            Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
            Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
            Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
            Coq_true, Coq_true, Coq_true, Coq_true, Coq_false)), (String
            ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
            Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_false,
            Coq_true, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
            Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
            Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
            ((Ascii (Coq_false, Coq_true, Coq_true, Coq_false, Coq_false,
            Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
            Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
            Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_false,
            Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
            ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
            Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
            Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
            Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
            Coq_false, Coq_true, Coq_false, Coq_true, Coq_false)), (String
            ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
            Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
            Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
            Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
            Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
            ((Ascii (Coq_true, Coq_true, Coq_false, Coq_true, Coq_false,
            Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
            Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
            Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
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
            Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
            EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
     | Coq_false ->
       (match o with
        | Osha_1 ->
          Error (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_true,
            Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii
            (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
            Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
            Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
            (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_true,
            Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
            (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
            Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
            Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
            (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
            Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
            (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
            Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
            Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
            (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true,
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
            (Coq_false, Coq_true, Coq_false, Coq_false, Coq_false, Coq_true,
            Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
            Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
            (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
            Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
            (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
            Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
            Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
            Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
            Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
            ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
            Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
            Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
            Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
            Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
            ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
            Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
            Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
            Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
            Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
            ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_false,
            Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
            Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
            Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
            Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
            ((Ascii (Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
            Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
            Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
            Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
            Coq_true, Coq_true, Coq_true, Coq_true, Coq_false)), (String
            ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
            Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_false,
            Coq_true, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
            Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
            Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
            ((Ascii (Coq_false, Coq_true, Coq_true, Coq_false, Coq_false,
            Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
            Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
            Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_false,
            Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
            ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
            Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
            Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
            Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
            Coq_false, Coq_true, Coq_false, Coq_true, Coq_false)), (String
            ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
            Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
            Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
            Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
            Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
            ((Ascii (Coq_true, Coq_true, Coq_false, Coq_true, Coq_false,
            Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
            Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
            Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
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
            Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
            EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
        | _ ->
          Error (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false,
            Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii
            (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
            Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
            Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
            (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
            Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
            (Coq_true, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
            Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
            Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
            (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false,
            Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
            (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
            Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
            Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
            (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true,
            Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
            (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
            Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
            Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
            (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true,
            Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
            (Coq_true, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
            Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
            Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
            (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true,
            Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
            (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
            Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
            Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
            (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
            Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
            (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
            Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
            Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
            (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
            Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
            (Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
            Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
            Coq_true, Coq_true, Coq_false, Coq_true, Coq_false, Coq_false)),
            (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_false,
            Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
            (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
            Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
            Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
            (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
            Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
            (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
            Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
            Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
            Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
            Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
            ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true,
            Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
            Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
            Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
            Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
            ((Ascii (Coq_true, Coq_false, Coq_true, Coq_true, Coq_false,
            Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_false,
            Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
            Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
            Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
            ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false,
            Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
            Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
            Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
            Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
            ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
            Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_false,
            Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_false,
            Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
            Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
            ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
            Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
            Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
            Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
            Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
            ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
            Coq_true, Coq_false, Coq_false)),
            EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
  | ExpCintptr.Ebinop (o, e1, e2, _) ->
    (match o with
     | Oadd ->
       bind (Obj.magic coq_Monad_optErr)
         (stacked_expr temps e1 (S s_extra) rvalue) (fun e1' ->
         bind (Obj.magic coq_Monad_optErr)
           (stacked_expr temps e2 s_extra Coq_true) (fun e2' ->
           ret (Obj.magic coq_Monad_optErr)
             (app e2'
               (app e1' (Coq_cons
                 ((match rvalue with
                   | Coq_true -> Srvalue (Ebinop (o, Coq_false))
                   | Coq_false -> Slvalue (Ebinop (o, Coq_false))), Coq_nil))))))
     | Osub ->
       bind (Obj.magic coq_Monad_optErr)
         (stacked_expr temps e1 (S s_extra) rvalue) (fun e1' ->
         bind (Obj.magic coq_Monad_optErr)
           (stacked_expr temps e2 s_extra Coq_true) (fun e2' ->
           ret (Obj.magic coq_Monad_optErr)
             (app e2'
               (app e1' (Coq_cons
                 ((match rvalue with
                   | Coq_true -> Srvalue (Ebinop (o, Coq_false))
                   | Coq_false -> Slvalue (Ebinop (o, Coq_false))), Coq_nil))))))
     | Omul ->
       bind (Obj.magic coq_Monad_optErr)
         (stacked_expr temps e1 (S s_extra) rvalue) (fun e1' ->
         bind (Obj.magic coq_Monad_optErr)
           (stacked_expr temps e2 s_extra Coq_true) (fun e2' ->
           ret (Obj.magic coq_Monad_optErr)
             (app e2'
               (app e1' (Coq_cons
                 ((match rvalue with
                   | Coq_true -> Srvalue (Ebinop (o, Coq_false))
                   | Coq_false -> Slvalue (Ebinop (o, Coq_false))), Coq_nil))))))
     | Odiv ->
       bind (Obj.magic coq_Monad_optErr)
         (stacked_expr temps e1 (S s_extra) rvalue) (fun e1' ->
         bind (Obj.magic coq_Monad_optErr)
           (stacked_expr temps e2 s_extra Coq_true) (fun e2' ->
           ret (Obj.magic coq_Monad_optErr)
             (app e2'
               (app e1' (Coq_cons
                 ((match rvalue with
                   | Coq_true -> Srvalue (Ebinop (o, Coq_false))
                   | Coq_false -> Slvalue (Ebinop (o, Coq_false))), Coq_nil))))))
     | Omod ->
       bind (Obj.magic coq_Monad_optErr)
         (stacked_expr temps e1 (S s_extra) rvalue) (fun e1' ->
         bind (Obj.magic coq_Monad_optErr)
           (stacked_expr temps e2 s_extra Coq_true) (fun e2' ->
           ret (Obj.magic coq_Monad_optErr)
             (app e2'
               (app e1' (Coq_cons
                 ((match rvalue with
                   | Coq_true -> Srvalue (Ebinop (o, Coq_false))
                   | Coq_false -> Slvalue (Ebinop (o, Coq_false))), Coq_nil))))))
     | Oexp ->
       bind (Obj.magic coq_Monad_optErr)
         (stacked_expr temps e1 (S s_extra) rvalue) (fun e1' ->
         bind (Obj.magic coq_Monad_optErr)
           (stacked_expr temps e2 s_extra Coq_true) (fun e2' ->
           ret (Obj.magic coq_Monad_optErr)
             (app e2'
               (app e1' (Coq_cons
                 ((match rvalue with
                   | Coq_true -> Srvalue (Ebinop (o, Coq_false))
                   | Coq_false -> Slvalue (Ebinop (o, Coq_false))), Coq_nil))))))
     | Oand ->
       bind (Obj.magic coq_Monad_optErr)
         (stacked_expr temps e1 (S s_extra) rvalue) (fun e1' ->
         bind (Obj.magic coq_Monad_optErr)
           (stacked_expr temps e2 s_extra Coq_true) (fun e2' ->
           ret (Obj.magic coq_Monad_optErr)
             (app e2'
               (app e1' (Coq_cons
                 ((match rvalue with
                   | Coq_true -> Srvalue (Ebinop (o, Coq_false))
                   | Coq_false -> Slvalue (Ebinop (o, Coq_false))), Coq_nil))))))
     | Oor ->
       bind (Obj.magic coq_Monad_optErr)
         (stacked_expr temps e1 (S s_extra) rvalue) (fun e1' ->
         bind (Obj.magic coq_Monad_optErr)
           (stacked_expr temps e2 s_extra Coq_true) (fun e2' ->
           ret (Obj.magic coq_Monad_optErr)
             (app e2'
               (app e1' (Coq_cons
                 ((match rvalue with
                   | Coq_true -> Srvalue (Ebinop (o, Coq_false))
                   | Coq_false -> Slvalue (Ebinop (o, Coq_false))), Coq_nil))))))
     | Oxor ->
       bind (Obj.magic coq_Monad_optErr)
         (stacked_expr temps e1 (S s_extra) rvalue) (fun e1' ->
         bind (Obj.magic coq_Monad_optErr)
           (stacked_expr temps e2 s_extra Coq_true) (fun e2' ->
           ret (Obj.magic coq_Monad_optErr)
             (app e2'
               (app e1' (Coq_cons
                 ((match rvalue with
                   | Coq_true -> Srvalue (Ebinop (o, Coq_false))
                   | Coq_false -> Slvalue (Ebinop (o, Coq_false))), Coq_nil))))))
     | Oshl ->
       bind (Obj.magic coq_Monad_optErr)
         (stacked_expr temps e1 (S s_extra) rvalue) (fun e1' ->
         bind (Obj.magic coq_Monad_optErr)
           (stacked_expr temps e2 s_extra Coq_true) (fun e2' ->
           ret (Obj.magic coq_Monad_optErr)
             (app e2'
               (app e1' (Coq_cons
                 ((match rvalue with
                   | Coq_true -> Srvalue (Ebinop (o, Coq_false))
                   | Coq_false -> Slvalue (Ebinop (o, Coq_false))), Coq_nil))))))
     | Oshr ->
       bind (Obj.magic coq_Monad_optErr)
         (stacked_expr temps e1 (S s_extra) rvalue) (fun e1' ->
         bind (Obj.magic coq_Monad_optErr)
           (stacked_expr temps e2 s_extra Coq_true) (fun e2' ->
           ret (Obj.magic coq_Monad_optErr)
             (app e2'
               (app e1' (Coq_cons
                 ((match rvalue with
                   | Coq_true -> Srvalue (Ebinop (o, Coq_false))
                   | Coq_false -> Slvalue (Ebinop (o, Coq_false))), Coq_nil))))))
     | Oeq ->
       bind (Obj.magic coq_Monad_optErr)
         (stacked_expr temps e1 (S s_extra) rvalue) (fun e1' ->
         bind (Obj.magic coq_Monad_optErr)
           (stacked_expr temps e2 s_extra Coq_true) (fun e2' ->
           ret (Obj.magic coq_Monad_optErr)
             (app e2'
               (app e1' (Coq_cons
                 ((match rvalue with
                   | Coq_true -> Srvalue (Ebinop (o, Coq_false))
                   | Coq_false -> Slvalue (Ebinop (o, Coq_false))), Coq_nil))))))
     | One ->
       bind (Obj.magic coq_Monad_optErr)
         (stacked_expr temps e1 (S s_extra) rvalue) (fun e1' ->
         bind (Obj.magic coq_Monad_optErr)
           (stacked_expr temps e2 s_extra Coq_true) (fun e2' ->
           ret (Obj.magic coq_Monad_optErr)
             (app e2'
               (app e1' (Coq_cons
                 ((match rvalue with
                   | Coq_true -> Srvalue (Ebinop (o, Coq_false))
                   | Coq_false -> Slvalue (Ebinop (o, Coq_false))), Coq_nil))))))
     | Olt ->
       bind (Obj.magic coq_Monad_optErr)
         (stacked_expr temps e1 (S s_extra) rvalue) (fun e1' ->
         bind (Obj.magic coq_Monad_optErr)
           (stacked_expr temps e2 s_extra Coq_true) (fun e2' ->
           ret (Obj.magic coq_Monad_optErr)
             (app e2'
               (app e1' (Coq_cons
                 ((match rvalue with
                   | Coq_true -> Srvalue (Ebinop (o, Coq_false))
                   | Coq_false -> Slvalue (Ebinop (o, Coq_false))), Coq_nil))))))
     | Ogt ->
       bind (Obj.magic coq_Monad_optErr)
         (stacked_expr temps e1 (S s_extra) rvalue) (fun e1' ->
         bind (Obj.magic coq_Monad_optErr)
           (stacked_expr temps e2 s_extra Coq_true) (fun e2' ->
           ret (Obj.magic coq_Monad_optErr)
             (app e2'
               (app e1' (Coq_cons
                 ((match rvalue with
                   | Coq_true -> Srvalue (Ebinop (o, Coq_false))
                   | Coq_false -> Slvalue (Ebinop (o, Coq_false))), Coq_nil))))))
     | Ole ->
       bind (Obj.magic coq_Monad_optErr)
         (stacked_expr temps e1 (S s_extra) rvalue) (fun e1' ->
         bind (Obj.magic coq_Monad_optErr)
           (stacked_expr temps e2 s_extra Coq_true) (fun e2' ->
           ret (Obj.magic coq_Monad_optErr)
             (app e2'
               (app e1' (Coq_cons
                 ((match rvalue with
                   | Coq_true -> Srvalue (Ebinop (o, Coq_false))
                   | Coq_false -> Slvalue (Ebinop (o, Coq_false))), Coq_nil))))))
     | Oge ->
       bind (Obj.magic coq_Monad_optErr)
         (stacked_expr temps e1 (S s_extra) rvalue) (fun e1' ->
         bind (Obj.magic coq_Monad_optErr)
           (stacked_expr temps e2 s_extra Coq_true) (fun e2' ->
           ret (Obj.magic coq_Monad_optErr)
             (app e2'
               (app e1' (Coq_cons
                 ((match rvalue with
                   | Coq_true -> Srvalue (Ebinop (o, Coq_false))
                   | Coq_false -> Slvalue (Ebinop (o, Coq_false))), Coq_nil))))))
     | Osha_2 ->
       Error (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_true,
         Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii
         (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
         Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
         Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
         (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_true,
         Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
         (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
         Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
         Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
         (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
         Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
         (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
         Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
         Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
         (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
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
         Coq_true, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
         Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
         ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
         Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
         Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
         Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
         ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
         Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
         Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_true, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
         Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
         Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
         ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true,
         Coq_false, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
         Coq_true, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
         Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
         Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_false,
         Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
         Coq_false, Coq_false, Coq_false, Coq_true, Coq_false)), (String
         ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
         Coq_true, Coq_false, Coq_true, Coq_false, Coq_false)), (String
         ((Ascii (Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
         Coq_true, Coq_true, Coq_false)),
         EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
  | ExpCintptr.Ecall0 (b, _) ->
    (match rvalue with
     | Coq_true ->
       ret (Obj.magic coq_Monad_optErr) (Coq_cons ((Srvalue (Ecall0 b)),
         Coq_nil))
     | Coq_false ->
       Error (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii
         (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
         Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
         Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
         (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
         Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
         (Coq_true, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
         Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
         (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false,
         Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
         (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
         Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
         Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
         (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
         Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
         Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
         Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
         Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
         Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true,
         Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
         Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
         Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_false,
         Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
         Coq_true, Coq_false, Coq_true, Coq_false, Coq_false)), (String
         ((Ascii (Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
         Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
         Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_true,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
         Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
         ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
         Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
         ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
         Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_false,
         Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
         Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
         Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
         Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
         Coq_true, Coq_false, Coq_true, Coq_false, Coq_false)), (String
         ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
         Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
         Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false,
         Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
         Coq_true, Coq_false, Coq_true, Coq_false, Coq_false)),
         EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
  | ExpCintptr.Ecall1 (b, e0, _) ->
    (match rvalue with
     | Coq_true ->
       bind (Obj.magic coq_Monad_optErr)
         (stacked_expr temps e0 s_extra Coq_true) (fun e' ->
         ret (Obj.magic coq_Monad_optErr)
           (app e' (Coq_cons ((Srvalue (Ecall1 b)), Coq_nil))))
     | Coq_false ->
       Error (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii
         (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
         Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
         Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
         (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
         Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
         (Coq_true, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
         Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
         (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false,
         Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
         (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
         Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
         Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
         (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
         Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
         Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
         Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
         Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
         Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true,
         Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
         Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
         Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_false,
         Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
         Coq_true, Coq_false, Coq_true, Coq_false, Coq_false)), (String
         ((Ascii (Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
         Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
         Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_true,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
         Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
         ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
         Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
         ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
         Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_false,
         Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
         Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
         Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
         Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
         Coq_true, Coq_false, Coq_true, Coq_false, Coq_false)), (String
         ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
         Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
         Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
         Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false,
         Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
         Coq_true, Coq_false, Coq_true, Coq_false, Coq_false)),
         EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

(** val stacked_exprs :
    nat PTree.t -> ExpCintptr.expr list -> nat -> statement list optErr **)

let rec stacked_exprs temps es s_extra =
  match es with
  | Coq_nil -> ret (Obj.magic coq_Monad_optErr) Coq_nil
  | Coq_cons (e, rest) ->
    bind (Obj.magic coq_Monad_optErr)
      (stacked_expr temps e (add (length rest) s_extra) Coq_true) (fun e' ->
      bind (Obj.magic coq_Monad_optErr) (stacked_exprs temps rest s_extra)
        (fun rest' -> ret (Obj.magic coq_Monad_optErr) (app rest' e')))

(** val ident_indices :
    nat PTree.t -> ident list -> nat -> nat list optErr **)

let rec ident_indices temps a offset =
  match a with
  | Coq_nil -> ret (Obj.magic coq_Monad_optErr) Coq_nil
  | Coq_cons (i, rest) ->
    bind (Obj.magic coq_Monad_optErr)
      (fromOption (PTree.get i (Obj.magic temps)) (String ((Ascii (Coq_true,
        Coq_true, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true,
        Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_true, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
        Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false,
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
        Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
        (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
        Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
        ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_true, Coq_true, Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
        Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
        Coq_false)),
        EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))
      (fun t0 ->
      bind (Obj.magic coq_Monad_optErr) (ident_indices temps rest offset)
        (fun resti ->
        ret (Obj.magic coq_Monad_optErr) (Coq_cons
          ((add (add t0 offset) (length rest)), resti))))

(** val set_indices : nat list -> statement list **)

let set_indices inds =
  map (fun x -> Sset x) inds

(** val optident : nat PTree.t -> ident option -> statement optErr **)

let optident temps = function
| Some b ->
  bind (Obj.magic coq_Monad_optErr)
    (fromOption (PTree.get b (Obj.magic temps)) (String ((Ascii (Coq_true,
      Coq_true, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
      Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true,
      Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
      Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
      Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true,
      Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
      Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
      Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
      Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
      (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
      Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
      Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
      (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_false, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
      Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
      (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
      Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
      (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
      Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
      Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
      (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
      Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
      (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
      Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
      ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
      Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
      Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
      Coq_true, Coq_false)),
      EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))
    (fun t0 -> ret (Obj.magic coq_Monad_optErr) (Sset t0))
| None -> ret (Obj.magic coq_Monad_optErr) Spop

(** val return_type : StmtClinear.coq_function -> coq_type **)

let return_type =
  fn_return

(** val toreturn : StmtClinear.coq_function -> bool option -> ret_type **)

let toreturn f = function
| Some b ->
  (match b with
   | Coq_true ->
     (match return_type f with
      | Tvoid -> Tvoid_method
      | Tarray (_, _) -> Terror
      | Tstruct (_, _) -> Terror
      | _ -> Tsome_method)
   | Coq_false ->
     (match return_type f with
      | Tvoid -> Tvoid_fun
      | Tarray (_, _) -> Terror
      | Tstruct (_, _) -> Terror
      | _ -> Tsome_fun))
| None ->
  (match return_type f with
   | Tvoid -> Tvoid_constructor
   | Tarray (_, _) -> Terror
   | Tstruct (_, _) -> Terror
   | _ -> Tsome_constructor)

(** val zero_stm : statement **)

let zero_stm =
  Srvalue (Econst_int256 Int256.zero)

(** val xzero_stms : nat -> statement list **)

let rec xzero_stms = function
| O -> Coq_nil
| S n -> app (xzero_stms n) (Coq_cons (zero_stm, Coq_nil))

(** val zero_stms : nat -> statement list **)

let zero_stms c = match c with
| O -> Coq_cons (Sskip, Coq_nil)
| S _ -> xzero_stms c

(** val z_stm : coq_Z -> statement **)

let z_stm z =
  Srvalue (Econst_int256 (Int256.repr z))

(** val stacked_code :
    nat PTree.t -> StmtClinear.coq_function -> StmtClinear.code -> code optErr **)

let rec stacked_code temps f = function
| Coq_nil -> ret (Obj.magic coq_Monad_optErr) Coq_nil
| Coq_cons (s, rest) ->
  bind (Obj.magic coq_Monad_optErr) (stacked_code temps f rest) (fun trest ->
    match s with
    | StmtClinear.Ssassign (lv, rv) ->
      bind (Obj.magic coq_Monad_optErr)
        (stacked_expr temps lv (S O) Coq_false) (fun lv' ->
        bind (Obj.magic coq_Monad_optErr) (stacked_expr temps rv O Coq_true)
          (fun rv' ->
          ret (Obj.magic coq_Monad_optErr)
            (app rv' (app lv' (Coq_cons (Ssassign, trest))))))
    | StmtClinear.Smassign (lv, rv) ->
      bind (Obj.magic coq_Monad_optErr)
        (stacked_expr temps lv (S O) Coq_false) (fun lv' ->
        bind (Obj.magic coq_Monad_optErr) (stacked_expr temps rv O Coq_true)
          (fun rv' ->
          ret (Obj.magic coq_Monad_optErr)
            (app rv' (app lv' (Coq_cons (Smassign, trest))))))
    | StmtClinear.Sset (i, rv) ->
      bind (Obj.magic coq_Monad_optErr)
        (fromOption (PTree.get i (Obj.magic temps)) (String ((Ascii
          (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false, Coq_true,
          Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
          Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
          (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
          Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
          (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
          Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
          Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
          (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
          Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
          (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
          Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
          Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
          (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false,
          Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
          (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
          Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
          Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
          (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true,
          Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
          (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
          Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
          Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
          (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
          Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
          (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
          Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
          Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
          (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
          Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
          (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
          Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
          Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
          (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true,
          Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
          (Coq_false, Coq_true, Coq_true, Coq_false, Coq_false, Coq_true,
          Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
          Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
          (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
          Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
          (Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
          Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
          Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
          (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false,
          Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
          (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
          Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
          Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
          (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
          Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
          (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
          Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
          Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
          EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
        (fun t0 ->
        bind (Obj.magic coq_Monad_optErr) (stacked_expr temps rv O Coq_true)
          (fun rv' ->
          ret (Obj.magic coq_Monad_optErr)
            (app rv' (Coq_cons ((Sset t0), trest)))))
    | Scall (rv, dest, args, retlbl) ->
      bind (Obj.magic coq_Monad_optErr) (Obj.magic optident temps rv)
        (fun t0 ->
        bind (Obj.magic coq_Monad_optErr) (stacked_exprs temps args (S O))
          (fun args' ->
          ret (Obj.magic coq_Monad_optErr)
            (app (Coq_cons ((Spushlabel (Lreturn retlbl)), Coq_nil))
              (app args'
                (app (Coq_cons ((Spushlabel (Lcall dest)), (Coq_cons
                  (Sjump_call, (Coq_cons ((Slabel retlbl), Coq_nil))))))
                  (app (Coq_cons (t0, Coq_nil)) trest))))))
    | Sreturn rv ->
      (match rv with
       | Some i ->
         bind (Obj.magic coq_Monad_optErr)
           (stacked_expr temps (ExpCintptr.Etempvar (i, (return_type f))) O
             Coq_true) (fun rv' ->
           ret (Obj.magic coq_Monad_optErr) (app rv' trest))
       | None ->
         ret (Obj.magic coq_Monad_optErr) (Coq_cons (Spushvoid, trest)))
    | StmtClinear.Sdone method0 ->
      ret (Obj.magic coq_Monad_optErr) (Coq_cons ((Sdone
        ((length (all_temps ftype (Obj.magic f))), (toreturn f method0))),
        trest))
    | StmtClinear.Slabel lbl ->
      ret (Obj.magic coq_Monad_optErr) (Coq_cons ((Slabel lbl), trest))
    | Sjump lbl ->
      ret (Obj.magic coq_Monad_optErr) (Coq_cons ((Spushlabel (Linternal
        lbl)), (Coq_cons (Sjump_internal, trest))))
    | StmtClinear.Sjumpi (cond, lbl) ->
      bind (Obj.magic coq_Monad_optErr) (stacked_expr temps cond O Coq_true)
        (fun cond' ->
        ret (Obj.magic coq_Monad_optErr)
          (app cond' (Coq_cons ((Spushlabel (Linternal lbl)), (Coq_cons
            (Sjumpi, trest))))))
    | StmtClinear.Shash (ex1, ex2, exo) ->
      bind (Obj.magic coq_Monad_optErr) (stacked_expr temps ex1 O Coq_true)
        (fun ex1' ->
        bind (Obj.magic coq_Monad_optErr) (stacked_expr temps ex2 O Coq_true)
          (fun ex2' ->
          bind (Obj.magic coq_Monad_optErr)
            (match exo with
             | Some ex -> stacked_expr temps ex O Coq_true
             | None -> ret (Obj.magic coq_Monad_optErr) ex2') (fun exo' ->
            bind (Obj.magic coq_Monad_optErr)
              (match exo with
               | Some _ ->
                 Success (Coq_cons ((Srvalue (Econst_int256
                   (Int256.repr (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
                     (Coq_xO Coq_xH)))))))))), Coq_nil))
               | None ->
                 Success (Coq_cons ((Srvalue (Econst_int256
                   (Int256.repr (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
                     Coq_xH))))))))), Coq_nil))) (fun len ->
              ret (Obj.magic coq_Monad_optErr)
                (app ex2'
                  (app (Coq_cons ((Srvalue Emload), (Coq_cons ((Srvalue
                    (Econst_int256
                    (Int256.repr (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO
                      (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))))))))),
                    (Coq_cons (Smassign, Coq_nil))))))
                    (app
                      (match exo with
                       | Some _ ->
                         app exo' (Coq_cons ((Srvalue Emload), (Coq_cons
                           ((Srvalue (Econst_int256
                           (Int256.repr
                             (Z.add (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO
                               (Coq_xO (Coq_xO (Coq_xO (Coq_xO
                               Coq_xH))))))))) (Zpos (Coq_xO (Coq_xO (Coq_xO
                               (Coq_xO (Coq_xO Coq_xH)))))))))), (Coq_cons
                           (Smassign, Coq_nil))))))
                       | None -> Coq_nil)
                      (app len
                        (app (Coq_cons ((Srvalue (Econst_int256
                          (Int256.repr (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO
                            (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))))))))),
                          Coq_nil))
                          (app (Coq_cons (Shash, Coq_nil))
                            (app ex1'
                              (app (Coq_cons (Smassign, Coq_nil)) trest))))))))))))
    | StmtClinear.Stransfer (a, v, fail) ->
      bind (Obj.magic coq_Monad_optErr)
        (stacked_expr temps a (S (S (S (S (S O))))) Coq_true) (fun a' ->
        bind (Obj.magic coq_Monad_optErr)
          (stacked_expr temps v (S (S (S (S O)))) Coq_true) (fun v' ->
          ret (Obj.magic coq_Monad_optErr)
            (app (Coq_cons (zero_stm, (Coq_cons (zero_stm, (Coq_cons
              (zero_stm, (Coq_cons (zero_stm, Coq_nil))))))))
              (app v'
                (app a'
                  (app (Coq_cons (zero_stm, (Coq_cons (Stransfer, (Coq_cons
                    ((Srvalue (Eunop Onotbool)), (Coq_cons ((Spushlabel
                    (Linternal fail)), (Coq_cons (Sjumpi, Coq_nil))))))))))
                    trest))))))
    | StmtClinear.Scallmethod (a, rvs, sg, v, args, fail) ->
      bind (Obj.magic coq_Monad_optErr)
        (stacked_expr temps a (S (S (S (S (S O))))) Coq_true) (fun a' ->
        bind (Obj.magic coq_Monad_optErr)
          (stacked_expr temps v (S (S (S (S O)))) Coq_true) (fun v' ->
          bind (Obj.magic coq_Monad_optErr)
            (stacked_exprs temps args (S (S (S (S (S (S (S O))))))))
            (fun args' ->
            bind (Obj.magic coq_Monad_optErr)
              (Obj.magic ident_indices temps rvs (S O)) (fun rv_inds ->
              let retcount = length rvs in
              let argcount = length args in
              ret (Obj.magic coq_Monad_optErr)
                (app (Coq_cons ((z_stm (arglen argcount)), (Coq_cons
                  ((z_stm (argpos retcount)), (Coq_cons
                  ((z_stm (retlen retcount)), (Coq_cons ((z_stm retpos),
                  Coq_nil))))))))
                  (app v'
                    (app a' (Coq_cons (zero_stm,
                      (app args' (Coq_cons ((Scallmethod (sg, argcount,
                        retcount)),
                        (app (set_indices rv_inds)
                          (app (Coq_cons ((Srvalue (Eunop Onotbool)),
                            (Coq_cons ((Spushlabel (Linternal fail)),
                            (Coq_cons (Sjumpi, Coq_nil)))))) trest))))))))))))))
    | StmtClinear.Slog (topics, args) ->
      bind (Obj.magic coq_Monad_optErr) (stacked_exprs temps topics O)
        (fun topics' ->
        bind (Obj.magic coq_Monad_optErr)
          (stacked_exprs temps args (length topics)) (fun args' ->
          ret (Obj.magic coq_Monad_optErr)
            (app topics'
              (app args' (Coq_cons ((Slog ((length topics), (length args))),
                trest))))))
    | StmtClinear.Srevert ->
      ret (Obj.magic coq_Monad_optErr) (Coq_cons (Srevert, trest))
    | StmtClinear.Sfetchargs fetch ->
      ret (Obj.magic coq_Monad_optErr) (Coq_cons
        ((match fetch with
          | Coq_true -> Sfetchargs (length (some_args ftype (Obj.magic f)))
          | Coq_false -> Sskip), trest))
    | Sintro ->
      let push_temps = zero_stms (length (some_temps ftype (Obj.magic f))) in
      ret (Obj.magic coq_Monad_optErr) (app push_temps trest))

(** val allocate_temps : (ident, coq_type) prod list -> nat -> nat PTree.t **)

let rec allocate_temps ids base =
  match ids with
  | Coq_nil -> PTree.empty
  | Coq_cons (p, rest) ->
    let Coq_pair (id, _) = p in
    PTree.set id base (allocate_temps rest (S base))

(** val allocate_all_temps : (ident, coq_type) prod list -> nat PTree.t **)

let allocate_all_temps ids =
  allocate_temps ids O

(** val allocate_fn_temps : StmtClinear.coq_function -> nat PTree.t **)

let allocate_fn_temps f =
  allocate_all_temps (all_temps ftype (Obj.magic f))

(** val stacked_function :
    StmtClinear.coq_function -> StmtStacked.coq_function optErr **)

let stacked_function f =
  let temps = allocate_fn_temps f in
  bind (Obj.magic coq_Monad_optErr)
    (Obj.magic stacked_code temps f f.StmtClinear.fn_code) (fun c ->
    ret (Obj.magic coq_Monad_optErr) c)

(** val stacked_fundef :
    StmtClinear.coq_function -> StmtStacked.coq_function optErr **)

let stacked_fundef =
  stacked_function

(** val stacked_fundefs :
    StmtClinear.coq_function PTree.t -> StmtStacked.coq_function PTree.t
    optErr **)

let stacked_fundefs t0 =
  transl_tree stacked_fundef t0

(** val stacked_methods :
    StmtClinear.coq_function option IntMap.t -> StmtStacked.coq_function
    option IntMap.t optErr **)

let stacked_methods methods =
  transl_map stacked_fundef methods

(** val stacked_constructor :
    StmtClinear.coq_function option -> StmtStacked.coq_function optErr **)

let stacked_constructor = function
| Some c ->
  bind (Obj.magic coq_Monad_optErr) (stacked_fundef c) (fun f ->
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

(** val stacked_genv : StmtClinear.genv -> genv optErr **)

let stacked_genv ge =
  let vars = ge.Genv.genv_vars in
  let funcs = ge.Genv.genv_funcs in
  let methods = ge.Genv.genv_methods in
  let defs = ge.Genv.genv_defs in
  let fundefs = ge.Genv.genv_fundefs in
  let methoddefs = ge.Genv.genv_methoddefs in
  let constructor = ge.Genv.genv_constructor in
  bind (Obj.magic coq_Monad_optErr) (Obj.magic stacked_fundefs fundefs)
    (fun fundefs0 ->
    bind (Obj.magic coq_Monad_optErr) (Obj.magic stacked_methods methoddefs)
      (fun methoddefs0 ->
      bind (Obj.magic coq_Monad_optErr)
        (Obj.magic stacked_constructor constructor) (fun constructor0 ->
        ret (Obj.magic coq_Monad_optErr) { Genv.genv_vars = vars;
          Genv.genv_funcs = funcs; Genv.genv_methods = methods;
          Genv.genv_defs = defs; Genv.genv_fundefs = fundefs0;
          Genv.genv_methoddefs = methoddefs0; Genv.genv_constructor = (Some
          constructor0) })))

(** val stacked_program : StmtClinear.program -> program optErr **)

let stacked_program = function
| Coq_pair (ge, body) ->
  bind (Obj.magic coq_Monad_optErr) (Obj.magic stacked_genv ge) (fun cge ->
    match label_functions cge.Genv.genv_fundefs with
    | Coq_true ->
      (match label_methods cge.Genv.genv_methoddefs with
       | Coq_true -> ret (Obj.magic coq_Monad_optErr) (Coq_pair (cge, body))
       | Coq_false ->
         Error (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
           Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
           Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
           Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true,
           Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_true,
           Coq_false, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
           Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
           Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
           Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
           ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
           Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
           Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
           Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
           ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_false,
           Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
           Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
           Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
           ((Ascii (Coq_false, Coq_true, Coq_true, Coq_false, Coq_false,
           Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
           Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
           Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
           Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
           ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false,
           Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
           Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
           Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
           Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
           EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
    | Coq_false ->
      Error (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true,
        Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
        Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true,
        Coq_true, Coq_false, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_true, Coq_true, Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
        Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
        Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true,
        Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
        Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
        Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_false,
        Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
        ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
        Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_true, Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
        Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
        Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
        Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
        Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
        ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
        Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
        Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
        EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
