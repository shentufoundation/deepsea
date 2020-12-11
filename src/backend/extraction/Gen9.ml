open AST
open Ascii
open BinNums
open BinPos
open Cop
open Ctypes
open Datatypes
open ExpCintptr
open GlobalenvCompile
open Globalenvs
open Int0
open Integers
open Language0
open List0
open Maps0
open Monad
open Nat0
open OptErrMonad
open Options
open PeanoNat
open StackEnv
open StmtCintptr
open String0
open Structure
open Trees
open Values

let __ = let rec f _ = Obj.repr f in Obj.repr f

(** val wasm_expr :
    nat PTree.t -> ExpCintptr.expr -> bool -> instr list optErr **)

let rec wasm_expr temps e rvalue =
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
  | Econst_int256 (i, _) ->
    ret (Obj.magic coq_Monad_optErr) (Coq_cons ((Const_256 i), Coq_nil))
  | Etempvar (i, _) ->
    (match rvalue with
     | Coq_true ->
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
           (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_true,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
           Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_false,
           Coq_true, Coq_false, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
           Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_true,
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
           Coq_true, Coq_true, Coq_true, Coq_false)),
           EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
         (fun ind ->
         ret (Obj.magic coq_Monad_optErr) (Coq_cons ((Local_get ind),
           Coq_nil)))
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
         ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
         Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
         Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
         Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_false,
         Coq_false)),
         EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
  | Esload (e0, _) ->
    bind (Obj.magic coq_Monad_optErr) (wasm_expr temps e0 Coq_false)
      (fun e' ->
      ret (Obj.magic coq_Monad_optErr)
        (app e'
          (app
            (set_eeiOffset Coq_nil Coq_pathOffset (S (S (S (S (S (S (S (S
              O)))))))))
            (app (Coq_cons ((Const_nat
              (get_idx_eei_mem_offset Coq_pathOffset)), (Coq_cons ((Const_nat
              (get_idx_eei_mem_offset Coq_resultOffset)), (Coq_cons ((Call
              (get_idx_eei Coq_eei_storageLoad)), Coq_nil))))))
              (load_resultOffset (S O))))))
  | Emload (e0, t0) ->
    bind (Obj.magic coq_Monad_optErr) (wasm_expr temps e0 Coq_false)
      (fun e' ->
      bind (Obj.magic coq_Monad_optErr)
        (match t0 with
         | Tpointer (p, _) ->
           (match p with
            | Coq_stor ->
              Success (Coq_cons ((Const_nat (S (S (S (S (S (S (S (S (S (S (S
                (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
                (S O))))))))))))))))))))))))))))))))), (Coq_cons ((Binop
                (Coq_i32 IOp32.Add)), (Coq_cons ((Global_set
                scratch_global_idx2),
                (app Coq_nil
                  (load_len_hash (S (S (S (S (S (S (S (S O))))))))))))))))
            | _ -> Success (Coq_cons ((Memop (Coq_i32 IOp32.Load)), Coq_nil)))
         | _ -> Success (Coq_cons ((Memop (Coq_i32 IOp32.Load)), Coq_nil)))
        (fun l -> ret (Obj.magic coq_Monad_optErr) (app e' l)))
  | Eaddr (e0, _) ->
    (match rvalue with
     | Coq_true ->
       bind (Obj.magic coq_Monad_optErr) (wasm_expr temps e0 Coq_false)
         (fun e' -> ret (Obj.magic coq_Monad_optErr) e')
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
  | Eunop (o, e0, _) ->
    (match rvalue with
     | Coq_true ->
       bind (Obj.magic coq_Monad_optErr) (wasm_expr temps e0 Coq_true)
         (fun e' ->
         bind (Obj.magic coq_Monad_optErr)
           (match o with
            | Onotbool -> Success (Coq_cons ((Testop (Coq_i32 __)), Coq_nil))
            | Onotint ->
              Success (Coq_cons ((Call (get_idx_aux Coq_aux_notint)),
                Coq_nil))
            | Oneg ->
              Success (Coq_cons ((Const (Coq_i32 I32.zero)), (Coq_cons
                ((Binop (Coq_i32 IOp32.Sub)), Coq_nil))))
            | Osha_1 ->
              Error (String ((Ascii (Coq_false, Coq_false, Coq_false,
                Coq_true, Coq_false, Coq_false, Coq_true, Coq_false)),
                (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false,
                Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
                (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true,
                Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
                Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
                Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
                Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
                Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
                Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
                ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
                Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
                (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true,
                Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
                Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
                Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
                Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
                Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
                Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
                ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false,
                Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
                Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
                Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
                Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
                Coq_false)), (String ((Ascii (Coq_false, Coq_false,
                Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
                Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
                Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
                (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_false,
                Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
                (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
                Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
                Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
                Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
                Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
                Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
                Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
                (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
                Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
                (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
                Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
                Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
                Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
                Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
                Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
                Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
                ((Ascii (Coq_true, Coq_false, Coq_true, Coq_true, Coq_false,
                Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
                Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
                Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
                Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
                Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
                Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
                ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
                Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
                Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
                Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
                Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
                Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
                Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
                (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_false,
                Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
                (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
                Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
                Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_true,
                Coq_false)), (String ((Ascii (Coq_false, Coq_false,
                Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
                Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_false,
                Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
                (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
                Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
                (Coq_false, Coq_true, Coq_true, Coq_false, Coq_false,
                Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
                Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
                Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_false,
                Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
                ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
                Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
                Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
                Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
                Coq_true, Coq_false, Coq_true, Coq_false, Coq_true,
                Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
                Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
                (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
                Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
                (Coq_true, Coq_false, Coq_true, Coq_true, Coq_false,
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
                EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
           (fun o' -> ret (Obj.magic coq_Monad_optErr) (app e' o')))
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
         Coq_true, Coq_true, Coq_false)),
         EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
  | Ebinop (o, e1, e2, _) ->
    bind (Obj.magic coq_Monad_optErr) (wasm_expr temps e1 rvalue) (fun e1' ->
      bind (Obj.magic coq_Monad_optErr) (wasm_expr temps e2 Coq_true)
        (fun e2' ->
        bind (Obj.magic coq_Monad_optErr)
          (match o with
           | Oadd -> Success (Coq_cons ((Binop (Coq_i32 IOp32.Add)), Coq_nil))
           | Osub -> Success (Coq_cons ((Binop (Coq_i32 IOp32.Sub)), Coq_nil))
           | Omul -> Success (Coq_cons ((Binop (Coq_i32 IOp32.Mul)), Coq_nil))
           | Odiv ->
             Success (Coq_cons ((Binop (Coq_i32 (IOp32.Div Unsigned))),
               Coq_nil))
           | Omod ->
             Success (Coq_cons ((Binop (Coq_i32 (IOp32.Rem Unsigned))),
               Coq_nil))
           | Oexp ->
             Success (Coq_cons ((Call (get_idx_aux Coq_aux_pow)), Coq_nil))
           | Oand -> Success (Coq_cons ((Binop (Coq_i32 IOp32.And)), Coq_nil))
           | Oor -> Success (Coq_cons ((Binop (Coq_i32 IOp32.Or)), Coq_nil))
           | Oxor -> Success (Coq_cons ((Binop (Coq_i32 IOp32.Xor)), Coq_nil))
           | Oshl -> Success (Coq_cons ((Binop (Coq_i32 IOp32.Shl)), Coq_nil))
           | Oshr ->
             Success (Coq_cons ((Binop (Coq_i32 (IOp32.Shr Unsigned))),
               Coq_nil))
           | Oeq -> Success (Coq_cons ((Relop (Coq_i32 IOp32.Eq)), Coq_nil))
           | One -> Success (Coq_cons ((Relop (Coq_i32 IOp32.Ne)), Coq_nil))
           | Olt ->
             Success (Coq_cons ((Relop (Coq_i32 (IOp32.Lt Unsigned))),
               Coq_nil))
           | Ogt ->
             Success (Coq_cons ((Relop (Coq_i32 (IOp32.Gt Unsigned))),
               Coq_nil))
           | Ole ->
             Success (Coq_cons ((Relop (Coq_i32 (IOp32.Le Unsigned))),
               Coq_nil))
           | Oge ->
             Success (Coq_cons ((Relop (Coq_i32 (IOp32.Ge Unsigned))),
               Coq_nil))
           | Osha_2 ->
             Error (String ((Ascii (Coq_false, Coq_false, Coq_false,
               Coq_true, Coq_false, Coq_false, Coq_true, Coq_false)), (String
               ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
               Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
               Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
               Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
               Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
               ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
               Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
               Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
               Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
               Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
               (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
               Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
               (Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
               Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
               Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
               Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
               Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
               ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false,
               Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
               Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
               Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
               Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
               (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_true,
               Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
               (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
               Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
               Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
               Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
               Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
               ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
               Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
               (Coq_false, Coq_true, Coq_false, Coq_false, Coq_false,
               Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
               Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
               Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
               Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
               ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
               Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
               Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
               Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
               Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
               Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
               Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
               ((Ascii (Coq_true, Coq_false, Coq_true, Coq_true, Coq_false,
               Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
               Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
               Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
               Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
               ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
               Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
               Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
               Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
               Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
               ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
               Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
               (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
               Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
               Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
               Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
               Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
               ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
               Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
               Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
               Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
               Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
               Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
               Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
               ((Ascii (Coq_false, Coq_true, Coq_true, Coq_false, Coq_false,
               Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
               Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
               Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_false,
               Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
               ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
               Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
               Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
               Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
               Coq_true, Coq_false, Coq_true, Coq_false, Coq_true,
               Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
               Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
               ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true,
               Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
               Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
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
               EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
          (fun o' -> ret (Obj.magic coq_Monad_optErr) (app e1' (app e2' o')))))
  | Ecall0 (b, _) ->
    (match rvalue with
     | Coq_true ->
       bind (Obj.magic coq_Monad_optErr) (wasm_builtin0 b) (fun b' ->
         ret (Obj.magic coq_Monad_optErr) b')
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
  | Ecall1 (b, e0, _) ->
    (match rvalue with
     | Coq_true ->
       bind (Obj.magic coq_Monad_optErr) (wasm_expr temps e0 Coq_true)
         (fun e' ->
         bind (Obj.magic coq_Monad_optErr) (Success (wasm_builtin1 b))
           (fun b' -> ret (Obj.magic coq_Monad_optErr) (app e' b')))
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

(** val wasm_exprs :
    nat PTree.t -> ExpCintptr.expr list -> instr list optErr **)

let rec wasm_exprs temps = function
| Coq_nil -> ret (Obj.magic coq_Monad_optErr) Coq_nil
| Coq_cons (e, rest) ->
  bind (Obj.magic coq_Monad_optErr) (wasm_expr temps e Coq_true) (fun e' ->
    bind (Obj.magic coq_Monad_optErr) (wasm_exprs temps rest) (fun rest' ->
      ret (Obj.magic coq_Monad_optErr) (app rest' e')))

(** val optident : nat PTree.t -> ident option -> instrs optErr **)

let optident temps = function
| Some b ->
  (match PTree.get b temps with
   | Some v ->
     ret (Obj.magic coq_Monad_optErr) (Coq_cons ((Local_set v), Coq_nil))
   | None ->
     Error (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
       Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
       Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
       Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
       Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
       (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
       Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
       Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
       (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
       Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
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
       Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
       Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
       Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
       Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
       ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
       Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
       Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
       Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
       Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
       (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
       Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
       Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
       (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
       Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
       Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
       (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
       Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
       Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
       (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
       Coq_true, Coq_true, Coq_false)),
       EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))
| None -> ret (Obj.magic coq_Monad_optErr) (Coq_cons (Drop, Coq_nil))

(** val wasm_statement :
    (positive, nat) prod list -> nat PTree.t -> coq_type -> statement -> nat
    -> instrs optErr **)

let rec wasm_statement funident_map temps return_type s cstack_depth =
  match s with
  | Sskip -> ret (Obj.magic coq_Monad_optErr) (Coq_cons (Nop, Coq_nil))
  | Ssassign (lv, rv) ->
    bind (Obj.magic coq_Monad_optErr) (wasm_expr temps lv Coq_false)
      (fun lv' ->
      bind (Obj.magic coq_Monad_optErr) (wasm_expr temps rv Coq_true)
        (fun rv' ->
        ret (Obj.magic coq_Monad_optErr)
          (app lv'
            (app
              (set_eeiOffset Coq_nil Coq_pathOffset (S (S (S (S (S (S (S (S
                O)))))))))
              (app (set_eeiOffset rv' Coq_valueOffset (S O)) (Coq_cons
                ((Const_nat (get_idx_eei_mem_offset Coq_pathOffset)),
                (Coq_cons ((Const_nat
                (get_idx_eei_mem_offset Coq_valueOffset)), (Coq_cons ((Call
                (get_idx_eei Coq_eei_storageStore)), Coq_nil)))))))))))
  | Smassign (lv, rv) ->
    bind (Obj.magic coq_Monad_optErr) (wasm_expr temps lv Coq_false)
      (fun lv' ->
      bind (Obj.magic coq_Monad_optErr) (wasm_expr temps rv Coq_true)
        (fun rv' ->
        match typeof lv with
        | Tpointer (_, _) ->
          (match rv with
           | Econst_int (_, _) ->
             ret (Obj.magic coq_Monad_optErr)
               (app lv'
                 (app rv' (Coq_cons ((Memop (Coq_i32 IOp32.Store)), Coq_nil))))
           | Econst_int256 (_, _) ->
             ret (Obj.magic coq_Monad_optErr)
               (app lv'
                 (app rv' (Coq_cons ((Memop (Coq_i32 IOp32.Store)), Coq_nil))))
           | Etempvar (_, _) ->
             ret (Obj.magic coq_Monad_optErr)
               (app lv'
                 (app rv' (Coq_cons ((Memop (Coq_i32 IOp32.Store)), Coq_nil))))
           | Esload (_, _) ->
             ret (Obj.magic coq_Monad_optErr)
               (app lv'
                 (app rv' (Coq_cons ((Memop (Coq_i32 IOp32.Store)), Coq_nil))))
           | Emload (_, t0) ->
             (match t0 with
              | Tpointer (mem, _) ->
                (match mem with
                 | Coq_mem ->
                   ret (Obj.magic coq_Monad_optErr)
                     (app lv'
                       (app rv' (Coq_cons ((Memop (Coq_i32 IOp32.Store)),
                         Coq_nil))))
                 | Coq_stor ->
                   ret (Obj.magic coq_Monad_optErr)
                     (app rv'
                       (store_len lv' (S (S (S (S (S (S (S (S O))))))))))
                 | Coq_call ->
                   ret (Obj.magic coq_Monad_optErr)
                     (app lv'
                       (app rv' (Coq_cons ((Memop (Coq_i32 IOp32.Store)),
                         Coq_nil)))))
              | _ ->
                Error (String ((Ascii (Coq_false, Coq_true, Coq_false,
                  Coq_false, Coq_false, Coq_false, Coq_true, Coq_false)),
                  (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false,
                  Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
                  (Coq_false, Coq_false, Coq_true, Coq_false, Coq_false,
                  Coq_true, Coq_true, Coq_false)), (String ((Ascii
                  (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
                  Coq_true, Coq_false, Coq_false)), (String ((Ascii
                  (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
                  Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
                  Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
                  Coq_true, Coq_false)), (String ((Ascii (Coq_false,
                  Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
                  Coq_true, Coq_false)), (String ((Ascii (Coq_true,
                  Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
                  Coq_true, Coq_false)), (String ((Ascii (Coq_false,
                  Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
                  Coq_false, Coq_false)), (String ((Ascii (Coq_true,
                  Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
                  Coq_true, Coq_false)), (String ((Ascii (Coq_false,
                  Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
                  Coq_true, Coq_false)), (String ((Ascii (Coq_false,
                  Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
                  Coq_false, Coq_false)), (String ((Ascii (Coq_true,
                  Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
                  Coq_true, Coq_false)), (String ((Ascii (Coq_true,
                  Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
                  Coq_true, Coq_false)), (String ((Ascii (Coq_true,
                  Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
                  Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
                  Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
                  Coq_false)), (String ((Ascii (Coq_true, Coq_true,
                  Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
                  Coq_false)), (String ((Ascii (Coq_true, Coq_false,
                  Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
                  Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
                  Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
                  (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true,
                  Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
                  (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
                  Coq_true, Coq_false, Coq_false)), (String ((Ascii
                  (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
                  Coq_true, Coq_true, Coq_false)), (String ((Ascii
                  (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
                  Coq_true, Coq_true, Coq_false)), (String ((Ascii
                  (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
                  Coq_true, Coq_false, Coq_false)), (String ((Ascii
                  (Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
                  Coq_false, Coq_true, Coq_false)), (String ((Ascii
                  (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
                  Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
                  Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
                  Coq_true, Coq_false)), (String ((Ascii (Coq_true,
                  Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
                  Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
                  Coq_true, Coq_true, Coq_false, Coq_true, Coq_false,
                  Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
                  Coq_false, Coq_false, Coq_false, Coq_true, Coq_false)),
                  (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
                  Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
                  (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
                  Coq_true, Coq_true, Coq_false)), (String ((Ascii
                  (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
                  Coq_true, Coq_false, Coq_false)), (String ((Ascii
                  (Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
                  Coq_true, Coq_true, Coq_false)),
                  EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
           | Eaddr (_, _) ->
             ret (Obj.magic coq_Monad_optErr)
               (app lv'
                 (app rv' (Coq_cons ((Memop (Coq_i32 IOp32.Store)), Coq_nil))))
           | Eunop (_, _, _) ->
             ret (Obj.magic coq_Monad_optErr)
               (app lv'
                 (app rv' (Coq_cons ((Memop (Coq_i32 IOp32.Store)), Coq_nil))))
           | Ebinop (_, _, _, _) ->
             ret (Obj.magic coq_Monad_optErr)
               (app lv'
                 (app rv' (Coq_cons ((Memop (Coq_i32 IOp32.Store)), Coq_nil))))
           | Ecall0 (_, _) ->
             ret (Obj.magic coq_Monad_optErr)
               (app lv'
                 (app rv' (Coq_cons ((Memop (Coq_i32 IOp32.Store)), Coq_nil))))
           | Ecall1 (_, _, _) ->
             ret (Obj.magic coq_Monad_optErr)
               (app lv'
                 (app rv' (Coq_cons ((Memop (Coq_i32 IOp32.Store)), Coq_nil)))))
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
            Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
            Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
            Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
            Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
            ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
            Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
            Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
            Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
            Coq_false, Coq_false, Coq_false, Coq_true, Coq_false)), (String
            ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_false,
            Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
            Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
            Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_false,
            Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
            ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
            Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
            Coq_true, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
            Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
            Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
            ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
            Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
            Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
            Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
            Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
            ((Ascii (Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
            Coq_false, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
            Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
            Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
            Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
            ((Ascii (Coq_true, Coq_false, Coq_true, Coq_true, Coq_false,
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
            Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
            Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_false,
            Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
            Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
            ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true,
            Coq_false, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
            Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
            Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
            Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
            ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true,
            Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
            Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
            Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
            Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
            ((Ascii (Coq_true, Coq_true, Coq_true, Coq_false, Coq_false,
            Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
            Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
            Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
            Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
            ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
            Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
            Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
            Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
            Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
            ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true,
            Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
            Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
            Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
            Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
            ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
            Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
            Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
            Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
            Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
            ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
            Coq_true, Coq_true, Coq_false)),
            EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
  | Sset (id, rv) ->
    bind (Obj.magic coq_Monad_optErr) (wasm_expr temps rv Coq_true)
      (fun rv' ->
      bind (Obj.magic coq_Monad_optErr)
        (match rv with
         | Emload (_, t0) ->
           (match t0 with
            | Tpointer (p, _) ->
              (match p with
               | Coq_stor ->
                 Success (Coq_cons (Drop, (Coq_cons (Drop, (Coq_cons (Drop,
                   (Coq_cons (Drop, (Coq_cons (Drop, (Coq_cons (Drop,
                   (Coq_cons (Drop, Coq_nil))))))))))))))
               | _ -> Success Coq_nil)
            | _ -> Success Coq_nil)
         | _ -> Success Coq_nil) (fun d ->
        let set_lookup =
          match PTree.get id temps with
          | Some v -> Coq_cons ((Local_set v), Coq_nil)
          | None -> Coq_nil
        in
        ret (Obj.magic coq_Monad_optErr) (app rv' (app d set_lookup))))
  | Scall (retval, lab, args) ->
    bind (Obj.magic coq_Monad_optErr) (optident temps retval) (fun t0 ->
      bind (Obj.magic coq_Monad_optErr) (wasm_exprs temps args) (fun args' ->
        let ido =
          fold_left (fun xs x ->
            match xs with
            | Some _ -> xs
            | None ->
              (match Pos.eqb (fst x) lab with
               | Coq_true -> Some (snd x)
               | Coq_false -> None)) funident_map None
        in
        (match ido with
         | Some id ->
           ret (Obj.magic coq_Monad_optErr)
             (app args' (app (Coq_cons ((Call id), Coq_nil)) t0))
         | None ->
           Error (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true,
             Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
             (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
             Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
             Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
             Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
             Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
             ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_false,
             Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
             Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
             Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
             Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
             ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
             Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
             Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
             Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
             Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
             ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
             Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
             Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
             Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_false,
             Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
             ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
             Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_false,
             Coq_true, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
             Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
             Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
             ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_true,
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
             Coq_true, Coq_true, Coq_false, Coq_true, Coq_false, Coq_true,
             Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
             Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
             ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true,
             Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
             Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
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
             Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
             Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
             ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true,
             Coq_false, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
             Coq_true, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
             Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
             Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
             ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false,
             Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
             Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
             Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
             Coq_true, Coq_false, Coq_true, Coq_false, Coq_false)),
             EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
  | Ssequence (s1, s2) ->
    bind (Obj.magic coq_Monad_optErr)
      (wasm_statement funident_map temps return_type s1 cstack_depth)
      (fun x1 ->
      bind (Obj.magic coq_Monad_optErr)
        (wasm_statement funident_map temps return_type s2 cstack_depth)
        (fun x2 -> ret (Obj.magic coq_Monad_optErr) (app x1 x2)))
  | Sifthenelse (c, strue, sfalse) ->
    bind (Obj.magic coq_Monad_optErr)
      (wasm_statement funident_map temps return_type sfalse
        (add cstack_depth (S O))) (fun nfalse ->
      bind (Obj.magic coq_Monad_optErr)
        (wasm_statement funident_map temps return_type strue
          (add cstack_depth (S O))) (fun ntrue ->
        bind (Obj.magic coq_Monad_optErr) (wasm_expr temps c Coq_true)
          (fun cond ->
          ret (Obj.magic coq_Monad_optErr)
            (app cond (Coq_cons ((If ((BT_valtype None), ntrue, nfalse)),
              Coq_nil))))))
  | Sloop sbody ->
    bind (Obj.magic coq_Monad_optErr)
      (wasm_statement funident_map temps return_type sbody
        (add cstack_depth (S O))) (fun n2 ->
      ret (Obj.magic coq_Monad_optErr) (Coq_cons ((Loop ((BT_valtype None),
        n2)), Coq_nil)))
  | Sbreak ->
    (match Nat.eqb cstack_depth O with
     | Coq_true ->
       Error (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true,
         Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
         (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
         Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
         Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
         (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true,
         Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
         (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
         Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
         Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
         (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
         Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
         (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
         Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
         Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
         (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false,
         Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
         (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
         Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
         Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
         (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false,
         Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
         (Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
         Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
         Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
         (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false,
         Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
         (Coq_true, Coq_true, Coq_false, Coq_true, Coq_false, Coq_true,
         Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
         Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
         (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false,
         Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
         (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
         Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
         Coq_true, Coq_true, Coq_false, Coq_true, Coq_false, Coq_false)),
         (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
         Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
         (Coq_false, Coq_true, Coq_false, Coq_false, Coq_false, Coq_true,
         Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
         Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
         (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
         Coq_true, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
         Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
         Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_false,
         Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
         Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
         Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
         Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_false,
         Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
         Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
         Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
         Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
         Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_true, Coq_true, Coq_true, Coq_false, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
         Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_false,
         Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
         ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
         Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
         Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
         Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
         Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
         Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
         Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_true, Coq_true, Coq_false, Coq_true, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
         Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
         Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_true, Coq_false, Coq_true, Coq_true, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
         Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
         Coq_true, Coq_true, Coq_false)),
         EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
     | Coq_false ->
       ret (Obj.magic coq_Monad_optErr) (Coq_cons ((Br cstack_depth),
         Coq_nil)))
  | Sreturn retval ->
    bind (Obj.magic coq_Monad_optErr)
      (match retval with
       | Some i -> wasm_expr temps (Etempvar (i, return_type)) Coq_true
       | None -> Success (Coq_cons ((Const_nat O), Coq_nil))) (fun rv' ->
      ret (Obj.magic coq_Monad_optErr) (app rv' (Coq_cons (Return, Coq_nil))))
  | Shash (ex1, ex2, exo) ->
    bind (Obj.magic coq_Monad_optErr) (wasm_expr temps ex1 Coq_true)
      (fun ex1' ->
      bind (Obj.magic coq_Monad_optErr) (wasm_expr temps ex2 Coq_true)
        (fun ex2' ->
        bind (Obj.magic coq_Monad_optErr)
          (match exo with
           | Some ex -> wasm_expr temps ex Coq_true
           | None -> ret (Obj.magic coq_Monad_optErr) ex2') (fun exo' ->
          ret (Obj.magic coq_Monad_optErr)
            (app ex2'
              (app (Coq_cons ((Memop (Coq_i32 IOp32.Load)), Coq_nil))
                (app exo'
                  (app (Coq_cons ((Memop (Coq_i32 IOp32.Load)), Coq_nil))
                    (app callExternalSha256TwoParam
                      (store_len ex1' (S (S (S (S (S (S (S (S O)))))))))))))))))
  | Stransfer (a, v) ->
    bind (Obj.magic coq_Monad_optErr) (wasm_expr temps a Coq_true) (fun a' ->
      bind (Obj.magic coq_Monad_optErr) (wasm_expr temps v Coq_true)
        (fun v' ->
        ret (Obj.magic coq_Monad_optErr)
          (app (set_eeiOffset a' Coq_addressOffset (S O))
            (app (set_eeiOffset v' Coq_valueOffset (S O))
              (app (set_eeiOffset Coq_nil Coq_dataOffset (S O)) (Coq_cons
                ((Const_nat default_gas_limit), (Coq_cons ((Cvtop (Coq_i32
                (IOp32.Extend_i32 Unsigned))), (Coq_cons ((Const_nat
                (get_idx_eei_mem_offset Coq_addressOffset)), (Coq_cons
                ((Const_nat (get_idx_eei_mem_offset Coq_valueOffset)),
                (Coq_cons ((Const_nat
                (get_idx_eei_mem_offset Coq_dataOffset)), (Coq_cons
                ((Const_nat O), (Coq_cons ((Call (get_idx_eei Coq_eei_call)),
                Coq_nil)))))))))))))))))))
  | Scallmethod (a, _, _, v, args) ->
    bind (Obj.magic coq_Monad_optErr) (wasm_expr temps a Coq_true) (fun a' ->
      bind (Obj.magic coq_Monad_optErr)
        (fold_left (fun xs x ->
          bind (Obj.magic coq_Monad_optErr) xs (fun xsl ->
            bind (Obj.magic coq_Monad_optErr) (wasm_expr temps x Coq_true)
              (fun rst -> ret (Obj.magic coq_Monad_optErr) (app xsl rst))))
          args (ret (Obj.magic coq_Monad_optErr) Coq_nil)) (fun _ ->
        bind (Obj.magic coq_Monad_optErr) (wasm_expr temps v Coq_true)
          (fun v' ->
          ret (Obj.magic coq_Monad_optErr)
            (app (set_eeiOffset a' Coq_addressOffset (S O))
              (app (set_eeiOffset v' Coq_valueOffset (S O))
                (app (set_eeiOffset Coq_nil Coq_dataOffset (S O)) (Coq_cons
                  ((Const_nat default_gas_limit), (Coq_cons ((Cvtop (Coq_i32
                  (IOp32.Extend_i32 Unsigned))), (Coq_cons ((Const_nat
                  (get_idx_eei_mem_offset Coq_addressOffset)), (Coq_cons
                  ((Const_nat (get_idx_eei_mem_offset Coq_valueOffset)),
                  (Coq_cons ((Const_nat
                  (get_idx_eei_mem_offset Coq_dataOffset)), (Coq_cons
                  ((Const_nat O), (Coq_cons ((Call
                  (get_idx_eei Coq_eei_call)), Coq_nil))))))))))))))))))))
  | Slog (topics, args) ->
    bind (Obj.magic coq_Monad_optErr)
      (fold_left (fun xs x ->
        bind (Obj.magic coq_Monad_optErr) xs (fun xsl ->
          bind (Obj.magic coq_Monad_optErr)
            (match x with
             | Econst_int256 (i, _) ->
               Success (Coq_cons ((Const_256
                 (Int256.shru
                   (Int256.shl i
                     (Int256.repr (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO
                       (Coq_xO (Coq_xI (Coq_xI Coq_xH))))))))))
                   (Int256.repr (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
                     (Coq_xI (Coq_xI Coq_xH))))))))))), (Coq_cons ((Const_256
                 (Int256.shru
                   (Int256.shl i
                     (Int256.repr (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO
                       (Coq_xO (Coq_xO (Coq_xI Coq_xH))))))))))
                   (Int256.repr (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
                     (Coq_xI (Coq_xI Coq_xH))))))))))), (Coq_cons ((Const_256
                 (Int256.shru
                   (Int256.shl i
                     (Int256.repr (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO
                       (Coq_xO (Coq_xI (Coq_xO Coq_xH))))))))))
                   (Int256.repr (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
                     (Coq_xI (Coq_xI Coq_xH))))))))))), (Coq_cons ((Const_256
                 (Int256.shru
                   (Int256.shl i
                     (Int256.repr (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO
                       (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))))))
                   (Int256.repr (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
                     (Coq_xI (Coq_xI Coq_xH))))))))))), (Coq_cons ((Const_256
                 (Int256.shru
                   (Int256.shl i
                     (Int256.repr (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO
                       (Coq_xO (Coq_xI Coq_xH)))))))))
                   (Int256.repr (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
                     (Coq_xI (Coq_xI Coq_xH))))))))))), (Coq_cons ((Const_256
                 (Int256.shru
                   (Int256.shl i
                     (Int256.repr (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO
                       (Coq_xO (Coq_xO Coq_xH)))))))))
                   (Int256.repr (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
                     (Coq_xI (Coq_xI Coq_xH))))))))))), (Coq_cons ((Const_256
                 (Int256.shru
                   (Int256.shl i
                     (Int256.repr (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO
                       (Coq_xO Coq_xH))))))))
                   (Int256.repr (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
                     (Coq_xI (Coq_xI Coq_xH))))))))))), (Coq_cons ((Const_256
                 (Int256.shru (Int256.shl i (Int256.repr Z0))
                   (Int256.repr (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
                     (Coq_xI (Coq_xI Coq_xH))))))))))),
                 Coq_nil))))))))))))))))
             | _ ->
               Error (String ((Ascii (Coq_false, Coq_true, Coq_true,
                 Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
                 (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true,
                 Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
                 (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
                 Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
                 Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
                 Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
                 Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
                 Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
                 Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
                 (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true,
                 Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
                 (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true,
                 Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
                 Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
                 Coq_true, Coq_false)), (String ((Ascii (Coq_false,
                 Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
                 Coq_false, Coq_false)), (String ((Ascii (Coq_true,
                 Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
                 Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
                 Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
                 Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
                 Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
                 (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true,
                 Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
                 (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
                 Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
                 Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
                 Coq_false, Coq_false)), (String ((Ascii (Coq_false,
                 Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
                 Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
                 Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
                 Coq_false)), (String ((Ascii (Coq_false, Coq_false,
                 Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
                 Coq_false)), (String ((Ascii (Coq_true, Coq_false,
                 Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
                 Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
                 Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
                 (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
                 Coq_true, Coq_true, Coq_true, Coq_false)),
                 EmptyString)))))))))))))))))))))))))))))))))))))))))))))
            (fun rst -> ret (Obj.magic coq_Monad_optErr) (app xsl rst))))
        topics (ret (Obj.magic coq_Monad_optErr) Coq_nil)) (fun topics' ->
      bind (Obj.magic coq_Monad_optErr)
        (fold_left (fun xs x ->
          bind (Obj.magic coq_Monad_optErr) xs (fun xsl ->
            bind (Obj.magic coq_Monad_optErr) (wasm_expr temps x Coq_true)
              (fun rst -> ret (Obj.magic coq_Monad_optErr) (app xsl rst))))
          args (ret (Obj.magic coq_Monad_optErr) Coq_nil)) (fun args' ->
        let topicsnumber =
          match topics with
          | Coq_nil -> O
          | Coq_cons (_, l) ->
            (match l with
             | Coq_nil -> S O
             | Coq_cons (_, l0) ->
               (match l0 with
                | Coq_nil -> S (S O)
                | Coq_cons (_, l1) ->
                  (match l1 with
                   | Coq_nil -> S (S (S O))
                   | Coq_cons (_, l2) ->
                     (match l2 with
                      | Coq_nil -> S (S (S (S O)))
                      | Coq_cons (_, _) -> S (S (S (S (S O))))))))
        in
        let argsnumber = length args in
        let compiledInstrs =
          app args'
            (app (storeLogArgs argsnumber)
              (app topics'
                (app
                  (match topics with
                   | Coq_nil -> Coq_nil
                   | Coq_cons (_, l) ->
                     (match l with
                      | Coq_nil ->
                        set_eeiOffset Coq_nil Coq_topic1Offset (S (S (S (S (S
                          (S (S (S O))))))))
                      | Coq_cons (_, l0) ->
                        (match l0 with
                         | Coq_nil ->
                           app
                             (set_eeiOffset Coq_nil Coq_topic1Offset (S (S (S
                               (S (S (S (S (S O)))))))))
                             (set_eeiOffset Coq_nil Coq_topic2Offset (S (S (S
                               (S (S (S (S (S O)))))))))
                         | Coq_cons (_, l1) ->
                           (match l1 with
                            | Coq_nil ->
                              app
                                (set_eeiOffset Coq_nil Coq_topic1Offset (S (S
                                  (S (S (S (S (S (S O)))))))))
                                (app
                                  (set_eeiOffset Coq_nil Coq_topic2Offset (S
                                    (S (S (S (S (S (S (S O)))))))))
                                  (set_eeiOffset Coq_nil Coq_topic3Offset (S
                                    (S (S (S (S (S (S (S O))))))))))
                            | Coq_cons (_, l2) ->
                              (match l2 with
                               | Coq_nil ->
                                 app
                                   (set_eeiOffset Coq_nil Coq_topic1Offset (S
                                     (S (S (S (S (S (S (S O)))))))))
                                   (app
                                     (set_eeiOffset Coq_nil Coq_topic2Offset
                                       (S (S (S (S (S (S (S (S O)))))))))
                                     (app
                                       (set_eeiOffset Coq_nil
                                         Coq_topic3Offset (S (S (S (S (S (S
                                         (S (S O)))))))))
                                       (set_eeiOffset Coq_nil
                                         Coq_topic4Offset (S (S (S (S (S (S
                                         (S (S O)))))))))))
                               | Coq_cons (_, _) -> Coq_nil))))) (Coq_cons
                  ((Const_nat (get_idx_eei_mem_offset Coq_dataOffset)),
                  (Coq_cons ((Const_nat (mul argsnumber (S (S (S (S O)))))),
                  (Coq_cons ((Const_nat topicsnumber), (Coq_cons ((Const_nat
                  (get_idx_eei_mem_offset Coq_topic1Offset)), (Coq_cons
                  ((Const_nat (get_idx_eei_mem_offset Coq_topic2Offset)),
                  (Coq_cons ((Const_nat
                  (get_idx_eei_mem_offset Coq_topic3Offset)), (Coq_cons
                  ((Const_nat (get_idx_eei_mem_offset Coq_topic4Offset)),
                  (Coq_cons ((Call (get_idx_eei Coq_eei_log)),
                  Coq_nil)))))))))))))))))))
        in
        (match Nat.eqb topicsnumber (S (S (S (S (S O))))) with
         | Coq_true ->
           Error (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false,
             Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
             (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
             Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
             Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
             (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
             Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
             (Coq_true, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
             Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
             Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
             Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
             Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
             ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
             Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
             Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
             Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
             Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
             ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
             Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
             Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
             Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
             Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
             ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false,
             Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
             Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
             Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
             Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
             ((Ascii (Coq_false, Coq_true, Coq_true, Coq_false, Coq_false,
             Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
             Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
             Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_false,
             Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
             ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
             Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_false,
             Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
             Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
             Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
             ((Ascii (Coq_true, Coq_true, Coq_true, Coq_false, Coq_false,
             Coq_true, Coq_true, Coq_false)),
             EmptyString))))))))))))))))))))))))))))))))))))))))))))))
         | Coq_false -> ret (Obj.magic coq_Monad_optErr) compiledInstrs)))
  | Srevert ->
    ret (Obj.magic coq_Monad_optErr) (Coq_cons ((Const_nat
      (get_idx_eei_mem_offset Coq_dataOffset)), (Coq_cons ((Const_nat
      eei_revert_output_data_length), (Coq_cons ((Call
      (get_idx_eei Coq_eei_revert)), Coq_nil))))))

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

(** val allocate_fn_temps : coq_function -> nat PTree.t **)

let allocate_fn_temps f =
  allocate_all_temps (app f.fn_params f.fn_temps)

(** val wasm_function :
    coq_Z PTree.t -> (positive, nat) prod list -> coq_function ->
    Language0.coq_function optErr **)

let wasm_function _ funident_map f =
  let wasm_fb =
    wasm_statement funident_map (allocate_fn_temps f) f.fn_return f.fn_body O
  in
  (match wasm_fb with
   | Success insts ->
     ret (Obj.magic coq_Monad_optErr) { Language0.fn_return = f.fn_return;
       Language0.fn_params = f.fn_params; Language0.fn_temps = f.fn_temps;
       Language0.fn_locals = f.fn_locals; Language0.fn_body =
       (app (Coq_cons ((Const_256 sp), (Coq_cons ((Const_256 sb), (Coq_cons
         ((Memop (Coq_i32 IOp32.Store)), Coq_nil)))))) insts) }
   | Error msg ->
     Error
       (append (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_false,
         Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
         (Coq_true, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
         Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
         Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
         (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
         Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
         (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
         Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
         Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
         (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
         ((Ascii (Coq_true, Coq_true, Coq_true, Coq_false, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
         Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
         Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
         Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
         Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
         Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
         Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
         Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
         Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_false,
         Coq_true, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
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
         ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
         Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
         Coq_false, Coq_true, Coq_false, Coq_true, Coq_false)), (String
         ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
         Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
         Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
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
         ((Ascii (Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
         Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_false,
         Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
         Coq_false)),
         EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
         msg))

(** val wasm_constructor :
    coq_function option -> coq_Z PTree.t -> (positive, nat) prod list ->
    Language0.coq_function option optErr **)

let wasm_constructor f global funident_map =
  match f with
  | Some f0 ->
    bind (Obj.magic coq_Monad_optErr)
      (Obj.magic wasm_function global funident_map f0) (fun cf ->
      ret (Obj.magic coq_Monad_optErr) (Some cf))
  | None -> ret (Obj.magic coq_Monad_optErr) None

(** val wasm_functions :
    coq_function PTree.t -> coq_Z PTree.t -> (positive, nat) prod list ->
    Language0.coq_function PTree.t optErr **)

let wasm_functions defs global funident_map =
  transl_tree (wasm_function global funident_map) defs

(** val wasm_methoddefs :
    coq_function option IntMap.t -> coq_Z PTree.t -> (positive, nat) prod
    list -> Language0.coq_function option IntMap.t optErr **)

let wasm_methoddefs defs global funident_map =
  transl_map (wasm_function global funident_map) defs

(** val wasm_func_identifier :
    (positive, 'a1) prod list -> nat -> nat -> (positive, nat) prod list **)

let rec wasm_func_identifier a idx base =
  match a with
  | Coq_nil -> Coq_nil
  | Coq_cons (x, xs) ->
    Coq_cons ((Coq_pair ((fst x), (add idx base))),
      (wasm_func_identifier xs (add idx (S O)) base))

(** val wasm_genv : genv -> Language0.genv optErr **)

let wasm_genv ge =
  let vars = ge.Genv.genv_vars in
  let names = ge.Genv.genv_funcs in
  let fundefs = ge.Genv.genv_fundefs in
  let sigs = ge.Genv.genv_methods in
  let defs = ge.Genv.genv_defs in
  let methoddefs = ge.Genv.genv_methoddefs in
  let constructor = ge.Genv.genv_constructor in
  let wasm_func_id_base =
    add (length sigs) (match constructor with
                       | Some _ -> S O
                       | None -> O)
  in
  let funident_map =
    wasm_func_identifier (PTree.elements fundefs) O
      (add wasm_func_id_base (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
        (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
        (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
        (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
        (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
        (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
        (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
        (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
        (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
        (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
        (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
        (S (S (S (S (S (S (S (S (S (S
        O)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
  in
  bind (Obj.magic coq_Monad_optErr)
    (Obj.magic allocate_addrs ge.Genv.genv_vars Z0 ge.Genv.genv_defs)
    (fun allocated ->
    bind (Obj.magic coq_Monad_optErr)
      (Obj.magic wasm_functions fundefs allocated funident_map)
      (fun fundefs0 ->
      bind (Obj.magic coq_Monad_optErr)
        (Obj.magic wasm_methoddefs methoddefs allocated funident_map)
        (fun methoddefs0 ->
        bind (Obj.magic coq_Monad_optErr)
          (Obj.magic wasm_constructor constructor allocated funident_map)
          (fun constructor0 ->
          ret (Obj.magic coq_Monad_optErr) { Genv.genv_vars = vars;
            Genv.genv_funcs = names; Genv.genv_methods = sigs;
            Genv.genv_defs = defs; Genv.genv_fundefs = fundefs0;
            Genv.genv_methoddefs = methoddefs0; Genv.genv_constructor =
            constructor0 }))))
