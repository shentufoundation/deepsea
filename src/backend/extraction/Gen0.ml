open AST
open Ascii
open BinPos
open Cop
open Ctypes
open Datatypes
open ExpMiniC
open Globalenvs
open Maps0
open Monad
open Nat0
open OptErrMonad
open PeanoNat
open StmtClocal
open StmtMiniC
open String0
open Trees

(** val sequentialize : StmtClocal.statement list -> StmtClocal.statement **)

let rec sequentialize = function
| Coq_nil -> StmtClocal.Sskip
| Coq_cons (s, ss0) -> StmtClocal.Ssequence (s, (sequentialize ss0))

(** val clocal_rvalue :
    (nat -> expr) -> (expr -> nat -> (StmtClocal.statement, nat) prod optErr)
    -> expr -> nat -> (StmtClocal.statement, nat) prod optErr **)

let clocal_rvalue scrmap =
  let rec clocal_rvalue0 cont ex scr =
    match ex with
    | Econst_int (_, _) ->
      Error (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii
        (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
        Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
        Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
        Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
        Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true,
        Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_true, Coq_true, Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
        (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
        Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
        Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
        Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_false)),
        (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
        Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
        (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
        Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
        Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
        (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
        (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
        Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
        Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
        Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
        Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
        Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false,
        Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
        Coq_false, Coq_false)),
        EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
    | Eglob (_, _) ->
      Error (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii
        (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
        Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
        Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
        Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
        Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
        Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_true, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
        Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false,
        Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
        Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
        Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
        Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
        Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false,
        Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
        Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false,
        Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
        Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
        Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
        Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
        ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
        Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_false,
        Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_false,
        Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
        (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
        Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
        ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_true, Coq_false)),
        EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
    | Ederef (ex0, ty) ->
      let cont' = fun ex' scr' -> cont (Ederef (ex', ty)) scr' in
      clocal_lvalue0 cont' ex0 scr
    | Eaddr (ex0, _) -> clocal_lvalue0 cont ex0 scr
    | Eunop (op, ex0, ty) ->
      (match op with
       | Osha_1 ->
         let cont' = fun ex' scr' ->
           let scre = scrmap scr' in
           let s0 = Smassign (scre, ex') in
           let s1 = Shash (scre, scre, None) in
           bind coq_Monad_optErr
             (Obj.magic cont (Ederef (scre, ty)) (add scr' (S O)))
             (fun res ->
             let Coq_pair (s2, scr'') = res in
             ret coq_Monad_optErr (Coq_pair
               ((sequentialize (Coq_cons (s0, (Coq_cons (s1, (Coq_cons (s2,
                  Coq_nil))))))), scr'')))
         in
         Obj.magic clocal_rvalue0 cont' ex0 scr
       | _ ->
         let cont' = fun ex' scr' -> cont (Eunop (op, ex', ty)) scr' in
         clocal_rvalue0 cont' ex0 scr)
    | Ebinop (op, ex1, ex2, ty) ->
      (match op with
       | Osha_2 ->
         let cont' = fun ex1' scr1 ->
           let cont'' = fun ex2' scr2 ->
             let scre1 = scrmap scr2 in
             let scre2 = scrmap (add scr2 (S O)) in
             let s0 = Smassign (scre1, ex1') in
             let s1 = Smassign (scre2, ex2') in
             let s2 = Shash (scre1, scre1, (Some scre2)) in
             bind coq_Monad_optErr
               (Obj.magic cont (Ederef (scre1, ty)) (add scr2 (S (S O))))
               (fun res ->
               let Coq_pair (s3, scr') = res in
               ret coq_Monad_optErr (Coq_pair
                 ((sequentialize (Coq_cons (s0, (Coq_cons (s1, (Coq_cons (s2,
                    (Coq_cons (s3, Coq_nil))))))))), scr')))
           in
           Obj.magic clocal_rvalue0 cont'' ex2 scr1
         in
         clocal_rvalue0 cont' ex1 scr
       | _ ->
         let cont' = fun ex1' scr1 ->
           let cont'' = fun ex2' scr2 ->
             cont (Ebinop (op, ex1', ex2', ty)) scr2
           in
           clocal_rvalue0 cont'' ex2 scr1
         in
         clocal_rvalue0 cont' ex1 scr)
    | Efield (_, _, _) ->
      Error (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
        Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
        Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
        ((Ascii (Coq_true, Coq_false, Coq_true, Coq_true, Coq_false,
        Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_false,
        Coq_true, Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
        Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
        ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false,
        Coq_false, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true,
        Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
        Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
        Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
        ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
        Coq_false, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
        Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
        Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
        (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
        Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
        Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
        Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
        Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
        Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
        Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
        ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
        Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
        Coq_false, Coq_false, Coq_false, Coq_true, Coq_false)), (String
        ((Ascii (Coq_false, Coq_true, Coq_true, Coq_false, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
        Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
        Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
        ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
        ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
        Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
        Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
        Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
        Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
        Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
        Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
        Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
        ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
        Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
        (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false,
        Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
        (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_true,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
        Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
        ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
        Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
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
        (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
        Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_true, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
        Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
        Coq_true, Coq_false, Coq_false)),
        EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
    | Eindex (_, _, _) ->
      Error (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
        Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
        Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
        ((Ascii (Coq_true, Coq_false, Coq_true, Coq_true, Coq_false,
        Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_false,
        Coq_true, Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
        Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
        ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false,
        Coq_false, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true,
        Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
        Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
        Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
        ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
        Coq_false, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
        Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
        Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
        (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
        Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
        Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
        Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
        Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
        Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
        Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
        ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
        Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
        Coq_false, Coq_false, Coq_false, Coq_true, Coq_false)), (String
        ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
        Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
        ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
        ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
        Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
        Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
        Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
        Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
        Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
        Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
        Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
        ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
        Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
        (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false,
        Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
        (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_true,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
        Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
        ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
        Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
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
        (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
        Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_true, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
        Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
        Coq_true, Coq_false, Coq_false)),
        EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
    | Ecall1 (b, ex0, ty) ->
      let cont' = fun ex' scr' -> cont (Ecall1 (b, ex', ty)) scr' in
      clocal_rvalue0 cont' ex0 scr
    | x -> cont x scr
  and clocal_lvalue0 cont ex scr =
    match ex with
    | Evar (id, t0) ->
      (match t0 with
       | Tpointer (p, ty) ->
         (match p with
          | Coq_mem -> cont (Evar (id, (Tpointer (Coq_mem, ty)))) scr
          | _ ->
            Error (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii
              (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
              Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
              Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
              Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
              Coq_true, Coq_true, Coq_false, Coq_true, Coq_false,
              Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
              Coq_true, Coq_true, Coq_true, Coq_false, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true,
              Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_true, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
              Coq_false, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
              ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
              Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
              Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true,
              Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
              Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
              ((Ascii (Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
              ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
              Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_false,
              Coq_true, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
              Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true,
              Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
              Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
              ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
              ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
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
              Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_false,
              Coq_false)),
              EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
       | _ ->
         Error (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_true,
           Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
           (Coq_false, Coq_true, Coq_true, Coq_true, Coq_true, Coq_true,
           Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
           Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_true, Coq_false, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
           Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_true, Coq_false, Coq_true, Coq_false, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false,
           Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
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
           (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
           Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
           Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_false,
           Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
           Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
           (Coq_false, Coq_true, Coq_false, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
           Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
           Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_false, Coq_false)),
           EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
    | Etempvar (id, ty) -> cont (Etempvar (id, ty)) scr
    | Ederef (ex0, _) -> clocal_rvalue0 cont ex0 scr
    | Eunop (u, ex0, t0) ->
      (match u with
       | Osha_1 ->
         (match t0 with
          | Tpointer (p, ty) ->
            (match p with
             | Coq_stor ->
               let cont' = fun ex' scr' ->
                 let scre = scrmap scr' in
                 let s0 = Smassign (scre, ex') in
                 let s1 = Shash (scre, scre, None) in
                 bind coq_Monad_optErr
                   (Obj.magic cont (Ederef (scre, (Tpointer (Coq_stor, ty))))
                     (add scr' (S O))) (fun res ->
                   let Coq_pair (s2, scr'') = res in
                   ret coq_Monad_optErr (Coq_pair
                     ((sequentialize (Coq_cons (s0, (Coq_cons (s1, (Coq_cons
                        (s2, Coq_nil))))))), scr'')))
               in
               Obj.magic clocal_rvalue0 cont' ex0 scr
             | _ ->
               Error (String ((Ascii (Coq_true, Coq_true, Coq_false,
                 Coq_false, Coq_false, Coq_false, Coq_true, Coq_false)),
                 (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true,
                 Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
                 (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
                 Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
                 Coq_true, Coq_false, Coq_true, Coq_false, Coq_true,
                 Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
                 Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
                 Coq_false)), (String ((Ascii (Coq_false, Coq_false,
                 Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
                 Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
                 Coq_true, Coq_false, Coq_true, Coq_false, Coq_false)),
                 (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true,
                 Coq_true, Coq_true, Coq_false, Coq_false)), (String ((Ascii
                 (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
                 Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true,
                 Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
                 Coq_true, Coq_false)), (String ((Ascii (Coq_false,
                 Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
                 Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
                 Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
                 Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
                 Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
                 (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false,
                 Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
                 (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false,
                 Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
                 Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
                 Coq_false, Coq_false)), (String ((Ascii (Coq_false,
                 Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
                 Coq_false, Coq_false)), (String ((Ascii (Coq_false,
                 Coq_false, Coq_true, Coq_false, Coq_true, Coq_false,
                 Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
                 Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
                 Coq_false)), (String ((Ascii (Coq_true, Coq_false,
                 Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
                 Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
                 Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
                 (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false,
                 Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
                 (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
                 Coq_true, Coq_false, Coq_false)), (String ((Ascii
                 (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
                 Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
                 Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
                 Coq_false)), (String ((Ascii (Coq_false, Coq_false,
                 Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
                 Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
                 Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
                 (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true,
                 Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
                 (Coq_true, Coq_false, Coq_true, Coq_true, Coq_false,
                 Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
                 Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
                 Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
                 Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
                 Coq_false)), (String ((Ascii (Coq_false, Coq_false,
                 Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
                 Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
                 Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
                 (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
                 Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
                 (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
                 Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
                 Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
                 Coq_false)), (String ((Ascii (Coq_false, Coq_false,
                 Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
                 Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
                 Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
                 (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_true,
                 Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
                 (Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
                 Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
                 Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
                 Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
                 Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
                 Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
                 Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
                 (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
                 Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
                 (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
                 Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
                 Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
                 Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
                 Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
                 (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
                 Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
                 (Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
                 Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
                 Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
                 Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
                 Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
                 Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
                 Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
                 (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_true,
                 Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
                 (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
                 Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true,
                 Coq_true, Coq_false, Coq_false, Coq_false, Coq_true,
                 Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
                 Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
                 Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
                 Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
                 (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true,
                 Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
                 (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
                 Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
                 Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
                 Coq_true, Coq_false)), (String ((Ascii (Coq_false,
                 Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
                 Coq_false, Coq_false)), (String ((Ascii (Coq_false,
                 Coq_true, Coq_false, Coq_false, Coq_false, Coq_true,
                 Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
                 Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
                 Coq_false)), (String ((Ascii (Coq_false, Coq_false,
                 Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
                 Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
                 Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
                 (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
                 Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
                 (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
                 Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
                 Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
                 Coq_true, Coq_false)), (String ((Ascii (Coq_false,
                 Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
                 Coq_false, Coq_false)), (String ((Ascii (Coq_true,
                 Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
                 Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
                 Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
                 Coq_false)), (String ((Ascii (Coq_false, Coq_false,
                 Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
                 Coq_false)), (String ((Ascii (Coq_true, Coq_false,
                 Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
                 Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
                 Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
                 (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
                 Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
                 (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false,
                 Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
                 Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
                 Coq_false, Coq_false)), (String ((Ascii (Coq_false,
                 Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
                 Coq_false)), (String ((Ascii (Coq_true, Coq_false,
                 Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
                 Coq_false)), (String ((Ascii (Coq_false, Coq_false,
                 Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
                 Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
                 Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
                 (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
                 Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
                 (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
                 Coq_true, Coq_false, Coq_false)),
                 EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
          | _ ->
            Error (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii
              (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
              Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
              Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
              Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
              Coq_true, Coq_true, Coq_false, Coq_true, Coq_false,
              Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
              Coq_true, Coq_true, Coq_true, Coq_false, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true,
              Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_true, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
              Coq_false, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
              ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
              Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
              Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true,
              Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
              Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
              ((Ascii (Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
              ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
              Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_false,
              Coq_true, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
              Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true,
              Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
              Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
              ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
              ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
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
              Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_false,
              Coq_false)),
              EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
       | _ ->
         Error (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_true,
           Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
           (Coq_false, Coq_true, Coq_true, Coq_true, Coq_true, Coq_true,
           Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
           Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_true, Coq_false, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
           Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_true, Coq_false, Coq_true, Coq_false, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false,
           Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
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
           (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
           Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
           Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_false,
           Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
           Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
           (Coq_false, Coq_true, Coq_false, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
           Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
           Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_false, Coq_false)),
           EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
    | Ebinop (b, ex1, ex2, t0) ->
      (match b with
       | Osha_2 ->
         (match t0 with
          | Tpointer (p, ty) ->
            (match p with
             | Coq_stor ->
               let cont' = fun ex1' scr1 ->
                 let cont'' = fun ex2' scr2 ->
                   let scre1 = scrmap scr2 in
                   let scre2 = scrmap (add scr2 (S O)) in
                   let s0 = Smassign (scre1, ex1') in
                   let s1 = Smassign (scre2, ex2') in
                   let s2 = Shash (scre1, scre1, (Some scre2)) in
                   bind coq_Monad_optErr
                     (Obj.magic cont (Ederef (scre1, (Tpointer (Coq_stor,
                       ty)))) (add scr2 (S (S O)))) (fun res ->
                     let Coq_pair (s3, scr') = res in
                     ret coq_Monad_optErr (Coq_pair
                       ((sequentialize (Coq_cons (s0, (Coq_cons (s1,
                          (Coq_cons (s2, (Coq_cons (s3, Coq_nil))))))))),
                       scr')))
                 in
                 Obj.magic clocal_rvalue0 cont'' ex2 scr1
               in
               clocal_rvalue0 cont' ex1 scr
             | _ ->
               Error (String ((Ascii (Coq_true, Coq_true, Coq_false,
                 Coq_false, Coq_false, Coq_false, Coq_true, Coq_false)),
                 (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true,
                 Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
                 (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
                 Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
                 Coq_true, Coq_false, Coq_true, Coq_false, Coq_true,
                 Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
                 Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
                 Coq_false)), (String ((Ascii (Coq_false, Coq_false,
                 Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
                 Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
                 Coq_true, Coq_false, Coq_true, Coq_false, Coq_false)),
                 (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true,
                 Coq_true, Coq_true, Coq_false, Coq_false)), (String ((Ascii
                 (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
                 Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true,
                 Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
                 Coq_true, Coq_false)), (String ((Ascii (Coq_false,
                 Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
                 Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
                 Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
                 Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
                 Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
                 (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false,
                 Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
                 (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false,
                 Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
                 Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
                 Coq_false, Coq_false)), (String ((Ascii (Coq_false,
                 Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
                 Coq_false, Coq_false)), (String ((Ascii (Coq_false,
                 Coq_false, Coq_true, Coq_false, Coq_true, Coq_false,
                 Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
                 Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
                 Coq_false)), (String ((Ascii (Coq_true, Coq_false,
                 Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
                 Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
                 Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
                 (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false,
                 Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
                 (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
                 Coq_true, Coq_false, Coq_false)), (String ((Ascii
                 (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
                 Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
                 Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
                 Coq_false)), (String ((Ascii (Coq_false, Coq_false,
                 Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
                 Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
                 Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
                 (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true,
                 Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
                 (Coq_true, Coq_false, Coq_true, Coq_true, Coq_false,
                 Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
                 Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
                 Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
                 Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
                 Coq_false)), (String ((Ascii (Coq_false, Coq_false,
                 Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
                 Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
                 Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
                 (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
                 Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
                 (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
                 Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
                 Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
                 Coq_false)), (String ((Ascii (Coq_false, Coq_false,
                 Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
                 Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
                 Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
                 (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_true,
                 Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
                 (Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
                 Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
                 Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
                 Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
                 Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
                 Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
                 Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
                 (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
                 Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
                 (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
                 Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
                 Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
                 Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
                 Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
                 (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
                 Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
                 (Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
                 Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
                 Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
                 Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
                 Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
                 Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
                 Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
                 (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_true,
                 Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
                 (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
                 Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true,
                 Coq_true, Coq_false, Coq_false, Coq_false, Coq_true,
                 Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
                 Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
                 Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
                 Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
                 (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true,
                 Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
                 (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
                 Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
                 Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
                 Coq_true, Coq_false)), (String ((Ascii (Coq_false,
                 Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
                 Coq_false, Coq_false)), (String ((Ascii (Coq_false,
                 Coq_true, Coq_false, Coq_false, Coq_false, Coq_true,
                 Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
                 Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
                 Coq_false)), (String ((Ascii (Coq_false, Coq_false,
                 Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
                 Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
                 Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
                 (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
                 Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
                 (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
                 Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
                 Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
                 Coq_true, Coq_false)), (String ((Ascii (Coq_false,
                 Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
                 Coq_false, Coq_false)), (String ((Ascii (Coq_true,
                 Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
                 Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
                 Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
                 Coq_false)), (String ((Ascii (Coq_false, Coq_false,
                 Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
                 Coq_false)), (String ((Ascii (Coq_true, Coq_false,
                 Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
                 Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
                 Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
                 (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
                 Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
                 (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false,
                 Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
                 Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
                 Coq_false, Coq_false)), (String ((Ascii (Coq_false,
                 Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
                 Coq_false)), (String ((Ascii (Coq_true, Coq_false,
                 Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
                 Coq_false)), (String ((Ascii (Coq_false, Coq_false,
                 Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
                 Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
                 Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
                 (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
                 Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
                 (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
                 Coq_true, Coq_false, Coq_false)),
                 EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
          | _ ->
            Error (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii
              (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
              Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
              Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
              Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
              Coq_true, Coq_true, Coq_false, Coq_true, Coq_false,
              Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
              Coq_true, Coq_true, Coq_true, Coq_false, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true,
              Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_true, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
              Coq_false, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
              ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
              Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
              Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true,
              Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
              Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
              ((Ascii (Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
              ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
              Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_false,
              Coq_true, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
              Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true,
              Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
              Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
              ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
              ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
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
              Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_false,
              Coq_false)),
              EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
       | _ ->
         Error (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_true,
           Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
           (Coq_false, Coq_true, Coq_true, Coq_true, Coq_true, Coq_true,
           Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
           Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_true, Coq_false, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
           Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_true, Coq_false, Coq_true, Coq_false, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false,
           Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
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
           (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
           Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
           Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_false,
           Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
           Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
           (Coq_false, Coq_true, Coq_false, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
           Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
           Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_false, Coq_false)),
           EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
    | Efield (ex0, id, t0) ->
      (match t0 with
       | Tpointer (p, ty) ->
         (match p with
          | Coq_mem ->
            let cont' = fun ex' ->
              cont (Efield (ex', id, (Tpointer (Coq_mem, ty))))
            in
            clocal_lvalue0 cont' ex0 scr
          | _ ->
            Error (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii
              (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
              Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
              Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
              Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
              Coq_true, Coq_true, Coq_false, Coq_true, Coq_false,
              Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
              Coq_true, Coq_true, Coq_true, Coq_false, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true,
              Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_true, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
              Coq_false, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
              ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
              Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
              Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true,
              Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
              Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
              ((Ascii (Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
              ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
              Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_false,
              Coq_true, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
              Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true,
              Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
              Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
              ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
              ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
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
              Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_false,
              Coq_false)),
              EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
       | _ ->
         Error (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_true,
           Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
           (Coq_false, Coq_true, Coq_true, Coq_true, Coq_true, Coq_true,
           Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
           Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_true, Coq_false, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
           Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_true, Coq_false, Coq_true, Coq_false, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false,
           Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
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
           (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
           Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
           Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_false,
           Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
           Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
           (Coq_false, Coq_true, Coq_false, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
           Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
           Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_false, Coq_false)),
           EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
    | Eindex (ex1, ex2, t0) ->
      (match t0 with
       | Tpointer (p, ty) ->
         (match p with
          | Coq_mem ->
            let cont' = fun ex1' scr1 ->
              let cont'' = fun ex2' scr2 ->
                cont (Eindex (ex1', ex2', (Tpointer (Coq_mem, ty)))) scr2
              in
              clocal_rvalue0 cont'' ex2 scr1
            in
            clocal_lvalue0 cont' ex1 scr
          | _ ->
            Error (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii
              (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
              Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
              Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
              Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
              Coq_true, Coq_true, Coq_false, Coq_true, Coq_false,
              Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
              Coq_true, Coq_true, Coq_true, Coq_false, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true,
              Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_true, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
              Coq_false, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
              ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
              Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
              Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true,
              Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
              Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
              ((Ascii (Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
              ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
              Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_false,
              Coq_true, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
              Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true,
              Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
              Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
              ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
              ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
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
              Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_false,
              Coq_false)),
              EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
       | _ ->
         Error (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_true,
           Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
           (Coq_false, Coq_true, Coq_true, Coq_true, Coq_true, Coq_true,
           Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
           Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_true, Coq_false, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
           Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_true, Coq_false, Coq_true, Coq_false, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false,
           Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
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
           (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
           Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
           Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_false,
           Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
           Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
           (Coq_false, Coq_true, Coq_false, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
           Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
           Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_false, Coq_false)),
           EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
    | _ ->
      Error (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
        Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
        Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
        ((Ascii (Coq_true, Coq_false, Coq_true, Coq_true, Coq_false,
        Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_false,
        Coq_true, Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
        Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
        ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false,
        Coq_false, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true,
        Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
        Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
        Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
        ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
        Coq_false, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
        Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
        Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
        (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
        Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
        Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
        Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
        Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
        Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
        Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
        ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
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
        (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
        Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
        Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
        (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
        Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true,
        Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
        (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false,
        Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
        (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_true,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
        Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
        ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
        Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
        Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
        (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
        Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
        Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
        (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false, Coq_true, Coq_false, Coq_false)),
        (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
        Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_true, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
        Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
        Coq_true, Coq_false, Coq_false)),
        EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
  in clocal_rvalue0

(** val clocal_lvalue :
    (nat -> expr) -> (expr -> nat -> (StmtClocal.statement, nat) prod optErr)
    -> expr -> nat -> (StmtClocal.statement, nat) prod optErr **)

let clocal_lvalue scrmap =
  let rec clocal_rvalue0 cont ex scr =
    match ex with
    | Econst_int (_, _) ->
      Error (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii
        (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
        Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
        Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
        Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
        Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true,
        Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_true, Coq_true, Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
        (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
        Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
        Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
        Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_false)),
        (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
        Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
        (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
        Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
        Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
        (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
        (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
        Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
        Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
        Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
        Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
        Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false,
        Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
        Coq_false, Coq_false)),
        EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
    | Eglob (_, _) ->
      Error (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii
        (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
        Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
        Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
        Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
        Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
        Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_true, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
        Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false,
        Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
        Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
        Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
        Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
        Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false,
        Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
        Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false,
        Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
        Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
        Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
        Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
        ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
        Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_false,
        Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_false,
        Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
        (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
        Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
        ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_true, Coq_false)),
        EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
    | Ederef (ex0, ty) ->
      let cont' = fun ex' scr' -> cont (Ederef (ex', ty)) scr' in
      clocal_lvalue0 cont' ex0 scr
    | Eaddr (ex0, _) -> clocal_lvalue0 cont ex0 scr
    | Eunop (op, ex0, ty) ->
      (match op with
       | Osha_1 ->
         let cont' = fun ex' scr' ->
           let scre = scrmap scr' in
           let s0 = Smassign (scre, ex') in
           let s1 = Shash (scre, scre, None) in
           bind coq_Monad_optErr
             (Obj.magic cont (Ederef (scre, ty)) (add scr' (S O)))
             (fun res ->
             let Coq_pair (s2, scr'') = res in
             ret coq_Monad_optErr (Coq_pair
               ((sequentialize (Coq_cons (s0, (Coq_cons (s1, (Coq_cons (s2,
                  Coq_nil))))))), scr'')))
         in
         Obj.magic clocal_rvalue0 cont' ex0 scr
       | _ ->
         let cont' = fun ex' scr' -> cont (Eunop (op, ex', ty)) scr' in
         clocal_rvalue0 cont' ex0 scr)
    | Ebinop (op, ex1, ex2, ty) ->
      (match op with
       | Osha_2 ->
         let cont' = fun ex1' scr1 ->
           let cont'' = fun ex2' scr2 ->
             let scre1 = scrmap scr2 in
             let scre2 = scrmap (add scr2 (S O)) in
             let s0 = Smassign (scre1, ex1') in
             let s1 = Smassign (scre2, ex2') in
             let s2 = Shash (scre1, scre1, (Some scre2)) in
             bind coq_Monad_optErr
               (Obj.magic cont (Ederef (scre1, ty)) (add scr2 (S (S O))))
               (fun res ->
               let Coq_pair (s3, scr') = res in
               ret coq_Monad_optErr (Coq_pair
                 ((sequentialize (Coq_cons (s0, (Coq_cons (s1, (Coq_cons (s2,
                    (Coq_cons (s3, Coq_nil))))))))), scr')))
           in
           Obj.magic clocal_rvalue0 cont'' ex2 scr1
         in
         clocal_rvalue0 cont' ex1 scr
       | _ ->
         let cont' = fun ex1' scr1 ->
           let cont'' = fun ex2' scr2 ->
             cont (Ebinop (op, ex1', ex2', ty)) scr2
           in
           clocal_rvalue0 cont'' ex2 scr1
         in
         clocal_rvalue0 cont' ex1 scr)
    | Efield (_, _, _) ->
      Error (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
        Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
        Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
        ((Ascii (Coq_true, Coq_false, Coq_true, Coq_true, Coq_false,
        Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_false,
        Coq_true, Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
        Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
        ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false,
        Coq_false, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true,
        Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
        Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
        Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
        ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
        Coq_false, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
        Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
        Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
        (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
        Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
        Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
        Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
        Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
        Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
        Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
        ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
        Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
        Coq_false, Coq_false, Coq_false, Coq_true, Coq_false)), (String
        ((Ascii (Coq_false, Coq_true, Coq_true, Coq_false, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
        Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
        Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
        ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
        ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
        Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
        Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
        Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
        Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
        Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
        Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
        Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
        ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
        Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
        (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false,
        Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
        (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_true,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
        Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
        ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
        Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
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
        (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
        Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_true, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
        Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
        Coq_true, Coq_false, Coq_false)),
        EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
    | Eindex (_, _, _) ->
      Error (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
        Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
        Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
        ((Ascii (Coq_true, Coq_false, Coq_true, Coq_true, Coq_false,
        Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_false,
        Coq_true, Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
        Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
        ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false,
        Coq_false, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true,
        Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
        Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
        Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
        ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
        Coq_false, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
        Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
        Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
        (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
        Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
        Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
        Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
        Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
        Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
        Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
        ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
        Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
        Coq_false, Coq_false, Coq_false, Coq_true, Coq_false)), (String
        ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
        Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
        ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
        ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
        Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
        Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
        Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
        Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
        Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
        Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
        Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
        ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
        Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
        (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false,
        Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
        (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_true,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
        Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
        ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
        Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
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
        (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
        Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_true, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
        Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
        Coq_true, Coq_false, Coq_false)),
        EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
    | Ecall1 (b, ex0, ty) ->
      let cont' = fun ex' scr' -> cont (Ecall1 (b, ex', ty)) scr' in
      clocal_rvalue0 cont' ex0 scr
    | x -> cont x scr
  and clocal_lvalue0 cont ex scr =
    match ex with
    | Evar (id, t0) ->
      (match t0 with
       | Tpointer (p, ty) ->
         (match p with
          | Coq_mem -> cont (Evar (id, (Tpointer (Coq_mem, ty)))) scr
          | _ ->
            Error (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii
              (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
              Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
              Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
              Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
              Coq_true, Coq_true, Coq_false, Coq_true, Coq_false,
              Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
              Coq_true, Coq_true, Coq_true, Coq_false, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true,
              Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_true, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
              Coq_false, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
              ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
              Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
              Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true,
              Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
              Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
              ((Ascii (Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
              ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
              Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_false,
              Coq_true, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
              Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true,
              Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
              Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
              ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
              ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
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
              Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_false,
              Coq_false)),
              EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
       | _ ->
         Error (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_true,
           Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
           (Coq_false, Coq_true, Coq_true, Coq_true, Coq_true, Coq_true,
           Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
           Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_true, Coq_false, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
           Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_true, Coq_false, Coq_true, Coq_false, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false,
           Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
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
           (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
           Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
           Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_false,
           Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
           Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
           (Coq_false, Coq_true, Coq_false, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
           Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
           Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_false, Coq_false)),
           EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
    | Etempvar (id, ty) -> cont (Etempvar (id, ty)) scr
    | Ederef (ex0, _) -> clocal_rvalue0 cont ex0 scr
    | Eunop (u, ex0, t0) ->
      (match u with
       | Osha_1 ->
         (match t0 with
          | Tpointer (p, ty) ->
            (match p with
             | Coq_stor ->
               let cont' = fun ex' scr' ->
                 let scre = scrmap scr' in
                 let s0 = Smassign (scre, ex') in
                 let s1 = Shash (scre, scre, None) in
                 bind coq_Monad_optErr
                   (Obj.magic cont (Ederef (scre, (Tpointer (Coq_stor, ty))))
                     (add scr' (S O))) (fun res ->
                   let Coq_pair (s2, scr'') = res in
                   ret coq_Monad_optErr (Coq_pair
                     ((sequentialize (Coq_cons (s0, (Coq_cons (s1, (Coq_cons
                        (s2, Coq_nil))))))), scr'')))
               in
               Obj.magic clocal_rvalue0 cont' ex0 scr
             | _ ->
               Error (String ((Ascii (Coq_true, Coq_true, Coq_false,
                 Coq_false, Coq_false, Coq_false, Coq_true, Coq_false)),
                 (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true,
                 Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
                 (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
                 Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
                 Coq_true, Coq_false, Coq_true, Coq_false, Coq_true,
                 Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
                 Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
                 Coq_false)), (String ((Ascii (Coq_false, Coq_false,
                 Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
                 Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
                 Coq_true, Coq_false, Coq_true, Coq_false, Coq_false)),
                 (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true,
                 Coq_true, Coq_true, Coq_false, Coq_false)), (String ((Ascii
                 (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
                 Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true,
                 Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
                 Coq_true, Coq_false)), (String ((Ascii (Coq_false,
                 Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
                 Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
                 Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
                 Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
                 Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
                 (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false,
                 Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
                 (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false,
                 Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
                 Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
                 Coq_false, Coq_false)), (String ((Ascii (Coq_false,
                 Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
                 Coq_false, Coq_false)), (String ((Ascii (Coq_false,
                 Coq_false, Coq_true, Coq_false, Coq_true, Coq_false,
                 Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
                 Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
                 Coq_false)), (String ((Ascii (Coq_true, Coq_false,
                 Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
                 Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
                 Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
                 (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false,
                 Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
                 (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
                 Coq_true, Coq_false, Coq_false)), (String ((Ascii
                 (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
                 Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
                 Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
                 Coq_false)), (String ((Ascii (Coq_false, Coq_false,
                 Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
                 Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
                 Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
                 (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true,
                 Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
                 (Coq_true, Coq_false, Coq_true, Coq_true, Coq_false,
                 Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
                 Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
                 Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
                 Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
                 Coq_false)), (String ((Ascii (Coq_false, Coq_false,
                 Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
                 Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
                 Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
                 (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
                 Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
                 (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
                 Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
                 Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
                 Coq_false)), (String ((Ascii (Coq_false, Coq_false,
                 Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
                 Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
                 Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
                 (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_true,
                 Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
                 (Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
                 Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
                 Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
                 Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
                 Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
                 Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
                 Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
                 (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
                 Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
                 (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
                 Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
                 Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
                 Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
                 Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
                 (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
                 Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
                 (Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
                 Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
                 Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
                 Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
                 Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
                 Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
                 Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
                 (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_true,
                 Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
                 (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
                 Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true,
                 Coq_true, Coq_false, Coq_false, Coq_false, Coq_true,
                 Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
                 Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
                 Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
                 Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
                 (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true,
                 Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
                 (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
                 Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
                 Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
                 Coq_true, Coq_false)), (String ((Ascii (Coq_false,
                 Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
                 Coq_false, Coq_false)), (String ((Ascii (Coq_false,
                 Coq_true, Coq_false, Coq_false, Coq_false, Coq_true,
                 Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
                 Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
                 Coq_false)), (String ((Ascii (Coq_false, Coq_false,
                 Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
                 Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
                 Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
                 (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
                 Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
                 (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
                 Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
                 Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
                 Coq_true, Coq_false)), (String ((Ascii (Coq_false,
                 Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
                 Coq_false, Coq_false)), (String ((Ascii (Coq_true,
                 Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
                 Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
                 Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
                 Coq_false)), (String ((Ascii (Coq_false, Coq_false,
                 Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
                 Coq_false)), (String ((Ascii (Coq_true, Coq_false,
                 Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
                 Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
                 Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
                 (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
                 Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
                 (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false,
                 Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
                 Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
                 Coq_false, Coq_false)), (String ((Ascii (Coq_false,
                 Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
                 Coq_false)), (String ((Ascii (Coq_true, Coq_false,
                 Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
                 Coq_false)), (String ((Ascii (Coq_false, Coq_false,
                 Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
                 Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
                 Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
                 (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
                 Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
                 (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
                 Coq_true, Coq_false, Coq_false)),
                 EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
          | _ ->
            Error (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii
              (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
              Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
              Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
              Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
              Coq_true, Coq_true, Coq_false, Coq_true, Coq_false,
              Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
              Coq_true, Coq_true, Coq_true, Coq_false, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true,
              Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_true, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
              Coq_false, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
              ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
              Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
              Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true,
              Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
              Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
              ((Ascii (Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
              ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
              Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_false,
              Coq_true, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
              Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true,
              Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
              Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
              ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
              ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
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
              Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_false,
              Coq_false)),
              EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
       | _ ->
         Error (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_true,
           Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
           (Coq_false, Coq_true, Coq_true, Coq_true, Coq_true, Coq_true,
           Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
           Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_true, Coq_false, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
           Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_true, Coq_false, Coq_true, Coq_false, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false,
           Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
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
           (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
           Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
           Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_false,
           Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
           Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
           (Coq_false, Coq_true, Coq_false, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
           Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
           Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_false, Coq_false)),
           EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
    | Ebinop (b, ex1, ex2, t0) ->
      (match b with
       | Osha_2 ->
         (match t0 with
          | Tpointer (p, ty) ->
            (match p with
             | Coq_stor ->
               let cont' = fun ex1' scr1 ->
                 let cont'' = fun ex2' scr2 ->
                   let scre1 = scrmap scr2 in
                   let scre2 = scrmap (add scr2 (S O)) in
                   let s0 = Smassign (scre1, ex1') in
                   let s1 = Smassign (scre2, ex2') in
                   let s2 = Shash (scre1, scre1, (Some scre2)) in
                   bind coq_Monad_optErr
                     (Obj.magic cont (Ederef (scre1, (Tpointer (Coq_stor,
                       ty)))) (add scr2 (S (S O)))) (fun res ->
                     let Coq_pair (s3, scr') = res in
                     ret coq_Monad_optErr (Coq_pair
                       ((sequentialize (Coq_cons (s0, (Coq_cons (s1,
                          (Coq_cons (s2, (Coq_cons (s3, Coq_nil))))))))),
                       scr')))
                 in
                 Obj.magic clocal_rvalue0 cont'' ex2 scr1
               in
               clocal_rvalue0 cont' ex1 scr
             | _ ->
               Error (String ((Ascii (Coq_true, Coq_true, Coq_false,
                 Coq_false, Coq_false, Coq_false, Coq_true, Coq_false)),
                 (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true,
                 Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
                 (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
                 Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
                 Coq_true, Coq_false, Coq_true, Coq_false, Coq_true,
                 Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
                 Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
                 Coq_false)), (String ((Ascii (Coq_false, Coq_false,
                 Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
                 Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
                 Coq_true, Coq_false, Coq_true, Coq_false, Coq_false)),
                 (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true,
                 Coq_true, Coq_true, Coq_false, Coq_false)), (String ((Ascii
                 (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
                 Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true,
                 Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
                 Coq_true, Coq_false)), (String ((Ascii (Coq_false,
                 Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
                 Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
                 Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
                 Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
                 Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
                 (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false,
                 Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
                 (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false,
                 Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
                 Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
                 Coq_false, Coq_false)), (String ((Ascii (Coq_false,
                 Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
                 Coq_false, Coq_false)), (String ((Ascii (Coq_false,
                 Coq_false, Coq_true, Coq_false, Coq_true, Coq_false,
                 Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
                 Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
                 Coq_false)), (String ((Ascii (Coq_true, Coq_false,
                 Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
                 Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
                 Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
                 (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false,
                 Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
                 (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
                 Coq_true, Coq_false, Coq_false)), (String ((Ascii
                 (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
                 Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
                 Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
                 Coq_false)), (String ((Ascii (Coq_false, Coq_false,
                 Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
                 Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
                 Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
                 (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true,
                 Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
                 (Coq_true, Coq_false, Coq_true, Coq_true, Coq_false,
                 Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
                 Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
                 Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
                 Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
                 Coq_false)), (String ((Ascii (Coq_false, Coq_false,
                 Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
                 Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
                 Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
                 (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
                 Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
                 (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
                 Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
                 Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
                 Coq_false)), (String ((Ascii (Coq_false, Coq_false,
                 Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
                 Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
                 Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
                 (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_true,
                 Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
                 (Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
                 Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
                 Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
                 Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
                 Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
                 Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
                 Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
                 (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
                 Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
                 (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
                 Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
                 Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
                 Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
                 Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
                 (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
                 Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
                 (Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
                 Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
                 Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
                 Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
                 Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
                 Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
                 Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
                 (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_true,
                 Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
                 (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
                 Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true,
                 Coq_true, Coq_false, Coq_false, Coq_false, Coq_true,
                 Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
                 Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
                 Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
                 Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
                 (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true,
                 Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
                 (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
                 Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
                 Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
                 Coq_true, Coq_false)), (String ((Ascii (Coq_false,
                 Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
                 Coq_false, Coq_false)), (String ((Ascii (Coq_false,
                 Coq_true, Coq_false, Coq_false, Coq_false, Coq_true,
                 Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
                 Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
                 Coq_false)), (String ((Ascii (Coq_false, Coq_false,
                 Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
                 Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
                 Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
                 (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
                 Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
                 (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
                 Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
                 Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
                 Coq_true, Coq_false)), (String ((Ascii (Coq_false,
                 Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
                 Coq_false, Coq_false)), (String ((Ascii (Coq_true,
                 Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
                 Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
                 Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
                 Coq_false)), (String ((Ascii (Coq_false, Coq_false,
                 Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
                 Coq_false)), (String ((Ascii (Coq_true, Coq_false,
                 Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
                 Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
                 Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
                 (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
                 Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
                 (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false,
                 Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
                 Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
                 Coq_false, Coq_false)), (String ((Ascii (Coq_false,
                 Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
                 Coq_false)), (String ((Ascii (Coq_true, Coq_false,
                 Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
                 Coq_false)), (String ((Ascii (Coq_false, Coq_false,
                 Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
                 Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
                 Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
                 (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
                 Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
                 (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
                 Coq_true, Coq_false, Coq_false)),
                 EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
          | _ ->
            Error (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii
              (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
              Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
              Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
              Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
              Coq_true, Coq_true, Coq_false, Coq_true, Coq_false,
              Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
              Coq_true, Coq_true, Coq_true, Coq_false, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true,
              Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_true, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
              Coq_false, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
              ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
              Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
              Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true,
              Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
              Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
              ((Ascii (Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
              ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
              Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_false,
              Coq_true, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
              Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true,
              Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
              Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
              ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
              ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
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
              Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_false,
              Coq_false)),
              EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
       | _ ->
         Error (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_true,
           Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
           (Coq_false, Coq_true, Coq_true, Coq_true, Coq_true, Coq_true,
           Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
           Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_true, Coq_false, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
           Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_true, Coq_false, Coq_true, Coq_false, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false,
           Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
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
           (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
           Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
           Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_false,
           Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
           Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
           (Coq_false, Coq_true, Coq_false, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
           Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
           Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_false, Coq_false)),
           EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
    | Efield (ex0, id, t0) ->
      (match t0 with
       | Tpointer (p, ty) ->
         (match p with
          | Coq_mem ->
            let cont' = fun ex' ->
              cont (Efield (ex', id, (Tpointer (Coq_mem, ty))))
            in
            clocal_lvalue0 cont' ex0 scr
          | _ ->
            Error (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii
              (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
              Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
              Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
              Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
              Coq_true, Coq_true, Coq_false, Coq_true, Coq_false,
              Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
              Coq_true, Coq_true, Coq_true, Coq_false, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true,
              Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_true, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
              Coq_false, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
              ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
              Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
              Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true,
              Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
              Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
              ((Ascii (Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
              ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
              Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_false,
              Coq_true, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
              Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true,
              Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
              Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
              ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
              ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
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
              Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_false,
              Coq_false)),
              EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
       | _ ->
         Error (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_true,
           Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
           (Coq_false, Coq_true, Coq_true, Coq_true, Coq_true, Coq_true,
           Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
           Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_true, Coq_false, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
           Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_true, Coq_false, Coq_true, Coq_false, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false,
           Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
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
           (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
           Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
           Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_false,
           Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
           Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
           (Coq_false, Coq_true, Coq_false, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
           Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
           Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_false, Coq_false)),
           EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
    | Eindex (ex1, ex2, t0) ->
      (match t0 with
       | Tpointer (p, ty) ->
         (match p with
          | Coq_mem ->
            let cont' = fun ex1' scr1 ->
              let cont'' = fun ex2' scr2 ->
                cont (Eindex (ex1', ex2', (Tpointer (Coq_mem, ty)))) scr2
              in
              clocal_rvalue0 cont'' ex2 scr1
            in
            clocal_lvalue0 cont' ex1 scr
          | _ ->
            Error (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii
              (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
              Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
              Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
              Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
              Coq_true, Coq_true, Coq_false, Coq_true, Coq_false,
              Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
              Coq_true, Coq_true, Coq_true, Coq_false, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true,
              Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_true, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
              Coq_false, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
              ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
              Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
              Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true,
              Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
              Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
              ((Ascii (Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
              ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
              Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
              Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_false,
              Coq_true, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
              Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true,
              Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
              Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
              ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
              ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
              Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
              Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
              Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
              ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
              Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
              Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
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
              Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_false,
              Coq_false)),
              EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
       | _ ->
         Error (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_true,
           Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
           (Coq_false, Coq_true, Coq_true, Coq_true, Coq_true, Coq_true,
           Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
           Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_true, Coq_false, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
           Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_true, Coq_false, Coq_true, Coq_false, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false,
           Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
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
           (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
           Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
           Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_false,
           Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
           Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
           (Coq_false, Coq_true, Coq_false, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
           Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
           Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_false, Coq_false)),
           EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
    | _ ->
      Error (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
        Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
        Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
        ((Ascii (Coq_true, Coq_false, Coq_true, Coq_true, Coq_false,
        Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_false,
        Coq_true, Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
        Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
        ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false,
        Coq_false, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true,
        Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
        Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
        Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
        ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
        Coq_false, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
        Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
        Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
        (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
        Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
        Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
        Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
        Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
        Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
        Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
        ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
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
        (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
        Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
        Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
        (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
        Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true,
        Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
        (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false,
        Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
        (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_true,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
        Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
        ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
        Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
        Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
        (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
        Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
        Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
        (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false, Coq_true, Coq_false, Coq_false)),
        (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
        Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_true, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
        Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
        Coq_true, Coq_false, Coq_false)),
        EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
  in clocal_lvalue0

(** val clocal_expr_list :
    (nat -> expr) -> (expr list -> nat -> (StmtClocal.statement, nat) prod
    optErr) -> expr list -> nat -> (StmtClocal.statement, nat) prod optErr **)

let rec clocal_expr_list scrmap cont exps scr =
  match exps with
  | Coq_nil -> cont Coq_nil O
  | Coq_cons (e, exps0) ->
    let cont' = fun exps' scr' ->
      let expr_cont = fun e' scr'' -> cont (Coq_cons (e', exps')) scr'' in
      clocal_rvalue scrmap expr_cont e scr'
    in
    clocal_expr_list scrmap cont' exps0 scr

(** val split_exps :
    nat -> expr list -> (expr list, expr list) prod optErr **)

let rec split_exps n exps =
  match n with
  | O -> ret (Obj.magic coq_Monad_optErr) (Coq_pair (Coq_nil, exps))
  | S n0 ->
    (match exps with
     | Coq_nil ->
       Error (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_true,
         Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii
         (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
         Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
         Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
         (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_true, Coq_false,
         Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true,
         Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
         Coq_true, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
         Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
         ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
         Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
         Coq_false)), EmptyString))))))))))))))))))))))))))))))))))
     | Coq_cons (e, exps0) ->
       bind (Obj.magic coq_Monad_optErr) (split_exps n0 exps0) (fun res ->
         let Coq_pair (l, r) = res in
         ret (Obj.magic coq_Monad_optErr) (Coq_pair ((Coq_cons (e, l)), r))))

(** val clocal_stm :
    (nat -> expr) -> statement -> (StmtClocal.statement, nat) prod optErr **)

let rec clocal_stm scrmap = function
| Sskip -> ret (Obj.magic coq_Monad_optErr) (Coq_pair (StmtClocal.Sskip, O))
| Sassign (lv, rv) ->
  (match typeof lv with
   | Tpointer (p, _) ->
     (match p with
      | Coq_mem ->
        let cont = fun lv' scr ->
          let cont' = fun rv' scr' ->
            ret coq_Monad_optErr (Coq_pair ((Smassign (lv', rv')), scr'))
          in
          clocal_rvalue scrmap (Obj.magic cont') rv scr
        in
        clocal_lvalue scrmap cont lv O
      | Coq_stor ->
        let cont = fun lv' scr ->
          let cont' = fun rv' scr' ->
            ret coq_Monad_optErr (Coq_pair ((Ssassign (lv', rv')), scr'))
          in
          clocal_rvalue scrmap (Obj.magic cont') rv scr
        in
        clocal_lvalue scrmap cont lv O
      | Coq_call ->
        Error (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true,
          Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii
          (Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_false,
          Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
          Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
          (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true,
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
          (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false,
          Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
          (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
          Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
          Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
          (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
          Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
          Coq_true, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
          Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
          Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
          ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
          Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
          Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
          Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
          Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
          ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
          Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
          Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
          Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
          Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
          ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
          Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
          Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
          Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
          Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
          ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
          Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
          Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
          Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
          Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
          EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
   | _ ->
     Error (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true,
       Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii
       (Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_false,
       Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
       Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
       (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false,
       Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
       Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
       Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
       Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
       (Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
       Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
       Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
       (Coq_true, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
       Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
       Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
       (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_false,
       Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
       Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
       Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_false,
       Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
       (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
       Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
       Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
       (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
       Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
       Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
       (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
       Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
       (Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
       Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
       Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
       (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
       Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
       Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
       (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
       Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
       Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
       (Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
       Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
       Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
       (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
       Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
       Coq_false, Coq_true, Coq_true, Coq_true, Coq_true, Coq_false)),
       (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
       Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
       Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
       EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
| Sset (id, rv) ->
  clocal_rvalue scrmap (fun rv' scr ->
    ret (Obj.magic coq_Monad_optErr) (Coq_pair ((StmtClocal.Sset (id, rv')),
      scr))) rv O
| Scall (id, label, args) ->
  let cont = fun args' scr ->
    ret coq_Monad_optErr (Coq_pair ((StmtClocal.Scall (id, label, args')),
      scr))
  in
  clocal_expr_list scrmap (Obj.magic cont) args O
| Ssequence (stm1, stm2) ->
  bind (Obj.magic coq_Monad_optErr) (clocal_stm scrmap stm1) (fun res1 ->
    let Coq_pair (seq1, scr1) = res1 in
    bind (Obj.magic coq_Monad_optErr) (clocal_stm scrmap stm2) (fun res2 ->
      let Coq_pair (seq2, scr2) = res2 in
      ret (Obj.magic coq_Monad_optErr) (Coq_pair ((StmtClocal.Ssequence
        (seq1, seq2)), (Nat.max scr1 scr2)))))
| Sifthenelse (ex, stm1, stm2) ->
  bind (Obj.magic coq_Monad_optErr) (clocal_stm scrmap stm1) (fun true_res ->
    let Coq_pair (true_stm, scr1) = true_res in
    bind (Obj.magic coq_Monad_optErr) (clocal_stm scrmap stm2)
      (fun false_res ->
      let Coq_pair (false_stm, scr2) = false_res in
      let scr = Nat.max scr1 scr2 in
      clocal_rvalue scrmap (fun ex' scr' ->
        ret (Obj.magic coq_Monad_optErr) (Coq_pair ((StmtClocal.Sifthenelse
          (ex', true_stm, false_stm)), (Nat.max scr scr')))) ex O))
| Sloop loop ->
  bind (Obj.magic coq_Monad_optErr) (clocal_stm scrmap loop) (fun res ->
    let Coq_pair (loop0, scr) = res in
    ret (Obj.magic coq_Monad_optErr) (Coq_pair ((StmtClocal.Sloop loop0),
      scr)))
| Sbreak -> ret (Obj.magic coq_Monad_optErr) (Coq_pair (StmtClocal.Sbreak, O))
| Sreturn retvar ->
  ret (Obj.magic coq_Monad_optErr) (Coq_pair ((StmtClocal.Sreturn retvar), O))
| Stransfer (addr, val0) ->
  let cont = fun addr' scr ->
    let cont' = fun val' scr' ->
      ret coq_Monad_optErr (Coq_pair ((StmtClocal.Stransfer (addr', val')),
        scr'))
    in
    clocal_rvalue scrmap (Obj.magic cont') val0 scr
  in
  clocal_rvalue scrmap cont addr O
| Scallmethod (addr, retvals, funsig, val0, args) ->
  let cont = fun all_exps scr ->
    match all_exps with
    | Coq_nil ->
      Error (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii
        (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
        Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
        Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
        Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
        ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true,
        Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
        (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
        ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false,
        Coq_false, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true,
        Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
        Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false,
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
        EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))
    | Coq_cons (_, l) ->
      (match l with
       | Coq_nil ->
         Error (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false,
           Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
           Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
           Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
           Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
           Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
           Coq_true, Coq_true, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_false,
           Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
           Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true,
           Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
           (Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
           Coq_true, Coq_false)),
           EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))
       | Coq_cons (_, _) ->
         ret coq_Monad_optErr (Coq_pair ((StmtClocal.Scallmethod (addr,
           retvals, funsig, val0, args)), scr)))
  in
  clocal_expr_list scrmap (Obj.magic cont) (Coq_cons (addr, (Coq_cons (val0,
    args)))) O
| Slog (topics, args) ->
  let cont = fun all_exps scr ->
    bind coq_Monad_optErr (Obj.magic split_exps (length topics) all_exps)
      (fun res ->
      let Coq_pair (topics', args') = res in
      ret coq_Monad_optErr (Coq_pair ((StmtClocal.Slog (topics', args')),
        scr)))
  in
  clocal_expr_list scrmap (Obj.magic cont) (app topics args) O
| Srevert ->
  ret (Obj.magic coq_Monad_optErr) (Coq_pair (StmtClocal.Srevert, O))

(** val max_id : (ident, coq_type) prod list -> nat **)

let rec max_id = function
| Coq_nil -> O
| Coq_cons (p, locs0) ->
  let Coq_pair (id, _) = p in Nat.max (Pos.to_nat id) (max_id locs0)

(** val make_scrmap : nat -> nat -> expr **)

let make_scrmap base n =
  Evar ((Pos.of_nat (add base n)), (Tpointer (Coq_mem, (Tpointer (Coq_stor,
    Tvoid)))))

(** val ptr_type : coq_type **)

let ptr_type =
  Tpointer (Coq_mem, (Tpointer (Coq_stor, Tvoid)))

(** val extend_locs :
    nat -> nat -> (ident, coq_type) prod list -> (ident, coq_type) prod list **)

let rec extend_locs n base locs =
  match n with
  | O -> locs
  | S n0 ->
    Coq_cons ((Coq_pair ((Pos.of_nat (add base n0)), ptr_type)),
      (extend_locs n0 base locs))

(** val clocal_function : coq_function -> StmtClocal.coq_function optErr **)

let clocal_function f =
  let base = add (max_id f.fn_locals) (S O) in
  bind (Obj.magic coq_Monad_optErr)
    (Obj.magic clocal_stm (make_scrmap base) f.fn_body) (fun res ->
    let Coq_pair (stm, n) = res in
    let locs = extend_locs n base f.fn_locals in
    ret (Obj.magic coq_Monad_optErr) { StmtClocal.fn_return = f.fn_return;
      StmtClocal.fn_params = f.fn_params; StmtClocal.fn_temps = f.fn_temps;
      StmtClocal.fn_locals = locs; StmtClocal.fn_body = stm })

(** val clocal_constructor :
    coq_function option -> StmtClocal.coq_function option optErr **)

let clocal_constructor = function
| Some f0 ->
  bind (Obj.magic coq_Monad_optErr) (Obj.magic clocal_function f0) (fun f1 ->
    ret (Obj.magic coq_Monad_optErr) (Some f1))
| None -> ret (Obj.magic coq_Monad_optErr) None

(** val clocal_functions :
    coq_function PTree.t -> StmtClocal.coq_function PTree.t optErr **)

let clocal_functions defs =
  transl_tree clocal_function defs

(** val clocal_methoddefs :
    coq_function option IntMap.t -> StmtClocal.coq_function option IntMap.t
    optErr **)

let clocal_methoddefs defs =
  transl_map clocal_function defs

(** val clocal_genv : genv -> StmtClocal.genv optErr **)

let clocal_genv ge =
  let vars = ge.Genv.genv_vars in
  let defs = ge.Genv.genv_defs in
  let names = ge.Genv.genv_funcs in
  let functions = ge.Genv.genv_fundefs in
  let sigs = ge.Genv.genv_methods in
  let methoddefs = ge.Genv.genv_methoddefs in
  let constructor = ge.Genv.genv_constructor in
  bind (Obj.magic coq_Monad_optErr) (Obj.magic clocal_functions functions)
    (fun functions0 ->
    bind (Obj.magic coq_Monad_optErr)
      (Obj.magic clocal_methoddefs methoddefs) (fun methoddefs0 ->
      bind (Obj.magic coq_Monad_optErr)
        (Obj.magic clocal_constructor constructor) (fun constructor0 ->
        ret (Obj.magic coq_Monad_optErr) { Genv.genv_vars = vars;
          Genv.genv_funcs = names; Genv.genv_methods = sigs; Genv.genv_defs =
          defs; Genv.genv_fundefs = functions0; Genv.genv_methoddefs =
          methoddefs0; Genv.genv_constructor = constructor0 })))
