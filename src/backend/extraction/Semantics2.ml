open AST
open BinNums
open Cop
open Coqlib
open Datatypes
open Globalenvs
open Integers
open Labels
open List0
open LowValues
open MachineModel
open Maps0
open PeanoNat
open Specif
open StmtExpressionless

(** val code_labels : code -> label list **)

let rec code_labels = function
| Coq_nil -> Coq_nil
| Coq_cons (s, rest) ->
  let lr = code_labels rest in
  (match s with
   | Slabel l -> Coq_cons (l, lr)
   | _ -> lr)

(** val function_labels : coq_function -> label list **)

let function_labels f =
  code_labels (fn_code f)

(** val genv_list_labels : genv -> label list **)

let genv_list_labels ge =
  flat_map function_labels (Genv.all_functions ge)

(** val stm_eq_dec : statement -> statement -> sumbool **)

let stm_eq_dec stm stm' =
  match stm with
  | Spush x ->
    (match stm' with
     | Spush s0 ->
       (match x with
        | Coq_inl x0 ->
          (match s0 with
           | Coq_inl v ->
             let rec f v0 x1 =
               match v0 with
               | Vunit -> (match x1 with
                           | Vunit -> Coq_left
                           | _ -> Coq_right)
               | Vint i ->
                 (match x1 with
                  | Vint i0 -> Int256.eq_dec i i0
                  | _ -> Coq_right)
               | Vhash v1 ->
                 (match x1 with
                  | Vhash v2 -> f v1 v2
                  | _ -> Coq_right)
               | Vhash2 (v1, v2) ->
                 (match x1 with
                  | Vhash2 (v3, v4) ->
                    (match f v1 v3 with
                     | Coq_left -> f v2 v4
                     | Coq_right -> Coq_right)
                  | _ -> Coq_right)
             in f x0 v
           | Coq_inr _ -> Coq_right)
        | Coq_inr x0 ->
          (match s0 with
           | Coq_inl _ -> Coq_right
           | Coq_inr l ->
             let rec f p x1 =
               match p with
               | Coq_xI p0 ->
                 (match x1 with
                  | Coq_xI p1 -> f p0 p1
                  | _ -> Coq_right)
               | Coq_xO p0 ->
                 (match x1 with
                  | Coq_xO p1 -> f p0 p1
                  | _ -> Coq_right)
               | Coq_xH -> (match x1 with
                            | Coq_xH -> Coq_left
                            | _ -> Coq_right)
             in f x0 l))
     | _ -> Coq_right)
  | Sdup x -> (match stm' with
               | Sdup n0 -> Nat.eq_dec x n0
               | _ -> Coq_right)
  | Ssload -> (match stm' with
               | Ssload -> Coq_left
               | _ -> Coq_right)
  | Smload -> (match stm' with
               | Smload -> Coq_left
               | _ -> Coq_right)
  | Sunop x ->
    (match stm' with
     | Sunop u0 ->
       (match x with
        | Onotbool -> (match u0 with
                       | Onotbool -> Coq_left
                       | _ -> Coq_right)
        | Onotint -> (match u0 with
                      | Onotint -> Coq_left
                      | _ -> Coq_right)
        | Oneg -> (match u0 with
                   | Oneg -> Coq_left
                   | _ -> Coq_right)
        | Osha_1 -> (match u0 with
                     | Osha_1 -> Coq_left
                     | _ -> Coq_right))
     | _ -> Coq_right)
  | Sbinop (x, x0) ->
    (match stm' with
     | Sbinop (b1, b2) ->
       (match x with
        | Oadd ->
          (match b1 with
           | Oadd ->
             (match x0 with
              | Coq_true ->
                (match b2 with
                 | Coq_true -> Coq_left
                 | Coq_false -> Coq_right)
              | Coq_false ->
                (match b2 with
                 | Coq_true -> Coq_right
                 | Coq_false -> Coq_left))
           | _ -> Coq_right)
        | Osub ->
          (match b1 with
           | Osub ->
             (match x0 with
              | Coq_true ->
                (match b2 with
                 | Coq_true -> Coq_left
                 | Coq_false -> Coq_right)
              | Coq_false ->
                (match b2 with
                 | Coq_true -> Coq_right
                 | Coq_false -> Coq_left))
           | _ -> Coq_right)
        | Omul ->
          (match b1 with
           | Omul ->
             (match x0 with
              | Coq_true ->
                (match b2 with
                 | Coq_true -> Coq_left
                 | Coq_false -> Coq_right)
              | Coq_false ->
                (match b2 with
                 | Coq_true -> Coq_right
                 | Coq_false -> Coq_left))
           | _ -> Coq_right)
        | Odiv ->
          (match b1 with
           | Odiv ->
             (match x0 with
              | Coq_true ->
                (match b2 with
                 | Coq_true -> Coq_left
                 | Coq_false -> Coq_right)
              | Coq_false ->
                (match b2 with
                 | Coq_true -> Coq_right
                 | Coq_false -> Coq_left))
           | _ -> Coq_right)
        | Omod ->
          (match b1 with
           | Omod ->
             (match x0 with
              | Coq_true ->
                (match b2 with
                 | Coq_true -> Coq_left
                 | Coq_false -> Coq_right)
              | Coq_false ->
                (match b2 with
                 | Coq_true -> Coq_right
                 | Coq_false -> Coq_left))
           | _ -> Coq_right)
        | Oexp ->
          (match b1 with
           | Oexp ->
             (match x0 with
              | Coq_true ->
                (match b2 with
                 | Coq_true -> Coq_left
                 | Coq_false -> Coq_right)
              | Coq_false ->
                (match b2 with
                 | Coq_true -> Coq_right
                 | Coq_false -> Coq_left))
           | _ -> Coq_right)
        | Oand ->
          (match b1 with
           | Oand ->
             (match x0 with
              | Coq_true ->
                (match b2 with
                 | Coq_true -> Coq_left
                 | Coq_false -> Coq_right)
              | Coq_false ->
                (match b2 with
                 | Coq_true -> Coq_right
                 | Coq_false -> Coq_left))
           | _ -> Coq_right)
        | Oor ->
          (match b1 with
           | Oor ->
             (match x0 with
              | Coq_true ->
                (match b2 with
                 | Coq_true -> Coq_left
                 | Coq_false -> Coq_right)
              | Coq_false ->
                (match b2 with
                 | Coq_true -> Coq_right
                 | Coq_false -> Coq_left))
           | _ -> Coq_right)
        | Oxor ->
          (match b1 with
           | Oxor ->
             (match x0 with
              | Coq_true ->
                (match b2 with
                 | Coq_true -> Coq_left
                 | Coq_false -> Coq_right)
              | Coq_false ->
                (match b2 with
                 | Coq_true -> Coq_right
                 | Coq_false -> Coq_left))
           | _ -> Coq_right)
        | Oshl ->
          (match b1 with
           | Oshl ->
             (match x0 with
              | Coq_true ->
                (match b2 with
                 | Coq_true -> Coq_left
                 | Coq_false -> Coq_right)
              | Coq_false ->
                (match b2 with
                 | Coq_true -> Coq_right
                 | Coq_false -> Coq_left))
           | _ -> Coq_right)
        | Oshr ->
          (match b1 with
           | Oshr ->
             (match x0 with
              | Coq_true ->
                (match b2 with
                 | Coq_true -> Coq_left
                 | Coq_false -> Coq_right)
              | Coq_false ->
                (match b2 with
                 | Coq_true -> Coq_right
                 | Coq_false -> Coq_left))
           | _ -> Coq_right)
        | Oeq ->
          (match b1 with
           | Oeq ->
             (match x0 with
              | Coq_true ->
                (match b2 with
                 | Coq_true -> Coq_left
                 | Coq_false -> Coq_right)
              | Coq_false ->
                (match b2 with
                 | Coq_true -> Coq_right
                 | Coq_false -> Coq_left))
           | _ -> Coq_right)
        | One ->
          (match b1 with
           | One ->
             (match x0 with
              | Coq_true ->
                (match b2 with
                 | Coq_true -> Coq_left
                 | Coq_false -> Coq_right)
              | Coq_false ->
                (match b2 with
                 | Coq_true -> Coq_right
                 | Coq_false -> Coq_left))
           | _ -> Coq_right)
        | Olt ->
          (match b1 with
           | Olt ->
             (match x0 with
              | Coq_true ->
                (match b2 with
                 | Coq_true -> Coq_left
                 | Coq_false -> Coq_right)
              | Coq_false ->
                (match b2 with
                 | Coq_true -> Coq_right
                 | Coq_false -> Coq_left))
           | _ -> Coq_right)
        | Ogt ->
          (match b1 with
           | Ogt ->
             (match x0 with
              | Coq_true ->
                (match b2 with
                 | Coq_true -> Coq_left
                 | Coq_false -> Coq_right)
              | Coq_false ->
                (match b2 with
                 | Coq_true -> Coq_right
                 | Coq_false -> Coq_left))
           | _ -> Coq_right)
        | Ole ->
          (match b1 with
           | Ole ->
             (match x0 with
              | Coq_true ->
                (match b2 with
                 | Coq_true -> Coq_left
                 | Coq_false -> Coq_right)
              | Coq_false ->
                (match b2 with
                 | Coq_true -> Coq_right
                 | Coq_false -> Coq_left))
           | _ -> Coq_right)
        | Oge ->
          (match b1 with
           | Oge ->
             (match x0 with
              | Coq_true ->
                (match b2 with
                 | Coq_true -> Coq_left
                 | Coq_false -> Coq_right)
              | Coq_false ->
                (match b2 with
                 | Coq_true -> Coq_right
                 | Coq_false -> Coq_left))
           | _ -> Coq_right)
        | Osha_2 ->
          (match b1 with
           | Osha_2 ->
             (match x0 with
              | Coq_true ->
                (match b2 with
                 | Coq_true -> Coq_left
                 | Coq_false -> Coq_right)
              | Coq_false ->
                (match b2 with
                 | Coq_true -> Coq_right
                 | Coq_false -> Coq_left))
           | _ -> Coq_right))
     | _ -> Coq_right)
  | Scall0 x ->
    (match stm' with
     | Scall0 b0 ->
       (match x with
        | Baddress -> (match b0 with
                       | Baddress -> Coq_left
                       | _ -> Coq_right)
        | Borigin -> (match b0 with
                      | Borigin -> Coq_left
                      | _ -> Coq_right)
        | Bcaller -> (match b0 with
                      | Bcaller -> Coq_left
                      | _ -> Coq_right)
        | Bcallvalue ->
          (match b0 with
           | Bcallvalue -> Coq_left
           | _ -> Coq_right)
        | Bcoinbase -> (match b0 with
                        | Bcoinbase -> Coq_left
                        | _ -> Coq_right)
        | Btimestamp ->
          (match b0 with
           | Btimestamp -> Coq_left
           | _ -> Coq_right)
        | Bnumber -> (match b0 with
                      | Bnumber -> Coq_left
                      | _ -> Coq_right)
        | Bchainid -> (match b0 with
                       | Bchainid -> Coq_left
                       | _ -> Coq_right)
        | Bselfbalance ->
          (match b0 with
           | Bselfbalance -> Coq_left
           | _ -> Coq_right))
     | _ -> Coq_right)
  | Scall1 x ->
    (match stm' with
     | Scall1 b0 ->
       (match x with
        | Bbalance ->
          (match b0 with
           | Bbalance -> Coq_left
           | Bblockhash -> Coq_right)
        | Bblockhash ->
          (match b0 with
           | Bbalance -> Coq_right
           | Bblockhash -> Coq_left))
     | _ -> Coq_right)
  | Sskip -> (match stm' with
              | Sskip -> Coq_left
              | _ -> Coq_right)
  | Spop -> (match stm' with
             | Spop -> Coq_left
             | _ -> Coq_right)
  | Ssstore -> (match stm' with
                | Ssstore -> Coq_left
                | _ -> Coq_right)
  | Smstore -> (match stm' with
                | Smstore -> Coq_left
                | _ -> Coq_right)
  | Sswap x -> (match stm' with
                | Sswap n0 -> Nat.eq_dec x n0
                | _ -> Coq_right)
  | Sdone x ->
    (match stm' with
     | Sdone r0 ->
       (match x with
        | Tvoid_method ->
          (match r0 with
           | Tvoid_method -> Coq_left
           | _ -> Coq_right)
        | Tconstructor ->
          (match r0 with
           | Tconstructor -> Coq_left
           | _ -> Coq_right)
        | Tfun -> (match r0 with
                   | Tfun -> Coq_left
                   | _ -> Coq_right)
        | Tsome_method ->
          (match r0 with
           | Tsome_method -> Coq_left
           | _ -> Coq_right))
     | _ -> Coq_right)
  | Slabel x -> (match stm' with
                 | Slabel l0 -> peq x l0
                 | _ -> Coq_right)
  | Sjump -> (match stm' with
              | Sjump -> Coq_left
              | _ -> Coq_right)
  | Sjumpi -> (match stm' with
               | Sjumpi -> Coq_left
               | _ -> Coq_right)
  | Shash -> (match stm' with
              | Shash -> Coq_left
              | _ -> Coq_right)
  | Stransfer -> (match stm' with
                  | Stransfer -> Coq_left
                  | _ -> Coq_right)
  | Scallmethod (x, x0, x1) ->
    (match stm' with
     | Scallmethod (i0, n1, n2) ->
       (match Int.eq_dec x i0 with
        | Coq_left ->
          (match Nat.eq_dec x0 n1 with
           | Coq_left -> Nat.eq_dec x1 n2
           | Coq_right -> Coq_right)
        | Coq_right -> Coq_right)
     | _ -> Coq_right)
  | Slog (x, x0) ->
    (match stm' with
     | Slog (n1, n2) ->
       (match Nat.eq_dec x n1 with
        | Coq_left -> Nat.eq_dec x0 n2
        | Coq_right -> Coq_right)
     | _ -> Coq_right)
  | Srevert -> (match stm' with
                | Srevert -> Coq_left
                | _ -> Coq_right)
  | Scalldataload ->
    (match stm' with
     | Scalldataload -> Coq_left
     | _ -> Coq_right)
  | Sconstructordataload x ->
    (match stm' with
     | Sconstructordataload n0 -> Nat.eq_dec x n0
     | _ -> Coq_right)

(** val function_starts_with_label : (label, coq_function) prod -> bool **)

let function_starts_with_label lf =
  match fn_code (snd lf) with
  | Coq_nil -> Coq_false
  | Coq_cons (start, _) ->
    (match stm_eq_dec start (Slabel (fst lf)) with
     | Coq_left -> Coq_true
     | Coq_right -> Coq_false)

(** val all_true : bool list -> bool **)

let rec all_true = function
| Coq_nil -> Coq_true
| Coq_cons (b, rest) ->
  (match b with
   | Coq_true -> all_true rest
   | Coq_false -> Coq_false)

(** val functions_start_with_labels :
    (label, coq_function) prod list -> bool **)

let functions_start_with_labels lfs =
  all_true (map function_starts_with_label lfs)

(** val label_verify : genv -> bool **)

let label_verify ge =
  match decide_label_norepet (genv_list_labels ge) with
  | Coq_true ->
    functions_start_with_labels (PTree.elements ge.Genv.genv_fundefs)
  | Coq_false -> Coq_false
