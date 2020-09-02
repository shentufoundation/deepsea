open BinInt
open BinNums
open BinPos
open Datatypes
open List0
open Specif
open ZArith_dec

(** val peq : positive -> positive -> sumbool **)

let peq =
  Pos.eq_dec

(** val plt : positive -> positive -> sumbool **)

let plt x y =
  let c = Pos.compare x y in (match c with
                              | Lt -> Coq_left
                              | _ -> Coq_right)

(** val zeq : coq_Z -> coq_Z -> sumbool **)

let zeq =
  Z.eq_dec

(** val zlt : coq_Z -> coq_Z -> sumbool **)

let zlt =
  coq_Z_lt_dec

(** val zle : coq_Z -> coq_Z -> sumbool **)

let zle =
  coq_Z_le_gt_dec

(** val option_map : ('a1 -> 'a2) -> 'a1 option -> 'a2 option **)

let option_map f = function
| Some y -> Some (f y)
| None -> None

(** val list_fold_left : ('a1 -> 'a2 -> 'a2) -> 'a2 -> 'a1 list -> 'a2 **)

let rec list_fold_left f accu = function
| Coq_nil -> accu
| Coq_cons (x, l') -> list_fold_left f (f x accu) l'

(** val list_fold_right : ('a1 -> 'a2 -> 'a2) -> 'a1 list -> 'a2 -> 'a2 **)

let list_fold_right f l base =
  list_fold_left f base (rev' l)

(** val proj_sumbool : sumbool -> bool **)

let proj_sumbool = function
| Coq_left -> Coq_true
| Coq_right -> Coq_false
