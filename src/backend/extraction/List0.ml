open Datatypes
open Specif

(** val in_dec : ('a1 -> 'a1 -> sumbool) -> 'a1 -> 'a1 list -> sumbool **)

let rec in_dec h a = function
| Coq_nil -> Coq_right
| Coq_cons (y, l0) ->
  let s = h y a in
  (match s with
   | Coq_left -> Coq_left
   | Coq_right -> in_dec h a l0)

(** val rev : 'a1 list -> 'a1 list **)

let rec rev = function
| Coq_nil -> Coq_nil
| Coq_cons (x, l') -> app (rev l') (Coq_cons (x, Coq_nil))

(** val rev_append : 'a1 list -> 'a1 list -> 'a1 list **)

let rec rev_append l l' =
  match l with
  | Coq_nil -> l'
  | Coq_cons (a, l0) -> rev_append l0 (Coq_cons (a, l'))

(** val rev' : 'a1 list -> 'a1 list **)

let rev' l =
  rev_append l Coq_nil

(** val map : ('a1 -> 'a2) -> 'a1 list -> 'a2 list **)

let rec map f = function
| Coq_nil -> Coq_nil
| Coq_cons (a, t) -> Coq_cons ((f a), (map f t))

(** val flat_map : ('a1 -> 'a2 list) -> 'a1 list -> 'a2 list **)

let rec flat_map f = function
| Coq_nil -> Coq_nil
| Coq_cons (x, t) -> app (f x) (flat_map f t)

(** val fold_left : ('a1 -> 'a2 -> 'a1) -> 'a2 list -> 'a1 -> 'a1 **)

let rec fold_left f l a0 =
  match l with
  | Coq_nil -> a0
  | Coq_cons (b, t) -> fold_left f t (f a0 b)
