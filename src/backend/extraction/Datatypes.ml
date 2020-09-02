
type coq_unit =
| Coq_tt

type bool =
| Coq_true
| Coq_false

(** val negb : bool -> bool **)

let negb = function
| Coq_true -> Coq_false
| Coq_false -> Coq_true

type nat =
| O
| S of nat

type 'a option =
| Some of 'a
| None

type ('a, 'b) sum =
| Coq_inl of 'a
| Coq_inr of 'b

type ('a, 'b) prod =
| Coq_pair of 'a * 'b

(** val fst : ('a1, 'a2) prod -> 'a1 **)

let fst = function
| Coq_pair (x, _) -> x

(** val snd : ('a1, 'a2) prod -> 'a2 **)

let snd = function
| Coq_pair (_, y) -> y

type 'a list =
| Coq_nil
| Coq_cons of 'a * 'a list

(** val length : 'a1 list -> nat **)

let rec length = function
| Coq_nil -> O
| Coq_cons (_, l') -> S (length l')

(** val app : 'a1 list -> 'a1 list -> 'a1 list **)

let rec app l m =
  match l with
  | Coq_nil -> m
  | Coq_cons (a, l1) -> Coq_cons (a, (app l1 m))

type comparison =
| Eq
| Lt
| Gt

(** val coq_CompOpp : comparison -> comparison **)

let coq_CompOpp = function
| Eq -> Eq
| Lt -> Gt
| Gt -> Lt
