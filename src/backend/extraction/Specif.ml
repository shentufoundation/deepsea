
type 'a coq_sig = 'a
  (* singleton inductive, whose constructor was exist *)

type ('a, 'p) sigT =
| Coq_existT of 'a * 'p

(** val projT1 : ('a1, 'a2) sigT -> 'a1 **)

let projT1 = function
| Coq_existT (a, _) -> a

(** val projT2 : ('a1, 'a2) sigT -> 'a2 **)

let projT2 = function
| Coq_existT (_, h) -> h

type sumbool =
| Coq_left
| Coq_right
