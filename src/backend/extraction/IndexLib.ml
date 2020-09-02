open BinNums
open Datatypes
open Integers
open Nat0

(** val sparse_positive : positive -> positive **)

let rec sparse_positive = function
| Coq_xI y -> Coq_xO (Coq_xI (sparse_positive y))
| Coq_xO y -> Coq_xO (Coq_xO (sparse_positive y))
| Coq_xH -> Coq_xO Coq_xH

(** val delay_sparse : positive -> positive **)

let delay_sparse = function
| Coq_xI r -> Coq_xI (sparse_positive r)
| Coq_xO r -> Coq_xO (sparse_positive r)
| Coq_xH -> Coq_xH

(** val pos_measure : positive -> nat **)

let rec pos_measure = function
| Coq_xI r -> S (pos_measure r)
| Coq_xO r -> S (pos_measure r)
| Coq_xH -> S O

(** val pick_first : positive -> positive -> nat -> positive **)

let rec pick_first x y = function
| O -> Coq_xH
| S n ->
  (match x with
   | Coq_xI r -> Coq_xI (pick_first y r n)
   | Coq_xO r -> Coq_xO (pick_first y r n)
   | Coq_xH -> Coq_xI (delay_sparse y))

(** val inject_positive : positive -> positive -> positive **)

let inject_positive x y =
  pick_first x y (add (pos_measure x) (pos_measure y))

(** val int32_index : Int.int -> positive **)

let int32_index = function
| Z0 -> Coq_xH
| Zpos p -> Coq_xO p
| Zneg p -> Coq_xI p
