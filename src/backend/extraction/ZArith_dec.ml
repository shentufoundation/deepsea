open BinInt
open BinNums
open Datatypes
open Specif

(** val coq_Z_lt_dec : coq_Z -> coq_Z -> sumbool **)

let coq_Z_lt_dec x y =
  match Z.compare x y with
  | Lt -> Coq_left
  | _ -> Coq_right

(** val coq_Z_le_dec : coq_Z -> coq_Z -> sumbool **)

let coq_Z_le_dec x y =
  match Z.compare x y with
  | Gt -> Coq_right
  | _ -> Coq_left

(** val coq_Z_le_gt_dec : coq_Z -> coq_Z -> sumbool **)

let coq_Z_le_gt_dec =
  coq_Z_le_dec
