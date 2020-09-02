open BinNums
open Coqlib
open Specif

type ident = positive

(** val ident_eq : positive -> positive -> sumbool **)

let ident_eq =
  peq

type label = ident
