open BinInt
open BinNums
open BinPos
open Datatypes
open List0
open Specif
open ZArith_dec

val peq : positive -> positive -> sumbool

val plt : positive -> positive -> sumbool

val zeq : coq_Z -> coq_Z -> sumbool

val zlt : coq_Z -> coq_Z -> sumbool

val zle : coq_Z -> coq_Z -> sumbool

val option_map : ('a1 -> 'a2) -> 'a1 option -> 'a2 option

val list_fold_left : ('a1 -> 'a2 -> 'a2) -> 'a2 -> 'a1 list -> 'a2

val list_fold_right : ('a1 -> 'a2 -> 'a2) -> 'a1 list -> 'a2 -> 'a2

val proj_sumbool : sumbool -> bool
