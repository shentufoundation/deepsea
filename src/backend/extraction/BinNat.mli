open BinNums
open BinPos
open Datatypes

module N :
 sig
  val succ_double : coq_N -> coq_N

  val double : coq_N -> coq_N

  val succ_pos : coq_N -> positive

  val sub : coq_N -> coq_N -> coq_N

  val compare : coq_N -> coq_N -> comparison

  val leb : coq_N -> coq_N -> bool

  val pos_div_eucl : positive -> coq_N -> (coq_N, coq_N) prod

  val coq_lor : coq_N -> coq_N -> coq_N

  val coq_land : coq_N -> coq_N -> coq_N

  val ldiff : coq_N -> coq_N -> coq_N

  val coq_lxor : coq_N -> coq_N -> coq_N

  val testbit : coq_N -> coq_N -> bool
 end
