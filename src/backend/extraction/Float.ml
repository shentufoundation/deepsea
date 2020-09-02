open Datatypes

module type Rep =
 sig
  val bitwidth : nat
 end

module Rep32 =
 struct
  (** val bitwidth : nat **)

  let bitwidth =
    S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
      (S (S (S (S (S (S (S O)))))))))))))))))))))))))))))))
 end

module Rep64 =
 struct
  (** val bitwidth : nat **)

  let bitwidth =
    S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
      (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
      (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
      O)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 end

module type S =
 sig
  type t

  val bitwidth : nat

  val zero : t
 end

module Make =
 functor (R:Rep) ->
 struct
  (** val bitwidth : nat **)

  let bitwidth =
    R.bitwidth

  type t = nat

  (** val zero : t **)

  let zero =
    O
 end

module F32 = Make(Rep32)

module F64 = Make(Rep64)
