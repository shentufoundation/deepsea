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

  val one : t

  val from_nat : nat -> t

  val to_nat : t -> nat
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

  (** val one : t **)

  let one =
    S O

  (** val from_nat : nat -> t **)

  let from_nat n =
    n

  (** val to_nat : t -> nat **)

  let to_nat n =
    n
 end

module I32 = Make(Rep32)

module I64 = Make(Rep64)
