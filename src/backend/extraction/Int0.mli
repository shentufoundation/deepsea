open Datatypes

module type Rep =
 sig
  val bitwidth : nat
 end

module Rep32 :
 sig
  val bitwidth : nat
 end

module Rep64 :
 sig
  val bitwidth : nat
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

module Make :
 functor (R:Rep) ->
 S

module I32 :
 sig
  type t

  val bitwidth : nat

  val zero : t

  val one : t

  val from_nat : nat -> t

  val to_nat : t -> nat
 end

module I64 :
 sig
  type t

  val bitwidth : nat

  val zero : t

  val one : t

  val from_nat : nat -> t

  val to_nat : t -> nat
 end
