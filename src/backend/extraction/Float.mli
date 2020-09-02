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
 end

module Make :
 functor (R:Rep) ->
 S

module F32 :
 sig
  type t

  val bitwidth : nat

  val zero : t
 end

module F64 :
 sig
  type t

  val bitwidth : nat

  val zero : t
 end
