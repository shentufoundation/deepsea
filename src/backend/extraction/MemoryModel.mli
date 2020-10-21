open BinInt
open BinNums
open Datatypes
open Integers

val public_retval_base : coq_Z

val abi_word_size : coq_Z

val funsig_aligned : Int.int -> Int256.int

val public_funsig_pos : nat -> coq_Z

val public_arg_pos : nat -> nat -> coq_Z

val public_arg_size : nat -> coq_Z

val argpos : nat -> coq_Z

val arglen : nat -> coq_Z

val retpos : coq_Z

val retlen : nat -> coq_Z

val call_data_arg_location : nat -> Int256.int
