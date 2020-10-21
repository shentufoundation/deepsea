open BinInt
open BinNums
open Datatypes
open Integers

(** val public_retval_base : coq_Z **)

let public_retval_base =
  Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH))))))

(** val abi_word_size : coq_Z **)

let abi_word_size =
  Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))

(** val funsig_aligned : Int.int -> Int256.int **)

let funsig_aligned funsig =
  let funsig0 = Int.unsigned funsig in
  Int256.repr
    (Z.shiftl funsig0 (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xI
      (Coq_xI Coq_xH)))))))))

(** val public_funsig_pos : nat -> coq_Z **)

let public_funsig_pos retval_count =
  Z.add public_retval_base (Z.mul abi_word_size (Z.of_nat retval_count))

(** val public_arg_pos : nat -> nat -> coq_Z **)

let public_arg_pos index retval_count =
  Z.add
    (Z.add (public_funsig_pos retval_count)
      (Z.mul (Z.of_nat index) abi_word_size)) (Zpos (Coq_xO (Coq_xO Coq_xH)))

(** val public_arg_size : nat -> coq_Z **)

let public_arg_size arg_count =
  Z.add (Z.mul (Z.of_nat arg_count) abi_word_size) (Zpos (Coq_xO (Coq_xO
    Coq_xH)))

(** val argpos : nat -> coq_Z **)

let argpos =
  public_funsig_pos

(** val arglen : nat -> coq_Z **)

let arglen =
  public_arg_size

(** val retpos : coq_Z **)

let retpos =
  public_retval_base

(** val retlen : nat -> coq_Z **)

let retlen retval_count =
  Z.mul abi_word_size (Z.of_nat retval_count)

(** val call_data_arg_location : nat -> Int256.int **)

let call_data_arg_location argi =
  Int256.repr
    (Z.add
      (Z.mul (Z.of_nat argi) (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
        Coq_xH))))))) (Zpos (Coq_xO (Coq_xO Coq_xH))))
