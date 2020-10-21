open AST
open BinInt
open BinNums
open Ctypes
open Datatypes
open Integers

(** val sp : Int256.int **)

let sp =
  Int256.repr
    (Z.add (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
      (Coq_xO (Coq_xI (Coq_xO (Coq_xO Coq_xH)))))))))))) (Zpos (Coq_xO
      (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))))

(** val sb : Int256.int **)

let sb =
  Int256.repr
    (Z.add (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
      (Coq_xO (Coq_xI (Coq_xO (Coq_xO Coq_xH)))))))))))) (Zpos (Coq_xO
      (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))))

(** val sizeof : coq_type -> Int256.int **)

let rec sizeof ty =
  Int256.repr (sizeof_words ty)

(** val offset : fieldlist -> ident -> Int256.int option **)

let rec offset fl id =
  match struct_field fl id with
  | Some p ->
    let Coq_pair (off, _) = p in
    Some
    (Int256.mul
      (Int256.repr (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))))
      (Int256.repr (Z.of_nat off)))
  | None -> None

(** val frame_size : (ident, coq_type) prod list -> Int256.int **)

let rec frame_size = function
| Coq_nil -> Int256.repr Z0
| Coq_cons (p, locs') ->
  let Coq_pair (_, ty) = p in
  Int256.add
    (Int256.mul
      (Int256.repr (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))))
      (sizeof ty)) (frame_size locs')

(** val mkfieldlist : (ident, coq_type) prod list -> fieldlist **)

let rec mkfieldlist = function
| Coq_nil -> Fnil
| Coq_cons (p, itl') ->
  let Coq_pair (i, t) = p in Fcons (i, t, (mkfieldlist itl'))
