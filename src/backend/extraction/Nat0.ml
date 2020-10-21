open Datatypes
open Decimal

(** val add : nat -> nat -> nat **)

let rec add n m =
  match n with
  | O -> m
  | S p -> S (add p m)

(** val mul : nat -> nat -> nat **)

let rec mul n m =
  match n with
  | O -> O
  | S p -> add m (mul p m)

(** val sub : nat -> nat -> nat **)

let rec sub n m =
  match n with
  | O -> n
  | S k -> (match m with
            | O -> n
            | S l -> sub k l)

(** val tail_add : nat -> nat -> nat **)

let rec tail_add n m =
  match n with
  | O -> m
  | S n0 -> tail_add n0 (S m)

(** val tail_addmul : nat -> nat -> nat -> nat **)

let rec tail_addmul r n m =
  match n with
  | O -> r
  | S n0 -> tail_addmul (tail_add m r) n0 m

(** val tail_mul : nat -> nat -> nat **)

let tail_mul n m =
  tail_addmul O n m

(** val of_uint_acc : uint -> nat -> nat **)

let rec of_uint_acc d acc =
  match d with
  | Nil -> acc
  | D0 d0 ->
    of_uint_acc d0 (tail_mul (S (S (S (S (S (S (S (S (S (S O)))))))))) acc)
  | D1 d0 ->
    of_uint_acc d0 (S
      (tail_mul (S (S (S (S (S (S (S (S (S (S O)))))))))) acc))
  | D2 d0 ->
    of_uint_acc d0 (S (S
      (tail_mul (S (S (S (S (S (S (S (S (S (S O)))))))))) acc)))
  | D3 d0 ->
    of_uint_acc d0 (S (S (S
      (tail_mul (S (S (S (S (S (S (S (S (S (S O)))))))))) acc))))
  | D4 d0 ->
    of_uint_acc d0 (S (S (S (S
      (tail_mul (S (S (S (S (S (S (S (S (S (S O)))))))))) acc)))))
  | D5 d0 ->
    of_uint_acc d0 (S (S (S (S (S
      (tail_mul (S (S (S (S (S (S (S (S (S (S O)))))))))) acc))))))
  | D6 d0 ->
    of_uint_acc d0 (S (S (S (S (S (S
      (tail_mul (S (S (S (S (S (S (S (S (S (S O)))))))))) acc)))))))
  | D7 d0 ->
    of_uint_acc d0 (S (S (S (S (S (S (S
      (tail_mul (S (S (S (S (S (S (S (S (S (S O)))))))))) acc))))))))
  | D8 d0 ->
    of_uint_acc d0 (S (S (S (S (S (S (S (S
      (tail_mul (S (S (S (S (S (S (S (S (S (S O)))))))))) acc)))))))))
  | D9 d0 ->
    of_uint_acc d0 (S (S (S (S (S (S (S (S (S
      (tail_mul (S (S (S (S (S (S (S (S (S (S O)))))))))) acc))))))))))

(** val of_uint : uint -> nat **)

let of_uint d =
  of_uint_acc d O
