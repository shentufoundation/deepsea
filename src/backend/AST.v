(* This file is unchanged from CompCert except commenting things out. *)


(** This file defines a number of data types and operations used in
  the abstract syntax trees of many of the intermediate languages. *)

Require Import cclib.Coqlib.
Require String.
Require Import cclib.Integers.

Set Implicit Arguments.

(** * Syntactic elements *)

(** Identifiers (names of local variables, of global symbols and functions,
  etc) are represented by the type [positive] of positive integers. *)

(* These are used. *)
Definition ident := positive.
Definition ident_eq := peq.

Parameter ident_of_string : String.string -> ident.

Definition label := ident.

(*
(** Function definitions are the union of internal and external functions.
but we model external functions differently, so we don't need fundef *)
Definition fundef (F: Type): Type := F.
*)


                        (* this is not used: *)
(*  | External: external_function -> fundef F *)


