(* This is the top level langauge. 

   It is almost unchanged from Clight, but there are still quite a few changes. *)

(* *********************************************************************)
(*                                                                     *)
(*              The Compcert verified compiler                         *)
(*                                                                     *)
(*          Xavier Leroy, INRIA Paris-Rocquencourt                     *)
(*                                                                     *)
(*  Copyright Institut National de Recherche en Informatique et en     *)
(*  Automatique.  All rights reserved.  This file is distributed       *)
(*  under the terms of the GNU General Public License as published by  *)
(*  the Free Software Foundation, either version 2 of the License, or  *)
(*  (at your option) any later version.  This file is also distributed *)
(*  under the terms of the INRIA Non-Commercial License Agreement.     *)
(*                                                                     *)
(* *********************************************************************)

(** The Clight language: a simplified version of Compcert C where all
  expressions are pure and assignments and function calls are
  statements, not expressions. *)

Require Import cclib.Coqlib.
Require Import cclib.Errors.
Require Import cclib.Maps.
Require Import cclib.Integers.
Require Import backend.AST.
Require Import backend.Ctypes.
Require Import backend.Cop. 
Require Import backend.MachineModel.
Require Import backend.Values.HighValues.

(** * Abstract syntax *)

(** ** Expressions *)

(** Clight expressions correspond to the "pure" subset of C expressions.
  The main omissions are string literals and assignment operators
  ([=], [+=], [++], etc).  In Clight, assignment is a statement,
  not an expression.  Additionally, an expression can also refer to
  temporary variables, which are a separate class of local variables
  that do not reside in memory and whose address cannot be taken.

  As in Compcert C, all expressions are annotated with their types,
  as needed to resolve operator overloading and type-dependent behaviors. *)


(* Compared to Clight, we have added array-deref, hash-deref, and calling builtins. *)
Inductive expr : Type :=
  | Econst_int: int -> type -> expr       (** r integer literal *)
  | Econst_int256: int256 -> type -> expr (** r 256-bit integer literal *)
  | Evar: ident -> type -> expr           (** r variable *)
  | Eglob: ident -> type -> expr          (** global variable *)
  | Etempvar: ident -> type -> expr       (** r temporary variable *)
  | Ederef: expr -> type -> expr          (** r pointer dereference (unary [*]) *)
  | Eaddr: expr -> type -> expr          (** take the address of an lvalue to produce an rvalue pointer *)
  | Eunop: unary_operation -> expr -> type -> expr  (** r unary operation *)
  | Ebinop: binary_operation -> expr -> expr -> type -> expr (** r binary operation *)
  | Efield: expr -> ident -> type -> expr (** r access to a member of a struct *)
  | Eindex: expr -> expr -> type -> expr
  (* Some function calls are compiled to special commands
  and have no side effects *)
  | Ecall0: builtin0 -> type -> expr
  | Ecall1: builtin1 -> expr -> type -> expr.


Remark expr_dec: forall (x y: expr), {x = y} + {x <> y}.
Proof. intros. decide equality;
try apply Int256.eq_dec; try apply Int.eq_dec;
try apply type_eq; try apply ident_eq; try decide equality.
Qed.

(** [sizeof] and [alignof] are derived forms. *)

(* TODO: if these are ever used by the middle-end, it should probably be changed to Econst_int256. *)
(*
Definition Esizeof (ty' ty: type) : expr := Econst_int (Int.repr(sizeof ty')) ty.
Definition Ealignof (ty' ty: type) : expr := Econst_int (Int.repr(alignof ty')) ty.
*)

(** Extract the type part of a type-annotated Clight expression. *)

Definition typeof (e: expr) : type :=
  match e with
  | Econst_int _ ty => ty
  | Econst_int256 _ ty => ty
  | Evar _ ty => ty
  | Eglob _ ty => ty
  | Etempvar _ ty => ty
  | Ederef _ ty => ty
  | Eaddr _ ty => ty
  | Eunop _ _ ty => ty
  | Ebinop _ _ _ ty => ty
  | Efield _ _ ty => ty
  | Eindex _ _ ty => ty
  | Ecall0 _ ty => ty
  | Ecall1 _ _ ty => ty
  end.
