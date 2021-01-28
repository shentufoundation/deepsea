(* This may need to be changed quite a bit, see the readme file. 

*)

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

(** This module defines the type of values that is used in the dynamic
  semantics of all our intermediate languages. *)

Require Import cclib.Coqlib.
Require Import cclib.Integers.
Require Import cclib.Maps.
Require Import backend.AST.
Require Import backend.IndexLib.


(* we want maps that are initialized to 0 by default,
which requires an IMap instead of a PTree *)
Module IdentIndexed <: INDEXED_TYPE.

Definition t := positive.
Definition index (i: positive) : positive := i.
Definition index_inv (i: positive) : positive := i.

Lemma index_invertible : forall x, index_inv (index x) = x.
Proof. reflexivity. Qed.
  
Lemma index_inj: forall (x y: positive), index x = index y -> x = y.
Proof.
simpl. intros x y H. exact H.
Qed.
Definition eq := peq.

End IdentIndexed.

Module Int256Map := IMap(Int256Indexed).

(* Currently skipping hashes; should come back to do the rest. *)
(* `ident_ext_extends i1 i2` is true if `i2` is of the form
   `Ihash (Ihash ... i1 ... ofs') ofs`. In other words, i2
    designates a sub-part of the object located at i1. *)
Inductive ident_ext_extends : int256 -> int256 -> Prop :=
| IIE_refl : forall i, ident_ext_extends i i.

(*| IIE_index : forall eid1 eid2 i,
    ident_ext_extends eid1 eid2 ->
    ident_ext_extends eid1 (Index eid2 i)
| IIE_field : forall eid1 eid2 id,
    ident_ext_extends eid1 eid2 ->
    ident_ext_extends eid1 (Field eid2 id).*)

(*Definition ident_ext_extends_inversion': forall i i2,
    ident_ext_extends i i2 ->
    forall i1 o, i = (Ihash i1 o) ->
    ident_ext_extends i1 i2.
Proof.                      
  induction 1; intros; subst.
  - constructor; constructor.
  - constructor.
    eapply IHident_ext_extends.
    reflexivity.
Qed.    

Lemma ident_ext_extends_inversion: forall i1 i2 o,
    ident_ext_extends (Ihash i1 o) i2 ->
    ident_ext_extends i1 i2.
Proof.                      
  intros; eauto using ident_ext_extends_inversion'.
Qed.

Fixpoint ident_ext_length i :=
  match i with
  | Iident _ => O
  | Ihash i _ => S (ident_ext_length i)
  end.

Lemma ident_ext_extends_longer : forall i j,
    ident_ext_extends i j ->
    (ident_ext_length i <= ident_ext_length j)%nat.
Proof.
  induction 1.
  - omega.
  - simpl.
    omega.
 Qed.

Lemma ident_ext_extends_disjoint : forall o1 o2,
    o1 <> o2 ->
    forall i' i0,
    ident_ext_extends (Ihash i0 o1) i' ->
    ~ ident_ext_extends (Ihash i0 o2) i'.
Proof.
  intros o1 o2 Hneq.
  unfold not.
  induction i'; intros i0 H1 H2.
  - inversion H1.
  - inversion H1; inversion H2; subst.
    + congruence.
    + apply ident_ext_extends_longer in H6.
      simpl in H6.
      omega.
    + apply ident_ext_extends_longer in H3.
      simpl in H3.
      omega.
    + unfold not in IHi'; eapply IHi'; eassumption.
Qed.
 *)

(** A value is either:
- Vunit. When a function returns unit, we compile it to code which actually pushes a value to the stack, namely this value.
- a machine integer;
- a pointer, which is also a machine integer;
- a hash
 *)
Inductive val: Type :=
| Vunit: val
| Vint: int256 -> val
| Vhash: val -> val
| Vhash2: val -> val -> val
.
    
Definition Vzero: val := Vint Int256.zero.
Definition Vone: val := Vint Int256.one.
Definition Vmone: val := Vint Int256.mone.

Definition Vtrue: val := Vint Int256.one.
Definition Vfalse: val := Vint Int256.zero.
