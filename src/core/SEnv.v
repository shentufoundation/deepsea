(* *********************************************************************)
(*    DeepSpec, the language of certified softwares                    *)
(*                                                                     *)
(*      Shu-Chun Weng, Yale University                                 *)
(*                                                                     *)
(*  Copyright (c) 2013-2015 Shu-Chun Weng <shu-chun.weng@yale.edu>.    *)
(*                                                                     *)
(*  This program is free software; you can redistribute it and/or      *)
(*  modify it under the terms of the GNU General Public License        *)
(*  version 2 as published by the Free Software Foundation.  Note that *)
(*  the only valid version of the GPL for this work is version 2, not  *)
(*  v2.2 or v3.x or whatever.                                          *)
(*                                                                     *)
(*  This program is distributed in the hope that it will be useful,    *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of     *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *)
(*  GNU General Public License for more details.                       *)
(*                                                                     *)
(*  You should have received a copy of the GNU General Public License  *)
(*  along with this program; if not, write to the Free Software        *)
(*  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,         *)
(*  MA 02110-1301 USA.                                                 *)
(* *********************************************************************)

(** Environment and utility definitions for synthesizing *)

(* CompCert modules *)
Require Import backend.Values.HighValues.
Require Import backend.Environments.AllEnvironments.
Require Import cclib.Maps.
Require Import backend.MemoryModel.
Require Import backend.Ctypes.

(* DeepSpec modules *)
Require Import DeepSpec.core.Cval.
Require Import DeepSpec.core.HyperType.
Require Import DeepSpec.core.MemoryModel.

Require Import DeepSpec.lib.SimpleMaps.
Require Export DeepSpec.lib.SimpleIndexedMaps.

Module TypePairProjection <: TypeProjection.
  Definition A := hyper_type_pair.
  Definition proj := tp_ft.
End TypePairProjection.

Module SpecTree := IPList(TypePairProjection).

Definition spec_env_t := SpecTree.t.

Definition senv_cond {tmp} (se : spec_env_t tmp)
  := forall i tp hti,
       forall eq : (tmp ! i)%alist ~~~ Some (@mk_hyper_type_pair tp hti), 
         ht_ft_cond (SpecTree.get_eq i se eq).

Definition lenv_cond `{LayerSpecClass}{tmp} (se : spec_env_t tmp)(le : temp_env)
  := forall i tp hti,
       forall eq : (tmp ! i)%alist ~~~ Some (@mk_hyper_type_pair tp hti),
         ht_ft_cond (SpecTree.get_eq i se eq) /\
         ht_rel_some (SpecTree.get_eq i se eq) (le ! i).

Lemma lenv_senv_cond `{LayerSpecClass}{tmp}{se : spec_env_t tmp}{le} :
    lenv_cond se le -> senv_cond se.
Proof.
  intros le_cond i tp hti eq.
  apply le_cond.
Qed.

Module HyperTypeProjection <: TypeProjection.
  Definition A := hyper_type_pair.
  Definition proj htp := HyperType (tp_type_pair htp).
End HyperTypeProjection.

Definition proj1_ex {A : Prop}{P : A -> Prop}(e : ex P) : A :=
  match e with ex_intro x _ => x end.
Definition proj2_ex {A : Prop}{P : A -> Prop}(e : ex P) : P (proj1_ex e) :=
  match e with ex_intro _ Px => Px end.
Definition proj1_sig2 {A P Q}(s : sig2 P Q) : A :=
  match s with exist2 a _ _ => a end.
Definition proj2_sig2 {A}{P Q : A -> Prop}(s : sig2 P Q)
  : P (proj1_sig2 s) := match s with exist2 _ p _ => p end.
Definition proj3_sig2 {A}{P Q : A -> Prop}(s : sig2 P Q)
  : Q (proj1_sig2 s) := match s with exist2 _ _ q => q end.

Definition car := @fst.
Definition cdr := @snd.
Definition cadr A B C a := car B C (cdr A _ a).
Definition cddr A B C a := cdr B C (cdr A _ a).
Definition caar A B C a := car A B (car _ C a).
Definition cdar A B C a := cdr A B (car _ C a).
Definition caddr A B C D a := car C D (cddr A B _ a).
Definition cdddr A B C D a := cdr C D (cddr A B _ a).
Definition cadddr A B C D E a := car D E (cdddr A B C _ a).
Definition cddddr A B C D E a := cdr D E (cdddr A B C _ a).
Definition caddddr A B C D E F a := car E F (cddddr A B C D _ a).
Definition cdddddr A B C D E F a := cdr E F (cddddr A B C D _ a).
Arguments car [A B] _.          Arguments cdr [A B] _.
Arguments cadr [A B C] _.       Arguments cddr [A B C] _.
Arguments caar [A B C] _.       Arguments cdar [A B C] _.
Arguments caddr [A B C D] _.    Arguments cdddr [A B C D] _.
Arguments cadddr [A B C D E] _. Arguments cddddr [A B C D E] _.
Arguments caddddr [A B C D E F] _. Arguments cdddddr [A B C D E F] _.

Definition proj12 A B C a := @proj1 B C (@proj2 A _ a).
Definition proj22 A B C a := @proj2 B C (@proj2 A _ a).
Definition proj122 A B C D a := @proj1 C D (@proj22 A B _ a).
Definition proj222 A B C D a := @proj2 C D (@proj22 A B _ a).
Arguments proj12 [A B C] _.    Arguments proj22 [A B C] _.
Arguments proj122 [A B C D] _. Arguments proj222 [A B C D] _.

Lemma some_injective {A}{a b : A} : Some a = Some b -> a = b.
Proof. injection 1. trivial. Qed.
