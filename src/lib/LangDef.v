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

(* Standard library modules *)
Require Import BinInt.
Require        Omega.

(* CompCert modules *)
(*Require Import compcert.common.Events.*)
Require Import cclib.Coqlib.
(*Require Import compcert.cfrontend.Clight.
Require Import compcert.cfrontend.ClightBigstep. *)

(* CompCertX modules *)
(* Require        compcertx.common.EventsX. *)

Local Open Scope Z_scope.


(* TODO: figure out a better statement of these, and move them to the backend directory. *)

(*
Definition exec_stmt `{compiler_config_ops : CompilerConfigOps}
      m0 ge env le (m : mem) s t le' m' out
  := let wbwim := EventsX.writable_block_with_init_mem_ops in
     let wb := Events.writable_block_with_init_mem_writable_block_ops m0 in
     ClightBigstep.exec_stmt ge (fun _ => function_entry2) env le m s t le' m' out.

Definition eval_funcall {mem}`{compiler_config_ops : CompilerConfigOps mem}
      m0 ge (m : mem) f vargs t m' vres
  := let wbwim := EventsX.writable_block_with_init_mem_ops in
     let wb := Events.writable_block_with_init_mem_writable_block_ops m0 in
     ClightBigstep.eval_funcall ge (fun _ => Clight.function_entry2)
                                m f vargs t m' vres.
*)

Lemma min_ex: forall (P: Z -> Prop) lo hi,
    (forall n, {P n} + {~ P n}) ->
    {n : Z | lo <= n < hi /\ P n /\ forall n', lo <= n' < n -> ~ P n'} +
    {forall n, lo <= n < hi -> ~ P n}.
Proof.
  intros.
  assert(HP: forall k, 0 <= k ->
      {n : Z | lo <= n < lo + k /\ P n /\ forall n' : Z, lo <= n' < n -> ~ P n'} +
      {forall n : Z, lo <= n < lo + k -> ~ P n}).
  apply natlike_rec2.
  right.
  intros.
  omega.

  intros z HR HT.
  destruct HT.
  left.
  destruct s as [n[HT HM]].
  exists n.
  split; trivial.
  omega.

  specialize (H (lo + z)).
  destruct H.
  left.
  exists (lo + z).
  split; auto.
  omega.

  right.
  intros.
  destruct (zeq n1 (lo + z)).
  subst.
  trivial.
  apply n.
  omega.

  destruct (zlt lo hi).
  replace hi with (lo + (hi - lo)) by omega. apply HP. omega.
  right; intros. omegaContradiction.

Qed.

Lemma min_ex_found_same:
  forall (P: Z -> Prop) lo hi n1 n2,
    (lo <= n1 < hi /\ P n1 /\ forall n', lo <= n' < n1 -> ~ P n') ->
    (lo <= n2 < hi /\ P n2 /\ forall n', lo <= n' < n2 -> ~ P n') ->
    n1 = n2.
Proof.
  intros ? ? ? ? ? (n1_range & Pn1 & n1_first) (n2_range & Pn2 & n2_first).
  destruct (Z.lt_total n1 n2) as [ ne | [ n_eq | ne ]]; try exact n_eq.
  - exfalso.
    refine (n2_first _ _ Pn1).
    split; [ apply n1_range | apply ne ].
  - exfalso.
    refine (n1_first _ _ Pn2).
    split; [ apply n2_range | apply ne ].
Qed.

Lemma min_ex_found_not_found_contradict:
  forall (P: Z -> Prop) lo hi n,
    (lo <= n < hi /\ P n /\ forall n', lo <= n' < n -> ~ P n') ->
    (forall n, lo <= n < hi -> ~ P n) ->
    False.
Proof.
  intros ? ? ? ? (n_range & Pn & _) not_found.
  eapply not_found; eassumption.
Qed.
