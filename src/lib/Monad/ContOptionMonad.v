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

Require Import DeepSpec.lib.Monad.Monad.
Require Import DeepSpec.lib.Monad.MonadZero.

Set Implicit Arguments.
Set Strict Implicit.

Import MonadNotation.
Open Scope monad_scope.

(*****************************************)


(** * The continuation-option monad
    Equivalent to [ContT (Maybe a)] in Haskell *)
Definition ContOpt a := forall r, (a -> option r) -> option r.


Global Instance Monad_ContOpt : Monad ContOpt :=
  { ret A a := fun r k => k a
  ; bind A B ma f  := fun r k => ma r (fun a => f a r k)
  }.

Global Instance Zero_ContOpt : MonadZero ContOpt :=
  { mzero _A := fun r k => None }.

(* Todo: replace uses of these functions with the typeclass ones. *)
Definition Csome {A}(a : A) : ContOpt A := fun r k => k a.
Definition Cnone {A}        : ContOpt A := fun r k => None.

Definition Cguard_sum {P Q}(b : {P} + {Q}) : ContOpt unit :=
  fun r k => if b then k tt else None.

Definition Cbind {A B}(ma : ContOpt A)(f : A -> ContOpt B) : ContOpt B :=
  fun r k => ma r (fun a => f a r k).
Definition Clift {A B}(f : A -> B) : ContOpt A -> ContOpt B :=
  fun m => Cbind m (fun a => Csome (f a)).

Definition runContOpt {A}(m : ContOpt A) : option A := m A (@Some A).
Definition optContOpt {A}(a' : option A) : ContOpt A :=
  fun r k =>
    match a' with
    | Some a => k a
    | None   => None
    end.

(** - [Csome] is also known as [return] or [unit]
    - [Cnone] is also known as [mzero]
    - [Cbind] is also known as [bind]
    - [optContOpt] is the left and right inverse of [runContOpt].
*)

Lemma run_opt_ContOpt_ident {A}(a' : option A)
    : runContOpt (optContOpt a') = a'.
Proof.
  destruct a'; reflexivity.
Qed.

(** How to represent that a [ContOpt] computation ruturns a certain value?
    [ContOpt_Some_eq m a] is a proposition that [m] will never fail to produce
    a result and the return value is [a].  This definition is not used, but the
    body can be seen inlined in various places.

    The other direction, with the equality on functions expressed extensionally,
    [opt_run_ContOpt_ident_ext : forall A (m : ContOpt A) r k,
    optContOpt (runContOpt m) r k = m r k], requires parametricity to prove. *)
Definition ContOpt_Some_eq {A}(m : ContOpt A)(a : A) : Prop :=
  forall r k, m r k = k a.
