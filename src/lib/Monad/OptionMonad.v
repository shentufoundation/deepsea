Require Import DeepSpec.lib.Monad.Monad.
Require Import DeepSpec.lib.Monad.MonadZero.
Require Import DeepSpec.lib.Monad.MonadPlus.
Require Import DeepSpec.lib.Monad.MonadInv.

Set Implicit Arguments.
Set Strict Implicit.

Import MonadNotation.
Open Scope monad_scope.

Global Instance Monad_option : Monad option :=
{ ret  := @Some
; bind := fun _ _ c1 c2 => match c1 with
                             | None => None
                             | Some v => c2 v
                           end
}.

Global Instance Zero_option : MonadZero option :=
{ mzero := @None }.

Global Instance Plus_option : MonadPlus option :=
{ mplus _A _B aM bM :=
    match aM with
    | None => liftM inr bM
    | Some a => Some (inl a)
    end
}.

Global Instance MonadInvBind_option: MonadInvBind Monad_option.
Proof.
  unfold MonadInvBind. intros.
  destruct ma.
  - exists a. reflexivity.
  - discriminate H.
Defined.

Global Instance MonadInvRet_option: MonadInvRet Monad_option.
Proof.
  unfold MonadInvRet. simpl. intros.
  injection H; auto.
Defined.
