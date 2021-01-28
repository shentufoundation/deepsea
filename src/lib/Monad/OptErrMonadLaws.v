Require Import FunctionalExtensionality.

Require Import DeepSpec.lib.Monad.Monad.
Require Import DeepSpec.lib.Monad.MonadTrans.
Require Import DeepSpec.lib.Monad.MonadLaws.
Require Import DeepSpec.lib.Monad.MonadState.
Require Import DeepSpec.lib.Monad.MonadZero.
Require Import DeepSpec.lib.Monad.MonadPlus.

Require Import DeepSpec.lib.Monad.OptErrMonad.

Set Implicit Arguments.
Set Strict Implicit.


Section Laws.
  Variable m : Type -> Type.
  Variable Monad_m : Monad m.
  Variable ML_m : MonadLaws Monad_m.

  Theorem equal_match : forall (A B : Type),
    forall (x y : optErr A) (a b : B) (f g : A -> B),
      x = y ->
      a = b ->
      f = g ->
      match x with
        | Success a => f a
        | Error _ => a
      end
        =
        match y with
          | Success a => g a
          | Error _ => b
        end.
  Proof.
    destruct x; destruct y; simpl; intros; congruence.
  Qed.

  Global Instance MonadLaws_optErr : MonadLaws Monad_optErr.
  Proof.
    constructor.
    { (* bind_of_return *)
      intros. simpl. reflexivity. }
    { (* return_of_bind *)
      simpl; intros.
      destruct aM; congruence. }
    { (* bind_associativity *)
      simpl; intros.
      destruct aM; [destruct (f a)|]; congruence. }
  Qed.
  
End Laws.
