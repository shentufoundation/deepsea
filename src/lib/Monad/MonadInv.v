Require Import DeepSpec.lib.Monad.Monad.
Require Import DeepSpec.lib.Monad.MonadLaws.

Set Implicit Arguments.
Set Strict Implicit.

Section MonadInvDef.
  Variable m : Type -> Type.
  Variable M : Monad m.
  Class MonadInvRet :=
    monad_inv_ret:
      forall {A} (x y: A),
        ret x = ret y -> x = y.

  Class MonadInvBind :=
    monad_inv_bind_extract:
      forall {A B} (f: A -> m B) (ma: m A) (b: B),
        bind ma f = ret b -> {a | ma = ret a}.

  Class MonadInvBindWeak :=
    monad_inv_bind_weak:
      forall {A B} (f: A -> m B) (ma: m A) (b: B),
        bind ma f = ret b -> exists a, f a = ret b.
End MonadInvDef.

Section MonadInv.
  Context `{HM: Monad}.
  Context `{HMLaws: !MonadLaws HM}.
  Context `{HMbind: !MonadInvBind HM}.
  Context `{HMret: !MonadInvRet HM}.

  Global Instance mond_inv_bind_inv_bind_weak:
    MonadInvBindWeak HM.
  Proof.
    intros A B f ma b H.
    destruct (monad_inv_bind_extract f ma b H) as [a Ha].
    exists a. subst.
    rewrite bind_of_return in H; tauto.
  Qed.

  Lemma monad_inv_bind {A B} (f: A -> m B) (ma: m A) (b: B):
    bind ma f = ret b ->
    {a | ma = ret a /\ f a = ret b}.
  Proof.
    intros H.
    destruct (monad_inv_bind_extract f ma b H) as [a Ha].
    exists a.
    split.
    * assumption.
    * subst.
      rewrite bind_of_return in H; tauto.
  Qed.

  Lemma monad_inv_bind_iff {A B} (f: A -> m B) (ma: m A) (b: B):
    bind ma f = ret b <->
    exists a, ma = ret a /\ f a = ret b.
  Proof.
    split.
    * intros H.
      apply monad_inv_bind in H.
      destruct H; eauto.
    * intros [a [Hma Hfa]].
      subst.
      rewrite bind_of_return; tauto.
  Qed.
  
  Lemma monad_inv_bind_inv {A B} {f: A -> m B} {ma: m A} {a: A} {b: B}:
    ma = ret a ->
    f a = ret b ->
    bind ma f = ret b.
  Proof.
    intros.
    apply monad_inv_bind_iff.
    exists a; tauto.
  Qed.

  Lemma monad_inv_ret_iff {A} (x y: A):
    ret x = ret y <-> x = y.
  Proof.
    split.
    apply monad_inv_ret; auto.
    apply f_equal.
  Qed.

  (* this version is used in the backend part. *)
  Lemma bind_some: forall A B (a: m A) (f: A -> m B) (b: B),
      bind a f = ret b ->
      exists a0, a = ret a0 /\ f a0 = ret b.
  Proof.
    intros.
    destruct (monad_inv_bind_extract _ _ _ H).
    eexists.
    split.
    - eassumption.
    - rewrite e in H.
      rewrite (@bind_of_return _ _ HMLaws) in H.
      assumption.
  Qed. 
  
End MonadInv.

