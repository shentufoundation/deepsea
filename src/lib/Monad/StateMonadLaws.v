Require Import FunctionalExtensionality.

Require Import DeepSpec.lib.Monad.Monad.
Require Import DeepSpec.lib.Monad.MonadTrans.
Require Import DeepSpec.lib.Monad.MonadLaws.
Require Import DeepSpec.lib.Monad.MonadState.
Require Import DeepSpec.lib.Monad.MonadZero.

Require Import StateMonad.

Set Implicit Arguments.
Set Strict Implicit.

Section Laws.
  Variable S : Type.
  Variable m : Type -> Type.
  Variable Monad_m : Monad m.
  Variable ML_m : MonadLaws Monad_m.


  Lemma equal_runStateT : forall t (m1 m2 : stateT S m t) ,
                            (forall s, runStateT m1 s = runStateT m2 s) -> m1 = m2.
  Proof.
    intros.
    destruct m1.  destruct m2.
    simpl in H.
    f_equal.
    extensionality s.
    auto.
  Qed.

  Global Instance MonadLaws_stateT : MonadLaws (@Monad_stateT S _ Monad_m).
  Proof.
    constructor.
    {(* bind_of_return *)
      intros.
      apply equal_runStateT.
      intros.
      simpl.
      rewrite bind_of_return by auto.
      reflexivity. }
    { (* return_of_bind *)
      simpl; intros.
      apply equal_runStateT.
      simpl; intros.
      rewrite return_of_bind.
      - reflexivity.
      - auto.
      - intros x.
        destruct x.
        rewrite H.
        reflexivity. }
    { (* bind_associativity *)
      simpl; intros.
      apply equal_runStateT.
      intros; simpl.
      repeat rewrite bind_associativity by auto.
      f_equal.
      extensionality a.
      destruct a.
      reflexivity. }
  Qed.

  Global Instance MonadTLaws_stateT : MonadTLaws _ _ (@MonadT_stateT S _ Monad_m).
  Proof.
    constructor.
    { simpl. intros.
      apply equal_runStateT.
      intros; simpl.
      rewrite bind_of_return; auto. }
    { simpl. intros.
      apply equal_runStateT.
      intros; simpl.
      rewrite bind_associativity by auto.
      rewrite bind_associativity by auto.
      f_equal.
      extensionality a.
      rewrite bind_of_return by auto.
      reflexivity. }
  Qed.


Global Instance MoandStateLaws_stateT : MonadStateLaws (@Monad_stateT S _ Monad_m) (@MonadState_stateT S _ Monad_m).
  Proof.
    constructor.
    { (* get_put *)
      simpl. apply equal_runStateT; intros; simpl.
      rewrite bind_of_return by auto.
      reflexivity. }
    { (* put_get *)
      simpl; intros; apply equal_runStateT; intros; simpl.
      repeat rewrite bind_of_return by auto.
      reflexivity. }
    { (* put_put *)
      simpl; intros; apply equal_runStateT; intros; simpl.
      repeat rewrite bind_of_return by auto.
      reflexivity. }
    { (* get_get *)
      simpl; intros; apply equal_runStateT; intros; simpl.
      repeat rewrite bind_of_return by auto.
      reflexivity. }
    {(* get_ignore*)
      simpl; intros; apply equal_runStateT; intros; simpl.
      repeat rewrite bind_of_return by auto.
      reflexivity. }
  Qed.
  
    (*
  Global Instance MonadReaderLaws_optionT (s : Type) (t : type s) (tT : typeOk t) (Mr : MonadReader s m) (MLr : MonadReaderLaws Monad_m _ _ Mr) : MonadReaderLaws _ _ _ (@Reader_optionT _ _ _ Mr).
  Proof.
    constructor.
    { simpl. unfold optionT_eq; simpl; intros; unfold liftM.
      rewrite local_bind; eauto with typeclass_instances.
      (erewrite bind_proper; [ | | | | eapply ask_local | ]); eauto with typeclass_instances.
      rewrite bind_associativity; eauto with typeclass_instances.
      rewrite bind_associativity; eauto with typeclass_instances.
      type_tac. 6: eapply preflexive.
      repeat rewrite bind_of_return; eauto with typeclass_instances.
      rewrite local_ret; eauto with typeclass_instances. type_tac.
      type_tac. eapply equal_match; eauto with typeclass_instances; type_tac.
      apply proper_fun; intros. repeat rewrite local_ret; eauto with typeclass_instances.
      type_tac; eauto with typeclass_instances. type_tac.
      type_tac. eapply equal_match; eauto with typeclass_instances; type_tac.
      type_tac.
      apply proper_fun; intros. repeat rewrite local_ret; eauto with typeclass_instances.
      type_tac. eauto with typeclass_instances.
      type_tac. type_tac. }
    { simpl. unfold optionT_eq; simpl; intros; unfold liftM.
      rewrite local_bind; eauto with typeclass_instances.
      type_tac.
      destruct x; destruct y; try solve [ inversion H4 ]; type_tac.
      inversion H4; assumption.
      rewrite local_ret; eauto with typeclass_instances; type_tac.
      type_tac. eapply equal_match; eauto with typeclass_instances; type_tac. }
    { simpl. unfold optionT_eq; simpl; intros; unfold liftM.
      rewrite local_ret; eauto with typeclass_instances; type_tac. }
    { simpl. unfold optionT_eq; simpl; intros; unfold liftM.
      rewrite local_local; eauto with typeclass_instances; type_tac. }
    { unfold local; simpl; intros. red. red. intros. red in H0.
      red; simpl. type_tac. }
    { Opaque lift. unfold ask; simpl; intros. red. type_tac.
      eapply lift_proper; eauto with typeclass_instances. Transparent lift. }
  Qed.
  *)

  Global Instance MonadZeroLaws_optionT (MZ: MonadZero m) (MZL: MonadZeroLaws _ MZ) : MonadZeroLaws (@Monad_stateT S _ Monad_m) _.
  Proof.
    constructor.
    { simpl; intros; apply equal_runStateT; intros; simpl.
      repeat rewrite bind_zero by auto.
      reflexivity. }
  Qed.

End Laws.
