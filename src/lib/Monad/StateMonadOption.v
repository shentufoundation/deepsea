Require Export DeepSpec.lib.Monad.Monad.
Require Export DeepSpec.lib.Monad.MonadLaws.
Require Export DeepSpec.lib.Monad.MonadInv.
Require Export DeepSpec.lib.Monad.StateMonad.
Require Export DeepSpec.lib.Monad.OptionMonad.
Require Export DeepSpec.lib.Monad.OptionMonadLaws.
Require Export DeepSpec.lib.Monad.MonadZero.
Require Export DeepSpec.lib.Monad.MonadState.
Require Export DeepSpec.lib.Monad.StateMonadLaws.

Set Implicit Arguments.
Set Strict Implicit.

Import MonadNotation.
Open Scope monad_scope.


Ltac dest_opt opt opt_new l :=
  let p := fresh "p" in
  let opt1 := fresh opt in
  destruct (monad_inv_bind _ _ _ opt) as (p & opt1 & opt_new);
  clear opt;
  rename opt1 into opt;
  destruct p as l.

Tactic Notation "dest_opt" hyp(opt) "as"
       ident(opt_new) simple_intropattern(l) :=
  dest_opt opt opt_new l.
(*
Ltac inj_state H :=
  match type of H with
  | runStateT _ _ = _ => injection H as <- <-
  | execStateT _ _ = _ => injection H as <-
  | evalStateT _ _ = _ => injection H as <-
  | ret (fst _) = _ => injection H as <-
  | Some (fst _) = _ => injection H as <-
  | ret (snd _) = _ => injection H as <-
  | Some (snd _) = _ => injection H as <-
  | _ = ret (_,_) => injection H as <- <-
  | _ = Some (_,_) => injection H as <- <-
  end;
  clear H.
*)
Section StateMonadOption.
  Variable D : Type.
  Definition osT := @stateT D option.
  Instance MosT : Monad osT := Monad_stateT D Monad_option.
  
  Instance MonadState_osT : MonadState D osT.
  Proof.
    apply MonadState_stateT.
    apply Monad_option.
  Defined.

  Instance MonadZero_osT : MonadZero osT.
  Proof.
    apply MonadZero_stateT.
    apply Monad_option.
    apply Zero_option.
  Defined.

  Instance MonadLaws_MosT : MonadLaws MosT.
  Proof.
    apply MonadLaws_stateT.
    apply MonadLaws_option.
  Defined.

  Lemma ret_inj {A} {v1 v2:A}:
    @ret _ Monad_option _ v1 = ret v2 ->
    v1 = v2.
  Proof.
    intro H. injection H as <-.
    reflexivity.
  Qed.

  Lemma ret_Some {A} {v1 v2:A}:
    @ret _ Monad_option _ v1 = Some v2 ->
    v1 = v2.
  Proof.
    apply ret_inj.
  Qed.

  Lemma Some_ret {A} {v1 v2:A}:
    Some v1 = @ret _ Monad_option _ v2 ->
    v1 = v2.
  Proof.
    apply ret_inj.
  Qed.

  Lemma runStateT_execStateT {A} {c: osT A} {d v s}:
    runStateT c d = ret (v, s) ->
    execStateT c d = ret s.
  Proof.
    intros.
    simpl; rewrite H; reflexivity.
  Qed.

  Lemma runStateT_execStateT' {A} {c: osT A} {d s}:
    (exists v, runStateT c d = ret (v, s)) ->
    execStateT c d = ret s.
  Proof.
    intros [v H].
    apply (runStateT_execStateT H).
  Qed.

  Lemma runStateT_evalStateT {A} {c: osT A} {d v s}:
    runStateT c d = ret (v, s) ->
    evalStateT c d = ret v.
  Proof.
    intros.
    simpl; rewrite H; reflexivity.
  Qed.

  Lemma runStateT_evalStateT' {A} {c: osT A} {d v}:
    (exists s, runStateT c d = ret (v, s)) ->
    evalStateT c d = ret v.
  Proof.
    intros [s H].
    apply (runStateT_evalStateT H).
  Qed.

  Lemma execStateT_runStateT {A} {c: osT A} {d s}:
    execStateT c d = ret s ->
    exists v, runStateT c d = ret (v, s).
  Proof.
    intros opt.
    dest_opt opt as opt' [v' s'].
    apply ret_inj in opt'. cbv in opt'. subst s'.
    exists v'. assumption.
  Qed.

  Lemma evalStateT_runStateT {A} {c: osT A} {d v}:
    evalStateT c d = ret v ->
    exists s, runStateT c d = ret (v, s).
  Proof.
    intros opt.
    dest_opt opt as opt' [v' s'].
    apply ret_inj in opt'. cbv in opt'. subst v'.
    exists s'. assumption.
  Qed.

  (** pure *)

  Lemma gets_pure_osT {A} {f: D -> A} {d s: D} {v: A}:
    runStateT (@gets osT _ _ _ _ f) d = ret (v, s) ->
    d = s.
  Proof.
    intros H.
    simpl in H.
    injection H. auto.
  Qed.

  Lemma guard_pure_osT {b: bool} {d s: D} {v:unit}:
    runStateT (guard b) d = ret (v, s) ->
    d = s.
  Proof.
    intros H.
    unfold guard in H. simpl in H.
    destruct b; simpl in H.
    - injection H; auto.
    - discriminate H.
  Qed.

  (** bind *)

  Lemma bind_osT {A B} {ma: osT A} {f: A -> osT B} {a: A} {b: B} {d s m: D}:
    runStateT ma d = ret (a, s) ->
    runStateT (f a) s = ret (b, m) ->
    runStateT (x <- ma;; f x) d = ret (b, m).
  Proof.
    intros opta optb.
    apply (monad_inv_bind_inv opta optb).
  Qed.

  Lemma bind_osT' {A B} {ma: osT A}  {f: A -> osT B} {a: A} {b: B} {d s m: D}:
    runStateT ma d = ret (a, s) /\
    ( runStateT ma d = ret (a, s) -> runStateT (f a) s = ret (b, m)) ->
    runStateT (x <- ma;; f x) d = ret (b, m).
  Proof.
    intros [opta optb].
    specialize (optb opta).
    apply (bind_osT opta optb).
  Qed.
  
  Lemma bind_rewrite_osT_mzero {A B} {c: osT A} {d: D} {f: A -> osT B}:
    runStateT c d = mzero ->
    runStateT (x<-c;;f x) d = mzero.
  Proof.
    intros opt.
    simpl. rewrite opt.
    reflexivity.
  Qed.

  Lemma bind_to_none_osT {A B} {c: osT A} {d: D} {f: A -> osT B}:
    runStateT (c >>= f) d = mzero ->
    {runStateT c d = mzero} +
    {exists v s, runStateT c d = ret (v, s) /\ runStateT (f v) s = mzero}.
  Proof.
    intros opt.
    simpl in opt.
    destruct (runStateT c d); [right|left].
    - destruct p as [v s].
      exists v, s. auto.
    - reflexivity.
  Qed.

  Lemma bind_rewrite_osT_runStateT {A B} {d1 d2: D}
        {c1 c2: osT A} {c3: A -> osT B}:
    runStateT c1 d1 = runStateT c2 d2 ->
    runStateT (x<-c1;;c3 x) d1 = runStateT (x<-c2;;c3 x) d2.
  Proof.
    intros opt.
    simpl.
    rewrite opt. reflexivity.
  Qed.

  Lemma bind_rewrite_osT_ret {A B} {d1 d2: D} {v:A}
        {c1: osT A} {c3: A -> osT B}:
    runStateT c1 d1 = ret (v, d2) ->
    runStateT (x<-c1;;c3 x) d1 = runStateT (c3 v) d2.
  Proof.
    intros opt.
    simpl.
    rewrite opt. reflexivity.
  Qed.

  Lemma split_runStateT {A B} {c1: osT A} {c2: A -> osT B} {d s: D} {b: B}:
    runStateT (x<-c1;; c2 x) d = ret (b, s) ->
    exists a m, runStateT c1 d = ret (a, m) /\ runStateT (c2 a) m = ret (b, s).
  Proof.
    intros H.
    destruct (monad_inv_bind _ _ _ H) as [[a m] [H1 H2]].
    exists a, m.
    auto.
  Qed.
  
  Lemma split_execStateT {A B} {c1: osT A} {c2: osT B} {d s: D}:
    execStateT (c1;; c2) d = ret s ->
    exists m, execStateT c1 d = ret m /\ execStateT c2 m = ret s.
  Proof.
    intros H.
    destruct (execStateT_runStateT H) as [b H']; clear H; rename H' into H.
    destruct (split_runStateT H) as (a & m & H1 & H2).
    exists m.
    split.
    - apply (runStateT_execStateT H1).
    - apply (runStateT_execStateT H2).
  Qed.

  (** get *)
  Lemma get_osT {d v s: D}:
    runStateT get d = ret (v, s) <->
    v = d /\ d = s.
  Proof.
    simpl.
    split.
    - intros H. injection H as <- <-. tauto.
    - intros [? ?]. subst. reflexivity.
  Qed.

  (** put *)
  Lemma put_osT {d d' s: D} {v: unit}:
    runStateT (put d') d = ret (v, s) <->
    v = tt /\ d' = s.
  Proof.
    simpl.
    split.
    - intros opt. injection opt as <- <-. tauto.
    - intros [H1 H2]. subst. reflexivity.
  Qed.

  (** gets *)
  Lemma gets_osT {A} {f: D -> A} {d s: D} {v: A}:
    runStateT (@gets osT _ _ _ _ f) d = ret (v, s) <->
    f d = v /\ d = s.
  Proof.
    simpl.
    split.
    - intros opt. injection opt as <- <-.
      tauto.
    - intros [H1 H2]. subst. reflexivity.
  Qed.
  
  Lemma gets_osT_eval {A} {f: D -> A} {d: D} {v: A}:
    evalStateT (@gets osT _ _ _ _ f) d = ret v <->
    f d = v.
  Proof.
    split.
    - intros opt.
      simpl in opt.
      injection opt.
      intro Hfeq.
      cut (Some (f d, d) = Some (v, d)).
      + intro H. injection H. auto.
      + injection opt as <-. reflexivity.
    - intros Hf.
      simpl. rewrite Hf. reflexivity.
  Qed.

  (** modify *)
  Lemma modify_osT {f : D -> D} {d s: D} {v: unit}:
    runStateT (@modify osT _ _ _ f) d = ret (v, s) <->
    v = tt /\ f d = s.
  Proof.
    simpl.
    split.
    - intros opt. injection opt as <- <-. auto.
    - intros [H1 H2]. subst. reflexivity.
  Qed.

  (** guard *)
  Lemma guard_osT {b: bool} {m v s}:
    runStateT (guard b) m = ret (v, s) <->
    b = true /\ v = tt /\ m = s.
  Proof.
    unfold guard.
    destruct b.
    - (* b = true *)
      simpl.
      split.
      + intros opt. injection opt as <- <-. tauto.
      + intros (? & ? & ?). subst. reflexivity.
    - (* b = false *)
      simpl.
      split.
      + discriminate.
      + intros (? & ? & ?). discriminate.
  Qed.
(*
  Lemma gets_guard_osT' {A} {f: D -> A} {g: A -> bool} {d s: D} {v: unit}:
    runStateT (x <- gets f;; guard (g x)) d = ret (v, s) ->
    g (f d) = true.
  Proof.
    unfold guard.
    simpl.
    destruct (g (f d)).
    - reflexivity.
    - simpl. discriminate.
  Qed.

  Lemma gets_guard_pure_osT {A} {f: D -> A} {g: A -> bool} {d s: D} {v: unit}:
    runStateT (x <- gets f;; guard (g x)) d = ret (v, s) ->
    d = s.
  Proof.
    intro H.
    destruct (split_runStateT H) as (v' & s' & H1 & H2).
    apply gets_pure_osT in H1.
    apply guard_pure_osT in H2.
    subst. reflexivity.
  Qed.

  Lemma gets_guard_osT {A} {f: D -> A} {g: A -> bool} {d s: D} {v: unit}:
    runStateT (x <- gets f;; guard (g x)) d = ret (v, s) ->
    g (f s) = true.
  Proof.
    intro H.
    assert (d = s).
    { apply (gets_guard_pure_osT H). }
    rewrite <- H0.
    apply (gets_guard_osT' H).
  Qed.
*)

  Lemma ret_osT {A} {a: A} {m s: D} {v}:
    runStateT (ret a) m = ret (v, s) <->
    a = v /\ m = s.
  Proof.
    simpl.
    split.
    - intros H. injection H as <- <-. tauto.
    - intros [? ?]. subst. reflexivity.
  Qed.

  Lemma match_osT {A} {e: option A} {d s : D} {v: A}:
    runStateT
      match e with
      | Some a => ret a
      | None => mzero
      end d = ret (v, s) <->
    e = Some v /\ d = s.
  Proof.
    destruct e; simpl.
    - (* e = Some a *)
      split.
      + intros opt. injection opt as <- <-. tauto.
      + intros [H1 H2]. injection H1 as <-. subst. reflexivity.
    - (* e = None *)
      split.
      + discriminate.
      + intros [? ?]. discriminate.
  Qed.
(*
  Lemma eval_ret_osT {A} {a: A} {m:D} {v}:
    @evalStateT D option Monad_option A (ret a) m = ret v ->
    a = v.
  Proof.
    intro H.
    destruct (evalStateT_runStateT H) as [s Hrun].
    apply run_ret_osT in Hrun.
    tauto.
  Qed.
*)
End StateMonadOption.

Ltac exec2run_util H l :=
  let H' := fresh in
  destruct (execStateT_runStateT H) as [l H'];
  clear H; rename H' into H.

Tactic Notation "exec2run" hyp(H) "as" simple_intropattern(l) :=
  exec2run_util H l.

Tactic Notation "exec2run" :=
  apply runStateT_execStateT'.

Ltac eval2run_util H l :=
  let H' := fresh in
  destruct (evalStateT_runStateT H) as [l H'];
  clear H; rename H' into H.

Tactic Notation "eval2run" hyp(H) "as" simple_intropattern(l) :=
  eval2run_util H l.

Tactic Notation "eval2run" :=
  apply runStateT_evalStateT'.

Tactic Notation "run2exec" hyp(H) :=
  apply runStateT_execStateT in H.

Tactic Notation "run2eval" hyp(H) :=
  apply runStateT_evalStateT in H.

(** rewrite runStateT c1 d = _ in
   runStateT (x<-c1;;c2 x) d
*)
Ltac bind_rewrite_osT H :=
  match type of H with
  | runStateT _ _ = ret (_, _) => erewrite (bind_rewrite_osT_ret H)
  | runStateT _ _ = mzero => erewrite (bind_rewrite_osT_mzero H)
  | runStateT _ _ = runStateT _ _ => erewrite (bind_rewrite_osT_runStateT H)
  end.

Ltac split_run_util H l :=
  apply split_runStateT in H;
  destruct H as l.

Tactic Notation "split_run" hyp(H) "as" simple_intropattern(l) :=
  split_run_util H l.

Lemma extract_if_rev {b : bool}:
  (if b then false else true) = true <->
  b = false.
Proof.
  destruct b.
  - (* b = true *) split; auto.
  - (* b = false *) tauto.
Qed.
