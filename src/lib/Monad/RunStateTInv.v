Require Import DeepSpec.lib.Monad.StateMonadOption.
Import DeepSpec.lib.Monad.Monad.MonadNotation.

Require Import MonadInv.
Require Import StateMonadOption.

Section StateMonadOption.
  Context {D : Type}.
  Instance MosT : Monad (osT D) := Monad_stateT D Monad_option.
  
  Lemma gets_osT2 {A} {f: D -> A} {d s: D} {v: A}:
    runStateT (gets f) d = ret (v, s) ->
    f d = v /\ d = s.
  Proof.
    intros H.
    simpl in H.
    injection H. auto.
  Qed.

  Lemma get_osT2 {d s v: D}:
    runStateT (get) d = (ret (v, s) : option (D*D)) ->
    d = v /\ d = s.
  Proof.
    intros H.
    simpl in H.
    injection H. auto.
  Qed.
  
  Lemma modify_osT {f: D -> D} {d s: D} {v: unit}:
    runStateT (modify f) d = ret (v, s) ->
    v = tt /\ f d = s.
  Proof.
    intros H.
    simpl in H.
    injection H. auto.
  Qed.

  Lemma put_osT {f d s: D} {v: unit}:
    runStateT (put f) d = (ret (v, s) : option (unit * D)) ->
    v = tt /\ f = s.
  Proof.
    intros H.
    compute in H.
    simpl in H.
    injection H. auto.
  Qed.
 
  Lemma ret_osT  {A}  {d s: D} {v u: A}:
    runStateT (ret v) d = ret (u, s) ->
      v = u /\ d = s.
  Proof.
    intros H.
    simpl in H.
    injection H.
    auto.
  Qed.

  Lemma guard_pure_osT2 {b: bool} {d s: D} {v:unit}:
    runStateT (guard b) d = ret (v, s) ->
    b = true /\ d = s.
  Proof.
    intros H.
    unfold guard in H. simpl in H.
    destruct b; simpl in H.
    - injection H; auto.
    - discriminate H.
  Qed.

  Import MonadNotation.


Lemma bind_runStateT : forall {A B} (m1 : osT D A) (m2 : A -> osT D B) d d' v,
    runStateT m1 d = Some (v, d') ->
    (runStateT (m1 >>= m2) d = runStateT (ret v >>= m2) d')  %monad.
Proof.
  simpl.
  intros.
  rewrite H.
  reflexivity.
Qed.
  
Lemma bind_of_guard_true : forall A (m : unit -> osT D A),
    (MonadZero.guard true >>= m)%monad = (m tt).
Proof.
Proof.
  simpl.
  intros.
  destruct (m tt) as [f].
  reflexivity.
Qed.

Lemma bind_gets_runStateT : forall A B (f : D -> A) (m : A -> osT D B) d,
    runStateT (gets f >>= m)%monad d = runStateT (m (f d)) d.
Proof.
  reflexivity.
Qed.

Lemma bind_modify_runStateT : forall B (f : D -> D) (m : unit -> osT D B) d,
    runStateT (modify f >>= m)%monad d = runStateT (m tt) (f d).
Proof.
  reflexivity.
Qed.

End StateMonadOption.

  (* For some reason, doing many "destructs" in ltac very slow. (I
     guess maybe the type checking algorithm for "match" is trying do
     something sophisticated? The runtime seems at least quadratic in
     the size of the term.) So instead we provide each lemma as a
     function you can "apply", this turns out to be much faster. *)
  Lemma split_runStateT_curried :
    forall {P : Prop} {D A B : Type} {c1 : osT D A} {c2 : A -> osT D B} {d s : D} {b : B},
      runStateT (x <- c1;; c2 x) d = Some (b, s) ->
      (forall (a : A) (m : D),
          runStateT c1 d = ret (a, m) -> runStateT (c2 a) m = ret (b, s) -> P) ->
      P.
    Proof.
      intros.
      destruct (split_runStateT H) as [? [? [? ?]]].
      eauto.
    Qed.

    Lemma get_osT2_curried :
     forall {P  : Prop} {D : Type} {d s v : D},
       runStateT (get (m:=osT D)) d = ret (v, s) ->
                  (d = v -> d = s -> P) ->
                  P.
    Proof.
      intros.
      destruct (get_osT2 H).
      eauto.
    Qed.


    Lemma put_osT_curried : forall {P:Prop} {D : Type} {f d s : D} {v : unit},
        runStateT (put (m:=osT D) f) d = ret (v, s) ->
        (v = tt -> f = s -> P) ->
        P.
    Proof.
      intros.
      destruct (put_osT H).
      auto.
    Qed.

    Lemma guard_pure_osT2_curried : forall {P:Prop} {D : Type} {b : bool} {d s : D} {v : unit},
        runStateT (guard (m:=osT D) b) d = ret (v, s) ->
        (b = true -> d = s -> P) ->
        P.
    Proof.
      intros.
      destruct (guard_pure_osT2 H).
      auto.
    Qed.

  Lemma gets_osT2_curried :
     forall {P : Prop} {D A : Type} {f : D -> A} {d s : D} {v : A},
      runStateT (gets (m:=osT D) f) d = ret (v, s) ->
      (f d = v -> d = s -> P) -> P.
    intros.
    destruct (gets_osT2 H).
    auto.
  Qed.

  Lemma modify_osT_curried : forall {P:Prop} {D : Type} {f : D -> D} {d s : D} {v : unit},
      runStateT (modify (m:=osT D) f) d = ret (v, s) ->
      (v = tt -> f d = s -> P) ->
      P.
  Proof.
    intros.
    destruct (modify_osT H).
    auto.
  Qed.

  Lemma ret_osT_curried : forall {P:Prop} {D A : Type} {d s : D} {v u : A},
      runStateT (ret (m:=osT D) v) d = Some (u, s)
      -> (v = u -> d = s -> P) -> P.
  Proof.
    intros.
    destruct (ret_osT H).
    auto.
  Qed.


  (* The inv_runStateT1 version deals with a single hypothesis from the context,
     without re-scanning all of them. *)
  Ltac inv_runStateT1 H  :=
    lazymatch type of H with
      | (runStateT (bind _ _) _ = ret (_, _))    =>
        let H1 := fresh H in
        let H2 := fresh H in
        apply (split_runStateT_curried H); intros ? ? H1 H2; clear H;
        inv_runStateT1 H1;
        inv_runStateT1 H2

      | (runStateT (gets _) _   = ret (_, _))     =>
        apply (gets_osT2_curried H); intros; clear H
      | (runStateT get _        = ret (_, _))     =>
        apply (get_osT2_curried H); intros; clear H
      | (runStateT (ret _) _    = ret (_, _))     =>
        apply (ret_osT_curried H); intros; clear H
      | (runStateT (modify _) _ = ret (_, _))     =>
        apply (modify_osT_curried H); intros; clear H
      | (runStateT (put _) _    = ret (_, _))     =>
        apply (put_osT_curried H); intros; clear H
      | (runStateT (guard _) _  = ret (_, _))     =>
        apply (guard_pure_osT2_curried H); intros; clear H

      (* And the same for Some instead of ret. They are definitionally
         equal, but match differently, so we include patterns for  both.*)
      | (runStateT (bind _ _) _ = Some (_, _))    =>
        let H1 := fresh H in
        let H2 := fresh H in
        apply (split_runStateT_curried H); intros ? ? H1 H2; clear H;
        inv_runStateT1 H1;
        inv_runStateT1 H2

      | (runStateT (gets _) _   = Some (_, _))     =>
        apply (gets_osT2_curried H); intros; clear H
      | (runStateT get _        = Some (_, _))     =>
        apply (get_osT2_curried H); intros; clear H
      | (runStateT (ret _) _    = Some (_, _))     =>
        apply (ret_osT_curried H); intros; clear H
      | (runStateT (modify _) _ = Some (_, _))     =>
        apply (modify_osT_curried H); intros; clear H
      | (runStateT (put _) _    = Some (_, _))     =>
        apply (put_osT_curried H); intros; clear H
      | (runStateT (guard _) _  = Some (_, _))     =>
        apply (guard_pure_osT2_curried H); intros; clear H
    | _ => idtac
    end.

  (* The _branching variant also destruct if-statements. *)
  Ltac inv_runStateT1_branching H  :=
    lazymatch type of H with
      | (runStateT (bind _ _) _ = ret (_, _))    =>
        let H1 := fresh H in
        let H2 := fresh H in
        apply (split_runStateT_curried H); intros ? ? H1 H2; clear H;
        inv_runStateT1_branching H1;
        inv_runStateT1_branching H2
      | (runStateT (gets _) _   = ret (_, _))     =>
        apply (gets_osT2_curried H); intros; clear H
      | (runStateT get _        = ret (_, _))     =>
        apply (get_osT2_curried H); intros; clear H
      | (runStateT (ret _) _    = ret (_, _))     =>
        apply (ret_osT_curried H); intros; clear H
      | (runStateT (modify _) _ = ret (_, _))     =>
        apply (modify_osT_curried H); intros; clear H
      | (runStateT (put _) _    = ret (_, _))     =>
        apply (put_osT_curried H); intros; clear H
      | (runStateT (guard _) _  = ret (_, _))     =>
        apply (guard_pure_osT2_curried H); intros; clear H
      | (runStateT (if ?b then _ else _) _ = ret _) =>
        destruct b eqn:?;
        inv_runStateT1_branching H

      (* And the same for Some instead of ret. They are definitionally
         equal, but match differently, so we include patterns for  both.*)
      | (runStateT (bind _ _) _ = Some (_, _))    =>
        let H1 := fresh H in
        let H2 := fresh H in
        apply (split_runStateT_curried H); intros ? ? H1 H2; clear H;
        inv_runStateT1_branching H1;
        inv_runStateT1_branching H2
      | (runStateT (gets _) _   = Some (_, _))     =>
        apply (gets_osT2_curried H); intros; clear H
      | (runStateT get _        = Some (_, _))     =>
        apply (get_osT2_curried H); intros; clear H
      | (runStateT (ret _) _    = Some (_, _))     =>
        apply (ret_osT_curried H); intros; clear H
      | (runStateT (modify _) _ = Some (_, _))     =>
        apply (modify_osT_curried H); intros; clear H
      | (runStateT (put _) _    = Some (_, _))     =>
        apply (put_osT_curried H); intros; clear H
      | (runStateT (guard _) _  = Some (_, _))     =>
        apply (guard_pure_osT2_curried H); intros; clear H
      | (runStateT (if ?b then _ else _) _ = Some  _) =>
        destruct b eqn:?;
        inv_runStateT1_branching H
    | _ => idtac        
    end.


(* strictly speaking the option monad is not really realated to RunStateT, 
   but it is used in the stmt_vc, so it is helpful to make invRunStateT 
   invert it also. *)

Lemma split_bind_option {A B} {c1: option A} {c2: A -> option B} {b: B}:
    (@bind option OptionMonad.Monad_option _ _ c1 c2) = ret b ->
    exists a, c1 = ret a /\ (c2 a) = ret b.
Proof.
  destruct c1.
  + simpl.
    eexists; split; auto.
  + simpl. inversion 1.
Qed.

  Ltac inv_runStateT :=
    repeat match goal with
           | [ H: (@bind option OptionMonad.Monad_option (_ * _) _ _ _) = ret _ |- _] => destruct (split_bind_option H) as [[? ?] [? ?]]; clear H

           | [ H: (@bind option OptionMonad.Monad_option _ _ _ _) = ret _ |- _] => destruct (split_bind_option H) as [? [? ?]]; clear H

           | [ H : runStateT _ _      = ret (_, _) |- _]         => inv_runStateT1 H
           | [ H : execStateT _  _        = ret _ |- _]          => apply execStateT_runStateT in H; destruct H as [? ?]
           | [ H : evalStateT _  _        = ret _ |- _]          => apply evalStateT_runStateT in H; destruct H as [? ?]
           | [ H : evalStateT mzero  _    = ret _ |- _]          => discriminate H

           (* The Some and ret constructors are definitionally equal, let's just match both.. *)
           | [ H : runStateT  _  _        = Some (_, _) |- _]    => inv_runStateT1 H
           | [ H : execStateT _  _        = Some _ |- _]         => apply execStateT_runStateT in H; destruct H as [? ?]
           | [ H : evalStateT _  _        = Some _ |- _]         => apply evalStateT_runStateT in H; destruct H as [? ?]
           | [ H : evalStateT mzero  _    = Some _ |- _]         => discriminate H

           end.

  Ltac inv_runStateT_branching :=
    repeat match goal with
           | [ H: (@bind option OptionMonad.Monad_option (_ * _) _ _ _) = ret _ |- _] => destruct (split_bind_option H) as [[? ?] [? ?]]; clear H

           | [ H: (@bind option OptionMonad.Monad_option _ _ _ _) = ret _ |- _] => destruct (split_bind_option H) as [? [? ?]]; clear H

           | [ H : runStateT _ _      = ret (_, _) |- _]         => inv_runStateT1_branching H
           | [ H : execStateT _  _        = ret _ |- _]          => apply execStateT_runStateT in H; destruct H as [? ?]
           | [ H : evalStateT _  _        = ret _ |- _]          => apply evalStateT_runStateT in H; destruct H as [? ?]
           | [ H : evalStateT mzero  _    = ret _ |- _]          => discriminate H

           (* The Some and ret constructors are definitionally equal, let's just match both.. *)
           | [ H : runStateT  _  _        = Some (_, _) |- _]    => inv_runStateT1_branching H
           | [ H : execStateT _  _        = Some _ |- _]         => apply execStateT_runStateT in H; destruct H as [? ?]
           | [ H : evalStateT _  _        = Some _ |- _]         => apply evalStateT_runStateT in H; destruct H as [? ?]
           | [ H : evalStateT mzero  _    = Some _ |- _]         => discriminate H

           end.
