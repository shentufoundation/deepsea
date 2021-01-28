Require Import ZArith.  
Require Import cclib.Coqlib.
Require Import cclib.Integers.

Lemma Zgt_bool_false: forall n m : Z,  (n >? m)%Z = false -> (n <= m)%Z.
Proof.
  intros.
  assert (H1 := Zgt_cases n m).
  rewrite H in H1.
  auto.
Qed.

Lemma Zlt_bool_false: forall n m : Z,  (n <? m)%Z = false -> (n >= m)%Z.
Proof.
  intros.
  assert (H1 := Zlt_cases n m).
  rewrite H in H1.
  auto.
Qed.

Lemma zle_bool_false: forall n m : Z,  (proj_sumbool (zle n m)) = false -> (n > m)%Z.
Proof.
  intros.
  destruct (zle n m); [discriminate | assumption ].
Qed.

Lemma zle_bool_true: forall n m : Z,  (proj_sumbool (zle n m)) = true -> (n <= m)%Z.
Proof.
  intros.
  intros.
  destruct (zle n m); [assumption| discriminate ].
Qed.

Lemma zlt_bool_false: forall n m : Z,  (proj_sumbool (zlt n m)) = false -> (n >= m)%Z.
Proof.
  intros.
  destruct (zlt n m); [discriminate | assumption ].
Qed.

Lemma zlt_bool_true: forall n m : Z,  (proj_sumbool (zlt n m)) = true -> (n < m)%Z.
Proof.
  intros.
  intros.
  destruct (zlt n m); [assumption| discriminate ].
Qed.

Lemma Int256eq_true : forall x y, Int256.eq x y = true -> x = y.
Proof.
  intros.
  assert (FOO := Int256.eq_spec x y).
  rewrite H in FOO.
  assumption.
Qed.

Lemma Int256eq_false : forall x y, Int256.eq x y = false -> x <> y.
Proof.
  intros.
  assert (FOO := Int256.eq_spec x y).
  rewrite H in FOO.
  assumption.
Qed.

Lemma geb_ge: forall n m : Z, (n >=? m) = true -> n >= m.
Proof.
  intros.
  rewrite Z.geb_le in H. omega.
Qed.

Lemma ngeb_lt: forall n m : Z, (n >=? m) = false -> n < m.
Proof.
  intros.
  assert (H2:= Zge_cases n m).
  rewrite H in H2.
  auto.
Qed.

Lemma if_and : forall (a b : bool),
    (if a then b else false) = true -> a = true /\ b = true.
Proof.
  destruct a, b; simpl; intros; auto.
Qed.


Ltac inv_arith :=
  repeat match goal with
          | [H : negb _ = true    |- _ ]  => rewrite Bool.negb_true_iff in H
          | [H : negb _ = false   |- _ ]  => rewrite <- Bool.negb_false_iff in H
          | [H : (andb _ _) = true  |- _ ]  => apply andb_prop in H; destruct H
          | [H : (if _ then _ else false) = true |- _] => apply if_and in H; destruct H                                                                              

          | [H : (Int256.eq _ _) = true |- _] => apply Int256eq_true in H
          | [H : (Int256.eq _ _) = false |- _] => apply Int256eq_false in H

          | [H : (_ >? _)%Z = true |- _]  =>  rewrite <- Zgt_is_gt_bool  in H
          | [H : (_ >? _)%Z = false |- _] =>  apply Zgt_bool_false  in H
          | [H : (_ <? _)%Z = true |- _]  =>  rewrite <- Zlt_is_lt_bool  in H
          | [H : (_ <? _)%Z = false |- _] =>  apply Zlt_bool_false in H
          | [H : (_ =? _)%Z = true |- _]  =>  rewrite Z.eqb_eq  in H
          | [H : (_ =? _)%Z = false |- _] =>  rewrite Z.eqb_neq  in H
          | [H : (_ >=? _)%Z = true |- _]  =>  apply geb_ge  in H
          | [H : (_ >=? _)%Z = false |- _] =>  apply ngeb_lt  in H
                                                                      
          | [H : (proj_sumbool (zle _ _)) = true  |- _] => apply zle_bool_true in H
          | [H : (proj_sumbool (zle _ _)) = false |- _] => apply zle_bool_false in H
          | [H : (proj_sumbool (zlt _ _)) = true  |- _] => apply zlt_bool_true in H
          | [H : (proj_sumbool (zlt _ _)) = false |- _] => apply zlt_bool_false in H
          end.
  
