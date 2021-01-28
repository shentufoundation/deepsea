(* Library for building map index types *)


Require Import cclib.Coqlib.
Require Import cclib.Integers.

Local Open Scope nat_scope.

(* interleave with zeros *)
Fixpoint sparse_positive (x: positive) : positive :=
  match x with
  | xH => xO xH
  | xO y => xO (xO (sparse_positive y))
  | xI y => xO (xI (sparse_positive y))
  end.

(* interleave with zeros, after the first bit *)
Definition delay_sparse (x: positive) : positive :=
  match x with
  | xH => xH
  | xI r => xI (sparse_positive r)
  | xO r => xO (sparse_positive r)
  end.

Fixpoint pos_measure (x: positive) : nat :=
  match x with
  | xH => S O
  | xI r => S (pos_measure r)
  | xO r => S (pos_measure r)
  end.

Fixpoint pick_first (x y: positive) (m: nat) {struct m} : positive :=
  match m with
  | O => xH (* should never reach here *)
  | S n =>
    match x with
    | xH => xI (delay_sparse y)
    | xO r => xO (pick_first y r n)
    | xI r => xI (pick_first y r n)
    end
  end.

(* Function to injectively map two positives into
a single positive *)
Definition inject_positive (x y: positive) : positive :=
  pick_first x y ((pos_measure x) + (pos_measure y)).

Lemma measure_comm: forall x y, (pos_measure x + pos_measure y) = (pos_measure y + pos_measure x).
Proof.
intros. apply plus_comm.
Qed.

Lemma measure_pos: forall x y,
  ((pos_measure x + S (pos_measure y)) = S (pos_measure x + pos_measure y)).
Proof.
auto.
Qed.

Lemma measure_plus_one: forall x, pos_measure x + 1 = S (pos_measure x).
Proof.
intro. omega.
Qed.

Lemma delay_matters: forall x y, delay_sparse x = sparse_positive y -> False.
Proof.
intros x y. revert x.
induction y.

destruct x. simpl. discriminate.
simpl. injection; clear H.
destruct x; simpl; discriminate.
unfold delay_sparse. simpl. discriminate.

destruct x; unfold delay_sparse; simpl.
discriminate.
injection; clear H.
destruct x; simpl.
injection; clear H.
fold (delay_sparse (xI x)).
apply IHy.
injection; clear H.
fold (delay_sparse (xO x)).
apply IHy.

injection; clear H.
fold (delay_sparse xH).
apply IHy.

discriminate.

intro x. destruct x; simpl; try discriminate.
injection; clear H. destruct x; simpl; discriminate.
Qed.

Lemma sparse_is_sparse: forall x y z,
  inject_positive x y = sparse_positive z -> False.
Proof.
intros x y z. revert x y.
induction z; intros x y; simpl.

-
  destruct x; unfold inject_positive; simpl.

  discriminate.

  injection; clear H.

  destruct y; simpl; try rewrite measure_pos; simpl.
  injection; clear H.
  fold (inject_positive x y).
  apply IHz.

  discriminate.

  rewrite measure_plus_one. simpl. injection; clear H.
  apply delay_matters.

  discriminate.

-
  unfold inject_positive.
  destruct x; simpl.
  destruct y; rewrite measure_comm; simpl; discriminate.
  destruct y; simpl;
    try rewrite measure_pos; try rewrite measure_plus_one; simpl.
  discriminate.
  injection; clear H.
  apply IHz.
  discriminate.
  discriminate.

-
  destruct x; try discriminate.
  unfold inject_positive; simpl.
  injection; clear H.
  destruct y; simpl;
    try rewrite measure_pos; try rewrite measure_plus_one; simpl; discriminate.
Qed.

Lemma delay_sparse_is_sparse: forall x y z,
  inject_positive x y = delay_sparse z -> False.
Proof.
intros x y z.
destruct z; unfold delay_sparse;
  destruct x; unfold inject_positive; simpl; try discriminate;
  injection; clear H.
-
  rewrite measure_comm.
  fold (inject_positive y x).
  apply sparse_is_sparse.
-
  apply delay_matters.
-
  rewrite measure_comm.
  fold (inject_positive y x).
  apply sparse_is_sparse.
Qed.

Lemma injective_sparse: forall x y, sparse_positive x = sparse_positive y -> x = y.
Proof.
induction x; simpl.
-
  destruct y; simpl.
  intro H.
  injection H.
  intro.
  assert (x=y).
  apply IHx.
  exact H0.
  rewrite H1; reflexivity.
  discriminate.
  discriminate.
-
  destruct y; simpl.
  discriminate.
  intro H; injection H.
  intro.
  assert (x=y).
  apply IHx; exact H0.
  rewrite H1; reflexivity.
  discriminate.
-
  destruct y; simpl.
  discriminate.
  discriminate.
  intro. reflexivity.
Qed.

Lemma injective_delay_sparse: forall x y, delay_sparse x = delay_sparse y -> x = y.
Proof.
destruct x; destruct y; simpl; intro;
  try discriminate; try reflexivity; injection H; intro.
assert (x=y).
apply injective_sparse. exact H0.
rewrite H1; reflexivity.
assert (x=y).
apply injective_sparse. exact H0.
rewrite H1; reflexivity.
Qed.


Lemma injective_positive_1:
  forall w x y z,
    inject_positive w x = inject_positive y z -> w = y.
Proof.
intros w x y z.
revert x w z.
induction y; intros x w z; unfold inject_positive; simpl.
+
  destruct z; destruct x; destruct w; simpl;
    try rewrite ?measure_pos; try rewrite ?measure_plus_one; simpl;
    try discriminate; intro; injection H; clear H.
  *
    fold (inject_positive w x).
    fold (inject_positive y z).
    intro.
    assert (w=y).
    revert H.
    apply IHy.
    rewrite H0.
    reflexivity.
  *
    fold (inject_positive y z).
    intro.
    exfalso.
    apply sparse_is_sparse with y z x.
    symmetry.
    exact H.
  *
    fold (inject_positive y z).
    intro.
    exfalso.
    apply delay_sparse_is_sparse with y z w.
    symmetry.
    exact H.
  *
    fold (inject_positive w x).
    fold (inject_positive y z).
    intro.
    assert (w=y).
    revert H.
    apply IHy.
    rewrite H0.
    reflexivity.
  *
    fold (inject_positive y z).
    intro; exfalso; apply sparse_is_sparse with y z x; symmetry; exact H.
  *
    fold (inject_positive w x).
    intro; exfalso; apply delay_sparse_is_sparse with w x y; exact H.
  *
    intro; exfalso.
    apply delay_matters with y x.
    symmetry. exact H.
  *
    intro.
    assert (w=y).
    apply injective_delay_sparse.
    exact H.
    rewrite H0; reflexivity.
+
  destruct z; destruct x; destruct w; simpl;
  try rewrite ?measure_pos; try rewrite ?measure_plus_one; simpl;
  try discriminate; intro; injection H; clear H.
  *
    fold (inject_positive w x).
    fold (inject_positive y z).
    intro.
    assert (w=y).
    revert H.
    apply IHy.
    rewrite H0.
    reflexivity.
  *
    fold (inject_positive y z).
    intro. exfalso.
    apply delay_sparse_is_sparse with y z w.
    symmetry. exact H.
  *
    fold (inject_positive w x).
    fold (inject_positive y z).
    intro.
    assert (w=y).
    revert H; apply IHy.
    rewrite H0; reflexivity.
  *
    fold (inject_positive w x).
    intro. exfalso.
    apply delay_sparse_is_sparse with w x y.
    exact H.
  *
    intro. assert (w=y).
    apply injective_delay_sparse.
    exact H.
    rewrite H0; reflexivity.
+
  destruct w; simpl; try discriminate; try reflexivity.
  rewrite measure_comm.
  fold (inject_positive x w).
  intro H; injection H; clear H.
  intro. exfalso.
  apply delay_sparse_is_sparse with x w z.
  exact H.
Qed.

Lemma injective_positive_2:
  forall w x y z,
    inject_positive w x = inject_positive y z -> x = z.
Proof.
intros w x y z.
destruct w; destruct y;
  unfold inject_positive; simpl;
  try rewrite ?measure_pos; simpl;
  try discriminate;
  intro H; injection H; clear H;
  try rewrite measure_comm; simpl.
-
  fold (inject_positive x w).
  rewrite measure_comm; fold (inject_positive z y).
  apply injective_positive_1.
-
  fold (inject_positive x w).
  intro; exfalso.
  apply delay_sparse_is_sparse with x w z; exact H.
-
  fold (inject_positive x w).
  rewrite measure_comm; fold (inject_positive z y).
  apply injective_positive_1.
-
  fold (inject_positive z y).
  intro; exfalso.
  apply delay_sparse_is_sparse with z y x; symmetry; exact H.
-
  apply injective_delay_sparse.
Qed.



Definition int_index (i : int256) : positive :=
  let (i, _) := i in
  match i with
  | Z0 => xH
  | Zpos p => xO p
  | Zneg p => xI p
  end.

Lemma int_index_injective: forall (x y: int256), int_index x = int_index y -> x = y.
Proof.
unfold int_index; destruct x; destruct y; intros.
apply Int256.mkint_eq.
destruct intval; destruct intval0; try (inversion H); try reflexivity.
Qed.

Definition int32_index (i : int) : positive :=
  let (i, _) := i in
  match i with
  | Z0 => xH
  | Zpos p => xO p
  | Zneg p => xI p
  end.

Lemma int32_index_injective: forall (x y: int), int32_index x = int32_index y -> x = y.
Proof.
unfold int32_index; destruct x; destruct y; intros.
apply Int.mkint_eq.
destruct intval; destruct intval0; try (inversion H); try reflexivity.
Qed.


