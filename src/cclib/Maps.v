
(* *********************************************************************)
(*                                                                     *)
(*              The Compcert verified compiler                         *)
(*                                                                     *)
(*          Xavier Leroy, INRIA Paris-Rocquencourt                     *)
(*                                                                     *)
(*  Copyright Institut National de Recherche en Informatique et en     *)
(*  Automatique.  All rights reserved.  This file is distributed       *)
(*  under the terms of the GNU General Public License as published by  *)
(*  the Free Software Foundation, either version 2 of the License, or  *)
(*  (at your option) any later version.  This file is also distributed *)
(*  under the terms of the INRIA Non-Commercial License Agreement.     *)
(*                                                                     *)
(* *********************************************************************)

(** Applicative finite maps are the main data structure used in this
  project.  A finite map associates data to keys.  The two main operations
  are [set k d m], which returns a map identical to [m] except that [d]
  is associated to [k], and [get k m] which returns the data associated
  to key [k] in map [m].  In this library, we distinguish two kinds of maps:
- Trees: the [get] operation returns an option type, either [None]
  if no data is associated to the key, or [Some d] otherwise.
- Maps: the [get] operation always returns a data.  If no data was explicitly
  associated with the key, a default data provided at map initialization time
  is returned.

  In this library, we provide efficient implementations of trees and
  maps whose keys range over the type [positive] of binary positive
  integers or any type that can be injected into [positive].  The
  implementation is based on radix-2 search trees (uncompressed
  Patricia trees) and guarantees logarithmic-time operations.  An
  inefficient implementation of maps as functions is also provided.
*)

Require Import Equivalence EquivDec.
Require Import Coqlib.

(* To avoid useless definitions of inductors in extracted code. *)
Local Unset Elimination Schemes.
Local Unset Case Analysis Schemes.

Set Implicit Arguments.

(** * The abstract signatures of trees *)

Module Type TREE.
  Parameter elt: Type.
  Parameter elt_eq: forall (a b: elt), {a = b} + {a <> b}.
  Parameter t: Type -> Type.
  Parameter empty: forall (A: Type), t A.
  Parameter get: forall (A: Type), elt -> t A -> option A.
  Parameter set: forall (A: Type), elt -> A -> t A -> t A.
  Parameter remove: forall (A: Type), elt -> t A -> t A.

  Definition get_default {A:Type} (default:A) (k:elt) (m : t A) : A :=
    match get k m with
    | Some v => v
    | NONE => default
    end.

  (** The ``good variables'' properties for trees, expressing
    commutations between [get], [set] and [remove]. *)
  Axiom gempty:
    forall (A: Type) (i: elt), get i (empty A) = None.
  Axiom gss:
    forall (A: Type) (i: elt) (x: A) (m: t A), get i (set i x m) = Some x.
  Axiom gso:
    forall (A: Type) (i j: elt) (x: A) (m: t A),
    i <> j -> get i (set j x m) = get i m.
  Axiom gsspec:
    forall (A: Type) (i j: elt) (x: A) (m: t A),
    get i (set j x m) = if elt_eq i j then Some x else get i m.
  (* We could implement the following, but it's not needed for the moment.
  Hypothesis gsident:
    forall (A: Type) (i: elt) (m: t A) (v: A),
    get i m = Some v -> set i v m = m.
  Hypothesis grident:
    forall (A: Type) (i: elt) (m: t A) (v: A),
    get i m = None -> remove i m = m.
  *)
  Axiom grs:
    forall (A: Type) (i: elt) (m: t A), get i (remove i m) = None.
  Axiom gro:
    forall (A: Type) (i j: elt) (m: t A),
    i <> j -> get i (remove j m) = get i m.
  Axiom grspec:
    forall (A: Type) (i j: elt) (m: t A),
    get i (remove j m) = if elt_eq i j then None else get i m.

  (** Extensional equality between trees. *)
  Parameter beq: forall (A: Type), (A -> A -> bool) -> t A -> t A -> bool.
  Axiom beq_correct:
    forall (A: Type) (eqA: A -> A -> bool) (t1 t2: t A),
    beq eqA t1 t2 = true <->
    (forall (x: elt),
     match get x t1, get x t2 with
     | None, None => True
     | Some y1, Some y2 => eqA y1 y2 = true
     | _, _ => False
    end).

  (** Applying a function to all data of a tree. *)
  Parameter map:
    forall (A B: Type), (elt -> A -> B) -> t A -> t B.
  Axiom gmap:
    forall (A B: Type) (f: elt -> A -> B) (i: elt) (m: t A),
    get i (map f m) = option_map (f i) (get i m).

  (** Same as [map], but the function does not receive the [elt] argument. *)
  Parameter map1:
    forall (A B: Type), (A -> B) -> t A -> t B.
  Axiom gmap1:
    forall (A B: Type) (f: A -> B) (i: elt) (m: t A),
    get i (map1 f m) = option_map f (get i m).

  (** Applying a function pairwise to all data of two trees. *)
  (*
  Parameter combine:
    forall (A B C: Type), (option A -> option B -> option C) -> t A -> t B -> t C.
  Axiom gcombine:
    forall (A B C: Type) (f: option A -> option B -> option C),
    f None None = None ->
    forall (m1: t A) (m2: t B) (i: elt),
    get i (combine f m1 m2) = f (get i m1) (get i m2). *)

  (** Enumerating the bindings of a tree. *)
  Parameter elements:
    forall (A: Type), t A -> list (elt * A).
  Axiom elements_correct:
    forall (A: Type) (m: t A) (i: elt) (v: A),
    get i m = Some v -> In (i, v) (elements m).
  Axiom elements_complete:
    forall (A: Type) (m: t A) (i: elt) (v: A),
    In (i, v) (elements m) -> get i m = Some v.
  Axiom elements_keys_norepet:
    forall (A: Type) (m: t A),
    list_norepet (List.map (@fst elt A) (elements m)).
  Axiom elements_extensional:
    forall (A: Type) (m n: t A),
    (forall i, get i m = get i n) ->
    elements m = elements n.
  Axiom elements_remove:
    forall (A: Type) i v (m: t A),
    get i m = Some v ->
    exists l1 l2, elements m = l1 ++ (i,v) :: l2 /\ elements (remove i m) = l1 ++ l2.

  (** Folding a function over all bindings of a tree. *)
  Parameter fold:
    forall (A B: Type), (B -> elt -> A -> B) -> t A -> B -> B.
  Axiom fold_spec:
    forall (A B: Type) (f: B -> elt -> A -> B) (v: B) (m: t A),
    fold f m v =
    List.fold_left (fun a p => f a (fst p) (snd p)) (elements m) v.
  (** Same as [fold], but the function does not receive the [elt] argument. *)
  Parameter fold1:
    forall (A B: Type), (B -> A -> B) -> t A -> B -> B.
  Axiom fold1_spec:
    forall (A B: Type) (f: B -> A -> B) (v: B) (m: t A),
    fold1 f m v =
    List.fold_left (fun a p => f a (snd p)) (elements m) v.
End TREE.

(** * The abstract signatures of maps *)

Module Type MAP.
  Parameter elt: Type.
  Parameter elt_eq: forall (a b: elt), {a = b} + {a <> b}.
  Parameter t: Type -> Type.
  Parameter init: forall (A: Type), A -> t A.
  Parameter get: forall (A: Type), elt -> t A -> A.
  Parameter set: forall (A: Type), elt -> A -> t A -> t A.
  Axiom gi:
    forall (A: Type) (i: elt) (x: A), get i (init x) = x.
  Axiom gss:
    forall (A: Type) (i: elt) (x: A) (m: t A), get i (set i x m) = x.
  Axiom gso:
    forall (A: Type) (i j: elt) (x: A) (m: t A),
    i <> j -> get i (set j x m) = get i m.
  Axiom gsspec:
    forall (A: Type) (i j: elt) (x: A) (m: t A),
    get i (set j x m) = if elt_eq i j then x else get i m.
  Axiom gsident:
    forall (A: Type) (i j: elt) (m: t A), get j (set i (get i m) m) = get j m.
  Parameter map: forall (A B: Type), (A -> B) -> t A -> t B.
  Axiom gmap:
    forall (A B: Type) (f: A -> B) (i: elt) (m: t A),
    get i (map f m) = f(get i m).
End MAP.

(** * An implementation of trees over type [positive] *)

Module PTree <: TREE.
  Definition elt := positive.
  Definition elt_eq := peq.

  Inductive tree (A : Type) : Type :=
    | Leaf : tree A
    | Node : tree A -> option A -> tree A -> tree A.

  Arguments Leaf [A].
  Arguments Node [A].
  Scheme tree_ind := Induction for tree Sort Prop.

  Definition t := tree.

  Definition empty (A : Type) := (Leaf : t A).

  Fixpoint get (A : Type) (i : positive) (m : t A) {struct i} : option A :=
    match m with
    | Leaf => None
    | Node l o r =>
        match i with
        | xH => o
        | xO ii => get ii l
        | xI ii => get ii r
        end
    end.

  Fixpoint set (A : Type) (i : positive) (v : A) (m : t A) {struct i} : t A :=
    match m with
    | Leaf =>
        match i with
        | xH => Node Leaf (Some v) Leaf
        | xO ii => Node (set ii v Leaf) None Leaf
        | xI ii => Node Leaf None (set ii v Leaf)
        end
    | Node l o r =>
        match i with
        | xH => Node l (Some v) r
        | xO ii => Node (set ii v l) o r
        | xI ii => Node l o (set ii v r)
        end
    end.

  Fixpoint remove (A : Type) (i : positive) (m : t A) {struct i} : t A :=
    match i with
    | xH =>
        match m with
        | Leaf => Leaf
        | Node Leaf o Leaf => Leaf
        | Node l o r => Node l None r
        end
    | xO ii =>
        match m with
        | Leaf => Leaf
        | Node l None Leaf =>
            match remove ii l with
            | Leaf => Leaf
            | mm => Node mm None Leaf
            end
        | Node l o r => Node (remove ii l) o r
        end
    | xI ii =>
        match m with
        | Leaf => Leaf
        | Node Leaf None r =>
            match remove ii r with
            | Leaf => Leaf
            | mm => Node Leaf None mm
            end
        | Node l o r => Node l o (remove ii r)
        end
    end.

  Theorem gempty:
    forall (A: Type) (i: positive), get i (empty A) = None.
  Proof.
    induction i; simpl; auto.
  Qed.

  Theorem gss:
    forall (A: Type) (i: positive) (x: A) (m: t A), get i (set i x m) = Some x.
  Proof.
    induction i; destruct m; simpl; auto.
  Qed.

    Lemma gleaf : forall (A : Type) (i : positive), get i (Leaf : t A) = None.
    Proof. exact gempty. Qed.

  Theorem gso:
    forall (A: Type) (i j: positive) (x: A) (m: t A),
    i <> j -> get i (set j x m) = get i m.
  Proof.
    induction i; intros; destruct j; destruct m; simpl;
       try rewrite <- (gleaf A i); auto; try apply IHi; congruence.
  Qed.

  Theorem gsspec:
    forall (A: Type) (i j: positive) (x: A) (m: t A),
    get i (set j x m) = if peq i j then Some x else get i m.
  Proof.
    intros.
    destruct (peq i j); [ rewrite e; apply gss | apply gso; auto ].
  Qed.

  Theorem gsident:
    forall (A: Type) (i: positive) (m: t A) (v: A),
    get i m = Some v -> set i v m = m.
  Proof.
    induction i; intros; destruct m; simpl; simpl in H; try congruence.
     rewrite (IHi m2 v H); congruence.
     rewrite (IHi m1 v H); congruence.
  Qed.

  Theorem set2:
    forall (A: Type) (i: elt) (m: t A) (v1 v2: A),
    set i v2 (set i v1 m) = set i v2 m.
  Proof.
    induction i; intros; destruct m; simpl; try (rewrite IHi); auto.
  Qed.


  Theorem set_swap : forall (A: Type)  (i1 i2: elt) (m: t A) (v1 v2: A),
      i1 <> i2 -> 
      set i2 v2 (set i1 v1 m) = set i1 v1 (set i2 v2 m) .
  Proof.
    induction i1; destruct i2; destruct m; simpl;
        try reflexivity; intros; try rewrite IHi1; congruence.
  Qed.
  
  Lemma rleaf : forall (A : Type) (i : positive), remove i (Leaf : t A) = Leaf.
  Proof. destruct i; simpl; auto. Qed.

  Theorem grs:
    forall (A: Type) (i: positive) (m: t A), get i (remove i m) = None.
  Proof.
    induction i; destruct m.
     simpl; auto.
     destruct m1; destruct o; destruct m2 as [ | ll oo rr]; simpl; auto.
      rewrite (rleaf A i); auto.
      cut (get i (remove i (Node ll oo rr)) = None).
        destruct (remove i (Node ll oo rr)); auto; apply IHi.
        apply IHi.
     simpl; auto.
     destruct m1 as [ | ll oo rr]; destruct o; destruct m2; simpl; auto.
      rewrite (rleaf A i); auto.
      cut (get i (remove i (Node ll oo rr)) = None).
        destruct (remove i (Node ll oo rr)); auto; apply IHi.
        apply IHi.
     simpl; auto.
     destruct m1; destruct m2; simpl; auto.
  Qed.

  Theorem gro:
    forall (A: Type) (i j: positive) (m: t A),
    i <> j -> get i (remove j m) = get i m.
  Proof.
    induction i; intros; destruct j; destruct m;
        try rewrite (rleaf A (xI j));
        try rewrite (rleaf A (xO j));
        try rewrite (rleaf A 1); auto;
        destruct m1; destruct o; destruct m2;
        simpl;
        try apply IHi; try congruence;
        try rewrite (rleaf A j); auto;
        try rewrite (gleaf A i); auto.
     cut (get i (remove j (Node m2_1 o m2_2)) = get i (Node m2_1 o m2_2));
        [ destruct (remove j (Node m2_1 o m2_2)); try rewrite (gleaf A i); auto
        | apply IHi; congruence ].
     destruct (remove j (Node m1_1 o0 m1_2)); simpl; try rewrite (gleaf A i);
        auto.
     destruct (remove j (Node m2_1 o m2_2)); simpl; try rewrite (gleaf A i);
        auto.
     cut (get i (remove j (Node m1_1 o0 m1_2)) = get i (Node m1_1 o0 m1_2));
        [ destruct (remove j (Node m1_1 o0 m1_2)); try rewrite (gleaf A i); auto
        | apply IHi; congruence ].
     destruct (remove j (Node m2_1 o m2_2)); simpl; try rewrite (gleaf A i);
        auto.
     destruct (remove j (Node m1_1 o0 m1_2)); simpl; try rewrite (gleaf A i);
        auto.
  Qed.

  Theorem grspec:
    forall (A: Type) (i j: elt) (m: t A),
    get i (remove j m) = if elt_eq i j then None else get i m.
  Proof.
    intros. destruct (elt_eq i j). subst j. apply grs. apply gro; auto.
  Qed.

  Section BOOLEAN_EQUALITY.

    Variable A: Type.
    Variable beqA: A -> A -> bool.

    Fixpoint bempty (m: t A) : bool :=
      match m with
      | Leaf => true
      | Node l None r => bempty l && bempty r
      | Node l (Some _) r => false
      end.

    Fixpoint beq (m1 m2: t A) {struct m1} : bool :=
      match m1, m2 with
      | Leaf, _ => bempty m2
      | _, Leaf => bempty m1
      | Node l1 o1 r1, Node l2 o2 r2 =>
          match o1, o2 with
          | None, None => true
          | Some y1, Some y2 => beqA y1 y2
          | _, _ => false
          end
          && beq l1 l2 && beq r1 r2
      end.

    Lemma bempty_correct:
      forall m, bempty m = true <-> (forall x, get x m = None).
    Proof.
      induction m; simpl.
      split; intros. apply gleaf. auto.
      destruct o; split; intros.
      congruence.
      generalize (H xH); simpl; congruence.
      destruct (andb_prop _ _ H). rewrite IHm1 in H0. rewrite IHm2 in H1.
      destruct x; simpl; auto.
      apply andb_true_intro; split.
      apply IHm1. intros; apply (H (xO x)).
      apply IHm2. intros; apply (H (xI x)).
    Qed.

    Lemma beq_correct:
      forall m1 m2,
      beq m1 m2 = true <->
      (forall (x: elt),
       match get x m1, get x m2 with
       | None, None => True
       | Some y1, Some y2 => beqA y1 y2 = true
       | _, _ => False
       end).
    Proof.
      induction m1; intros.
    - simpl. rewrite bempty_correct. split; intros.
      rewrite gleaf. rewrite H. auto.
      generalize (H x). rewrite gleaf. destruct (get x m2); tauto.
    - destruct m2.
      + unfold beq. rewrite bempty_correct. split; intros.
        rewrite H. rewrite gleaf. auto.
        generalize (H x). rewrite gleaf. destruct (get x (Node m1_1 o m1_2)); tauto.
      + simpl. split; intros.
        * destruct (andb_prop _ _ H). destruct (andb_prop _ _ H0).
          rewrite IHm1_1 in H3. rewrite IHm1_2 in H1.
          destruct x; simpl. apply H1. apply H3.
          destruct o; destruct o0; auto || congruence.
        * apply andb_true_intro. split. apply andb_true_intro. split.
          generalize (H xH); simpl. destruct o; destruct o0; tauto.
          apply IHm1_1. intros; apply (H (xO x)).
          apply IHm1_2. intros; apply (H (xI x)).
    Qed.

  End BOOLEAN_EQUALITY.

  Fixpoint prev_append (i j: positive) {struct i} : positive :=
    match i with
      | xH => j
      | xI i' => prev_append i' (xI j)
      | xO i' => prev_append i' (xO j)
    end.

  Definition prev (i: positive) : positive :=
    prev_append i xH.

  Lemma prev_append_prev i j:
    prev (prev_append i j) = prev_append j i.
  Proof.
    revert j. unfold prev.
    induction i as [i IH|i IH|]. 3: reflexivity.
    intros j. simpl. rewrite IH. reflexivity.
    intros j. simpl. rewrite IH. reflexivity.
  Qed.

  Lemma prev_involutive i :
    prev (prev i) = i.
  Proof (prev_append_prev i xH).

  Lemma prev_append_inj i j j' :
    prev_append i j = prev_append i j' -> j = j'.
  Proof.
    revert j j'.
    induction i as [i Hi|i Hi|]; intros j j' H; auto;
    specialize (Hi _ _ H); congruence.
  Qed.

    Fixpoint xmap (A B : Type) (f : positive -> A -> B) (m : t A) (i : positive)
             {struct m} : t B :=
      match m with
      | Leaf => Leaf
      | Node l o r => Node (xmap f l (xO i))
                           (match o with None => None | Some x => Some (f (prev i) x) end)
                           (xmap f r (xI i))
      end.

  Definition map (A B : Type) (f : positive -> A -> B) m := xmap f m xH.

    Lemma xgmap:
      forall (A B: Type) (f: positive -> A -> B) (i j : positive) (m: t A),
      get i (xmap f m j) = option_map (f (prev (prev_append i j))) (get i m).
    Proof.
      induction i; intros; destruct m; simpl; auto.
    Qed.

  Theorem gmap:
    forall (A B: Type) (f: positive -> A -> B) (i: positive) (m: t A),
    get i (map f m) = option_map (f i) (get i m).
  Proof.
    intros A B f i m.
    unfold map.
    rewrite xgmap. repeat f_equal. exact (prev_involutive i).
  Qed.

  Fixpoint map1 (A B: Type) (f: A -> B) (m: t A) {struct m} : t B :=
    match m with
    | Leaf => Leaf
    | Node l o r => Node (map1 f l) (option_map f o) (map1 f r)
    end.

  Theorem gmap1:
    forall (A B: Type) (f: A -> B) (i: elt) (m: t A),
    get i (map1 f m) = option_map f (get i m).
  Proof.
    induction i; intros; destruct m; simpl; auto.
  Qed.

  Definition Node' (A: Type) (l: t A) (x: option A) (r: t A): t A :=
    match l, x, r with
    | Leaf, None, Leaf => Leaf
    | _, _, _ => Node l x r
    end.

  Lemma gnode':
    forall (A: Type) (l r: t A) (x: option A) (i: positive),
    get i (Node' l x r) = get i (Node l x r).
  Proof.
    intros. unfold Node'.
    destruct l; destruct x; destruct r; auto.
    destruct i; simpl; auto; rewrite gleaf; auto.
  Qed.

  Fixpoint filter1 (A: Type) (pred: A -> bool) (m: t A) {struct m} : t A :=
    match m with
    | Leaf => Leaf
    | Node l o r =>
        let o' := match o with None => None | Some x => if pred x then o else None end in
        Node' (filter1 pred l) o' (filter1 pred r)
    end.

  Theorem gfilter1:
    forall (A: Type) (pred: A -> bool) (i: elt) (m: t A),
    get i (filter1 pred m) =
    match get i m with None => None | Some x => if pred x then Some x else None end.
  Proof.
    intros until m. revert m i. induction m; simpl; intros.
    rewrite gleaf; auto.
    rewrite gnode'. destruct i; simpl; auto. destruct o; auto.
  Qed.

  Section COMBINE.

  Variables A B C: Type.
  Variable f: option A -> option B -> option C.
  Hypothesis f_none_none: f None None = None.

  Fixpoint xcombine_l (m : t A) {struct m} : t C :=
      match m with
      | Leaf => Leaf
      | Node l o r => Node' (xcombine_l l) (f o None) (xcombine_l r)
      end.

  Lemma xgcombine_l :
          forall (m: t A) (i : positive),
          get i (xcombine_l m) = f (get i m) None.
    Proof.
      induction m; intros; simpl.
      repeat rewrite gleaf. auto.
      rewrite gnode'. destruct i; simpl; auto.
    Qed.

  Fixpoint xcombine_r (m : t B) {struct m} : t C :=
      match m with
      | Leaf => Leaf
      | Node l o r => Node' (xcombine_r l) (f None o) (xcombine_r r)
      end.

  Lemma xgcombine_r :
          forall (m: t B) (i : positive),
          get i (xcombine_r m) = f None (get i m).
    Proof.
      induction m; intros; simpl.
      repeat rewrite gleaf. auto.
      rewrite gnode'. destruct i; simpl; auto.
    Qed.

  Fixpoint combine (m1: t A) (m2: t B) {struct m1} : t C :=
    match m1 with
    | Leaf => xcombine_r m2
    | Node l1 o1 r1 =>
        match m2 with
        | Leaf => xcombine_l m1
        | Node l2 o2 r2 => Node' (combine l1 l2) (f o1 o2) (combine r1 r2)
        end
    end.

  Theorem gcombine:
      forall (m1: t A) (m2: t B) (i: positive),
      get i (combine m1 m2) = f (get i m1) (get i m2).
  Proof.
    induction m1; intros; simpl.
    rewrite gleaf. apply xgcombine_r.
    destruct m2; simpl.
    rewrite gleaf. rewrite <- xgcombine_l. auto.
    repeat rewrite gnode'. destruct i; simpl; auto.
  Qed.

  End COMBINE.

  Lemma xcombine_lr :
    forall (A B: Type) (f g : option A -> option A -> option B) (m : t A),
    (forall (i j : option A), f i j = g j i) ->
    xcombine_l f m = xcombine_r g m.
    Proof.
      induction m; intros; simpl; auto.
      rewrite IHm1; auto.
      rewrite IHm2; auto.
      rewrite H; auto.
    Qed.

  Theorem combine_commut:
    forall (A B: Type) (f g: option A -> option A -> option B),
    (forall (i j: option A), f i j = g j i) ->
    forall (m1 m2: t A),
    combine f m1 m2 = combine g m2 m1.
  Proof.
    intros A B f g EQ1.
    assert (EQ2: forall (i j: option A), g i j = f j i).
      intros; auto.
    induction m1; intros; destruct m2; simpl;
      try rewrite EQ1;
      repeat rewrite (xcombine_lr f g);
      repeat rewrite (xcombine_lr g f);
      auto.
     rewrite IHm1_1.
     rewrite IHm1_2.
     auto.
  Qed.

    Fixpoint xelements (A : Type) (m : t A) (i : positive)
                       (k: list (positive * A)) {struct m}
                       : list (positive * A) :=
      match m with
      | Leaf => k
      | Node l None r =>
          xelements l (xO i) (xelements r (xI i) k)
      | Node l (Some x) r =>
          xelements l (xO i)
            ((prev i, x) :: xelements r (xI i) k)
      end.

  Definition elements (A: Type) (m : t A) := xelements m xH nil.

  Remark xelements_append:
    forall A (m: t A) i k1 k2,
    xelements m i (k1 ++ k2) = xelements m i k1 ++ k2.
  Proof.
    induction m; intros; simpl.
  - auto.
  - destruct o; rewrite IHm2; rewrite <- IHm1; auto.
  Qed.

  Remark xelements_leaf:
    forall A i, xelements (@Leaf A) i nil = nil.
  Proof.
    intros; reflexivity.
  Qed.

  Remark xelements_node:
    forall A (m1: t A) o (m2: t A) i,
    xelements (Node m1 o m2) i nil =
       xelements m1 (xO i) nil
    ++ match o with None => nil | Some v => (prev i, v) :: nil end
    ++ xelements m2 (xI i) nil.
  Proof.
    intros. simpl. destruct o; simpl; rewrite <- xelements_append; auto.
  Qed.

    Lemma xelements_incl:
      forall (A: Type) (m: t A) (i : positive) k x,
      In x k -> In x (xelements m i k).
    Proof.
      induction m; intros; simpl.
      auto.
      destruct o.
      apply IHm1. simpl; right; auto.
      auto.
    Qed.

    Lemma xelements_correct:
      forall (A: Type) (m: t A) (i j : positive) (v: A) k,
      get i m = Some v -> In (prev (prev_append i j), v) (xelements m j k).
    Proof.
      induction m; intros.
       rewrite (gleaf A i) in H; congruence.
       destruct o; destruct i; simpl; simpl in H.
        apply xelements_incl. right. auto.
        auto.
        inv H. apply xelements_incl. left. reflexivity.
        apply xelements_incl. auto.
        auto.
        inv H.
    Qed.

  Theorem elements_correct:
    forall (A: Type) (m: t A) (i: positive) (v: A),
    get i m = Some v -> In (i, v) (elements m).
  Proof.
    intros A m i v H.
    generalize (xelements_correct m i xH nil H). rewrite prev_append_prev. exact id.
  Qed.

  Lemma in_xelements:
    forall (A: Type) (m: t A) (i k: positive) (v: A) ,
    In (k, v) (xelements m i nil) ->
    exists j, k = prev (prev_append j i) /\ get j m = Some v.
  Proof.
    induction m; intros.
  - rewrite xelements_leaf in H. contradiction.
  - rewrite xelements_node in H. rewrite ! in_app_iff in H. destruct H as [P | [P | P]].
    + exploit IHm1; eauto. intros (j & Q & R). exists (xO j); auto.
    + destruct o; simpl in P; intuition auto. inv H. exists xH; auto.
    + exploit IHm2; eauto. intros (j & Q & R). exists (xI j); auto.
  Qed.

  Theorem elements_complete:
    forall (A: Type) (m: t A) (i: positive) (v: A),
    In (i, v) (elements m) -> get i m = Some v.
  Proof.
    unfold elements. intros A m i v H. exploit in_xelements; eauto. intros (j & P & Q).
    rewrite prev_append_prev in P. change i with (prev_append 1 i) in P.
    exploit prev_append_inj; eauto. intros; congruence.
  Qed.

  Definition xkeys (A: Type) (m: t A) (i: positive) :=
    List.map (@fst positive A) (xelements m i nil).

  Remark xkeys_leaf:
    forall A i, xkeys (@Leaf A) i = nil.
  Proof.
    intros; reflexivity.
  Qed.

  Remark xkeys_node:
    forall A (m1: t A) o (m2: t A) i,
    xkeys (Node m1 o m2) i =
       xkeys m1 (xO i)
    ++ match o with None => nil | Some v => prev i :: nil end
    ++ xkeys m2 (xI i).
  Proof.
    intros. unfold xkeys. rewrite xelements_node. rewrite ! map_app. destruct o; auto.
  Qed.

  Lemma in_xkeys:
    forall (A: Type) (m: t A) (i k: positive),
    In k (xkeys m i) ->
    (exists j, k = prev (prev_append j i)).
  Proof.
    unfold xkeys; intros.
    apply (list_in_map_inv) in H. destruct H as ((j, v) & -> & H).
    exploit in_xelements; eauto. intros (k & P & Q). exists k; auto.
  Qed.

  Lemma xelements_keys_norepet:
    forall (A: Type) (m: t A) (i: positive),
    list_norepet (xkeys m i).
  Proof.
    induction m; intros.
  - rewrite xkeys_leaf; constructor.
  - assert (NOTIN1: ~ In (prev i) (xkeys m1 (xO i))).
    { red; intros. exploit in_xkeys; eauto. intros (j & EQ).
      rewrite prev_append_prev in EQ. simpl in EQ. apply prev_append_inj in EQ. discriminate. }
    assert (NOTIN2: ~ In (prev i) (xkeys m2 (xI i))).
    { red; intros. exploit in_xkeys; eauto. intros (j & EQ).
      rewrite prev_append_prev in EQ. simpl in EQ. apply prev_append_inj in EQ. discriminate. }
    assert (DISJ: forall x, In x (xkeys m1 (xO i)) -> In x (xkeys m2 (xI i)) -> False).
    { intros. exploit in_xkeys. eexact H. intros (j1 & EQ1).
      exploit in_xkeys. eexact H0. intros (j2 & EQ2).
      rewrite prev_append_prev in *. simpl in *. rewrite EQ2 in EQ1. apply prev_append_inj in EQ1. discriminate. }
    rewrite xkeys_node. apply list_norepet_append. auto.
    destruct o; simpl; auto. constructor; auto.
    red; intros. red; intros; subst y. destruct o; simpl in H0.
    destruct H0. subst x. tauto. eauto. eauto.
  Qed.

  Theorem elements_keys_norepet:
    forall (A: Type) (m: t A),
    list_norepet (List.map (@fst elt A) (elements m)).
  Proof.
    intros. apply (xelements_keys_norepet m xH).
  Qed.

  Remark xelements_empty:
    forall (A: Type) (m: t A) i, (forall i, get i m = None) -> xelements m i nil = nil.
  Proof.
    induction m; intros.
    auto.
    rewrite xelements_node. rewrite IHm1, IHm2. destruct o; auto.
    generalize (H xH); simpl; congruence.
    intros. apply (H (xI i0)).
    intros. apply (H (xO i0)).
  Qed.

  Theorem elements_canonical_order':
    forall (A B: Type) (R: A -> B -> Prop) (m: t A) (n: t B),
    (forall i, option_rel R (get i m) (get i n)) ->
    list_forall2
      (fun i_x i_y => fst i_x = fst i_y /\ R (snd i_x) (snd i_y))
      (elements m) (elements n).
  Proof.
    intros until n. unfold elements. generalize 1%positive. revert m n.
    induction m; intros.
  - simpl. rewrite xelements_empty. constructor.
    intros. specialize (H i). rewrite gempty in H. inv H; auto.
  - destruct n as [ | n1 o' n2 ].
  + rewrite (xelements_empty (Node m1 o m2)). simpl; constructor.
    intros. specialize (H i). rewrite gempty in H. inv H; auto.
  + rewrite ! xelements_node. repeat apply list_forall2_app.
    apply IHm1. intros. apply (H (xO i)).
    generalize (H xH); simpl; intros OR; inv OR.
    constructor.
    constructor. auto. constructor.
    apply IHm2. intros. apply (H (xI i)).
  Qed.

  Theorem elements_canonical_order:
    forall (A B: Type) (R: A -> B -> Prop) (m: t A) (n: t B),
    (forall i x, get i m = Some x -> exists y, get i n = Some y /\ R x y) ->
    (forall i y, get i n = Some y -> exists x, get i m = Some x /\ R x y) ->
    list_forall2
      (fun i_x i_y => fst i_x = fst i_y /\ R (snd i_x) (snd i_y))
      (elements m) (elements n).
  Proof.
    intros. apply elements_canonical_order'.
    intros. destruct (get i m) as [x|] eqn:GM.
    exploit H; eauto. intros (y & P & Q). rewrite P; constructor; auto.
    destruct (get i n) as [y|] eqn:GN.
    exploit H0; eauto. intros (x & P & Q). congruence.
    constructor.
  Qed.

  Theorem elements_extensional:
    forall (A: Type) (m n: t A),
    (forall i, get i m = get i n) ->
    elements m = elements n.
  Proof.
    intros.
    exploit (@elements_canonical_order' _ _ (fun (x y: A) => x = y) m n).
    intros. rewrite H. destruct (get i n); constructor; auto.
    induction 1. auto. destruct a1 as [a2 a3]; destruct b1 as [b2 b3]; simpl in *.
    destruct H0. congruence.
  Qed.

  Lemma xelements_remove:
    forall (A: Type) v (m: t A) i j,
    get i m = Some v ->
    exists l1 l2,
    xelements m j nil = l1 ++ (prev (prev_append i j), v) :: l2
    /\ xelements (remove i m) j nil = l1 ++ l2.
  Proof.
    induction m; intros.
  - rewrite gleaf in H; discriminate.
  - assert (REMOVE: xelements (remove i (Node m1 o m2)) j nil =
                    xelements (match i with
                               | xH => Node m1 None m2
                               | xO ii => Node (remove ii m1) o m2
                               | xI ii => Node m1 o (remove ii m2) end)
                              j nil).
    {
      destruct i; simpl remove.
      destruct m1; auto. destruct o; auto. destruct (remove i m2); auto.
      destruct o; auto. destruct m2; auto. destruct (remove i m1); auto.
      destruct m1; auto. destruct m2; auto.
    }
    rewrite REMOVE. destruct i; simpl in H.
    + destruct (IHm2 i (xI j) H) as (l1 & l2 & EQ & EQ').
      exists (xelements m1 (xO j) nil ++
              match o with None => nil | Some x => (prev j, x) :: nil end ++
              l1);
      exists l2; split.
      rewrite xelements_node, EQ, ! app_ass. auto.
      rewrite xelements_node, EQ', ! app_ass. auto.
    + destruct (IHm1 i (xO j) H) as (l1 & l2 & EQ & EQ').
      exists l1;
      exists (l2 ++
              match o with None => nil | Some x => (prev j, x) :: nil end ++
              xelements m2 (xI j) nil);
      split.
      rewrite xelements_node, EQ, ! app_ass. auto.
      rewrite xelements_node, EQ', ! app_ass. auto.
    + subst o. exists (xelements m1 (xO j) nil); exists (xelements m2 (xI j) nil); split.
      rewrite xelements_node. rewrite prev_append_prev. auto.
      rewrite xelements_node; auto.
  Qed.

  Theorem elements_remove:
    forall (A: Type) i v (m: t A),
    get i m = Some v ->
    exists l1 l2, elements m = l1 ++ (i,v) :: l2 /\ elements (remove i m) = l1 ++ l2.
  Proof.
    intros. exploit xelements_remove. eauto. instantiate (1 := xH).
    rewrite prev_append_prev. auto.
  Qed.

  Fixpoint xfold (A B: Type) (f: B -> positive -> A -> B)
                 (i: positive) (m: t A) (v: B) {struct m} : B :=
    match m with
    | Leaf => v
    | Node l None r =>
        let v1 := xfold f (xO i) l v in
        xfold f (xI i) r v1
    | Node l (Some x) r =>
        let v1 := xfold f (xO i) l v in
        let v2 := f v1 (prev i) x in
        xfold f (xI i) r v2
    end.

  Definition fold (A B : Type) (f: B -> positive -> A -> B) (m: t A) (v: B) :=
    xfold f xH m v.

  Lemma xfold_xelements:
    forall (A B: Type) (f: B -> positive -> A -> B) m i v l,
    List.fold_left (fun a p => f a (fst p) (snd p)) l (xfold f i m v) =
    List.fold_left (fun a p => f a (fst p) (snd p)) (xelements m i l) v.
  Proof.
    induction m; intros.
    simpl. auto.
    destruct o; simpl.
    rewrite <- IHm1. simpl. rewrite <- IHm2. auto.
    rewrite <- IHm1. rewrite <- IHm2. auto.
  Qed.

  Theorem fold_spec:
    forall (A B: Type) (f: B -> positive -> A -> B) (v: B) (m: t A),
    fold f m v =
    List.fold_left (fun a p => f a (fst p) (snd p)) (elements m) v.
  Proof.
    intros. unfold fold, elements. rewrite <- xfold_xelements. auto.
  Qed.

  Fixpoint fold1 (A B: Type) (f: B -> A -> B) (m: t A) (v: B) {struct m} : B :=
    match m with
    | Leaf => v
    | Node l None r =>
        let v1 := fold1 f l v in
        fold1 f r v1
    | Node l (Some x) r =>
        let v1 := fold1 f l v in
        let v2 := f v1 x in
        fold1 f r v2
    end.

  Lemma fold1_xelements:
    forall (A B: Type) (f: B -> A -> B) m i v l,
    List.fold_left (fun a p => f a (snd p)) l (fold1 f m v) =
    List.fold_left (fun a p => f a (snd p)) (xelements m i l) v.
  Proof.
    induction m; intros.
    simpl. auto.
    destruct o; simpl.
    rewrite <- IHm1. simpl. rewrite <- IHm2. auto.
    rewrite <- IHm1. rewrite <- IHm2. auto.
  Qed.

  Theorem fold1_spec:
    forall (A B: Type) (f: B -> A -> B) (v: B) (m: t A),
    fold1 f m v =
    List.fold_left (fun a p => f a (snd p)) (elements m) v.
  Proof.
    intros. apply fold1_xelements with (l := @nil (positive * A)).
  Qed.

  (* This is used in the proofless backend as a simple way to union two globalenvs.
     It will not be well-behaved if there is overlap between the keys, and we
     currently don't prove any theorems about it. *)
  Definition union (A:Type) (m1 m2 : t A) : t A :=
    fold (fun m k v => set k v m) m1 m2.

  Definition get_default {A:Type} (default:A) (k:elt) (m : t A) : A :=
    match get k m with
    | Some v => v
    | NONE => default
    end.

End PTree.

(** * An implementation of maps over type [positive] *)

Module PMap <: MAP.
  Definition elt := positive.
  Definition elt_eq := peq.

  Definition t (A : Type) : Type := (A * PTree.t A)%type.

  Definition init (A : Type) (x : A) :=
    (x, PTree.empty A).

  Definition get (A : Type) (i : positive) (m : t A) :=
    match PTree.get i (snd m) with
    | Some x => x
    | None => fst m
    end.

  Definition set (A : Type) (i : positive) (x : A) (m : t A) :=
    (fst m, PTree.set i x (snd m)).

  Theorem gi:
    forall (A: Type) (i: positive) (x: A), get i (init x) = x.
  Proof.
    intros. unfold init. unfold get. simpl. rewrite PTree.gempty. auto.
  Qed.

  Theorem gss:
    forall (A: Type) (i: positive) (x: A) (m: t A), get i (set i x m) = x.
  Proof.
    intros. unfold get. unfold set. simpl. rewrite PTree.gss. auto.
  Qed.

  Theorem gso:
    forall (A: Type) (i j: positive) (x: A) (m: t A),
    i <> j -> get i (set j x m) = get i m.
  Proof.
    intros. unfold get. unfold set. simpl. rewrite PTree.gso; auto.
  Qed.

  Theorem gsspec:
    forall (A: Type) (i j: positive) (x: A) (m: t A),
    get i (set j x m) = if peq i j then x else get i m.
  Proof.
    intros. destruct (peq i j).
     rewrite e. apply gss. auto.
     apply gso. auto.
  Qed.

  Theorem gsident:
    forall (A: Type) (i j: positive) (m: t A),
    get j (set i (get i m) m) = get j m.
  Proof.
    intros. destruct (peq i j).
     rewrite e. rewrite gss. auto.
     rewrite gso; auto.
  Qed.

  Definition map (A B : Type) (f : A -> B) (m : t A) : t B :=
    (f (fst m), PTree.map1 f (snd m)).

  Theorem gmap:
    forall (A B: Type) (f: A -> B) (i: positive) (m: t A),
    get i (map f m) = f(get i m).
  Proof.
    intros. unfold map. unfold get. simpl. rewrite PTree.gmap1.
    unfold option_map. destruct (PTree.get i (snd m)); auto.
  Qed.

  Theorem set2:
    forall (A: Type) (i: elt) (x y: A) (m: t A),
    set i y (set i x m) = set i y m.
  Proof.
    intros. unfold set. simpl. decEq. apply PTree.set2.
  Qed.

End PMap.

(** * An implementation of maps over any type that injects into type [positive] *)

Module Type INDEXED_TYPE.
  Parameter t: Type.
  Parameter index: t -> positive.
  Parameter index_inv : positive -> t.
  Axiom index_invertible : forall x, index_inv (index x) = x.
  Axiom index_inj: forall (x y: t), index x = index y -> x = y.
  
  Parameter eq: forall (x y: t), {x = y} + {x <> y}.
End INDEXED_TYPE.

Module IMap(X: INDEXED_TYPE).

  Definition elt := X.t.
  Definition elt_eq := X.eq.
  Definition t : Type -> Type := PMap.t.
  Definition init (A: Type) (x: A) := PMap.init x.
  Definition get (A: Type) (i: X.t) (m: t A) := PMap.get (X.index i) m.
  Definition set (A: Type) (i: X.t) (v: A) (m: t A) := PMap.set (X.index i) v m.
  Definition map (A B: Type) (f: A -> B) (m: t A) : t B := PMap.map f m.

  Lemma gi:
    forall (A: Type) (x: A) (i: X.t), get i (init x) = x.
  Proof.
    intros. unfold get, init. apply PMap.gi.
  Qed.

  Lemma gss:
    forall (A: Type) (i: X.t) (x: A) (m: t A), get i (set i x m) = x.
  Proof.
    intros. unfold get, set. apply PMap.gss.
  Qed.

  Lemma gso:
    forall (A: Type) (i j: X.t) (x: A) (m: t A),
    i <> j -> get i (set j x m) = get i m.
  Proof.
    intros. unfold get, set. apply PMap.gso.
    red. intro. apply H. apply X.index_inj; auto.
  Qed.

  Lemma gsspec:
    forall (A: Type) (i j: X.t) (x: A) (m: t A),
    get i (set j x m) = if X.eq i j then x else get i m.
  Proof.
    intros. unfold get, set.
    rewrite PMap.gsspec.
    case (X.eq i j); intro.
    subst j. rewrite peq_true. reflexivity.
    rewrite peq_false. reflexivity.
    red; intro. elim n. apply X.index_inj; auto.
  Qed.

  Lemma gmap:
    forall (A B: Type) (f: A -> B) (i: X.t) (m: t A),
    get i (map f m) = f(get i m).
  Proof.
    intros. unfold map, get. apply PMap.gmap.
  Qed.

  Lemma set2:
    forall (A: Type) (i: elt) (x y: A) (m: t A),
    set i y (set i x m) = set i y m.
  Proof.
    intros. unfold set. apply PMap.set2.
  Qed.

  (* This is used in the proofless backend as a simple way to union two globalenvs.
     It will not be well-behaved if there is overlap between the keys, and we
     currently don't prove any theorems about it. *)
  Definition union (A:Type) (m1 m2 : t A) : t A :=
    (fst m1, PTree.union (snd m1) (snd m2)).
End IMap.

Module ZIndexed.
  Definition t := Z.
  Definition index (z: Z): positive :=
    match z with
    | Z0 => xH
    | Zpos p => xO p
    | Zneg p => xI p
    end.

  Definition index_inv (p:positive) : Z :=
    match p with
    | xH => Z0
    | xO p' => Zpos p'
    | xI p' => Zneg p'
    end.

  Lemma index_invertible : forall x,
    index_inv (index x) = x.
  Proof.  
    intros; destruct x; reflexivity.
  Qed.
    
  Lemma index_inj: forall (x y: Z), index x = index y -> x = y.
  Proof.
    intros.
    assert (H0: index_inv (index x) = index_inv (index y)) by congruence.
    rewrite ?index_invertible in H0.
    assumption.
  Qed.

  Definition eq := zeq.
End ZIndexed.

Module ZMap := IMap(ZIndexed).

Module NIndexed.
  Definition t := N.
  Definition index (n: N): positive :=
    match n with
    | N0 => xH
    | Npos p => xO p
    end.

  Definition index_inv (p:positive) : N :=
    match p with
    | xH => N0
    | xO p' => Npos p'
    | xI _ => N0
    end.

  Lemma index_invertible : forall x,
    index_inv (index x) = x.
  Proof.  
    intros; destruct x; reflexivity.
  Qed.
    
  Lemma index_inj: forall (x y: N), index x = index y -> x = y.
  Proof.
    intros.
    assert (H0: index_inv (index x) = index_inv (index y)) by congruence.
    rewrite ?index_invertible in H0.
    assumption.
  Qed.

  
  Lemma eq: forall (x y: N), {x = y} + {x <> y}.
  Proof.
    decide equality. apply peq.
  Qed.
End NIndexed.

Module NMap := IMap(NIndexed).

(** * An implementation of maps over any type with decidable equality *)

Module Type EQUALITY_TYPE.
  Parameter t: Type.
  Parameter eq: forall (x y: t), {x = y} + {x <> y}.
End EQUALITY_TYPE.

Module EMap(X: EQUALITY_TYPE) <: MAP.

  Definition elt := X.t.
  Definition elt_eq := X.eq.
  Definition t (A: Type) := X.t -> A.
  Definition init (A: Type) (v: A) := fun (_: X.t) => v.
  Definition get (A: Type) (x: X.t) (m: t A) := m x.
  Definition set (A: Type) (x: X.t) (v: A) (m: t A) :=
    fun (y: X.t) => if X.eq y x then v else m y.
  Lemma gi:
    forall (A: Type) (i: elt) (x: A), init x i = x.
  Proof.
    intros. reflexivity.
  Qed.
  Lemma gss:
    forall (A: Type) (i: elt) (x: A) (m: t A), (set i x m) i = x.
  Proof.
    intros. unfold set. case (X.eq i i); intro.
    reflexivity. tauto.
  Qed.
  Lemma gso:
    forall (A: Type) (i j: elt) (x: A) (m: t A),
    i <> j -> (set j x m) i = m i.
  Proof.
    intros. unfold set. case (X.eq i j); intro.
    congruence. reflexivity.
  Qed.
  Lemma gsspec:
    forall (A: Type) (i j: elt) (x: A) (m: t A),
    get i (set j x m) = if elt_eq i j then x else get i m.
  Proof.
    intros. unfold get, set, elt_eq. reflexivity.
  Qed.
  Lemma gsident:
    forall (A: Type) (i j: elt) (m: t A), get j (set i (get i m) m) = get j m.
  Proof.
    intros. unfold get, set. case (X.eq j i); intro.
    congruence. reflexivity.
  Qed.
  Definition map (A B: Type) (f: A -> B) (m: t A) :=
    fun (x: X.t) => f(m x).
  Lemma gmap:
    forall (A B: Type) (f: A -> B) (i: elt) (m: t A),
    get i (map f m) = f(get i m).
  Proof.
    intros. unfold get, map. reflexivity.
  Qed.
End EMap.

(** * An implementation of trees over any type that injects into type [positive] *)

Module ITree(X: INDEXED_TYPE) <: TREE.

  Definition elt := X.t.
  Definition elt_eq := X.eq.
  Definition t (A: Type) : Type :=
    { tr : PTree.t A | forall x, PTree.get x tr <> None ->
                                 X.index (X.index_inv x) = x }.

  Program Definition empty (A: Type): t A := PTree.empty A.
  Next Obligation.
    rewrite PTree.gempty in H.
    congruence.
  Qed.
    
  Program Definition get (A: Type) (k: elt) (m: t A): option A := PTree.get (X.index k) m.

  Definition get_default {A:Type} (default:A) (k:elt) (m : t A) : A :=
    match get k m with
    | Some v => v
    | NONE => default
    end.

  Program Definition set (A: Type) (k: elt) (v: A) (m: t A): t A
    := PTree.set (X.index k) v m.
  Next Obligation.
    destruct m as [m INV]; simpl in H.
    destruct (peq x (X.index k)).
    + rewrite e.
      rewrite X.index_invertible.
      reflexivity.
    + rewrite PTree.gso in H by assumption.
      auto.
  Qed.
      
  Program Definition remove (A: Type) (k: elt) (m: t A): t A
    := PTree.remove (X.index k) m.
  Next Obligation.
    destruct m as [m INV]; simpl in H.
    destruct (peq x (X.index k)).
    + rewrite e.
      rewrite X.index_invertible.
      reflexivity.
    + rewrite PTree.gro in H by assumption.
      auto.
  Qed.    
    
  Theorem gempty:
    forall (A: Type) (i: elt), get i (empty A) = None.
  Proof.
    intros. apply PTree.gempty.
  Qed.
  
  Theorem gss:
    forall (A: Type) (i: elt) (x: A) (m: t A), get i (set i x m) = Some x.
  Proof.
    intros. apply PTree.gss.
  Qed.
  
  Theorem gso:
    forall (A: Type) (i j: elt) (x: A) (m: t A),
    i <> j -> get i (set j x m) = get i m.
  Proof.
    intros. apply PTree.gso. red; intros; elim H; apply X.index_inj; auto.
  Qed.
  
  Theorem gsspec:
    forall (A: Type) (i j: elt) (x: A) (m: t A),
    get i (set j x m) = if elt_eq i j then Some x else get i m.
  Proof.
    intros. destruct (elt_eq i j). subst j; apply gss. apply gso; auto.
  Qed.
  
  Theorem grs:
    forall (A: Type) (i: elt) (m: t A), get i (remove i m) = None.
  Proof.
    intros. apply PTree.grs.
  Qed.
  
  Theorem gro:
    forall (A: Type) (i j: elt) (m: t A),
    i <> j -> get i (remove j m) = get i m.
  Proof.
    intros. apply PTree.gro. red; intros; elim H; apply X.index_inj; auto.
  Qed.
  
  Theorem grspec:
    forall (A: Type) (i j: elt) (m: t A),
    get i (remove j m) = if elt_eq i j then None else get i m.
  Proof.
    intros. destruct (elt_eq i j). subst j; apply grs. apply gro; auto.
  Qed.

  Program Definition beq: forall (A: Type), (A -> A -> bool) -> t A -> t A -> bool
    := PTree.beq.
  
  Theorem beq_correct:
    forall (A: Type) (eqA: A -> A -> bool) (t1 t2: t A),
    beq eqA t1 t2 = true <->
    forall (x: elt),
     match get x t1, get x t2 with
     | None, None => True
     | Some y1, Some y2 => eqA y1 y2 = true
     | _, _ => False
    end.
  Proof.
    unfold beq, get. split; intros.
    + rewrite PTree.beq_correct in H. apply H.
    + rewrite PTree.beq_correct.
      destruct t1 as [t1 INV1].
      destruct t2 as [t2 INV2].
      simpl in *.
      intros x.
      specialize (H (X.index_inv x)).
      destruct (PTree.get x t1) eqn:e1; destruct (PTree.get x t2) eqn:e2; simpl.
      * rewrite (INV1 x), e1, e2 in H; congruence.
      * rewrite (INV1 x), e1, e2 in H; congruence.
      * rewrite (INV2 x), e1, e2 in H; congruence.
      * auto.
  Qed.


  (* This probably could be defined, but it seems annoying...*)
  (*
  Program Definition combine: forall (A B C: Type), (option A -> option B -> option C) -> t A -> t B -> t C := PTree.combine.
  Next Obligation.
    destruct x3 as [m1 INV1].
    destruct x4 as [m2 INV2].
    simpl in *.
    
  
  Theorem gcombine:
    forall (A B C: Type) (f: option A -> option B -> option C),
    f None None = None ->
    forall (m1: t A) (m2: t B) (i: elt),
    get i (combine f m1 m2) = f (get i m1) (get i m2).
  Proof.
    intros. apply PTree.gcombine. auto.
  Qed.
  *)

  Program Definition map (A B: Type) (f : elt -> A -> B) (m: t A) : t B :=
    PTree.map (fun p a => f (X.index_inv p) a) m.
  Next Obligation.
    destruct m as [m INV].
    simpl in H.
    apply INV.
    rewrite PTree.gmap in H.
    intros C.
    rewrite C in H.
    simpl in H.
    congruence.
  Qed.
    
  Theorem gmap:
    forall (A B: Type) (f: elt -> A -> B) (i: elt) (m: t A),
      get i (map f m) = option_map (f i) (get i m).
  Proof.
    intros.
    unfold get, map.
    replace (f i) with (f (X.index_inv (X.index i)))
      by (f_equal; exact (X.index_invertible i)). 
    change (f (X.index_inv (X.index i)))
     with  ((fun (p : positive) (a : A) => f (X.index_inv p) a) (X.index i)).
    apply PTree.gmap.
  Qed.

  Program Definition map1:
    forall (A B: Type), (A -> B) -> t A -> t B := PTree.map1.
  Next Obligation.
    destruct x2 as [m INV].
    simpl in H.
    apply INV.
    rewrite PTree.gmap1 in H.
    intros C.
    rewrite C in H.
    simpl in H.
    congruence.
  Qed.

  Theorem gmap1:
    forall (A B: Type) (f: A -> B) (i: elt) (m: t A),
      get i (map1 f m) = option_map f (get i m).
  Proof.
    unfold get, map1.
    intros.
    apply PTree.gmap1.
  Qed.
    
  (** Enumerating the bindings of a tree. *)
  Program Definition elements (A: Type) (m: t A) : list (elt * A)
    := List.map (fun xA => (X.index_inv (fst xA), snd xA)) (PTree.elements m).
                                       
  Theorem elements_correct:
    forall (A: Type) (m: t A) (i: elt) (v: A),
      get i m = Some v -> In (i, v) (elements m).
  Proof.
    unfold get, elements.
    intros.
    apply PTree.elements_correct in H.
    rewrite List.in_map_iff.
    exists (X.index i, v).
    split; auto.
    simpl.
    rewrite X.index_invertible.
    reflexivity.
  Qed.

  Theorem elements_complete:
    forall (A: Type) (m: t A) (i: elt) (v: A),
      In (i, v) (elements m) -> get i m = Some v.
  Proof.    
    unfold elements, get.
    intros.
    destruct m as [m INV].
    simpl in *.
    apply PTree.elements_complete.
    rewrite in_map_iff in H.
    destruct H as [[p u] [H1 H2]].
    simpl in H1.
    inversion H1. subst.
    rewrite INV.
    + assumption.
    + apply PTree.elements_complete in H2.
      congruence.
  Qed.
  
  Theorem elements_keys_norepet:
    forall (A: Type) (m: t A),
      list_norepet (List.map (@fst elt A) (elements m)).
  Proof.
    intros.
    destruct m as [m INV].
    unfold elements.
    rewrite map_map.
    simpl.
    rewrite <- (@map_map _ _ _ fst). 
    apply list_map_norepet.
    + apply PTree.elements_keys_norepet.
    + intros.
      intros C.
      assert (X.index (X.index_inv x) = X.index (X.index_inv y)) by congruence.
      rewrite (INV x) in H2.
      rewrite (INV y) in H2.
      * congruence.
      * rewrite in_map_iff in H0.
        destruct H0 as [[k a] [Hfst Hin]].
        apply PTree.elements_complete in Hin.
        simpl in Hfst; inversion Hfst; subst.
        congruence.
      * rewrite in_map_iff in H.
        destruct H as [[k a] [Hfst Hin]].
        apply PTree.elements_complete in Hin.
        simpl in Hfst; inversion Hfst; subst.
        congruence.
  Qed.
  
  Theorem elements_extensional:
    forall (A: Type) (m n: t A),
    (forall i, get i m = get i n) ->
    elements m = elements n.
  Proof.
    unfold get, elements.
    destruct m as [m INVm].
    destruct n as [n INVn].
    simpl.
    intros.
    rewrite (PTree.elements_extensional n m); [reflexivity|].
    intros i.
    specialize (H (X.index_inv i)).
    destruct (PTree.get i n) eqn:?;
    destruct (PTree.get i m) eqn:?.
    * rewrite (INVm i) in H; congruence.
    * rewrite (INVn i) in H; congruence.
    * rewrite (INVm i) in H; congruence.
    * reflexivity.
  Qed.
  
  Theorem elements_remove:
    forall (A: Type) i v (m: t A),
    get i m = Some v ->
    exists l1 l2, elements m = l1 ++ (i,v) :: l2 /\ elements (remove i m) = l1 ++ l2.
  Proof.
    unfold get, remove, elements.
    intros.
    destruct m as [m INV].
    simpl in *.
    assert (H' := PTree.elements_remove _ _ H).
    destruct H' as [l1 [l2 [H1 H2]]].
    exists (List.map (fun xA : positive * A => (X.index_inv (fst xA), snd xA)) l1).
    exists (List.map (fun xA : positive * A => (X.index_inv (fst xA), snd xA)) l2).
    rewrite H1.
    rewrite H2.
    repeat rewrite map_app.
    simpl.
    rewrite X.index_invertible.
    split; reflexivity.
  Qed.
    
  (** Folding a function over all bindings of a tree. *)
  Program Definition fold (A B: Type) (f : B -> elt -> A -> B) (m : t A) (b: B) : B
    := PTree.fold (fun b x a => f b (X.index_inv x) a) m b.

  Lemma fold_left_equal : forall A B (f g : A->B->A) l v,
      (forall x y, f x y = g x y) ->
      fold_left f l v = fold_left g l v.
  Proof.
    induction l.
    - reflexivity.
    - simpl. intros.
      rewrite (IHl _ H).
      rewrite H.
      reflexivity.
  Qed.

  Lemma fold_left_map : forall A B C (f : A->B->A) (g : C -> B) (l : list C) v,
      fold_left f (List.map g l) v = fold_left (fun x y => f x (g y)) l v.
  Proof.
    induction l.
    - reflexivity.
    - simpl. intros.
      rewrite IHl.
      reflexivity.
  Qed.
  
  Theorem fold_spec:
    forall (A B: Type) (f: B -> elt -> A -> B) (v: B) (m: t A),
    fold f m v =
    List.fold_left (fun a p => f a (fst p) (snd p)) (elements m) v.
  Proof.
    intros.
    unfold fold, elements.
    rewrite PTree.fold_spec.
    rewrite fold_left_map.
    apply fold_left_equal.
    reflexivity.
  Qed.
  
  Program Definition fold1 (A B: Type) (f : B -> A -> B) (m: t A) (v: B) : B
    := PTree.fold1 f m v.
  
  Theorem fold1_spec:
    forall (A B: Type) (f: B -> A -> B) (v: B) (m: t A),
    fold1 f m v =
    List.fold_left (fun a p => f a (snd p)) (elements m) v.
  Proof.
    intros.
    unfold fold1, elements.
    rewrite PTree.fold1_spec.
    rewrite fold_left_map.
    apply fold_left_equal.
    reflexivity.
  Qed.

End ITree.

Module ZTree := ITree(ZIndexed).

Require Import cclib.Integers.

Module Int256Indexed <: INDEXED_TYPE.

Definition t := int256.
Definition index (i : int256) :=
  let (intval, _) := i in
  match intval with
  | Z0 => xH
  | Zpos p => xO p
  | Zneg p => xI p
  end.

Definition index_inv p :=
  match p with
  | xH => Int256.zero
  | xO p' => Int256.repr (Zpos p')
  | xI p' => Int256.repr (Zneg p')
  end.

  Lemma zlt_proof_irrelevance :
    forall {x y : Z}  (p q : x < y), p = q.
  Proof.
    intros.
    apply Eqdep_dec.eq_proofs_unicity.
    decide equality.
  Qed.

  Lemma range_proof_irrelevance :
    forall {x y z : Z}  (p q : x < y < z), p = q.
  Proof.
    intros.
    destruct p as [p1 p2].
    destruct q as [q1 q2].
    rewrite (zlt_proof_irrelevance p1 q1).
    rewrite (zlt_proof_irrelevance p2 q2).
    reflexivity.
  Qed.

  Transparent Int256.Z_mod_modulus.
  Lemma modulus_nop : forall x,
      -1 < x < Int256.modulus -> Int256.Z_mod_modulus x = x.
  Proof.
    intros.
    unfold Int256.Z_mod_modulus.
    destruct x.
    + reflexivity.
    + rewrite Int256.P_mod_two_p_eq.
      rewrite Zmod_small by (unfold Int256.modulus in H; omega).
      reflexivity.
    + destruct H.
      unfold Z.lt in H.
      simpl in H.
      destruct p; simpl in H; congruence.
  Qed.
  Opaque Int256.Z_mod_modulus.
  
  Transparent Int256.repr.
  Lemma index_invertible : forall x, index_inv (index x) = x.
  Proof.
    intros.
    destruct x as [x x_range].
    destruct x; simpl.
    + unfold Int256.zero.
      unfold Int256.repr.
      rewrite (range_proof_irrelevance (Int256.Z_mod_modulus_range' 0) x_range).
      reflexivity.
    + unfold Int256.repr.
      generalize (Int256.Z_mod_modulus_range' (Z.pos p)).      
      rewrite (modulus_nop x_range).
      intros a.
      rewrite (range_proof_irrelevance a x_range).
      reflexivity.
    + unfold Int256.repr.
      generalize (Int256.Z_mod_modulus_range' (Z.neg p)).      
      rewrite (modulus_nop x_range).
      intros a.
      rewrite (range_proof_irrelevance a x_range).
      reflexivity.
  Qed.
  Opaque Int256.repr.
  
  Lemma index_inj: forall (x y: int256), index x = index y -> x = y.
  Proof.
    unfold index; destruct x; destruct y; intros;
      try discriminate; try reflexivity.
    apply Int256.mkint_eq.
    destruct intval; destruct intval0; try (inversion H); try reflexivity.
  Qed.

  Definition eq := Int256.eq_dec.

End Int256Indexed.

Module Int256Tree := ITree(Int256Indexed).
Module Int256Map := IMap(Int256Indexed).

(** * Additional properties over trees *)

Module Tree_Properties(T: TREE).

  (** An induction principle over [fold]. *)

  Section TREE_FOLD_IND.

  Variables V A: Type.
  Variable f: A -> T.elt -> V -> A.
  Variable P: T.t V -> A -> Prop.
  Variable init: A.
  Variable m_final: T.t V.

  Hypothesis P_compat:
    forall m m' a,
    (forall x, T.get x m = T.get x m') ->
    P m a -> P m' a.

  Hypothesis H_base:
    P (T.empty _) init.

  Hypothesis H_rec:
    forall m a k v,
    T.get k m = None -> T.get k m_final = Some v -> P m a -> P (T.set k v m) (f a k v).

  Let f' (a: A) (p : T.elt * V) := f a (fst p) (snd p).

  Let P' (l: list (T.elt * V)) (a: A) : Prop :=
    forall m, list_equiv l (T.elements m) -> P m a.

  Remark H_base':
    P' nil init.
  Proof.
    red; intros. apply P_compat with (T.empty _); auto.
    intros. rewrite T.gempty. symmetry. case_eq (T.get x m); intros; auto.
    assert (In (x, v) nil). rewrite (H (x, v)). apply T.elements_correct. auto.
    contradiction.
  Qed.

  Remark H_rec':
    forall k v l a,
    ~In k (List.map (@fst T.elt V) l) ->
    In (k, v) (T.elements m_final) ->
    P' l a ->
    P' (l ++ (k, v) :: nil) (f a k v).
  Proof.
    unfold P'; intros.
    set (m0 := T.remove k m).
    apply P_compat with (T.set k v m0).
      intros. unfold m0. rewrite T.gsspec. destruct (T.elt_eq x k).
      symmetry. apply T.elements_complete. rewrite <- (H2 (x, v)).
      apply in_or_app. simpl. intuition congruence.
      apply T.gro. auto.
    apply H_rec. unfold m0. apply T.grs. apply T.elements_complete. auto.
    apply H1. red. intros [k' v'].
    split; intros.
    apply T.elements_correct. unfold m0. rewrite T.gro. apply T.elements_complete.
    rewrite <- (H2 (k', v')). apply in_or_app. auto.
    red; intro; subst k'. elim H. change k with (fst (k, v')). apply in_map. auto.
    assert (T.get k' m0 = Some v'). apply T.elements_complete. auto.
    unfold m0 in H4. rewrite T.grspec in H4. destruct (T.elt_eq k' k). congruence.
    assert (In (k', v') (T.elements m)). apply T.elements_correct; auto.
    rewrite <- (H2 (k', v')) in H5. destruct (in_app_or _ _ _ H5). auto.
    simpl in H6. intuition congruence.
  Qed.

  Lemma fold_rec_aux:
    forall l1 l2 a,
    list_equiv (l2 ++ l1) (T.elements m_final) ->
    list_disjoint (List.map (@fst T.elt V) l1) (List.map (@fst T.elt V) l2) ->
    list_norepet (List.map (@fst T.elt V) l1) ->
    P' l2 a -> P' (l2 ++ l1) (List.fold_left f' l1 a).
  Proof.
    induction l1; intros; simpl.
    rewrite <- List.app_nil_end. auto.
    destruct a as [k v]; simpl in *. inv H1.
    change ((k, v) :: l1) with (((k, v) :: nil) ++ l1). rewrite <- List.app_ass. apply IHl1.
    rewrite app_ass. auto.
    red; intros. rewrite map_app in H3. destruct (in_app_or _ _ _ H3). apply H0; auto with coqlib.
    simpl in H4. intuition congruence.
    auto.
    unfold f'. simpl. apply H_rec'; auto. eapply list_disjoint_notin; eauto with coqlib.
    rewrite <- (H (k, v)). apply in_or_app. simpl. auto.
  Qed.

  Theorem fold_rec:
    P m_final (T.fold f m_final init).
  Proof.
    intros. rewrite T.fold_spec. fold f'.
    assert (P' (nil ++ T.elements m_final) (List.fold_left f' (T.elements m_final) init)).
      apply fold_rec_aux.
      simpl. red; intros; tauto.
      simpl. red; intros. elim H0.
      apply T.elements_keys_norepet.
      apply H_base'.
    simpl in H. red in H. apply H. red; intros. tauto.
  Qed.

  End TREE_FOLD_IND.

  (** A nonnegative measure over trees *)

  Section MEASURE.

  Variable V: Type.

  Definition cardinal (x: T.t V) : nat := List.length (T.elements x).

  Theorem cardinal_remove:
    forall x m y, T.get x m = Some y -> (cardinal (T.remove x m) < cardinal m)%nat.
  Proof.
    unfold cardinal; intros.
    exploit T.elements_remove; eauto. intros (l1 & l2 & P & Q).
    rewrite P, Q. rewrite ! app_length. simpl. omega.
  Qed.

  Theorem cardinal_set:
    forall x m y, T.get x m = None -> (cardinal m < cardinal (T.set x y m))%nat.
  Proof.
    intros. set (m' := T.set x y m).
    replace (cardinal m) with (cardinal (T.remove x m')).
    apply cardinal_remove with y. unfold m'; apply T.gss.
    unfold cardinal. f_equal. apply T.elements_extensional.
    intros. unfold m'. rewrite T.grspec, T.gsspec.
    destruct (T.elt_eq i x); auto. congruence.
  Qed.

  End MEASURE.

  (** Forall and exists *)

  Section FORALL_EXISTS.

  Variable A: Type.

  Definition for_all (m: T.t A) (f: T.elt -> A -> bool) : bool :=
    T.fold (fun b x a => b && f x a) m true.

  Lemma for_all_correct:
    forall m f,
    for_all m f = true <-> (forall x a, T.get x m = Some a -> f x a = true).
  Proof.
    intros m0 f.
    unfold for_all. apply fold_rec; intros.
  - (* Extensionality *)
    rewrite H0. split; intros. rewrite <- H in H2; auto. rewrite H in H2; auto.
  - (* Base case *)
    split; intros. rewrite T.gempty in H0; congruence. auto.
  - (* Inductive case *)
    split; intros.
    destruct (andb_prop _ _ H2). rewrite T.gsspec in H3. destruct (T.elt_eq x k).
    inv H3. auto.
    apply H1; auto.
    apply andb_true_intro. split.
    rewrite H1. intros. apply H2. rewrite T.gso; auto. congruence.
    apply H2. apply T.gss.
  Qed.

  Definition exists_ (m: T.t A) (f: T.elt -> A -> bool) : bool :=
    T.fold (fun b x a => b || f x a) m false.

  Lemma exists_correct:
    forall m f,
    exists_ m f = true <-> (exists x a, T.get x m = Some a /\ f x a = true).
  Proof.
    intros m0 f.
    unfold exists_. apply fold_rec; intros.
  - (* Extensionality *)
    rewrite H0. split; intros (x0 & a0 & P & Q); exists x0; exists a0; split; auto; congruence.
  - (* Base case *)
    split; intros. congruence. destruct H as (x & a & P & Q). rewrite T.gempty in P; congruence.
  - (* Inductive case *)
    split; intros.
    destruct (orb_true_elim _ _ H2).
    rewrite H1 in e. destruct e as (x1 & a1 & P & Q).
    exists x1; exists a1; split; auto. rewrite T.gso; auto. congruence.
    exists k; exists v; split; auto. apply T.gss.
    destruct H2 as (x1 & a1 & P & Q). apply orb_true_intro.
    rewrite T.gsspec in P. destruct (T.elt_eq x1 k).
    inv P. right; auto.
    left. apply H1. exists x1; exists a1; auto.
  Qed.

  Remark exists_for_all:
    forall m f,
    exists_ m f = negb (for_all m (fun x a => negb (f x a))).
  Proof.
    intros. unfold exists_, for_all. rewrite ! T.fold_spec.
    change false with (negb true). generalize (T.elements m) true.
    induction l; simpl; intros.
    auto.
    rewrite <- IHl. f_equal.
    destruct b; destruct (f (fst a) (snd a)); reflexivity.
  Qed.

  Remark for_all_exists:
    forall m f,
    for_all m f = negb (exists_ m (fun x a => negb (f x a))).
  Proof.
    intros. unfold exists_, for_all. rewrite ! T.fold_spec.
    change true with (negb false). generalize (T.elements m) false.
    induction l; simpl; intros.
    auto.
    rewrite <- IHl. f_equal.
    destruct b; destruct (f (fst a) (snd a)); reflexivity.
  Qed.

  Lemma for_all_false:
    forall m f,
    for_all m f = false <-> (exists x a, T.get x m = Some a /\ f x a = false).
  Proof.
    intros. rewrite for_all_exists.
    rewrite negb_false_iff. rewrite exists_correct.
    split; intros (x & a & P & Q); exists x; exists a; split; auto.
    rewrite negb_true_iff in Q. auto.
    rewrite Q; auto.
  Qed.

  Lemma exists_false:
    forall m f,
    exists_ m f = false <-> (forall x a, T.get x m = Some a -> f x a = false).
  Proof.
    intros. rewrite exists_for_all.
    rewrite negb_false_iff. rewrite for_all_correct.
    split; intros. apply H in H0. rewrite negb_true_iff in H0. auto. rewrite H; auto.
  Qed.

  End FORALL_EXISTS.

  (** More about [beq] *)

  Section BOOLEAN_EQUALITY.

  Variable A: Type.
  Variable beqA: A -> A -> bool.

  Theorem beq_false:
    forall m1 m2,
    T.beq beqA m1 m2 = false <->
    exists x, match T.get x m1, T.get x m2 with
              | None, None => False
              | Some a1, Some a2 => beqA a1 a2 = false
              | _, _ => True
              end.
  Proof.
    intros; split; intros.
  - (* beq = false -> existence *)
    set (p1 := fun x a1 => match T.get x m2 with None => false | Some a2 => beqA a1 a2 end).
    set (p2 := fun x a2 => match T.get x m1 with None => false | Some a1 => beqA a1 a2 end).
    destruct (for_all m1 p1) eqn:F1; [destruct (for_all m2 p2) eqn:F2 | idtac].
    + cut (T.beq beqA m1 m2 = true). congruence.
      rewrite for_all_correct in *. rewrite T.beq_correct; intros.
      destruct (T.get x m1) as [a1|] eqn:X1.
      generalize (F1 _ _ X1). unfold p1. destruct (T.get x m2); congruence.
      destruct (T.get x m2) as [a2|] eqn:X2; auto.
      generalize (F2 _ _ X2). unfold p2. rewrite X1. congruence.
    + rewrite for_all_false in F2. destruct F2 as (x & a & P & Q).
      exists x. rewrite P. unfold p2 in Q. destruct (T.get x m1); auto.
    + rewrite for_all_false in F1. destruct F1 as (x & a & P & Q).
      exists x. rewrite P. unfold p1 in Q. destruct (T.get x m2); auto.
  - (* existence -> beq = false *)
    destruct H as [x P].
    destruct (T.beq beqA m1 m2) eqn:E; auto.
    rewrite T.beq_correct in E.
    generalize (E x). destruct (T.get x m1); destruct (T.get x m2); tauto || congruence.
  Qed.

  End BOOLEAN_EQUALITY.

  (** Extensional equality between trees *)

  Section EXTENSIONAL_EQUALITY.

  Variable A: Type.
  Variable eqA: A -> A -> Prop.
  Hypothesis eqAeq: Equivalence eqA.

  Definition Equal (m1 m2: T.t A) : Prop :=
    forall x, match T.get x m1, T.get x m2 with
                  | None, None => True
                  | Some a1, Some a2 => a1 === a2
                  | _, _ => False
              end.

  Lemma Equal_refl: forall m, Equal m m.
  Proof.
    intros; red; intros. destruct (T.get x m); auto. reflexivity.
  Qed.

  Lemma Equal_sym: forall m1 m2, Equal m1 m2 -> Equal m2 m1.
  Proof.
    intros; red; intros. generalize (H x). destruct (T.get x m1); destruct (T.get x m2); auto. intros; symmetry; auto.
  Qed.

  Lemma Equal_trans: forall m1 m2 m3, Equal m1 m2 -> Equal m2 m3 -> Equal m1 m3.
  Proof.
    intros; red; intros. generalize (H x) (H0 x).
    destruct (T.get x m1); destruct (T.get x m2); try tauto;
    destruct (T.get x m3); try tauto.
    intros. transitivity a0; auto.
  Qed.

  Instance Equal_Equivalence : Equivalence Equal := {
    Equivalence_Reflexive := Equal_refl;
    Equivalence_Symmetric := Equal_sym;
    Equivalence_Transitive := Equal_trans
  }.

  Hypothesis eqAdec: EqDec A eqA.

  Program Definition Equal_dec (m1 m2: T.t A) : { m1 === m2 } + { m1 =/= m2 } :=
    match T.beq (fun a1 a2 => proj_sumbool (a1 == a2)) m1 m2 with
    | true => left _
    | false => right _
    end.
  Next Obligation.
    rename Heq_anonymous into B.
    symmetry in B. rewrite T.beq_correct in B.
    red; intros. generalize (B x).
    destruct (T.get x m1); destruct (T.get x m2); auto.
    intros. eapply proj_sumbool_true; eauto.
  Qed.
  Next Obligation.
    assert (T.beq (fun a1 a2 => proj_sumbool (a1 == a2)) m1 m2 = true).
    apply T.beq_correct; intros.
    generalize (H x).
    destruct (T.get x m1); destruct (T.get x m2); try tauto.
    intros. apply proj_sumbool_is_true; auto.
    unfold equiv, complement in H0. congruence.
  Qed.

  Instance Equal_EqDec : EqDec (T.t A) Equal := Equal_dec.

  End EXTENSIONAL_EQUALITY.

  (** Creating a tree from a list of (key, value) pairs. *)

  Section OF_LIST.

  Variable A: Type.

  Let f := fun (m: T.t A) (k_v: T.elt * A) => T.set (fst k_v) (snd k_v) m.

  Definition of_list (l: list (T.elt * A)) : T.t A :=
    List.fold_left f l (T.empty _).

  Lemma in_of_list:
    forall l k v, T.get k (of_list l) = Some v -> In (k, v) l.
  Proof.
    assert (REC: forall k v l m,
             T.get k (fold_left f l m) = Some v -> In (k, v) l \/ T.get k m = Some v).
    { induction l as [ | [k1 v1] l]; simpl; intros.
    - tauto.
    - apply IHl in H. unfold f in H. simpl in H. rewrite T.gsspec in H.
      destruct H; auto.
      destruct (T.elt_eq k k1). inv H. auto. auto.
    }
    intros. apply REC in H. rewrite T.gempty in H. intuition congruence.
  Qed.

  Lemma of_list_dom:
    forall l k, In k (map fst l) -> exists v, T.get k (of_list l) = Some v.
  Proof.
    assert (REC: forall k l m,
              In k (map fst l) \/ (exists v, T.get k m = Some v) ->
              exists v, T.get k (fold_left f l m) = Some v).
    { induction l as [ | [k1 v1] l]; simpl; intros.
    - tauto.
    - apply IHl. unfold f; rewrite T.gsspec. simpl. destruct (T.elt_eq k k1).
      right; econstructor; eauto.
      intuition congruence.
    }
    intros. apply REC. auto.
  Qed.

  Remark of_list_unchanged:
    forall k l m, ~In k (map fst l) -> T.get k (List.fold_left f l m) = T.get k m.
  Proof.
    induction l as [ | [k1 v1] l]; simpl; intros.
  - auto.
  - rewrite IHl by tauto. unfold f; apply T.gso; intuition auto.
  Qed.

  Lemma of_list_unique:
    forall k v l1 l2,
    ~In k (map fst l2) -> T.get k (of_list (l1 ++ (k, v) :: l2)) = Some v.
  Proof.
    intros. unfold of_list. rewrite fold_left_app. simpl.
    rewrite of_list_unchanged by auto. unfold f; apply T.gss.
  Qed.

  Lemma of_list_norepet:
    forall l k v, list_norepet (map fst l) -> In (k, v) l -> T.get k (of_list l) = Some v.
  Proof.
    assert (REC: forall k v l m,
              list_norepet (map fst l) ->
              In (k, v) l ->
              T.get k (fold_left f l m) = Some v).
    { induction l as [ | [k1 v1] l]; simpl; intros.
      contradiction.
      inv H. destruct H0.
      inv H. rewrite of_list_unchanged by auto. apply T.gss.
      apply IHl; auto.
    }
    intros; apply REC; auto.
  Qed.

  Lemma of_list_elements:
    forall m k, T.get k (of_list (T.elements m)) = T.get k m.
  Proof.
    intros. destruct (T.get k m) as [v|] eqn:M.
  - apply of_list_norepet. apply T.elements_keys_norepet. apply T.elements_correct; auto.
  - destruct (T.get k (of_list (T.elements m))) as [v|] eqn:M'; auto.
    apply in_of_list in M'. apply T.elements_complete in M'. congruence.
  Qed.

  End OF_LIST.

  Lemma of_list_related:
    forall (A B: Type) (R: A -> B -> Prop) k l1 l2,
    list_forall2 (fun ka kb => fst ka = fst kb /\ R (snd ka) (snd kb)) l1 l2 ->
    option_rel R (T.get k (of_list l1)) (T.get k (of_list l2)).
  Proof.
    intros until k. unfold of_list.
    set (R' := fun ka kb => fst ka = fst kb /\ R (snd ka) (snd kb)).
    set (fa := fun (m : T.t A) (k_v : T.elt * A) => T.set (fst k_v) (snd k_v) m).
    set (fb := fun (m : T.t B) (k_v : T.elt * B) => T.set (fst k_v) (snd k_v) m).
    assert (REC: forall l1 l2, list_forall2 R' l1 l2 ->
                 forall m1 m2, option_rel R (T.get k m1) (T.get k m2) ->
                 option_rel R (T.get k (fold_left fa l1 m1)) (T.get k (fold_left fb l2 m2))).
    { induction 1; intros; simpl.
    - auto.
    - apply IHlist_forall2. unfold fa, fb. rewrite ! T.gsspec.
      destruct H as [E F]. rewrite E. destruct (T.elt_eq k (fst b1)).
      constructor; auto.
      auto. }
    intros. apply REC; auto. rewrite ! T.gempty. constructor.
  Qed.


  (* folding a commutative function respects permutations. *)

  Require Import Permutation.

  (* norepet is from Compcert's Coqlib, NoDup is from the Coq standard lib, they mean the same thing. *)
  Lemma norepet_NoDup : forall A (l : list A), list_norepet l -> NoDup l.
  Proof.  
    induction 1; constructor; auto.
  Qed.

  Lemma NoDup_map : forall A B (f:A->B) (l : list A), NoDup (map f l) -> NoDup l.
  Proof.
    intros.
    remember (map f l) as l1.
    revert l Heql1.
    induction H.
    + destruct l; simpl in *; try congruence; constructor.
    + intros.
      destruct l0; simpl in *; try congruence; constructor;
        inversion Heql1; subst; auto using in_map.
  Qed.    

  Lemma elements_NoDup : forall A (m : T.t A), NoDup (T.elements m).
  Proof.
    intros.
    apply NoDup_map with (f:=fst).
    apply norepet_NoDup.
    apply T.elements_keys_norepet.
  Qed.

  Lemma set_permutation : forall {A} k (v:A) m,
      T.get k m = None ->
      Permutation (T.elements (T.set k v m)) ((k,v) :: T.elements m).
  Proof.
    intros.
    apply NoDup_Permutation. 
    - apply elements_NoDup.
    - constructor.
       intros HIn; apply T.elements_complete in HIn; congruence.
       apply elements_NoDup.
    - intros [k' v']. split.
      * destruct (T.elt_eq k' k).
        + subst. 
          intros HIn.
          apply T.elements_complete in HIn.
          rewrite T.gss in HIn.
          inversion HIn; subst.
          simpl; auto.
        + intros HIn.
          apply T.elements_complete in HIn.
          rewrite T.gso in HIn by auto.
          right.
          apply T.elements_correct.
          auto.
      * destruct 1.
        + inversion H0;  subst.
          apply T.elements_correct.
          rewrite T.gss. auto.
        + assert (k'<>k).
            { apply T.elements_complete in H0. intros ?. congruence. }
          apply T.elements_correct.
          apply T.elements_complete in H0.  
          rewrite T.gso; auto.
  Qed.

  Lemma get_permutation : forall {A} k (v:A) m,
      T.get k m = Some v ->
      exists lst, Permutation (T.elements m) (lst ++ (k,v)::nil).
  Proof.
    intros A k v m Hget.
    assert (Hin := T.elements_correct Hget).
    assert (Hnodup := T.elements_keys_norepet m).
    apply norepet_NoDup in Hnodup.
    destruct (in_split _ _ Hin) as [elements1 [ elements2  elements_eq]].
    exists (elements1 ++ elements2).
    rewrite elements_eq in *.
    clear Hin elements_eq.
    rewrite <- !app_assoc.
    apply Permutation_app_head.
    apply Permutation_cons_append.
  Qed.

  Lemma NoDup_snoc : forall {A} (x:A) l,
      NoDup (x::l) -> NoDup (l++x::nil).
  Proof.
    induction l.
    - constructor.
      simpl; intuition.
      constructor.
    - simpl.
      inversion 1.
      inversion H3.
      simpl in *.
      subst.
      constructor.
      + rewrite in_app.
        simpl in *.
        intuition.
      + apply IHl.        
        constructor;
          intuition.
  Qed.
      
  
  Section fold_permutation.
    Context  (A B K : Type)
             (f1 : A->B->A) 
             (f1_comm : forall (b1 b2 : B) a, f1 (f1 a b1) b2 = f1 (f1 a b2) b1)
             (f : A -> K -> B -> A)
             (f_comm : forall k1 b1 k2 b2 a,
                 k1 <> k2 -> 
                 f (f a k1 b1) k2 b2 = f (f a k2 b2) k1 b1).

    Lemma fold1_permutation : forall l l',
        Permutation l l'  -> forall init,
        List.fold_left f1 l init = List.fold_left f1 l' init.
    Proof.
      induction 1; intros.
      - reflexivity.
      - simpl. rewrite IHPermutation.
        reflexivity.
      - simpl. rewrite f1_comm. reflexivity.
      - rewrite IHPermutation1, IHPermutation2.
        reflexivity.
    Qed.

    Definition NoDup_keys (l : list (K*B)) := NoDup (map fst l).

    Lemma Permutation_NoDup_keys : forall l l',
        Permutation l l' -> NoDup_keys l -> NoDup_keys l'.
    Proof.
      unfold NoDup_keys.
      intros.
      apply Permutation_NoDup with (map fst l).
      - apply Permutation_map.
        assumption.
      - assumption.
    Qed.

    Lemma NoDup_keys_inv_cons : forall a l,
        NoDup_keys (a::l) ->
        NoDup_keys l.
    Proof.
      unfold NoDup_keys.
      simpl.
      intros.
      rewrite NoDup_cons_iff in H.
      tauto.
    Qed.

    Lemma fold_permutation : forall l l',
        Permutation l l' -> NoDup_keys l -> forall init,
            List.fold_left (fun a p => f a (fst p) (snd p)) l  init
          = List.fold_left (fun a p => f a (fst p) (snd p)) l' init.
    Proof.
      induction 1; intros.
      - reflexivity.
      - simpl. rewrite IHPermutation.
        + reflexivity.
        + apply NoDup_keys_inv_cons in H0.
          assumption.
      - simpl.
        rewrite f_comm.
        + reflexivity.
        + unfold NoDup_keys in H.
          simpl in H.
          rewrite !NoDup_cons_iff in H.
          simpl in H.
          intros H1.
          symmetry in H1.
          tauto.
      - rewrite IHPermutation1, IHPermutation2 by eauto using Permutation_NoDup_keys .
        reflexivity.
    Qed.
  End fold_permutation.

  Section fold1_set.
    Context  {A} {B} (f : A->B->A)
             (f_comm : forall (x y : B) a, f (f a x) y = f (f a y) x).

    Lemma f1_comm : forall (x y : T.elt * B) (a : A),
        (fun a0 p => f a0 (snd p))
          ((fun a0 p => f a0 (snd p)) a x) y =
        (fun a0 p => f a0 (snd p))
          ((fun a0 p => f a0 (snd p)) a y) x.
    intros [kx x] [ky y] init.
    simpl.  
    apply f_comm.
    Qed.

    Lemma fold1_extensional : forall init m m',
        (forall k,  T.get k m = T.get k m') ->
        T.fold1 f m init = T.fold1 f m' init.
    Proof.
      intros.
      repeat rewrite T.fold1_spec.
      refine (fold1_permutation _ _ _ _).
      + intros.
        apply f_comm.
      + apply NoDup_Permutation; try apply elements_NoDup.
        intros [k' v'].
        split; intros HIn;
        apply T.elements_complete in HIn;
        apply T.elements_correct;
        rewrite H in *;  
        auto.
    Qed.

    Lemma elements_set_decompose : forall {A} k (v:A) m,
      T.elements (T.set k v m) = T.elements (T.set k v (T.remove k m)).
    Proof.
      intros.
      apply  T.elements_extensional.
      intros i. destruct (T.elt_eq i k).
      + subst. repeat rewrite T.gss. reflexivity.
      + repeat rewrite T.gso by auto. rewrite T.gro by auto.
        reflexivity.
    Qed.

    Lemma fold1_set : forall init k v m,
      T.fold1 f (T.set k v m) init = T.fold1 f (T.remove k m) (f init v).
    Proof.
      intros.
      repeat rewrite T.fold1_spec.
      rewrite elements_set_decompose.
      assert (HPerm: Permutation (T.elements (T.set k v (T.remove k m)))
                                 ((k,v):: T.elements (T.remove k m)))
        by (apply set_permutation; apply T.grs).
      rewrite (fold1_permutation _ f1_comm HPerm).
      reflexivity.
    Qed.

    Lemma fold1_get : forall init k v m,
      T.get k m = Some v ->
      T.fold1 f m init = T.fold1 f (T.remove k m) (f init v).
    Proof.
      intros.
      replace (T.fold1 f m init)
        with  (T.fold1 f (T.set k v m) init).
        + apply fold1_set.
        + apply fold1_extensional.
          intros k0.
          destruct (T.elt_eq k0 k); subst.
           - rewrite T.gss; auto.
           - rewrite T.gso; auto.
    Qed.


    Lemma fold1_remove_set : forall init k v m,
        T.fold1 f (T.remove k (T.set k v m)) init = T.fold1 f (T.remove k m) init.
    Proof.
      intros.
      apply fold1_extensional.
      intros k0.
      repeat rewrite T.grspec.
      destruct (T.elt_eq k0 k); subst.
      + reflexivity.
      + rewrite T.gso; auto.
    Qed.
  End fold1_set.

  Section sum.
    Require Import ZArith.
    Definition sum (m : T.t Z) := T.fold1 Z.add m 0.

    Lemma plus_comm : forall x y a : Z, a + x + y = a + y + x.
    intros; omega.
    Qed.

    Theorem constant_sum : forall (m : T.t Z) k v k' v' i,
        T.get k  m = Some v ->
        T.get k' m = Some v' ->
        k <> k' ->
        sum (T.set k (v - i) (T.set k' (v' + i) m)) = sum m.
    Proof.
      intros m k v k' v' i Hget_k Hget_k' Hneq.
      unfold sum.
      rewrite fold1_get with (k0:=k') (v0:=(v'+i));
        [|apply plus_comm|rewrite T.gso by congruence; rewrite T.gss; reflexivity].
      rewrite fold1_get with (k0:=k) (v0:=(v - i));
        [|apply plus_comm|rewrite T.gro by congruence; rewrite T.gss; reflexivity].
      rewrite fold1_get with (k0:=k') (v0:=v') (m0:=m);
        [|apply plus_comm|congruence].
      rewrite fold1_get with (k0:=k) (v0:=v) (m0:=T.remove k' m);
        [|apply plus_comm|rewrite T.gro; congruence].
      replace (T.fold1 Z.add
                       (T.remove k (T.remove k' (T.set k (v - i) (T.set k' (v' + i) m))))
                       (0 + (v' + i) + (v - i)))
      with (T.fold1 Z.add
                   (T.remove k (T.remove k' m))
                   (0 + (v' + i) + (v - i))).
      + f_equal.
        omega.
      + apply fold1_extensional.
        - apply plus_comm.
        - intros k0.
          destruct (T.elt_eq k0 k); destruct (T.elt_eq k0 k'); subst;
          (repeat first [rewrite T.grs| rewrite T.gro by auto|rewrite T.gso by auto ]);
          reflexivity.
    Qed.

    Definition nonzero_elements (m : T.t Z) :=
      List.filter (fun e => negb (Z.eqb (snd e) 0)) (T.elements m).

    Lemma nonzero_elements_spec : forall m k v,
        In (k,v) (nonzero_elements m) <->
        (In (k,v) (T.elements m) /\ v<>0).
    Proof.
      unfold nonzero_elements.
      intros.
      rewrite filter_In.
      rewrite negb_true_iff.
      simpl.
      rewrite Z.eqb_neq.
      intuition.
    Qed.

    Lemma sum_fold_left_nonzero : forall m init,
      List.fold_left (fun a p => a + (snd p)) (nonzero_elements m) init =
      List.fold_left (fun a p => a + (snd p)) (T.elements m) init.
    Proof.
      intros m.
      unfold nonzero_elements.
      induction (T.elements m).
      - reflexivity.
      - simpl.
        destruct (snd a =? 0) eqn:?.
        + rewrite Z.eqb_eq in Heqb.
          simpl.
          intros init.
          replace (init + snd a) with init by omega.
          apply IHl.
        + intros init.
          simpl.
          apply IHl.
    Qed.

    Lemma NoDup_filter : forall A f (l:list A), NoDup l -> NoDup (filter f l).
    Proof.
      induction l.
      + simpl; constructor.
      + intros H.
        rewrite NoDup_cons_iff in H.
        destruct H.
        simpl.
        destruct (f a).
        * apply NoDup_cons.
          rewrite filter_In; tauto.
          auto.
        * auto.
    Qed.

    Lemma nonzero_elements_extensional:
      forall (m n: T.t Z),
      (forall i, T.get_default 0 i m = T.get_default 0 i n) ->
      Permutation (nonzero_elements m) (nonzero_elements n).
    Proof.
      intros.
      apply NoDup_Permutation.
      - apply NoDup_filter.
        apply elements_NoDup.
      - apply NoDup_filter.
        apply elements_NoDup.
      - intros [k v].
        rewrite ?nonzero_elements_spec.
        unfold T.get_default in H.
        intuition.
        + apply T.elements_complete in H1.
          apply T.elements_correct.
          specialize (H k).
          rewrite H1 in H.
          destruct (T.get k n) as [v'|]; [congruence|subst;tauto].
        + apply T.elements_complete in H1.
          apply T.elements_correct.
          specialize (H k).
          rewrite H1 in H.
          destruct (T.get k m) as [v'|]; [congruence|subst;tauto].
    Qed.

    Lemma sum_extensional:
      forall (m n: T.t Z) init,
        (forall i, T.get_default 0 i m = T.get_default 0 i n) ->
        T.fold1 Z.add m init = T.fold1 Z.add n init.
    Proof.
      intros.
      rewrite ?T.fold1_spec.
      rewrite <- ?sum_fold_left_nonzero.
      apply fold1_permutation.
      - intros; omega.
      - apply nonzero_elements_extensional.
        auto.
    Qed.

    Lemma  get_default_ss : forall A (def:A) k v m,
        T.get_default def k (T.set k v m) = v.
    Proof.
      intros.
      unfold T.get_default.
      rewrite T.gss.
      reflexivity.
    Qed.

    Lemma get_default_so : forall A (def:A) k k0 v m,
        k <> k0 ->
        T.get_default def k (T.set k0 v m) = T.get_default def k m.
    Proof.
      intros.
      unfold T.get_default.
      rewrite T.gso; auto.
    Qed.

    Lemma get_default_ro : forall A (def:A) k k0 m,
        k <> k0 ->
        T.get_default def k (T.remove k0 m) = T.get_default def k m.
    Proof.
      intros.
      unfold T.get_default.
      rewrite T.gro; auto.
    Qed.

    Lemma get_default_rs : forall A (def:A) k m,
        T.get_default def k (T.remove k m) = def.
    Proof.
      intros.
      unfold T.get_default.
      rewrite T.grs; auto.
    Qed.

    Lemma sum_get_default : forall init k v m,
      T.get_default 0 k m = v ->
      T.fold1 Z.add m init = T.fold1 Z.add (T.remove k m) (Z.add init v).
    Proof.
      intros.
      replace (T.fold1 Z.add m init)
        with  (T.fold1 Z.add (T.set k v m) init).
        + apply fold1_set.
          intros; omega.
        + apply sum_extensional.
          intros k0.
          destruct (T.elt_eq k0 k); subst.
           - rewrite get_default_ss; auto.
           - rewrite get_default_so; auto.
    Qed.

    (* A fancier version of the theorem constant_sum, which knows about 
       reading the default value zero. *)
    Theorem constant_sum' : forall (m : T.t Z) k v k' v' i,
        T.get_default 0 k  m = v ->
        T.get_default 0 k' m = v' ->
        k <> k' ->
        sum (T.set k (v - i) (T.set k' (v' + i) m)) = sum m.
    Proof.
      intros m k v k' v' i Hget_k Hget_k' Hneq.
      unfold sum.
      rewrite fold1_get with (k0:=k') (v0:=(v'+i));
        [|apply plus_comm|rewrite T.gso by congruence; rewrite T.gss; reflexivity].
      rewrite fold1_get with (k0:=k) (v0:=(v - i));
        [|apply plus_comm|rewrite T.gro by congruence; rewrite T.gss; reflexivity].

      rewrite sum_get_default with (k:=k') (v:=v') (m:=m); [|assumption].
      rewrite sum_get_default with (k:=k) (v:=v) (m:=T.remove k' m); [|rewrite get_default_ro; auto].
      replace (T.fold1 Z.add
                       (T.remove k (T.remove k' (T.set k (v - i) (T.set k' (v' + i) m))))
                       (0 + (v' + i) + (v - i)))
      with (T.fold1 Z.add
                   (T.remove k (T.remove k' m))
                   (0 + (v' + i) + (v - i))).
      + f_equal.
        omega.
      + apply fold1_extensional.
        - intros; omega.
        - intros k0.
          destruct (T.elt_eq k0 k); destruct (T.elt_eq k0 k'); subst;
          (repeat first [rewrite T.grs| rewrite T.gro by auto|rewrite T.gso by auto ]);
          reflexivity.
    Qed.

    Lemma sum_nonnegative' : forall (m : T.t Z) init,
        (forall k v, T.get k m = Some v -> v >= 0) ->
        T.fold1 Z.add m init >= init.
    Proof.
      intros.
      rewrite T.fold1_spec.
      assert (H' : forall k v, In (k,v) (T.elements m) -> v >= 0).
      {
        intros.
        apply T.elements_complete in H0.
        eauto.
      }
      clear H.
      revert init H'.
      induction (T.elements m); intros; simpl in *.
      - omega.
      - apply Zge_trans with (init + snd a).
        apply IHl.
        + intros; apply H' with k. auto.        
        + assert (snd a >= 0).
          { destruct a as [k v]. apply H' with k. left. reflexivity. }
          omega.
    Qed.
    
    Lemma sum_nonnegative : forall (m : T.t Z),
        (forall k v, T.get k m = Some v -> v >= 0) ->
        sum m >= 0.
    Proof.
      intros.
      unfold sum.
      apply sum_nonnegative'; auto; omega.
    Qed.

    Lemma sum_bound1 : forall (m : T.t Z) k1 B,
        (forall k v, T.get k m = Some v -> v >= 0) ->
        sum m <= B ->
        T.get_default 0 k1 m <= B.
    Proof.
      intros.
      assert (B >= 0).
      {
        pose (sum_nonnegative H).
        omega.
      }

      unfold sum in H0.
      rewrite (@sum_get_default _ k1 _ _ eq_refl) in H0.
      assert (forall (k : T.elt) (v : Z),
      T.get k (T.remove k1 m) = Some v -> v >= 0).
      {
        intros.
        destruct (T.elt_eq k k1); subst.
        - rewrite T.grs in H2; congruence.
        - rewrite !T.gro  in H2 by congruence.
          eapply H; eauto.
      }
      assert (H4 := @sum_nonnegative' (T.remove k1 m)
                                      (0 + T.get_default 0 k1 m)
                                      H2).
      omega.
    Qed.
    
    Lemma sum_bound2 : forall (m : T.t Z) k1 k2 B,
        k1<>k2 ->
        (forall k v, T.get k m = Some v -> v >= 0) ->
        sum m <= B ->
        T.get_default 0 k1 m + T.get_default 0 k2 m <= B.
    Proof.
      intros.
      assert (B >= 0).
      {
        pose (sum_nonnegative H0).
        omega.
      }

      unfold sum in H1.
      rewrite (@sum_get_default _ k1 _ _ eq_refl) in H1.
      rewrite (@sum_get_default _ k2 _ _ eq_refl) in H1.
      assert (forall (k : T.elt) (v : Z),
      T.get k (T.remove k2 (T.remove k1 m)) = Some v -> v >= 0).
      {
        intros.
        destruct (T.elt_eq k k2), (T.elt_eq k k1); subst.
        - rewrite T.grs in H3; congruence.
        - rewrite T.grs in H3. congruence.
        - rewrite !T.gro, T.grs in H3 by congruence. congruence.
        - rewrite !T.gro  in H3 by congruence.
          eapply H0; eauto.
      }
      assert (H4 := @sum_nonnegative' (T.remove k2 (T.remove k1 m))
                                      (0 + T.get_default 0 k1 m + T.get_default 0 k2 (T.remove k1 m))
                                      H3).
      remember ( T.fold1 Z.add (T.remove k2 (T.remove k1 m))
                         (0 + T.get_default 0 k1 m + T.get_default 0 k2 (T.remove k1 m))) as blah.
      clear Heqblah.
      rewrite get_default_ro in H4 by congruence.
      omega.
    Qed.
    
    Lemma sum_swap : forall (m : T.t Z) k v k' v',
        k <> k' ->
        sum (T.set k v (T.set k' v' m)) = 
        sum (T.set k' v' (T.set k v m)).
    Proof.
      intros.
      unfold sum.
      apply fold1_extensional.
      - intros; omega.
      - intros k0.
        destruct (T.elt_eq k0 k); destruct (T.elt_eq k0 k'); subst;
          try congruence;
            (repeat first [rewrite T.gss| rewrite T.grs| rewrite T.gro by auto|rewrite T.gso by auto ];
             auto).
    Qed.

  End sum.
End Tree_Properties.

Module PTree_Properties := Tree_Properties(PTree).
Module ZTree_Properties := Tree_Properties(ZTree).
Module Int256Tree_Properties := Tree_Properties(Int256Tree).

(* Ideally this would be a module functor, but I'm too lazy to figure out
   how to make it depend on both TREE and MAP at the same time,
   so I just prove it for Int256Map & Int256Tree for now. *)
Section MapOfTree.
   Context (A B : Type) (f : A -> B) (default : A).

   
   Definition map_of_tree (t : Int256Tree.t A) : Int256Map.t B :=
    Int256Tree.fold (fun m k v => Int256Map.set k (f v) m)
                    t
                    (Int256Map.init (f default)).

   Lemma mot_ins_commute :
     let f := (fun m k v => Int256Map.set k (f v) m)
     in
     forall k1 b1 k2 b2 a,
         k1 <> k2 -> 
         f (f a k1 b1) k2 b2 = f (f a k2 b2) k1 b1.
   Proof.
     intros.
     unfold f0.
     unfold Int256Map.set.
     unfold PMap.set.
     simpl.
     rewrite PTree.set_swap.
     - reflexivity.
     - intros C; apply Int256Indexed.index_inj in C.
       congruence.
   Qed.
       
   Lemma fold_left_last : forall A B (f:A->B->A) l x init,
     fold_left f (l ++ x :: nil) init = f (fold_left f l init) x.
   Proof.
     intros.
     rewrite fold_left_app.
     reflexivity.
   Qed.

   Require Import Permutation.
   
   Lemma map_of_tree_Some : forall i t v,
    Int256Tree.get i t = Some v ->
    Int256Map.get i (map_of_tree t) = f v.
   Proof.
     intros.
     unfold map_of_tree.
     rewrite Int256Tree.fold_spec.
     assert (Hin := Int256Tree.elements_correct _ _ H).
     assert (Hnodup := Int256Tree.elements_keys_norepet t).
     apply Int256Tree_Properties.norepet_NoDup in Hnodup.
     destruct (in_split _ _ Hin) as [elements1 [ elements2  elements_eq]].
     rewrite elements_eq in *.
     clear Hin elements_eq.
     rewrite (@Int256Tree_Properties.fold_permutation _ _ _
                (fun m k v => Int256Map.set k (f v) m)
                mot_ins_commute                                      
                (elements1 ++ (i, v) :: elements2)
                (elements1 ++ elements2 ++ (i, v) :: nil)).
     - rewrite !app_assoc.
       rewrite fold_left_last.
       simpl.
       rewrite Int256Map.gss.
       reflexivity.
     - apply Permutation_app_head.
       apply Permutation_cons_append.
     - exact Hnodup.
   Qed.

   Lemma map_of_tree_None' : forall i lst m,
       ~In i (map fst lst) ->
       Int256Map.get i m = f default ->
    Int256Map.get i
                  (fold_left
                     (fun a p  =>
                        Int256Map.set (fst p) (f (snd p)) a) lst
                     m)
    = f default.
   Proof.
     induction lst.
     - simpl.
       auto.
     - simpl.
       intros.
       apply IHlst.
       tauto.
       rewrite Int256Map.gso; auto; tauto.
   Qed.
   
   Lemma map_of_tree_None : forall i t,
    Int256Tree.get i t = None ->
    Int256Map.get i (map_of_tree t) = f default.
   Proof.
     intros.
     unfold map_of_tree.
     rewrite Int256Tree.fold_spec.
     rewrite map_of_tree_None'.
     - auto.
     - intros Hin.
       apply list_in_map_inv in Hin.
       destruct Hin as [[v ty] [v_eq Hin]].
       apply Int256Tree.elements_complete in Hin.
       simpl in v_eq; inversion v_eq; subst.
       congruence.
     - rewrite Int256Map.gi; auto.
   Qed.   

   Lemma map_of_tree_set : forall i t v,
       map_of_tree (Int256Tree.set i v t)
       = Int256Map.set i (f v) (map_of_tree t).
   Proof.
     (* I'm pretty sure this is true, but proving it seems annoying. *)
   Admitted.

End MapOfTree.


   
(** * Useful notations *)

Notation "a ! b" := (PTree.get b a) (at level 1).
Notation "a !! b" := (PMap.get b a) (at level 1).
