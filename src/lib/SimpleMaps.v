
(* Compcert comes with a TREE interface for finite maps, but the Compcert
   implementation of it is in terms of balanced trees, which generally does not 
   reduce well at typechecking-time. 

   Here we provide a simple (and slow) implementation of the same
   interface in terms just functions. *)

Require Import Equivalence EquivDec.
Require Import Coqlib.
Require Import Maps.
Require Import Coq.Logic.FunctionalExtensionality.

(* To avoid useless definitions of inductors in extracted code. *)
Local Unset Elimination Schemes.
Local Unset Case Analysis Schemes.

Set Implicit Arguments.


(* This is a reduced version of Compcert's TREE interface, omitting 
   things that can't be supported by the simple association-list implementation below. *)
Module Type ALIST.
  Variable elt: Type.
  Variable elt_eq: forall (a b: elt), {a = b} + {a <> b}.
  Variable t: Type -> Type.
  Variable empty: forall (A: Type), t A.
  Variable get: forall (A: Type), elt -> t A -> option A.
  Variable set: forall (A: Type), elt -> A -> t A -> t A.

  Hypothesis gempty:
    forall (A: Type) (i: elt), get i (empty A) = None.
  Hypothesis gss:
    forall (A: Type) (i: elt) (x: A) (m: t A), get i (set i x m) = Some x.
  Hypothesis gso:
    forall (A: Type) (i j: elt) (x: A) (m: t A),
    i <> j -> get i (set j x m) = get i m.
  Hypothesis gsspec:
    forall (A: Type) (i j: elt) (x: A) (m: t A),
      get i (set j x m) = if elt_eq i j then Some x else get i m.
  (*
  Hypothesis gsident:
    forall (A: Type) (i: elt) (m: t A) (v: A),
    get i m = Some v -> set i v m = m.
  *)

  (** Applying a function to all data of a tree. *)
  Variable map:
    forall (A B: Type), (elt -> A -> B) -> t A -> t B.
  Hypothesis gmap:
    forall (A B: Type) (f: elt -> A -> B) (i: elt) (m: t A),
    get i (map f m) = option_map (f i) (get i m).

  (** Same as [map], but the function does not receive the [elt] argument. *)
  Variable map1:
    forall (A B: Type), (A -> B) -> t A -> t B.
  Hypothesis gmap1:
    forall (A B: Type) (f: A -> B) (i: elt) (m: t A),
    get i (map1 f m) = option_map f (get i m).


  (** Enumerating the bindings of a tree. *)
  Variable elements:
    forall (A: Type), t A -> list (elt * A).
  Hypothesis elements_correct:
    forall (A: Type) (m: t A) (i: elt) (v: A),
      get i m = Some v -> In (i, v) (elements m).
  (*
  Hypothesis elements_complete:
    forall (A: Type) (m: t A) (i: elt) (v: A),
    In (i, v) (elements m) -> get i m = Some v.
  Hypothesis elements_keys_norepet:
    forall (A: Type) (m: t A), 
    list_norepet (List.map (@fst elt A) (elements m)).
  *)
End ALIST.

Open Scope positive.


(* Maps as association lists. 
   This does not quite fit the interface, because we don't have that extensional 
   equality implies exact equality, but we don't need that for the DeepSpec development. *)
Module AList <: ALIST.
  Definition elt := positive.
  Definition elt_eq := peq.

  Definition t (A:Type) := list (positive * A).

  Definition empty {A : Type} : t A := nil.

  
  Fixpoint get {A : Type} (i : positive) (m : t A) : option A :=
    match m with
      | (j, a) :: m' => if (i =? j) then Some a else get i m'
      | nil => None
    end.
                 
  Definition set (A : Type) (i : positive) (v : A) (m : t A)  : t A :=
    (i,v) :: m.


  (* We don't need remove.
  Definition remove (A : Type) (i : positive) (m : t A) {struct i} : t A :=
  *)

  Theorem gempty:
    forall (A: Type) (i: positive), get i empty = (@None A).
  Proof.
    reflexivity.
  Qed.

  Theorem gss:
    forall (A: Type) (i: positive) (x: A) (m: t A), get i (set i x m) = Some x.
  Proof.
    intros.
    unfold get,set.
    simpl.
    rewrite Pos.eqb_refl.
    reflexivity.
  Qed.

  Theorem gso:
    forall (A: Type) (i j: positive) (x: A) (m: t A),
    i <> j -> get i (set j x m) = get i m.
  Proof.
    unfold get, set.
    intros.
    simpl.
    replace (i =? j) with false.
      reflexivity.
      symmetry; rewrite Pos.eqb_neq; assumption.
  Qed.

  Theorem gsspec:
    forall (A: Type) (i j: positive) (x: A) (m: t A),
    get i (set j x m) = if peq i j then Some x else get i m.
  Proof.
    intros.
    destruct (peq i j);
      [ rewrite e; apply gss | apply gso; auto ].
  Qed.


  (* not true with this representation. *)
  (*
  Theorem gsident:
    forall (A: Type) (i: positive) (m: t A) (v: A),
    get i m = Some v -> set i v m = m.
  Proof.    
    induction i; intros; destruct m; simpl; simpl in H; try congruence.
     rewrite (IHi m2 v H); congruence.
     rewrite (IHi m1 v H); congruence.
  Qed.
  *)

  (* This is used at one point in the development, in SynthesisStmt.v.
     However, that part deals directly with compcert operational semantics, 
     so we will have had to insert some kind of conversion to PTrees in any case. *)
  (*  
  Theorem set2:
    forall (A: Type) (i: elt) (m: t A) (v1 v2: A),
    set i v2 (set i v1 m) = set i v2 m. *)

  (* We don't use remove. *)
  (*
  Theorem grs:
    forall (A: Type) (i: positive) (m: t A), get i (remove i m) = None.
  Theorem gro:
    forall (A: Type) (i j: positive) (m: t A),
    i <> j -> get i (remove j m) = get i m.
  Theorem grspec:
    forall (A: Type) (i j: elt) (m: t A),
    get i (remove j m) = if elt_eq i j then None else get i m.
   *)
  
  (* We could prove these, but they are not used in the development. *)
  (*
  Section BOOLEAN_EQUALITY.
    Variable A: Type.
    Variable beqA: A -> A -> bool.

    Fixpoint bempty (m: t A) : bool := 

    Fixpoint beq (m1 m2: t A) {struct m1} : bool :=

    Lemma bempty_correct:
      forall m, bempty m = true <-> (forall x, get x m = None).

    Lemma beq_correct:
      forall m1 m2,
      beq m1 m2 = true <->
      (forall (x: elt),
       match get x m1, get x m2 with
       | None, None => True
       | Some y1, Some y2 => beqA y1 y2 = true
       | _, _ => False
       end).
  End BOOLEAN_EQUALITY.
   *)


  
  Definition map (A B : Type) (f : positive -> A -> B) m := List.map (fun p => (fst p, f (fst p) (snd p))) m.

  Theorem gmap:
    forall (A B: Type) (f: positive -> A -> B) (i: positive) (m: t A),
    get i (map f m) = option_map (f i) (get i m).
  Proof.
    induction m.
    + reflexivity.
    + destruct a as [j a].
      unfold get.
      simpl.
      destruct (i =? j) eqn:Heq.
      * rewrite Peqb_eq in Heq.
        subst.
        reflexivity.
      * unfold get in IHm.
        rewrite IHm.
        reflexivity.
  Qed.

  Definition map1 (A B: Type) (f: A -> B) (m: t A) : t B :=
    List.map (fun p => (fst p, f (snd p))) m.

  Theorem gmap1:
    forall (A B: Type) (f: A -> B) (i: elt) (m: t A),
    get i (map1 f m) = option_map f (get i m).
  Proof.
    induction m.
    + reflexivity.
    + destruct a as [j a]. 
      simpl.
      unfold get.
      simpl.
      destruct (i =? j)%positive eqn:Heq.
      * rewrite Peqb_eq in Heq.
        subst.
        reflexivity.
      * unfold get in IHm.
        rewrite IHm.
        reflexivity.
  Qed.

  (* not used. 
  Fixpoint filter1 (A: Type) (pred: A -> bool) (m: t A) {struct m} : t A := 

  Theorem gfilter1:
    forall (A: Type) (pred: A -> bool) (i: elt) (m: t A),
    get i (filter1 pred m) =
    match get i m with None => None | Some x => if pred x then Some x else None end.
  Proof.
   *)
  
  Definition elements (A: Type) (m: t A) := m.
  
  Theorem elements_correct:
    forall (A: Type) (m: t A) (i: positive) (v: A),
    get i m = Some v -> In (i, v) (elements m).
  Proof.
    induction m.
    + compute.
      intros; congruence.
    + destruct a as [j a]. 
      simpl.
      unfold get.
      simpl.
      intros.
      destruct (i =? j)%positive eqn:Heq.
      * rewrite Peqb_eq in Heq.
        subst.
        left.
        congruence.
      * unfold get in IHm.
        specialize IHm with i v.
        rewrite H in IHm.
        right; auto.
  Qed.

  (* This *was* used, a lot, but it's not true using this representation. *)
  (*
  Theorem elements_complete:
    forall (A: Type) (m: t A) (i: positive) (v: A),
    In (i, v) (elements m) -> get i m = Some v.
  Proof.
  *)

  (* Happily, the following is good enough for our purposes. *)
    Theorem elements_domain_complete:
    forall (A: Type) (m: t A) (i: positive),
      In i (List.map fst (elements m)) -> exists v, get i m = Some v.
    Proof.
      intros A m i H.
      induction m.
      - destruct H.
      - idtac.
        destruct a as [j u].
        destruct (Pos.eq_dec j i)%positive as [Heq | Hneq].
        + exists u.
          rewrite Heq.
          unfold AList.get.
          rewrite Pos.eqb_refl.
          reflexivity.
       + idtac.
          change ((j,u)::m) with (AList.set j u m).            
          rewrite AList.gso by auto.
          apply IHm.
          destruct H.
          * simpl in H.
            tauto.
          * exact H.
    Qed.


  Lemma elements_set_monotone {A} x i a (m : AList.t A) :
      In x (List.map fst (elements m)) ->
      In x (List.map fst (elements (set i a m))).
  Proof.
    destruct (peq x i) as [ i_eq | i_ne ]; subst.
    - unfold AList.set, AList.elements.
      simpl.
      auto.
    - unfold AList.set, AList.elements.
      simpl; right; auto. 
  Qed.
    
  (* Ditto. *)
  (*
  Theorem elements_keys_norepet:
    forall (A: Type) (m: t A), 
      list_norepet (List.map (@fst elt A) (elements m)).

  Theorem elements_canonical_order:
    forall (A B: Type) (R: A -> B -> Prop) (m: t A) (n: t B),
    (forall i x, get i m = Some x -> exists y, get i n = Some y /\ R x y) ->
    (forall i y, get i n = Some y -> exists x, get i m = Some x /\ R x y) ->
    list_forall2
      (fun i_x i_y => fst i_x = fst i_y /\ R (snd i_x) (snd i_y))
      (elements m) (elements n).
  *)

  (* Not true, not used. *)
  (*
  Theorem elements_extensional:
    forall (A: Type) (m n: t A),
    (forall i, get i m = get i n) ->
    elements m = elements n.
   *)

  (*
  Definition fold (A B : Type) (f: B -> positive -> A -> B) (m: t A) (v: B) :=
    xfold f xH m v.

  Theorem fold_spec:
    forall (A B: Type) (f: B -> positive -> A -> B) (v: B) (m: t A),
    fold f m v =
    List.fold_left (fun a p => f a (fst p) (snd p)) (elements m) v.
  *)

  (*
  Fixpoint fold1 (A B: Type) (f: B -> A -> B) (m: t A) (v: B) {struct m} : B := 

  Theorem fold1_spec:
    forall (A B: Type) (f: B -> A -> B) (v: B) (m: t A),
    fold1 f m v =
    List.fold_left (fun a p => f a (snd p)) (elements m) v.
  Proof.
  *)

End AList.


Definition dummy : nat := (42%nat).

Fixpoint PTree_of_AList {A:Type} (m: AList.t A)  : PTree.t A :=
  match m with
    | nil => PTree.empty A
    | (i,a)::m => PTree.set i a (PTree_of_AList m)
  end.


Close Scope positive.

Notation "a ! b" := (AList.get b a) (at level 1) : alist_scope.
Delimit Scope alist_scope with alist.
