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

Require Export Coq.Program.Equality.

Require Import Coqlib.
Require Import Maps.

Record Isomorphism {A} (X Y : A) : Type := {
  iso_f : forall F : A -> Type, F X -> F Y;
  iso_g : forall F : A -> Type, F Y -> F X
     (* := fun F => iso_f (fun x => F x -> F X) id *)
}.

Arguments iso_f {_ _ _} _ [_] _.
Arguments iso_g {_ _ _} _ [_] _.
Coercion iso_f : Isomorphism >-> Funclass.

Notation "X ~~~ Y" := (Isomorphism X Y) (at level 70, no associativity).

Definition identity_automorphism {A X} : @Isomorphism A X X
  := {| iso_f F fx := fx; iso_g f fx := fx |}.

Definition iso_concat {A}{X Y Z : A}(iso_xy : X ~~~ Y)(iso_yz : Y ~~~ Z) :=
  {| iso_f F fx := iso_yz F (iso_xy F fx);
     iso_g F fy := iso_g iso_xy (iso_g iso_yz fy) |}.

Definition iso_inverse {A}{X Y : A}(iso : X ~~~ Y) :=
  {| iso_f := iso_g iso; iso_g := iso_f iso |}.

Notation "p @ q" := (iso_concat p q) (at level 40, left associativity).
Notation "! p" := (iso_inverse p) (at level 35, right associativity).

Definition isomorphism_compose {A}{X Y Z : A}(iso_xy : X ~~~ Y)
  (iso_yz : Y ~~~ Z) :
    forall F fx, iso_yz F (iso_xy F fx) = (iso_xy @ iso_yz) F fx.
Proof.
  intros F fx; reflexivity.
Qed.

Lemma isomorphism_decidable_neq_absurd {A}{X Y : A}(dec : A -> bool) :
  X ~~~ Y -> dec X = true -> dec Y = false -> False.
Proof.
  intros iso dec_x dec_y.
  set (F := fun Z : A => match dec Z with
                         | true => True
                         | false => False
                         end).
  assert (fx : F X).
  { unfold F; rewrite dec_x; exact I. }

  assert (fy : F Y).
  { apply iso, fx. }

  unfold F in fy.
  rewrite dec_y in fy.
  exact fy.
Qed.

(** Defining the isomorphism using Leibniz equality means that we have to
    rely on some properties of functors.  One of them, especially, is based
    on paramatricity, which can't be proved inside Coq, and is axiomatized
    as [functor_identity_paramatricity].

    Hopefully it is consistent.  It implies eq_rect_eq, which is already
    implied by proof irrelevance required by CompCert.
*)
Axiom functor_identity_paramatricity :
  forall (A : Type)(X : A)(f : forall F, F X -> F X)
    (F : A -> Type)(x : F X), x = f F x.

Module AxiomImplication.
  Lemma eq_rect_eq :
      forall (U : Type) (p : U) (Q : U -> Type) (x : Q p) (h : p = p),
      x = eq_rect p Q x p h.
  Proof.
    intros.
    exact (functor_identity_paramatricity U p
            (fun Q x => eq_rect p Q x p h) Q x).
  Qed.
End AxiomImplication.

Lemma isomorphism_identity_gf' {A}{X Y : A}(iso1 iso2 : X ~~~ Y)(F : A -> Type)
    (x : F X) : x = iso_g iso1 (iso_f iso2 x).
Proof.
  set (iso_comp := iso2 @ !iso1).
  change (iso_g iso1 (iso_f iso2 x)) with (iso_f iso_comp x).
  apply functor_identity_paramatricity.
Qed.

Lemma isomorphism_identity_fg' {A}{X Y : A}(iso1 iso2 : X ~~~ Y)(F : A -> Type)
    (y : F Y) : y = iso_f iso1 (iso_g iso2 y).
Proof.
  set (iso_comp := !iso1 @ iso2).
  change (iso_f iso1 (iso_g iso2 y)) with (iso_g iso_comp y).
  apply functor_identity_paramatricity.
Qed.

Lemma isomorphism_identity_gf {A}{X Y : A}(iso : X ~~~ Y)(F : A -> Type)
    (x : F X) : x = iso_g iso (iso_f iso x).
Proof (isomorphism_identity_gf' iso iso F x).

Lemma isomorphism_identity_fg {A}{X Y : A}(iso : X ~~~ Y)(F : A -> Type)
    (y : F Y) : y = iso_f iso (iso_g iso y).
Proof (isomorphism_identity_fg' iso iso F y).

Lemma isomorphism_unique {A}{X Y : A}(iso1 iso2 : X ~~~ Y)(F : A -> Type)
    (x : F X) : iso_f iso1 x = iso_f iso2 x.
Proof.
  transitivity (iso_f iso2 (iso_g iso2 (iso_f iso1 x))).
    apply isomorphism_identity_fg.
  f_equal; symmetry.
  apply isomorphism_identity_gf'.
Qed.

Lemma isomorphism_injective {A}{X Y : A}(iso : X ~~~ Y) :
    forall F x1 x2, iso F x1 = iso F x2 -> x1 = x2.
Proof.
  intros F x1 x2 eq.
  rewrite (isomorphism_identity_gf iso F x1).
  rewrite eq.
  symmetry; apply isomorphism_identity_gf.
Qed.

(* To avoid useless definitions of inductors in extracted code. *)
Local Unset Elimination Schemes.
Local Unset Case Analysis Schemes.

(** * An implementation of indexed trees over type [positive]
    Element types are indexed by a tree [PTree.t A] along with a projection
    [proj] that extract the actual [Type] from [A].  *)
Module Type TypeProjection.
  Variable A : Type.
  Variable proj : A -> Type.
End TypeProjection.

Module IPTree(TP : TypeProjection).
  Definition elt := positive.
  Definition elt_eq := peq.

  Definition OptionalType (a : option TP.A) : Type :=
    match a with
    | None => unit
    | Some a' => TP.proj a'
    end.

  Inductive tree : PTree.t TP.A -> Type :=
    | Leaf : tree PTree.Leaf
    | Node : forall l r o, tree l -> OptionalType o -> tree r ->
                           tree (PTree.Node l o r).

  Arguments Node [l r].
  Scheme tree_ind := Induction for tree Sort Prop.

  Definition get_index {idx}(_ : tree idx) := idx.

  Definition t := tree.

  Definition empty : t (PTree.empty TP.A) := Leaf.

  Fixpoint get {idx}(i : positive)(m : t idx){struct i} :
      OptionalType (PTree.get i idx).
  Proof.
    destruct idx.
    - destruct i; simpl; exact tt.
    - set (tuple := match m in (tree t')
                    return match t' with
                           | PTree.Leaf => t PTree.Leaf * unit * t PTree.Leaf
                           | PTree.Node l o r => t l * OptionalType o * t r
                           end with
                    | Leaf => ((Leaf, tt), Leaf)
                    | Node _ _ _ m1 o0 m2 => ((m1, o0), m2)
                    end).
      destruct i; simpl; try apply get; apply tuple.
  Defined.

  Definition get_eq {idx}(i : positive){OA}(m : t idx) :
      PTree.get i idx ~~~ OA -> OptionalType OA
    := fun iso => iso OptionalType (get i m).

  Fixpoint set {idx}(i : positive)(A : TP.A)(v : TP.proj A)(m : t idx){struct i} :
      t (PTree.set i A idx).
  Proof.
    destruct idx.
    - destruct i; simpl; apply Node; try apply (set _ i);
        solve [ exact Leaf | exact tt | exact v ].
    - set (tuple := match m in (tree t')
                    return match t' with
                           | PTree.Leaf => t PTree.Leaf * unit * t PTree.Leaf
                           | PTree.Node l o r => t l * OptionalType o * t r
                           end with
                     | Leaf => ((Leaf, tt), Leaf)
                     | Node _ _ _ m1 o0 m2 => ((m1, o0), m2)
                     end).
      destruct i; simpl; apply Node; try apply (set _ i);
        solve [ apply tuple | exact v ].
  Defined.

  Fixpoint remove {idx}(i : positive)(m : t idx){struct i} :
      t (PTree.remove i idx).
  Proof.
    (* Comments in between tactics roughly match cases of PTree.remove *)

    destruct i; destruct idx.
    (* match i, m with *)

    (* | xI i, Leaf *)
    - exact Leaf.
    - set (tuple := match m in (tree t')
                    return match t' with
                           | PTree.Leaf => t PTree.Leaf * unit * t PTree.Leaf
                           | PTree.Node l o r => t l * OptionalType o * t r
                           end with
                     | Leaf => (Leaf, tt, Leaf)
                     | Node _ _ _ m1 o0 m2 => (m1, o0, m2)
                     end).
      set (m1 := fst (fst tuple)). set (o0 := snd (fst tuple)).
      set (m2 := snd tuple).
      simpl; destruct idx1. destruct o.
    (* | xI i, Node (Some a) Leaf o0 m2 *)
      + exact (Node _ m1 o0 (remove _ i m2)).
    (* | xI i, Node None     Leaf _  m2 *)
      + set (rm_i_r := PTree.remove i idx2).
        (* match PTree.remove i r with *)
        refine (match rm_i_r as t'
                return (PTree.remove i idx2 ~~~ t' ->
                        t match t' with
                          | PTree.Leaf => PTree.Leaf
                          | PTree.Node l o r => PTree.Node PTree.Leaf None
                                                           (PTree.Node l o r)
                          end) with
        (* | PTree.Leaf *)
                | PTree.Leaf => fun _ => Leaf
                | PTree.Node l' o' r' => fun rm_iso => _ (* !!! *)
                end identity_automorphism).
        (* | mm *)
        refine (Node None Leaf tt _).
        apply rm_iso.
        exact (remove _ i m2).
        (* end *)
    (* | xI i, Node o m1 o0 m2 *)
      + exact (Node _ m1 o0 (remove _ i m2)).

    (* | xO i, Leaf *)
    - exact Leaf.
    - set (tuple := match m in (tree t')
                    return match t' with
                           | PTree.Leaf => t PTree.Leaf * unit * t PTree.Leaf
                           | PTree.Node l o r => t l * OptionalType o * t r
                           end with
                     | Leaf => (Leaf, tt, Leaf)
                     | Node _ _ _ m1 o0 m2 => (m1, o0, m2)
                     end).
      set (m1 := fst (fst tuple)). set (o0 := snd (fst tuple)).
      set (m2 := snd tuple).
      simpl; destruct o. 2: destruct idx2.
    (* | xO i, Node (Some a) m1 o0 m2 *)
      + exact (Node _ (remove _ i m1) o0 m2).
    (* | xO i, Node None     m1 _  m2 *)
      + set (rm_i_l := PTree.remove i idx1).
        (* match PTree.remove i l with *)
        refine (match rm_i_l as t'
                return (PTree.remove i idx1 ~~~ t' ->
                        t match t' with
                          | PTree.Leaf => PTree.Leaf
                          | PTree.Node l o r => PTree.Node (PTree.Node l o r)
                                                           None PTree.Leaf
                          end) with
        (* | PTree.Leaf *)
                | PTree.Leaf => fun _ => Leaf
                | PTree.Node l' o' r' => fun rm_iso => _ (* !!! *)
                end identity_automorphism).
        (* | mm *)
        refine (Node None _ tt m2).
        apply rm_iso.
        exact (remove _ i m1).
        (* end *)
    (* | xO i, Node None m1 o0 m2 *)
      + exact (Node None (remove _ i m1) tt m2).

    (* | xH, Leaf *)
    - exact Leaf.
    - set (tuple := match m in (tree t')
                    return match t' with
                           | PTree.Leaf => t PTree.Leaf * unit * t PTree.Leaf
                           | PTree.Node l o r => t l * OptionalType o * t r
                           end with
                     | Leaf => (Leaf, tt, Leaf)
                     | Node _ _ _ m1 o0 m2 => (m1, o0, m2)
                     end).
      set (m1 := fst (fst tuple)). set (o0 := snd (fst tuple)).
      set (m2 := snd tuple).
      simpl; destruct idx1. destruct idx2.
    (* | xH, Node o Leaf o0 Leaf *)
      + exact Leaf.
    (* | xH, Node o Leaf o0 m2 *)
      + exact (Node None Leaf tt m2).
    (* | xH, Node o m1   o0 m2 *)
      + exact (Node None m1   tt m2).
    (* end *)
  Defined.

  Fixpoint remove_eq {idx}(i : positive) A (m : t (PTree.set i A idx)){struct i} :
      PTree.get i idx ~~~ None -> t idx.
  Proof.
    destruct i; simpl in m; unfold PTree.get; try fold (@PTree.get TP.A i);
      destruct idx; try (intros _; exact Leaf).

    (* xI i *)
    - intros idx2_i_iso_none.
      set (tuple := match m in (tree t')
                    return match t' with
                           | PTree.Leaf => t PTree.Leaf * unit * t PTree.Leaf
                           | PTree.Node l o r => t l * OptionalType o * t r
                           end with
                     | Leaf => (Leaf, tt, Leaf)
                     | Node _ _ _ m1 o0 m2 => (m1, o0, m2)
                     end).
      set (m1 := fst (fst tuple)). set (o0 := snd (fst tuple)).
      set (m2 := snd tuple).
      refine (Node _ m1 o0 _).
      apply remove_eq with i A; assumption.
    (* xO i *)
    - intros idx1_i_iso_none.
      set (tuple := match m in (tree t')
                    return match t' with
                           | PTree.Leaf => t PTree.Leaf * unit * t PTree.Leaf
                           | PTree.Node l o r => t l * OptionalType o * t r
                           end with
                     | Leaf => (Leaf, tt, Leaf)
                     | Node _ _ _ m1 o0 m2 => (m1, o0, m2)
                     end).
      set (m1 := fst (fst tuple)). set (o0 := snd (fst tuple)).
      set (m2 := snd tuple).
      refine (Node _ _ o0 m2).
      apply remove_eq with i A; assumption.
    (* xH *)
    - intros o_iso_none.
      set (tuple := match m in (tree t')
                    return match t' with
                           | PTree.Leaf => t PTree.Leaf * unit * t PTree.Leaf
                           | PTree.Node l o r => t l * OptionalType o * t r
                           end with
                     | Leaf => (Leaf, tt, Leaf)
                     | Node _ _ _ m1 o0 m2 => (m1, o0, m2)
                     end).
      set (m1 := fst (fst tuple)). set (m2 := snd tuple).
      refine (Node _ m1 _ m2).
      apply o_iso_none, tt.
  Defined.
  Global Arguments remove_eq {_} _ {_} _ _.

  Theorem gempty (i : positive)(iso : PTree.Leaf ! i ~~~ None) :
      iso OptionalType (get i empty) = tt.
  Proof.
    induction i; symmetry;
      apply (functor_identity_paramatricity _ _ (iso_f iso) OptionalType).
  Qed.

  Lemma gleaf (i : positive)(iso : PTree.Leaf ! i ~~~ None) :
      iso OptionalType (get i Leaf) = tt.
  Proof (gempty i iso).

  Theorem get_eq_fusion {idx OA OA'}(i : positive)(m : t idx)
    (iso : PTree.get i idx ~~~ OA)(iso' : OA ~~~ OA') :
      iso' OptionalType (get_eq i m iso) = get_eq i m (iso @ iso').
  Proof.
    unfold get_eq.
    rewrite isomorphism_compose.
    reflexivity.
  Qed.

  Theorem gss {idx A}(i : positive)(x : TP.proj A)(m : t idx)
    (iso : (PTree.set i A idx) ! i ~~~ Some A) :
      get i (set i A x m) = iso_g iso (F := OptionalType) x.
  Proof.
    generalize dependent idx; induction i.
    - destruct m; simpl; auto.
    - destruct m; simpl; auto.
    - destruct m; simpl; intro iso;
        apply (functor_identity_paramatricity _ _ (iso_g iso) OptionalType).
  Qed.

  Theorem gess' {idx A OA}(i : positive)(x : TP.proj A)(m : t idx)
    iso (iso' : Some A ~~~ OA) :
      get_eq i (OA := OA) (set i A x m) iso = iso' OptionalType x.
  Proof.
    unfold get_eq.
    set (iso_comp := iso @ !iso').
    rewrite (gss _ _ _ iso_comp).
    symmetry.
    apply (isomorphism_identity_fg iso _ (iso' OptionalType x)).
  Qed.

  Theorem gess {idx A}(i : positive)(x : TP.proj A)(m : t idx) iso :
      get_eq i (OA := Some A) (set i A x m) iso = x.
  Proof (gess' i x m iso identity_automorphism).

  Definition gleaf_iso {i} : (@PTree.Leaf TP.A) ! i ~~~ None.
  Proof. destruct i; exact identity_automorphism. Defined.

  Theorem gso {idx A}(i j : positive)(x : TP.proj A)(m : t idx)
      iso : i <> j -> get i (set j A x m) = iso_f iso (get i m).
  Proof.
    generalize dependent idx; generalize dependent j.
    induction i.
    - intros j idx m iso neq.
      destruct idx.
      + destruct j; simpl in * |- *.
        * rewrite <- (gleaf i gleaf_iso).
          set (iso_comp := (@gleaf_iso i) @ iso).
          change (iso_f iso (iso_f gleaf_iso (get i Leaf))) with
                 (iso_f iso_comp (get i Leaf)).
          apply IHi; congruence.
        * destruct i; apply functor_identity_paramatricity.
        * destruct i; apply functor_identity_paramatricity.
      + destruct j; simpl in * |- *.
        * apply IHi; congruence.
        * apply functor_identity_paramatricity.
        * apply functor_identity_paramatricity.
    - intros j idx m iso neq.
      destruct m.
      + destruct j; simpl in * |- *.
        * destruct i; apply functor_identity_paramatricity.
        * rewrite <- (gleaf i gleaf_iso).
          set (iso_comp := (@gleaf_iso i) @ iso).
          change (iso_f iso (iso_f gleaf_iso (get i Leaf))) with
                 (iso_f iso_comp (get i Leaf)).
          apply IHi; congruence.
        * destruct i; apply functor_identity_paramatricity.
      + destruct j; simpl in * |- *.
        * apply functor_identity_paramatricity.
        * apply IHi; congruence.
        * apply functor_identity_paramatricity.
    - intros j idx m iso neq.
      destruct m.
      + destruct j; simpl in * |- *.
        * apply (functor_identity_paramatricity _ None _ OptionalType).
        * apply (functor_identity_paramatricity _ None _ OptionalType).
        * congruence.
      + destruct j; simpl in * |- *.
        * apply functor_identity_paramatricity.
        * apply functor_identity_paramatricity.
        * congruence.
  Qed.

  Theorem geso {idx A OA}(i j : positive)(x : TP.proj A)(m : t idx) iso iso' :
      i <> j -> get_eq i (set j A x m) iso = @get_eq _ i OA m iso'.
  Proof.
    unfold get_eq.
    intros neq.
    rewrite (gso _ _ x m (iso' @ !iso) neq).
    set (iso_comp := (iso' @ !iso) @ iso).
    change (iso_f iso (iso_f (iso' @ !iso) (get i m))) with
           (iso_f iso_comp (get i m)).
    apply isomorphism_unique.
  Qed.

  Theorem gres {idx A}(i : positive)(m : t (PTree.set i A idx)) iso :
      get i (remove_eq i m iso) = iso_g iso (F := OptionalType) tt.
  Proof.
    generalize dependent idx; induction i; destruct idx.
    (* xI i, PTree.Leaf *)
    - intros m iso.
      apply (functor_identity_paramatricity _ _ (iso_g iso) OptionalType).
    (* xI i, PTree.Node *)
    - intros m; apply IHi.
    (* xO i, PTree.Leaf *)
    - intros m iso.
      apply (functor_identity_paramatricity _ _ (iso_g iso) OptionalType).
    (* xO i, PTree.Node *)
    - intros m; apply IHi.
    (* xH, PTree.Leaf *)
    - intros m iso.
      apply (functor_identity_paramatricity _ _ (iso_g iso) OptionalType).
    (* xH, PTree.None *)
    - intros m iso.
      reflexivity.
  Qed.

  Theorem geres' {idx A OA}(i : positive)(m : t (PTree.set i A idx)) iso iso' :
      get_eq i (OA := OA) (remove_eq i m iso) iso' = iso_g (!iso' @ iso) tt.
  Proof.
    unfold get_eq, iso_concat, iso_inverse, iso_g at 1, iso_g at 1.
    f_equal.
    apply gres.
  Qed.

  Theorem geres {idx A}(i : positive)(m : t (PTree.set i A idx)) iso :
      get_eq i (OA := None) (remove_eq i m iso) iso = tt.
  Proof.
    rewrite (isomorphism_identity_fg iso OptionalType tt).
    apply geres'.
  Qed.

  Lemma rleaf {A}(i : positive)(m : t (PTree.set i A PTree.Leaf)) iso :
      remove_eq i m iso = Leaf.
  Proof. destruct i; reflexivity. Qed.

  Theorem greo {idx A}(i j : positive)(m : t (PTree.set j A idx)) iso iso' :
      i <> j -> get i (remove_eq j m iso) = iso_f iso' (get i m).
  Proof.
    generalize dependent idx; generalize dependent j.
    induction i; destruct idx.
    (* xI i, PTree.Leaf *)
    - intros m iso iso' neq.
      destruct j; simpl.
      + set (m2 := let (_, H) :=
          match m in (tree t')
          return match t' with
                 | PTree.Leaf => t PTree.Leaf * unit * t PTree.Leaf
                 | PTree.Node l o r => t l * OptionalType o * t r
                 end with
          | Leaf => (Leaf, tt, Leaf)
          | Node l r o1 m1 o0 m2 => (m1, o0, m2)
          end
        in H).
        assert (gleaf_iso OptionalType (get i (remove_eq j m2 gleaf_iso)) = tt).
          destruct i; destruct j; simpl; reflexivity.
        rewrite <- H at 1.
        apply (isomorphism_injective (iso_inverse (@gleaf_iso i)) _
                 (gleaf_iso OptionalType (get i (remove_eq j m2 gleaf_iso)))).
        (* iso_f (!gleaf_iso) (iso_f gleaf_iso (get i ...)) => get i ... *)
        unfold iso_inverse at 1, iso_f at 1;
          rewrite <- (isomorphism_identity_gf (@gleaf_iso i)).
        rewrite isomorphism_compose.
        apply IHi; congruence.
      + destruct i; apply (functor_identity_paramatricity _ _ iso' OptionalType).
      + destruct i; apply (functor_identity_paramatricity _ _ iso' OptionalType).
    (* xI i, PTree.Node *)
    - intros m iso iso' neq.
      destruct j; simpl.
      + apply IHi; congruence.
      + apply functor_identity_paramatricity.
      + apply functor_identity_paramatricity.
    (* xO i, PTree.Leaf *)
    - intros m iso iso' neq.
      destruct j; simpl.
      + destruct i; apply (functor_identity_paramatricity _ _ iso' OptionalType).
      + set (m1 := let (H, _) := let (H, _) :=
          match m in (tree t')
          return match t' with
                 | PTree.Leaf => t PTree.Leaf * unit * t PTree.Leaf
                 | PTree.Node l o r => t l * OptionalType o * t r
                 end with
          | Leaf => (Leaf, tt, Leaf)
          | Node l r o1 m1 o0 m2 => (m1, o0, m2)
          end
        in H in H).
        assert (gleaf_iso OptionalType (get i (remove_eq j m1 gleaf_iso)) = tt).
          destruct i; destruct j; simpl; reflexivity.
        rewrite <- H at 1.
        apply (isomorphism_injective (iso_inverse (@gleaf_iso i)) _
                 (gleaf_iso OptionalType (get i (remove_eq j m1 gleaf_iso)))).
        (* iso_f (!gleaf_iso) (iso_f gleaf_iso (get i ...)) => get i ... *)
        unfold iso_inverse at 1, iso_f at 1;
          rewrite <- (isomorphism_identity_gf (@gleaf_iso i)).
        rewrite isomorphism_compose.
        apply IHi; congruence.
      + destruct i; apply (functor_identity_paramatricity _ _ iso' OptionalType).
    (* xO i, PTree.Node *)
    - intros m iso iso' neq.
      destruct j; simpl.
      + apply functor_identity_paramatricity.
      + apply IHi; congruence.
      + apply functor_identity_paramatricity.
    (* xH, PTree.Leaf *)
    - intros m iso iso' neq.
      destruct j; try congruence; simpl in m |- *;
        dependent destruction m; destruct o0;
        apply (functor_identity_paramatricity _ _ (iso_f iso') OptionalType).
    (* xH, PTree.Node *)
    - intros m iso iso' neq.
      destruct j; simpl.
      + apply (functor_identity_paramatricity _ _ (iso_f iso') OptionalType).
      + apply (functor_identity_paramatricity _ _ (iso_f iso') OptionalType).
      + congruence.
  (* TODO: solve universe inconsistency *)
  Admitted.

  Theorem gereo {idx A OA}(i j : positive)(m : t (PTree.set j A idx))
    iso iso' iso'' :
      i <> j -> get_eq i (OA := OA) (remove_eq j m iso) iso'
              = iso_f iso' (get_eq i m iso'').
  Proof.
    intros neq; unfold get_eq.
    f_equal; apply greo; assumption.
  Qed.

End IPTree.

Module PTreeAux.
  Theorem smap1:
    forall (A B: Type) (f: A -> B) (a: A) (i: PTree.elt) (m: PTree.t A),
    PTree.set i (f a) (PTree.map1 f m) = PTree.map1 f (PTree.set i a m).
  Proof.
    induction i; destruct m; simpl; try rewrite <- IHi; reflexivity.
  Qed.
End PTreeAux.

Module PMapAux.
  Theorem smap:
    forall (A B: Type) (f: A -> B) (a: A) (i: PMap.elt) (m: PMap.t A),
    PMap.set i (f a) (PMap.map f m) = PMap.map f (PMap.set i a m).
  Proof.
    intros.
    unfold PMap.map, PMap.set; simpl.
    f_equal; apply PTreeAux.smap1.
  Qed.
End PMapAux.

Module ZMapAux.
  Theorem smap:
    forall (A B: Type) (f: A -> B) (a: A) (i: ZMap.elt) (m: ZMap.t A),
    ZMap.set i (f a) (ZMap.map f m) = ZMap.map f (ZMap.set i a m).
  Proof.
    intros.
    apply PMapAux.smap.
  Qed.
End ZMapAux.
