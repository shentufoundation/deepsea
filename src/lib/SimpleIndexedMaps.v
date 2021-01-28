(* A simpler version of Shu-chun's "indexed map" data structure.
   This one  just stores things in an association list instead of a tree. *)

Require Export Coq.Program.Equality.

Require Import Coqlib.
Require Import Maps.
Require Import DeepSpec.lib.SimpleMaps.

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

Open Scope positive.

(** * An implementation of indexed trees over type [positive]
    Element types are indexed by a tree [PTree.t A] along with a projection
    [proj] that extract the actual [Type] from [A].  *)
Module Type TypeProjection.
  Variable A : Type.
  Variable proj : A -> Type.
End TypeProjection.

Module IPList(TP : TypeProjection).
  Definition elt := positive.
  Definition elt_eq := peq.

  Definition OptionalType (a : option TP.A) : Type :=
    match a with
    | None => unit
    | Some a' => TP.proj a'
    end.

  Inductive ilist : AList.t TP.A -> Type :=
  | empty  : ilist AList.empty
  | set : forall {tps : AList.t TP.A}  i tp (x : TP.proj tp) (xs : ilist tps), ilist (AList.set i tp tps).

  Definition get_index {idx}(_ : ilist idx) := idx.

  Definition t := ilist.

  
  (* Todo: add this to the AList interface. *)
  Lemma AList_get_unfold : forall (A:Type) i j (x:A) l,
      AList.get i ((j,x)::l) = if (i =? j) then Some x else AList.get i l.
  Proof.
    reflexivity.
  Qed.
     
  Fixpoint get {idx}(i : positive)(m : t idx){struct m} :
      OptionalType (AList.get i idx) :=
    match m with
      | empty => tt
      | set tps j tp x xs =>
        if (i =? j) as b
           return (OptionalType (if b then Some tp else AList.get i tps))
        then x
        else get i xs                                              
    end.
  
  Definition get_eq {idx}(i : positive){OA}(m : t idx) :
      AList.get i idx ~~~ OA -> OptionalType OA
    := fun iso => iso OptionalType (get i m).

  (* Fixpoint remove {idx}(i : positive)(m : t idx){struct i} : t (PTree.remove i idx). *)
  (* Fixpoint remove_eq {idx}(i : positive) A (m : t (PTree.set i A idx)){struct i} :
       PTree.get i idx ~~~ None -> t idx. *)

  (* TODO: move this to the other file. *)
  Notation "a ! b" := (AList.get b a) (at level 1).
 
  Theorem gempty (i : positive)(iso : AList.empty ! i ~~~ None) :
      iso OptionalType (get i empty) = tt.
  Proof.
    simpl.
    symmetry.
    apply (functor_identity_paramatricity _ _ (iso_f iso) OptionalType).
  Qed.

  Lemma gleaf (i : positive)(iso : AList.empty ! i ~~~ None) :
      iso OptionalType (get i empty) = tt.
  Proof (gempty i iso).

  Theorem get_eq_fusion {idx OA OA'}(i : positive)(m : t idx)
    (iso : AList.get i idx ~~~ OA)(iso' : OA ~~~ OA') :
      iso' OptionalType (get_eq i m iso) = get_eq i m (iso @ iso').
  Proof.
    unfold get_eq.
    rewrite isomorphism_compose.
    reflexivity.
  Qed.

  Theorem gss {idx A}(i : positive)(x : TP.proj A)(m : t idx)
    (iso : (AList.set i A idx) ! i ~~~ Some A) :
      get i (set i A x m) = iso_g iso (F := OptionalType) x.
  Proof.
    revert iso.
    simpl.
    rewrite Pos.eqb_refl.
    intros iso.
    rewrite <- (functor_identity_paramatricity _ _ (iso_g iso)).
    reflexivity.
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

  Definition gleaf_iso {i} : (@AList.empty TP.A) ! i ~~~ None.
  Proof. destruct i; exact identity_automorphism. Defined.

Lemma eqb_neq' : forall x y, x<>y -> (x =? y) = false.
  Proof.
    intros.
    rewrite <- Pos.eqb_neq in H.
    auto.
  Qed.
    
  Theorem gso {idx A}(i j : positive)(x : TP.proj A)(m : t idx) iso :
      i <> j -> get i (set j A x m) = iso_f iso (get i m).
  Proof.
    intros Hij.
    generalize dependent idx.
    simpl.
    rewrite (eqb_neq' _ _ Hij).
    intros.
    apply functor_identity_paramatricity.
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

  (*
  Theorem gres {idx A}(i : positive)(m : t (PTree.set i A idx)) iso :
      get i (remove_eq i m iso) = iso_g iso (F := OptionalType) tt.
  Proof.
  Theorem geres' {idx A OA}(i : positive)(m : t (PTree.set i A idx)) iso iso' :
      get_eq i (OA := OA) (remove_eq i m iso) iso' = iso_g (!iso' @ iso) tt.

  Theorem geres {idx A}(i : positive)(m : t (PTree.set i A idx)) iso :
      get_eq i (OA := None) (remove_eq i m iso) iso = tt.

  Lemma rleaf {A}(i : positive)(m : t (PTree.set i A PTree.Leaf)) iso :
      remove_eq i m iso = Leaf.

  Theorem greo {idx A}(i j : positive)(m : t (PTree.set j A idx)) iso iso' :
      i <> j -> get i (remove_eq j m iso) = iso_f iso' (get i m).

  Theorem gereo {idx A OA}(i j : positive)(m : t (PTree.set j A idx))
    iso iso' iso'' :
      i <> j -> get_eq i (OA := OA) (remove_eq j m iso) iso'
              = iso_f iso' (get_eq i m iso'').
   *)
End IPList.

(*
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
*)
