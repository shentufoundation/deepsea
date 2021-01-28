Require Import Coq.Program.Basics.
Require Import Coq.Classes.Morphisms.
Require Import Coq.Classes.RelationClasses.
Require Export Coq.Classes.RelationPairs.
Require Import Coq.Setoids.Setoid.

(** * Interface *)

(** Lenses are identified by the corresponding projection, which is
  also used to define the [get] operation. Note that because we use
  the letter [S] to denote the source type, which conflicts with the
  successor function on [nat] from the standard library, we need to
  use a verbose style below to avoid arguments being named [S0] or
  something such. (This is an obvious software engineering quirk
  with the way Coq generates identifier.) *)

Class LensOps {S V: Type} (π: S -> V) := {
  set: V -> S -> S
}.

Arguments set {S V} π {_} _ _ : simpl never.

Class LensSetGet {S V} π
  `{lens_ops: LensOps S V π} :=
{
  lens_set_get s:
    set π (π s) s = s
}.

Class LensGetSet {S V} π
  `{lens_ops: LensOps S V π} :=
{
  lens_get_set v s:
    π (set π v s) = v
}.

Class LensSetSet {S V} π
  `{lens_set: LensOps S V π} :=
{
  lens_set_set u v s:
    set π u (set π v s) = set π u s
}.

Class Lens {S V} π `{lens_ops: LensOps S V π} := {
  lens_lens_set_get :> LensSetGet π;
  lens_lens_get_set :> LensGetSet π;
  lens_lens_set_set :> LensSetSet π
}.


(** * Theory *)

(** ** Getters are measures, cf. [Coq.Classes.RelationPairs] *)

Instance lens_get_measure {S V} `{LensOps S V}:
  Measure (π).

(** ** The [modify] operation *)

Section MODIFY.
  Context {S V π} `{HSV: Lens S V π}.

  Definition modify (f: V -> V) (s: S) :=
    set π (f (π s)) s.

  Lemma lens_unfold_modify f s:
    modify f s = set π (f (π s)) s.
  Proof.
    reflexivity.
  Qed.
End MODIFY.

Arguments modify {S V} π {_} _ _.

(** ** The [same_context] relation *)

Section SAMECONTEXT.
  Context {S V π} `{lens_ops: LensOps S V π} `{HSV: LensSetSet S V π}.

  Definition same_context: relation S :=
    fun s t => forall v, set π v s = set π v t.

  Lemma lens_set_same_context v s:
    same_context (set π v s) s.
  Proof.
    intro u.
    apply lens_set_set.
  Qed.

  Lemma lens_modify_same_context f s:
    same_context (modify π f s) s.
  Proof.
    intro u.
    unfold modify.
    apply lens_set_set.
  Qed.

  Global Instance same_context_equiv: Equivalence same_context.
  Proof.
    split.
    * intros s v.
      reflexivity.
    * intros s t Hst u.
      symmetry; now auto.
    * intros s1 s2 s3 H12 H23 u.
      transitivity (set π u s2); now auto.
  Qed.

  Global Instance same_context_set_mor:
    Proper (eq ==> same_context ==> eq) (set π).
  Proof.
    intros u v Huv s t Hst.
    subst.
    apply Hst.
  Qed.

  Global Instance same_context_modify_mor f:
    Proper (same_context ==> same_context) (modify π f).
  Proof.
    intros s t Hst u.
    unfold modify.
    rewrite !lens_set_set.
    apply Hst.
  Qed.
End SAMECONTEXT.

Arguments same_context {S V} π {_} _ _.

(** ** Consequences of [LensGetSet] *)

Section GETSET.
  Context {S V} `{Hgs: LensGetSet S V}.

  Lemma lens_get_modify f s:
    π (modify π f s) = f (π s).
  Proof.
    apply lens_get_set.
  Qed.

  Theorem lens_eq_set (s: S) (u v: V):
    set π u s = set π v s <-> u = v.
  Proof.
    split; intros.
    * rewrite <- (lens_get_set u s).
      rewrite <- (lens_get_set v s).
      apply f_equal.
      assumption.
    * apply (f_equal (fun x => set π x s)).
      assumption.
  Qed.
End GETSET.

(** ** Consequences of [LensSetGet] *)

Section SETGET.
  Context {S V} `{HSV: LensSetGet S V}.

  (* same_context ∧ (eq @@ π) ⇔ eq *)
  Lemma lens_same_context_eq s1 s2:
    same_context π s1 s2 ->
    π s1 = π s2 ->
    s1 = s2.
  Proof.
    intros Hc Hv.
    rewrite <- (lens_set_get s1).
    rewrite <- (lens_set_get s2).
    rewrite Hv.
    apply Hc.
  Qed.
End SETGET.

(** ** Orthogonal lenses *)

(** We say that two lenses on [S] are orthogonal when they give us
  access to two independent parts of the larger structure [S],
  ie. modifying one will not affect the other. The property
  [lens_ortho_setr_setl] below is sufficient to express this.

  Note that for any two orthogonal lenses [π] and [ρ], you should
  never declare instances of both [OrthogonalLenses π ρ] and
  [OrthogonalLenses ρ π], since that would produce a loop in the
  [autorewrite] rules, which use [ortho_lenses_set_set]. *)

Class Orthogonal {S U V} (π: S -> U) (ρ: S -> V)
  `{πs: !LensOps π}
  `{ρs: !LensOps ρ}: Prop :=
{
  lens_ortho_setr_setl u v s:
    set ρ v (set π u s) = set π u (set ρ v s)
}.

Section ORTHOGONAL_LENSES.
  Context {S U V} (π: S -> U) (ρ: S -> V).
  Context `{πops: !LensOps π}.
  Context `{ρops: !LensOps ρ}.
  Context `{Hπρ: !Orthogonal π ρ}.

  Lemma lens_ortho_getl_setr `{Hπgs: !LensGetSet π} `{Hπsg: !LensSetGet π} s v:
    π (set ρ v s) = π s.
  Proof.
    rewrite <- (lens_set_get (π := π) s) at 1.
    rewrite lens_ortho_setr_setl.
    rewrite lens_get_set.
    reflexivity.
  Qed.

  Lemma lens_ortho_getr_setl `{Hρgs: !LensGetSet ρ} `{Hρsg: !LensSetGet ρ} s u:
    ρ (set π u s) = ρ s.
  Proof.
    rewrite <- (lens_set_get (π := ρ) s) at 1.
    rewrite <- lens_ortho_setr_setl.
    rewrite lens_get_set.
    reflexivity.
  Qed.
End ORTHOGONAL_LENSES.

Hint Rewrite
    @lens_get_set
    @lens_set_get
    @lens_set_set
    @lens_unfold_modify
    @lens_eq_set
    @lens_set_same_context
    @lens_modify_same_context
  using typeclasses eauto : lens.


(** * Tactics *)

(** The [autorewrite] base above can be used for normalization,
  however it is unable to handle some of the theorems related to
  orthogonal lenses.

  For example, we want to use [lens_ortho_getl_setr] to rewrite
  occurences of [α (set β v s)], where [α] and [β] are orthogonal
  lenses, into simply [α s]. However, our usual trick for combining
  [autorewrite] and typeclass resolution does not work, because the
  instance of [LensOps α] cannot be obtained by unification when using
  [@lens_ortho_getl_setr], which means the premises contain
  existentials and we would need [erewrite].

  Furthermore, we wish to normalize series of nested [set] so that
  their order matches the declared instances of [Orthogonal]. However,
  rewriting with [@lens_ortho_setr_setl] will only match the first
  pair, and fail if they are already in the right order (because a
  corresponding instance of [Orthogonal] won't be found).

  So we use the tactic below to apply those explicitely in addition
  to using [autorewrite]. *)

Ltac lens_norm_ortho :=
  repeat progress
    match goal with
      | H: context [set ?β ?v (set ?α ?u ?s)] |- _ =>
        rewrite (lens_ortho_setr_setl u v s) in H
      | |- context [set ?β ?v (set ?α ?u ?s)] =>
        rewrite (lens_ortho_setr_setl u v s)
      | H: context [?α (set ?β ?v ?s)] |- _ =>
        rewrite (lens_ortho_getl_setr α β s v) in H
      | |- context [?α (set ?β ?v ?s)] =>
        rewrite (lens_ortho_getl_setr α β s v)
      | H: context [?β (set ?α ?u ?s)] |- _ =>
        rewrite (lens_ortho_getr_setl α β s u) in H
      | |- context [?β (set ?α ?u ?s)] =>
        rewrite (lens_ortho_getr_setl α β s u)
    end.

Ltac lens_norm :=
  repeat progress (simpl in *;
                   lens_norm_ortho;
                   autorewrite with lens in *).

Ltac lens_simpl :=
  repeat progress (lens_norm; autorewrite with lens_simpl in *).

Ltac lens_unfold :=
  repeat progress (lens_simpl; unfold set in *).


(** * Instances *)

(** ** Pairs *)

Section PAIR.
  Global Instance fst_lensops A B: LensOps (@fst A B) := {
    set a x := (a, snd x)
  }.

  Global Instance fst_lens A B: Lens (@fst A B).
  Proof.
    split; split; intuition.
  Qed.

  Global Instance snd_lensops A B: LensOps (@snd A B) := {
    set b x := (fst x, b)
  }.

  Global Instance snd_lens A B: Lens (@snd A B).
  Proof.
    split; split; intuition.
  Qed.

  (** Here are some product-specific theorems. *)

  Lemma fst_same_context_eq_snd {A B} (x y: A * B):
    same_context (@fst A B) x y <-> snd x = snd y.
  Proof.
    destruct x as [a1 b1].
    destruct y as [a2 b2].
    split; intro H.
    * specialize (H a1).
      inversion H.
      reflexivity.
    * intro a.
      compute in *.
      congruence.
  Qed.

  Lemma snd_same_context_eq_fst {A B} (x y: A * B):
    same_context (@snd A B) x y <-> fst x = fst y.
  Proof.
    destruct x as [a1 b1].
    destruct y as [a2 b2].
    split; intro H.
    * specialize (H b1).
      inversion H.
      reflexivity.
    * intro b.
      compute in *.
      congruence.
  Qed.
End PAIR.

Hint Rewrite
  @fst_same_context_eq_snd
  @snd_same_context_eq_fst
  : lens_simpl.

(** ** Composing lens *)

Section COMPOSE.
  Context {A B C} (π: A -> B) (ρ: B -> C) `{Hπ: Lens _ _ π} `{Hρ: Lens _ _ ρ}.

  Instance compose_lensops: LensOps (compose ρ π) := {
    set c a := modify π (set ρ c) a
  }.

  Lemma lens_compose_get_simpl a:
    compose ρ π a = ρ (π a).
  Proof.
    reflexivity.
  Qed.

  Lemma lens_compose_set_simpl c a:
    set (compose ρ π) c a = modify π (set ρ c) a.
  Proof.
    reflexivity.
  Qed.

  Instance lens_compose_get_set: LensGetSet (compose ρ π).
  Proof.
    constructor; simpl; intros.
    rewrite lens_compose_get_simpl.
    rewrite lens_compose_set_simpl.
    autorewrite with lens.
    reflexivity.
  Qed.

  Instance lens_compose_set_get: LensSetGet (compose ρ π).
  Proof.
    constructor; simpl; intros.
    rewrite lens_compose_get_simpl.
    rewrite lens_compose_set_simpl.
    autorewrite with lens.
    reflexivity.
  Qed.

  Instance lens_compose_set_set: LensSetSet (compose ρ π).
  Proof.
    constructor; simpl; intros.
    rewrite !lens_compose_set_simpl.
    autorewrite with lens.
    reflexivity.
  Qed.

  Instance lens_compose: Lens (compose ρ π) := {}.
End COMPOSE.

Hint Rewrite
    @lens_compose_get_simpl
    @lens_compose_set_simpl
  using typeclasses eauto : lens_simpl.

(** ** Initial and terminal objects *)

Section INITIAL.
  Context {V: Type} (π: Empty_set -> V).

  Global Instance lens_empty_ops: LensOps π := {
    set a := Empty_set_rect _
  }.

  Global Instance lens_empty: Lens π.
  Proof.
    split; split; tauto.
  Qed.
End INITIAL.

Section COPRODUCT.
  Context {A B V} {α: A -> V} {β: B -> V}.
  Context `{αops: LensOps A V α}.
  Context `{βops: LensOps B V β}.

  Definition fcoprod (f: A -> V) (g: B -> V): A + B -> V :=
    fun s => match s with
               | inl a => f a
               | inr b => g b
             end.

  Notation "[ f , g ]" := (fcoprod f g).

  Instance lens_coprod_ops: LensOps [α, β] := {
    set v ab :=
      match ab with
        | inl a => inl (set α v a)
        | inr b => inr (set β v b)
      end
  }.

  Instance lens_coprod_get_set:
    LensGetSet α ->
    LensGetSet β ->
    LensGetSet [α,β].
  Proof.
    constructor.
    intros v [a|b];
    unfold set; simpl.
    * lens_norm; reflexivity.
    * lens_norm; reflexivity.
  Qed.

  Instance lens_coprod_set_get:
    LensSetGet α ->
    LensSetGet β ->
    LensSetGet [α,β].
  Proof.
    constructor.
    intros [a|b];
    unfold set; simpl.
    * lens_norm; reflexivity.
    * lens_norm; reflexivity.
  Qed.

  Instance lens_coprod_set_set:
    LensSetSet α ->
    LensSetSet β ->
    LensSetSet [α,β].
  Proof.
    constructor.
    intros u v [a|b];
    unfold set; simpl;
    f_equal.
    * lens_norm; reflexivity.
    * lens_norm; reflexivity.
  Qed.

  Global Instance lens_coprod:
    Lens α ->
    Lens β ->
    Lens [α,β].
  Proof.
    constructor;
    typeclasses eauto.
  Qed.
End COPRODUCT.

Section TERMINAL.
  Context {S: Type} (π: S -> unit).

  Instance lens_unit_ops: LensOps (S := S) π := {
    set u s := s
  }.

  Instance lens_unit: Lens π.
  Proof.
    assert (forall u v: unit, u = v) by (intros [] []; reflexivity).
    firstorder.
  Qed.
End TERMINAL.

(** Interestingly, the product only works for orthogonal lenses. *)

Section PRODUCT.
  Context {S A B} {α: S -> A} {β: S -> B}.
  Context `{αops: LensOps S A α}.
  Context `{βops: LensOps S B β}.

  Definition fprod (f: S -> A) (g: S -> B): S -> A * B :=
    fun s => (f s, g s).

  Notation "〈  f , g 〉" := (fprod f g).

  Instance lens_prod_ops: LensOps 〈α, β〉 := {
    set ab s := set α (fst ab) (set β (snd ab) s)
  }.

  Ltac go :=
    unfold set, fprod;
    simpl;
    lens_norm;
    try reflexivity.

  Instance lens_prod_get_set:
    Orthogonal α β ->
    LensSetGet β ->
    LensGetSet α ->
    LensGetSet β ->
    LensGetSet 〈α,β〉.
  Proof.
    constructor.
    intros [a b] s.
    go.
  Qed.

  Instance lens_prod_set_get:
    LensSetGet α ->
    LensSetGet β ->
    LensSetGet 〈α,β〉.
  Proof.
    constructor.
    intros s.
    go.
  Qed.

  Instance lens_prod_set_set:
    Orthogonal α β ->
    LensSetSet α ->
    LensSetSet β ->
    LensSetSet 〈α,β〉.
  Proof.
    constructor.
    intros [a1 b1] [a2 b2] s.
    go.
  Qed.

  Global Instance lens_prod:
    Orthogonal α β ->
    Lens α ->
    Lens β ->
    Lens 〈α,β〉.
  Proof.
    constructor;
    typeclasses eauto.
  Qed.
End PRODUCT.
