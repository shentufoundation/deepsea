
(* Model of function temps (and args) *)

Require Import cclib.Coqlib.
Require Import backend.AST.
Require Import backend.Ctypes.
Require Import cclib.Maps.
Require Import backend.Environments.AllEnvironments.
Require Import backend.Values.HighValues.
Require Import backend.Options.
Require Import cclib.Integers.

(* All the different intermediate languages (down to stack, which gets rid of temps) represent args and temps as a list of this form: *)
Definition typed_idents : Type := list (positive * type).

Section FILTER.

  (* The peq in the standard library is opaque, but for us it's better if
     it computes. *)
  Definition my_peq : forall x y : positive, {x = y} + {x <> y}.
  decide equality.
  Defined.
  
(* filter t by removing ones with duplicate identifiers and removing the ones from excl.

This is used in the semantics whenever you use temps, in order to
enforce that there are no duplicates. E.g. at the top level, when
estimating gas for calling a function, the compiled code will not push
duplicates, so it also calls this filter function to get a tighter
bound.

*)
Fixpoint unique_temps_excl (t: typed_idents) (excl: typed_idents) : typed_idents :=
  match t with nil => nil
  | (tmp, ty) :: rest => let utemps := unique_temps_excl rest excl in
    match in_dec my_peq tmp (map fst utemps) with
    | left tmp_in => utemps (* found in the rest of t, remove duplicate *)
    | right tmp_notin => match in_dec my_peq tmp (map fst excl) with
      | left tmp_excl => utemps (* found in excl, remove *)
      | right tmp_notexcl => (tmp, ty) :: utemps
      end
    end
  end.

Lemma unique_temps_excl_norepet: forall t excl,
  list_norepet (map fst (unique_temps_excl t excl)).
Proof.
induction t; simpl; intros.
constructor.
destruct a.
destruct (in_dec my_peq p (map fst (unique_temps_excl t excl))); auto.
destruct (in_dec my_peq p (map fst excl)); auto.
simpl. constructor; auto.
Qed.

Lemma unique_temps_excl_in: forall t excl x,
  In x (map fst (unique_temps_excl t excl)) ->
  In x (map fst t).
Proof.
induction t; simpl; intros.
auto.
destruct a.
destruct (in_dec my_peq p (map fst (unique_temps_excl t excl))).
apply IHt in H. auto.
destruct (in_dec my_peq p (map fst excl)).
apply IHt in H. auto.
simpl in H.
destruct H. subst. auto.
apply IHt in H. auto.
Qed.

Lemma unique_temps_excl_disjoint: forall t excl,
  list_disjoint (map fst (unique_temps_excl t excl)) (map fst excl).
Proof.
induction t; simpl; intros.
unfold list_disjoint. simpl. auto.
destruct a.
destruct (in_dec my_peq p (map fst (unique_temps_excl t excl))).
apply IHt.
destruct (in_dec my_peq p (map fst excl)).
apply IHt.
simpl.
unfold list_disjoint; simpl; intros.
destruct H; intro; subst. contradiction.
unfold list_disjoint in IHt. assert (A := IHt excl y y H H0). auto.
Qed.

Lemma unique_temps_excl_defined : forall i ty lst excl,
    In (i,ty) lst ->
    ~In i (map fst excl) ->
    In i (map fst (unique_temps_excl lst excl)).
Proof.
  induction lst.
  - intros.
    simpl in *.
    auto.
  - intros.
    simpl in *.
    destruct a.
    destruct (in_dec my_peq p (map fst (unique_temps_excl lst excl))).
    + destruct H.
      * inversion H.
        subst.
        assumption.
      * apply IHlst;
        assumption.
    + destruct H.
      * inversion H.
        subst.
        destruct (in_dec my_peq i (map fst excl)).
          tauto.
          simpl; auto.
      * destruct (in_dec my_peq p (map fst excl)).
          apply IHlst; assumption.
          simpl. right. apply IHlst; assumption.
Qed.

Definition unique_temps (t: typed_idents) : typed_idents :=
  unique_temps_excl t nil.

Lemma unique_temps_disjoint: forall t excl,
  list_disjoint (map fst (unique_temps_excl t excl)) (map fst (unique_temps excl)).
Proof.
unfold list_disjoint. intros. intro; subst.
apply unique_temps_excl_in in H0.
assert (A := unique_temps_excl_disjoint t excl y y H H0). auto.
Qed.

Definition unique_all_temps (t: typed_idents) (t': typed_idents) : typed_idents :=
  unique_temps_excl t t' ++ unique_temps t'.

Lemma norepet_disjoint: forall A (t t': list A),
  list_disjoint t t' ->
  list_norepet t ->
  list_norepet t' ->
  list_norepet (t ++ t').
Proof.
induction t; simpl; intros.
auto.
inv H0.
assert (~ In a t'). { intro. unfold list_disjoint in H. unfold not in H. eapply H; eauto. simpl. auto. }
apply list_disjoint_cons_left in H.
constructor.
intro. apply in_app_or in H2. destruct H2; contradiction.
apply IHt; auto.
Qed.

Lemma unique_all_temps_norepet: forall t t',
  list_norepet (map fst (unique_all_temps t t')).
Proof.
unfold unique_all_temps.
intros.
rewrite map_app.
apply norepet_disjoint.
apply unique_temps_disjoint.
apply unique_temps_excl_norepet.
apply unique_temps_excl_norepet.
Qed.

End FILTER.

Section INIT.

(* Functions and lemmas related to setting the initial args in the temp_env. *)
  
(*
init_args is the first part of the semantics of calling a function, then init_temps adds zeros for actual temporaries. 

For proof's sake, add these to the temp_env in the order that they get pushed to the stack. 
Outputs None if args and vals are different lengths.
 *)
Fixpoint init_args (args: typed_idents) (vals: list val) : option temp_env :=
  match args, vals with
  | nil, nil => Some (PTree.empty _)
  | (name, _)::rest, v::vs => r <- init_args rest vs ;; Some ((PTree.set name v r):temp_env)
  | _, _ => None
  end.

Remark init_args_len: forall args vals le,
  init_args args vals = Some le -> length args = length vals.
Proof. induction args; induction vals; simpl; intros.
auto. discriminate. destruct a; discriminate. destruct a.
BindSome H r r_eq. SomeSome H le. replace (length vals) with (length args).
auto. eapply IHargs; eauto.
Qed.

Lemma init_args_defined : forall (args: typed_idents) (vals: list val) i ty t,
    In (i, ty) args ->
    init_args args vals = Some t ->
    exists v, PTree.get i t = Some v.
Proof.
  induction args; destruct vals; simpl; try congruence; try tauto.
  - destruct a; intros; simpl in *.
    congruence.
  - intros.    
    destruct H.
    + subst; simpl in *.
      destruct (init_args args vals) as [t'|]; simpl in *; [|congruence].
      simpl in *.
      inversion H0.
      subst.
      exists v.
      rewrite PTree.gss.
      reflexivity.
    + destruct a as [i' v'].
      destruct (init_args args vals) as [t'|] eqn:H_eqt'; simpl in *; [|congruence].
      specialize (IHargs vals _ ty t' H H_eqt').
      inversion H0.
      destruct (peq i' i).
      * subst.
        rewrite PTree.gss.
        eauto.
      * rewrite PTree.gso by congruence.
        exact IHargs.
Qed.

Fixpoint init_temps (temps: typed_idents) (base: temp_env) : temp_env :=
  match temps with
  | nil => base
  | (name, _)::rest => init_temps rest (PTree.set name (Vint Int256.zero) base)
  end.


End INIT.

(* generic function with temps *)
Record ftemps : Type := mkftemps {
  f_t : Type;
  f_temps : f_t -> typed_idents;
  f_args : f_t -> typed_idents
}.

Section FUNCTIONTYPE.

(* All the intermediate langauges have a notion of arguments and temps, so here we prove some lemmas about those. *)
  

Variable ftype : ftemps.
Definition function : Type := f_t ftype.
Definition fn_temps := f_temps ftype.
Definition fn_params := f_args ftype.

Section FUNCTION.

Variable f : function.
Definition temps := fn_temps f.
Definition params := fn_params f.

(* The function temps and args after removing duplicates. *)
Definition some_temps : typed_idents :=
  unique_temps_excl temps params.

Definition some_args : typed_idents :=
  unique_temps params.


Lemma some_args_defined : forall i ty,
     In (i, ty) params ->
     exists ty', In (i,ty') some_args.
Proof.
  unfold some_args.
  intros i ty.
  induction params.
  - simpl; intros; tauto.
  - simpl.
    unfold unique_temps.
    simpl.
    intros [H | H].
    + subst.
      destruct (in_dec my_peq i (map fst (unique_temps_excl t nil))) as [i0 | i0].
      * apply list_in_map_inv in i0.
        destruct i0 as [[i' ty'] [Hi' H']].
        simpl in Hi'.
        subst.
        exists ty'.
        exact H'.
      * exists ty.
        simpl.
        auto.
    + specialize (IHt H).
      destruct a as [i0 ty0].
      assert (Hin: In i (map fst (unique_temps_excl t nil))).
        {
          apply unique_temps_excl_defined with ty.
          assumption.
          simpl; auto.
        }
        apply list_in_map_inv in Hin.
        destruct Hin as [[i' ty'] [Hin1 Hin2]].
      destruct (in_dec my_peq i0 (map fst (unique_temps_excl t nil))).
      * simpl in *; subst.
        exists ty'.
        assumption.
      * simpl in *; subst.
        exists ty'.
        right.
        assumption.
  Qed.

        (*
When combining temps, we want a few properties which are required by how they are used on the Stack.
1. The temps should be have unique identifiers. This allows random access sets and gets, with nice proofs.
2. Args are pushed first, and temps pushed after, so in list form they are (temps ++ args)
*)
Definition all_temps : typed_idents :=
  unique_all_temps temps params.

Remark all_temps_split: all_temps = some_temps ++ some_args.
Proof. auto. Qed.

(* Returns None if the length of the argument list is different form the number of values provided. *)
Definition function_initial_temps (vals: list val) : option temp_env :=
  args_initial <- init_args some_args vals ;;
  Some (init_temps some_temps args_initial).

Lemma init_temps_defined : forall i ty lst te,
    In (i, ty) lst \/ PTree.get i te <> None ->
    exists v, PTree.get i (init_temps lst te) = Some v.
Proof.
  induction lst.
  - inversion 1.
    + inversion H0.
    + simpl.
      destruct (PTree.get i te) as [|v].
      * eexists; reflexivity.
      * congruence.
  - intros.
    simpl in H.
    destruct H as [[H|H] | H].
    + subst.
      simpl.
      apply IHlst.
      right.
      rewrite PTree.gss.
      congruence.
    + destruct a as [a aty].
      simpl.      
      destruct (IHlst (PTree.set a (Vint Int256.zero) te)) as [v Hv].
      left; assumption.
      exists v; apply Hv.
    + destruct a as [a aty].
      simpl.
      destruct (IHlst (PTree.set a (Vint Int256.zero) te)) as [v Hv].
      { right.
        destruct (my_peq a i).
        - subst.
          rewrite PTree.gss.
          congruence.
        - rewrite PTree.gso; congruence.
      }
      exists v; apply Hv.
Qed.




End FUNCTION.

End FUNCTIONTYPE.


(* match two functions that have the same temps but different type.

  This is used in the proofs when going between two intermediate
  languages which both have functions with temps and args. The lemmas
  says that the corresponding enviroments in the two langauges are the
  same.
 *)
Section FUNCTIONMATCH.

Variable ftype1 : ftemps.
Definition function1 : Type := f_t ftype1.
Definition fn_temps1 := f_temps ftype1.
Definition fn_params1 := f_args ftype1.

Variable ftype2 : ftemps.
Definition function2 : Type := f_t ftype2.
Definition fn_temps2 := f_temps ftype2.
Definition fn_params2 := f_args ftype2.


Inductive match_ftemps: function1 -> function2 -> Prop :=
  | match_ftemps_intro: forall f f',
    fn_temps1 f = fn_temps2 f' ->
    fn_params1 f = fn_params2 f' ->
    match_ftemps f f'.

Lemma match_some_temps: forall f f',
  match_ftemps f f' ->
  some_temps ftype1 f = some_temps ftype2 f'.
Proof.
intros. inv H. unfold some_temps.
replace (temps ftype1 f) with (temps ftype2 f').
replace (params ftype1 f) with (params ftype2 f').
auto.
Qed.

Lemma match_some_args: forall f f',
  match_ftemps f f' ->
  some_args ftype1 f = some_args ftype2 f'.
Proof.
intros. inv H. unfold some_args.
replace (params ftype1 f) with (params ftype2 f').
auto.
Qed.

Lemma match_all_temps: forall f f',
  match_ftemps f f' ->
  all_temps ftype1 f = all_temps ftype2 f'.
Proof.
intros. inv H. unfold all_temps.
replace (temps ftype1 f) with (temps ftype2 f').
replace (params ftype1 f) with (params ftype2 f').
auto.
Qed.

Lemma match_initial_temps: forall f f' vals le,
  match_ftemps f f' ->
  function_initial_temps ftype1 f vals = Some le ->
  function_initial_temps ftype2 f' vals = Some le.
Proof.
unfold function_initial_temps.
intros. BindSome H0 args_initial args_eq. SomeSome H0 le.
replace (some_args ftype2 f') with (some_args ftype1 f).
replace (some_temps ftype2 f') with (some_temps ftype1 f).
rewrite args_eq. simpl. auto.
apply match_some_temps; auto. apply match_some_args; auto.
Qed.


End FUNCTIONMATCH.

Lemma function_initial_temps_defined : forall i ty (ftype  : ftemps) (f :function ftype) t,
    In (i, ty) (fn_temps ftype f) ->
    ~In i (map fst (params ftype f))  ->
   exists v, PTree.get i
               (init_temps (some_temps ftype f)
                           t)
             = Some v.
Proof.
  intros i ty ftype f t Hin Hnin.
  assert (in_some_temps
   := unique_temps_excl_defined i ty (fn_temps ftype f) _ Hin Hnin).
  apply list_in_map_inv in in_some_temps.
  destruct in_some_temps as [ [i' ty'] [i'_eq Hin']].
  subst.
  
  destruct (init_temps_defined i' ty' (some_temps ftype f) t) as [v H].
  { left.
    exact Hin'.
  }
  exists v.
  exact H.
Qed.


Definition function_initial_temps_defined2
  : forall (ftype:ftemps) (f:function ftype)
           (vals: list val) (i:positive) (ty:type) t,
   In (i, ty) (params ftype f) ->
   function_initial_temps ftype f vals = Some t ->
   exists v, PTree.get i t = Some v.
Proof.
  intros ftype f vals i ty t Hin Hinitial_temps.
  unfold function_initial_temps in Hinitial_temps.
  destruct (init_args (some_args ftype f) vals) as [t'|] eqn:Hinit_args; simpl in *; [|congruence] .
  assert (Hin': exists ty', In (i,ty') (some_args ftype f) ).
  {
    apply some_args_defined with ty.
    exact Hin.
  }
  destruct Hin' as [ty' Hin'].
  destruct (init_args_defined (some_args ftype f) vals i ty' t' Hin' Hinit_args).
  destruct (init_temps_defined i ty' (some_temps ftype f) t') as [v Hv].
  { right.
    congruence.
  }
  inversion Hinitial_temps.    
  exists v.
  exact Hv.
Qed.


(* Helper function for (possibly) setting temps.*)
Definition optset (key: option ident) (v: val) (le: temp_env) : temp_env :=
  match key with None => le
  | Some id => PTree.set id v le 
  end.

(* Helper function to set a list of keys to a list of values.
For the sake of the proof, order matters. The first ident/value pair is set first.
Not a total function because the lists must be the same length *)
Fixpoint listset (keys: list ident) (vals: list val) (le: temp_env) : option temp_env :=
  match keys, vals with
  | nil, nil => Some le
  | k::ks, v::vs => listset ks vs (PTree.set k v le)
  | _, _ => None
  end.

Remark listset_len: forall rv_keys rvs le le',
  listset rv_keys rvs le = Some le' -> length rvs = length rv_keys.
Proof. induction rv_keys; induction rvs; intros; inv H. auto. simpl.
replace (length rv_keys) with (length rvs). auto. eapply IHrv_keys; eauto.
Qed.
