(* *********************************************************************)
(*    DeepSpec, the language of certified softwares                    *)
(*                                                                     *)
(*      Shu-Chun Weng, Yale University                                 *)
(*                                                                     *)
(*  Copyright (c) 2013-2015 Shu-Chun Weng <shu-chun.weng@yale.edu>.    *)
(*                                                                     *)
(*  This program is free software; you can redistribute it and/or      *)
(*  modify it under the terms of the GNU General Public License        *)
(*  version 2 as published by the Free Software Foundation.  Note that *)
(*  the only valid version of the GPL for this work is version 2, not  *)
(*  v2.2 or v3.x or whatever.                                          *)
(*                                                                     *)
(*  This program is distributed in the hope that it will be useful,    *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of     *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *)
(*  GNU General Public License for more details.                       *)
(*                                                                     *)
(*  You should have received a copy of the GNU General Public License  *)
(*  along with this program; if not, write to the Free Software        *)
(*  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,         *)
(*  MA 02110-1301 USA.                                                 *)
(* *********************************************************************)

(** Primitive type pairing and operators on them. *)

(* Standard library modules *)
Require Import ZArith.
Require Import Znumtheory.

(* CompCert modules *)
Require Import backend.Values.HighValues.
Require Import cclib.Integers.
Require Import backend.AST.
Require Import backend.Cop.
Require Import backend.Ctypes.

(* DeepSpec modules *)

Require Import DeepSpec.core.Cval.
Require Import DeepSpec.core.HyperType.
Require Import DeepSpec.core.MemoryModel.
Require Import DeepSpec.lib.OProp.
Require Import DeepSpec.lib.Monad.ContOptionMonad.

(** * [unit] corresponds to [void] *)
Notation tvoid_unit := (Tpair unit Tvoid).

Section HYPER_TYPE_UNIT.
  Global Instance void_unit_impl : HyperTypeImpl tvoid_unit := {
    ht_cval := fun _ => CVany;
    ht_ft_cond := fun _ => True;
    ht_default := tt;
    ht_valid_ft_cond := fun _ => False;
    ht_valid_ft_ocond := ofalse1;
    (* ht_inject j a b := True *)
  }.
  Global Instance void_unit : HyperType tvoid_unit.
  Proof. esplit.
    - (* ht_ft_rel_core *)
      intros.
      exists CVany.
      reflexivity.

    - (* ht_default_ft_cond *)
      exact I.

    - (* ht_valid_ft_ocond_same *)
      simpl; split; trivial.
  Qed.

  Definition void_unit_pair := mk_hyper_type_pair tvoid_unit.

End HYPER_TYPE_UNIT.

Section HYPER_LTYPE_COMPOSITION.
  Context`{HM : HyperMem}.
  Context {tp : type_pair}.
  Variable (whole : ltype_pair tp).

  Context`{hti : HyperTypeImpl tp, hlt : !HyperLType whole}.

  Section HYPER_LTYPE_FIELD.
    Context {tp_field : type_pair}.
    Variable (tid : ident).
    Context`{htif : !HyperTypeImpl tp_field,
             hfieldi : !HyperFieldImpl tp tp_field tid,
             hfield : !HyperField tp tp_field tid}.

    Definition field_ltype_pair : ltype_pair tp_field := {|
      ltype_tp_marker := create_type_marker tp_field;

      ltype_get s := Hfield_get (whole.(ltype_get) s);
      ltype_set v s :=
        whole.(ltype_set)
          (Hfield_set v (whole.(ltype_get) s))
        s;

      ltype_set_ocond :=
        (whole.(ltype_set_ocond) /\ whole.(ltype_get_extra_ocond))%oprop1;
      ltype_get_extra_ocond :=
        omap1 (fun p y => p (whole.(ltype_get) y)) ht_valid_ft_ocond;

      ltype_ghost := whole.(ltype_ghost);
      ltype_ident := (Field (whole.(ltype_ident)) tid)
    |}.

    Lemma nat_le_discrete (n m : nat) :
        (n <= m -> exists n', n' + n = m)%nat.
    Proof.
      induction 1.
      - exists 0%nat; reflexivity.
      - destruct IHle as [ n'' <- ].
        exists (S n'').
        reflexivity.
    Qed.

    Context {htw : HyperType tp}.
    Global Instance field_ltype : HyperLType field_ltype_pair.
    Proof.
      esplit.
      esplit.      
      - (* ltype_get_match *)
        intros j d m mm dc.

        simpl in dc.
        rewrite oand1_distr in dc; destruct dc as [ whole_dc dc ].
        rewrite OProp1map1, <- ht_valid_ft_ocond_same in dc; try exact I.

        destruct hlt as [hlt_dir].
        simpl in H. specialize (hlt_dir H).
        assert (IH := ltype_get_match (HyperLTypeDir := hlt_dir) _ _ _ mm whole_dc).
        
        assert (field_match := Hfield_get_correct (whole.(ltype_get) d)
                                (proj1 IH) dc).
        inversion field_match as [| f v ? f_eq access_some ];
          subst; clear field_match.

        (* Unify information from [HyperField] and [Cval] *)
        destruct (cval_sem_field_access _ _ _ _ (eq_sym access_some))
          as (strid' & flds'  & t2 & delta & ty_eq' & field_offset_eq' & match_imply).

        destruct (Hfield_delta_correct) as [strid [ofs [flds [ty_eq field_offset_eq]]]].
        rewrite ty_eq' in ty_eq; clear ty_eq'.
        inversion ty_eq; subst.
        rewrite field_offset_eq' in field_offset_eq; clear field_offset_eq'.
        inversion field_offset_eq; subst.

        split.
        + (* ht_ft_cond *)
          apply Hfield_get_returns, IH.
        + (* cval_match_indirect *)
          unfold ltype_ident; simpl.
          apply match_imply, IH.

      - (* ltype_set_match *)
        simpl.
        intros f j d m fc mm dc m' disjoint_eq cm.
        admit.
    Admitted.
  End HYPER_LTYPE_FIELD.
  
  Section HYPER_LTYPE_INDEX.
    Context {tp_elem : type_pair}.
    Context`{htif : HyperTypeImpl tp_elem,
             hidxi : !HyperIndexImpl tp tp_elem,
             hidx : !HyperIndex tp tp_elem}.

    Variable (idx : Z).

    Definition indexing_ltype_pair : ltype_pair tp_elem := {|
      ltype_tp_marker := create_type_marker tp_elem;

      ltype_get s := Hindex_get idx (whole.(ltype_get) s);
      ltype_set v s :=
        whole.(ltype_set)
          (Hindex_set idx v (whole.(ltype_get) s))
        s;

      ltype_set_ocond :=
        (whole.(ltype_set_ocond) /\ whole.(ltype_get_extra_ocond))%oprop1;
      ltype_get_extra_ocond := otrue1;

      ltype_ghost := whole.(ltype_ghost);
      ltype_ident := (Index whole.(ltype_ident) (Int256.repr idx))
    |}.

    Require Import cclib.Coqlib.
    
    Global Instance indexing_ltype :
      forall idx_in_bound : 0 <= idx < Hindex_size,
      HyperLType indexing_ltype_pair.
    Proof.
      (* Prepare commonly used assertions *)
      assert (array_type_eq := Hindex_array_type).

      intros idx_in_bound.


      esplit.
      esplit.
            
      - (* ltype_get_match *)
        intros j d m mm dc.

        simpl in dc.
        rewrite oand1_distr in dc.
        apply proj1 in dc.

        assert (index_match := Hindex_get_correct (whole.(ltype_get) d)
                                 _ idx_in_bound).
        inversion index_match as [| f v ? f_eq index_some ];
          subst; clear index_match.

        (* Unify information from [HyperIndex] and [Cval] *)
        destruct (cval_sem_array_indexing _ _ _ _ (eq_sym index_some))
          as (t2 & n & ty_eq' & match_imply).
        rewrite array_type_eq in ty_eq'.
        inversion ty_eq'; subst; clear ty_eq'.

        destruct hlt as [hlt_dir].
        specialize (hlt_dir H).
        assert (IH := ltype_get_match (HyperLTypeDir := hlt_dir) _ _ _ mm dc).
        split.
        + (* ht_ft_cond *)
          apply Hindex_get_returns, IH.
          apply idx_in_bound.
        + (* cval_match_indirect *)
          simpl.
          apply match_imply, IH.

      - (* ltype_set_match *)
        simpl.
        intros f j d m fc mm dc m' disjoint_eq same_d cm.
        destruct hlt as [hlt_dir].
        specialize (hlt_dir H).
        apply (ltype_set_match (HyperLTypeDir:=hlt_dir)) with (m:=m); try assumption.
        + (* ht_ft_cond (Hindex_set f (ltype_get whole d)) *)
          apply Hindex_set_returns; try assumption.
          eapply ltype_get_match; [apply mm | apply dc].
        + (* oProp1 whole.(ltype_set_ocond) d *)
          rewrite oand1_distr in dc; apply dc.
        + (* disjoint_eq *)
          intros i'.
          destruct (disjoint_eq i') as [disjoint_eq' | disjoint_eq' ].
          * left.
            apply ident_ext_extends_inversion_Index in disjoint_eq'.
            exact disjoint_eq'.
          * right.
            exact disjoint_eq'.
        + (* cval_match_indirect *)
          assert (index_get_rel := Hindex_set_correct f (ltype_get whole d)
                                     _ idx_in_bound).
          inversion index_get_rel as [| ? ? v_eq f_eq index_get_eq ];
            subst; clear index_get_rel.
          unfold cval_array_update in index_get_eq.
          assert (orig := proj2 (ltype_get_match _ _ _ (proj2 mm) dc)).
          destruct (ht_cval _) as [| | | | []] in index_get_eq at 2, orig;
            try discriminate index_get_eq.
          rewrite array_type_eq in index_get_eq, orig |- *.
          destruct c as [map].
          destruct (zle 0 idx && zlt idx Hindex_size);
            try discriminate index_get_eq.
          injection index_get_eq as ->. 

          inversion orig as [ ? ? ? ? ?  | | ? ? ? ? orig_array | ];
            try match goal with [ H: is_immediate_type _ = true |- _] => now inversion H end; subst.
          inversion orig_array as [ ? ? ? ? orig_indir ]; subst.
          constructor; constructor.
          intros idx' idx_in_range'.
          specialize (orig_indir _ idx_in_range').
          destruct (zeq idx' idx) as [ -> | idx_ne ].
          * rewrite Maps.ZMap.gss.
            apply cm.
          * { rewrite Maps.ZMap.gso by assumption.
            apply cval_match_indirect_eq with (fst m).
            - apply orig_indir.
            - intros i' Hextends.
              destruct (disjoint_eq i') as [disjoint_eq' | disjoint_eq'].
              contradict disjoint_eq'.
              eapply ident_ext_extends_disjoint_Index with (o1:= Int256.repr idx').
              + intros Heq.
                destruct Hindex_size_bound.                
                apply Int256.repr_injective in Heq; omega.
              + assumption.
              + symmetry; assumption.
            }
    Qed.
  End HYPER_LTYPE_INDEX.

  Section HYPER_LTYPE_HASH.
    Context {tp_elem : type_pair}.
    Context`{htif : HyperTypeImpl tp_elem,
             hidxi : !HyperIntHashImpl tp tp_elem,
             hidx : !HyperIntHash tp tp_elem}.                             

    Variable (idx : int256).

    Definition inthash_ltype_pair : ltype_pair tp_elem := {|
      ltype_tp_marker := create_type_marker tp_elem;

      ltype_get s := Hhash_get idx (whole.(ltype_get) s);
      ltype_set v s :=
        whole.(ltype_set)
          (Hhash_set idx v (whole.(ltype_get) s))
        s;

      ltype_set_ocond :=
        (whole.(ltype_set_ocond) /\ whole.(ltype_get_extra_ocond))%oprop1;
      ltype_get_extra_ocond := otrue1;

      ltype_ghost := whole.(ltype_ghost);
      ltype_ident := (Index whole.(ltype_ident) idx)
   |}.


    Global Instance inthash_ltype :
      HyperLType inthash_ltype_pair.
    Proof.
      assert (hash_type_eq := Hhash_type).
      esplit.
      esplit.
      - (* ltype_get_match *)
        intros j d m mm dc.

        simpl in dc.
        rewrite oand1_distr in dc.
        apply proj1 in dc.

        assert (hash_match := Hhash_get_correct (whole.(ltype_get) d) idx).
        inversion hash_match as [| f v ? f_eq hash_some ];
          subst; clear hash_match.

        (* Unify information from [HyperIndex] and [Cval] *)
        destruct (cval_sem_hashmap_lookup _ _ _ _ (eq_sym hash_some))
          as (t2 & ty_eq' & match_imply).
        rewrite hash_type_eq in ty_eq'.
        inversion ty_eq'; subst; clear ty_eq'.

        destruct hlt as [hlt_dir].
        specialize (hlt_dir H).
        assert (IH := ltype_get_match (HyperLTypeDir := hlt_dir) _ _ _ mm dc).
        split.
        + (* ht_ft_cond *)
          apply Hhash_get_returns, IH.
        + (* cval_match_indirect *)
          simpl.
          apply match_imply, IH.

      - (* ltype_set_match *)
        simpl.
        intros f j d m fc mm dc m' disjoint_eq same_d cm.
        destruct hlt as [hlt_dir].
        specialize (hlt_dir H).
        apply (ltype_set_match (HyperLTypeDir:=hlt_dir)) with (m:=m); try assumption.
        + (* ht_ft_cond (Hindex_set f (ltype_get whole d)) *)
          apply Hhash_set_returns; try assumption.
          eapply ltype_get_match; [apply mm | apply dc].
        + (* oProp1 whole.(ltype_set_ocond) d *)
          rewrite oand1_distr in dc; apply dc.
        + (* disjoint_eq *)
          intros i'.
          destruct (disjoint_eq i') as [disjoint_eq' | disjoint_eq' ].
          * left.
            apply ident_ext_extends_inversion_Index in disjoint_eq'.
            exact disjoint_eq'.
          * right.
            exact disjoint_eq'.

        + (* cval_match_indirect *)
          assert (index_get_rel := Hhash_set_correct f (ltype_get whole d)
                                     idx).
          inversion index_get_rel as [| ? ? v_eq f_eq index_get_eq ];
            subst; clear index_get_rel.
          unfold cval_hashmap_update in index_get_eq.
          assert (orig := proj2 (ltype_get_match _ _ _ (proj2 mm) dc)).
          destruct (ht_cval _) as [| | | | []] in index_get_eq at 2, orig;
            try discriminate index_get_eq.
          rewrite hash_type_eq in index_get_eq, orig |- *.
          injection index_get_eq as ->.
          inversion orig as [ ? ? ? ? ?  | | | ? ? ?  orig_array ];
            try match goal with [ H: is_immediate_type _ = true |- _] => now inversion H end; subst.
          inversion orig_array as [ ? ? ?  orig_indir ]; subst.
          constructor; constructor.
          intros idx'.
          specialize (orig_indir idx').
          destruct (Int256.eq_dec idx' idx) as [ -> | idx_ne ].
          * rewrite Maps.Int256Map.gss.
            apply cm.
          * { rewrite Maps.Int256Map.gso by assumption.
            apply cval_match_indirect_eq with (fst m).
            - apply orig_indir.
            - intros i' Hextends.
              destruct (disjoint_eq i') as [disjoint_eq' | disjoint_eq'].
              contradict disjoint_eq'.
              eapply ident_ext_extends_disjoint_Index with (o1:= idx').
                assumption.
                assumption.
                symmetry; assumption. }
    Qed.

  End HYPER_LTYPE_HASH.

End HYPER_LTYPE_COMPOSITION.

(** * Integers as booleans *)
Notation tint_bool := (Tpair bool tint).

Section Integer256Extra.
Lemma modulus_gt_zero : Int256.modulus > 0.
Proof.
  unfold Z.gt, Z.compare, Int256.modulus, two_power_nat;
  reflexivity.
Qed.

Theorem sub_sub : forall x y z,
    Int256.sub (Int256.sub x y) z = Int256.sub x (Int256.add y z).
Proof.
    intros x y z.
    rewrite (Int256.sub_add_opp x), Int256.sub_add_l.
    symmetry; apply Int256.sub_add_r.
Qed.

Theorem add_repr : forall x y,
    Int256.add (Int256.repr x) (Int256.repr y) = Int256.repr (x + y).
Proof.
  intros x y.
  rewrite Int256.add_unsigned.
  apply Int256.eqm_samerepr, Int256.eqm_add;
    rewrite Int256.unsigned_repr_eq;
    apply Int256.eqmod_sym, Int256.eqmod_mod;
    reflexivity.
Qed.

Theorem sub_repr : forall x y,
    Int256.sub (Int256.repr x) (Int256.repr y) = Int256.repr (x - y).
Proof.
  intros x y.
  unfold Int256.sub.
  apply Int256.eqm_samerepr, Int256.eqm_sub;
    rewrite Int256.unsigned_repr_eq;
    apply Int256.eqmod_sym, Int256.eqmod_mod;
    reflexivity.
Qed.

Theorem mul_repr : forall x y,
    Int256.mul (Int256.repr x) (Int256.repr y) = Int256.repr (x * y).
Proof.
  intros x y.
  unfold Int256.mul.
  apply Int256.eqm_samerepr, Int256.eqm_mult;
    rewrite Int256.unsigned_repr_eq;
    apply Int256.eqmod_sym, Int256.eqmod_mod;
    reflexivity.
Qed.

Theorem add_shifted : forall x y z,
    Int256.add (Int256.sub x z) (Int256.add y z) = Int256.add x y.
Proof.
  intros x y z.
  rewrite Int256.add_permut, Int256.sub_add_opp, Int256.add_assoc,
         (Int256.add_commut (Int256.neg z)), Int256.add_neg_zero, Int256.add_zero.
  apply Int256.add_commut.
Qed.
End Integer256Extra.

Section HYPER_TYPE_BOOL.
  Local Open Scope Z_scope.

  Lemma small_modulo : 0 mod Int256.modulus = 0 /\ 1 mod Int256.modulus = 1.
  Proof.
    split; apply Zmod_small; split;
      try solve
        [ apply Zle_refl |
          unfold Int256.modulus, two_power_nat, Z.lt, Z.compare; trivial ].

    apply Zlt_le_weak.
    assert (1 = Z.succ 0).
      unfold Z.succ, Z.add; reflexivity.
    rewrite H; apply Zle_lt_succ.
    apply Zle_refl.
  Qed.
  Definition zero_mod_modulus := proj1 small_modulo.
  Definition one_mod_modulus := proj2 small_modulo.
  Ltac rewrite_unsigned_repr :=
    try unfold Int256.zero, Int256.one;
    try rewrite Int256.unsigned_repr_eq;
    try rewrite zero_mod_modulus;
    try rewrite one_mod_modulus.

  Local Instance int_bool_iso_impl : HyperTypeIsoImpl tint_bool := {
    ht_iso_ty_cond v := match v with
      | Vint i => Int256.unsigned i = 0 \/ Int256.unsigned i = 1
      | _ => False
      end;
    ht_iso_ft_cond f := True;

    ht_iso_default := false;

    ht_implement f := Vint (if f then Int256.one else Int256.zero);
    ht_spec v := match v with
      | Vint i => Int256.unsigned i =? 1
      | _ => false
      end
  }.
  Local Instance int_bool_iso : HyperTypeIso tint_bool.
  Proof. esplit.
    - (* ht_implement_returns *)
      intros; simpl.
      destruct f; simpl; rewrite_unsigned_repr; [ right | left ]; reflexivity.

    - (* ht_spec_returns *)
      intros; simpl.
      trivial.

    - (* ht_iso_default_ft_cond *)
      exact I.

    - (* ht_proof_left *)
      intros; simpl.
      destruct f; simpl; rewrite_unsigned_repr; reflexivity.

    - (* ht_proof_right *)
      intros v vc; simpl in vc |- *.
      destruct v; try contradiction.
      destruct vc as [ H | H ]; rewrite H; simpl;
        unfold Vtrue, Vfalse, Int256.zero, Int256.one;
        rewrite <- H; rewrite (Int256.repr_unsigned i); reflexivity.
  Qed.
  Global Instance int_bool_impl : HyperTypeImpl tint_bool := _.
  Global Instance int_bool : HyperType tint_bool := _.

  Definition int_bool_pair :=
    @mk_hyper_type_pair tint_bool int_bool_impl.

  Lemma ht_ty_cond_0_1 :
      ht_ty_cond tint_bool (CVval (Vint (Int256.repr 0))) /\
      ht_ty_cond tint_bool (CVval (Vint (Int256.repr 1))).
  Proof.
    split; [ exists false | exists true ]; split;
      solve [ exact I | reflexivity ].
  Qed.
  Definition int_bool_zero := proj1 ht_ty_cond_0_1.
  Definition int_bool_one  := proj2 ht_ty_cond_0_1.

  Global Instance int_bool_notbool_impl
      : HyperUnaryImpl Onotbool tint_bool tint_bool := {
    Hunary_cond ft := True;
    Hunary_ocond := otrue1;
    Hunary := negb
  }.
  Global Instance int_bool_notbool : HyperUnaryOp Onotbool tint_bool tint_bool.
  Proof. esplit.
    - (* Hunary_ocond_same *)
      reflexivity.

    - (* Hunary_returns *)
      simpl; trivial.

    - (* Hunary_correct *)
      intros f v fc oc rel.
      simpl in rel; rewrite <- rel.
      destruct f; constructor; reflexivity.
  Qed.
  Global Instance int_bool_notbool_passthrough
      : HyperUnaryPassthrough Onotbool tint_bool tint_bool.
  (* Proof. esplit.
    simpl; congruence.
  Qed. *)

  Global Instance int_bool_or_impl
      : HyperBinaryImpl Oor tint_bool tint_bool tint_bool := {
    Hbinary_cond f f' := True;
    Hbinary_ocond := otrue2;
    Hbinary := orb
  }.

  Global Instance int_bool_or
     : HyperBinaryOp Oor tint_bool tint_bool tint_bool := {
  }.
  Proof.
    - (* Hbinary_ocond_same *)
      reflexivity.

    - (* Hbinary_returns *)
      intros; simpl; tauto.

    - (* Hbinary_correct *)
      intros f f' v v' fc fc' oc rel rel'.
      simpl in rel, rel'; rewrite <- rel, <- rel'.
      destruct f; destruct f';
        constructor; reflexivity.
  Qed.
  Global Instance int_bool_or_passthrough
      : HyperBinaryPassthrough Oor tint_bool tint_bool tint_bool.
  (* Proof. esplit.
    simpl; congruence.
  Qed. *)

  Global Instance int_bool_xor_impl
      : HyperBinaryImpl Oxor tint_bool tint_bool tint_bool := {
    Hbinary_cond f f' := True;
    Hbinary_ocond := otrue2;
    Hbinary := xorb
  }.
  Global Instance int_bool_xor
      : HyperBinaryOp Oxor tint_bool tint_bool tint_bool := {
  }.
  Proof.
    - (* Hbinary_ocond_same *)
      reflexivity.

    - (* Hbinary_returns *)
      intros; simpl; tauto.

    - (* Hbinary_correct *)
      intros f f' v v' fc fc' oc rel rel'.
      simpl in rel, rel'; rewrite <- rel, <- rel'.
      destruct f; destruct f';
        constructor; reflexivity.
  Qed.
  Global Instance int_bool_xor_passthrough
      : HyperBinaryPassthrough Oxor tint_bool tint_bool tint_bool.
  (* Proof. esplit.
    simpl; congruence.
  Qed. *)

  Global Instance int_bool_and_impl
      : HyperBinaryImpl Oand tint_bool tint_bool tint_bool := {
    Hbinary_cond f f' := True;
    Hbinary_ocond := otrue2;
    Hbinary := andb
  }.
  Global Instance int_bool_and
      : HyperBinaryOp Oand tint_bool tint_bool tint_bool := {
  }.
  Proof.
    - (* Hbinary_ocond_same *)
      reflexivity.

    - (* Hbinary_returns *)
      intros; simpl; tauto.

    - (* Hbinary_correct *)
      intros f f' v v' fc fc' oc rel rel'.
      simpl in rel, rel'; rewrite <- rel, <- rel'.
      destruct f; destruct f';
        constructor; reflexivity.
  Qed.
  Global Instance int_bool_and_passthrough
      : HyperBinaryPassthrough Oand tint_bool tint_bool tint_bool.
  (* Proof. esplit.
    simpl; congruence.
  Qed. *)

  Global Instance int_bool_eq_impl
      : HyperBinaryImpl Oeq tint_bool tint_bool tint_bool := {
    Hbinary_cond f f' := True;
    Hbinary_ocond := otrue2;
    Hbinary f f' := negb (xorb f f')
  }.
  Global Instance int_bool_eq
      : HyperBinaryOp Oeq tint_bool tint_bool tint_bool := {
  }.
  Proof.
    - (* Hbinary_ocond_same *)
      reflexivity.

    - (* Hbinary_returns *)
      intros; simpl; tauto.

    - (* Hbinary_correct *)
      intros f f' v v' fc fc' oc rel rel'.
      simpl in rel, rel'; rewrite <- rel, <- rel'.
      destruct f; destruct f';
        constructor; reflexivity.
  Qed.
  Global Instance int_bool_eq_passthrough
      : HyperBinaryPassthrough Oeq tint_bool tint_bool tint_bool.
  (* Proof. esplit.
    simpl; congruence.
  Qed. *)

  Global Instance int_bool_ne_impl
      : HyperBinaryImpl One tint_bool tint_bool tint_bool := {
    Hbinary_cond f f' := True;
    Hbinary_ocond := otrue2;
    Hbinary := xorb
  }.
  Global Instance int_bool_ne
      : HyperBinaryOp One tint_bool tint_bool tint_bool := {
  }.
  Proof.
    - (* Hbinary_ocond_same *)
      reflexivity.

    - (* Hbinary_returns *)
      intros; simpl; tauto.

    - (* Hbinary_correct *)
      intros f f' v v' fc fc' oc rel rel'.
      simpl in rel, rel'; rewrite <- rel, <- rel'.
      destruct f; destruct f';
        constructor; reflexivity.
  Qed.
  Global Instance int_bool_ne_passthrough
      : HyperBinaryPassthrough One tint_bool tint_bool tint_bool.
  (* Proof. esplit.
    simpl; congruence.
  Qed. *)

  (* Other operations, e.g. addition, are not defined on tint_bool so
     there are no other instances, as demonstrated by the lacking of
     ``HyperBinaryOp Oadd tint_bool ...'' *)
End HYPER_TYPE_BOOL.

(** * Integers as Z *)
Section HYPER_TYPE_INT.
  Local Open Scope Z_scope.

  Class IntegerBound (bound : Z) : Prop := {
    integer_bound_within_modulus : Int256.modulus >= bound;
    integer_bound_positive : 0 < bound
  }.

  Definition Z_bounded (bound : Z){_ : IntegerBound bound} := Z.

  Typeclasses Opaque Z_bounded.

  Opaque Int256.repr.
  
Section BOUNDED.
  (* All the definitions will need these from here on. *)
  Variable bound : Z.
  Context `{bound_cond : IntegerBound bound}.

  Definition tint_Z_bounded
    := Tpair (Z_bounded bound) tint.

  Lemma unsigned_repr_eq (f : Z_bounded bound)(cond : -1 < f /\ bound > f)
      : Int256.unsigned (Int256.repr f) = f.
  Proof.
    rewrite Int256.unsigned_repr_eq.
    apply Zmod_small.
    destruct cond as [ m1_lt_f bound_gt_f ].
    split.
    - assert (0 = Z.succ (-1)).
        reflexivity.
      rewrite H; apply Zlt_le_succ; assumption.
    - apply Zgt_lt.
      eapply Zle_gt_trans;
       [ apply Zge_le; apply integer_bound_within_modulus | exact bound_gt_f ].
  Qed.

  Lemma bounded_repr_injective  : forall (f f' : Z_bounded bound)
                                 (cond : -1 < f /\ bound > f)
                                 (cond' : -1 < f' /\ bound > f'),
      (Int256.repr f) = (Int256.repr f') -> f = f'.
  Proof.
    intros.
    pose integer_bound_within_modulus.
    apply Int256.repr_injective in H; auto; omega.
  Qed.
    
  Instance int_Z_iso_impl : HyperTypeIsoImpl tint_Z_bounded := {
    ht_iso_ty_cond v := match v with
      | Vint i => bound > Int256.unsigned i
      | _ => False
      end;
    ht_iso_ft_cond f := -1 < f /\ bound > f;

    ht_iso_default := 0;

    ht_implement f := (Vint (Int256.repr f));
    ht_spec v := match v with
      | Vint i => Int256.unsigned i
      | _ => 0
      end
   }.

  
  Instance int_Z_iso : HyperTypeIso tint_Z_bounded.
  Proof. esplit.
    - (* ht_implement_returns *)
      intros f fc; simpl in fc |- *.
      rewrite unsigned_repr_eq;
      apply fc.

    - (* ht_spec_returns *)
      intros v vc; simpl in vc |- *.
      destruct v; try contradiction.
      split; try assumption.
      destruct i.
      unfold Int256.unsigned, Int256.intval.
      apply intrange.

    - (* ht_iso_default_ft_cond *)
      split; [ reflexivity |].
      apply Z.lt_gt, integer_bound_positive.

    - (* ht_proof_left *)
      intros f fc; simpl in fc |- *; unfold Z_bounded.
      apply unsigned_repr_eq, fc.

    - (* ht_proof_right *)
      intros v vc; simpl in vc |- *.
      destruct v; try contradiction.
      rewrite (Int256.repr_unsigned i).
      reflexivity.
  Qed.
  Instance int_Z_impl : HyperTypeImpl tint_Z_bounded := _.
  Instance int_Z : HyperType tint_Z_bounded := _.

  (* Notbool is not defined on tint_Z_bounded so the instance is not
     defined. Even if it was defined, the never satisfied
     pre-condition would render it useless in practice. *)

  Instance int_Z_bounded_add_impl
      : HyperBinaryImpl Oadd tint_Z_bounded tint_Z_bounded tint_Z_bounded := {
    Hbinary_cond f f' := f + f' < bound;
    Hbinary_ocond := oprop2 (fun f f' => f + f' < bound);
    Hbinary := Z.add
  }.
  Instance int_Z_bounded_add
      : HyperBinaryOp Oadd tint_Z_bounded tint_Z_bounded tint_Z_bounded := {
  }.
  Proof.
    - (* Hbinary_ocond_same *)
      reflexivity.

    - (* Hbinary_returns *)
      intros f f' fc fc' oc.
      split.
      + (* -1 < Hbinary f f' *)
        apply Z.le_succ_l; change (Z.succ (-1)) with (Z.succ (-1) + Z.succ (-1)).
        apply Z.add_le_mono; apply Z.le_succ_l; [ apply fc | apply fc' ].
      + (* bound > Hbinary f f' *)
        apply Z.lt_gt, oc.

    - (* Hbinary_correct *)
      intros f f' v v' fc fc' oc rel rel'.
      simpl in rel, rel' |- *.
      rewrite <- rel, <- rel'.
      constructor.
      rewrite add_repr; reflexivity.
  Qed.
  Instance int_Z_bounded_add_passthrough
      : HyperBinaryPassthrough Oadd tint_Z_bounded tint_Z_bounded tint_Z_bounded.
  (* Proof. esplit.
    simpl; congruence.
  Qed. *)

  Instance int_Z_bounded_sub_impl
      : HyperBinaryImpl Osub tint_Z_bounded tint_Z_bounded tint_Z_bounded := {
    Hbinary_cond f f' := f >= f';
    Hbinary_ocond := oprop2 (fun f f' => f >= f');
    Hbinary := Z.sub
  }.
  Instance int_Z_bounded_sub
      : HyperBinaryOp Osub tint_Z_bounded tint_Z_bounded tint_Z_bounded := {
  }.
  Proof.
    - (* Hbinary_ocond_same *)
      reflexivity.

    - (* Hbinary_returns *)
      intros f f' fc fc' oc.
      split.
      + (* -1 < Hbinary f f' *)
        simpl.
        rewrite <- Z.le_succ_l, <- Z.le_add_le_sub_r.
        apply Z.ge_le, oc.
      + (* bound > Hbinary f f' *)
        simpl in fc, fc' |- *.
        omega.

    - (* Hbinary_correct *)
      intros f f' v v' fc fc' oc rel rel'.
      simpl in rel, rel' |- *.
      rewrite <- rel, <- rel'.
      constructor.
      simpl.
      rewrite sub_repr; reflexivity.
  Qed.
  Instance int_Z_bounded_sub_passthrough
      : HyperBinaryPassthrough Osub tint_Z_bounded tint_Z_bounded tint_Z_bounded.
  (* Proof. esplit.
    simpl; congruence.
  Qed. *)

  Instance int_Z_bounded_mul_impl
      : HyperBinaryImpl Omul tint_Z_bounded tint_Z_bounded tint_Z_bounded := {
    Hbinary_cond f f' := f * f' < bound;
    Hbinary_ocond := oprop2 (fun f f' => f * f' < bound);
    Hbinary := Z.mul
  }.
  Instance int_Z_bounded_mul
      : HyperBinaryOp Omul tint_Z_bounded tint_Z_bounded tint_Z_bounded := {
  }.
  Proof.
    - (* Hbinary_ocond_same *)
      reflexivity.

    - (* Hbinary_returns *)
      intros f f' fc fc' oc.
      split.
      + (* -1 < Hbinary f f' *)
        apply Z.le_succ_l, Z.mul_nonneg_nonneg;
          change 0 with (Z.succ (-1)); apply Z.le_succ_l;
          [ apply fc | apply fc' ].
      + (* bound > Hbinary f f' *)
        apply Z.lt_gt, oc.

    - (* Hbinary_correct *)
      intros f f' v v' fc fc' oc rel rel'.
      simpl in rel, rel' |- *.
      rewrite <- rel, <- rel'.
      constructor.
      rewrite mul_repr; reflexivity.
  Qed.
  Instance int_Z_bounded_mul_passthrough
      : HyperBinaryPassthrough Omul tint_Z_bounded tint_Z_bounded tint_Z_bounded.
  (* Proof. esplit.
    simpl; congruence.
  Qed. *)

  Instance int_Z_bounded_mod_impl
      : HyperBinaryImpl Omod tint_Z_bounded tint_Z_bounded tint_Z_bounded := {
    Hbinary_cond f f' := f' <> 0;
    Hbinary_ocond := oprop2 (fun f f' => f' <> 0);
    Hbinary := Z.modulo
  }.
  Instance int_Z_bounded_mod
      : HyperBinaryOp Omod tint_Z_bounded tint_Z_bounded tint_Z_bounded := {
  }.
  Proof.
    - (* Hbinary_ocond_same *)
      reflexivity.

    - (* Hbinary_returns *)
      intros f f' fc fc' oc.
      simpl in oc, fc' |- *.
      split.
      + (* -1 < Hbinary f f' *)
        rewrite <- Z.le_succ_l.
        apply Z_mod_lt.
        apply proj1 in fc'.
        rewrite <- Z.le_succ_l, Z.lt_eq_cases in fc'.
        destruct fc' as [ lt | eq ];
          [ apply Z.lt_gt, lt | contradiction (oc (eq_sym eq)) ].
      + (* bound > Hbinary f f' *)
        eapply Zgt_trans; [ apply fc' |].
        apply Z.lt_gt, Z_mod_lt.
        apply proj1 in fc'.
        rewrite <- Z.le_succ_l, Z.lt_eq_cases in fc'.
        destruct fc' as [ lt | eq ];
          [ apply Z.lt_gt, lt | contradiction (oc (eq_sym eq)) ].

    - (* Hbinary_correct *)
      intros f f' v v' fc fc' oc rel rel'.
      simpl in rel, rel', fc, fc', oc |- *.

      assert (f_eq := unsigned_repr_eq _ fc).
      assert (f_eq' := unsigned_repr_eq _ fc').

      rewrite <- rel, <- rel'.
      simpl. (* unfold sem_mod, sem_binarith; simpl. *)
      assert (f_zero_spec := Int256.eq_spec (Int256.repr f') Int256.zero).
      destruct (Int256.eq (Int256.repr f') Int256.zero).
      + apply (f_equal Int256.unsigned) in f_zero_spec.
        rewrite Int256.unsigned_zero, f_eq' in f_zero_spec.
        contradiction (oc f_zero_spec).
      + constructor.
        unfold Int256.modu; rewrite f_eq, f_eq'.
        reflexivity.
  Qed.
  Instance int_Z_bounded_mod_passthrough
      : HyperBinaryPassthrough Omod tint_Z_bounded tint_Z_bounded tint_Z_bounded.
  (* Proof. esplit.
    simpl; congruence.
  Qed. *)

  (* The div operation is not defined in the backend yet. *)
  Instance int_Z_bounded_div_impl
      : HyperBinaryImpl Odiv tint_Z_bounded tint_Z_bounded tint_Z_bounded := {
    Hbinary_cond f f' := f' <> 0;
    Hbinary_ocond := oprop2 (fun f f' => f' <> 0);
    Hbinary := Z.div
  }.
  Instance int_Z_bounded_div
      : HyperBinaryOp Odiv tint_Z_bounded tint_Z_bounded tint_Z_bounded := {
  }.
  Proof.
    - (* Hbinary_ocond_same *)
      reflexivity.

    - (* Hbinary_returns *)
      intros f f' fc fc' oc.
      simpl in oc, fc, fc' |- *.

      assert (f_pos' : 0 < f').
      { apply proj1 in fc'.
        rewrite <- Z.le_succ_l, Z.lt_eq_cases in fc'.
        destruct fc' as [ lt | eq ];
          [ exact lt | contradiction (oc (eq_sym eq)) ].
      }

      split.
      + (* -1 < Hbinary f f' *)
        rewrite <- Z.le_succ_l.
        apply Z_div_pos; [ apply Z.lt_gt, f_pos' |].
        rewrite <- Z.le_succ_l in fc.
        apply fc.
      + (* bound > Hbinary f f' *)
        apply Z.lt_gt, Zdiv_lt_upper_bound; [ exact f_pos' |].
        eapply Z.lt_le_trans; [ apply Z.gt_lt, fc |].
        replace bound with (bound * 1) at 1 by apply Z.mul_1_r.
        apply Zmult_le_compat_l;
          [| apply Z.lt_le_incl, integer_bound_positive ].
        rewrite <- Z.le_succ_l in f_pos'.
        exact f_pos'.

    - (* Hbinary_correct *)
      intros f f' v v' fc fc' oc rel rel'.
      simpl in rel, rel', fc, fc', oc |- *.

      assert (f_eq := unsigned_repr_eq _ fc).
      assert (f_eq' := unsigned_repr_eq _ fc').

      rewrite <- rel, <- rel'.
      simpl.
      assert (f_zero_spec := Int256.eq_spec (Int256.repr f') Int256.zero).
      destruct (Int256.eq (Int256.repr f') Int256.zero).
      + apply (f_equal Int256.unsigned) in f_zero_spec.
        rewrite Int256.unsigned_zero, f_eq' in f_zero_spec.
        contradiction (oc f_zero_spec).
      + constructor.
        simpl.
        unfold Int256.divu; rewrite f_eq, f_eq'.
        reflexivity.
  Qed.
  Instance int_Z_bounded_div_passthrough
      : HyperBinaryPassthrough Odiv tint_Z_bounded tint_Z_bounded tint_Z_bounded.
  (* Proof. esplit.
    simpl; congruence.
  Qed. *)

  Instance int_Z_bounded_eq_impl
      : HyperBinaryImpl Oeq tint_Z_bounded tint_Z_bounded tint_bool := {
    Hbinary_cond f f' := True;
    Hbinary_ocond := otrue2;
    Hbinary := Z.eqb
  }.
  Instance int_Z_bounded_eq
      : HyperBinaryOp Oeq tint_Z_bounded tint_Z_bounded tint_bool := {
  }.
  Proof.
    - (* Hbinary_ocond_same *)
      reflexivity.

    - (* Hbinary_returns *)
      intros; exact I.

    - (* Hbinary_correct *)
      intros f f' v v' fc fc' oc rel rel'.
      simpl in rel, rel' |- *.
      rewrite <- rel, <- rel'.
      constructor.
      destruct (val_eq_dec (Vint (Int256.repr f)) (Vint (Int256.repr f'))).
      + pose (bounded_repr_injective f f' fc fc').
        rewrite e0; try congruence.
        rewrite Z.eqb_refl.
        reflexivity.
      + rewrite (proj2 (Z.eqb_neq f f')).
        * reflexivity.
        * intros Heq.
          subst.
          apply n.
          reflexivity.
  Qed.
  Instance int_Z_bounded_eq_passthrough
      : HyperBinaryPassthrough Oeq tint_Z_bounded tint_Z_bounded tint_bool.
  (* Proof. esplit.
    simpl; congruence.
  Qed. *)

  Instance int_Z_bounded_ne_impl
      : HyperBinaryImpl One tint_Z_bounded tint_Z_bounded tint_bool := {
    Hbinary_cond f f' := True;
    Hbinary_ocond := otrue2;
    Hbinary f f' := negb (Z.eqb f f')
  }.
  Instance int_Z_bounded_ne
      : HyperBinaryOp One tint_Z_bounded tint_Z_bounded tint_bool := {
  }.
  Proof.
    - (* Hbinary_ocond_same *)
      reflexivity.

    - (* Hbinary_returns *)
      intros; exact I.

    - (* Hbinary_correct *)
      intros f f' v v' fc fc' oc rel rel'.
      simpl in rel, rel' |- *.
      rewrite <- rel, <- rel'.
      constructor.
      destruct (val_eq_dec (Vint (Int256.repr f)) (Vint (Int256.repr f'))).
      + pose (bounded_repr_injective f f' fc fc').
        rewrite e0; try congruence.
        rewrite Z.eqb_refl.
        reflexivity.
      + rewrite (proj2 (Z.eqb_neq f f')).
        * reflexivity.
        * intros Heq.
          subst.
          apply n.
          reflexivity.
  Qed.
  Instance int_Z_bounded_ne_passthrough
      : HyperBinaryPassthrough One tint_Z_bounded tint_Z_bounded tint_bool.
  (* Proof. esplit.
    simpl; congruence.
  Qed. *)

  Instance int_Z_bounded_lt_impl
      : HyperBinaryImpl Olt tint_Z_bounded tint_Z_bounded tint_bool := {
    Hbinary_cond f f' := True;
    Hbinary_ocond := otrue2;
    Hbinary := Z.ltb
  }.
  Instance int_Z_bounded_lt
      : HyperBinaryOp Olt tint_Z_bounded tint_Z_bounded tint_bool := {
  }.
  Proof.
    - (* Hbinary_ocond_same *)
      reflexivity.

    - (* Hbinary_returns *)
      intros; exact I.

    - (* Hbinary_correct *)
      intros f f' v v' fc fc' oc rel rel'.
      simpl in rel, rel' |- *.
      rewrite <- rel, <- rel'.
      constructor.
      unfold Int256.cmpu, Int256.ltu, Coqlib.zlt.
      rewrite 2 unsigned_repr_eq; try assumption.
      destruct (Z_lt_dec f f') as [ dec_lt | dec_ge ],
               (f <? f') eqn:ltb_eq.
      + reflexivity.
      + exfalso.
        rewrite (proj2 (Z.ltb_lt _ _) dec_lt) in ltb_eq; discriminate.
      + contradict dec_ge.
        apply Z.ltb_lt, ltb_eq.
      + reflexivity.
  Qed.
  Instance int_Z_bounded_lt_passthrough
      : HyperBinaryPassthrough Olt tint_Z_bounded tint_Z_bounded tint_bool.
  (* Proof. esplit.
    simpl; congruence.
  Qed. *)

  Instance int_Z_bounded_gt_impl
      : HyperBinaryImpl Ogt tint_Z_bounded tint_Z_bounded tint_bool := {
    Hbinary_cond f f' := True;
    Hbinary_ocond := otrue2;
    Hbinary := Z.gtb
  }.
  Instance int_Z_bounded_gt
      : HyperBinaryOp Ogt tint_Z_bounded tint_Z_bounded tint_bool := {
  }.
  Proof.
    - (* Hbinary_ocond_same *)
      reflexivity.

    - (* Hbinary_returns *)
      intros; exact I.

    - (* Hbinary_correct *)
      intros f f' v v' fc fc' oc rel rel'.
      simpl in rel, rel' |- *.
      rewrite <- rel, <- rel'.
      constructor.
      unfold Int256.cmpu, Int256.ltu, Coqlib.zlt.
      rewrite 2 unsigned_repr_eq; try assumption.
      destruct (Z_lt_dec f' f) as [ dec_lt | dec_ge ],
               (f >? f') eqn:ltb_eq.
      + reflexivity.
      + exfalso.
        rewrite Z.gtb_ltb, (proj2 (Z.ltb_lt _ _) dec_lt) in ltb_eq; discriminate.
      + contradict dec_ge.
        rewrite Z.gtb_ltb in ltb_eq.
        apply Z.ltb_lt, ltb_eq.
      + reflexivity.
  Qed.
  Instance int_Z_bounded_gt_passthrough
      : HyperBinaryPassthrough Ogt tint_Z_bounded tint_Z_bounded tint_bool.
  (* Proof. esplit.
    simpl; congruence.
  Qed. *)

  Instance int_Z_bounded_le_impl
      : HyperBinaryImpl Ole tint_Z_bounded tint_Z_bounded tint_bool := {
    Hbinary_cond f f' := True;
    Hbinary_ocond := otrue2;
    Hbinary := Z.leb
  }.
  Instance int_Z_bounded_le
      : HyperBinaryOp Ole tint_Z_bounded tint_Z_bounded tint_bool := {
  }.
  Proof.
    - (* Hbinary_ocond_same *)
      reflexivity.

    - (* Hbinary_returns *)
      intros; exact I.

    - (* Hbinary_correct *)
      intros f f' v v' fc fc' oc rel rel'.
      simpl in rel, rel' |- *.
      rewrite <- rel, <- rel'.
      constructor.
      unfold Int256.cmpu, Int256.ltu, Coqlib.zlt.
      rewrite 2 unsigned_repr_eq; try assumption.
      destruct (Z_lt_dec f' f) as [ dec_lt | dec_ge ],
               (f <=? f') eqn:ltb_eq.
      + exfalso.
        rewrite Z.leb_antisym, (proj2 (Z.ltb_lt _ _) dec_lt) in ltb_eq;
          discriminate.
      + reflexivity.
      + reflexivity.
      + contradict dec_ge.
        apply (f_equal negb) in ltb_eq.
        rewrite Z.leb_antisym, negb_involutive in ltb_eq.
        apply Z.ltb_lt, ltb_eq.
  Qed.
  Instance int_Z_bounded_le_passthrough
      : HyperBinaryPassthrough Ole tint_Z_bounded tint_Z_bounded tint_bool.
  (* Proof. esplit.
    simpl; congruence.
  Qed. *)

  Instance int_Z_bounded_ge_impl
      : HyperBinaryImpl Oge tint_Z_bounded tint_Z_bounded tint_bool := {
    Hbinary_cond f f' := True;
    Hbinary_ocond := otrue2;
    Hbinary := Z.geb
  }.
  Instance int_Z_bounded_ge
      : HyperBinaryOp Oge tint_Z_bounded tint_Z_bounded tint_bool := {
  }.
  Proof.
    - (* Hbinary_ocond_same *)
      reflexivity.

    - (* Hbinary_returns *)
      intros; exact I.

    - (* Hbinary_correct *)
      intros f f' v v' fc fc' oc rel rel'.
      simpl in rel, rel' |- *.
      rewrite <- rel, <- rel'.
      constructor.
      unfold Int256.cmpu, Int256.ltu, Coqlib.zlt.
      rewrite 2 unsigned_repr_eq; try assumption.
      destruct (Z_lt_dec f f') as [ dec_lt | dec_ge ],
               (f >=? f') eqn:geb_eq.
      + exfalso.
        apply Z.geb_le, Z.le_ge in geb_eq.
        exact (geb_eq dec_lt).
      + reflexivity.
      + reflexivity.
      + contradict dec_ge.
        rewrite Z.geb_leb in geb_eq.
        apply Z.leb_gt, geb_eq.
  Qed.
  Instance int_Z_bounded_ge_passthrough
      : HyperBinaryPassthrough Oge tint_Z_bounded tint_Z_bounded tint_bool.
  (* Proof. esplit.
    simpl; congruence.
  Qed. *)

End BOUNDED.

  Global Instance modulus_bound : IntegerBound Int256.modulus := {
    integer_bound_within_modulus := Z.le_ge _ _ (Zle_refl Int256.modulus);
    integer_bound_positive := Z.gt_lt _ _ (Int256.modulus_pos)
  }.

  Definition tint_Z32 := tint_Z_bounded Int256.modulus.
  Typeclasses Transparent tint_Z32 tint_Z_bounded.

  Definition int_Z32_impl := int_Z_impl Int256.modulus.
  Definition int_Z32 := int_Z Int256.modulus.
  (* Definition int_Z32_by_value := int_Z_by_value Int256.modulus.* *)
  Definition int_Z32_pair := @mk_hyper_type_pair tint_Z32 int_Z32_impl (*int_Z32*).
  (*Definition int_Z32_self_cast := int_Z_self_cast Int256.modulus.
  Definition int_Z32_arg_ret := int_Z_arg_ret. *)
  Existing Instance int_Z32_impl.

  Lemma int_Z32_ty_cond z
      : ht_ty_cond tint_Z32 (CVval (HighValues.Vint (Int256.repr z))).
  Proof.
    unfold ht_ty_cond, int_Z32_impl, int_Z_impl.
    exists (Int256.Z_mod_modulus z).
    split.
    - (* ht_ft_cond (Int256.Z_mod_modulus z) *)
      split; [| apply Z.lt_gt ]; apply Int256.Z_mod_modulus_range'.
    - (* ht_rel (Int256.Z_mod_modulus z) (Vint (Int256.repr z)) *)
      simpl; do 2 f_equal.
      rewrite Int256.Z_mod_modulus_eq.
      apply Int256.eqm_samerepr, Int256.eqmod_sym, Int256.eqmod_mod, Int256.modulus_pos.
  Qed.

  Definition int_Z32_add_impl := int_Z_bounded_add_impl Int256.modulus.
  Definition int_Z32_sub_impl := int_Z_bounded_sub_impl Int256.modulus.
  Definition int_Z32_mul_impl := int_Z_bounded_mul_impl Int256.modulus.
  Definition int_Z32_mod_impl := int_Z_bounded_mod_impl Int256.modulus.
  Definition int_Z32_div_impl := int_Z_bounded_div_impl Int256.modulus.

  Definition int_Z32_add := int_Z_bounded_add Int256.modulus.
  Definition int_Z32_sub := int_Z_bounded_sub Int256.modulus.
  Definition int_Z32_mul := int_Z_bounded_mul Int256.modulus.
  Definition int_Z32_mod := int_Z_bounded_mod Int256.modulus.
  Definition int_Z32_div := int_Z_bounded_div Int256.modulus.

  Definition int_Z32_add_passthrough := int_Z_bounded_add_passthrough Int256.modulus.
  Definition int_Z32_sub_passthrough := int_Z_bounded_sub_passthrough Int256.modulus.
  Definition int_Z32_mul_passthrough := int_Z_bounded_mul_passthrough Int256.modulus.
  Definition int_Z32_mod_passthrough := int_Z_bounded_mod_passthrough Int256.modulus.
  Definition int_Z32_div_passthrough := int_Z_bounded_div_passthrough Int256.modulus.  
  
  Definition int_Z32_eq_impl := int_Z_bounded_eq_impl Int256.modulus.
  Definition int_Z32_ne_impl := int_Z_bounded_ne_impl Int256.modulus.
  Definition int_Z32_lt_impl := int_Z_bounded_lt_impl Int256.modulus.
  Definition int_Z32_gt_impl := int_Z_bounded_gt_impl Int256.modulus.
  Definition int_Z32_le_impl := int_Z_bounded_le_impl Int256.modulus.
  Definition int_Z32_ge_impl := int_Z_bounded_ge_impl Int256.modulus.

  Definition int_Z32_eq := int_Z_bounded_eq Int256.modulus.
  Definition int_Z32_ne := int_Z_bounded_ne Int256.modulus.
  Definition int_Z32_lt := int_Z_bounded_lt Int256.modulus.
  Definition int_Z32_gt := int_Z_bounded_gt Int256.modulus.
  Definition int_Z32_le := int_Z_bounded_le Int256.modulus.
  Definition int_Z32_ge := int_Z_bounded_ge Int256.modulus.

  Definition int_Z32_eq_passthrough := int_Z_bounded_eq_passthrough Int256.modulus.
  Definition int_Z32_ne_passthrough := int_Z_bounded_ne_passthrough Int256.modulus.
  Definition int_Z32_lt_passthrough := int_Z_bounded_lt_passthrough Int256.modulus.
  Definition int_Z32_gt_passthrough := int_Z_bounded_gt_passthrough Int256.modulus.
  Definition int_Z32_le_passthrough := int_Z_bounded_le_passthrough Int256.modulus.
  Definition int_Z32_ge_passthrough := int_Z_bounded_ge_passthrough Int256.modulus.

End HYPER_TYPE_INT.

Notation Z32 := (Z_bounded Int256.modulus).

Existing Instances int_Z32_impl int_Z32 (*int_Z32_by_value int_Z32_arg_ret*).
Existing Instances int_Z32_add_impl int_Z32_sub_impl int_Z32_mul_impl
                   int_Z32_mod_impl int_Z32_div_impl.
Existing Instances int_Z32_add int_Z32_sub int_Z32_mul int_Z32_mod int_Z32_div.
Existing Instances int_Z32_add_passthrough int_Z32_sub_passthrough
                   int_Z32_mul_passthrough int_Z32_mod_passthrough
                   int_Z32_div_passthrough.
Existing Instances int_Z32_eq_impl int_Z32_ne_impl int_Z32_lt_impl
                   int_Z32_gt_impl int_Z32_le_impl int_Z32_ge_impl.
Existing Instances int_Z32_eq int_Z32_ne int_Z32_lt int_Z32_gt
                   int_Z32_le int_Z32_ge.
Existing Instances int_Z32_eq_passthrough int_Z32_ne_passthrough
                   int_Z32_lt_passthrough int_Z32_gt_passthrough
                   int_Z32_le_passthrough int_Z32_ge_passthrough.

Require Import Omega.

Section HYPER_TYPE_UNSIGNED.
  (* Local Open Scope Z_scope. *)

  Definition tint_U := Tpair int256 tint.

  Instance int_U_iso_impl : HyperTypeIsoImpl tint_U :=
    {
      ht_iso_ft_cond f := True;
      ht_iso_ty_cond v := match v with
                            | Vint i => Int256.modulus > Int256.unsigned i
                            | _ => False
                          end;
      ht_iso_default := Int256.zero;
      ht_implement f := (Vint f);
      ht_spec v := match v with
                     | Vint i => i
                     | _ => Int256.zero
                   end
    }.

  Instance int_U_iso : HyperTypeIso tint_U.
  Proof.
    esplit.
    - (* ht_implement_returns *)
      intros f fc. simpl in *.
      destruct f.
      simpl. omega.
    - (* ht_spec_returns *)
      intros v vc; simpl in *.
      destruct v; try contradiction.
      split; auto.
    - (* ht_iso_default_ft_cond *)
      unfold ht_iso_ft_cond. simpl; trivial.
    - (* ht_proof_left *)
      intros f fc; reflexivity.
    - (* ht_proof_right *)
      intros [] vc; simpl in *; try contradiction.
      reflexivity.
  Qed.

  Instance int_U_impl : HyperTypeImpl tint_U := _.
  Instance int_U : HyperType tint_U := _.

  (**[int_U_add]******************************************)
  Instance int_U_add_impl : HyperBinaryImpl Oadd tint_U tint_U tint_U :=
    {
      Hbinary_cond f f' := True;
      Hbinary_ocond := otrue2;
      Hbinary x y := Int256.add x y
    }.

  Instance int_U_add : HyperBinaryOp Oadd tint_U tint_U tint_U :=
    {
    }.
  Proof.
    - (* Hbinary_ocond_same *)
      reflexivity.
    - (* Hbinary_returns *)
      intros f f' fc fc' oc.
      split; omega.
    - (* Hbinary_correct *)
      intros f f' v v' fc fc' oc rel rel'.
      simpl in *. subst v v'.
      constructor.
      reflexivity.
  Qed.
  Instance int_U_add_passthrough : HyperBinaryPassthrough Oadd tint_U tint_U tint_U.
  (* Proof.
    esplit.
    simpl; congruence.
  Qed. *)

  (**[int_U_sub]******************************************)
  Instance int_U_sub_impl : HyperBinaryImpl Osub tint_U tint_U tint_U :=
    {
      Hbinary_cond f f' := True;
      Hbinary_ocond := otrue2;
      Hbinary x y := Int256.sub x y
    }.

  Instance int_U_sub : HyperBinaryOp Osub tint_U tint_U tint_U :=
    {
    }.
  Proof.
    - (* Hbinary_ocond_same *)
      reflexivity.
    - (* Hbinary_returns *)
      intros f f' fc fc' oc.
      split; omega.
    - (* Hbinary_correct *)
      intros f f' v v' fc fc' oc rel rel'.
      simpl in *.
      subst v v'.
      constructor. reflexivity.
  Qed.
  Instance int_U_sub_passthrough : HyperBinaryPassthrough Osub tint_U tint_U tint_U.
  (* Proof.
    esplit.
    simpl; congruence.
  Qed. *)


  (**[int_U_mul]******************************************)
  Instance int_U_mul_impl : HyperBinaryImpl Omul tint_U tint_U tint_U :=
    {
      Hbinary_cond f f' := True;
      Hbinary_ocond := otrue2;
      Hbinary x y := Int256.mul x y
    }.

  Instance int_U_mul : HyperBinaryOp Omul tint_U tint_U tint_U :=
    {
    }.
  Proof.
    - (* Hbinary_ocond_same *)
      reflexivity.
    - (* Hbinary_returns *)
      intros f f' fc fc' oc.
      split; simpl; omega.
    - (* Hbinary_correct *)
      intros f f' v v' fc fc' oc rel rel'.
      simpl in *.
      subst v v'.
      constructor.
      reflexivity.
  Qed.
  Instance int_U_mul_passthrough : HyperBinaryPassthrough Omul tint_U tint_U tint_U.
  (* Proof.
    esplit.
    simpl; congruence.
  Qed. *)

  (**[int_U_mod]******************************************)
  Instance int_U_mod_impl : HyperBinaryImpl Omod tint_U tint_U tint_U :=
    {
      Hbinary_cond f f' := f' <> Int256.zero;
      Hbinary_ocond := oprop2 (fun f f' => f' <> Int256.zero);
      Hbinary := Int256.modu
    }.
    
  Instance int_U_mod : HyperBinaryOp Omod tint_U tint_U tint_U :=
    {
    }.
  Proof.
    - (* Hbinary_ocond_same *)
      reflexivity.
    - (* Hbinary_returns *)
      intros f f' fc fc' oc.
      simpl in *.
      split; omega.
    - (* Hbinary_correct *)
      intros f f' v v' fc fc' oc rel rel'.
      subst v v'. simpl in *.
      constructor.
      reflexivity.
  Qed.
  Instance int_U_mod_passthrough : HyperBinaryPassthrough Omod tint_U tint_U tint_U.
  (* Proof.
    esplit.
    simpl; congruence.
  Qed. *)

  (**[int_U_div]******************************************)
  Instance int_U_div_impl : HyperBinaryImpl Odiv tint_U tint_U tint_U :=
    {
      Hbinary_cond f f' := f' <> Int256.zero;
      Hbinary_ocond := oprop2 (fun f f' => f' <> Int256.zero);
      Hbinary := Int256.divu
    }.
  Instance int_U_div : HyperBinaryOp Odiv tint_U tint_U tint_U :=
    {
    }.
  Proof.
    - (* Hbinary_ocond_same *)
      reflexivity.
    - (* Hbinary_returns *)
      intros f f' fc fc' oc.
      simpl in *.
      split; omega.
    - (* Hbinary_correct *)
      intros f f' v v' fc fc' oc rel rel'.
      subst v v'; simpl in *.
      constructor. reflexivity.
  Qed.
  Instance int_U_div_passthrough : HyperBinaryPassthrough Odiv tint_U tint_U tint_U.
  (* Proof.
    esplit.
    simpl; congruence.
  Qed. *)

  (**[int_U_eq]******************************************)
  Instance int_U_eq_impl : HyperBinaryImpl Oeq tint_U tint_U tint_bool :=
    {
      Hbinary_cond f f' := True;
      Hbinary_ocond := otrue2;
      Hbinary := Int256.cmpu Ceq
    }.

  Opaque val_eq_dec.
  
  Instance int_U_eq : HyperBinaryOp Oeq tint_U tint_U tint_bool :=
    {
    }.
  Proof.
    - (* Hbinary_ocond_same *)
      reflexivity.
    - (* Hbinary_returns *)
      trivial.
    - (* Hbinary_correct *)
      intros f f' v v' fc fc' oc rel rel'.
      subst v v'; simpl in *.
      constructor; simpl.
      assert (eqspec := Int256.eq_spec f f').
      destruct (val_eq_dec (Vint f) (Vint f'));
        destruct (Int256.eq f f');
        simpl in *;
        try reflexivity;
        congruence.
  Qed.
  Instance int_U_eq_passthrough : HyperBinaryPassthrough Oeq tint_U tint_U tint_bool .
  (* Proof.
    esplit.
    simpl; congruence.
  Qed. *)

  (**[int_U_ne]******************************************)
  Instance int_U_ne_impl : HyperBinaryImpl One tint_U tint_U tint_bool :=
    {
      Hbinary_cond f f' := True;
      Hbinary_ocond := otrue2;
      Hbinary := Int256.cmpu Cne
    }.

  Instance int_U_ne : HyperBinaryOp One tint_U tint_U tint_bool :=
    {
    }.
  Proof.
    - (* Hbinary_ocond_same *)
      reflexivity.
    - (* Hbinary_returns *)
      trivial.
    - (* Hbinary_correct *)
      intros f f' v v' fc fc' oc rel rel'.
      subst v v'; simpl in *.
      constructor; simpl.
      assert (eqspec := Int256.eq_spec f f').
      destruct (val_eq_dec (Vint f) (Vint f'));
        destruct (Int256.eq f f');
        simpl in *;
        try reflexivity;
        congruence.
  Qed.
  Instance int_U_ne_passthrough : HyperBinaryPassthrough One tint_U tint_U tint_bool.
  (* Proof.
    esplit.
    simpl; congruence.
  Qed. *)
  

  (**[int_U_lt]******************************************)
  Instance int_U_lt_impl : HyperBinaryImpl Olt tint_U tint_U tint_bool :=
    {
      Hbinary_cond f f' := True;
      Hbinary_ocond := otrue2;
      Hbinary := Int256.cmpu Clt
    }.

  Instance int_U_lt : HyperBinaryOp Olt tint_U tint_U tint_bool :=
    {
    }.
  Proof.
    - (* Hbinary_ocond_same *)
      reflexivity.
    - (* Hbinary_returns *)
      trivial.
    - (* Hbinary_correct *)
      intros f f' v v' fc fc' oc rel rel'.
      subst v v'.
      constructor; simpl in *.
      destruct (Int256.ltu f f'); reflexivity.
  Qed.
  Instance int_U_lt_passthrough : HyperBinaryPassthrough Olt tint_U tint_U tint_bool.
  (* Proof.
    esplit.
    simpl; congruence.
  Qed. *)

  (**[int_U_gt]******************************************)
  Instance int_U_gt_impl : HyperBinaryImpl Ogt tint_U tint_U tint_bool :=
    {
      Hbinary_cond f f' := True;
      Hbinary_ocond := otrue2;
      Hbinary := Int256.cmpu Cgt
    }.

  Instance int_U_gt : HyperBinaryOp Ogt tint_U tint_U tint_bool :=
    {
    }.
  Proof.
    - (* Hbinary_ocond_same *)
      reflexivity.
    - (* Hbinary_returns *)
      trivial.
    - (* Hbinary_correct *)
      intros f f' v v' fc fc' oc rel rel'.
      subst v v'.
      constructor; simpl in *.
      destruct (Int256.ltu f' f); reflexivity.
  Qed.
  Instance int_U_gt_passthrough : HyperBinaryPassthrough Ogt tint_U tint_U tint_bool.
  (* Proof.
    esplit.
    simpl; congruence.
  Qed. *)

  (**[int_U_le]******************************************)
  Instance int_U_le_impl : HyperBinaryImpl Ole tint_U tint_U tint_bool :=
    {
      Hbinary_cond f f' := True;
      Hbinary_ocond := otrue2;
      Hbinary := Int256.cmpu Cle
    }.

  Instance int_U_le : HyperBinaryOp Ole tint_U tint_U tint_bool :=
    {
    }.
  Proof.
    - (* Hbinary_ocond_same *)
      reflexivity.
    - (* Hbinary_returns *)
      trivial.
    - (* Hbinary_correct *)
      intros f f' v v' fc fc' oc rel rel'.
      subst v v'.
      constructor; simpl in *.
      destruct (Int256.ltu f' f); reflexivity.
  Qed.
  Instance int_U_le_passthrough : HyperBinaryPassthrough Ole tint_U tint_U tint_bool.
  (* Proof.
    esplit.
    simpl; congruence.
  Qed. *)

  (**[int_U_ge]******************************************)
  Instance int_U_ge_impl : HyperBinaryImpl Oge tint_U tint_U tint_bool :=
    {
      Hbinary_cond f f' := True;
      Hbinary_ocond := otrue2;
      Hbinary := Int256.cmpu Cge
    }.

  Instance int_U_ge : HyperBinaryOp Oge tint_U tint_U tint_bool :=
    {
    }.
  Proof.
    - (* Hbinary_ocond_same *)
      reflexivity.
    - (* Hbinary_returns *)
      trivial.
    - (* Hbinary_correct *)
      intros f f' v v' fc fc' oc rel rel'.
      subst v v'.
      constructor; simpl in *.
      destruct (Int256.ltu f f'); reflexivity.
  Qed.
  Instance int_U_ge_passthrough : HyperBinaryPassthrough Oge tint_U tint_U tint_bool.
  (* Proof.
    esplit.
    simpl; congruence.
  Qed. *)

  (**[int_U_notint]******************************************)
  Instance int_U_notint_impl : HyperUnaryImpl Onotint tint_U tint_U :=
    {
      Hunary_cond ft := True;
      Hunary_ocond := otrue1;
      Hunary := Int256.not
    }.

  Instance int_U_notint : HyperUnaryOp Onotint tint_U tint_U.
  Proof.
    esplit.
    - (* Hunary_ocond_same *)
      reflexivity.
    - (* Hunary_returns *)
      trivial.
    - (* Hunary_correct *)
      simpl.
      intros f v fc oc rel.
      subst v. constructor. reflexivity.
  Qed.
  Instance int_U_notint_passthrough : HyperUnaryPassthrough Onotint tint_U tint_U.
  (* Proof.
    esplit.
    simpl; congruence.
  Qed. *)

  (**[int_U_and]******************************************)
  Instance int_U_and_impl : HyperBinaryImpl Oand tint_U tint_U tint_U :=
    {
      Hbinary_cond f f' := True;
      Hbinary_ocond := otrue2;
      Hbinary := Int256.and
    }.

  Instance int_U_and : HyperBinaryOp Oand tint_U tint_U tint_U :=
    {
    }.
  Proof.
    - (* Hbinary_ocond_same *)
      reflexivity.
    - (* Hbinary_returns *)
      trivial.
    - (* Hbinary_correct *)
      intros f f' v v' fc fc' oc rel rel'.
      simpl in *. subst v v'.
      constructor. reflexivity.
  Qed.
  Instance int_U_and_passthrough : HyperBinaryPassthrough Oand tint_U tint_U tint_U.
  (* Proof.
    esplit.
    simpl; congruence.
  Qed. *)

  
  (**[int_U_or]******************************************)
  Instance int_U_or_impl : HyperBinaryImpl Oor tint_U tint_U tint_U :=
    {
      Hbinary_cond f f' := True;
      Hbinary_ocond := otrue2;
      Hbinary := Int256.or
    }.
  Instance int_U_or : HyperBinaryOp Oor tint_U tint_U tint_U :=
    {
    }.
  Proof.
    - (* Hbinary_ocond_same *)
      reflexivity.
    - (* Hbinary_returns *)
      trivial.
    - (* Hbinary_correct *)
      intros f f' v v' fc fc' oc rel rel'.
      simpl in *. subst v v'.
      constructor. reflexivity.
  Qed.
  Instance int_U_or_passthrough : HyperBinaryPassthrough Oor tint_U tint_U tint_U.
  (* Proof.
    esplit.
    simpl; congruence.
  Qed. *)

  (**[int_U_shl]******************************************)
  Instance int_U_shl_impl : HyperBinaryImpl Oshl tint_U tint_Z32 tint_U :=
    {
      Hbinary_cond f f' := f' < Int256.zwordsize;
      Hbinary_ocond := oprop2 (fun f f' => f' < Int256.zwordsize);
      Hbinary x y := Int256.shl x (Int256.repr y)
    }.
  Lemma lt_zwordsize_lt_iwordsize :
    forall f,
      -1 < f < Int256.zwordsize ->
      Int256.ltu (Int256.repr f) Int256.iwordsize = true.
  Proof.
    intros.
    unfold Int256.ltu, zlt.
    unfold Int256.iwordsize.
    assert (Int256.wordsize = 256%nat) by reflexivity.
    assert (Int256.zwordsize = 256) by reflexivity.
    rewrite H1 in *.
    repeat rewrite (unsigned_repr_eq _ _).
    - destruct (Z_lt_dec f 256); auto.
      omega.
    - split.
      + omega.
      + unfold Int256.modulus.
        rewrite H0.
        rewrite two_power_nat_equiv.
        reflexivity.
    - unfold Int256.modulus.
      rewrite two_power_nat_equiv.
      rewrite H0.
      split.
      + omega.
      + cut (2 ^ Z.of_nat 256 > 256).
        * omega.
        * reflexivity.
  Qed.
  
  Instance int_U_shl : HyperBinaryOp Oshl tint_U tint_Z32 tint_U :=
    {
    }.
  Proof.
    - (* Hbinary_ocond_same *)
      reflexivity.
    - (* Hbinary_returns *)
      trivial.
    - (* Hbinary_correct *)
      intros f f' v v' fc fc' oc rel rel'.
      subst v v'; simpl in *.
      constructor.
      reflexivity.
  Qed.
  Instance int_U_shl_passthrough : HyperBinaryPassthrough Oshl tint_U tint_Z32 tint_U.
  (* Proof.
    esplit.
    simpl; congruence.
  Qed. *)

  (**[int_U_shr]******************************************)
  Instance int_U_shr_impl : HyperBinaryImpl Oshr tint_U tint_Z32 tint_U :=
    {
      Hbinary_cond f f' := f' < Int256.zwordsize;
      Hbinary_ocond := oprop2 (fun f f' => f' < Int256.zwordsize);
      Hbinary x y := Int256.shru x (Int256.repr y)
    }.
  Instance int_U_shr : HyperBinaryOp Oshr tint_U tint_Z32 tint_U :=
    {
    }.
  Proof.
    - (* Hbinary_ocond_same *)
      reflexivity.
    - (* Hbinary_returns *)
      trivial.
    - (* Hbinary_correct *)
      intros f f' v v' fc fc' oc rel rel'.
      subst v v'; simpl in *.
      constructor.
      reflexivity.
  Qed.
  Instance int_U_shr_passthrough : HyperBinaryPassthrough Oshr tint_U tint_Z32 tint_U.
  (* Proof.
    esplit.
    simpl; congruence.
  Qed. *)


  (**[int_U_xor]******************************************)
  Instance int_U_xor_impl : HyperBinaryImpl Oxor tint_U tint_U tint_U :=
    {
      Hbinary_cond f f' := True;
      Hbinary_ocond := otrue2;
      Hbinary := Int256.xor
    }.
  Instance int_U_xor : HyperBinaryOp Oxor tint_U tint_U tint_U :=
    {
    }.
  Proof.
    - (* Hbinary_ocond_same *)
      reflexivity.
    - (* Hbinary_returns *)
      trivial.
    - (* Hbinary_correct *)
      intros f f' v v' fc fc' oc rel rel'.
      simpl in *. subst v v'.
      constructor; simpl.
      reflexivity.
  Qed.
  Instance int_U_xor_passthrough : HyperBinaryPassthrough Oxor tint_U tint_U tint_U.
  (* Proof.
    esplit.
    simpl; congruence.
  Qed. *)
  
  Typeclasses Transparent tint_U.
  Definition int_U_pair := @mk_hyper_type_pair tint_U int_U_impl.
  Existing Instance int_U_impl.

  Lemma int_U_ty_cond z :
    ht_ty_cond tint_U (CVval (HighValues.Vint z)).
  Proof.
    unfold ht_ty_cond, int_U_impl.
    exists z.
    split.
    - split.
    - simpl; do 2 f_equal.
  Qed.

(*  Definition tint_U_naturally_aligned : naturally_aligned (unpair_ty tint_U) := (eq_refl _). *)

End HYPER_TYPE_UNSIGNED.



Existing Instances int_U_impl int_U (* int_U_by_value int_U_arg_ret*).
Existing Instances
         int_U_add_impl int_U_sub_impl int_U_mul_impl
         int_U_mod_impl int_U_div_impl.
Existing Instances
         int_U_add int_U_sub
         int_U_mul int_U_mod int_U_div.
Existing Instances
         int_U_add_passthrough int_U_sub_passthrough
         int_U_mul_passthrough int_U_mod_passthrough int_U_div_passthrough.
Existing Instances
         int_U_eq_impl int_U_ne_impl
         int_U_lt_impl int_U_gt_impl int_U_le_impl int_U_ge_impl.
Existing Instances
         int_U_eq int_U_ne
         int_U_lt int_U_gt int_U_le int_U_ge.
Existing Instances
         int_U_eq_passthrough int_U_ne_passthrough
         int_U_lt_passthrough int_U_gt_passthrough
         int_U_le_passthrough int_U_ge_passthrough.
Existing Instances
         int_U_notint_impl int_U_and_impl int_U_or_impl
         int_U_shl_impl int_U_shr_impl int_U_xor_impl.
Existing Instances
         int_U_notint int_U_and int_U_or
         int_U_shl int_U_shr int_U_xor.
Existing Instances
         int_U_notint_passthrough int_U_and_passthrough int_U_or_passthrough
         int_U_shl_passthrough int_U_shr_passthrough int_U_xor_passthrough.

Require Import backend.MachineModel.

Section WITH_DATA.

Context `{LayerSpec : LayerSpecClass}.

Local Arguments HyperBuiltin0 {_} {tp} {hti} (HyperBuiltin0Impl0).
Local Arguments HyperBuiltin1 {_} {arg_tp} {tp} {arg_hti} {hti} (HyperBuiltin1Impl0).

Instance builtin0_address_impl : HyperBuiltin0Impl tint_U
  := Build_HyperBuiltin0Impl tint_U me_address Baddress.
Instance builtin0_address : HyperBuiltin0 builtin0_address_impl.
constructor; reflexivity.
Qed.

Instance builtin0_origin_impl : HyperBuiltin0Impl tint_U
  := Build_HyperBuiltin0Impl tint_U me_origin Borigin.
Instance builtin0_origin : HyperBuiltin0 builtin0_origin_impl.
constructor; reflexivity.
Qed.

Instance builtin0_caller_impl : HyperBuiltin0Impl tint_U
  := Build_HyperBuiltin0Impl tint_U me_caller Bcaller.
Instance builtin0_caller : HyperBuiltin0 builtin0_caller_impl.
constructor; reflexivity.
Qed.

Instance builtin0_callvalue_impl : HyperBuiltin0Impl tint_U
  := Build_HyperBuiltin0Impl tint_U me_callvalue Bcallvalue.
Instance builtin0_callvalue : HyperBuiltin0 builtin0_callvalue_impl.
constructor; reflexivity.
Qed.

Instance builtin0_coinbase_impl : HyperBuiltin0Impl tint_U
  := Build_HyperBuiltin0Impl tint_U me_coinbase Bcoinbase.
Instance builtin0_coinbase : HyperBuiltin0 builtin0_coinbase_impl.
constructor; reflexivity.
Qed.

Instance builtin0_timestamp_impl : HyperBuiltin0Impl tint_U
  := Build_HyperBuiltin0Impl tint_U me_timestamp Btimestamp.
Instance builtin0_timestamp : HyperBuiltin0 builtin0_timestamp_impl.
constructor; reflexivity.
Qed.

Instance builtin0_number_impl : HyperBuiltin0Impl tint_U
  := Build_HyperBuiltin0Impl tint_U me_number Bnumber.
Instance builtin0_number : HyperBuiltin0 builtin0_number_impl.
constructor; reflexivity.
Qed.

Instance builtin1_balance_impl : HyperBuiltin1Impl tint_U tint_U
  := Build_HyperBuiltin1Impl tint_U tint_U me_balance Bbalance.
Instance builtin1_balance : HyperBuiltin1 builtin1_balance_impl.
constructor.
- (* HBuiltin1_return *)
  constructor.
- (* HBuiltin1_correct *)
  intros.
  simpl in H0.
  exists (Vint f).
  split.
  + symmetry. apply H0.
  + reflexivity.
Qed.

Instance builtin1_blockhash_impl : HyperBuiltin1Impl tint_U tint_U
  := Build_HyperBuiltin1Impl tint_U tint_U me_blockhash Bblockhash.
Instance builtin1_blockhash : HyperBuiltin1 builtin1_blockhash_impl.
constructor.
- (* HBuiltin1_return *)
  constructor.
- (* HBuiltin1_correct *)
  intros.
  simpl in H0.
  exists (Vint f).
  split.
  + symmetry. apply H0.
  + reflexivity.
Qed.

End WITH_DATA.

(* For the keccak-256 hashes, we represent them symbolically, as a
  tree of the hash operations that have been done. This corresponds 
  to an assumption that the hash functions are injective. *)

Require Import backend.SymbolicKeccak.

Inductive hashvalue : Set :=
| hashval_int256 : int256 -> hashvalue
| hashval_hash1 : hashvalue -> hashvalue
| hashval_hash2 : hashvalue -> hashvalue -> hashvalue.

Fixpoint val_of_hashvalue hv :=
  match hv with
  | hashval_int256 i => Vint i
  | hashval_hash1 hv1 => sha_1 (val_of_hashvalue hv1)
  | hashval_hash2 hv1 hv2 => sha_2 (val_of_hashvalue hv1) (val_of_hashvalue hv2)
  end.

Lemma val_of_hashvalue_injective : forall hv hv',
    val_of_hashvalue hv = val_of_hashvalue hv' ->
    hv = hv'.
Proof.
  induction hv; destruct hv'; simpl; intros;
  try match goal with 
  | [ H : Vint ?i = sha_1 ?v |- _] => pose (sha_1_range v i)
  | [ H : sha_1 ?v = Vint ?i |- _] => pose (sha_1_range v i)
  | [ H : Vint ?i = sha_2 ?v1 ?v2 |- _] => pose (sha_2_range v1 v2 i)
  | [ H : sha_2 ?v1 ?v2 = Vint ?i |- _] => pose (sha_2_range v1 v2 i)
  | [ H : sha_1 _ = sha_1 _ |- _ ] => pose (sha_1_injective _ _ H)
  | [ H : sha_2 _ _ = sha_2 _ _ |- _] => destruct (sha_2_injective _ _ _ _ H)
  | [ H : sha_1 ?u = sha_2 ?v1 ?v2 |- _] => pose (sha_1_sha_2_range v1 v2 u)
  | [ H :  sha_2 ?v1 ?v2 = sha_1 ?u |- _] => pose (sha_1_sha_2_range v1 v2 u)
      end;
  try congruence; f_equal; eauto.
Qed.
  
Definition tint_hashvalue := Tpair hashvalue tint.

Instance int_hashvalue_impl : HyperTypeImpl tint_hashvalue := {
  ht_cval hv := CVval (val_of_hashvalue hv);
  ht_default := hashval_int256 Int256.zero;
  ht_ft_cond hv := True;
  ht_valid_ft_cond hv := True;
  ht_valid_ft_ocond := otrue1;
  (* ht_inject i hv1 hv2 := hv1 = hv2 *)
}.

Instance int_hashvalue : HyperType tint_hashvalue.
 constructor.
 - intros hv _.   
   eexists. reflexivity.
 - simpl. tauto.
 - simpl. tauto.
Qed.

Definition int_hashvalue_pair :=
    @mk_hyper_type_pair tint_hashvalue int_hashvalue_impl.

Global Instance int_hashvalue_hash1_impl
  : HyperUnaryImpl Osha_1 tint_hashvalue tint_hashvalue := {
 Hunary_cond ft := True;
 Hunary_ocond := otrue1;
 Hunary := hashval_hash1
}.

Global Instance int_hashvalue_hash1 : HyperUnaryOp Osha_1 tint_hashvalue tint_hashvalue.
Proof.
  esplit.
  - (* Hunary_ocond_same *)
    reflexivity.
  - (* Hunary_returns. *)
    simpl.
    trivial.
  - (* Hunary_correct. *)
    intros f v fc oc rel.
    rewrite <- rel.
    simpl.
    apply ht_some_cval.
    simpl.
    reflexivity.
Qed.

Global Instance int_hashvalue_hash1_passthrough : HyperUnaryPassthrough  Osha_1 tint_hashvalue tint_hashvalue.
(* Proof.
  constructor.
  simpl.
  intros; congruence.
Qed. *)

Global Instance int_hashvalue_hash1_U_impl
  : HyperUnaryImpl Osha_1 tint_U tint_hashvalue := {
 Hunary_cond ft := True;
 Hunary_ocond := otrue1;
 Hunary f := (hashval_hash1 (hashval_int256 f))
}.
             
Global Instance int_hashvalue_hash1_U : HyperUnaryOp Osha_1 tint_U tint_hashvalue.
Proof.
  esplit.
  - (* Hunary_ocond_same *)
    reflexivity.
  - (* Hunary_returns. *)
    simpl.
    trivial.
  - (* Hunary_correct. *)
    intros f v fc oc rel.
    rewrite <- rel.
    simpl.
    apply ht_some_cval.
    simpl.
    reflexivity.
Qed.

Global Instance int_hashvalue_hash1_U_passthrough : HyperUnaryPassthrough Osha_1 tint_U tint_hashvalue.
(* Proof.
   constructor.
   simpl.
   intros; congruence.
Qed. *)

Global Instance int_hashvalue_hash2_impl :
  HyperBinaryImpl Osha_2 tint_hashvalue tint_hashvalue tint_hashvalue := {
    Hbinary_cond f f' := True;
    Hbinary_ocond := otrue2;
    Hbinary := hashval_hash2
  }.

  Global Instance int_hashvalue_hash2 
     : HyperBinaryOp Osha_2 tint_hashvalue tint_hashvalue tint_hashvalue := {
  }.
  Proof.
    - (* Hbinary_ocond_same *)
      reflexivity.

    - (* Hbinary_returns *)
      intros; simpl; tauto.

    - (* Hbinary_correct *)
      intros f f' v v' fc fc' oc rel rel'.
      simpl in rel, rel'; rewrite <- rel, <- rel'.
      simpl.
      apply ht_some_cval.
      simpl.
      reflexivity.
  Qed.
  
  Global Instance int_hashvalue_hash2_passthrough
     : HyperBinaryPassthrough Osha_2 tint_hashvalue tint_hashvalue tint_hashvalue := {
                                                                                    }.
  (* Proof.
    simpl.
    intros.
    congruence.
  Qed. *)


Global Instance int_hashvalue_hash2_U1impl :
  HyperBinaryImpl Osha_2 tint_U tint_hashvalue tint_hashvalue := {
    Hbinary_cond f f' := True;
    Hbinary_ocond := otrue2;
    Hbinary x y := hashval_hash2 (hashval_int256 x) y
  }.

  Global Instance int_hashvalue_hash2_U1 
     : HyperBinaryOp Osha_2 tint_U tint_hashvalue tint_hashvalue := {
  }.
  Proof.
    - (* Hbinary_ocond_same *)
      reflexivity.

    - (* Hbinary_returns *)
      intros; simpl; tauto.

    - (* Hbinary_correct *)
      intros f f' v v' fc fc' oc rel rel'.
      simpl in rel, rel'; rewrite <- rel, <- rel'.
      simpl.
      apply ht_some_cval.
      simpl.
      reflexivity.
  Qed.
  
  Global Instance int_hashvalue_hash2_U1_passthrough
     : HyperBinaryPassthrough Osha_2 tint_U tint_hashvalue tint_hashvalue := {
                                                                                    }.
  (* Proof.
    simpl.
    intros.
    congruence.
  Qed. *)


Global Instance int_hashvalue_hash2_U2_impl :
  HyperBinaryImpl Osha_2 tint_hashvalue tint_U tint_hashvalue := {
    Hbinary_cond f f' := True;
    Hbinary_ocond := otrue2;
    Hbinary x y := hashval_hash2 x (hashval_int256 y)
  }.

  Global Instance int_hashvalue_hash2_U2
     : HyperBinaryOp Osha_2 tint_hashvalue tint_U tint_hashvalue := {
  }.
  Proof.
    - (* Hbinary_ocond_same *)
      reflexivity.

    - (* Hbinary_returns *)
      intros; simpl; tauto.

    - (* Hbinary_correct *)
      intros f f' v v' fc fc' oc rel rel'.
      simpl in rel, rel'; rewrite <- rel, <- rel'.
      simpl.
      apply ht_some_cval.
      simpl.
      reflexivity.
  Qed.
  
  Global Instance int_hashvalue_hash2_U2_passthrough
     : HyperBinaryPassthrough Osha_2 tint_hashvalue tint_U tint_hashvalue := {
                                                                                    }.
  (* Proof.
    simpl.
    intros.
    congruence.
  Qed. *)

Global Instance int_hashvalue_hash2_U12_impl :
  HyperBinaryImpl Osha_2 tint_U tint_U tint_hashvalue := {
    Hbinary_cond f f' := True;
    Hbinary_ocond := otrue2;
    Hbinary x y := hashval_hash2 (hashval_int256 x) (hashval_int256 y)
  }.

  Global Instance int_hashvalue_hash2_U12
     : HyperBinaryOp Osha_2 tint_U tint_U tint_hashvalue := {
  }.
  Proof.
    - (* Hbinary_ocond_same *)
      reflexivity.

    - (* Hbinary_returns *)
      intros; simpl; tauto.

    - (* Hbinary_correct *)
      intros f f' v v' fc fc' oc rel rel'.
      simpl in rel, rel'; rewrite <- rel, <- rel'.
      simpl.
      apply ht_some_cval.
      simpl.
      reflexivity.
  Qed.
  
  Global Instance int_hashvalue_hash2_U12_passthrough
     : HyperBinaryPassthrough Osha_2 tint_U tint_U tint_hashvalue := {
                                                                                    }.
  (* Proof.
    simpl.
    intros.
    congruence.
  Qed. *)

  Definition hashvalue_eq_dec : forall (hv hv' : hashvalue), 
      {hv = hv'} + {hv <> hv'}.
    decide equality.
    apply Int256.eq_dec.
  Defined.
  
  Definition hashvalue_eqb (hv hv' : hashvalue) : bool :=
    if hashvalue_eq_dec hv hv' then true else false.    

  Global Instance hashvalue_bool_eq_impl
    : HyperBinaryImpl Oeq tint_hashvalue tint_hashvalue tint_bool := {
    Hbinary_cond f f' := True;
    Hbinary_ocond := otrue2;
    Hbinary f f' := hashvalue_eqb f f'
  }.

  Global Instance hashvalue_bool_eq
      : HyperBinaryOp Oeq tint_hashvalue tint_hashvalue tint_bool := {
  }.
  Proof.
    - (* Hbinary_ocond_same *)
      reflexivity.

    - (* Hbinary_returns *)
      intros; simpl; tauto.

    - (* Hbinary_correct *)
      intros f f' v v' fc fc' oc rel rel'.
      subst v v'; simpl in *.
      constructor; simpl.
      unfold hashvalue_eqb.
      destruct (hashvalue_eq_dec f f');
        destruct (val_eq_dec (val_of_hashvalue f) (val_of_hashvalue f'));
        try reflexivity.
      + subst.
        congruence.
      + contradict n.
        auto using val_of_hashvalue_injective.
  Qed.
  Global Instance hashvalue_bool_eq_passthrough
      : HyperBinaryPassthrough Oeq tint_hashvalue tint_hashvalue tint_bool.
  (* Proof. esplit.
    simpl; congruence.
  Qed. *)

  Global Instance hashvalue_bool_ne_impl
      : HyperBinaryImpl One tint_hashvalue tint_hashvalue tint_bool := {
    Hbinary_cond f f' := True;
    Hbinary_ocond := otrue2;
    Hbinary f f' := negb (hashvalue_eqb f f')
  }.
  Global Instance hashvalue_bool_ne
      : HyperBinaryOp One tint_hashvalue tint_hashvalue tint_bool := {
  }.
  Proof.
    - (* Hbinary_ocond_same *)
      reflexivity.

    - (* Hbinary_returns *)
      intros; simpl; tauto.

    - (* Hbinary_correct *)
      intros f f' v v' fc fc' oc rel rel'.
      subst v v'; simpl in *.
      constructor; simpl.
      unfold hashvalue_eqb.
      destruct (hashvalue_eq_dec f f');
        destruct (val_eq_dec (val_of_hashvalue f) (val_of_hashvalue f'));
        try reflexivity.
      + subst.
        congruence.
      + contradict n.
        auto using val_of_hashvalue_injective.
  Qed.
  Global Instance int_hashvalue_ne_passthrough
      : HyperBinaryPassthrough One tint_hashvalue tint_hashvalue tint_bool.
  (* Proof. esplit.
    simpl; congruence.
  Qed. *)
