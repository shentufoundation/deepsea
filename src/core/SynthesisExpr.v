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

(** Synthesizing Clight expressions, functional expressions, and
    specifications from DeepSpec along with the proofs.  *)

(* Standard library modules *)
Require Import BinInt.

(* CompCert modules *)
Require Import backend.AST.
(*Require Import compcert.common.Events.
Require Import compcert.common.Globalenvs.  (* Genv *) *)
Require Import backend.Values.HighValues.
Require Import cclib.Integers.
Require Import cclib.Maps.  (* PTree *)
Require Import backend.Expressions.ExpMiniC.
Require Import backend.Expressions.SemanticsMiniC.
Require Import backend.Statements.StmtMiniC.
Require Import backend.Cop.
Require Import backend.Ctypes.
Require Import backend.MachineModel.

(* DeepSpec modules *)
Require Import DeepSpec.lib.SimpleMaps.
Require Import DeepSpec.lib.SimpleIndexedMaps.
Require Import DeepSpec.lib.OProp.
Require Import DeepSpec.lib.Monad.ContOptionMonad.
Require Import DeepSpec.core.Cval.
Require Import DeepSpec.core.SEnv.
Require Import DeepSpec.core.HyperType.
Require Import DeepSpec.core.HyperTypeInst.
(*Require Import DeepSpec.core.HyperMem.*)
Require Import DeepSpec.core.MemoryModel.
Require Import DeepSpec.core.Syntax.

Require Import DeepSpec.backend.phase.MiniC.Semantics.

(*Generalizable Variables tp tpo.  (* Enabling `{ } and `( ) *)*)

(* Prevent use_convert_proper from making setoid rewriting very slow. *)
(* Ltac liblayers.lib.LogicalRelations.use_convert_proper ::= fail. *)  (* Todo, liblayers-related *)

Section EXPR_FUNC.
  (* Context `{LayerSpec : LayerSpecClass}. *)
  Context`{HM : HyperMem}.
  Context {ctx: int256}.
  Context (me : machine_env GetHighData).

  Let genv_t := genv.
  (* Let env := PTree.empty _ . *)

  (* This is like an extra typecheck.
     If we tried to encode the entire type system for the typed intermediate langauge into the expr_constr
     datatype, Coq gets very slow.

     The main thing here is that we check that tempvars are well typed with respect to the typing environment tmp.  *)
  Fixpoint synth_expr_wellformed
      (tmp : AList.t hyper_type_pair)
     `{HyperTypeImpl}
      (e : expr_constr tp) {struct e} : Type :=
    match e with
    | ECconst_int _ _ f i => True
    | ECconst_int256 _ _ f i => True
    | ECtempvar _ _ i => (tmp ! i ~~~ Some (mk_hyper_type_pair tp))%alist
    | ECbuiltin0 _ _ _ => True
    | ECbuiltin1 _ _ _  _ _ e' => synth_expr_wellformed tmp e'
    | ECunop _ op _ tpo _ _ e' => synth_expr_wellformed tmp e'
    | ECbinop _ op _ tpl tpr _ _ _ el er =>
      synth_expr_wellformed tmp el * synth_expr_wellformed tmp er
    end%type

  with synth_lexpr_wellformed tmp `{HyperTypeImpl}(e : lexpr_constr tp) {struct e}: Type :=
    match e with
    | LCvar _ _ v => True
    | LCfield _ x _ _ _ pfieldi e' => synth_lexpr_wellformed tmp e'
    | LCindex _ _ _ _ _ e' idx =>
      synth_lexpr_wellformed tmp e' * synth_expr_wellformed tmp idx
    | LChash _ _ _ _ _  e' idx =>
      synth_lexpr_wellformed tmp e' * synth_expr_wellformed tmp idx
    end%type.

  (* the synth_expr_ functions generate C expressions. *)
  Fixpoint synth_expr_expr
      (tmp : AList.t hyper_type_pair)
     `{HyperTypeImpl}
      (e : expr_constr tp) : expr :=
    match e with
    | ECconst_int _ _ _ i => Econst_int i (unpair_ty tp)
    | ECconst_int256 _ _ _ i => Econst_int256 i (unpair_ty tp)
    | ECtempvar _ _ tid => Etempvar tid (unpair_ty tp)
    | ECbuiltin0 _ _ _ => Ecall0 Hbuiltin0 (unpair_ty tp)
    | ECbuiltin1 _ _ _ _ _ e' => Ecall1 Hbuiltin1 (synth_expr_expr tmp e') (unpair_ty tp)
    | ECunop _ op _ _ _ _ e' =>
      Eunop op (synth_expr_expr tmp e') (unpair_ty tp)
    | ECbinop _ op _ _ _ _ _ _ el er =>
      Ebinop op (synth_expr_expr tmp el) (synth_expr_expr tmp er) (unpair_ty tp)
    end.

  Fixpoint synth_lexpr_expr tmp `{HyperTypeImpl}(e : lexpr_constr tp) : expr :=
    match e with
    | LCvar _ _ v => Eglob (ident_ext_base v.(ltype_ident)) (unpair_ty tp)
    | LCfield _ x _ _ _ pfieldi e' =>
      Efield (synth_lexpr_expr tmp e') x (unpair_ty tp)
    | LCindex _ _ _ _ _ e' idx =>
      Eindex (synth_lexpr_expr tmp e') (synth_expr_expr tmp idx) (unpair_ty tp)
    | LChash _ _ _ _ _  e' idx =>
      Eindex (synth_lexpr_expr tmp e') (synth_expr_expr tmp idx) (unpair_ty tp)
      (* The hash becomes an index into a global array, though we should actually
         treat it like that in synthesis, too. *)
    end.

  (* the synth_ _spec  are the desugaring into a functional Gallina spec.
     rexpressions just return a value in the "functional" part of the type pair tp. *)
  Fixpoint synth_expr_spec
      (tmp : AList.t hyper_type_pair)
     `{HyperTypeImpl}
      (e : expr_constr tp)
      : synth_expr_wellformed tmp e -> spec_env_t tmp -> unpair_ft tp :=
    match e with
    | ECconst_int _ _ f _ => fun _ _ => f
    | ECconst_int256 _ _ f _ => fun _ _ => f
    | ECtempvar _ _ i => fun wf se => SpecTree.get_eq i se wf
    | ECbuiltin0 _ _ _ => fun wf se => Hquery0 me
    | ECbuiltin1 _ _ _ _ _ e' => fun wf se => Hquery1 me (synth_expr_spec tmp e' wf se)
    | ECunop _ op _ tpo _ _ e' =>
      fun wf se => Hunary (synth_expr_spec tmp e' wf se)
    | ECbinop _ op _ tpl tpr _ _ _ el er =>
      fun wf se => Hbinary (synth_expr_spec tmp el (car wf) se)
                           (synth_expr_spec tmp er (cdr wf) se)
    end.

  (* lexpressions are desugared into lenses. *)
  Fixpoint synth_lexpr_spec tmp `{HyperTypeImpl} (e : lexpr_constr tp)
      : synth_lexpr_wellformed tmp e -> spec_env_t tmp -> ltype_pair tp :=
    match e with
    | LCvar _ _ v => fun _ _ => v
    | LCfield _ tid _ _ _ pfieldi e' => fun wf se =>
      let ptr := synth_lexpr_spec tmp e' wf se in
      field_ltype_pair ptr tid
    | LCindex _ _ _ _ _ e_arr e_idx => fun wf se =>
      let ptr := synth_lexpr_spec tmp e_arr (car wf) se in
      indexing_ltype_pair ptr (synth_expr_spec tmp e_idx (cdr wf) se)
    | LChash _ _ _ _ _ e_arr e_idx => fun wf se =>
      let ptr := synth_lexpr_spec tmp e_arr (car wf) se in
      inthash_ltype_pair ptr (synth_expr_spec tmp e_idx (cdr wf) se)
    end.

  (* TODO: the expression synthesis is probably the only place that uses the OProp machinery,
     but actually it is probably not needed, and we could simplify the code by just using Props. *)

  (* This is the "synthesis safety condition". It will eventually be part of the verification condition
     to be proven in FooProof.v *)
  Fixpoint synth_expr_ocond
      (tmp : AList.t hyper_type_pair)
     `{HyperTypeImpl}
      (e : expr_constr tp)
      : synth_expr_wellformed tmp e -> OProp1 (spec_env_t tmp) :=
    match e with
    | ECconst_int _ _ f i => fun _ => otrue1
    | ECconst_int256 _ _ f i => fun _ => otrue1
    | ECtempvar _ _ t => fun _ => otrue1
    | ECbuiltin0 _ _ _ => fun _ => otrue1                                    
    | ECbuiltin1 _ _ _ _ _ e' => fun wf => synth_expr_ocond tmp e' wf
    | ECunop _ op _ _ _ _ e' => fun wf =>
      synth_expr_ocond tmp e' wf /\
      Hunary_ocond m{ synth_expr_spec tmp e' wf }
    | ECbinop _ op _ _ _ _ _ _ el er => fun wf =>
      synth_expr_ocond tmp el (car wf) /\
      synth_expr_ocond tmp er (cdr wf) /\
      Hbinary_ocond m{ synth_expr_spec tmp el (car wf) }
                    m{ synth_expr_spec tmp er (cdr wf) }
    end%oprop1

  with synth_lexpr_ocond tmp `{HyperTypeImpl}(e : lexpr_constr tp)
      : synth_lexpr_wellformed tmp e -> OProp1 (spec_env_t tmp) :=
    match e with
    | LCvar _ _ v => fun _ => otrue1
    | LCfield _ x _ _ _ pfieldi e' => fun wf =>
      synth_lexpr_ocond tmp e' wf
    | LCindex _ _ _ _ _ e_arr e_idx => fun wf =>
      synth_lexpr_ocond tmp e_arr (car wf) /\
      synth_expr_ocond tmp e_idx (cdr wf) /\
      {{ fun se => 0 <= synth_expr_spec tmp e_idx (cdr wf) se < Hindex_size }}%Z
    | LChash  _ _ _ _ _ e_arr e_idx => fun wf =>
      synth_lexpr_ocond tmp e_arr (car wf) /\
      synth_expr_ocond tmp e_idx (cdr wf)
    end%oprop1.

  (* The only passthrough-related condition that has to be proved manually by the programmer
     is for the respec construct. *)
  Fixpoint synth_expr_passthrough_ocond
      (tmp : AList.t hyper_type_pair)
     `{HyperTypeImpl}
      (e : expr_constr tp)
      : synth_expr_wellformed tmp e -> OProp :=
    match e with
    | ECconst_int _ _ f i => fun _ => otrue
    | ECconst_int256 _ _ f i => fun _ => otrue
    | ECtempvar _ _ t => fun _ => otrue
    | ECbuiltin0 _ _ _ => fun _ => otrue                                
    | ECbuiltin1 _ _ _ _ _ e' => fun wf => synth_expr_passthrough_ocond tmp e' wf
    | ECunop _ op _ _ _ _ e' => fun wf => synth_expr_passthrough_ocond tmp e' wf
    | ECbinop _ op _ _ _ _ _ _ el er => fun wf =>
      synth_expr_passthrough_ocond tmp el (car wf) /\
      synth_expr_passthrough_ocond tmp er (cdr wf)
    end%oprop

  with synth_lexpr_passthrough_ocond tmp `{HyperTypeImpl}(e : lexpr_constr tp)
      : synth_lexpr_wellformed tmp e -> OProp :=
    match e with
    | LCvar _ _ v => fun _ => otrue
    | LCfield _ x _ _ _ pfieldi e' => fun wf =>
      synth_lexpr_passthrough_ocond tmp e' wf
    | LCindex _ _ _ _ _ e_arr e_idx => fun wf =>
      synth_lexpr_passthrough_ocond tmp e_arr (car wf) /\
      synth_expr_passthrough_ocond tmp e_idx (cdr wf)
    | LChash  _ _ _ _ _ e_arr e_idx => fun wf =>
      synth_lexpr_passthrough_ocond tmp e_arr (car wf) /\
      synth_expr_passthrough_ocond tmp e_idx (cdr wf)
    end%oprop.

  (* Correctness proofs for expression synthesis! *)
  Lemma synth_expr_expr_preserves_type tmp `{HyperTypeImpl} e :
      synth_expr_wellformed tmp e ->
      typeof (synth_expr_expr tmp e) = unpair_ty tp.
      (* typeof takes a Clight expression and computes its type. *)
  Proof.
    generalize dependent tmp.
    induction e; try reflexivity.
  Qed.

  Lemma synth_lexpr_expr_preserves_type tmp `{HyperTypeImpl} e :
      (* synth_lexpr_wellformed tmp e -> *)
      typeof (synth_lexpr_expr tmp e) = unpair_ty tp.
  Proof.
    destruct e; reflexivity.
  Qed.

  (* Generally speaking, we tend to pass around just values of type ft, rather than
     a dependent type of the value and a proof, and then we prove the ft_cond separately. *)
  Theorem synth_expr_spec_satisfies_ft_cond {tmp}`{HyperType} e :
    forall wf se, expr_constr_prf e ->
      oProp1 (synth_expr_ocond tmp e wf) se -> senv_cond se ->
      ht_ft_cond (synth_expr_spec tmp e wf se).
  Proof.
    intros wf se ec c se_cond. induction ec; simpl in wf, c |- *.
    - (* ECconst_int *)
      apply fc.
    - (* ECtempvar *)
      apply se_cond.
    - (* ECbuiltin0 *)
      apply Hbuiltin0_returns.
    - (* ECbuiltin1 *)
      apply Hbuiltin1_returns.
      apply IHec. apply c.
    - (* ECunop *)
      rewrite oand1_distr, OProp1map1, <- Hunary_ocond_same in c; [| exact I ].
      apply Hunary_returns; [ eapply IHec |];
        solve [ (* apply le_cond | *) apply c ].
    - (* ECbinop *)
      rewrite 2 oand1_distr, OProp2map1, <- Hbinary_ocond_same in c; try exact I.
      apply Hbinary_returns; [ eapply IHec1 | eapply IHec2 |];
        solve [ (* apply le_cond | *) apply c ].
  Qed.

  (* Should move this lemma to Integers.v *)
  Lemma Int256_lt_repr : forall x y,
      (0 <= x < Int256.modulus)%Z ->
      (0 <= y < Int256.modulus)%Z ->
      (Int256.lt (Int256.repr x) (Int256.repr y) = true) <-> (x < y)%Z.
  Admitted.

Require Import Omega.  

  (* This is the main theorem, relating the C expression and the desugaring. *)
  Theorem synth_expr_spec_correct {tmp}`{HyperType} e :
    forall wf se le, expr_constr_prf e ->
      oProp1 (synth_expr_ocond tmp e wf) se -> lenv_cond se le ->
      exists v,
        (forall m, eval_rvalue ctx me m le (synth_expr_expr tmp e) v) /\
        ht_rel (synth_expr_spec tmp e wf se) v

  with synth_lexpr_spec_correct {tmp}`{HyperType} e :
    forall wf se le, lexpr_constr_prf e ->
      oProp1 (synth_lexpr_ocond tmp e wf) se -> lenv_cond se le ->
      let ltp := synth_lexpr_spec tmp e wf se in (*= (i, ofs) -> *)
        (forall m,
          eval_lvalue ctx me m le (synth_lexpr_expr tmp e) ltp.(ltype_ident)) /\
        HyperLType ltp.
  Proof.
  { (* synth_expr_spec_correct *)
    intros wf se le ec c le_cond (* gcond *); induction ec; simpl in * |- *.

    - (* ECconst_int256 *)
      exists (Vint i); split; [ constructor |].

      unfold ht_rel; rewrite rel.
      constructor; reflexivity.

    - (* ECtempvar *)
      assert (some_rel := proj2 (le_cond _ _ _ wf)).
      inversion some_rel as [| ? v rel f_eq le_t_eq ]; subst.
      exists v; split; [ constructor; symmetry |]; assumption.

    - (* ECbuiltin0 *)
      exists (me_query me (Qcall0 Hbuiltin0)).
      split.
      + intros m.
        apply eval_Ecall0.
        reflexivity.
      + unfold ht_rel.
        rewrite Hbuiltin0_correct.
        apply CVMval.

    - (* ECbuiltin1 *)
      destruct (IHec wf c) as (v & eval & rel).
      assert (p_cond := synth_expr_spec_satisfies_ft_cond e
                          wf _ ec c (lenv_senv_cond le_cond)).
      exists (me_query me (Qcall1 Hbuiltin1 v)).
      split.
      + intros m.
        apply eval_Ecall1 with v; only 1: apply eval.
        reflexivity.
      + unfold ht_rel.
        assert (builtin1_correct := Hbuiltin1_correct me _ _ p_cond eq_refl).
        destruct builtin1_correct as (v' & Harg & Hret).
        rewrite Hret.
        replace v with v' by (inversion rel; congruence).
        apply CVMval.

    - (* ECunop *)
      rewrite oand1_distr, OProp1map1, <- Hunary_ocond_same in c; [| exact I ].
      destruct (IHec wf (proj1 c)) as (v & eval & rel).
      assert (p_cond := synth_expr_spec_satisfies_ft_cond e
                          wf _ ec (proj1 c) (lenv_senv_cond le_cond)).
      assert (unary_correct := Hunary_correct _ _ p_cond (proj2 c) eq_refl).
      inversion unary_correct as [| ? ? ? f_eq cval_op_eq ]; subst.
      eapply eq_sym, cval_sem_unary_operation in cval_op_eq; [| exact rel ].
      destruct cval_op_eq as (v' & sem_eq & rel').

      exists v'; split; [| assumption ].
      intros m.
      eapply eval_Eunop with (v1:=v); [ apply eval |].
      apply sem_eq.

    - (* ECbinop *)
      rewrite 2 oand1_distr, OProp2map1, <- Hbinary_ocond_same in c; try exact I.
      destruct c as (c1 & c2 & binary_cond).
      destruct (IHec1 (car wf) c1) as (v1 & eval1 & rel1),
               (IHec2 (cdr wf) c2) as (v2 & eval2 & rel2).
      assert (p1_cond := synth_expr_spec_satisfies_ft_cond el (car wf) _ ec1 c1 (lenv_senv_cond le_cond)).
      assert (p2_cond := synth_expr_spec_satisfies_ft_cond er (cdr wf) _ ec2 c2 (lenv_senv_cond le_cond)).
      assert (bin_correct := Hbinary_correct _ _ _ _ p1_cond p2_cond
                                             binary_cond eq_refl eq_refl).
      inversion bin_correct as [| ? ? ? f_eq cval_op_eq ]; subst.
      eapply eq_sym, cval_sem_binary_operation in cval_op_eq;
        [| exact rel1 | exact rel2 ].
      destruct cval_op_eq as (v' & sem_eq & rel').

      exists v'; split; [| assumption ].
      intros m.
      eapply eval_Ebinop with _ _ v1 v2; [ apply eval1 | apply eval2 |].
      apply sem_eq.
  }
  
  { (* synth_lexpr_spec_ptr_correct *)
    intros wf se le ec c le_cond (* gcond *).
    induction ec; intros; simpl in * |- *.
    - (* LCvar *)
      unfold ltp.
      assert (v_ltype := v_prf.(VARltype)).
      assert (absorption := fun A B b f => @conj A B (f b) b);
        apply absorption; clear absorption.
      + (* HyperLtype *)
        exact v_ltype.
      + (* eval_lvalue *)
        intros hyper_ltype m.
        rewrite v_prf.(VARoffset_zero).
        simpl. 
        apply eval_Eglob.
        reflexivity.
    - (* LCfield *)
      destruct (IHec _ c) as (eval & struct_ltype).
      unfold ltp; split; [| eauto with typeclass_instances ].

      destruct Hfield_delta_correct
        as (tid & flds & tpl_eq (* & field_type_eq & field_offset_eq *)).
      intros m.
      eapply eval_Efield.
      + apply eval.
      + unfold ExtEnv.access_field.
        rewrite synth_lexpr_expr_preserves_type.
        destruct tpl_eq as [fList [tpl_eq1 tpl_eq2]].
        rewrite tpl_eq1.
        rewrite tpl_eq2.
        reflexivity.
    - (* LCindex *)
      rewrite 2 oand1_distr in c.
      destruct c as (arr_c & idx_c & idx_in_bound).
      destruct (IHec _ arr_c) as (array_eval & array_ltype).
      destruct (synth_expr_spec_correct _ _ _ _ _ _ _ _
                  H0 idx_c le_cond) as (index & index_eval & index_rel).

      unfold ltp; split; [| eauto with typeclass_instances ].
      intros m.
      eapply eval_Eindex.
      + (* eval_expr array *)
        simpl.
        apply array_eval.
      + inversion index_rel.
        subst index v.
        apply index_eval.
      + simpl.
        inversion index_rel.
        subst index v.
        unfold ExtEnv.index_array.
        rewrite (synth_lexpr_expr_preserves_type tmp e).
        rewrite Hindex_array_type.
        simpl in idx_in_bound.
        destruct idx_in_bound as [idx_in_bound1 idx_in_bound2].
        assert (0 <= synth_expr_spec tmp idx (cdr wf) se < Int256.modulus)%Z
           as idx_in_bound_modulus.
        {
          split.
          - exact idx_in_bound1.
          - transitivity Hindex_size.
            exact idx_in_bound2.
            destruct Hindex_size_bound; assumption.
        }
        match goal with [|- (if ?X then _ else _) = _] =>
                        replace X with true
          by (symmetry;
          apply (proj2 (Int256_lt_repr _ _ idx_in_bound_modulus Hindex_size_bound)  idx_in_bound2))
        end.
        reflexivity.
    - (* LChash *)
      rewrite oand1_distr in c.
      destruct c as (arr_c & idx_c).
      destruct (IHec _ arr_c) as (array_eval & array_ltype).
      destruct (synth_expr_spec_correct _ _ _ _ _ _ _ _
                  H0 idx_c le_cond) as (index & index_eval & index_rel).

      unfold ltp; split; [| eauto with typeclass_instances ].

      intros m.
      eapply eval_Eindex.
      + (* eval_hash *)
        simpl.
        apply array_eval.
      + inversion index_rel.
        subst index v.
        apply index_eval.
      + simpl.
        inversion index_rel.
        subst index v.
        unfold ExtEnv.index_array.
        rewrite (synth_lexpr_expr_preserves_type tmp e).
        rewrite Hhash_type.
        reflexivity.
  }
  Qed.

  Theorem synth_lexpr_spec_is_ghost_eq {tmp}`{HyperTypeImpl} e wf se :
    (synth_lexpr_spec tmp e wf se).(ltype_ghost) = lexpr_is_ghost e.
  Proof.
    induction e; solve [ reflexivity | apply IHe ].
  Qed.

  Lemma synth_lexpr_passthrough tmp `{HyperTypeImpl} e wf :
    lexpr_constr_passthrough_prf e ->
    oProp (synth_lexpr_passthrough_ocond tmp e wf) ->
    forall se, variable_passthrough_prf (synth_lexpr_spec tmp e wf se).
  Proof.
  Admitted.

End EXPR_FUNC.
