Require Import backend.phase.MiniC.Language.
Require Import backend.phase.MiniC.BigstepSemantics.
Require Import backend.phase.MiniC.Semantics.
Require Import backend.Values.
Require Import backend.MemoryModel.
Require Import core.MemoryModel.

Section STMT_FUNC.

Context `{HM : HyperMem}.
    
(* -----------------stuff that should go in core.SynthesisStmt  ------------------ *)

Require Import cclib.Maps.
Require Import core.HyperType.
Require Import DeepSpec.core.SEnv.
Require Import DeepSpec.core.Syntax.

 (* todo: move this: *)
  Definition empty_temp_env := PTree.empty val.

  Let env := empty_temp_env.

  (*Variable m0 : MemLow.*)
  (* --------------------- actual refinement proof ------------------ *)

Require Import DeepSpec.Runtime.
Require Import token.EdsgerIdents.
Require Import token.DataTypes.
Require Import token.DataTypeOps.
Require Import token.DataTypeProofs.
Require Import token.LayerFIXEDSUPPLYTOKEN.
Require Import token.Invariant.
Require Import token.ObjFixedSupplyTokenCodeProofs.
Require Import token.RefineFIXEDSUPPLYTOKEN.


Require Import lib.Monad.StateMonad.

Context (ge : genv) (me : MachineModel.machine_env).

(* Note, later we should fix the the (fst ml) by making the operational 
   semantics thread through an abstract datatype also. *)

(* Context`{LayerSpec : LayerSpecClass}.  (* TODO: this line will go away when we fix the type of HyperMem. *) *)
Set TypeClasses Debug.

(*Context {memModelOps : MemoryModelOps storage_env}. *)
Context`{CTXT_prf : !Layer_FIXEDSUPPLYTOKEN_Context_prf}.

Existing Instances RefineFIXEDSUPPLYTOKEN.memModelOps RefineFIXEDSUPPLYTOKEN.CTXT_prf FIXEDSUPPLYTOKEN_hypermem GlobalLayerSpec FIXEDSUPPLYTOKEN_overlay_spec FIXEDSUPPLYTOKEN_underlay_spec.

Require Import lib.Monad.RunStateTInv.


(* This should go in a generated file somewhere, 
   probably ObjFixedSupplyTokenCode.v. *)
Instance FixedSupplyToken_balances_var_hyperltype :
  HyperLType FixedSupplyToken_balances_var.
Proof.
  constructor.
  constructor.
  - (* ltype_get_match *)
    intros j d m mm dc; split.
    + apply mm.
    + apply mm.
  - (* ltype_set_match *)
    intros f j d m fc mm dc m' disjoint_eq same_d match_indirect.
    constructor.
    + destruct mm as [Hrelate _]. (* relate_AbData *)
      rewrite same_d in *.
      destruct Hrelate.
      constructor; auto.
    + constructor. (* match_AbData *)
      split.
      * exact match_indirect.
      * exact fc.
Qed.

(* This ad-hoc lemma will probably not be needed in the generic proofs. *)
Lemma match_indirect_basic: forall m ty i cv,
    is_immediate_type ty ->
    cval_match_indirect m ty i cv ->
    cval_match (IdentExtMap.get i m) cv.
Proof.
  intros m ty i cv H mm.
  inversion mm; subst; try (now inversion H).
  assumption.
Qed.

      
Theorem totalSupply_correctness :
(*    forall wf ge M, MakeProgram.make_globalenv GetLowDataX (M, GetLowLayer) = Errors.OK ge -> *)
    forall f d d',
    runStateT (FixedSupplyToken_totalSupply_opt me) d = Some (f, d') ->
    forall j ml (mcond : mem_match j d ml) lg,
    synth_func_cond FixedSupplyToken_totalSupply FixedSupplyToken_totalSupply_wf me d ->
    exists ml' lg' g' vres,
      mem_match j d' ml' /\
      ht_rel (tp:=tint_Z32) f vres /\
      eval_funcall  ge me (fst ml) lg
                    FixedSupplyToken_totalSupply_cfun (nil (*<-args*))
                    (fst ml') lg' g' vres.
Proof.
  intros f d d' Hrun j ml mcond lg Hvc.
  assert (mcond_copy := mcond).
  destruct mcond_copy as [? [[Hmatch_balances]]].
  Transparent  FixedSupplyToken_totalSupply_opt.
  unfold FixedSupplyToken_totalSupply_opt in Hrun.
  inv_runStateT.
  subst.

  unfold FixedSupplyToken_totalSupply_cfun.

(* We first want to build a lens corresponding to the hashtable itself, 
   and then to one particular dereferenced location in it.
      (LChash tint_Z32
        (LCvar FixedSupplyToken_balances_var)
        (ECconst_int256 tint_U (Int256.repr 0) (Int256.repr 0)))
*)
    pose (l_balance := inthash_ltype_pair FixedSupplyToken_balances_var (Int256.zero)).
g    pose (balance0 := l_balance.(ltype_get) d').

    assert (ht_ft_cond balance0 /\
            cval_match_indirect (fst ml) (unpair_ty tint_Z32)
                                (Ihash (Iident var_FixedSupplyToken_balances_ident) Int256.zero)
                                (ht_cval (balance0)))
           as H_balance_get.
    {
      assert (balance0_hyperltype : HyperLType (inthash_ltype_pair FixedSupplyToken_balances_var Int256.zero)).
      { eapply inthash_ltype.
        eapply FixedSupplyToken_balances_var_hyperltype.
        apply thash_int_HASH_Z_Z32_hash.  
      }

      destruct balance0_hyperltype as [balance0_hyperltype_dir].
      specialize (balance0_hyperltype_dir eq_refl).
      destruct (ltype_get_match j d' ml mcond) as [balance0_fc balance0_match].
      { simpl. exact I. }
      simpl in balance0_fc, balance0_match.
      split; [exact balance0_fc | exact balance0_match].
    }
    destruct H_balance_get as [balance0_fc balance0_match].

 (* Next, for the arithmetic operation we want to prove something like 
      exists v,
        (forall m, eval_expr ge env le m (synth_expr_expr tmp e) v) /\
        ht_rel (synth_expr_spec tmp e wf se) v
  *)
    assert (bin_cond : @Hbinary_cond Osub tint_Z32 tint_Z32 tint_Z32
                                     int_Z32_sub_impl _totalSupply balance0).
    { simpl.

      cbv -[Int256.modulus zeq zle zlt Z.iter Z.le Z.lt Z.gt Z.ge Z.eqb Z.leb Z.ltb Z.geb Z.gtb Z.mul Z.div Z.modulo Z.add Z.sub Z.shiftl Z.shiftr Z.lxor Z.land Z.lor Int256.add Int256.sub Int256.mul Int256.modu Int256.divu Int256.cmpu Int256.not Int256.and Int256.or Int256.xor Int256.shl Int256.shru Ziteri Z.of_nat List.length
             (*ret bind mzero get put gets guard modify runStateT evalStateT execStateT *)
             is_true bool_dec ZMap.get ZMap.set Int256Tree.get Int256Tree.set hlist_hd
             
             balances update_balances]
        in Hvc.
      destruct Hvc as [[_ Hvc] _].
      specialize (Hvc balance0 d').
      destruct Hvc as [_ Hvc].
      {
        reflexivity.
      }
      apply Hvc.
      auto.
    }

    apply match_indirect_basic in balance0_match; [|constructor].


    assert (fc_totalSupply : ht_ft_cond (tp:=tint_Z32) _totalSupply).
    {
      split; reflexivity.
    } 
    assert (bin_correct := Hbinary_correct _ _
                                           (ht_cval (tp:=tint_Z32) _totalSupply)
                                           (ht_cval (tp:=tint_Z32) balance0)
                                           fc_totalSupply balance0_fc
                                           bin_cond eq_refl eq_refl).
    unfold ht_cval_some in bin_correct.
    assert (bin_correct' :
              Some (ht_cval (tp:=tint_Z32) (Hbinary (HyperBinaryImpl:=int_Z32_sub_impl) (tpl:=tint_Z32) _totalSupply  balance0)) =
              (cval_binary_operation Osub (ht_cval (tp:=tint_Z32) _totalSupply)
                                     (unpair_ty tint_Z32) (ht_cval balance0) 
                                     (unpair_ty tint_Z32))).
    { remember _totalSupply as _totalSupply'.
      inversion bin_correct.
      simpl.
      f_equal.
      subst.
      exact H3.
    }
    clear bin_correct.
    symmetry in bin_correct'.

    assert (Hmatch_totalSupply : cval_match (Vint (Int256.repr _totalSupply)) (ht_cval (tp:=tint_Z32)  _totalSupply)).
    {
      apply CVMval.
    }
    eapply (cval_sem_binary_operation _ _ _ _ _ _ _ _  Hmatch_totalSupply balance0_match) in bin_correct'.
    destruct bin_correct' as [v [bin_correct_step bin_correct_match]].


  destruct (TempModel.function_initial_temps_defined 12%positive tint ftype FixedSupplyToken_totalSupply_cfun (@PTree.empty _ : temp_env)) as [v12 Hv12] ;[simpl; auto | simpl;tauto | ].

  destruct (TempModel.function_initial_temps_defined 10%positive tint ftype FixedSupplyToken_totalSupply_cfun (@PTree.empty _ : temp_env)) as [v10 Hv10] ;[simpl; auto | simpl;tauto | ].
                                            
  
  (* Yay, that's all the setup we need, now we can run the operational 
     semantics. *)
    
  eexists.
  eexists.
  eexists.
  eexists.
  split; [|split].
  - exact mcond.
  - (* The return value. *)
    exact bin_correct_match.
  - eapply eval_funcall_internal.
    + reflexivity.
    + match goal with
        [|- exec_stmt _ _ ?TEMPS _ _ _ _ _ _ _ _] =>
        set (the_temps := TEMPS)
      end.
      simpl.

     eapply exec_Sseq_1. 
     eapply exec_Sseq_1.
     eapply exec_Sset.
       { exact Hv12. }
     eapply eval_Elvalue.
     eapply eval_Ehashderef.
     eapply eval_Evar.
     eapply eval_Econst_int256.

     (* eapply exec_Sseq_1. *)
     eapply exec_Sset.
       { exact Hv10. }
     eapply eval_Ebinop.
     eapply eval_Econst_int256.
     eapply eval_Etempvar. rewrite PTree.gss. reflexivity.

     exact bin_correct_step.

     eapply  exec_Sreturn_some.
     eapply  eval_Etempvar.
     rewrite PTree.gss.
     reflexivity.
Qed.     

(*

Theorem transfer_correctness :
(*    forall wf ge M, MakeProgram.make_globalenv GetLowDataX (M, GetLowLayer) = Errors.OK ge -> *)
  forall toA toA_val tokens tokens_val f d d',
    (* These things come from "good_values" in SynthesisFunc.v *)
      ht_ft_cond (tp:=tint_U) toA      -> ht_rel (tp:=tint_U) toA toA_val ->
      ht_ft_cond (tp:=tint_Z32) tokens -> ht_rel (tp:=tint_Z32) tokens tokens_val ->  
    runStateT (FixedSupplyToken_transfer_opt toA tokens me) d = Some (f, d') ->
    forall j ml (mcond : mem_match j d ml) lg,
    synth_func_cond FixedSupplyToken_transfer FixedSupplyToken_transfer_wf toA tokens me d ->
    exists ml' lg' g' vres,
      mem_match j d' ml' /\
      ht_rel (tp:=tint_bool) f vres /\
      eval_funcall  ge me (fst ml) lg
                    FixedSupplyToken_transfer_cfun (toA_val :: tokens_val :: nil (*<-args*))
                    (fst ml') lg' g' vres.
Proof.
  intros toA toA_val tokens tokens_val f d d'
         Hft_cond_toA Hrel_toA Hft_cond_tokens Hrel_tokens
         Hrun j ml mcond lg Hvc.
  assert (mcond_copy := mcond).
  destruct mcond_copy as [? [[Hmatch_balances]]].
  Transparent  FixedSupplyToken_transfer_opt.
  unfold FixedSupplyToken_transfer_opt in Hrun.
  remember (Hquery0 me) as fromA.

  assert (fromA_rel : ht_cval fromA = (CVval (MachineModel.me_query me (MachineModel.Qcall0 Hbuiltin0)))).
  {
    subst.
    apply Hbuiltin_correct.
  }
  (* hide the HeqfromA equation from `subst`. *)
  change (fromA = Hquery0 me) with ((fun x=>x) (fromA = Hquery0 me)) in HeqfromA.
  
  inv_runStateT.    
  destruct (Integers.Int256.cmpu Cne fromA toA)%Z eqn:?.
  match goal with [H: runStateT (if ?X then _ else _) _ = _ |-_] =>
    destruct X eqn:? end.
  inv_runStateT;subst.
  (* There are three goals, corresponding to each path through the if-statements.
     The proof for each one is similar.
   *)
  {
    
  unfold FixedSupplyToken_transfer_cfun.  
  rename x into fromA.
  rename x4 into d.
  
    pose (l_balance_fromA := inthash_ltype_pair FixedSupplyToken_balances_var fromA).
    pose (balance_fromA := l_balance_fromA.(ltype_get) d).
    assert (ht_ft_cond balance_fromA /\
            cval_match_indirect (fst ml) (unpair_ty tint_Z32)
                                (Ihash (Iident var_FixedSupplyToken_balances_ident) fromA)
                                (ht_cval (balance_fromA)))
           as H_balance_get.
    {
      assert (balance_fromA_hyperltype : HyperLType (inthash_ltype_pair FixedSupplyToken_balances_var fromA)).
      { eapply inthash_ltype.
        eapply FixedSupplyToken_balances_var_hyperltype.
        apply thash_int_HASH_Z_Z32_hash.  
      }

      destruct balance_fromA_hyperltype as [balance_fromA_hyperltype_dir].
      specialize (balance_fromA_hyperltype_dir eq_refl).
      destruct (ltype_get_match j d ml mcond) as [balance_fromA_fc balance_fromA_match].
      { simpl. exact I. }
      simpl in balance_fromA_fc, balance_fromA_match.
      split; [exact balance_fromA_fc | exact balance_fromA_match].
    }
    destruct H_balance_get as [balance_fromA_fc balance_fromA_match].

    
(*    destruct (TempModel.function_initial_temps_defined2 ftype FixedSupplyToken_transfer_cfun (tokens_val :: toA_val :: nil) 11%positive tint
      (PTree.set 15 (Vint Int256.zero)          
       (PTree.set 14
          (Vint Int256.zero)             
          (PTree.set 13
             (Vint Int256.zero)
             (PTree.set 10
                (Vint Int256.zero)
                (PTree.set 11 tokens_val (PTree.set 12 toA_val (@PTree.empty val))))))))
      as [v11 Hv11];
      [simpl; auto | reflexivity | ]. *)
  
    destruct (TempModel.function_initial_temps_defined2 ftype FixedSupplyToken_transfer_cfun (toA_val :: tokens_val::nil) 11%positive tint
      (PTree.set 15 (Vint Int256.zero)          
       (PTree.set 14
          (Vint Int256.zero)             
          (PTree.set 13
             (Vint Int256.zero)
             (PTree.set 10
                (Vint Int256.zero)
                (PTree.set 11 toA_val (PTree.set 12 tokens_val (@PTree.empty val))))))))
      as [v11 Hv11];
      [simpl; auto | reflexivity | ].
    
  (* Fixme: this should not actually be PTree.empty, it should be init_args. *)    
    destruct (TempModel.function_initial_temps_defined 13%positive tint ftype FixedSupplyToken_transfer_cfun (@PTree.empty _ : temp_env)) as [v13 Hv13]; [simpl; tauto | simpl;intuition;congruence | ].
    destruct (TempModel.function_initial_temps_defined 14%positive tint ftype FixedSupplyToken_transfer_cfun (@PTree.empty _ : temp_env)) as [v14 Hv14]; [simpl; tauto | simpl;intuition;congruence | ].
    destruct (TempModel.function_initial_temps_defined 15%positive tint ftype FixedSupplyToken_transfer_cfun (@PTree.empty _ : temp_env)) as [v15 Hv15]; [simpl; tauto | simpl;intuition;congruence | ].
          
    assert (Hft_cond_fromA : ht_ft_cond (tp:=tint_U) fromA).
    {
      split; reflexivity.
    }
    assert (bin_correct_ne := Hbinary_correct (op := One) _ _
                                           (ht_cval (tp:=tint_U) fromA)
                                           (ht_cval (tp:=tint_U) toA)
                                           Hft_cond_fromA Hft_cond_toA
                                           I eq_refl eq_refl).
    unfold ht_cval_some in bin_correct_ne.
    assert (bin_correct_ne' :
              Some (ht_cval (tp:=tint_bool) (Hbinary (HyperBinaryImpl:=int_U_ne_impl) (tpl:=tint_U) fromA toA)) =
              (cval_binary_operation One (ht_cval (tp:=tint_U) fromA)
                                     (unpair_ty tint_U) (ht_cval toA) 
                                     (unpair_ty tint_bool))).
    { inversion bin_correct_ne.
      simpl.
      f_equal.
    }
    clear bin_correct_ne.
    symmetry in bin_correct_ne'.

    unfold tint_U in bin_correct_ne'.
    unfold addr in bin_correct_ne'.
    rewrite fromA_rel in bin_correct_ne'.

    assert (Hmatch_fromA : cval_match (Vint fromA) (CVval (MachineModel.me_query me (MachineModel.Qcall0 Hbuiltin0)))).
    {
      rewrite <- fromA_rel.
      apply CVMval.
    } 
    
    eapply (cval_sem_binary_operation _ _ _ _ _ _ _ _  Hmatch_fromA Hrel_toA) in bin_correct_ne'.
    destruct bin_correct_ne' as [v_ne [bin_correct_ne_step bin_correct_ne_match]].

    assert (bin_cond_ge : @Hbinary_cond Oge tint_Z32 tint_Z32 tint_bool
                                     int_Z32_ge_impl balance_fromA tokens)
     by exact I.    
    
    apply match_indirect_basic in balance_fromA_match; [|constructor].
    assert (bin_correct_ge := Hbinary_correct _ _
                                           (ht_cval (tp:=tint_Z32) balance_fromA)
                                           (ht_cval (tp:=tint_Z32) tokens)
                                           balance_fromA_fc
                                           Hft_cond_tokens 
                                           bin_cond_ge eq_refl eq_refl).
    unfold ht_cval_some in bin_correct_ge.
    assert (bin_correct_ge' :
              Some (ht_cval (tp:=tint_bool) (Hbinary (HyperBinaryImpl:=int_Z32_ge_impl) (tpl:=tint_Z32)  balance_fromA tokens )) =
              (cval_binary_operation Oge
                                     (ht_cval balance_fromA) (unpair_ty tint_Z32) 
                                     (ht_cval (tp:=tint_Z32) tokens)
                                     (unpair_ty tint_bool))).
    { 
      inversion bin_correct_ge.
      simpl.
      f_equal.
      subst.
      exact H3.
    }
    clear bin_correct_ge.
    symmetry in bin_correct_ge'.
    apply (cval_sem_binary_operation _ _ _ _ _ _ _ _   balance_fromA_match Hrel_tokens) in bin_correct_ge'.    
    destruct bin_correct_ge' as [v_ge [bin_correct_ge_step bin_correct_ge_match]].    

    pose (ne_result := Hbinary (HyperBinaryImpl:=int_U_ne_impl) fromA toA).
    pose (ge_result := Hbinary (HyperBinaryImpl:=int_Z32_ge_impl) balance_fromA tokens).
    
    assert (bin_cond_and : @Hbinary_cond Oand tint_bool tint_bool tint_bool
                                         int_bool_and_impl ne_result ge_result)
           by exact I.

    assert (bin_correct_and := Hbinary_correct (HyperBinaryImpl0:=int_bool_and_impl)
                                           _ _
                                           (ht_cval (tp:=tint_bool) ne_result)
                                           (ht_cval (tp:=tint_bool) ge_result)
                                           (Hbinary_returns _ _ Hft_cond_fromA Hft_cond_toA I)
                                           (Hbinary_returns _ _ balance_fromA_fc Hft_cond_tokens bin_cond_ge)
                                           bin_cond_and eq_refl eq_refl).
    unfold ht_cval_some in bin_correct_and.
    assert (bin_correct_and' :
              Some (ht_cval (tp:=tint_bool) (Hbinary (HyperBinaryImpl:=int_bool_and_impl) (tpl:=tint_bool)  ne_result ge_result )) =
              (cval_binary_operation Oand
                                     (ht_cval ne_result) (unpair_ty tint_bool) 
                                     (ht_cval (tp:=tint_bool) ge_result)
                                     (unpair_ty tint_bool))).
    { 
      inversion bin_correct_and.
      simpl.
      f_equal.
      subst.
      exact H3.
    }
    clear bin_correct_and.
    symmetry in bin_correct_and'.
    apply (cval_sem_binary_operation _ _ _ _ _ _ _ _   bin_correct_ne_match bin_correct_ge_match) in bin_correct_and'.    
    destruct bin_correct_and' as [v_and [bin_correct_and_step bin_correct_and_match]].    
    
    
    eexists.
    eexists.
    eexists.
    eexists.
    split; [|split].

    - admit. (* exact mcond *)
    - (* the return value *)
      unfold ht_rel.
      simpl.
      apply   CVMval.        
    - eapply eval_funcall_internal.
      + reflexivity.
      + match goal with
          [|- exec_stmt _ _ ?TEMPS _ _ _ _ _ _ _ _] =>
          set (the_temps := TEMPS)
        end.
        simpl.
    
     eapply exec_Sseq_1. 
     eapply exec_Sseq_1.
     eapply exec_Sset.
     { exact Hv13. }
     eapply eval_Ecall0.
      reflexivity.
    eapply exec_Sseq_1.
    eapply exec_Sset.
    { rewrite PTree.gso.
      exact Hv14.
      congruence.
    }
    eapply eval_Elvalue.
    eapply eval_Ehashderef.
    eapply eval_Evar.
    eapply eval_Etempvar.
    { unfold ht_cval in fromA_rel.
      simpl in fromA_rel.
      inversion fromA_rel as [fromA_rel'].
      symmetry.
      rewrite PTree.gss.
      f_equal.
      exact fromA_rel'.
    }
    eapply exec_Sseq_1.
    eapply exec_Sset.
    { repeat rewrite PTree.gso by congruence.
      exact Hv15.
    }

    eapply eval_Elvalue.
    eapply eval_Ehashderef.
    eapply eval_Evar.
    eapply eval_Etempvar.
    {
      repeat rewrite PTree.gso by congruence.
      unfold the_temps.
      (* stuck here  exact Hv11. *) admit.
    }

    (* We are in the "then" branch of the if statement. *)
    eapply exec_Sifthenelse.
    { eapply eval_Ebinop. (* and *)
      - eapply eval_Ebinop. (* <> *)
        + eapply eval_Etempvar.
          rewrite !PTree.gso by congruence.
          rewrite PTree.gss.
          reflexivity.
        + eapply eval_Etempvar.
          simpl in the_temps.
          unfold the_temps.
          simpl. 
          reflexivity.
        + match goal with [|- sem_binary_operation One ?X _ _ _ = _] =>
                    replace X with (Vint fromA)
                                     by (inversion fromA_rel as [fromA_rel_eq]; rewrite fromA_rel_eq; reflexivity)
          end.
          exact bin_correct_ne_step.
      - eapply eval_Ebinop. (* >= *)
        + eapply eval_Etempvar.
          rewrite !PTree.gso by congruence.
          rewrite PTree.gss.
          reflexivity.
        + eapply eval_Etempvar.
          simpl in the_temps.
          unfold the_temps.
          simpl. 
          reflexivity.
        + exact bin_correct_ge_step.
      - exact bin_correct_and_step.
*)


End STMT_FUNC.


