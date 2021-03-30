(* Skeleton by Edgser for token.ds *)
Require Import BinPos.
Require Import DeepSpec.Runtime.
Require Import DeepSpec.core.SynthesisFunc.
Require Import token.EdsgerIdents.
Require Import token.DataTypes.
Require Import token.DataTypeOps.
Require Import token.DataTypeProofs.
(*Require Import liblayers.compcertx.MakeProgram.
Require Import liblayers.compcertx.MemWithData.*)

Require Import token.LayerFIXEDSUPPLYTOKEN.
Require Import lib.ArithInv.
Require Import lib.Monad.RunStateTInv.
Require Import token.Invariant.
Transparent balances_sum balances_nonnegative.

Section EdsgerGen.

Existing Instance GlobalLayerSpec.
  
Existing Instances FIXEDSUPPLYTOKEN_overlay_spec
                   FIXEDSUPPLYTOKEN_data_ops
                   FIXEDSUPPLYTOKEN_data.

Context {memModelOps : MemoryModelOps mem}.
                     
Lemma FixedSupplyToken_constructor_vc me d :
    high_level_invariant d ->
    synth_func_cond FixedSupplyToken_constructor FixedSupplyToken_constructor_wf
                    me d.
Proof.
  intros.
  apply FixedSupplyToken_constructor_cond_eq; auto.
  unfold FixedSupplyToken_constructor_cond.
  intuition.
Qed.

Lemma FixedSupplyToken_constructor_oblg me d :
    high_level_invariant d ->
    synth_func_obligation FixedSupplyToken_constructor FixedSupplyToken_constructor_wf
                          me d.
Proof.
  intros.
  apply FixedSupplyToken_constructor_obligation_eq; auto.
  unfold FixedSupplyToken_constructor_obligation.
  intuition.
Qed.


Lemma FixedSupplyToken_totalSupply_vc me d :
    high_level_invariant d ->
    synth_func_cond FixedSupplyToken_totalSupply FixedSupplyToken_totalSupply_wf
                    me d.
Proof.
  intros.
  apply FixedSupplyToken_totalSupply_cond_eq; auto.
  unfold FixedSupplyToken_totalSupply_cond.
  intuition.
  inv_runStateT.
  subst.
  apply Zle_ge.  
  apply Int256Tree_Properties.sum_bound1.
  + intros.
    apply Zle_ge.
    unfold balances_nonnegative in H1.
    eauto.
  + unfold balances_sum in H2.
    omega.
Qed.

Lemma FixedSupplyToken_totalSupply_oblg me d :
    high_level_invariant d ->
    synth_func_obligation FixedSupplyToken_totalSupply FixedSupplyToken_totalSupply_wf
                          me d.
Proof.
  intros.
  apply FixedSupplyToken_totalSupply_obligation_eq; auto.
  unfold FixedSupplyToken_totalSupply_obligation.
  intuition.
Qed.

Lemma FixedSupplyToken_balanceOf_vc a0 me d :
    ht_ft_cond a0 -> ht_valid_ft_cond a0 ->
    high_level_invariant d ->
    synth_func_cond FixedSupplyToken_balanceOf FixedSupplyToken_balanceOf_wf
                    a0 me d.
Proof.
  intros.
  apply FixedSupplyToken_balanceOf_cond_eq; auto.
  unfold FixedSupplyToken_balanceOf_cond.
  intuition.
Qed.

Lemma FixedSupplyToken_balanceOf_oblg a0 me d :
    ht_ft_cond a0 -> ht_valid_ft_cond a0 ->
    high_level_invariant d ->
    synth_func_obligation FixedSupplyToken_balanceOf FixedSupplyToken_balanceOf_wf
                          a0 me d.
Proof.
  intros.
  apply FixedSupplyToken_balanceOf_obligation_eq; auto.
  unfold FixedSupplyToken_balanceOf_obligation.
  intuition.
Qed.


Lemma FixedSupplyToken_transfer_vc a0 a1 me d :
    ht_ft_cond a0 -> ht_valid_ft_cond a0 ->
    ht_ft_cond a1 -> ht_valid_ft_cond a1 ->
    high_level_invariant d ->
    synth_func_cond FixedSupplyToken_transfer FixedSupplyToken_transfer_wf
                    a0 a1 me d.
Proof.
  intros.
  apply FixedSupplyToken_transfer_cond_eq; auto.
  unfold FixedSupplyToken_transfer_cond.
  intuition.
  +
    inv_runStateT.    
    subst.
    inv_arith.
    auto.
  + inv_runStateT.
    subst.
    inv_arith.
    assert (100000 <  Int256.modulus) by reflexivity.

    assert (Hbound: Int256Tree_Properties.sum (balances g0) <= 100000).
    { unfold balances_sum in H11.
      omega.
    }

    assert (Hnonnegative : forall k v, Int256Tree.get k (balances g0) = Some v -> v >= 0).
    {
      unfold balances_nonnegative in H6.
      intros k v Hlookup.
      specialize (H6 k v Hlookup).
      omega.
    }

    assert (l := Int256Tree_Properties.sum_bound2 H8 Hnonnegative Hbound).    
    apply Zle_lt_trans with (Int256Tree.get_default 0 a0 (balances g0) + Int256Tree.get_default 0 (MachineModel.me_caller me) (balances g0)).
    omega.
    apply Zle_lt_trans with 100000.
    rewrite Zplus_comm.
    exact l.
    omega.
Qed.

Lemma FixedSupplyToken_transfer_oblg a0 a1 me d :
    ht_ft_cond a0 -> ht_valid_ft_cond a0 ->
    ht_ft_cond a1 -> ht_valid_ft_cond a1 ->
    high_level_invariant d ->
    synth_func_obligation FixedSupplyToken_transfer FixedSupplyToken_transfer_wf
                          a0 a1 me d.
Proof.
  intros.
  apply FixedSupplyToken_transfer_obligation_eq; auto.
  unfold FixedSupplyToken_transfer_obligation.
  intuition.
Qed.

Lemma FixedSupplyToken_approve_vc a0 a1 me d :
    ht_ft_cond a0 -> ht_valid_ft_cond a0 ->
    ht_ft_cond a1 -> ht_valid_ft_cond a1 ->
    high_level_invariant d ->
    synth_func_cond FixedSupplyToken_approve FixedSupplyToken_approve_wf
                    a0 a1 me d.
Proof.
  intros.
  apply FixedSupplyToken_approve_cond_eq; auto.
  unfold FixedSupplyToken_approve_cond.
  intuition.
Qed.

Lemma FixedSupplyToken_approve_oblg a0 a1 me d :
    ht_ft_cond a0 -> ht_valid_ft_cond a0 ->
    ht_ft_cond a1 -> ht_valid_ft_cond a1 ->
    high_level_invariant d ->
    synth_func_obligation FixedSupplyToken_approve FixedSupplyToken_approve_wf
                          a0 a1 me d.
Proof.
  intros.
  apply FixedSupplyToken_approve_obligation_eq; auto.
  unfold FixedSupplyToken_approve_obligation.
  intuition.
Qed.

Lemma FixedSupplyToken_transferFrom_vc a0 a1 a2 me d :
    ht_ft_cond a0 -> ht_valid_ft_cond a0 ->
    ht_ft_cond a1 -> ht_valid_ft_cond a1 ->
    ht_ft_cond a2 -> ht_valid_ft_cond a2 ->
    high_level_invariant d ->
    synth_func_cond FixedSupplyToken_transferFrom FixedSupplyToken_transferFrom_wf
                    a0 a1 a2 me d.
Proof.
  intros.
  apply FixedSupplyToken_transferFrom_cond_eq; auto.
  unfold FixedSupplyToken_transferFrom_cond.
  intuition.
  + inv_runStateT.
    subst.
    inv_arith.
    auto.
  + inv_runStateT.
    subst.
    unfold  Int256.cmpu in *.
    inv_arith.
    clear H17. (* temp. *)
    assert (100000 <  Int256.modulus) by reflexivity.

    assert (Hbound: Int256Tree_Properties.sum (balances g0) <= 100000).
    { unfold balances_sum in H15.
      omega.
    }

    assert (Hnonnegative : forall k v, Int256Tree.get k (balances g0) = Some v -> v >= 0).
    {
      unfold balances_nonnegative in H10.
      intros k v Hlookup.
      specialize (H10 k v Hlookup).
      omega.
    }
    assert (l := Int256Tree_Properties.sum_bound2 H12 Hnonnegative Hbound).


    apply Zle_lt_trans with (Int256Tree.get_default 0 a0 (balances g0) + Int256Tree.get_default 0 a1 (balances g0)).
    omega.
    apply Zle_lt_trans with 100000.
    exact l.
    omega.
Qed.

Lemma FixedSupplyToken_transferFrom_oblg a0 a1 a2 me d :
    ht_ft_cond a0 -> ht_valid_ft_cond a0 ->
    ht_ft_cond a1 -> ht_valid_ft_cond a1 ->
    ht_ft_cond a2 -> ht_valid_ft_cond a2 ->
    high_level_invariant d ->
    synth_func_obligation FixedSupplyToken_transferFrom FixedSupplyToken_transferFrom_wf
                          a0 a1 a2 me d.
Proof.
  intros.
  apply FixedSupplyToken_transferFrom_obligation_eq; auto.
  unfold FixedSupplyToken_transferFrom_obligation.
  intuition.
Qed.

Lemma FixedSupplyToken_constructor_vc me d :
    high_level_invariant d ->
    synth_func_cond FixedSupplyToken_constructor FixedSupplyToken_constructor_wf
                    me d.
Proof.
  admit.
Qed.

Lemma FixedSupplyToken_constructor_oblg me d :
    high_level_invariant d ->
    synth_func_obligation FixedSupplyToken_constructor FixedSupplyToken_constructor_wf
                          me d.
Proof.
  admit.
Qed.

Lemma FixedSupplyToken_totalSupply_vc me d :
    high_level_invariant d ->
    synth_func_cond FixedSupplyToken_totalSupply FixedSupplyToken_totalSupply_wf
                    me d.
Proof.
  admit.
Qed.

Lemma FixedSupplyToken_totalSupply_oblg me d :
    high_level_invariant d ->
    synth_func_obligation FixedSupplyToken_totalSupply FixedSupplyToken_totalSupply_wf
                          me d.
Proof.
  admit.
Qed.

Lemma FixedSupplyToken_balanceOf_vc a0 me d :
    ht_ft_cond a0 -> ht_valid_ft_cond a0 ->
    high_level_invariant d ->
    synth_func_cond FixedSupplyToken_balanceOf FixedSupplyToken_balanceOf_wf
                    a0 me d.
Proof.
  admit.
Qed.

Lemma FixedSupplyToken_balanceOf_oblg a0 me d :
    ht_ft_cond a0 -> ht_valid_ft_cond a0 ->
    high_level_invariant d ->
    synth_func_obligation FixedSupplyToken_balanceOf FixedSupplyToken_balanceOf_wf
                          a0 me d.
Proof.
  admit.
Qed.

Lemma FixedSupplyToken_transfer_vc a0 a1 me d :
    ht_ft_cond a0 -> ht_valid_ft_cond a0 ->
    ht_ft_cond a1 -> ht_valid_ft_cond a1 ->
    high_level_invariant d ->
    synth_func_cond FixedSupplyToken_transfer FixedSupplyToken_transfer_wf
                    a0 a1 me d.
Proof.
  admit.
Qed.

Lemma FixedSupplyToken_transfer_oblg a0 a1 me d :
    ht_ft_cond a0 -> ht_valid_ft_cond a0 ->
    ht_ft_cond a1 -> ht_valid_ft_cond a1 ->
    high_level_invariant d ->
    synth_func_obligation FixedSupplyToken_transfer FixedSupplyToken_transfer_wf
                          a0 a1 me d.
Proof.
  admit.
Qed.

Lemma FixedSupplyToken_approve_vc a0 a1 me d :
    ht_ft_cond a0 -> ht_valid_ft_cond a0 ->
    ht_ft_cond a1 -> ht_valid_ft_cond a1 ->
    high_level_invariant d ->
    synth_func_cond FixedSupplyToken_approve FixedSupplyToken_approve_wf
                    a0 a1 me d.
Proof.
  admit.
Qed.

Lemma FixedSupplyToken_approve_oblg a0 a1 me d :
    ht_ft_cond a0 -> ht_valid_ft_cond a0 ->
    ht_ft_cond a1 -> ht_valid_ft_cond a1 ->
    high_level_invariant d ->
    synth_func_obligation FixedSupplyToken_approve FixedSupplyToken_approve_wf
                          a0 a1 me d.
Proof.
  admit.
Qed.

Lemma FixedSupplyToken_transferFrom_vc a0 a1 a2 me d :
    ht_ft_cond a0 -> ht_valid_ft_cond a0 ->
    ht_ft_cond a1 -> ht_valid_ft_cond a1 ->
    ht_ft_cond a2 -> ht_valid_ft_cond a2 ->
    high_level_invariant d ->
    synth_func_cond FixedSupplyToken_transferFrom FixedSupplyToken_transferFrom_wf
                    a0 a1 a2 me d.
Proof.
  admit.
Qed.

Lemma FixedSupplyToken_transferFrom_oblg a0 a1 a2 me d :
    ht_ft_cond a0 -> ht_valid_ft_cond a0 ->
    ht_ft_cond a1 -> ht_valid_ft_cond a1 ->
    ht_ft_cond a2 -> ht_valid_ft_cond a2 ->
    high_level_invariant d ->
    synth_func_obligation FixedSupplyToken_transferFrom FixedSupplyToken_transferFrom_wf
                          a0 a1 a2 me d.
Proof.
  admit.
Qed.

End EdsgerGen.
