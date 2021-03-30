(* Skeleton by Edgser for spblind.ds *)
Require Import BinPos.
Require Import DeepSpec.Runtime.
Require Import spblind.EdsgerIdents.
Require Import spblind.DataTypes.
Require Import spblind.DataTypeOps.
Require Import spblind.DataTypeProofs.
Require Import liblayers.compcertx.MakeProgram.
Require Import liblayers.compcertx.MemWithData.

Require Import spblind.LayerBLINDAUCTION.

Section EdsgerGen.

Context {mem}`{Hmem: Mem.MemoryModel mem}.
Context`{Hmwd: UseMemWithData mem}.
Context`{make_program_ops: !MakeProgramOps Clight.function Ctypes.type Clight.fundef Ctypes.type}.
Context`{Hmake_program: !MakeProgram Clight.function Ctypes.type Clight.fundef Ctypes.type}.
Instance GlobalLayerSpec : LayerSpecClass := {
  make_program_ops := make_program_ops;
  Hmake_program := Hmake_program;
  GetHighData := global_abstract_data_type
}.
Context`{global_abdata : !GlobalAbData init_global_abstract_data global_low_level_invariant}.
Existing Instances BLINDAUCTION_overlay_spec.

Lemma BlindAuction_initialize_spec_requires_kernel_mode a0 a1 d d' :
    BlindAuction_initialize_spec a0 a1 d = Some d' ->
    kernel_mode d.
Proof.
  intros spec.
  unfold BlindAuction_initialize_spec in spec.
  simpl.
  CommonTactic.subdestruct; auto.
Qed.

Lemma BlindAuction_initialize_vc a0 a1 me d :
    ht_ft_cond a0 -> ht_valid_ft_cond a0 ->
    ht_ft_cond a1 -> ht_valid_ft_cond a1 ->
    high_level_invariant d ->
    synth_func_cond BlindAuction_initialize BlindAuction_initialize_wf
                    a0 a1 me d.
Proof.
  admit.
Qed.

Lemma BlindAuction_initialize_oblg a0 a1 me d :
    ht_ft_cond a0 -> ht_valid_ft_cond a0 ->
    ht_ft_cond a1 -> ht_valid_ft_cond a1 ->
    high_level_invariant d ->
    synth_func_obligation BlindAuction_initialize BlindAuction_initialize_wf
                          a0 a1 me d.
Proof.
  admit.
Qed.

Lemma BlindAuction_transferb_spec_requires_kernel_mode a0 a1 a2 d d' :
    BlindAuction_transferb_spec a0 a1 a2 d = Some d' ->
    kernel_mode d.
Proof.
  intros spec.
  unfold BlindAuction_transferb_spec in spec.
  simpl.
  CommonTactic.subdestruct; auto.
Qed.

Lemma BlindAuction_transferb_vc a0 a1 a2 me d :
    ht_ft_cond a0 -> ht_valid_ft_cond a0 ->
    ht_ft_cond a1 -> ht_valid_ft_cond a1 ->
    ht_ft_cond a2 -> ht_valid_ft_cond a2 ->
    high_level_invariant d ->
    synth_func_cond BlindAuction_transferb BlindAuction_transferb_wf
                    a0 a1 a2 me d.
Proof.
  admit.
Qed.

Lemma BlindAuction_transferb_oblg a0 a1 a2 me d :
    ht_ft_cond a0 -> ht_valid_ft_cond a0 ->
    ht_ft_cond a1 -> ht_valid_ft_cond a1 ->
    ht_ft_cond a2 -> ht_valid_ft_cond a2 ->
    high_level_invariant d ->
    synth_func_obligation BlindAuction_transferb BlindAuction_transferb_wf
                          a0 a1 a2 me d.
Proof.
  admit.
Qed.

Lemma BlindAuction_reveal_spec_requires_kernel_mode a0 a1 d d' :
    BlindAuction_reveal_spec a0 a1 d = Some d' ->
    kernel_mode d.
Proof.
  intros spec.
  unfold BlindAuction_reveal_spec in spec.
  simpl.
  CommonTactic.subdestruct; auto.
Qed.

Lemma BlindAuction_reveal_vc a0 a1 me d :
    ht_ft_cond a0 -> ht_valid_ft_cond a0 ->
    ht_ft_cond a1 -> ht_valid_ft_cond a1 ->
    high_level_invariant d ->
    synth_func_cond BlindAuction_reveal BlindAuction_reveal_wf
                    a0 a1 me d.
Proof.
  admit.
Qed.

Lemma BlindAuction_reveal_oblg a0 a1 me d :
    ht_ft_cond a0 -> ht_valid_ft_cond a0 ->
    ht_ft_cond a1 -> ht_valid_ft_cond a1 ->
    high_level_invariant d ->
    synth_func_obligation BlindAuction_reveal BlindAuction_reveal_wf
                          a0 a1 me d.
Proof.
  admit.
Qed.

Lemma BlindAuction_bid_spec_requires_kernel_mode a0 d d' :
    BlindAuction_bid_spec a0 d = Some d' ->
    kernel_mode d.
Proof.
  intros spec.
  unfold BlindAuction_bid_spec in spec.
  simpl.
  CommonTactic.subdestruct; auto.
Qed.

Lemma BlindAuction_bid_vc a0 me d :
    ht_ft_cond a0 -> ht_valid_ft_cond a0 ->
    high_level_invariant d ->
    synth_func_cond BlindAuction_bid BlindAuction_bid_wf
                    a0 me d.
Proof.
  admit.
Qed.

Lemma BlindAuction_bid_oblg a0 me d :
    ht_ft_cond a0 -> ht_valid_ft_cond a0 ->
    high_level_invariant d ->
    synth_func_obligation BlindAuction_bid BlindAuction_bid_wf
                          a0 me d.
Proof.
  admit.
Qed.

Lemma BlindAuction_withdraw_spec_requires_kernel_mode d d' :
    BlindAuction_withdraw_spec d = Some d' ->
    kernel_mode d.
Proof.
  intros spec.
  unfold BlindAuction_withdraw_spec in spec.
  simpl.
  CommonTactic.subdestruct; auto.
Qed.

Lemma BlindAuction_withdraw_vc me d :
    high_level_invariant d ->
    synth_func_cond BlindAuction_withdraw BlindAuction_withdraw_wf
                    me d.
Proof.
  admit.
Qed.

Lemma BlindAuction_withdraw_oblg me d :
    high_level_invariant d ->
    synth_func_obligation BlindAuction_withdraw BlindAuction_withdraw_wf
                          me d.
Proof.
  admit.
Qed.

Lemma BlindAuction_auctionEnd_spec_requires_kernel_mode d d' :
    BlindAuction_auctionEnd_spec d = Some d' ->
    kernel_mode d.
Proof.
  intros spec.
  unfold BlindAuction_auctionEnd_spec in spec.
  simpl.
  CommonTactic.subdestruct; auto.
Qed.

Lemma BlindAuction_auctionEnd_vc me d :
    high_level_invariant d ->
    synth_func_cond BlindAuction_auctionEnd BlindAuction_auctionEnd_wf
                    me d.
Proof.
  admit.
Qed.

Lemma BlindAuction_auctionEnd_oblg me d :
    high_level_invariant d ->
    synth_func_obligation BlindAuction_auctionEnd BlindAuction_auctionEnd_wf
                          me d.
Proof.
  admit.
Qed.

End EdsgerGen.
