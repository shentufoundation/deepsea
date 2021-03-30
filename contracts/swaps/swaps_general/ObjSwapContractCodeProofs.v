(* Skeleton by Edgser for swaps_general.ds *)
Require Import BinPos.
Require Import DeepSpec.Runtime.
Require Import swaps_general.EdsgerIdents.
Require Import swaps_general.DataTypes.
Require Import swaps_general.DataTypeOps.
Require Import swaps_general.DataTypeProofs.
Require Import liblayers.compcertx.MakeProgram.
Require Import liblayers.compcertx.MemWithData.

Require Import swaps_general.LayerSWAPCONTRACT.

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
Existing Instances SWAPCONTRACT_overlay_spec.

Lemma SwapContract_initialize_spec_requires_kernel_mode a0 a1 a2 a3 d d' :
    SwapContract_initialize_spec a0 a1 a2 a3 d = Some d' ->
    kernel_mode d.
Proof.
  intros spec.
  unfold SwapContract_initialize_spec in spec.
  simpl.
  CommonTactic.subdestruct; auto.
Qed.

Lemma SwapContract_initialize_vc a0 a1 a2 a3 me d :
    ht_ft_cond a0 -> ht_valid_ft_cond a0 ->
    ht_ft_cond a1 -> ht_valid_ft_cond a1 ->
    ht_ft_cond a2 -> ht_valid_ft_cond a2 ->
    ht_ft_cond a3 -> ht_valid_ft_cond a3 ->
    high_level_invariant d ->
    synth_func_cond SwapContract_initialize SwapContract_initialize_wf
                    a0 a1 a2 a3 me d.
Proof.
  admit.
Qed.

Lemma SwapContract_initialize_oblg a0 a1 a2 a3 me d :
    ht_ft_cond a0 -> ht_valid_ft_cond a0 ->
    ht_ft_cond a1 -> ht_valid_ft_cond a1 ->
    ht_ft_cond a2 -> ht_valid_ft_cond a2 ->
    ht_ft_cond a3 -> ht_valid_ft_cond a3 ->
    high_level_invariant d ->
    synth_func_obligation SwapContract_initialize SwapContract_initialize_wf
                          a0 a1 a2 a3 me d.
Proof.
  admit.
Qed.

Lemma SwapContract_unlock_spec_requires_kernel_mode a0 a1 d d' :
    SwapContract_unlock_spec a0 a1 d = Some d' ->
    kernel_mode d.
Proof.
  intros spec.
  unfold SwapContract_unlock_spec in spec.
  simpl.
  CommonTactic.subdestruct; auto.
Qed.

Lemma SwapContract_unlock_vc a0 a1 me d :
    ht_ft_cond a0 -> ht_valid_ft_cond a0 ->
    ht_ft_cond a1 -> ht_valid_ft_cond a1 ->
    high_level_invariant d ->
    synth_func_cond SwapContract_unlock SwapContract_unlock_wf
                    a0 a1 me d.
Proof.
  admit.
Qed.

Lemma SwapContract_unlock_oblg a0 a1 me d :
    ht_ft_cond a0 -> ht_valid_ft_cond a0 ->
    ht_ft_cond a1 -> ht_valid_ft_cond a1 ->
    high_level_invariant d ->
    synth_func_obligation SwapContract_unlock SwapContract_unlock_wf
                          a0 a1 me d.
Proof.
  admit.
Qed.

Lemma SwapContract_claim_spec_requires_kernel_mode d d' :
    SwapContract_claim_spec d = Some d' ->
    kernel_mode d.
Proof.
  intros spec.
  unfold SwapContract_claim_spec in spec.
  simpl.
  CommonTactic.subdestruct; auto.
Qed.

Lemma SwapContract_claim_vc me d :
    high_level_invariant d ->
    synth_func_cond SwapContract_claim SwapContract_claim_wf
                    me d.
Proof.
  admit.
Qed.

Lemma SwapContract_claim_oblg me d :
    high_level_invariant d ->
    synth_func_obligation SwapContract_claim SwapContract_claim_wf
                          me d.
Proof.
  admit.
Qed.

Lemma SwapContract_refund_spec_requires_kernel_mode d d' :
    SwapContract_refund_spec d = Some d' ->
    kernel_mode d.
Proof.
  intros spec.
  unfold SwapContract_refund_spec in spec.
  simpl.
  CommonTactic.subdestruct; auto.
Qed.

Lemma SwapContract_refund_vc me d :
    high_level_invariant d ->
    synth_func_cond SwapContract_refund SwapContract_refund_wf
                    me d.
Proof.
  admit.
Qed.

Lemma SwapContract_refund_oblg me d :
    high_level_invariant d ->
    synth_func_obligation SwapContract_refund SwapContract_refund_wf
                          me d.
Proof.
  admit.
Qed.

End EdsgerGen.
