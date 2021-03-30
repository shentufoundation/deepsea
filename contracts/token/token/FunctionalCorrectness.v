Require Import token.DataTypeOps.
Require Import token.LayerFIXEDSUPPLYTOKEN.
Require Import token.Invariant.

Require Import DeepSpec.lib.Monad.StateMonadOption.
Require Import DeepSpec.lib.Monad.RunStateTInv.
Require Import lib.ArithInv.
Import DeepSpec.lib.Monad.Monad.MonadNotation.

Require Import ZArith.
Require Import cclib.Maps.
Require Import cclib.Integers.

Section WithMem.

Import core.MemoryModel.

(*
(* Todo: move this lemma to inv_arith. *)
Lemma cmpu_Cne_true : forall x y, Int256.cmpu Cne x y = true -> x<>y.
Proof.
  intros.
  unfold Int256.cmpu in H.
  rewrite Bool.negb_true_iff in H.
  apply Int256eq_false in H.
  assumption.
Qed. *)
  
(* We have now loaded the specification of the transfer method. *)
(* Print FixedSupplyToken_transfer_opt. *)

Transparent balances_sum.

Context {memModelOps : MemoryModelOps mem}.

(* We can now prove that the transfer method does not create or destroy tokens. *)
Theorem transfer_constant_balances_sum : forall toA n d d' me b,
    runStateT (FixedSupplyToken_transfer_opt toA n me) d = Some (b, d')
    -> balances_sum d' = balances_sum d.
Proof.
  intros.
  Transparent FixedSupplyToken_transfer_opt.
  unfold FixedSupplyToken_transfer_opt in H.
  inv_runStateT.
  subst.  
  inv_arith.
  unfold balances_sum.
  subst.
  autorewrite with updates.
  remember (balances m2) as m.
  rewrite Int256Tree_Properties.sum_swap by congruence.
  apply Int256Tree_Properties.constant_sum'.
    + reflexivity.
    + reflexivity.
    + congruence.
Qed.

(* And similar for the transferFrom method. (The proof is actually
   identical.) *)
Theorem transferFrom_constant_balances_sum : forall fromA toA n d d' me b,
    runStateT (FixedSupplyToken_transferFrom_opt fromA toA n me) d = Some (b, d')
    -> balances_sum d' = balances_sum d.
Proof.
  intros.
  Transparent FixedSupplyToken_transferFrom_opt.
  unfold FixedSupplyToken_transferFrom_opt in H.
  inv_runStateT.
  subst.  
  inv_arith.
  unfold balances_sum.
  subst.
  autorewrite with updates.
  remember (balances m2) as m.
  rewrite Int256Tree_Properties.sum_swap by congruence.
  apply Int256Tree_Properties.constant_sum'.
    + reflexivity.
    + reflexivity.
    + congruence.
Qed.

End WithMem.
