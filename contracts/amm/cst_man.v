Add LoadPath "./amm" as amm.

(*From Coq Require Import ssreflect ssrfun ssrbool.
Set Implicit Arguments.
Unset Strict Implicit.
Unset Printing Implicit Defensive.*)
Require Import Reals.
Require Import Lra.
Require Import Lia.
Require Import Interval.Tactic.
Require Import Interval.Real.Taylor.
Require Import Interval.Poly.Datatypes.
Require Import Interval.Poly.Taylor_poly.
Require Import Interval.Poly.Taylor_model_sharp.
Require Import Interval.Interval.Interval_compl.
Require Import DeepSpec.lib.Monad.Monad.
Require Import amm.LayerAMM.
Require Import BinPos.
Require Import DeepSpec.Runtime.
Require Import amm.EdsgerIdents.
Require Import amm.DataTypes.
Require Import amm.DataTypeOps.
Require Import amm.DataTypeProofs.
Require Import DeepSpec.lib.Monad.Monad.
Require Import DeepSpec.lib.Monad.MonadState.
Require Import DeepSpec.lib.Monad.StateMonad.
Require Import DeepSpec.lib.Monad.OptionMonad.
Require Import DeepSpec.lib.Monad.MonadZero.
Require Import DeepSpec.core.SynthesisStmt.
Require Import DeepSpec.core.SynthesisFunc.
Require Import backend.MachineModel.
Existing Instance MonadState_DS.
Existing Instance MonadZero_DS.
Require Export amm.LayerAMMLIB.
From Coquelicot Require Import Coquelicot.
Require Import Interval.Missing.Coquelicot.

Import DeepSpec.lib.Monad.Monad.MonadNotation.

Module TR := TaylorPoly FullR PolR.
Module SPoly := SeqPoly FullR.
Section C_man.
Import core.MemoryModel.
Context {memModelOps : MemoryModelOps mem}.

Definition state := global_abstract_data_type.

Definition init_state := init_global_abstract_data.

Definition wei := Z.

Definition addr := int256.

Definition blocknumber := int256.

Existing Instance GlobalLayerSpec.

Context`{global_abdata : !GlobalAbData init_global_abstract_data global_low_level_invariant}.
Open Scope R_scope.

Require Import lib.Monad.RunStateTInv.
Require Import lib.ArithInv.

Definition reserve_beta (s : state) : Z := (DataTypeOps.AutomatedMarketMaker__reserve0 s).

Definition reserve_alpha (s : state) : Z := (DataTypeOps.AutomatedMarketMaker__reserve1 s).

Definition get_balance0 (s: state) (a: addr) :=
    (Int256Tree.get_default (0%Z)
      a (FixedSupplyToken_balances s)).

Context (token0_address token1_address amm_address : addr).

Context (coinbase : int256)
          (timestamp : int256)
          (number : int256)
          (balance : int256 -> int256)
          (blockhash : int256 -> int256)
          (prev_contract_state : state).

 Definition make_machine_env (caller: addr)
                              : machine_env state
    := {| me_address := amm_address;
          me_origin := caller;
          me_caller := caller;
          me_callvalue := Int256.repr (0);
          me_coinbase := coinbase; 
          me_timestamp := timestamp;
          me_number := number;
          me_chainid := Int256.zero;
          me_selfbalance := Int256.zero;
          me_balance := balance;
          me_blockhash := blockhash;
          (* not implemented *)
          me_transfer _ _ d := (Int256.one, d);
          me_callmethod _ _ _ _ _ _ _ _ _ _ := False;
          me_log _ _ _ := prev_contract_state;
        |}.

Set Typeclasses Debug.
Variable (s s': state).
Variables (a toA : addr).
Variable (r : Z32).

Definition delta_beta : Z := (get_balance0 s a) - (reserve_beta s).
Hypothesis del_alp : runStateT (AutomatedMarketMaker_simpleSwap0_opt toA (make_machine_env a)) s = Some (r , s').
 
Definition delta_alpha : Z := r.
Definition market_price : R := (IZR (delta_beta))/(IZR (delta_alpha)).
Definition eps : R := (IZR ((reserve_beta s) + delta_beta)/ (IZR ((reserve_alpha s) - delta_alpha) * market_price)) - (1%R).

Definition f (x : R) := x + (1 / x).

Definition kappa := (5/100)%R. (* approximation of (3/2) - (sqrt 2).*)

Definition T_f_1 (n : nat) := 
match n with
 | 0%nat => (fun x => sqrt(1+x) + (1/sqrt (1 + x)))
 | 1%nat => (fun x => 1/(2* sqrt(1+x)) - (1/ (2 * (1 + x) * (sqrt (1 + x)))))
 | 2%nat => (fun x => (2 - x)/ (4 * ((1 + x)^2) * sqrt(1 +x)))
 | _ => (fun x => 0%R)
end.

Lemma lower_bnd : forall eta , 0 <= eta <= 1 ->
(2 - eta) / (8 * ((1 + eta)^2) * sqrt (1 + eta)) >= 1 / 48.
Proof. intros.
       interval with (i_bisect eta).
Qed.

Lemma pos_deriv : forall x: R, x >= 1 ->
1 - (1/ sqrt(1 + x)) - 5/100 >= 0.
Proof. intros.
       interval.
Qed.

Definition extended_f (x : R) : R :=
((sqrt(1 + x) - 1)^2  - ((5/100) * (1 + x))).

Definition extended_f' (x : R) : R :=
1 - (1/ sqrt(1 + x)) - 5/100.

Definition P_gt_1 (x : R) : Prop := (x >=1)%R.

Lemma is_deriv_f : forall (x : R),
P_gt_1 x ->
Derive.is_derive extended_f x (extended_f' x).
Proof. intros. 
       unfold extended_f.
       unfold extended_f'.
       apply (is_derive_minus (fun x0 : R => (sqrt (1 + x0) - 1)^2 ) (fun x0 : R => 5 / 100 * (1 + x0)) x 
       (1 - 1/ sqrt (1 + x)) (5 / 100)).
       + replace (1 - 1 / sqrt (1 + x)) with (scal (1 / (2 * sqrt (1 + x))) (2 * (sqrt (1 + x) - 1))).
         apply (is_derive_comp (fun x0 : R => x0 ^ 2) (fun x0 : R => sqrt (1 + x0) -1) x (2 * (sqrt (1 +x) -1)) (1 / (2 * sqrt (1 + x)))).
         ++ replace 2 with (INR 2 * 1).
            replace (sqrt (1 + x) -1) with ((sqrt (1+x) -1) ^ Init.Nat.pred 2) at 2.
            apply (is_derive_pow (fun x0 : R => x0) 2%nat (sqrt (1 +x)-1) 1).
            replace 1 with (@one R_Ring) at 3.
            apply (@is_derive_id R_AbsRing).
            unfold one. simpl. reflexivity.
            unfold Init.Nat.pred. simpl. lra.
            change (INR 2) with 2. lra.
         ++ replace (1 / (2 * sqrt (1+x))) with (1 / (2 * sqrt (1+x)) - 0) by lra.
            apply (is_derive_minus (fun x0 : R => sqrt (1 + x0)) (fun x0 : R => 1%R) x (1 / (2 * sqrt (1 + x))) (0%R)).
            - replace (1 / (2 * sqrt (1 + x))) with (scal 1 (1 / (2 * sqrt (1 + x)))).
              apply (is_derive_comp (fun x0 : R => sqrt x0) (fun x0 : R => 1 + x0) x (1 / (2 * sqrt (1 + x))) 1).
              apply (is_derive_sqrt (fun x0 : R => x0) (1 +x) 1).
              replace 1 with (@one R_Ring) at 2.
              apply (@is_derive_id R_AbsRing).
              unfold one. simpl. reflexivity. unfold P_gt_1 in H. lra.
              replace 1 with (0 + 1) at 1 by lra.
              apply (is_derive_plus (fun x0 : R => 1%R) (fun x0 : R => x0) x 0 1).
              replace 0 with (@zero R_Ring).              
              apply (@is_derive_const R_AbsRing).
              unfold zero. simpl. reflexivity.
              replace 1 with (@one R_Ring).
              apply (@is_derive_id R_AbsRing).
              unfold one. simpl. reflexivity.
              unfold scal. simpl. unfold mult. simpl. lra.
              replace 0 with (@zero R_Ring).              
              apply (@is_derive_const R_AbsRing).
              unfold zero. simpl. reflexivity.
              unfold scal. simpl. unfold mult. simpl. 
              rewrite -> !Rmult_minus_distr_l. field. apply not_eq_sym. apply Rlt_not_eq.
              unfold P_gt_1 in H. interval.
      + replace (5/100) with (5/100 * 1) at 1 by lra.
        apply (is_derive_scal (fun x0 : R => 1 + x0) x (5/100) 1).
        replace 1 with (0 + 1) at 1 by lra.
        apply (is_derive_plus (fun x0 : R => 1%R) (fun x0 : R => x0) x 0 1).
        replace 0 with (@zero R_Ring).
        apply (@is_derive_const R_AbsRing).
        unfold zero. simpl. reflexivity.
        replace 1 with (@one R_Ring).
        apply (@is_derive_id R_AbsRing).
        unfold one. simpl. reflexivity.
Qed.

Lemma eps_sq : eps >= 1 ->
(sqrt(1 + eps) - 1)^2  - ((5/100) * (1 + eps)) >= 0.
Proof. intros.
       assert (Hcon : Stdlib.connected P_gt_1).
        { unfold Stdlib.connected. intros. unfold P_gt_1. unfold P_gt_1 in H0.
          destruct H2. apply (Rge_trans z x 1). apply Rle_ge. apply H2. apply H0. }
       assert (Hincr : (Rincr P_gt_1 extended_f)).
        { apply (Rderive_pos_imp_incr Hcon is_deriv_f).
          unfold Rpos_over. intros. apply Rge_le. unfold extended_f'. 
          apply pos_deriv. unfold P_gt_1 in H0. apply H0. }
       assert (H_incr_implies : (extended_f 1) >= 0 -> (Rincr P_gt_1 extended_f) ->
               eps >= 1 -> ((sqrt(1 + eps) - 1)^2  - ((5/100) * (1 + eps))) >= 0).
         { intros.
           replace ((sqrt (1 + eps) - 1) ^ 2 - 5 / 100 * (1 + eps)) with (extended_f eps).
           assert (H_1 : extended_f 1 >=0).
           { unfold extended_f. interval. }
           assert (H_2 : extended_f eps >= extended_f 1). 
           { unfold Rincr in Hincr. apply Rle_ge. apply Hincr.
             unfold P_gt_1. lra. unfold P_gt_1. apply H. apply Rge_le . apply H.  }
        apply (Rge_trans (extended_f eps) (extended_f 1) 0).
        apply H_2.
        apply H_1.
        unfold extended_f.
        reflexivity. }
        apply H_incr_implies.
        unfold extended_f. simpl.
        interval. apply Hincr. apply H.
Qed.

Ltac deriv_sqrt x := try (apply (derivable_pt_lim_comp (fun x0 : R => 1 + x0) (fun x0 : R => sqrt x0) x (1) (1 / (2 * (sqrt (1 + x))))));
                   try (replace 1%R with (0 + 1)%R at 1 by lra);
                   try (apply (derivable_pt_lim_plus (fun x0 : R => 1) id x 0 1));
                   try (apply derivable_pt_lim_const);
                   try (apply derivable_pt_lim_id);
                   try (replace (1 / (2 * sqrt (1 + x))) with (/ (2 * sqrt (1 + x))) by lra);
                   try (apply derivable_pt_lim_sqrt);
                   try (apply Fourier_util.Rlt_zero_pos_plus1).

Lemma deriv_lim_T_f : forall (k : nat) (x : R),
(k <= 1)%nat ->
0 < x < 1 ->
derivable_pt_lim (T_f_1 k) x (T_f_1 (S k) x).
Proof. intros.
       assert (Hk : (k =0)%nat \/ (k = 1)%nat) by lia.
       destruct Hk.
       - rewrite -> H1. simpl.
         replace (1 / (2 * sqrt (1 + x)) - 1 / (2 * (1 + x) * sqrt (1 + x))) with
         (1 / (2 * sqrt (1 + x)) + - 1 / (2 * (1 + x) * sqrt (1 + x))) by lra.
         apply (derivable_pt_lim_plus (fun x0 : R => sqrt (1 + x0)) (fun x0 : R => 1 / sqrt (1 + x0)) x (1 / (2 * sqrt (1 + x))) 
         (- 1 / (2 * (1 + x) * sqrt (1 + x)))).
         -- replace (1 / (2 * sqrt (1 + x))) with (1 / (2 * sqrt (1 + x)) * 1) by lra.
            apply (derivable_pt_lim_comp (fun x0 : R => 1 + x0) (fun x0 : R => sqrt x0) x (1) (1 / (2 * (sqrt (1 + x))))).
            replace 1%R with (0 + 1)%R at 1 by lra.
            apply (derivable_pt_lim_plus (fun x0 : R => 1) id x 0 1).
            apply derivable_pt_lim_const.
            apply derivable_pt_lim_id.
            replace (1 / (2 * sqrt (1 + x))) with (/ (2 * sqrt (1 + x))) by lra.
            apply derivable_pt_lim_sqrt.
            apply Fourier_util.Rlt_zero_pos_plus1.
            destruct H0. apply H0.
         -- replace (-1 / (2 * (1 + x) * sqrt (1 + x))) with (-1 / (1 + x) * / (2 * sqrt (1 + x))).
            apply (derivable_pt_lim_comp (fun x0 : R => sqrt (1 + x0)) (fun x0 : R => 1 / x0) x (/ (2 * sqrt (1 + x))) (-1 / (1 + x))).
            replace (/ (2 * sqrt (1 + x))) with (1 / (2 * sqrt (1 + x)) * 1) by lra.
            apply (derivable_pt_lim_comp (fun x0 : R => 1 + x0) (fun x0 : R => sqrt x0) x (1) (1 / (2 * (sqrt (1 + x))))).
            replace 1%R with (0 + 1)%R at 1 by lra.
            apply (derivable_pt_lim_plus (fun x0 : R => 1) id x 0 1).
            apply derivable_pt_lim_const.
            apply derivable_pt_lim_id.
            replace (1 / (2 * sqrt (1 + x))) with (/ (2 * sqrt (1 + x))) by lra.
            apply derivable_pt_lim_sqrt.
            apply Fourier_util.Rlt_zero_pos_plus1.
            destruct H0. apply H0.
            replace (-1 / (1 + x)) with ((0 * sqrt (1 + x) - 1 * 1) / (sqrt (1 + x))²).
            apply (derivable_pt_lim_div (fun x0 : R => 1%R) (fun x0 : R => x0) (sqrt (1 + x)) 0 1).
            apply derivable_pt_lim_const.
            apply derivable_pt_lim_id.
            apply not_eq_sym.
            apply Rlt_not_eq.
            apply sqrt_lt_R0.
            apply Fourier_util.Rlt_zero_pos_plus1.
            destruct H0. apply H0.
            replace (0 * sqrt (1 + x)) with 0 by lra.
            replace (0 - 1* 1) with (-1)%R by lra.
            rewrite -> Rsqr_sqrt. reflexivity. apply Rlt_le.
            apply Fourier_util.Rlt_zero_pos_plus1. destruct H0. apply H0.
            unfold Rdiv.
            field. split.
            apply not_eq_sym.
            apply Rlt_not_eq.
            apply sqrt_lt_R0.
            apply Fourier_util.Rlt_zero_pos_plus1.
            destruct H0. apply H0.
            apply not_eq_sym.
            apply Rlt_not_eq.
            apply Fourier_util.Rlt_zero_pos_plus1.
            destruct H0. apply H0.
         - rewrite -> H1. simpl.
           replace ((2 - x) / (4 * ((1 + x) * ((1 + x) * 1)) * sqrt (1 + x))) with
           ((-1 / (4 * (1 + x) * sqrt (1 + x))) - (-3/(4 * (1 + x) * (1 + x) * sqrt (1 + x)))).
           apply (derivable_pt_lim_minus (fun x0 : R => 1 / (2 * sqrt (1 + x0))) (fun x0 : R => 1 / (2 * (1 + x0) * sqrt (1 + x0))) x (-1 / (4 * (1 + x) * sqrt (1 + x))) 
           (-3/(4 * (1 + x) * (1 + x) * sqrt (1 + x)))).
           -- replace (-1 / (4 * (1 + x) * sqrt (1 + x))) with (1 / 2 * (- 1 / (2 * (1 + x) * sqrt (1 + x)))).
              apply (derivable_pt_lim_locally_ext (mult_real_fct (1/2) (fun x0 : R => 1 / sqrt (1 + x0))) (fun x0 : R => 1 / (2 * sqrt (1 + x0)) ) x 0 1 
              (1 / 2 * (- 1 / (2 * (1 + x) * sqrt (1 + x))))). apply H0.
              intros. unfold mult_real_fct. field. apply not_eq_sym. apply Rlt_not_eq.
              apply sqrt_lt_R0. apply Fourier_util.Rlt_zero_pos_plus1. destruct H2. apply H2. 
              apply (derivable_pt_lim_scal (fun x0 : R => 1 / sqrt (1 + x0)) (1/2)%R x (- 1 / (2 * (1 + x) * sqrt (1 + x)))).
              replace (-1 / (2 * (1 + x) * sqrt (1 + x))) with (-1 / (1 + x) * / (2 * sqrt (1 + x))).
              apply (derivable_pt_lim_comp (fun x0 : R => sqrt (1 + x0)) (fun x0 : R => 1 / x0) x (/ (2 * sqrt (1 + x))) (-1 / (1 + x))).
              replace (/ (2 * sqrt (1 + x))) with (1 / (2 * sqrt (1 + x)) * 1) by lra.
              apply (derivable_pt_lim_comp (fun x0 : R => 1 + x0) (fun x0 : R => sqrt x0) x (1) (1 / (2 * (sqrt (1 + x))))).
              replace 1%R with (0 + 1)%R at 1 by lra.
              apply (derivable_pt_lim_plus (fun x0 : R => 1) id x 0 1).
              apply derivable_pt_lim_const.
              apply derivable_pt_lim_id.
              replace (1 / (2 * sqrt (1 + x))) with (/ (2 * sqrt (1 + x))) by lra.
              apply derivable_pt_lim_sqrt.
              apply Fourier_util.Rlt_zero_pos_plus1.
              destruct H0. apply H0.
              replace (-1 / (1 + x)) with ((0 * sqrt (1 + x) - 1 * 1) / (sqrt (1 + x))²).
              apply (derivable_pt_lim_div (fun x0 : R => 1%R) (fun x0 : R => x0) (sqrt (1 + x)) 0 1).
              apply derivable_pt_lim_const.
              apply derivable_pt_lim_id.
              apply not_eq_sym.
              apply Rlt_not_eq.
              apply sqrt_lt_R0.
              apply Fourier_util.Rlt_zero_pos_plus1.
              destruct H0. apply H0.
              replace (0 * sqrt (1 + x)) with 0 by lra.
              replace (0 - 1* 1) with (-1)%R by lra.
              Search (sqrt (_))².
              rewrite -> Rsqr_sqrt. reflexivity. apply Rlt_le.
              apply Fourier_util.Rlt_zero_pos_plus1. destruct H0. apply H0.
              unfold Rdiv.
              field. split.
              apply not_eq_sym.
              apply Rlt_not_eq.
              apply sqrt_lt_R0.
              apply Fourier_util.Rlt_zero_pos_plus1.
              destruct H0. apply H0.
              apply not_eq_sym.
              apply Rlt_not_eq.
              apply Fourier_util.Rlt_zero_pos_plus1.
              destruct H0. apply H0.
              field. split. apply not_eq_sym. apply Rlt_not_eq. apply sqrt_lt_R0.
              apply Fourier_util.Rlt_zero_pos_plus1. destruct H0. apply H0. apply not_eq_sym.
              apply Rlt_not_eq. apply Fourier_util.Rlt_zero_pos_plus1. destruct H0. apply H0.
           -- replace (-3 / (4 * (1 + x) * (1 + x) * sqrt (1 + x))) with 
              ((1/ 2) * (-3 / (2 * (1 + x) * (1 + x) * sqrt (1 + x)))).
              apply (derivable_pt_lim_locally_ext (mult_real_fct (1/2) (fun x0 : R => 1 / ((1 + x0) * sqrt (1 + x0)))) 
               (fun x0 : R => 1 / (2 * (1 + x0)* sqrt (1 + x0)) ) x 0 1 
              (1 / 2 * (- 3 / (2 * (1 + x) * (1 + x) * sqrt (1 + x))))). apply H0.
              intros. unfold mult_real_fct. field. split. apply not_eq_sym. apply Rlt_not_eq.
              apply sqrt_lt_R0. apply Fourier_util.Rlt_zero_pos_plus1. destruct H2. apply H2. 
              apply not_eq_sym. apply Rlt_not_eq.
              apply Fourier_util.Rlt_zero_pos_plus1. destruct H2. apply H2.
              apply (derivable_pt_lim_scal (fun x0 : R => 1 / ((1 + x0) * sqrt (1 + x0))) (1/2)%R x (- 3 / (2 * (1 + x) * (1 + x) * sqrt (1 + x)))). 
              replace (-3 / (2 * (1 + x) * (1 + x) * sqrt (1 + x))) with 
              ( -1 / ((1 + x) * (1 + x) * (1 + x)) * (3 / 2 * sqrt (1 + x))).
              apply (derivable_pt_lim_comp (fun x0 : R => (1 + x0) * sqrt (1 + x0)) 
              (fun x0 : R => 1 / x0) x ((3 / 2) * sqrt (1 + x)) (-1 / ((1 + x) * (1 + x) * (1 + x)))).
            ++ replace (3 / 2 * sqrt (1 + x)) with (1 * sqrt (1 + x) + (1 + x) * (1 / (2 * sqrt (1 + x)))).
               apply (derivable_pt_lim_locally_ext (mult_fct (fun x0 : R => 1 + x0) (fun x0 : R => sqrt (1 + x0)))
               (fun x0 : R => (1 + x0) * (sqrt (1 + x0))) x 0 1 (1 * sqrt (1 + x) + (1 + x) * (1 / (2 * sqrt (1 + x))))).
               apply H0. intros. 
               unfold mult_fct. reflexivity.
               apply (derivable_pt_lim_mult (fun x0 : R => 1 + x0) (fun x0 : R => sqrt (1 + x0)) x 1 (1/ (2 * sqrt (1 + x)))).
               replace 1%R with (0 + 1)%R at 1 by lra.
               apply (derivable_pt_lim_plus (fun x0 : R => 1) id x 0 1).
               apply derivable_pt_lim_const.
               apply derivable_pt_lim_id.
               replace (1 / (2 * sqrt (1 + x))) with (1 / (2 * sqrt (1 + x)) * 1) by lra.
               apply (derivable_pt_lim_comp (fun x0 : R => 1 + x0) (fun x0 : R => sqrt x0) x 1 (1 / (2 * sqrt (1 + x)))).        
               replace 1%R with (0 + 1)%R at 1 by lra.
               apply (derivable_pt_lim_plus (fun x0 : R => 1) id x 0 1).
               apply derivable_pt_lim_const.
               apply derivable_pt_lim_id.
               replace (1 / (2 * sqrt (1 + x))) with (/ (2 * sqrt (1 + x))) by lra.
               apply derivable_pt_lim_sqrt.
               apply Fourier_util.Rlt_zero_pos_plus1. destruct H0. apply H0.
               replace ((1 + x) * (1 / (2 * sqrt (1 + x)))) with ((1 + x) / (2 * sqrt (1 + x))) by lra.
               replace ((1 + x) / (2 * sqrt (1 + x))) with (sqrt (1 + x) / 2).
               lra.
               apply (Stdlib.Rdiv_eq_reg (sqrt (1 + x)) 2 (1 + x) (2 * sqrt (1 + x))).
               replace (sqrt (1 + x) * (2 * sqrt (1 + x))) with (2 * (sqrt (1 + x) * sqrt (1 + x))) by lra.
               apply (Rmult_eq_compat_l 2 (sqrt (1 + x) * sqrt (1 + x)) (1 + x)).
               apply sqrt_sqrt. apply Rlt_le. apply Fourier_util.Rlt_zero_pos_plus1. destruct H0. apply H0.
               apply not_eq_sym. apply Rlt_not_eq. lra. apply not_eq_sym. apply Rlt_not_eq.
               apply Rmult_lt_0_compat. lra. apply sqrt_lt_R0. apply Fourier_util.Rlt_zero_pos_plus1. destruct H0. apply H0.
               replace (-1 / ((1 + x) * (1 + x) * (1 + x))) with ((0 *((1 + x) * sqrt (1 + x)) - 1 * 1) / ((1 + x) * sqrt (1 + x))²).
               apply (derivable_pt_lim_locally_ext ((fun x0 : R => 1) / (fun x0 : R => x0))%F  (fun x0 : R => 1 / x0) ((1 + x) * sqrt (1 + x)) 0 3
               ((0 * ((1 + x) * sqrt (1 + x)) - 1 * 1) /((1 + x) * sqrt (1 + x))²)).
               split. interval. interval. intros. unfold div_fct. reflexivity.
               apply (derivable_pt_lim_div (fun x0 : R => 1%R) (fun x0 : R => x0) ((1 + x) * sqrt (1 + x)) 0 1).
               apply derivable_pt_lim_const.
               apply derivable_pt_lim_id.
               apply not_eq_sym. apply Rlt_not_eq. apply Rmult_lt_0_compat. apply Fourier_util.Rlt_zero_pos_plus1.
               destruct H0. apply H0. apply sqrt_lt_R0. apply Fourier_util.Rlt_zero_pos_plus1. destruct H0. apply H0.
               replace (0 * ((1 + x) * sqrt (1 + x)) - 1 * 1) with (-1)%R by lra.
               rewrite -> Rsqr_mult.
               rewrite -> Rsqr_sqrt.
               rewrite -> Rsqr_pow2. field.
               apply not_eq_sym. apply Rlt_not_eq. apply Fourier_util.Rlt_zero_pos_plus1. destruct H0. apply H0.
               apply Rlt_le. apply Fourier_util.Rlt_zero_pos_plus1. destruct H0. apply H0. 
               ring_simplify.
               replace (-1 / ((1 + x) * (1 + x) * (1 + x)) * (3 / 2) *sqrt (1 + x)) with 
               (-1 / ((1 + x) * (1 + x) * (1 +x)) * (3 * sqrt (1 + x)/ 2)) by lra.
               replace (-1 / ((1 + x) * (1 + x) * (1 + x)) * (3 * sqrt (1 + x) / 2)) with 
               ((-1 * (3 * sqrt (1+ x)) /(((1 +x) * (1 + x) * (1 + x)) * 2))).
               eapply Stdlib.Rdiv_eq_reg. 
               assert (forall x y z : R, (x * y) * z = x * (y * z)).
               { intros. ring. }
               assert (sqrt (1+x) ^ 2 = 1+x).
               { unfold pow. replace (sqrt (1 + x) * 1) with (sqrt (1 + x)) by lra.
                 apply sqrt_sqrt. apply Rlt_le. lra. } 
               field_simplify. rewrite !H3. field.
               apply not_eq_sym. apply Rlt_not_eq. apply Rmult_lt_0_compat ; try lra.
               apply Rmult_lt_0_compat ; try lra. apply Rmult_lt_0_compat; lra.
               apply not_eq_sym. apply Rlt_not_eq. apply Rmult_lt_0_compat ; try lra.
               apply Rmult_lt_0_compat ; try lra. apply sqrt_lt_R0. lra.
               assert (forall x y z w : R, w <> 0 -> z <> 0 -> (x * y) / (z * w) = (x / z) * (y / w)).
               { intros. field. split. apply H2. apply H3. }
               apply (H2 (-1)%R (3 * sqrt (1 + x)) ((1 + x) * (1 + x) * (1 + x)) 2). lra.
               apply not_eq_sym. apply Rlt_not_eq. apply Rmult_lt_0_compat ; try lra.
               apply Rmult_lt_0_compat ; try lra.
               symmetry. replace (-3)%R with (1 *-3)%R at 1 by lra.
               replace 4%R  with (2 * 2)%R at 1 by lra.
               assert (forall x y z w : R, w <> 0 -> z <> 0 -> (x * y) / (z * w) = (x / z) * (y / w)).
               { intros. field. split. apply H2. apply H3. }
               replace (2 * 2 * (1 + x) * (1 + x) * sqrt (1 + x)) with 
               (2 * (2 * (1 + x) * (1 + x) * sqrt (1 + x))) by lra.
               apply (H2 1 (-3)%R 2 (2 * (1 + x) * (1 + x) * sqrt (1 + x))).
               apply not_eq_sym. apply Rlt_not_eq. apply Rmult_lt_0_compat ; try lra.
               apply Rmult_lt_0_compat ; try lra. apply sqrt_lt_R0. lra. lra.
               rewrite -> (Rdiv_minus (-1)%R (4 * (1 + x) * sqrt (1 + x)) (-3)%R (4 * (1 + x) * (1 + x) * sqrt (1 + x))).
               field. split. apply not_eq_sym. apply Rlt_not_eq. apply sqrt_lt_R0. lra.
               apply not_eq_sym. apply Rlt_not_eq. lra.
               apply not_eq_sym. apply Rlt_not_eq. apply Rmult_lt_0_compat ; try lra. apply sqrt_lt_R0. lra.
               apply not_eq_sym. apply Rlt_not_eq. apply Rmult_lt_0_compat ; try lra. apply Rmult_lt_0_compat ; try lra.
               apply sqrt_lt_R0. lra.
Qed.

Lemma cont_lim_T_f : forall (k : nat) (x : R),
(k <= 1)%nat ->
0 <= x <= 1 -> continuity_pt (T_f_1 k) x.
Proof. intros. 
       assert (Hk : (k =0)%nat \/ (k = 1)%nat) by lia.
       destruct Hk.
       - rewrite -> H1. simpl.
         apply (continuity_pt_plus (fun x0 : R => sqrt (1 +x0)) (fun x0 : R => 1 / sqrt (1 + x0)) x).
         apply (continuity_pt_comp (fun x0 : R => 1 + x0) (fun x0 : R => sqrt x0) x).
         apply (continuity_pt_plus (fun x0 : R => 1%R) (fun x0 : R => x0) x).
         apply (continuity_pt_const (fun _ : R => 1) x).
         unfold constant. reflexivity.
         apply (continuity_pt_id x).
         apply (continuity_pt_sqrt (1 +x)). lra.
         apply (continuity_pt_comp (fun x0 : R => sqrt ( 1 + x0)) (fun x0 : R => 1/ x0) x).
         apply (continuity_pt_comp (fun x0 : R => 1 + x0) (fun x0 : R => sqrt x0) x).
         apply (continuity_pt_plus (fun x0 : R => 1%R) (fun x0 : R => x0) x).
         apply (continuity_pt_const (fun _ : R => 1) x).
         unfold constant. reflexivity.
         apply (continuity_pt_id x).
         apply (continuity_pt_sqrt (1 +x)). lra.
         apply (continuity_pt_locally_ext (/ (fun x0 : R => x0))%F (fun x0 : R => 1/ x0) 1 (sqrt (1 +x))). lra.
         intros. unfold inv_fct. lra.
         apply (continuity_pt_inv (fun x0 : R => x0) (sqrt (1 +x))).
         apply (continuity_pt_id (sqrt (1 +x))).
         apply not_eq_sym. apply Rlt_not_eq. interval.
       - rewrite -> H1. simpl.
         apply (continuity_pt_minus (fun x0 : R => 1 / (2 * sqrt (1 + x0))) (fun x0 : R => 1 / (2 * (1 + x0) * sqrt (1 + x0))) x).
         apply (continuity_pt_comp (fun x0 : R => 2 * sqrt ( 1 + x0)) (fun x0 : R => 1/ x0) x).
         apply (continuity_pt_locally_ext ((fun x0 : R => 2%R) * (fun x0 : R => sqrt (1 + x0)))%F 
         (fun x0 : R => 2 * sqrt ( 1 + x0)) 1 x). lra. intros. unfold mult_fct. reflexivity.
         apply (continuity_pt_mult (fun _ : R => 2) (fun x0 : R => sqrt (1 + x0)) x).
         apply (continuity_pt_const (fun _ : R => 2) x).
         unfold constant. reflexivity.
         apply (continuity_pt_comp (fun x0 : R => 1 + x0) (fun x0 : R => sqrt x0) x).
         apply (continuity_pt_plus (fun x0 : R => 1%R) (fun x0 : R => x0) x).
         apply (continuity_pt_const (fun _ : R => 1) x).
         unfold constant. reflexivity.
         apply (continuity_pt_id x).
         apply (continuity_pt_sqrt (1 +x)). lra.
         apply (continuity_pt_locally_ext (/ (fun x0 : R => x0))%F (fun x0 : R => 1/ x0) 1 (2 * sqrt (1 +x))). lra.
         intros. unfold inv_fct. Locate "/". lra.
         apply (continuity_pt_inv (fun x0 : R => x0) (2 * sqrt (1 +x))).
         apply (continuity_pt_id (2 * sqrt (1 +x))).
         apply not_eq_sym. apply Rlt_not_eq. apply Rmult_lt_0_compat. lra. interval.
         apply (continuity_pt_comp (fun x0 : R => 2 * (1 + x0) * sqrt ( 1 + x0)) (fun x0 : R => 1/ x0) x).
         apply (continuity_pt_locally_ext
          ((fun x0 : R => 2%R) * (fun x0 : R => ((1 + x0) * sqrt (1 + x0))%R))%F 
          (fun x0 : R => 2 * (1 + x0) * sqrt ( 1 + x0))
          1 x). lra. intros. unfold mult_fct.
          lra.
         apply continuity_pt_mult.
         apply continuity_pt_const. unfold constant. reflexivity.
         apply (continuity_pt_locally_ext 
         ((fun x0 : R => (1 + x0)%R) * (fun x0 : R => sqrt (1 + x0)))%F 
         (fun x0 : R => (1 + x0) * sqrt (1 + x0)) 1 x). lra. intros. unfold mult_fct. reflexivity.
         apply continuity_pt_mult.
         apply continuity_pt_plus.
         apply continuity_pt_const. unfold constant. reflexivity.
         apply continuity_pt_id.
         apply (continuity_pt_comp (fun x0 : R => 1 + x0) (fun x0 : R => sqrt x0) x).
         apply (continuity_pt_plus (fun x0 : R => 1%R) (fun x0 : R => x0) x).
         apply (continuity_pt_const (fun _ : R => 1) x).
         unfold constant. reflexivity.
         apply (continuity_pt_id x).
         apply (continuity_pt_sqrt (1 +x)). lra.
         apply (continuity_pt_locally_ext (/ (fun x0 : R => x0))%F (fun x0 : R => 1/ x0) 1 (2 * (1 + x) * sqrt (1 +x))). lra.
         intros. unfold inv_fct. Locate "/". lra.
         apply (continuity_pt_inv (fun x0 : R => x0) (2 * (1 + x) * sqrt (1 +x))).
         apply continuity_pt_id.
         apply not_eq_sym. apply Rlt_not_eq. apply Rmult_lt_0_compat. lra. interval.
Qed.


Lemma taylor_m : 0 < eps <= 1 ->
exists eta ,
 (0 <> eps -> (0 < eta < eps \/ eps < eta < 0)) /\ sqrt (1 + eps) + 1/sqrt(1 + eps) -2 =  (((2 - eta) / (8* ((1 + eta)^2) * sqrt (1 + eta))) * eps^2).
Proof. intros.
       About Taylor.Taylor_Lagrange.
       edestruct (Taylor.Taylor_Lagrange 0 1 1 T_f_1) with 0 eps.
         ++ apply deriv_lim_T_f.
         ++ apply cont_lim_T_f.
         ++ interval.
         ++ lra.
         ++ exists x. 
            destruct H0 as [H1 H2]. cbn in H1.
            split.
            - apply H2.
            - rewrite !Rplus_0_r in H1.
              rewrite !Rminus_0_r in H1.
              rewrite !sqrt_1 in H1.
              rewrite !Rcomplements.Rdiv_1 in H1.
              rewrite !Rmult_1_r in H1.
              rewrite !Rcomplements.Rminus_eq_0 in H1.
              rewrite !Rmult_0_l in H1.
              replace (1 + 1 + 0)%R with 2%R in H1 by lra.
              replace (1 + 1)%R with 2%R in H1 by lra.
              unfold pow. rewrite !Rmult_1_r. rewrite -> H1.
              field. 
              assert (Hsqrt : x > -1 -> sqrt (1 + x) <> 0).
               { intros. apply not_eq_sym. apply Rlt_not_eq. apply sqrt_lt_R0. lra. }
              assert (Heps : 0 < x < eps ).
               { destruct H2 ; lra. }
              split.
              apply Hsqrt. lra. lra.
Qed.

Lemma cst_func_0_1 : 0 <= eps <= 1 ->
sqrt (1 + eps) + 1/sqrt(1 + eps) -2 >= (1/48) * (eps ^2).
Proof. intros.
       assert (Heq_dec : {0 < eps} + {0 = eps}).
       { apply Rle_lt_or_eq_dec. destruct H. apply H. }
       destruct Heq_dec.  
       + destruct taylor_m as [eta H2].
         lra.
         destruct H2 as [H2a H2b].
         assert (Heta : (2 - eta) / (8 * (1 + eta) ^ 2 * sqrt (1 + eta)) >= 1/48).
         { apply lower_bnd. destruct H2a.
           apply Rlt_not_eq. apply r0.
           split. apply Rlt_le.  apply H0. 
           destruct H. apply Rlt_le. apply (Rlt_le_trans eta eps 1).
           destruct H0. apply H2. apply H1. 
           assert (Hc : eps < 0).
           { destruct H0. apply (Rlt_trans eps eta 0). apply H0. apply H1. } lra. }
       rewrite -> H2b.
       apply (Rmult_ge_compat_r (eps^2) ((2 - eta) /
       (8 * (1 + eta) ^ 2 * sqrt (1 + eta))) (1/48)).
       apply Rle_ge. apply pow2_ge_0. apply Heta.
       + rewrite <- e. simpl.
         replace (1 + 0)%R with 1%R by lra.
         replace (0 * 1)%R with 0%R by lra.
         rewrite !sqrt_1. lra.
Qed.

Lemma cst_func_ge_1 : eps >= 1 ->
sqrt ( 1 + eps) + (1 / sqrt (1 + eps)) -2 >= ((5/100) * sqrt (1 + eps)).
Proof. intros.
       apply (Rminus_ge (sqrt ( 1 + eps) + (1 / sqrt (1 + eps)) -2) ((5/100) * sqrt (1 + eps))).
       replace (sqrt (1 + eps) + 1 / sqrt (1 + eps) - 2 - 5 / 100 * sqrt (1 + eps)) with 
       ((1 / sqrt ( 1 + eps)) * ((sqrt(1 + eps) - 1)^2  - ((5/100) * (1 + eps)))).
       replace 0%R with ((1 / sqrt (1 + eps)) * 0) by lra.
       apply (Rmult_ge_compat_l (1 / sqrt ( 1 + eps)) ((sqrt(1 + eps) - 1)^2  - ((5/100) * (1 + eps))) 0%R).
       interval. apply eps_sq. apply H.
       assert  (Hsqr : (sqrt (1 + eps))^2 = 1 + eps).
        { unfold pow. replace (sqrt (1 + eps) * 1) with (sqrt (1 + eps)) by lra. apply sqrt_sqrt. interval. }
       field_simplify ; try interval. rewrite !Hsqr. field ; interval.
Qed.

End C_man.