Require Import ZArith.
Require Import DeepSpec.cclib.Integers.
Require Import Zorder.
Require Import Lia.
Require Import QArith.
Require Import Lqa.
Require Import QArith_base.

(* to solve the inequality in prt_inf.v , first solev it in Q using some standard arithmetic properties,
then use properties of inject and transitivity of < to show it holds over Z *)
(*Axiom ZInEq_multpos : (forall (a b c : Z32), c > 0 -> a > b <-> a * c > b * c)%Z.
  Hint Rewrite ZInEq_multpos : ZInEq.

Axiom ZInEq_ldiv : (forall (a b c : Z32), b > 0 -> a / b > c <-> a > b * c)%Z.

Axiom ZInEq_denadd : (forall (a b c : Z32), c > 0 -> a / b > a / (b + c))%Z.
*)
(* Properties of fractions *)
(* this axiom is a lemma in a later version of the standard library *)
Axiom Qinv_plus_distr : forall a b c, ( c > 0 -> ((a + b)/c = a/c + b/c))%Q .

Axiom Qinv_minus_distr : forall a b c, ( c > 0 -> ((a - b)/c = a/c - b/c))%Q.

Axiom Qinv_mult : forall a b, (a * / b = a / b)%Q.

Notation "[ i ]" := (inject_Z i) : Q_scope. 

Lemma common_denom : forall (a b c : Q), (c > 0 -> ((a - (b/c)) == (a*c - b)/c))%Q.
Proof. intros.
       setoid_replace a with (a * c / c) at 1.
       setoid_rewrite <- Qinv_minus_distr. reflexivity. lra.
       Search ( _ * _)%Q.
       symmetry. apply Qdiv_mult_l.
       apply Qnot_eq_sym.
         apply Qlt_not_eq. lra.
Qed.
 
Lemma mul_denom : forall ( a b c d : Q), (d > 0 -> a*d < b*c -> a < b*(c/d))%Q.
Proof.  intros.
        replace (b * (c / d)) with (b * c / d).
Search (_ / _)%Q.
        apply Qlt_shift_div_l. apply H. apply H0. 
        unfold Qdiv. Search (_ * _)%Q. Admitted.

Lemma increasing_k_math_rat : forall (i j k : Q), (i > 0 -> j > 0 -> k > 0 ->
 i * j < (i + k) * (j - (k * [997] * j) / (i * [1000] + k * [997])))%Q.
Proof. intros.
       assert (H1a : [1000] >0 ).
            { change 0 with [0]. rewrite <- Zlt_Qlt. lia. }
       assert (H1b : [997] >0).
            { change 0 with [0]. rewrite <- Zlt_Qlt. lia. }
       setoid_replace (j - k * [997]* j / (i * [1000] +  k*[997]))%Q with 
       (((j * (i * [1000] + k * [997])) - (k * [997] *j))/ (i * [1000] + k * [997]))%Q.
       + apply (mul_denom (i * j)%Q (i + k)%Q (j * (i * [1000] + k * [997]) - k * [997] * j)%Q 
         (i * [1000] + k * [997])%Q ). 
         nra. ring_simplify.
         assert (H2 : (997 < 1000)%Z).
          { lia. }
         eapply Qplus_lt_r. repeat apply Qmult_lt_r ; auto.
       + eapply common_denom. nra.
Qed.

Search inject_Z.
Lemma inject_Z_minus : forall x y , [x - y] = [x] - [y].
Proof. intros.
       change (x - y)%Z with (x + - y) %Z.
       rewrite -> inject_Z_plus. rewrite -> inject_Z_opp.
       change ([x] + - [y])%Q with ([x] - [y])%Q. reflexivity.
Qed.

Lemma inject_Z_div_pos : forall x y , (x > 0)%Z -> (y > 0)%Z -> ([x / y] <= [x] / [y])%Q.
Proof. intros.
       rewrite -> (Z_div_mod_eq x y).
       destruct (x mod y)%Z eqn : Hrem.
       + replace (y * (x / y) + 0)%Z with (y * (x / y))%Z by lia.
         replace (y * (x / y))%Z with  ((x / y) * y)%Z by lia.
         rewrite -> Z.div_mul.
         rewrite -> inject_Z_mult.
         setoid_rewrite -> Qdiv_mult_l.
         apply Qle_refl.
         apply Qnot_eq_sym.
         apply Qlt_not_eq.
         change 0 with [0]. rewrite <- Zlt_Qlt. lia.
         apply Z.neq_sym. apply Z.lt_neq. lia.
       + replace (y * (x / y))%Z with  ((x / y) * y)%Z by lia.
         rewrite -> Z.div_add_l.
         repeat rewrite -> inject_Z_plus.
         rewrite -> inject_Z_mult.
         assert (Hpos : (Z.pos p / y)%Z = 0%Z).
         { apply Z.div_small. rewrite <- Hrem. apply Z_mod_lt. lia. } 
         rewrite -> Hpos.
         rewrite -> Qinv_plus_distr.
         setoid_rewrite -> Qdiv_mult_l.
         rewrite -> Qplus_le_r.
         replace ([Z.pos p] / [y])%Q with ([Z.pos p] * (/ [y]))%Q .
         apply Qmult_le_0_compat.
         change 0 with [0]. rewrite <- Zle_Qle. lia.
         apply Qinv_le_0_compat.
         change 0 with [0].
         rewrite <- Zle_Qle.
         apply Z.lt_le_incl. lia.
         apply Qinv_mult.
         apply Qnot_eq_sym.
         apply Qlt_not_eq.
         change 0 with [0] ; rewrite <- Zlt_Qlt; lia.
         change 0 with [0] ; rewrite <- Zlt_Qlt; lia.
         apply Z.neq_sym. apply Z.lt_neq. lia.
       + assert (Hneg : (0 <= (x mod y) < y)%Z).
          { apply Z_mod_lt. lia. }
         rewrite -> Hrem in Hneg. 
         lia. 
       + lia.
Qed.

Lemma Qminus_le_l : forall x y z , x >= y -> z - x <= z - y.
Proof. Admitted.

Lemma increasing_k_math : forall (i j k : Z), (i > 0 -> j > 0 -> k > 0 ->
 i * j < (i + k) * (j - (k * 997 * j) / (i * 1000 + k * 997)))%Z.
Proof. intros.
       rewrite -> Zlt_Qlt.
       repeat rewrite -> inject_Z_mult.
       repeat rewrite -> inject_Z_plus.
       rewrite -> inject_Z_minus.
       assert (Hineq1 : [j] - [k * 997 * j]/[i * 1000 + k * 997] <= 
               [j] - [k * 997 * j / (i * 1000 + k * 997)]).
       { eapply Qminus_le_l. eapply inject_Z_div_pos. 
        Search (_ > _)%Z (_ > _) %Z (_ * _ > _)%Z. repeat (apply Zmult_gt_0_compat). lia. lia. lia. lia. } 
       assert ( Hineq2 : [i] * [j] < ([i] + [k]) * ([j] - [k * 997 * j] / [i * 1000 + k * 997])).
       { repeat rewrite -> inject_Z_mult.
         rewrite -> inject_Z_plus.
         repeat rewrite -> inject_Z_mult.
         apply (increasing_k_math_rat [i] [j] [k]) ;
         change 0 with [0]; rewrite <- Zlt_Qlt; lia. }
       apply (Qlt_le_trans ([i] * [j]) (([i] + [k]) *
         ([j] -
          [k * 997 * j] / [i * 1000 + k * 997])) (([i] + [k]) * ([j] -
         [k * 997 * j / (i * 1000 + k * 997)]))).
       apply Hineq2.
       rewrite Qmult_le_l.
       apply Hineq1.
       assert (Hq1 : [i] > 0).
       { change 0 with [0]. rewrite <- Zlt_Qlt. lia. }
       assert (Hq2 : [k] > 0).
       { change 0 with [0]. rewrite <- Zlt_Qlt. lia. }
       lra.
Qed.

Search (_ < _)%Z (_ < _)%Z (_ - _ < _ - _)%Z.
(*Lemma mul_denom : forall ( a b c d : Z), (a > 0 -> c > 0 -> d > 0 ->
a*d < b*c -> a < b*c/d)%Z .
Proof. 
Admitted.

Lemma common_denom : forall (a b c : Z) , ( c > 0 -> (a + (b/c)) = (a*c + b)/c )%Z.
Proof. Admitted.

Lemma increasing_k_math : forall (i j k : Z), (i > 0 -> j > 0 -> k > 0 -> 
    i * j < (i + k) * (j - (k * 997 * j) / (i * 1000 + k * 997)))%Z.
Proof. intros.
       replace (j - k * 997 * j / (i * 1000 + k * 997))%Z with 
       (((j * (i * 1000 + k * 997)) - (k * 997 *j))/ (i * 1000 + k * 997))%Z.
       + apply (mul_denom (i * j)%Z (i + k)%Z (j * (i * 1000 + k * 997) - k * 997 * j)%Z 
         (i * 1000 + k * 997)%Z ). replace (i * j < (i + k) * ((j * (i * 1000 + k * 997) - k * 997 * j) /
         (i * 1000 + k * 997)))%Z with
         (i * j * (i * 1000 + k * 997) < (i + k) * (j * (i * 1000 + k * 997) - k * 997 * j))%Z.
         replace (i * j * (i * 1000 + k * 997))%Z with (i * j * i * 1000 + i *j * k * 997)%Z by lia.
         replace ((i + k) * (j * (i * 1000 + k * 997) - k * 997 * j))%Z with 
         ((i + k) * (j * i * 1000 + j * k * 997 - k * 997 *j))%Z by lia.
         replace (j * k * 997)%Z with (k * 997 * j)%Z by lia.
         replace (j * i * 1000 + k * 997 * j - k * 997 * j)% Z with (j * i * 1000)%Z by lia.
         replace ((i + k) * (j * i * 1000))%Z with (i * j * i * 1000 + k * j * i * 1000)%Z by lia.
         replace (i * j * i * 1000 + i * j * k * 997)%Z with 
         (i * j * k * 997 + i * j * i * 1000)%Z by lia.
         replace (i * j * i * 1000 + k * j * i * 1000)%Z with (k * j * i * 1000 + i * j * i * 1000)%Z by lia.
         eapply Zplus_lt_compat_r.
         replace (i * j * k * 997)%Z with (997 * i * j * k)%Z by lia.
         replace (k * j * i * 1000)%Z with (1000 * i * j * k)%Z by lia.
         eapply Zmult_gt_0_lt_compat_r. apply H1.
         eapply Zmult_gt_0_lt_compat_r. apply H0.
         eapply Zmult_gt_0_lt_compat_r. apply H. lia. eapply mul_denom.
    From Coq Require Import ZArith Psatz.
    Open Scope Z_scope.
    simpl. Fail lia. Print Lia.lia. Print Lia.zchecker_no_abstract.
    intros.
    (*
    psatz Z 100.
    lia.

    assert (forall (v v' v'' : Z), v'' > 0 -> v < v' -> v * v'' < v' * v'').
      intros. 
      pose (H19 := H16 (v * v'') (v' * v'')); clearbody H19.
      apply H19.
      unfold Z.sub.
      apply mul_sub_distr.
    pose (H17 := H16 (i * j) ((i + k) * (j - k * 997 * j / (i * 1000 + k * 997))) (i * 1000 + k * 997)); clearbody H17.
    *)
    assert (0 < (i * 1000 + k * 997)). omega.
    (* multiplied both sides by the denominator of the divisor and needed to use comparisons between "(W / V) * V" and W *)
    pose (Zmult_nc:=(Zmult_lt_compat_r (i * j) ((i + k) * (j - k * 997 * j / (i * 1000 + k * 997))) (i * 1000 + k * 997) H2)); clearbody Zmult_nc.
  Admitted.*)