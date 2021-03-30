(* Skeleton by Edgser for spblind.ds *)
Require Import BinPos.
Require Import DeepSpec.Runtime.
Require Import spblind.EdsgerIdents.
Require Import spblind.DataTypes.
Require Import spblind.DataTypeOps.

Section EdsgerGen.


(* (* commented out because the Ethereum backend doesn't do any proofs. *)
Global Instance tint_U_hyper_arg_ret : HyperArgRet tint_U.
Proof. esplit.
  - (* ht_basic *) admit.
  - (* ht_cast_ident *) admit.
  - (* ht_injective *) admit.
  - (* ht_inject_refl *) admit.
  - (* ht_has_type *) admit.
Qed.
*)

(* (* commented out because the Ethereum backend doesn't do any proofs. *)
Global Instance tint_hashvalue_hyper_arg_ret : HyperArgRet tint_hashvalue.
Proof. esplit.
  - (* ht_basic *) admit.
  - (* ht_cast_ident *) admit.
  - (* ht_injective *) admit.
  - (* ht_inject_refl *) admit.
  - (* ht_has_type *) admit.
Qed.
*)

End EdsgerGen.
