Require Import ZArith.

Require Import cclib.Maps.
Require Import token.DataTypeOps.

(* This must match the constant defined in the token.ds file, it would be
   better if the frontend generated this definition. *)
Definition _totalSupply : Z := 100000 %Z.

Definition balances_sum (d : global_abstract_data_type) : Z :=
  Int256Tree_Properties.sum (FixedSupplyToken_balances d).

Definition balances_nonnegative d :=
  forall i n,
    Int256Tree.get i (FixedSupplyToken_balances d) = Some n
    -> (0 <= n)%Z.

(* Additionally we need that all the balance entries are non-negative,
   but we will get that from the hashmap ft_cond. *)

Definition inv d :=
  balances_nonnegative d /\
  balances_sum d = _totalSupply.

Global Opaque balances_nonnegative balances_sum. (* to prevent it from being expanded when defining the VC. *)