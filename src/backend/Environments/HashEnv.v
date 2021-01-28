Require Import cclib.Coqlib.
Require Import backend.Ctypes.
Require Import cclib.Maps.
Require Import cclib.Integers.
Require Import backend.Values.LowValues.
Require Import backend.AST.
Require Import backend.IndexLib.

Inductive HashKey :=
| singleton : int256 -> HashKey
| pair : HashKey -> int256 -> HashKey.

Module HashKeyIndexed <: INDEXED_TYPE.

  Definition t := HashKey.

  Fixpoint index (hk : HashKey) : positive :=
    match hk with
    | singleton i => int_index i
    | pair hk i => inject_positive (index hk) (int_index i)
    end.

Definition index_inv (p : positive) : HashKey.
Admitted.

Lemma index_invertible : forall x, index_inv (index x) = x.
Admitted.

Lemma index_inj: forall (x y: HashKey), index x = index y -> x = y.
Proof.
Admitted.

Lemma eq: forall (x y: HashKey), {x = y} + {x <> y}.
Proof.
  induction x; destruct y; intros; decide equality;
  try decide equality; try apply Int256.eq_dec.
Qed.

End HashKeyIndexed.

Module HashKeyMap := IMap(HashKeyIndexed).

Definition hash_env : Type := HashKeyMap.t val.

Notation "he [ i ]" := (HashKeyMap.get i he) (at level 80).
Notation "he [ i |-> v ]" := (HashKeyMap.set i v he) (at level 80).

Definition empty_hash_env : hash_env :=
  (HashKeyMap.init Vzero).

Fixpoint hkOfVal (v:val) : option HashKey :=
  match v with
  | Vhash (Vint i) => Some (singleton i)
  | Vhash2 v (Vint i) => option_map (fun hk => pair hk i) (hkOfVal v)
  | _ => None
  end.

Definition read (ptr:val) (he:hash_env) : option val :=
  option_map (fun hk => he[hk]) (hkOfVal ptr).

Definition write (ptr:val) (v:val) (he:hash_env) : option hash_env :=
  option_map (fun hk => he[hk |-> v]) (hkOfVal ptr).
