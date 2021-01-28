(* Ethereum contracts use Keccak256 hashes a lot, both as an operation
available to the user, and in order to implement hash mappings etc.

In order for the compiled code to run correctly, we must assume that
there are no hash collissions. This is of course not provable for the
real hash implementation, so instead we use the "symbolic" model
defined in this file, where the hash functions just construct values
in an algebraic data type. Eventually, there will be a compiler phase
that replaces the symbolic hashes with the real ones, withan axiom
stating that this does not introduce collissions.
*)

Require Import cclib.Integers.
Require Import backend.IndexLib.
Require Import backend.Values.HighValues.

Definition sha_1 v :=
  Vhash v.

Definition sha_2 v1 v2 :=
  Vhash2 v1 v2.
  
Lemma sha_1_range : forall v i, sha_1 v <> Vint i.
Proof.
  intros.
  unfold sha_1.
  congruence.
Qed.

Lemma sha_2_range : forall v v' i, sha_2 v v' <> Vint i.
Proof.
  intros.
  unfold sha_2; destruct v; destruct v'; simpl;
  congruence.
Qed.

Lemma sha_1_sha_2_range : forall v v' u, sha_2 v v' <> sha_1 u.
Proof.
  intros.
  unfold sha_1, sha_2.
  destruct v; destruct v'; simpl; congruence.
Qed.
  
Lemma sha_1_injective : forall v u,
    sha_1 v = sha_1 u ->
    v = u. 
Proof.
  unfold sha_1.
  intros; congruence.
Qed.    

Lemma sha_2_injective : forall v1 v2 u1 u2,
    sha_2 v1 v2 = sha_2 u1 u2 ->
    v1 = u1 /\ v2 = u2.
Proof.
  intros.
  destruct v1; destruct v2; destruct u1; destruct u2; simpl in * ; split; inversion H; auto.
Qed.
  
(* The function must satisfy this property:
  me_query_SHA2Pointers: forall a1p a2i,
    me_query (Qcall2 Bsha_2 (Vptr a1p) (Vint a2i)) = (Vptr (Ihash a1p a2i))
*)
