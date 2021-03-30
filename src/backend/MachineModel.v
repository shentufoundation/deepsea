(* The EVM allows contracts to query lots of things
about itself and about the blockchain.
These are the "builtin" expresions.
We treat these like global variable lookups, because
the only way to access the values at any level is fixed.
For example, to compute caller you need to use the
"caller" builtin expression up in DeepSEA, and this must
compile into a "CALLER" command in the bytecode.
*)

Require Import cclib.Coqlib.
Require Import cclib.Maps.
Require Import cclib.Integers.
Require Import backend.IndexLib.
Require Import backend.Values.LowValues.
Require Import backend.Values.HighValues.
Require Import backend.Environments.ExtEnv.
Require Import backend.AbstractData.

(* Defines the set of builtin expressions in the EVM. 
   They are split into datatypes based on the number of arguments they take.
*)
Inductive builtin0: Type :=
| Baddress: builtin0
| Borigin: builtin0
| Bcaller: builtin0
| Bcallvalue: builtin0
| Bcoinbase: builtin0
| Btimestamp: builtin0
| Bnumber: builtin0
| Bchainid: builtin0
| Bselfbalance: builtin0.

Inductive builtin1: Type :=
| Bbalance: builtin1
| Bblockhash: builtin1.

Inductive state_query : Type :=
| Qcall0: builtin0 -> state_query
| Qcall1: builtin1 -> val -> state_query.

(* Each language semantics has a machine_env (in fact, the same one)
which specifies the result of doing the EVM builtin operations.

For builtin queries the machine_env contains the information, and
below we define a relation me_query which presents this in a uniform
way to the rest of the compiler correctness proof.

The machine_env currently contains two abstract relations for
transfers and methods calls to other contracts. These are mostly
placeholders, they will probably need some redesign to be useful to
write code about contracts. 

The entire backend is parametrized on an abstract data type which
keeps state related to transfers and method calls, to be filled in 
at a later time.
 *)

Section WITH_DATA.
Context adata {data_ops: CompatDataOps adata}.

Record machine_env  : Type := mkmachine {
  me_address : int256;  (* Todo: make it int160. *)
  me_origin : int256;   (* Todo: make it int160. *)                     
  me_caller : int256;
  me_callvalue : int256;
  me_coinbase : int256;
  me_timestamp : int256;
  me_number : int256;
  me_chainid : int256;
  me_selfbalance : int256;
  me_balance : int256 -> int256;   (* Todo: make it int160. *)
  me_blockhash : int256 -> int256;

  (* todo: this is bad because it doesn't deal with potential reentrancy. *)
  (* Returns an int representing success/failure, and the new abstract state. *)
  me_transfer : forall (addr value: int256)(d: adata), (int256 * adata);
  (* addr, sig, value, args, prev_data, prev_storage, new_data, new_storage, success, retvals *)
  me_callmethod : val -> int -> val -> list val -> adata -> ext_env -> adata -> ext_env -> int256 -> list val -> Prop;
  me_log : forall (topics : list val) (args : list val), adata -> adata
}.

Definition me_query (me : machine_env) (q: state_query) : val :=
  match q with
  | Qcall0 Baddress => Vint (me_address me)
  | Qcall0 Borigin => Vint (me_origin me)
  | Qcall0 Bcaller => Vint (me_caller me)
  | Qcall0 Bcallvalue => Vint (me_callvalue me)
  | Qcall0 Bcoinbase => Vint (me_coinbase me)
  | Qcall0 Btimestamp => Vint (me_timestamp me)
  | Qcall0 Bnumber => Vint (me_number me)
  | Qcall0 Bchainid => Vint (me_chainid me)
  | Qcall0 Bselfbalance => Vint (me_selfbalance me)
  | Qcall1 Bbalance (Vint addr) => Vint (me_balance me addr)
  | Qcall1 Bbalance _ => Vunit (* ill-typed query. *)
  | Qcall1 Bblockhash (Vint n) => Vint (me_blockhash me n)
  | Qcall1 Bblockhash _ => Vunit (* ill-typed query. *)
  end.
  
(*
(* This is now done with a relation *)

Module QueryIndexed <: INDEXED_TYPE.

Definition t := state_query.

Definition index (q: state_query) : positive :=
  match q with
  | Qsha_1 v => xO (xO (xO (xO (xO (int_index v)))))
  | Qsha_2 v1 v2 => xO (xO (xO (xO (xI (inject_positive (int_index v1) (int_index v2))))))
  | Qaddress => xO (xO (xO (xI (xO xH))))
  | Qbalance v => xO (xO (xO (xI (xI (int_index v)))))
  | Qorigin => xO (xO (xI (xO (xO xH))))
  | Qcaller => xO (xO (xI (xO (xI xH))))
  | Qcallvalue => xO (xO (xI (xI (xO xH))))
  | Qcalldataload v => xO (xO (xI (xI (xI (int_index v)))))
  | Qcalldatasize => xO (xI (xO (xO (xO xH))))
  | Qcodesize => xO (xI (xO (xO (xI xH))))
  | Qgasprice => xO (xI (xO (xI (xO xH))))
  | Qextcodesize v => xO (xI (xO (xI (xI (int_index v)))))
  | Qblockhash v => xO (xI (xI (xO (xO (int_index v)))))
  | Qcoinbase => xO (xI (xI (xO (xI xH))))
  | Qtimestamp => xO (xI (xI (xI (xO xH))))
  | Qnumber => xO (xI (xI (xI (xI xH))))
  | Qdifficulty => xI (xO (xO (xO (xO xH))))
  | Qgaslimit => xI (xO (xO (xO (xI xH))))
  | Qgas => xI (xO (xO (xO (xI (xO xH)))))
  end.

Lemma index_inj: forall q r, index q = index r -> q = r.
Proof.
destruct q; destruct r; simpl; try discriminate; try reflexivity;
  intro H; injection H; clear H; intro H.
-
  replace i0 with i; try reflexivity; apply int_index_injective; exact H.
-
  replace i1 with i. replace i2 with i0.
  reflexivity.
  apply int_index_injective.
  apply injective_positive_2 with (int_index i) (int_index i1).
  exact H.
  apply int_index_injective.
  apply injective_positive_1 with (int_index i0) (int_index i2).
  exact H.
-
  replace i0 with i; try reflexivity; apply int_index_injective; exact H.
-
  replace i0 with i; try reflexivity; apply int_index_injective; exact H.
-
  replace i0 with i; try reflexivity; apply int_index_injective; exact H.
-
  replace i0 with i; try reflexivity; apply int_index_injective; exact H.
Qed.

Lemma eq: forall (x y: state_query), {x = y} + {x <> y}.
Proof.
intros. decide equality; apply Int256.eq_dec.
Qed.

End QueryIndexed.

Module StateMap := IMap(QueryIndexed).

Definition state_env := StateMap.t int256.

*)

End WITH_DATA.

Arguments  me_address {adata}.
Arguments  me_origin {adata}.
Arguments  me_caller {adata}.
Arguments  me_callvalue {adata}.
Arguments  me_coinbase {adata}.
Arguments  me_timestamp {adata}.
Arguments  me_number {adata}.
Arguments  me_chainid {adata}.
Arguments  me_selfbalance {adata}.
Arguments  me_balance {adata}.
Arguments  me_blockhash {adata}.
Arguments  me_transfer {adata}.
Arguments  me_callmethod {adata}.
Arguments  me_query {adata}.
Arguments me_log {adata}.
