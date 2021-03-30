(* 
The MachineModelLow is exactly the same as the MachineModel,
except the me_transfer, me_callmethods, and me_log now expects
a low value instead of a high value.

*)

Require Import cclib.Coqlib.
Require Import cclib.Maps.
Require Import cclib.Integers.
Require Import backend.IndexLib.
Require Import backend.Values.LowValues.
Require Import backend.Environments.StackEnv.
Require Import backend.Environments.HashEnv.
Require Import backend.AbstractData.
Require Import backend.MachineModel.


Inductive state_query : Type :=
| Qcall0: builtin0 -> state_query
| Qcall1: builtin1 -> val -> state_query.


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

  me_transfer : forall (addr value: val)(d: adata), (int256 * adata);
  (* addr, sig, value, args, prev_data, prev_storage, new_data, new_storage, success, retvals *)
  me_callmethod : val -> int -> val -> list val -> adata -> stack_env -> hash_env -> adata -> stack_env -> hash_env -> int256 -> list val -> Prop;
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
