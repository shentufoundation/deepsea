(* This code is adapted from liblayers.compcertx.AbstractData and
   liblayers.compcertx.MemWithData.
    
   We should revisit it when we port liblayers to work with
   DeepSEA/ethereum, but it shold be enough as a placeholder for
   now. *)

Require Import ZArith.
Require Import backend.Values.LowValues.
Require Import backend.MemoryModel.
Require Import backend.Environments.ExtEnv.

(* In CompcertX, the type of memory is abstract and there is a
   typeclass MemoryModelOps with operations on it, but for our
   purposes we can just define it concretely. *) 
Definition mem := ext_env.  


Class CompatDataOps data :=
  {
    empty_data : data;
    high_level_invariant: data -> Prop
  }.

Class CompatData data `{data_ops: CompatDataOps data} :=
  {
    empty_data_high_level_invariant:
      high_level_invariant empty_data
  }.

Record compatdata :=
  {
    cdata_type : Type;
    cdata_ops : CompatDataOps cdata_type;
    cdata_prf : CompatData cdata_type
  }.

Global Existing Instance cdata_ops.
Global Existing Instance cdata_prf.

Definition cdata := Build_compatdata.
Global Arguments cdata _ {_ _}.

Definition mwd (D: compatdata) := (mem * cdata_type D)%type.


