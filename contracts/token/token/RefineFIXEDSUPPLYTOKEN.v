(* This should be automatically generated, but I put in a manually written version for now. *)

Require Import BinNums.
Require Import DeepSpec.core.HyperTypeInst.
Require Import DeepSpec.Runtime.
Require Import DeepSpec.Linking.
Require Import token.EdsgerIdents.
Require Import token.DataTypes.
Require Import token.DataTypeOps.
Require Import token.DataTypeProofs.

Require Import token.LayerFIXEDSUPPLYTOKEN.


(*
Context {mem}`{Hmem: Mem.MemoryModel mem}.
Context`{Hmwd: UseMemWithData mem}.
Context`{make_program_ops: !MakeProgramOps Clight.function Ctypes.type Clight.fundef Ctypes.type}.
Context`{Hmake_program: !MakeProgram Clight.function Ctypes.type Clight.fundef Ctypes.type}.
Context`{global_abdata : !GlobalAbData init_global_abstract_data global_low_level_invariant}.

Context {memModelOps : MemoryModelOps mem}.
 *)

Existing Instances GlobalLayerSpec FIXEDSUPPLYTOKEN_overlay_spec FIXEDSUPPLYTOKEN_underlay_spec.

Context`{CTXT_prf : !Layer_FIXEDSUPPLYTOKEN_Context_prf}.

(*
Record relate_RData (j : meminj) (habd : GetHighData) (labd : GetLowData) : Prop
    := mkrelate_RData {
}.*)

Record match_RData (habd : GetHighData) (m : mem) (j : MemoryModel.meminj) : Prop
    := MATCH_RDATA {
  balances_ma : variable_match FixedSupplyToken_balances_var habd m
}.

Local Hint Resolve MATCH_RDATA.

Global Instance rel_ops: CompatRelOps GetHighDataX GetLowDataX :=
{
  relate_AbData f d1 d2 := True;
  match_AbData d1 m f := match_RData d1 m f;
  new_glbl := var_FixedSupplyToken_balances_ident :: nil
}.


Global Instance rel_prf: CompatRel GetHighDataX GetLowDataX.


Existing Instance GlobalLayerSpec.

Instance FIXEDSUPPLYTOKEN_hypermem : MemoryModel.HyperMem
  := { Hcompatrel := {| crel_prf := rel_prf |} }.

