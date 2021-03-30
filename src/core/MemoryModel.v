Require Import ZArith.
Require Import backend.Values.HighValues.
Require Import backend.MemoryModel.
Require Export backend.AbstractData.
 
Class MemoryModelOps (mem: Type) := {
 empty: mem;
}.
Require Export DeepSpec.lib.Lens.
Section LAYER_SPEC_CLASSES.
Class LayerSpecClass : Type := {
  
  
  memModelOps :> MemoryModelOps mem;
  
  GetHighData : Type;
  GetLowData := GetHighData
}.
Context`{LayerSpec : LayerSpecClass}.
Class CompatRelOps (D1 D2: compatdata) :=
  {
    relate_AbData: meminj -> cdata_type D1 -> cdata_type D2 -> Prop;
    match_AbData: cdata_type D1 -> mem -> meminj -> Prop;
    new_glbl: list AST.ident
  }.
Class CompatRel D1 D2 `{ops: !CompatRelOps D1 D2} :=
  {
    
  }.
Class compatrel D1 D2 :=
  {
    crel_ops :> CompatRelOps D1 D2;
    crel_prf :> CompatRel D1 D2
  }.
Class OverlaySpecClass : Type := {
  
  
  cdataOpsHigh      :> CompatDataOps GetHighData;
  cdataHigh         :> CompatData GetHighData;
  GetHighDataX      : compatdata
                    := @cdata GetHighData _ _
 
}.
Class UnderlaySpecClass : Type := {
  cdataOpsLow       : CompatDataOps GetLowData;
  cdataLow          : CompatData GetLowData;
  GetLowDataX       : compatdata
                    := cdata  GetLowData ;
  MemLow            := mwd GetLowDataX; 
   
}.
Context`{OverlaySpec : !OverlaySpecClass}.
Context`{UnderlaySpec : !UnderlaySpecClass}.
Class HyperMem : Type := {
  Hcompatrel :> compatrel GetHighDataX GetLowDataX;
  mem_match j (d : GetHighData)(ml : MemLow) :=
    relate_AbData j d (snd ml) /\ match_AbData d (fst ml) j
}.
Context`{HM : !HyperMem}.
End LAYER_SPEC_CLASSES.
