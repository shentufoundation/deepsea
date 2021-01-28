(* *********************************************************************)
(*    DeepSpec, the language of certified softwares                    *)
(*                                                                     *)
(*      Shu-Chun Weng, Yale University                                 *)
(*                                                                     *)
(*  Copyright (c) 2013-2015 Shu-Chun Weng <shu-chun.weng@yale.edu>.    *)
(*                                                                     *)
(*  This program is free software; you can redistribute it and/or      *)
(*  modify it under the terms of the GNU General Public License        *)
(*  version 2 as published by the Free Software Foundation.  Note that *)
(*  the only valid version of the GPL for this work is version 2, not  *)
(*  v2.2 or v3.x or whatever.                                          *)
(*                                                                     *)
(*  This program is distributed in the hope that it will be useful,    *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of     *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *)
(*  GNU General Public License for more details.                       *)
(*                                                                     *)
(*  You should have received a copy of the GNU General Public License  *)
(*  along with this program; if not, write to the Free Software        *)
(*  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,         *)
(*  MA 02110-1301 USA.                                                 *)
(* *********************************************************************)

(** Adapter module aggregating all abstractions in liblayers in CompCertX.
    For the adapter _type class_, see [HyperMem]. *)

(* CompCertX modules *)
(* Require        compcertx.common.EventsX. *)

Require Import ZArith.
Require Import backend.Values.HighValues.
Require Import backend.MemoryModel.
Require Export backend.AbstractData.
(* Require Import liblayers.compat.CompatLayers. *) (* Todo, liblayers-related *)

(** * Operations on memory states *)
(* This is copied from Compcert/CompcertX as a temporary thing, 
   but I didn't copy the corresponding axioms.
   TODO: think about which operations the generated DeepSEA code actually uses.  *)
Class MemoryModelOps (mem: Type) := {
 empty: mem;
}.

Require Export DeepSpec.lib.Lens.

Section LAYER_SPEC_CLASSES.

(* All the synthesis functions are in a context containing a LayerSpecClass.
   The programmer using DeepSpec will eventually need to provide instances for it, this
   contains all the proof obligations for the various theorems.

   Edsger will generate instances in LayerXX.v

   The LayerSpecClass is made up of a double-inheritance diamond
                      OverLaySpecClass
                   /                   \
                  /                     \
   LayerSpecClass                        HyperMem
                  \                      /
                   \                    /
                     UnderLaySpecClass /

   There is some complicated technical reason that they need to be split up, which we sadly
   no longer remember.
 *)

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

(* This contains cdata, which is the layer invariant for the abstract data.

   In fact, CompatDataOps contain three things,
     the high level invariant, which is specified by the DeepSpec programmer in the .ds source file
     "low level invariant" (misnomer) about assembly level stuff.
     kernel mode
  *)
Class OverlaySpecClass : Type := {
  
  cdataOpsHigh      :> CompatDataOps GetHighData;
  cdataHigh         :> CompatData GetHighData;
  GetHighDataX      : compatdata
                    := @cdata GetHighData _ _
 
}.

(* This contains the same thing (for the lower layer), and additionally
   GetLowLayer, which is the layer interface for the underlay (a map from
   identifiers to method semantics).
*)
Class UnderlaySpecClass : Type := {

  cdataOpsLow       : CompatDataOps GetLowData;
  cdataLow          : CompatData GetLowData;
  GetLowDataX       : compatdata
                    := cdata (*mem _ *) GetLowData (* _ _ *);
  MemLow            := mwd GetLowDataX; 

}.

Context`{OverlaySpec : !OverlaySpecClass}.
Context`{UnderlaySpec : !UnderlaySpecClass}.

(* The refinement relation (compat_rel gives relate_AbFata and match_Abdata,
    and we also define a name mem_match for talking about them together).
*)
Class HyperMem : Type := {
  Hcompatrel :> compatrel GetHighDataX GetLowDataX;
  mem_match j (d : GetHighData)(ml : MemLow) :=
    relate_AbData j d (snd ml) /\ match_AbData d (fst ml) j
}.


Context`{HM : !HyperMem}.

End LAYER_SPEC_CLASSES.
