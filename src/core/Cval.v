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

(** Defining the "extended C value" [cval], its operations, and its relation
    with the vanilla C value type [val]. *)
(** This file defines a cval layer above CompCert C memory model. It also
  * defines some operators for cval, and proves simulation relation between
  * these two layers for each operator. *)

(* Standard library modules *)
Require Import BinInt.  (* Z_scope, (_ | _) *)

Require Import cclib.Coqlib.
Require Import cclib.Integers.
Require Import cclib.Maps.
Require Import cclib.Errors.
Require Import backend.Cop.
Require Import backend.Ctypes.
Require Import backend.Values.HighValues.
Require Import backend.Environments.ExtEnv.

Notation tint := (Tint I256 Unsigned).
Notation tchar := (Tint I8 Unsigned).
Notation tchar_pointer := (Tpointer tchar).

Inductive cval : Type :=
  | CVval : val -> cval
  | CVany : cval (* corresponds to any C value, can be refined by any, even undefined value *)
  | CVstruct : cstruct_val -> cval
  | CVarray : carray_val -> cval
  | CVhashmap : chashmap_val -> cval

with cstruct_val : (*fieldlist ->*) Type :=
  | CSmap (map : PTree.t cval)

with carray_val : Type :=
  | CAmap (map : ZMap.t cval)

with chashmap_val : Type :=
   | CHmap (map : Int256Map.t cval)
.

Definition cval_unary_operation op (v: cval) t : option cval :=
  match v with
  | CVval v' => match sem_unary_operation op v' t with
    | Some v'' => Some (CVval v'')
    | None => None
    end
  | _ => None
  end.

Definition cval_binary_operation (*`{Mem.MemoryModelOps}*)
    op cv1 t1 cv2 t2 : option cval :=
  match cv1, cv2 with
  | CVval v1, CVval v2 =>
    (*if is_pointer_comparison op t1 t2 then None
    else*)
    match sem_binary_operation op v1 t1 v2 t2 (*Mem.empty*) with
    | Some v => Some (CVval v)
    | None => None
    end
  | _, _ => None
  end.

Definition cval_field_access f cv t : option cval :=
  match cv, t with
  | CVstruct (CSmap map), Tstruct _ flds =>
    match struct_field flds f with
    | Some _ =>  PTree.get f map
    | _ => None
    end
  | _, _ => None
  end.

Definition cval_field_update f cv t newfield : option cval :=
  match cv, t with
  | CVstruct (CSmap map), Tstruct _ flds =>
    (* match field_offset f flds with
    | OK _ => *) Some (CVstruct (CSmap (PTree.set f newfield map)))
(*    | _ => None
    end*)
  | _, _ => None
  end.

Definition cval_array_indexing idx cv t : option cval :=
  match cv, t with
  | CVarray (CAmap map), Tarray _ n =>
    if zle 0 idx && zlt idx n then
      Some (ZMap.get idx map)
    else
      None
  | _, _ => None
  end.

Definition cval_array_update idx cv t newelem : option cval :=
  match cv, t with
  | CVarray (CAmap map), Tarray _ n  =>
    if zle 0 idx && zlt idx n then
      Some (CVarray (CAmap (ZMap.set idx newelem map)))
    else
      None
  | _, _ => None
  end.

Definition cval_hashmap_lookup idx cv t : option cval :=
  match cv, t with
  | CVhashmap (CHmap map),  Thashmap tint _ =>
      Some (Int256Map.get idx map)
  | _, _ => None
  end.

Definition cval_hashmap_update idx cv t newelem : option cval :=
  match cv, t with
  | CVhashmap (CHmap map), Thashmap tint n  =>
      Some (CVhashmap (CHmap (Int256Map.set idx newelem map)))
  | _, _ => None
  end.

Inductive cval_basic : cval -> Prop :=
  | CVBint : forall i, cval_basic (CVval (Vint i))
  .

Inductive cval_basic_match : val -> cval -> Prop :=
| CVBMval_int : forall i, cval_basic_match (Vint i) (CVval (Vint i))
| CVBMany : forall v, cval_basic_match v CVany
  .

  Inductive cval_match : val -> cval -> Prop :=
  | CVMval : forall v,  cval_match v (CVval v)
      (* ^^ In the C backend there is an extra condition that if v is a pointer
            the identifier must be less than "glob_threshold", 100000. *)
  | CVMany : forall v, cval_match v CVany.

  Definition is_immediate_type (ty : type) : bool :=
    match ty with
    | Tint _ _ => true
    | _ => false
    end.

  Inductive cval_match_indirect (m : ext_env)
      : type -> ident_ext -> cval -> Prop :=
  | CVMIval : forall ty b v cv,
      is_immediate_type ty = true ->
      IdentExtMap.get b m = EEVal v ->
      cval_match v cv ->
      cval_match_indirect m ty b cv
  | CVMIstruct : forall i flds b vs,
    cval_match_struct m flds b vs ->
    cval_match_indirect m (Tstruct i flds) b (CVstruct vs)
  | CVMIarray : forall ty n b vs,
    cval_match_array m ty b vs n ->
    cval_match_indirect m (Tarray ty n) b  (CVarray vs)
  | CVMIhash : forall ty  b vs,
    cval_match_hash_int m ty b vs ->
    cval_match_indirect m (Thashmap tint ty) b (CVhashmap vs)

  with cval_match_struct (m : ext_env)
      : fieldlist -> ident_ext -> cstruct_val -> Prop :=
  | CFMmapped : forall flds b map, (* struct's head address is b *)
      (forall i ofs ty,
      struct_field flds i = Some (ofs, ty) -> (* ofs is just the count number of field. TODO: maybe we don't need it at all? *)
      exists cv,
        PTree.get i map = Some cv /\
        cval_match_indirect m ty (Field b i) cv) ->
    cval_match_struct m flds b (CSmap map)

  with cval_match_array (m : ext_env)
      : type -> ident_ext -> carray_val -> Z -> Prop :=
  | CAMmapped : forall ty b map n,
    (forall idx, 0 <= idx < n ->
      cval_match_indirect m ty (Index b (Int256.repr idx))
                          (ZMap.get idx map)) ->
    cval_match_array m ty b (CAmap map) n

  with cval_match_hash_int (m : ext_env)
      : type -> ident_ext -> chashmap_val -> Prop :=
  | CHIMmapped : forall ty b map,
    (forall idx, 
      cval_match_indirect m ty (Index b idx)
                          (Int256Map.get idx map)) ->
    cval_match_hash_int m ty b (CHmap map) .

  Lemma cval_basic_match_cval_match v cv :
    cval_basic_match v cv -> cval_match v cv.
  Proof.
    inversion 1; constructor; reflexivity.
  Qed.

  (* v1 ---sem_unary---> v2
     |                   |
 cval_match          cval_match
     |                   |
     cv1 --cval_unary--> cv2 *)
  Lemma cval_sem_unary_operation op v1 cv1 cv2 t :
    cval_match v1 cv1 ->
    cval_unary_operation op cv1 t = Some cv2 ->
    exists v2, sem_unary_operation op v1 t = Some v2 /\ cval_match v2 cv2.
  Proof.
    inversion 1 as [v1i | ?]; subst; clear H.
    - destruct (sem_unary_operation op v1 t) as [ vi |] eqn:unary_eq; try discriminate.
      + simpl.
        rewrite unary_eq.
        injection 1 as <-.
        eexists.
        split; [reflexivity|].
        constructor.
      + simpl.
        rewrite unary_eq.
        intros; congruence.
    - destruct (sem_unary_operation op v1 t) as [ vi |] eqn:unary_eq; try discriminate.
  Qed.

  Ltac TrivialExists :=
    match goal with
    | |- exists v', Some ?v = Some v' /\ _ => exists v; split; auto
    | |- exists v', (forall _, Some ?v = Some v') /\ _ => exists v; split; auto
    | _ => idtac
    end.

  (* v1,v2 ---sem_binary---> v
     |                       |
 cval_match              cval_match
     |                       |
    cv1,cv2 --cval_binary--> cv *)
  Lemma cval_sem_binary_operation op v1 v2 cv1 cv2 cv t1 t2 :
      cval_match v1 cv1 -> cval_match v2 cv2 ->
      cval_binary_operation op cv1 t1 cv2 t2 = Some cv ->
      exists v, sem_binary_operation op v1 t1 v2 t2 = Some v /\
                cval_match v cv.
  Proof.
    inversion 1 as [ v1i | ];
    inversion 1 as [ v2i | ];
    subst; try discriminate; clear H H2.
    unfold cval_binary_operation.
    destruct cv as [ vi | | | |];
      destruct (sem_binary_operation op v1 t1 v2 t2) eqn:binary_eq;
      try discriminate;
      injection 1 as ->.
    TrivialExists.
    constructor.
  Qed.

  (* cv1 ---cval_field_access f---> cv2
      |                              |
 match_indirect                 match_indirect
      |                              |
  t1, b, ofs --access field f--> t2, b, (ofs + delta) *)
  Lemma cval_sem_field_access f cv1 cv2 t1 :
      cval_field_access f cv1 t1 = Some cv2 ->
      exists id flds t2 delta,
        t1 = Tstruct id flds /\
        struct_field flds f = Some (delta, t2) /\
        forall m b,
          cval_match_indirect m t1 b cv1 ->
          cval_match_indirect m t2 (Field b f) cv2.
  Proof.
    unfold cval_field_access.
    destruct cv1 as [| | | [] |]; try discriminate.
    destruct c as [map].
    destruct t1 as [| | | | | | tid flds  | |]; simpl; try discriminate.
    destruct (struct_field flds f) as [[delta t2]|] eqn:offset_eq; try discriminate.
    intros cv2_eq.

    exists tid, flds, t2, delta.
    repeat split; try assumption.

    intros m b match_indir.
    inversion match_indir as [| ? ? ? ? match_struct | |];
              try match goal with [ H: is_immediate_type _ = true |- _] => now inversion H end; subst.
    inversion match_struct as [ ? ? ? match_field ]; subst.
    destruct (match_field _ _ _ offset_eq)
      as (cv' & cv'_eq & match_indir').
    rewrite cv2_eq in cv'_eq.
    injection cv'_eq as ->.
    exact match_indir'.
  Qed.

  Require Import lib.ArithInv.

  (* cv1 ---cval_array_indexing idx---> cv2
      |                                  |
 match_indirect                     match_indirect
      |                                  |
  t1, b, ofs -- access index idx --> t2, b, (ofs + idx * sizeof t2) *)
  Lemma cval_sem_array_indexing idx cv1 cv2 t1 :
      cval_array_indexing idx cv1 t1 = Some cv2 ->
      exists t2 n,
        t1 = Tarray t2 n /\
        forall m b,
          cval_match_indirect m t1 b cv1 ->
          cval_match_indirect m t2 (Index b (Int256.repr idx)) cv2.
  Proof.
    unfold cval_array_indexing.
    destruct cv1 as [| | | | []]; try discriminate.
    destruct c as [map].
    destruct t1 as [| | | | | t2 n | | |]; try discriminate.
    destruct (zle 0 idx && zlt idx z) eqn:bound_check; try discriminate.
    intros cv2_eq; injection cv2_eq as <-.   
    exists t1, z.
    repeat split; try assumption.

    intros m b match_indir.
    inversion match_indir as [| | ? ? ? ? match_array |];
      try match goal with [ H: is_immediate_type  _ = true |- _] => now inversion H end; subst.
    inversion match_array as [ ? ? ? ? ? bounded match_index ]; subst.
    apply H.
    inv_arith.
    split; auto.
  Qed.

  Lemma cval_sem_hashmap_lookup idx cv1 cv2 t1 :
      cval_hashmap_lookup idx cv1 t1 = Some cv2 ->
      exists t2,
        t1 = Thashmap tint t2 /\
        forall m b,
          cval_match_indirect m t1 b cv1 ->
          cval_match_indirect m t2 (Index b idx) cv2.
  Proof.
    unfold cval_hashmap_lookup.
    destruct cv1 as [| | | | []]; try discriminate.
    (* destruct c as [map]. *)
    destruct t1 as [| | | | t1 t2 | | | | ]; try discriminate.
    destruct t1; try discriminate.
    destruct i; try discriminate.
    destruct s; try discriminate.
    intros cv2_eq; injection cv2_eq as <-.   
    exists t2.
    repeat split; try assumption.

    intros m b match_indir.
    inversion match_indir;
      try match goal with [ H: is_immediate_type  _ =  true |- _] => now inversion H end; subst.
    inversion H2 as [ ? ? ? match_hash]; subst.
    apply match_hash.
  Qed.

  Lemma cval_match_indirect_eq m m' i ty cv :
      cval_match_indirect m ty i cv ->
      (forall i',
          ident_ext_extends i i' ->
          IdentExtMap.get i' m = IdentExtMap.get i' m') ->
      cval_match_indirect m' ty i cv
  with cval_match_struct_eq m m' flds i csv :
      cval_match_struct m flds i csv ->
      (forall i',
          ident_ext_extends i i' ->
          IdentExtMap.get i' m = IdentExtMap.get i' m') ->
      cval_match_struct m' flds i csv
  with cval_match_array_eq m m' i ty cav n :
         cval_match_array m ty i cav n ->
      (forall i',
          ident_ext_extends i i' ->
          IdentExtMap.get i' m = IdentExtMap.get i' m') ->         
      cval_match_array m' ty i cav n
  with cval_match_hash_int_eq m m' i ty chv :
         cval_match_hash_int m ty i chv ->
      (forall i',
          ident_ext_extends i i' ->
          IdentExtMap.get i' m = IdentExtMap.get i' m') ->         
      cval_match_hash_int m' ty i chv.  
  Proof.
  { intros match_indirect load_eq.
    destruct match_indirect; subst.
    - rewrite load_eq in H0 by (apply IIE_refl).
      apply CVMIval with v; eauto.
    - apply CVMIstruct, cval_match_struct_eq with m; assumption.
    - apply CVMIarray, cval_match_array_eq with m; assumption.
    - apply CVMIhash, cval_match_hash_int_eq with m; assumption.
  }
  { intros match_struct load_eq.
    destruct match_struct as [ ? ? ? match_indir ].
    constructor; try assumption.
    intros ? ? ? offset_eq.
    destruct (match_indir _ _ _ offset_eq)
      as (cv' & cv'_eq & cv'_match).
    exists cv'; split.
    - assumption.
    - apply cval_match_indirect_eq with m.
      + apply cv'_match.
      + intros i' Hextends.
        apply load_eq.
        apply ident_ext_extends_inversion_Field in Hextends.
        exact Hextends.
  }
  { intros match_array load_eq.
    destruct match_array as [ ? ? ? ? match_indirect ].
    constructor.
    intros idx idx_range.
    apply cval_match_indirect_eq with m.
    + apply match_indirect; assumption.
      + intros i' Hextends.
        apply load_eq.
        apply ident_ext_extends_inversion_Index in Hextends.
        exact Hextends.
  }
  { intros match_hash_int load_eq.
    destruct match_hash_int as [ ? ? ? match_indirect ].
    constructor.
    intros idx.
    apply cval_match_indirect_eq with m.
    + apply match_indirect; assumption.
    + intros i' Hextends.
      apply load_eq.
      apply ident_ext_extends_inversion_Index in Hextends.
      exact Hextends.
  }
Qed.

