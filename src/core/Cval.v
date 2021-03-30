Require Import BinInt.  
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
  | CVany : cval 
  | CVstruct : cstruct_val -> cval
  | CVarray : carray_val -> cval
  | CVhashmap : chashmap_val -> cval
with cstruct_val :  Type :=
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
Definition cval_binary_operation 
    op cv1 t1 cv2 t2 : option cval :=
  match cv1, cv2 with
  | CVval v1, CVval v2 =>
    
    match sem_binary_operation op v1 t1 v2 t2  with
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
     Some (CVstruct (CSmap (PTree.set f newfield map)))
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
  | CFMmapped : forall flds b map, 
      (forall i ofs ty,
      struct_field flds i = Some (ofs, ty) -> 
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
  Proof. Admitted.
  
  Lemma cval_sem_unary_operation op v1 cv1 cv2 t :
    cval_match v1 cv1 ->
    cval_unary_operation op cv1 t = Some cv2 ->
    exists v2, sem_unary_operation op v1 t = Some v2 /\ cval_match v2 cv2.
  Proof. Admitted.
  Ltac TrivialExists :=
    match goal with
    | |- exists v', Some ?v = Some v' /\ _ => exists v; split; auto
    | |- exists v', (forall _, Some ?v = Some v') /\ _ => exists v; split; auto
    | _ => idtac
    end.
  
  Lemma cval_sem_binary_operation op v1 v2 cv1 cv2 cv t1 t2 :
      cval_match v1 cv1 -> cval_match v2 cv2 ->
      cval_binary_operation op cv1 t1 cv2 t2 = Some cv ->
      exists v, sem_binary_operation op v1 t1 v2 t2 = Some v /\
                cval_match v cv.
  Proof. Admitted.
  
  Lemma cval_sem_field_access f cv1 cv2 t1 :
      cval_field_access f cv1 t1 = Some cv2 ->
      exists id flds t2 delta,
        t1 = Tstruct id flds /\
        struct_field flds f = Some (delta, t2) /\
        forall m b,
          cval_match_indirect m t1 b cv1 ->
          cval_match_indirect m t2 (Field b f) cv2.
  Proof. Admitted.
  Require Import lib.ArithInv.
  
  Lemma cval_sem_array_indexing idx cv1 cv2 t1 :
      cval_array_indexing idx cv1 t1 = Some cv2 ->
      exists t2 n,
        t1 = Tarray t2 n /\
        forall m b,
          cval_match_indirect m t1 b cv1 ->
          cval_match_indirect m t2 (Index b (Int256.repr idx)) cv2.
  Proof. Admitted.
  Lemma cval_sem_hashmap_lookup idx cv1 cv2 t1 :
      cval_hashmap_lookup idx cv1 t1 = Some cv2 ->
      exists t2,
        t1 = Thashmap tint t2 /\
        forall m b,
          cval_match_indirect m t1 b cv1 ->
          cval_match_indirect m t2 (Index b idx) cv2.
  Proof. Admitted.
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
  Proof. Admitted.
