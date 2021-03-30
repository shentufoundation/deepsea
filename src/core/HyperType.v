Require Import BinInt.  
Require Import Coq.Lists.List.
Require Import BinInt.    
Require Import cclib.Coqlib.
Require Import cclib.Errors.
Require Import backend.Values.HighValues.
Require Import cclib.Integers.
Require Import cclib.Maps.
Require Import backend.AST.
Require Import backend.Cop.
Require Import backend.Ctypes.
Require Import backend.MemoryModel.
Require Import backend.MachineModel.
Require Import backend.Environments.ExtEnv.
Require Import DeepSpec.core.Cval.
Require Import DeepSpec.core.MemoryModel.
Require Import DeepSpec.lib.OProp.
Require Import DeepSpec.lib.Monad.ContOptionMonad.
Inductive type_marker {T} (t : T) := create_type_marker : type_marker t.
Section HYPER_TYPE.
  
  
  Inductive type_pair : Type :=
    Tpair : Type -> type -> type_pair.
  Definition unpair_ty tp := match tp with Tpair _ ty => ty end.
  Definition unpair_ft tp := match tp with Tpair ft _ => ft end.
  
  
  Class HyperTypeImpl (tp : type_pair) : Type := {
    
    ht_cval : unpair_ft tp -> cval;
    
    ht_default : unpair_ft tp;
    
    ht_ft_cond : unpair_ft tp -> Prop;
    
    ht_ty_cond : cval -> Prop
               := fun cv => exists f, ht_ft_cond f /\ ht_cval f = cv;
    
    ht_valid_ft_cond : unpair_ft tp -> Prop;
    ht_valid_ty_cond : cval -> Prop
      := fun cv => exists f, ht_valid_ft_cond f /\ ht_ft_cond f /\ ht_cval f = cv;
    
    ht_valid_ft_ocond : OProp1 (unpair_ft tp)
  }.
  
  Class HyperType (tp : type_pair)`{hti : HyperTypeImpl tp} : Prop := {
    
    
    ht_ft_rel_core : forall f, ht_ft_cond f -> exists cv, ht_cval f = cv;
    
    ht_ft_rel : forall f, ht_ft_cond f -> exists cv, ht_ty_cond cv /\ ht_cval f = cv
      := fun f fc => match ht_ft_rel_core f fc with ex_intro cv r =>
           ex_intro _ cv (conj (ex_intro _ f (conj fc r)) r) end;
    
    ht_ty_rel : forall cv, ht_ty_cond cv -> exists f, ht_ft_cond f /\ ht_cval f = cv
      := fun cv vc => vc;
    
    ht_default_ft_cond : ht_ft_cond ht_default;
    
    ht_valid_ft_ocond_same :
      forall f, ht_valid_ft_cond f <-> oProp1 ht_valid_ft_ocond f
  }.
  
  Inductive ht_option_cval `{HyperTypeImpl} :
      option (unpair_ft tp) -> option cval -> Prop :=
  | ht_none_cval : ht_option_cval None None
  | ht_some_cval : forall f v, ht_cval f = v -> ht_option_cval (Some f) (Some v).
  Definition ht_cval_some `{HyperTypeImpl} f vo := ht_option_cval (Some f) vo.
  
  
  Definition ht_rel `{HyperTypeImpl} f v :=
    cval_match v (ht_cval f).
  Inductive ht_option_rel `{HyperTypeImpl} :
      option (unpair_ft tp) -> option val -> Prop :=
  | ht_none_rel : ht_option_rel None None
  | ht_some_rel : forall f v, ht_rel f v -> ht_option_rel (Some f) (Some v).
  Definition ht_rel_some `{HyperTypeImpl} f vo :=
    ht_option_rel (Some f) vo.
  
  
  
  Class HyperByValueType (tp : type_pair) `{hti : HyperTypeImpl tp} : Prop := {
    ht_by_value_access_mode : is_immediate_type (unpair_ty tp) = true;
    
    
  }.
  
  
  
  
  Global Arguments ht_ty_cond tp {_} v.
  Global Arguments ht_valid_ty_cond tp {_} cv.
  
  Class hyper_type_pair : Type := mk_hyper_type_pair {
    _tp_type_pair      : type_pair;
    _tp_ty             := unpair_ty _tp_type_pair;
    _tp_ft             := unpair_ft _tp_type_pair;
    tp_hyper_type_impl :> HyperTypeImpl _tp_type_pair
  }.
  Definition tp_type_pair := @_tp_type_pair.
  Definition tp_ty        := @_tp_ty.
  Definition tp_ft        := @_tp_ft.
  
  Global Arguments mk_hyper_type_pair _ {_}.
  
  
  
  
  
  
  Context `{LayerSpec : LayerSpecClass}.
  
  Class HyperBuiltin0Impl (tp  : type_pair) : Type := {
    Hquery0 : machine_env GetHighData -> unpair_ft tp;
    Hbuiltin0 : MachineModel.builtin0
  }.
  Class HyperBuiltin0 (tp  : type_pair) 
    `{hti : HyperTypeImpl tp, !HyperBuiltin0Impl tp} : Prop := {
    Hbuiltin0_returns : forall me,
      ht_ft_cond (Hquery0 me);
    Hbuiltin0_correct : forall me,
      ht_cval (Hquery0 me) = (CVval (me_query me (Qcall0 Hbuiltin0)));
  }.
  Class HyperBuiltin1Impl (arg_tp tp  : type_pair) : Type := {
    Hquery1 : machine_env GetHighData -> unpair_ft arg_tp -> unpair_ft tp;
    Hbuiltin1 : MachineModel.builtin1
  }.
  Class HyperBuiltin1 (arg_tp tp  : type_pair) 
    `{arg_hti : HyperTypeImpl arg_tp, hti : HyperTypeImpl tp, !HyperBuiltin1Impl arg_tp tp} : Prop := {
    Hbuiltin1_returns : forall me f,
      ht_ft_cond f ->
      ht_ft_cond (Hquery1 me f);
    Hbuiltin1_correct : forall me f v,
      ht_ft_cond f ->
      ht_cval f = v ->
      exists v',
        v = CVval v' /\
        ht_cval (Hquery1 me f) = CVval (me_query me (Qcall1 Hbuiltin1 v'));
  }.
  
  Class HyperUnaryImpl (op : unary_operation)(tp tpo : type_pair) : Type := {
    
    Hunary_cond : unpair_ft tp -> Prop;
    Hunary_ocond : OProp1 (unpair_ft tp);
    
    Hunary : unpair_ft tp -> unpair_ft tpo;
  }.
  Class HyperUnaryOp (op : unary_operation)(tp tpo : type_pair)
     `{hti : HyperTypeImpl tp, htio : HyperTypeImpl tpo, !HyperUnaryImpl op tp tpo}
      : Prop := {
    Hunary_ocond_same : forall f, Hunary_cond f <-> oProp1 Hunary_ocond f;
    
    Hunary_returns : forall f, ht_ft_cond f -> Hunary_cond f ->
      ht_ft_cond (Hunary f);
    Hunary_correct : forall f v, ht_ft_cond f -> Hunary_cond f -> ht_cval f = v ->
      ht_cval_some (Hunary f) (cval_unary_operation op v (unpair_ty tp));
  }.
  Class HyperUnaryPassthrough op (tp tpo : type_pair)
     `{hti : HyperTypeImpl tp, htio : HyperTypeImpl tpo, !HyperUnaryImpl op tp tpo}
     : Prop := {
    
  }.
  
  Class HyperBinaryImpl (op : binary_operation)(tpl tpr tpo : type_pair)
      : Type := {
    
    Hbinary_cond : unpair_ft tpl -> unpair_ft tpr -> Prop;
    Hbinary_ocond : OProp2 (unpair_ft tpl) (unpair_ft tpr);
    Hbinary : unpair_ft tpl -> unpair_ft tpr -> unpair_ft tpo;
  }.
  Class HyperBinaryOp (op : binary_operation)(tpl tpr tpo : type_pair)
     `{htil : HyperTypeImpl tpl, htir : HyperTypeImpl tpr,
       htio : HyperTypeImpl tpo, !HyperBinaryImpl op tpl tpr tpo} : Prop := {
    Hbinary_ocond_same : forall f f', Hbinary_cond f f' <->
                                      oProp2 Hbinary_ocond f f';
    
    Hbinary_returns : forall f f', ht_ft_cond f -> ht_ft_cond f' ->
      Hbinary_cond f f' -> ht_ft_cond (Hbinary f f');
    Hbinary_correct : forall f f' v v',
      ht_ft_cond f -> ht_ft_cond f' -> Hbinary_cond f f' ->
      ht_cval f = v -> ht_cval f' = v' ->
      ht_cval_some (Hbinary f f') (cval_binary_operation op v (unpair_ty tpl)
                                                            v' (unpair_ty tpr))
  }.
  Class HyperBinaryPassthrough op (tpl tpr tpo : type_pair)
     `{htil : HyperTypeImpl tpl, htir : HyperTypeImpl tpr,
       htio : HyperTypeImpl tpo, !HyperBinaryImpl op tpl tpr tpo} : Prop := {
    
  }.
End HYPER_TYPE.
Add Printing Constructor hyper_type_pair.
Section HLIST.
  Inductive HList (F : hyper_type_pair -> Type) :
      list hyper_type_pair -> Type :=
  | HNil  : HList F nil
  | HCons : forall (x : hyper_type_pair)(ls : list hyper_type_pair),
              F x -> HList F ls -> HList F (x :: ls).
  Global Arguments HNil {_}.
  Global Arguments HCons {_} _ _ _ _.
  Definition hlist_hd {a b F} (hlist : HList F (a :: b)) : F a :=
    match hlist in HList _ x return match x with
                                    | nil => unit
                                    | cons a _ => F a
                                    end with
    | HNil => tt
    | HCons _ _ x _ => x
    end.
  Definition hlist_tl {a b F} (hlist : HList F (a :: b)) : HList F b :=
    match hlist in HList _ x return match x with
                                    | nil => unit
                                    | cons _ ls => HList F ls
                                    end with
    | HNil => tt
    | HCons _ _ _ x => x
    end.
  Fixpoint HList_map_nodep {a F A}
    (f : forall htp : hyper_type_pair, F htp -> A)(hlist : HList F a) :
      list A :=
    match hlist with
    | HNil => nil
    | HCons x _ y hl => cons (f x y) (HList_map_nodep f hl)
    end.
  Fixpoint HList_map {a F G}
    (f : forall htp : hyper_type_pair, F htp -> G htp)(hlist : HList F a) :
      HList G a :=
    match hlist with
    | HNil => HNil
    | HCons x _ y hl => HCons _ _ (f x y) (HList_map f hl)
    end.
  Fixpoint HList_fold_right_nodep {a F A}
      (f : forall htp : hyper_type_pair, F htp -> A -> A)(a0 : A)
      (hlist : HList F a) : A :=
    match hlist with
    | HNil => a0
    | HCons x _ y hl => f x y (HList_fold_right_nodep f a0 hl)
    end.
  Fixpoint list_curried_type (params : list hyper_type_pair) T : Type :=
    match params with
    | nil => T
    | htp :: l => tp_ft htp -> list_curried_type l T
    end.
  Fixpoint apply_curried_func {params T} :
      list_curried_type params T -> HList tp_ft params -> T :=
    match params with
    | nil => fun t _ => t
    | htp :: l => fun f hlist => @apply_curried_func l T
                                   (f (hlist_hd hlist)) (hlist_tl hlist)
    end.
End HLIST.
Section HLIST_PROPS.
  Context`{LayerSpecClass}. 
  Inductive ht_list_rel : forall ls, HList tp_ft ls -> list val -> Prop :=
  | ht_nil_rel : ht_list_rel nil HNil nil
  | ht_cons_rel : forall htp ls f fs v vs,
    ht_rel f v -> ht_list_rel ls fs vs ->
    ht_list_rel (cons htp ls) (HCons htp ls f fs) (cons v vs).
  Inductive ht_list_ft_cond :
      forall {ls}(es1 : HList tp_ft ls), Prop :=
  | ht_nil_ft_cond : ht_list_ft_cond HNil
  | ht_cons_ft_cond : forall htp ls e es,
    ht_ft_cond e -> ht_list_ft_cond es ->
    ht_list_ft_cond (HCons htp ls e es).
  
  Inductive ht_list_valid_ft_cond:
    forall {ls} (es1 : HList tp_ft ls), Prop :=
  | ht_nil_valid_ft_cond: ht_list_valid_ft_cond HNil
  | ht_cons_valid_ft_cond: forall htp ls e es,
      ht_valid_ft_cond e -> ht_list_valid_ft_cond es ->
      ht_list_valid_ft_cond (HCons htp ls e es).
  End HLIST_PROPS.
Section HYPER_CONSTRUCTOR.
  Fixpoint compose_fieldlist fld_ids fld_tps {struct fld_tps} :=
    match fld_tps, fld_ids with
    | nil, nil => Some Fnil
    | htp :: res_tps, fld_id :: res_ids =>
      match compose_fieldlist res_ids res_tps with
      | Some res => Some (Fcons fld_id (tp_ty htp) res)
      | None => None
      end
    | _, _ => None
    end.
  Lemma compose_fieldlist_length_eq_ids_fieldlist :
    forall fld_ids fld_tps fieldlist,
    compose_fieldlist fld_ids fld_tps = Some fieldlist ->
    length fld_ids = fieldlist_length fieldlist.
  Proof. Admitted.
  Definition compose_Tstruct struct_id fld_ids fld_tps :=
    match compose_fieldlist fld_ids fld_tps with
    | Some fieldlist => Some (Tstruct struct_id fieldlist)
    | None => None
    end.
  Fixpoint compose_cstruct_val map fld_ids {fld_tps}
      (fs : HList tp_ft fld_tps){struct fs} :=
    match fs with
    | HNil => map
    | HCons htp _ f res => match fld_ids with
      | nil => map  
      | fld_id :: res_ids =>
        compose_cstruct_val (PTree.set fld_id (ht_cval f) map) res_ids res
      end
    end.
  Definition compose_cval_struct fld_ids {fld_tps}(fs : HList tp_ft fld_tps) :=
    CVstruct (CSmap (compose_cstruct_val (PTree.empty cval) fld_ids fs)).
  Class HyperConstructor tp `{hti : HyperTypeImpl tp} fld_ids fld_tps constr
      : Prop := {
    hc_compose_ty_eq : exists struct_id,
      compose_Tstruct struct_id fld_ids fld_tps = Some (unpair_ty tp);
    hc_fld_ids_norepet : list_norepet fld_ids;
    hc_constr_correct : forall args : HList tp_ft fld_tps,
      compose_cval_struct fld_ids args
        = ht_cval (apply_curried_func constr args);
    hc_constr_returns : forall args : HList tp_ft fld_tps,
      ht_list_ft_cond args ->
      ht_ft_cond (apply_curried_func constr args)
  }.
  Class HyperConstructorPassthrough
      tp `{hti : HyperTypeImpl tp} fld_tps (constr: list_curried_type fld_tps (unpair_ft tp)) : Prop := {
  }.
End HYPER_CONSTRUCTOR.
Section HYPER_LTYPE.
  Context`{HM : HyperMem}.
  Definition GetOpt S V := S -> ContOpt V.
  Definition SetOpt S V := V -> S -> ContOpt S.
  Definition getter_of_data tp := GetOpt GetHighData (unpair_ft tp).
  Definition setter_of_data tp := SetOpt GetHighData (unpair_ft tp).
  Record ltype_pair tp := {
    ltype_tp_marker : type_marker tp;   
    ltype_get : GetHighData -> unpair_ft tp;                   
    ltype_set : unpair_ft tp -> GetHighData -> GetHighData;
    ltype_set_ocond : OProp1 GetHighData;                      
    ltype_get_extra_ocond : OProp1 GetHighData;                
    ltype_ghost : bool;     
    ltype_ident : ident_ext;    
  }.
  Global Arguments ltype_get {_} _ _.
  Global Arguments ltype_set {_} _ _ _.
  Global Arguments ltype_set_ocond {_} _.
  Global Arguments ltype_get_extra_ocond {_} _.
  Global Arguments ltype_ghost {_} _.
  Global Arguments ltype_ident {_} _.
  Class HyperLTypeDir {tp}`{HyperTypeImpl tp} (i : ident_ext) 
      (getter : GetHighData -> unpair_ft tp)
      (setter : unpair_ft tp -> GetHighData -> GetHighData)
      (setter_ocond getter_extra_ocond : OProp1 GetHighData) : Prop := {
    
    
    ltype_get_match : forall j d m, match_AbData d m j ->
      oProp1 (setter_ocond /\ getter_extra_ocond)%oprop1 d ->
      ht_ft_cond (getter d) /\
      cval_match_indirect m (unpair_ty tp) i (ht_cval (getter d));
    
    ltype_set_match : forall f j d m, ht_ft_cond f ->
      mem_match j d m -> oProp1 setter_ocond d ->
      forall m',  
        (forall i',  
          ident_ext_extends i i' \/
          IdentExtMap.get i' (fst m') = IdentExtMap.get i' (fst m)) ->
        (snd m) = (snd m') ->
        cval_match_indirect (fst m') (unpair_ty tp) i (ht_cval f) ->
        mem_match j (setter f d) m'
  }.
  
  Class HyperLTypeStatic {tp}(lt : ltype_pair tp) (new_glbl : list ident) : Prop := {
  }.
  
  Class HyperLType {tp}`{hti : HyperTypeImpl tp}(lt : ltype_pair tp) : Prop := {
    
    
    hyper_ltype_direct :
      lt.(ltype_ghost) = false ->
      HyperLTypeDir lt.(ltype_ident) 
                    lt.(ltype_get) lt.(ltype_set)
                    lt.(ltype_set_ocond) lt.(ltype_get_extra_ocond);
    
    
  }.
  Lemma cval_match_indirect_by_value_load `{hbvt : HyperByValueType} m i cv:
      cval_match_indirect m (unpair_ty tp) i cv ->
      exists v, IdentExtMap.get i m = EEVal v
             /\ cval_match v cv.
  Proof. Admitted.
  Lemma cval_match_indirect_by_value_store `{hbvt : HyperByValueType} m i v cv :
      IdentExtMap.get i m = EEVal v ->
      cval_match v cv ->
      cval_match_indirect m (unpair_ty tp) i cv.
  Proof. Admitted.
  
  
  Class HyperFieldImpl (tp_struct tp_field : type_pair)(id : ident) : Type := {
    Hfield_get : unpair_ft tp_struct -> unpair_ft tp_field;
    Hfield_set : unpair_ft tp_field -> unpair_ft tp_struct -> unpair_ft tp_struct
  }.
  Class HyperField (tp_struct tp_field : type_pair)(id : ident)
    `{htis : HyperTypeImpl tp_struct, htif : HyperTypeImpl tp_field,
     !HyperFieldImpl tp_struct tp_field id} : Prop := {
    Hfield_size_pos : sizeof_words (unpair_ty tp_field) > 0;
    Hfield_get_returns : forall f, ht_ft_cond f -> ht_ft_cond (Hfield_get f);
    Hfield_set_returns : forall f st, ht_ft_cond f -> ht_ft_cond st ->
      ht_ft_cond (Hfield_set f st);
    Hfield_get_correct : forall st, ht_ft_cond st -> ht_valid_ft_cond st ->
      ht_cval_some (Hfield_get st)
        (cval_field_access id (ht_cval st) (unpair_ty tp_struct));
    Hfield_set_correct : forall f st, ht_ft_cond f -> ht_ft_cond st ->
      ht_cval_some (Hfield_set f st)
                   (cval_field_update id (ht_cval st) (unpair_ty tp_struct) (ht_cval f));
    
    Hfield_delta_correct :
     exists x ofs fList,
        (unpair_ty tp_struct = Tstruct x fList  /\
         struct_field fList id = Some (ofs, unpair_ty tp_field))
  }.
  
  Class HyperFieldPassthrough (tp_struct tp_field : type_pair)(id : ident)
    `{htis : HyperTypeImpl tp_struct, htif : HyperTypeImpl tp_field,
     !HyperFieldImpl tp_struct tp_field id} : Prop := {
    
  }.
  Class HyperIndexImpl (tp_array tp_elem : type_pair) : Type := {
    Hindex_size : Z;
    Hindex_get : Z -> unpair_ft tp_array -> unpair_ft tp_elem;
    Hindex_set : Z -> unpair_ft tp_elem -> unpair_ft tp_array -> unpair_ft tp_array
  }.
  Class HyperIndex (tp_array tp_elem : type_pair)
    `{htis : HyperTypeImpl tp_array, htif : HyperTypeImpl tp_elem,
     !HyperIndexImpl tp_array tp_elem} : Prop := {
    Hindex_get_returns : forall a i, (0 <= i < Hindex_size)%Z ->
      ht_ft_cond a -> ht_ft_cond (Hindex_get i a);
    Hindex_set_returns : forall f a i, (0 <= i < Hindex_size)%Z ->
      ht_ft_cond f -> ht_ft_cond a -> ht_ft_cond (Hindex_set i f a);
    Hindex_get_correct : forall a i, (0 <= i < Hindex_size)%Z ->
      ht_cval_some (Hindex_get i a)
        (cval_array_indexing i (ht_cval a) (unpair_ty tp_array));
    Hindex_set_correct : forall f a i,
      (0 <= i < Hindex_size)%Z ->
      ht_cval_some (Hindex_set i f a)
        (cval_array_update i (ht_cval a) (unpair_ty tp_array) (ht_cval f));
    Hindex_array_type :
      unpair_ty tp_array = Tarray (unpair_ty tp_elem) Hindex_size ;
    Hindex_size_bound : 0 <= Hindex_size < Int256.modulus
  }.
  Class HyperIndexPassthrough (tp_array tp_elem : type_pair)
    `{htis : HyperTypeImpl tp_array, htif : HyperTypeImpl tp_elem,
     !HyperIndexImpl tp_array tp_elem} : Prop := {
    
  }.
  Class HyperIntHashImpl (tp_array tp_elem : type_pair) : Type := {
    
    Hhash_get : int256 -> unpair_ft tp_array -> unpair_ft tp_elem;
    Hhash_set : int256 -> unpair_ft tp_elem -> unpair_ft tp_array -> unpair_ft tp_array
  }.
  Class HyperIntHash (tp_array tp_elem : type_pair)
    `{htis : HyperTypeImpl tp_array,  htif : HyperTypeImpl tp_elem,
     !HyperIntHashImpl tp_array tp_elem} : Prop := {
    Hhash_get_returns : forall a i, 
      ht_ft_cond a -> ht_ft_cond (Hhash_get i a);
    Hhash_set_returns : forall f a i,
      ht_ft_cond f -> ht_ft_cond a -> ht_ft_cond (Hhash_set i f a);
    Hhash_get_correct : forall a i, 
      ht_cval_some (Hhash_get i a)
        (cval_hashmap_lookup i (ht_cval a) (unpair_ty tp_array));
    Hhash_set_correct : forall f a i,
      ht_cval_some (Hhash_set i f a)
        (cval_hashmap_update i (ht_cval a) (unpair_ty tp_array) (ht_cval f));
    Hhash_type :
      unpair_ty tp_array = Thashmap tint (unpair_ty tp_elem)
  }.
  
End HYPER_LTYPE.
Section ISO_TYPE.
  Class HyperTypeIsoImpl (tp : type_pair) : Type := {
    ht_iso_ft_cond : unpair_ft tp -> Prop;
    ht_iso_ty_cond : val -> Prop;
    ht_iso_default : unpair_ft tp;
    ht_implement : unpair_ft tp -> val;
    ht_spec : val -> unpair_ft tp
  }.
  Class HyperTypeIso (tp : type_pair)`{hti : HyperTypeIsoImpl tp} : Prop := {
    ht_implement_returns : forall f, ht_iso_ft_cond f -> ht_iso_ty_cond (ht_implement f);
    ht_spec_returns : forall v, ht_iso_ty_cond v -> ht_iso_ft_cond (ht_spec v);
    ht_iso_default_ft_cond : ht_iso_ft_cond ht_iso_default;
    ht_proof_left : forall f, ht_iso_ft_cond f -> ht_spec (ht_implement f) = f;
    ht_proof_right : forall v, ht_iso_ty_cond v -> ht_implement (ht_spec v) = v 
  }.
  Global Arguments ht_iso_ty_cond tp {_} v.
  Context`{HyperTypeIso}.
  Global Instance hyper_type_iso_impl : HyperTypeImpl tp := {
    ht_cval f := CVval (ht_implement f);
    ht_ft_cond := ht_iso_ft_cond;
    ht_default := ht_iso_default;
    ht_valid_ft_cond f := True;
    ht_valid_ft_ocond := otrue1;
    
  }.
  Global Instance hyper_type_iso : HyperType tp.
  Proof. Admitted.
End ISO_TYPE.
