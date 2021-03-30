Require Import BinIntDef.
Require Import BinPosDef.
Require Import Bool.
Require Import backend.AST.
Require Import backend.MemoryModel.
Require Import backend.Values.HighValues.
Require Import cclib.Coqlib.
Require Import cclib.Integers.
Require Import cclib.Maps.  
Require Import backend.Expressions.ExpMiniC.
Require Import backend.Statements.StmtMiniC.
Require Import backend.Cop.  
Require Import backend.Ctypes.
Require Import backend.MachineModel.
Require Import DeepSpec.lib.OProp.
Require Import DeepSpec.core.Cval.
Require Import DeepSpec.core.SEnv.
Require Import DeepSpec.core.HyperType.
Require Import DeepSpec.core.HyperTypeInst.
Require Import DeepSpec.core.MemoryModel.
Require Import DeepSpec.core.Syntax.
Require Import DeepSpec.core.SynthesisExpr.
Require        DeepSpec.lib.LangDef.
Require Import DeepSpec.lib.SimpleMaps.
Require Import DeepSpec.lib.SimpleIndexedMaps.
Require Import backend.phase.MiniC.Semantics.
Require Import backend.phase.MiniC.BigstepSemantics.
Local Open Scope Z.
Opaque Monad.bind Monad.ret  MonadState.get MonadState.put MonadState.gets MonadState.modify StateMonad.runStateT StateMonad.execStateT StateMonad.evalStateT MonadZero.mzero.
Section STMT_FUNC.
  Context`{HM : HyperMem}.
  Context (me : machine_env GetHighData).
  
  
  Record stmt_spec_record returns : Type
      := mk_stmt_spec_record{
    ss_mem : GetHighData;
    
    ss_return : tp_ft returns   
  }.
  
  Global Arguments mk_stmt_spec_record _ _  _.
  Global Arguments ss_mem    {_} _.
  
  Global Arguments ss_return {_} _.
  
  Lemma stmt_spec_record_surjective {returns} r :
      r = mk_stmt_spec_record returns r.(ss_mem)  r.(ss_return).
  Proof. Admitted.
  
  Record stmt_output_equivalent {tmp returns}
      (j : meminj)     
      dest  
      (ml : MemLow)    
      (se : spec_env_t tmp)   
      le                      
      (r : stmt_spec_record returns) : Prop := mk_stmt_output_equivalent{
    so_mem : mem_match j r.(ss_mem) ml;   
    so_se  : lenv_cond se le;             
    so_return : function_return_dec returns = true -> ht_rel_some r.(ss_return) (le ! dest);            
    so_ft_cond : ht_ft_cond r.(ss_return)
  }.
  
  Global Arguments so_mem     {_ _ _ _ _ _ _ _} _.
  Global Arguments so_se      {_ _ _ _ _ _ _ _} _ _ _ _ _.
  Global Arguments so_return  {_ _ _ _ _ _ _ _} _ _.
  Global Arguments so_ft_cond {_ _ _ _ _ _ _ _} _.
  
  
  Definition gss_iso {tmp i}{htp : hyper_type_pair} :
      ((AList.set i htp tmp) ! i ~~~ Some htp)%alist.
  Proof.
    simpl.
    rewrite Pos.eqb_refl.
    exact identity_automorphism.
  Defined.
  Definition gso_iso {tmp i j}{htp htp' : hyper_type_pair} :
      i <> j ->
      (tmp ! i ~~~ Some htp)%alist ->
      ((AList.set j htp' tmp) ! i ~~~ Some htp)%alist.
  Proof.
    intros.
    rewrite AList.gso by assumption.
    exact X.
  Defined.
  
  
  Definition for_start m (start_idx : Z) :=
    (@mk_stmt_spec_record void_unit_pair m  tt, start_idx).
  Definition for_step {tmp id_it}(se : spec_env_t tmp) :
    let tmp' := AList.set id_it int_Z32_pair tmp in
      (SpecTree.t tmp' -> GetHighData -> stmt_spec_record void_unit_pair) ->
      stmt_spec_record void_unit_pair * Z ->
      stmt_spec_record void_unit_pair * Z :=
    fun f p =>
      let '(r, idx) := p in
      let r' := f (SpecTree.set id_it int_Z32_pair idx se) r.(ss_mem) in
      ({|
        ss_mem := r'.(ss_mem);
        
        ss_return := r'.(ss_return)
      |}, idx + 1).
  Definition fold_start htp (start_idx : Z)(start_recur : tp_ft htp) :=
    (start_recur, start_idx).
  Definition fold_step {htp tmp id_it id_recur}(se : spec_env_t tmp) m :
    let tmp' := AList.set id_recur htp (AList.set id_it int_Z32_pair tmp) in
      (SpecTree.t tmp' -> GetHighData -> stmt_spec_record htp) ->
      tp_ft htp * Z -> tp_ft htp * Z :=
    fun f p =>
      let '(r, idx) := p in
      ((f (SpecTree.set id_recur htp r
           (SpecTree.set id_it int_Z32_pair idx se)) m).(ss_return),
       idx + 1).
  Definition dest_not_exist dest (tmp : AList.t hyper_type_pair) :=
    (tmp ! dest ~~~ None)%alist.
  Lemma dest_not_exist_ext {tmp dest} id htp :
      dest_not_exist dest (AList.set id htp tmp) ->
      dest_not_exist dest tmp.
  Proof. Admitted.
  Definition yield_expr dest e :=
    Sset dest e.
  
  Fixpoint synth_stmt_pure {returns}(c : cmd_constr returns) : bool :=
    match c with
    | CCskip => true
    | CClet _ tp _ id c1 c2 => synth_stmt_pure c1 && synth_stmt_pure c2
    | CCload tp _ e => true
    | CCstore _ _ el er => false
    | CCsequence _ c1 c2 => synth_stmt_pure c1 && synth_stmt_pure c2
    | CCifthenelse _ e c_true c_false =>
      synth_stmt_pure c_true && synth_stmt_pure c_false
    | CCfor id_it id_end e1 e2 c3 => false
      
    | CCfirst _ id_it id_end id_dest e1 e2 c3 c4 c5 =>
      synth_stmt_pure c3 && synth_stmt_pure c4 && synth_stmt_pure c5
    | CCfold tp _ id_it id_end id_recur id_dest e1 e2 e3 c4 => true
    | CCcall argt ret prim args => prim.(PRIMpure)
    | CCtransfer _ _ => true                                           
    
    | CCyield tp _ e => true
    | CCconstr _ _ _ _ _ _ _ => false
    | CCassert c => synth_stmt_pure c
    
    | CCdeny c => synth_stmt_pure c
    | CCpanic _ _ => true
    | CCrespec _ _ c _ => true
    | CCrespec_opt _ _ c _ => false
    end.
  
  Definition fold_synth_expr_wellformed tmp {tps}(es : expr_constr_list tps) :=
    HList_fold_right_nodep
      (fun htp (r : expr_constr _) a => prod (synth_expr_wellformed tmp (tp := tp_type_pair htp) r) a)
      True
      es.
  Fixpoint synth_stmt_wellformed {returns}(c : cmd_constr returns) dest tmp : Type :=
    match c with
    | CCskip => dest_not_exist dest tmp
    | CClet _ tp _ id c1 c2 =>
      let tmp' := AList.set id (mk_hyper_type_pair tp) tmp in
      ((tmp ! id ~~~ None) * dest_not_exist dest tmp') *
      (synth_stmt_wellformed c1 id tmp * synth_stmt_wellformed c2 dest tmp')
    | CCload tp _ e => synth_lexpr_wellformed tmp e
    | CCstore _ _ el er =>
      synth_lexpr_wellformed tmp el * synth_expr_wellformed tmp er
    | CCsequence _ c1 c2 =>
      synth_stmt_wellformed c1 dest tmp * synth_stmt_wellformed c2 dest tmp
    | CCifthenelse _ e c_true c_false =>
      synth_expr_wellformed tmp e *
      (synth_stmt_wellformed c_true dest tmp *
       synth_stmt_wellformed c_false dest tmp)
    | CCfor id_it id_end e1 e2 c3 =>
      let tmp' := AList.set id_end int_Z32_pair tmp in
      let tmp'' := AList.set id_it int_Z32_pair tmp' in
      ((tmp ! id_end ~~~ None) * ((tmp' ! id_it ~~~ None) *
       dest_not_exist dest tmp'')) *
      (synth_expr_wellformed tmp e1 * (synth_expr_wellformed tmp e2 *
       synth_stmt_wellformed c3 dest tmp''))
    | CCfirst _ id_it id_end id_dest e1 e2 c3 c4 c5 =>
      let tmp' := AList.set id_end int_Z32_pair tmp in
      let tmp'' := AList.set id_it int_Z32_pair tmp' in
      ((tmp ! id_end ~~~ None) * ((tmp' ! id_it ~~~ None) *
       ((tmp'' ! id_dest ~~~ None) * (dest_not_exist dest tmp'' *
        ((synth_stmt_pure c3 ~~~ true)))))) *
      (synth_expr_wellformed tmp e1 * (synth_expr_wellformed tmp e2 *
       (synth_stmt_wellformed c3 id_dest tmp'' *
       (synth_stmt_wellformed c4 dest tmp'' *
        synth_stmt_wellformed c5 dest tmp''))))
    | CCfold tp _ id_it id_end id_recur id_dest e1 e2 e3 c4 =>
      let tmp' := AList.set id_end int_Z32_pair tmp in
      let tmp'' := AList.set id_it int_Z32_pair tmp' in
      let tmp''' := AList.set id_recur (mk_hyper_type_pair tp) tmp'' in
      ((tmp ! id_end ~~~ None) * ((tmp' ! id_it ~~~ None) *
       ((tmp'' ! id_recur ~~~ None) * ((tmp''' ! id_dest ~~~ None) *                
       (dest_not_exist dest tmp''' * (synth_stmt_pure c4 ~~~ true)))))) *           
      (synth_expr_wellformed tmp e1 * (synth_expr_wellformed tmp e2 *
       (synth_expr_wellformed tmp e3 * synth_stmt_wellformed c4 id_dest tmp''')))
    | CCcall argt ret prim args => fold_synth_expr_wellformed tmp args
    
    | CCconstr _ _ _ _ el flds _ =>
      synth_lexpr_wellformed tmp el * fold_synth_expr_wellformed tmp flds
    | CCtransfer e1 e2 =>
      synth_expr_wellformed tmp e1 * synth_expr_wellformed tmp e2
                                                                 
    | CCyield tp _ e => synth_expr_wellformed tmp e
    | CCassert c => (synth_stmt_pure c ~~~ true) * synth_stmt_wellformed c dest tmp
    | CCdeny c   => (synth_stmt_pure c ~~~ true) * synth_stmt_wellformed c dest tmp
    
    | CCpanic _ _ => True
    | CCrespec _ tmp' c spec =>
      ((tmp ~~~ tmp') * (synth_stmt_pure c ~~~ true)) *
      synth_stmt_wellformed c dest tmp
    | CCrespec_opt _ tmp' c spec => (tmp ~~~ tmp') * synth_stmt_wellformed c dest tmp
    end%type%alist.
  
  Fixpoint synth_stmt_locals {returns}(c : cmd_constr returns) dest tmp :
      list (ident * type) :=
    match c with
    | CCskip => nil
    | CClet _ tp _ id c1 c2 =>
      (id, unpair_ty tp) ::
        synth_stmt_locals c1 id tmp ++
          synth_stmt_locals c2 dest (AList.set id (mk_hyper_type_pair tp) tmp)
    | CCload tp _ e => nil
    | CCstore _ _ el er => nil
    | CCsequence _ c1 c2 =>
      synth_stmt_locals c1 dest tmp ++ synth_stmt_locals c2 dest tmp
    | CCifthenelse _ e c_true c_false =>
      synth_stmt_locals c_true dest tmp ++ synth_stmt_locals c_false dest tmp
    | CCfor id_it id_end e1 e2 c3 =>
      let tmp' := AList.set id_end int_Z32_pair tmp in
      let tmp'' := AList.set id_it int_Z32_pair tmp' in
      (id_it, tint) :: (id_end, tint) :: synth_stmt_locals c3 dest tmp''
    | CCfirst _ id_it id_end id_dest e1 e2 c3 c4 c5 =>
      let tmp' := AList.set id_end int_Z32_pair tmp in
      let tmp'' := AList.set id_it int_Z32_pair tmp' in
      (id_it, tint) :: (id_end, tint) :: (id_dest, tint) ::
        synth_stmt_locals c3 id_dest tmp'' ++ synth_stmt_locals c4 dest tmp'' ++
        synth_stmt_locals c5 dest tmp''
    | CCfold tp _ id_it id_end id_recur id_dest e1 e2 e3 c4 =>
      let tmp' := AList.set id_end int_Z32_pair tmp in
      let tmp'' := AList.set id_it int_Z32_pair tmp' in
      let tmp''' := AList.set id_recur (mk_hyper_type_pair tp) tmp'' in
      (id_it, tint) :: (id_end, tint) :: (id_recur, unpair_ty tp) ::
        (id_dest, unpair_ty tp) :: synth_stmt_locals c4 id_dest tmp'''
    | CCcall argt ret prim args => nil
    
    | CCtransfer _ _ => nil
    | CCconstr _ _ _ _ _ _ _ => nil
    | CCyield tp _ e => nil
    | CCassert _ => nil
    | CCdeny _ => nil
    
    | CCpanic _ _ => nil
    | CCrespec _ tmp' c cpec => synth_stmt_locals c dest tmp
    | CCrespec_opt _ tmp' c spec => synth_stmt_locals c dest tmp
    end.
  Fixpoint synth_CCconstr_stmt tmp target fld_ids {fld_tps}
      (flds : expr_constr_list fld_tps){struct flds} : statement :=
    match flds with
    | HNil => Sskip
    | HCons htp _ fld_e res_es => match fld_ids with
      | nil => Sskip  
      | fld_id :: res_ids =>
        Ssequence
          (Sassign (Efield target fld_id (tp_ty htp))
                   (synth_expr_expr tmp fld_e))
          (synth_CCconstr_stmt tmp target res_ids res_es)
      end
    end.
  
  Fixpoint synth_stmt_stmt {returns}(c : cmd_constr returns) dest tmp : statement :=
    match c with
    | CCskip => Sskip
    | CClet _ tp _ id c1 c2 =>
      let tmp' := AList.set id (mk_hyper_type_pair tp) tmp in
      Ssequence (synth_stmt_stmt c1 id tmp) (synth_stmt_stmt c2 dest tmp')
    | CCload tp _ el => Sset dest (synth_lexpr_expr tmp el)
    | CCstore _ _ el er =>
      if lexpr_is_ghost el then
        Sskip
      else
        Sassign (synth_lexpr_expr tmp el) (synth_expr_expr tmp er)
    | CCsequence _ c1 c2 =>
      Ssequence (synth_stmt_stmt c1 dest tmp) (synth_stmt_stmt c2 dest tmp)
    | CCifthenelse _ e c_true c_false =>
      Sifthenelse (synth_expr_expr tmp e) (synth_stmt_stmt c_true dest tmp)
                                          (synth_stmt_stmt c_false dest tmp)
    | CCfor id_it id_end e1 e2 c3 =>
      let tmp' := AList.set id_end int_Z32_pair tmp in
      let tmp'' := AList.set id_it int_Z32_pair tmp' in
      Ssequence (Sset id_end (synth_expr_expr tmp e2))
        (Sfor (Sset id_it (synth_expr_expr tmp e1))
              (Ebinop Olt (Etempvar id_it tint) (Etempvar id_end tint) tint)
              (synth_stmt_stmt c3 dest tmp'')
              (Sset id_it (Ebinop Oadd (Etempvar id_it tint)
                                       (Econst_int256 Int256.one tint) tint)))
    | CCfirst _ id_it id_end id_dest e1 e2 c3 c4 c5 =>
      let tmp' := AList.set id_end int_Z32_pair tmp in
      let tmp'' := AList.set id_it int_Z32_pair tmp' in
      let s3 := synth_stmt_stmt c3 id_dest tmp'' in
      let s4 := synth_stmt_stmt c4 dest tmp'' in
      let s5 := synth_stmt_stmt c5 dest tmp'' in
      Ssequence
        (Sset id_end (synth_expr_expr tmp e2))
        (Ssequence
          (Sset id_it (synth_expr_expr tmp e1))
          (Sloop
            (Ssequence
              (Sifthenelse
                (Ebinop Olt (Etempvar id_it tint) (Etempvar id_end tint) tint)
                Sskip
                (Ssequence
                  (Sset id_it (Etempvar id_end tint))
                  (Ssequence s5 Sbreak)))
              (Ssequence
                s3
                (Sifthenelse
                  (Etempvar id_dest tint)
                  (Ssequence s4 Sbreak)
                  (Sset id_it (Ebinop Oadd (Etempvar id_it tint)
                                           (Econst_int256 Int256.one tint) tint)))))
            ))
    | CCfold tp _ id_it id_end id_recur id_dest e1 e2 e3 c4 =>
      let tmp' := AList.set id_end int_Z32_pair tmp in
      let tmp'' := AList.set id_it int_Z32_pair tmp' in
      let tmp''' := AList.set id_recur (mk_hyper_type_pair tp) tmp'' in
      Ssequence (Ssequence (Sset id_end (synth_expr_expr tmp e2))
        (Sfor (Ssequence (Sset id_it (synth_expr_expr tmp e1))
                         (Sset id_recur (synth_expr_expr tmp e3)))
              (Ebinop Olt (Etempvar id_it tint) (Etempvar id_end tint) tint)
              (synth_stmt_stmt c4 id_dest tmp''')
              (Ssequence (Sset id_it (Ebinop Oadd (Etempvar id_it tint)
                                             (Econst_int256 Int256.one tint) tint))
                         (Sset id_recur (Etempvar id_dest (unpair_ty tp))))))
        (Sset dest (Etempvar id_recur (unpair_ty tp)))
    | CCcall argt ret prim args =>
      
      
      let arg_ty := to_typelist argt in
      let prim_type := Tfunction arg_ty (tp_ty ret)  in
      let synth_expr_htp htp := synth_expr_expr tmp (tp := tp_type_pair htp) in
      let return_tmp := if function_return_dec ret then
                          Some dest
                        else
                          None in
      Scall return_tmp prim.(PRIMident) 
                       (HList_map_nodep synth_expr_htp args)
    
    | CCtransfer e1 e2 => 
      Stransfer (synth_expr_expr tmp e1) (synth_expr_expr tmp e2)
                                       
    | CCyield tp _ e => Sset dest (synth_expr_expr tmp e)
    | CCconstr _ _ fld_ids _ el args _ =>
      if lexpr_is_ghost el then
        Sskip
      else
        let target := synth_lexpr_expr tmp el in
        synth_CCconstr_stmt tmp target fld_ids args
    | CCassert c1 =>      
      Ssequence (synth_stmt_stmt c1 dest tmp)
                (Sifthenelse (Etempvar dest tint)
                             Sskip
                             Srevert)
    | CCdeny c1 =>
      Ssequence (synth_stmt_stmt c1 dest tmp)
                (Sifthenelse (Etempvar dest tint)
                             Srevert
                             Sskip)
    
    | CCpanic _ _ => Srevert
    | CCrespec _ tmp' c spec => synth_stmt_stmt c dest tmp
    | CCrespec_opt _ tmp' c spec => synth_stmt_stmt c dest tmp
    end.
  Fixpoint map2_synth_expr_spec {tmp tps} es (se : spec_env_t tmp)
      : @fold_synth_expr_wellformed tmp tps es -> HList tp_ft tps :=
    match es with
    | HNil => fun _ => HNil
    | HCons x ls e es => fun wf =>
      let (wf, IHwf) := wf
      in HCons x ls (synth_expr_spec me tmp e wf se) (map2_synth_expr_spec es se IHwf)
    end.
Require Import DeepSpec.lib.Monad.Monad.
Require Import DeepSpec.lib.Monad.MonadState.
Require Import DeepSpec.lib.Monad.MonadZero.
Require Import DeepSpec.lib.Monad.StateMonad.
Require Import DeepSpec.lib.Monad.StateMonadOption.
Import MonadNotation.
Open Scope monad_scope.
Existing Instances Monad_DS MonadLaws_DS MonadState_DS MonadZero_DS.
Lemma Z_nonneg_peano_ind (P : Z -> Prop) :
  P 0%Z ->
  (forall z, 0 <= z -> P z -> P (Z.succ z)) ->
  (forall p, P (Z.neg p)) ->
  forall z, P z.
Proof. Admitted.
Lemma Z_pos_peano_ind (P : Z -> Prop) :
  (forall z, z <= 0 -> P z) ->
  (forall z, 0 <= z -> P z -> P (Z.succ z)) ->
  forall z, P z.
Proof. Admitted.
Fixpoint Niteri {A:Type} (f:Z->A->A) (n:nat) (x:A) {struct n} : A :=
  match n with
  | O => x
  | S n' => f (Z.of_nat n') (Niteri f n' x)
  end.
Lemma Niteri_swap:
  forall (n:nat) (A:Type) (f:Z->A->A) (x:A),
    Niteri (fun z => f (Z.succ z)) n (f 0 x) = f (Z.of_nat n) (Niteri f n x).
Proof. Admitted.
Lemma Niteri_succ:
  forall n (A:Type) (f:Z->A->A) (x:A),
    Niteri f (S n) x = f (Z.of_nat n) (Niteri f n x).
Proof. Admitted.
  
Lemma Niteri_add:
  forall p q (A:Type) (f:Z->A->A) (x:A),
    Niteri f (p+q) x = Niteri (fun z => f (z+(Z.of_nat q))) p (Niteri f q x).
Proof. Admitted.
Lemma Niteri_add1:
  forall p (A:Type) (f:Z->A->A) (x:A),
    Niteri f (S p) x = Niteri (fun z => f (Z.succ z)) p (Niteri f 1 x).
Proof. Admitted.
Definition Ziteri {A} (f:Z->A->A) (steps:Z) (x:A) :=
    Niteri f (Z.to_nat steps) x.
Lemma Ziteri_base':
  forall z (A:Type) (f:Z->A->A) (x:A),
    z < 0 ->
    Ziteri f z x = x.
Proof. Admitted.
Lemma Ziteri_base:
  forall z (A:Type) (f:Z->A->A) (x:A),
    z <= 0 ->
    Ziteri f z x = x.
Proof. Admitted.
Lemma Ziteri_neg:
  forall p (A:Type) (f:Z->A->A) (x:A),
    Ziteri f (Zneg p) x = x.
Proof. Admitted.
Lemma Ziteri_0:
  forall (A:Type) (f:Z->A->A) (x:A),
    Ziteri f 0 x = x.
Proof. Admitted.
Lemma Ziteri_swap:
  forall z (A:Type) (f:Z->A->A) (x:A),
    0 <= z ->
    Ziteri (fun z => f (Z.succ z)) z (f 0 x) = f z (Ziteri f z x).
Proof. Admitted.
Lemma Ziteri_succ:
  forall z (A:Type) (f:Z->A->A) (x:A),
    0 <= z ->
    Ziteri f (Z.succ z) x = f z (Ziteri f z x).
Proof. Admitted.
Lemma Ziteri_add1:
  forall p (A:Type) (f:Z->A->A) (x:A),
    0 <= p ->
    Ziteri f (Z.succ p) x = Ziteri (fun z => f (Z.succ z)) p (Ziteri f 1 x).
Proof. Admitted.
Definition failure_passing {A} (h: DS A -> DS A) : Prop :=
  forall m x,
  runStateT x m = mzero ->
  runStateT (h x) m = mzero.
Lemma Niteri_from_none {A} (f: Z -> DS A -> DS A) (fp: forall n, failure_passing (f n)) n:
  forall x m,
  runStateT x m = mzero ->
  runStateT (Niteri f n x) m = mzero.
Proof. Admitted.
Lemma Niteri_cut_0 {A} {f: Z -> DS A -> DS A} (fp: forall n, failure_passing (f n)) {n x m v s}:
  runStateT (Niteri f n x) m = ret (v, s) ->
  exists v' s', runStateT x m = ret (v', s').
Proof. Admitted.
Lemma Niteri_Ziteri {A} (f: Z -> DS A -> DS A) {P}:
  forall x n,
  P (Niteri f (Z.to_nat n) x) ->
  P (Ziteri f n x).
Proof. Admitted.
Lemma Ziteri_from_none {A} (f: Z -> DS A -> DS A) (fp: forall n, failure_passing (f n)) n:
  forall x m,
    runStateT x m = mzero ->
    runStateT (Ziteri f n x) m = mzero.
Proof. Admitted.
Lemma Ziteri_cut_0 {A} {f: Z -> DS A -> DS A} (fp: forall n, failure_passing (f n)) {n x m v s}:
  runStateT (Ziteri f n x) m = ret (v, s) ->
  exists v' s', runStateT x m = ret (v', s').
Proof. Admitted.
Definition oZiteri_wrapper {A} (f: Z -> A -> DS A) n x : DS A :=
  r <- x;; f n r.
Lemma oZiteri_wrapper_unfolder {A} (f: Z -> A -> DS A) n x:
  oZiteri_wrapper f n x = (r <- x;; f n r).
  reflexivity.
Qed.
Definition oZiteri {A} (f: Z -> A -> DS A) n x : DS A :=
  Ziteri (oZiteri_wrapper f) n (ret x).
Definition oNiteri {A} (f: Z -> A -> DS A) n x : DS A :=
  Niteri (oZiteri_wrapper f) n (ret x).
Lemma oNiteri_unfolder {A} (f: Z -> A -> DS A) n x:
  oNiteri f n x = Niteri (oZiteri_wrapper f) n (ret x).
  reflexivity.
Qed.
Lemma oZiteri_unfolder {A} (f: Z -> A -> DS A) n x:
  oZiteri f n x = Ziteri (oZiteri_wrapper f) n (ret x).
  reflexivity.
Qed.
Lemma oZiteri_oNiteri {A} (f: Z -> A -> DS A) n x:
  oZiteri f n x = oNiteri f (Z.to_nat n) x.
Proof. Admitted.
Definition first_map (check: Z -> DS (tp_ft int_bool_pair))
           (start bound : Z) :=
  oZiteri
    ( fun i l => m <- get;; h <- check (start + i);; put m)
    (bound - start)
    tt.
Definition fold_step_opt {htp tmp id_it id_recur}(se : spec_env_t tmp) (start:Z):
  let tmp' := AList.set id_recur htp (AList.set id_it int_Z32_pair tmp) in
  (SpecTree.t tmp' -> DS (tp_ft htp)) ->
  Z ->
  tp_ft htp ->
  DS (tp_ft htp) :=
  fun c i r =>
    c (SpecTree.set id_recur htp r
                    (SpecTree.set id_it int_Z32_pair (start+i) se)).
Lemma oNiteri_link_head {A} {f: Z -> A -> DS A} {n x m v s}:
  runStateT (oNiteri f 1 x) m = ret (v, s) ->
  runStateT (oNiteri (fun z => f (Z.succ z)) n v) s =
  runStateT (oNiteri f (S n) x) m.
Proof. Admitted.
Lemma oZiteri_link_head {A} {f: Z -> A -> DS A} {n x m v s}:
  0 <= n ->
  runStateT (oZiteri f 1 x) m = ret (v, s) ->
  runStateT (oZiteri (fun z => f (Z.succ z)) n v) s =
  runStateT (oZiteri f (Z.succ n) x) m.
Proof. Admitted.
Lemma failure_passing_oZiteri_wrapper {A} (f: Z -> A -> DS A):
  forall n, failure_passing ((oZiteri_wrapper f) n).
Proof. Admitted.
Lemma oZiteri_0 {A} {f: Z -> A -> DS A} {x:A}:
  oZiteri f 0 x = ret x.
Proof. Admitted.
Lemma oZiteri_base {A} {f: Z -> A -> DS A} {x:A} n:
  n <= 0 ->
  oZiteri f n x = ret x.
Proof. Admitted.
Lemma oZiteri_cut_head {A} {f: Z -> A -> DS A} {n x m v s}:
  0 <= n ->
  runStateT (oZiteri f (Z.succ n) x) m = ret (v, s) ->
  exists v' s', runStateT (oZiteri f 1 x) m = ret (v', s') /\
                runStateT (oZiteri (fun z => f (Z.succ z)) n v') s' = ret (v, s).
Proof. Admitted.
Lemma Ziteri_1 {A} {f: Z -> A -> A} {x:A}:
  Ziteri f 1 x = f 0 x.
Proof. Admitted.
Lemma oZiteri_1 {A} {f: Z -> A -> DS A} {x m}:
  runStateT (oZiteri f 1 x) m = runStateT (f 0 x) m.
Proof. Admitted.
Lemma oZiteri_neg {A} {f: Z -> A -> DS A} {x:A} {p}:
  oZiteri f (Z.neg p) x = ret x.
Proof. Admitted.
Lemma oZiteri_cut_tail {A} {f: Z -> A -> DS A} {n x m v s}:
  0 <= n ->
  runStateT (oZiteri f (Z.succ n) x) m = ret (v, s) ->
  exists v' s', runStateT (oZiteri f n x) m = ret (v', s') /\
                runStateT (oZiteri (fun z => f (n + z)) 1 v') s' = ret (v, s).
Proof. Admitted.
Lemma oZiteri_mid {A} {f: Z -> A -> DS A} n0 n1 {x m v s}:
  0 <= n0 ->
  0 <= n1 ->
  runStateT (oZiteri f (n0+n1) x) m = ret (v, s) ->
  exists v' s', runStateT (oZiteri f n0 x) m = ret (v', s') /\
                runStateT (oZiteri (fun z => f (n0 + z)) n1 v') s' = ret (v, s).
Proof. Admitted.
Lemma oZiteri_pure {A} {f: Z -> A -> DS A}:
  forall n m,
  (forall z, 0 <= z < n -> forall r v s, runStateT (f z r) m = ret (v, s) -> m = s) ->
  forall x v s, runStateT (oZiteri f n x) m = ret (v, s) ->
                m = s.
Proof. Admitted.
Lemma oZiteri_connect {A} {f: Z -> A -> DS A}:
  forall n x m,
    (forall z, 0 <= z < n ->
     forall v' s', runStateT (oZiteri f z x) m = ret (v', s') ->
                   exists v s, runStateT (f z v') s' = ret (v, s)) ->
  exists v s, runStateT (oZiteri f n x) m = ret (v, s).
Proof. Admitted.
Lemma oZiteri_pure_connect {A} {f: Z -> A -> DS A}:
  forall n x m,
    (forall z, 0 <= z < n ->
     forall v', runStateT (oZiteri f z x) m = ret (v', m) ->
                   exists v, runStateT (f z v') m = ret (v, m)) ->
    exists v, runStateT (oZiteri f n x) m = ret (v, m).
Proof. Admitted.
Definition for_step_opt {tmp} {id_it : positive} (se : spec_env_t tmp) (start:Z) :
  let tmp' := AList.set id_it int_Z32_pair tmp in
  (spec_env_t tmp' -> DS unit) ->
  Z ->
  DS (tp_ft void_unit_pair) ->
  DS (tp_ft void_unit_pair) :=
  fun c i ma =>
    ma;; c (SpecTree.set id_it int_Z32_pair (start+i) se).
Lemma for_from_none {tmp} id_it (se: spec_env_t tmp):
  forall p f s0 start c,
    runStateT c s0 = mzero ->
    runStateT (Niteri (@for_step_opt _ id_it se start f)
                      p
                      c) s0 = mzero.
Proof. Admitted.
Lemma for_from_none' {tmp} id_it (se: spec_env_t tmp):
  forall p f s0 start init,
    runStateT init s0 = mzero ->
    runStateT (Niteri (fun z => @for_step_opt _ id_it se start f (Z.succ z))
                      p
                      init) s0 = mzero.
Proof. Admitted.
Lemma for_n_plus_1__runStateT:
  forall tmp id_it (se:spec_env_t tmp)
    (f: SpecTree.ilist (AList.set id_it int_Z32_pair tmp) -> stateT GetHighData option (tp_ft void_unit_pair)) d start mh m',
    runStateT (f (SpecTree.set id_it int_Z32_pair start se)) mh = ret (tt, m') ->
    runStateT
      (Niteri
         (fun z : Z =>
            @for_step_opt _ id_it se start f (Z.succ z)) d
         (Niteri (for_step_opt se start f) 1 (ret tt)))
      mh =
    runStateT
      (Niteri (for_step_opt se (start+1) f) d (ret tt))
      m'.
Proof. Admitted.
Lemma for_n_plus_1__execStateT:
  forall tmp id_it (se:spec_env_t tmp) (f: SpecTree.ilist (AList.set id_it int_Z32_pair tmp) -> stateT GetHighData option (tp_ft void_unit_pair)) d start mh m',
    execStateT (f (SpecTree.set id_it int_Z32_pair start se)) mh = ret m' ->
    execStateT
      (Ziteri
         (fun z : Z =>
            @for_step_opt _ id_it se start f (Z.succ z)) d
         (Ziteri (for_step_opt se start f) 1 (ret tt)))
      mh =
    execStateT
      (Ziteri (for_step_opt se (start+1) f) d (ret tt))
      m'.
Proof. Admitted.
Lemma for_cut_opt' {tmp id_it} (se : spec_env_t tmp):
  let tmp' := AList.set id_it int_Z32_pair tmp in
  forall f s0 start steps s,
    0 <= steps ->
    runStateT (Ziteri (@for_step_opt _ id_it se start f)
                       (Z.succ steps)
                       (ret tt)) s0 = ret (tt, s) ->
    exists s',
      runStateT (f (SpecTree.set id_it int_Z32_pair start se)) s0 = ret (tt, s')
      /\ runStateT (Ziteri (@for_step_opt _ id_it se (start+1) f)
                          steps
                          (ret tt)) s' = ret (tt, s).
Proof. Admitted.
Lemma oZiteri_fail_at_1 {A} {f: Z -> A -> DS A} {n x m}:
  0 <= n ->
  runStateT (oZiteri f 1 x) m = mzero ->
  runStateT (oZiteri f (Z.succ n) x) m = mzero.
Proof. Admitted.
Lemma oZiteri_replace {A} {f: Z -> A -> DS A} {g: Z -> A -> DS A}:
  (forall z r s, runStateT (f z r) s = runStateT (g z r) s) ->
  forall n x m, runStateT (oZiteri f n x) m = runStateT (oZiteri g n x) m.
Proof. Admitted.
Lemma fold_n_plus_1__runStateT {htp tmp id_it id_recur} (se:spec_env_t tmp) f mh start init:
  let tmp''' := AList.set id_recur htp (AList.set id_it int_Z32_pair tmp) in
  let se_recur := SpecTree.set id_recur htp init (SpecTree.set id_it int_Z32_pair start se) in
  forall v' s',
  runStateT (f se_recur) mh = ret (v', s') ->
  forall n,
    0 <= n ->
  runStateT
    (oZiteri
       (fold_step_opt se start f) (Z.succ n) init) mh =
  runStateT
    (oZiteri (fold_step_opt se (Z.succ start) f) n v') s'.
Proof. Admitted.
Lemma fold_cut_opt' {htp tmp id_it id_recur} (se : spec_env_t tmp) f mh start init:
  let tmp''' := AList.set id_recur htp (AList.set id_it int_Z32_pair tmp) in
  let se_recur := SpecTree.set id_recur htp init (SpecTree.set id_it int_Z32_pair start se) in
  forall steps v s,
    0 <= steps ->
    runStateT (oZiteri (@fold_step_opt _ _ id_it id_recur se start f)
                      (Z.succ steps)
                      init) mh = ret (v, s) ->
    exists v' s',
      runStateT (f se_recur) mh = ret (v', s') /\
      runStateT (oZiteri (@fold_step_opt _ _ id_it id_recur se (Z.succ start) f)
                        steps
                        v') s' = ret (v, s).
Proof. Admitted.
Lemma for_cut_opt {tmp id_it} (se : spec_env_t tmp):
  let tmp' := AList.set id_it int_Z32_pair tmp in
  forall f s0 start steps s,
    0 <= steps ->
    execStateT (Ziteri (@for_step_opt _ id_it se start f)
                       (Z.succ steps)
                       (ret tt)) s0 = ret s ->
    exists s',
      execStateT (f (SpecTree.set id_it int_Z32_pair start se)) s0 = ret s'
      /\ execStateT (Ziteri (@for_step_opt _ id_it se (start+1) f)
                           steps
                           (ret tt)) s' = ret s.
Proof. Admitted.
Fixpoint Nfind (check:Z -> DS bool) (n:nat) {struct n} : DS (option Z) :=
  match n with
  | O => ret None
  | S n' => res <- check (Z.of_nat n);;
           if res
           then ret (Some (Z.of_nat n))
           else Nfind check n'
  end.
Definition Zfind (check:Z -> DS bool) (z:Z) :=
  Nfind check (Z.to_nat z).
Lemma Zfind_succ (check:Z -> DS bool) (z:Z):
  0 <= z ->
  Zfind check (Z.succ z) =
  (res <- check (Z.succ z);;
      if res
      then ret (Some (Z.succ z))
      else Zfind check z).
Proof. Admitted.
Lemma Zfind_pure (check:Z -> DS bool):
  (forall z m v s, runStateT (check z) m = ret (v, s) -> m = s) ->
  forall z m v s, runStateT (Zfind check z) m = ret (v, s) ->
             m = s.
Proof. Admitted.
Definition first_spec {A}
           (check: Z -> DS (tp_ft int_bool_pair)) (f: Z -> A)
           (g: Z -> A) start bound (m:GetHighData) :=
  let P n := is_true
           (match evalStateT (check n) m with
            | Some b => b
            | None => false
            end)
  in
  let Pdec n : {P n} + {~ P n} := bool_dec _ true in
  match LangDef.min_ex P start bound Pdec with
  | inleft (exist n _) => f n
  | inright _ => g bound
  end.
Ltac dest_first H :=
  try unfold first_spec in H;
  match type of H with
  | runStateT match ?X with _ => _ end _ = _ =>
    let n := fresh "n" in
    let n_range := fresh "n_range" in
    let Pn := fresh "Pn" in
    let n_least := fresh "n_least" in
    let no_n := fresh "no_n" in
    destruct X as [(n & n_range & Pn & n_least) | no_n]
  end.
Ltac dest_first_eqn H :=
  try unfold first_spec in H;
  match type of H with
  | runStateT match ?X with _ => _ end _ = _ =>
    let n := fresh "n" in
    let n_range := fresh "n_range" in
    let Pn := fresh "Pn" in
    let n_least := fresh "n_least" in
    let no_n := fresh "no_n" in
    let first_eq := fresh "first_eq" in
    destruct X as [(n & n_range & Pn & n_least) | no_n] eqn:first_eq
  end.
Definition ext_call_me (ext_contract : int256) := {|
  me_address := ext_contract;
  me_origin := me_origin me;
  me_caller := me_address me;
  me_callvalue := me_callvalue me; 
  me_coinbase := me_coinbase me;
  me_timestamp := me_timestamp me;
  me_number := me_number me;
  me_chainid := me_chainid me;
  me_selfbalance := me_selfbalance me;
  me_balance := me_balance me;
  me_blockhash := me_blockhash me;
  me_transfer := me_transfer me;
  me_callmethod := me_callmethod me;
  me_log := me_log me;
|}.
Fixpoint synth_stmt_spec_opt {returns}(c : cmd_constr returns) dest tmp :
  synth_stmt_wellformed c dest tmp -> spec_env_t tmp -> DS (tp_ft returns) :=
  match c with
  | CCskip => fun wf se => ret tt
  | CClet _r tp _ id c1 c2 =>
    let htp := mk_hyper_type_pair tp in
    (fun (wf : let tmp' := AList.set id (mk_hyper_type_pair tp) tmp in
            ((tmp ! id ~~~ None) * dest_not_exist dest tmp') *
            (synth_stmt_wellformed c1 id tmp * synth_stmt_wellformed c2 dest tmp'))
       se =>
       spec1 <- (synth_stmt_spec_opt c1 id tmp (cadr wf) se);;
       let se' := SpecTree.set id htp spec1 se in
       (synth_stmt_spec_opt c2 dest _ (cddr wf) se'))
  | CCload tp _ e => fun wf se =>
    gets (synth_lexpr_spec me tmp e wf se).(ltype_get)
  | CCstore _ _ el er => fun wf se =>
    let f := synth_expr_spec me tmp er (cdr wf) se in
    modify ((synth_lexpr_spec me tmp el (car wf) se).(ltype_set) f)
  | CCsequence _ c1 c2 =>
    fun (wf : synth_stmt_wellformed c1 dest tmp * synth_stmt_wellformed c2 dest tmp)
      se =>
      (synth_stmt_spec_opt c1 dest tmp (car wf) se) ;;
      (synth_stmt_spec_opt c2 dest tmp (cdr wf) se)
  | CCifthenelse _ e c_true c_false =>
    fun (wf : synth_expr_wellformed tmp e *
             (synth_stmt_wellformed c_true dest tmp * synth_stmt_wellformed c_false dest tmp))
        se =>
      if synth_expr_spec me tmp e (car wf) se
      then synth_stmt_spec_opt c_true  dest tmp (cadr wf) se
      else synth_stmt_spec_opt c_false dest tmp (cddr wf) se
  | CCfor id_it id_end e1 e2 c3 =>
    fun (wf : let tmp' := AList.set id_end int_Z32_pair tmp in
            let tmp'' := AList.set id_it int_Z32_pair tmp' in
            ((tmp ! id_end ~~~ None) * ((tmp' ! id_it ~~~ None) *
             dest_not_exist dest tmp'')) *
            (synth_expr_wellformed tmp e1 * (synth_expr_wellformed tmp e2 *
             synth_stmt_wellformed c3 dest tmp'')))
      se =>
      let start := synth_expr_spec me tmp e1 (cadr wf) se in
      let bound := synth_expr_spec me tmp e2 (caddr wf) se in
      let initial_se := SpecTree.set id_end int_Z32_pair bound se in
      Ziteri (for_step_opt initial_se start
                         (synth_stmt_spec_opt c3 dest _ (cdddr wf)))
           (bound - start)
           (ret tt)
  | CCfirst _ id_it id_end id_dest e1 e2 c3 c4 c5 =>
    fun (wf : let tmp' := AList.set id_end int_Z32_pair tmp in
              let tmp'' := AList.set id_it int_Z32_pair tmp' in
              ((tmp ! id_end ~~~ None) * ((tmp' ! id_it ~~~ None) *
               ((tmp'' ! id_dest ~~~ None) * (dest_not_exist dest tmp'' *
                ((synth_stmt_pure c3 ~~~ true)))))) *
              (synth_expr_wellformed tmp e1 * (synth_expr_wellformed tmp e2 *
               (synth_stmt_wellformed c3 id_dest tmp'' *
               (synth_stmt_wellformed c4 dest tmp'' *
                synth_stmt_wellformed c5 dest tmp'')))))
      se =>
      let start := (synth_expr_spec me tmp e1 (cadr wf) se) in
      let bound := (synth_expr_spec me tmp e2 (caddr wf) se) in
      m <- get;;
      first_map
      (fun n =>
         synth_stmt_spec_opt c3 id_dest _ (cadddr wf)
           (SpecTree.set id_it int_Z32_pair n
           (SpecTree.set id_end int_Z32_pair bound se)))
      start bound;;
      first_spec
      (fun n =>
         synth_stmt_spec_opt c3 id_dest _ (cadddr wf)
           (SpecTree.set id_it int_Z32_pair n
           (SpecTree.set id_end int_Z32_pair bound se)))
      (fun n =>
         synth_stmt_spec_opt c4 dest _ (caddddr wf)
           (SpecTree.set id_it int_Z32_pair n
           (SpecTree.set id_end int_Z32_pair bound se)))
      (fun n =>
         synth_stmt_spec_opt c5 dest _ (cdddddr wf)
           (SpecTree.set id_it int_Z32_pair bound 
           (SpecTree.set id_end int_Z32_pair n se)))
      start
      bound
      m
  | CCfold tp _ id_it id_end id_recur id_dest e1 e2 e3 c4 =>
    let htp := mk_hyper_type_pair tp in
    fun (wf : let tmp' := AList.set id_end int_Z32_pair tmp in
              let tmp'' := AList.set id_it int_Z32_pair tmp' in
              let tmp''' := AList.set id_recur (mk_hyper_type_pair tp) tmp'' in
              ((tmp ! id_end ~~~ None) * ((tmp' ! id_it ~~~ None) *
               ((tmp'' ! id_recur ~~~ None) * ((tmp''' ! id_dest ~~~ None) *
               (dest_not_exist dest tmp''' * (synth_stmt_pure c4 ~~~ true)))))) *
              (synth_expr_wellformed tmp e1 * (synth_expr_wellformed tmp e2 *
               (synth_expr_wellformed tmp e3 * synth_stmt_wellformed c4 id_dest tmp'''))))
        se =>
    let start := synth_expr_spec me tmp e1 (cadr wf) se in
    let bound := synth_expr_spec me tmp e2 (caddr wf) se in
    let init := synth_expr_spec me tmp e3 (cadddr wf) se in
    let initial_se := SpecTree.set id_end int_Z32_pair bound se in
    oZiteri (fold_step_opt initial_se start
                          (synth_stmt_spec_opt c4 id_dest _ (cddddr wf)))
           (bound - start)
           init
  | CCcall argt ret prim args =>
    fun wf se =>
      let argv := map2_synth_expr_spec args se wf in
      prim.(PRIMsem_opt) argv me
  
  | CCtransfer e1 e2 =>
    fun (wf : synth_expr_wellformed tmp e1 *
              synth_expr_wellformed tmp e2)
        se =>
      d <- get ;;
      let (success , d') :=
          me_transfer me
                      (synth_expr_spec me tmp e1 (car wf) se)
                      (synth_expr_spec me tmp e2 (cdr wf) se)
                      d in
      if (Int256.eq success Int256.one)
      then put d'
      else mzero
  | CCyield tp _ e =>
    fun wf se =>
      ret (synth_expr_spec me tmp e wf se)
  | CCconstr _ _ _ _ el flds constr =>
    fun wf se =>
      let l := synth_lexpr_spec me tmp el (car wf) se in
      modify (l.(ltype_set)
               (apply_curried_func
                  constr
                  (map2_synth_expr_spec flds se (cdr wf))))
  | CCassert c =>
    fun (wf: (synth_stmt_pure c ~~~ true) * synth_stmt_wellformed c dest tmp) se =>
      v <- synth_stmt_spec_opt c dest tmp (cdr wf) se;;
      guard v
  | CCdeny c =>
    fun (wf: (synth_stmt_pure c ~~~ true) * synth_stmt_wellformed c dest tmp) se =>
      v <- synth_stmt_spec_opt c dest tmp (cdr wf) se;;
      guard (negb v)
  
  | CCpanic _ _ => fun _ _ => mzero
  | CCrespec _ tmp' c spec =>
      fun (wf : ((tmp ~~~ tmp') * (synth_stmt_pure c ~~~ true)) *
              synth_stmt_wellformed c dest tmp) se =>
        m <- get;;
        v <- spec (iso_f (caar wf) se);;
        put m;;
        ret v
  | CCrespec_opt _ tmp' c spec =>
    fun (wf : (tmp ~~~ tmp') * synth_stmt_wellformed c dest tmp) se =>
      spec me (iso_f (car wf) se)
  
end %alist.
Definition synth_stmt_spec_ret {returns} (c : cmd_constr returns) dest tmp:
  synth_stmt_wellformed c dest tmp -> spec_env_t tmp -> GetHighData -> tp_ft returns :=
  fun wf se m =>
    match evalStateT (synth_stmt_spec_opt c dest tmp wf se) m with
    | Some v => v
    | None => ht_default
    end.
  
Lemma fold_pure htp id_it id_recur tmp
      (c:SpecTree.t (AList.set id_recur htp
                    (AList.set id_it int_Z32_pair tmp)) -> DS (tp_ft htp))
      (init:tp_ft htp) steps start:
  (forall m v s se, runStateT (c se) m = ret (v, s) -> m = s) ->
  forall m v s se, runStateT (oZiteri (@fold_step_opt htp tmp id_it id_recur se start c) steps init) m = ret (v, s) ->
              m = s.
Proof. Admitted.
Lemma get_put_pure {A} {s v d} {f: DS A}:
  runStateT (m <- get;; f ;; put m) d = ret (v, s) ->
  d = s.
Proof. Admitted.
Lemma first_map_pure' (check: Z -> DS bool) start steps:
  
  forall m l s, runStateT (first_map check start (start + steps)) m = ret (l, s) ->
           m = s.
Proof. Admitted.
Lemma first_map_pure (check: Z -> DS bool) start bound:
  
  forall m l s, runStateT (first_map check start bound) m = ret (l, s) ->
           m = s.
Proof. Admitted.
  
Fixpoint fold_CCcall_spec_ocond {tmp tps} es
  : @fold_synth_expr_wellformed tmp tps es ->
    OProp3 (HList tp_ft tps) (spec_env_t tmp) GetHighData ->
    OProp2 (spec_env_t tmp) GetHighData :=
  match es with
  | HNil => fun _ acc => ounlift32a HNil acc
  | HCons x ls e es =>
    fun wf acc =>
    let (wf, IHwf) := wf in
    fold_CCcall_spec_ocond es IHwf
      (omap3 (fun p args se m =>
          p (HCons x _ (synth_expr_spec me _ e wf se) args) se m)
        acc)
  end.
  
Lemma exists_impl {A} (P:A->Prop) (Q:A->Prop):
  (forall x, Q x -> P x) ->
  (exists x, Q x) ->
  exists x, P x.
Proof. Admitted.
Lemma exists_impl2 {A B} (P:A->B->Prop) (Q:A->B->Prop):
  (forall x y, Q x y -> P x y) ->
  (exists x y, Q x y) ->
  exists x y, P x y.
Proof. Admitted.
Lemma first_map_osT' (check: Z -> DS bool) start steps m:
  (forall i, 0 <= i < steps -> exists v, runStateT (check (start+i)) m = ret (v, m)) ->
  exists l, runStateT (first_map check start (start+steps)) m = ret (l, m).
Proof. Admitted.
Lemma first_map_osT (check: Z -> DS bool) start bound m:
  (forall i, start <= i < bound -> exists v, runStateT (check i) m = ret (v, m)) ->
  exists l, runStateT (first_map check start bound) m = ret (l, m).
Proof. Admitted.
Lemma first_map_osT_weak' (check: Z -> DS bool) start steps m:
  (forall i, 0 <= i < steps -> exists v m', runStateT (check (start+i)) m = ret (v, m')) ->
  exists l, runStateT (first_map check start (start+steps)) m = ret (l, m).
Proof. Admitted.
Lemma first_map_osT_weak (check: Z -> DS bool) start bound m:
  (forall i, start <= i < bound -> exists v m', runStateT (check i) m = ret (v, m')) ->
  exists l, runStateT (first_map check start bound) m = ret (l, m).
Proof. Admitted.
  Lemma first_map_mid n (check: Z -> DS bool) start bound:
    start <= n < bound ->
    forall m v s, runStateT (first_map check start bound) m = ret (v, s) ->
    exists v' s', runStateT (check n) m = ret (v', s').
  Proof. Admitted.
  Lemma some_injective_isomorphism {A}{a b : A} : Some a ~~~ Some b -> a ~~~ b.
  Proof. Admitted.
  Lemma set_lenv_cond tmp (se : SpecTree.t tmp) le i htp f v :
    let set_se := SpecTree.set i htp f se in
    let set_le := PTree.set i v le in
      ht_ft_cond f ->
      ht_rel f v ->
      lenv_cond se le ->
      lenv_cond set_se set_le.
  Proof. Admitted.
  Lemma unset_lenv_cond {tmp i htp f}{se : SpecTree.t tmp}{le} :
      (tmp ! i ~~~ None)%alist ->
      lenv_cond (SpecTree.set i htp f se) le -> lenv_cond se le.
  Proof. Admitted.
  
  Lemma set_le_lenv_cond tmp (se : SpecTree.t tmp) le i v :
      (tmp ! i)%alist ~~~ None -> lenv_cond se le -> lenv_cond se (PTree.set i v le).
  Proof. Admitted.
  
  Lemma set_se_lenv_cond tmp (se : SpecTree.t tmp) le i htp f v :
    let set_se := SpecTree.set i htp f se in
      ht_ft_cond f ->
      ht_rel f v ->
      le ! i = Some v ->
      lenv_cond se le ->
      lenv_cond set_se le.
  Proof. Admitted.
  Lemma get_neq_idx_neq {tmp i j}{htp : hyper_type_pair} :
    (tmp ! i)%alist ~~~ Some htp -> (tmp ! j)%alist ~~~ None -> i <> j.
  Proof. Admitted.
  Definition Z_to_val : Z -> val := @ht_implement tint_Z32 (int_Z_iso_impl _).
  Lemma min_ex_elim_range {T} P lo hi Pdec (found_branch : Z -> T) not_found_branch :
      ~ P lo ->
      match LangDef.min_ex P lo hi Pdec with
      | inleft (exist n _) => found_branch n
      | inright _ => not_found_branch
      end =
      match LangDef.min_ex P (Z.succ lo) hi Pdec with
      | inleft (exist m _) => found_branch m
      | inright _ => not_found_branch
      end.
  Proof. Admitted.
  
  Fixpoint fold_expr_constr_list_cond {tmp tps} es
    : @fold_synth_expr_wellformed tmp tps es ->
      spec_env_t tmp -> Prop :=
    match es with
    | HNil => fun _ _ => True
    | HCons x ls e es => fun wf se =>
      let (wf, IHwf) := wf in
      oProp1 (synth_expr_ocond me tmp e wf /\
    (omap1 (fun (p : unpair_ft (tp_type_pair x) -> Prop)
                (y : spec_env_t tmp) =>
              p
                (synth_expr_spec me tmp e wf y))
           ht_valid_ft_ocond) 
             )%oprop1 se /\ 
      fold_expr_constr_list_cond es IHwf se
    end.
  Fixpoint synth_stmt_cond {returns}(c : cmd_constr returns) dest tmp :
      synth_stmt_wellformed c dest tmp -> spec_env_t tmp -> GetHighData -> Prop :=
    match c as t return synth_stmt_wellformed t dest tmp -> _ with
    | CCskip => fun _ _ _ => True
    | CClet _ tp _ id c1 c2 =>
      fun (wf : let tmp' := AList.set id (mk_hyper_type_pair tp) tmp in
                ((tmp ! id ~~~ None) * dest_not_exist dest tmp') *
                (synth_stmt_wellformed c1 id tmp * synth_stmt_wellformed c2 dest tmp'))
        se m =>
      synth_stmt_cond c1 id _ (cadr wf) se m /\
      (forall v m1, runStateT (synth_stmt_spec_opt c1 id tmp (cadr wf) se) m = ret (v, m1) ->
                    synth_stmt_cond c2 dest _ (cddr wf)
                                    (SpecTree.set id (mk_hyper_type_pair tp) v se) m1)
    | CCload tp _ e => fun wf se d =>
      oProp1 (synth_lexpr_ocond me tmp e wf) se /\
      oProp1
        ((synth_lexpr_spec me tmp e wf se).(ltype_set_ocond) /\
         (synth_lexpr_spec me tmp e wf se).(ltype_get_extra_ocond))%oprop1 d
    | CCstore _ _ el er => fun wf se d =>
      oProp1 (synth_lexpr_ocond me tmp el (car wf)) se /\
      oProp1 (synth_expr_ocond me tmp er (cdr wf)) se /\
      ht_valid_ft_cond (synth_expr_spec me tmp er (cdr wf) se) /\
      oProp1 (synth_lexpr_spec me tmp el (car wf) se).(ltype_set_ocond) d
    | CCsequence _ c1 c2 =>
      fun (wf : synth_stmt_wellformed c1 dest tmp * synth_stmt_wellformed c2 dest tmp)
        se m =>
      synth_stmt_cond c1 dest tmp (car wf) se m /\
      (forall m1, execStateT (synth_stmt_spec_opt c1 dest tmp (car wf) se) m = ret m1 ->
        synth_stmt_cond c2 dest tmp (cdr wf) se m1)
    | CCifthenelse _ e c_true c_false =>
      fun (wf : synth_expr_wellformed tmp e *
               (synth_stmt_wellformed c_true dest tmp * synth_stmt_wellformed c_false dest tmp))
          se m =>
      oProp1 (synth_expr_ocond me tmp e (car wf)) se /\
      (synth_expr_spec me tmp e (car wf) se = true ->
        synth_stmt_cond c_true dest tmp (cadr wf) se m) /\
      (synth_expr_spec me tmp e (car wf) se = false ->
        synth_stmt_cond c_false dest tmp (cddr wf) se m)
    | CCfor id_it id_end e1 e2 c3 =>
      fun (wf : let tmp' := AList.set id_end int_Z32_pair tmp in
                let tmp'' := AList.set id_it int_Z32_pair tmp' in
                ((tmp ! id_end ~~~ None) * ((tmp' ! id_it ~~~ None) *
                 dest_not_exist dest tmp'')) *
                (synth_expr_wellformed tmp e1 * (synth_expr_wellformed tmp e2 *
                 synth_stmt_wellformed c3 dest tmp'')))
          se m =>
      oProp1 (synth_expr_ocond me tmp e1 (cadr wf)) se /\
      oProp1 (synth_expr_ocond me tmp e2 (caddr wf)) se /\
      (let start := synth_expr_spec me tmp e1 (cadr wf) se in
       let bound := synth_expr_spec me tmp e2 (caddr wf) se in
       let initial_se := SpecTree.set id_end int_Z32_pair bound se in
       forall n, start <= n < bound ->
         forall m',
           execStateT (Ziteri
             (for_step_opt initial_se start (synth_stmt_spec_opt c3 dest _ (cdddr wf)))
             (n - start)
             (ret tt)) m = ret m' ->
         synth_stmt_cond c3 dest _ (cdddr wf)
           (SpecTree.set id_it int_Z32_pair n
             (SpecTree.set id_end int_Z32_pair bound se)) m')
    | CCfold tp _ id_it id_end id_recur id_dest e1 e2 e3 c4 =>
      let htp := mk_hyper_type_pair tp in
      fun (wf : let tmp' := AList.set id_end int_Z32_pair tmp in
                let tmp'' := AList.set id_it int_Z32_pair tmp' in
                let tmp''' := AList.set id_recur (mk_hyper_type_pair tp) tmp'' in
                ((tmp ! id_end ~~~ None) * ((tmp' ! id_it ~~~ None) *
                 ((tmp'' ! id_recur ~~~ None) * ((tmp''' ! id_dest ~~~ None) *
                 (dest_not_exist dest tmp''' * (synth_stmt_pure c4 ~~~ true)))))) *
                (synth_expr_wellformed tmp e1 * (synth_expr_wellformed tmp e2 *
                 (synth_expr_wellformed tmp e3 * synth_stmt_wellformed c4 id_dest tmp'''))))
          se m =>
      oProp1 (synth_expr_ocond me tmp e1 (cadr wf)) se /\
      oProp1 (synth_expr_ocond me tmp e2 (caddr wf)) se /\
      oProp1 (synth_expr_ocond me tmp e3 (cadddr wf)) se /\
      (let start := synth_expr_spec me tmp e1 (cadr wf) se in
       let bound := synth_expr_spec me tmp e2 (caddr wf) se in
      let init := synth_expr_spec me tmp e3 (cadddr wf) se in
      let initial_se := SpecTree.set id_end int_Z32_pair bound se in
       forall n, start <= n < bound ->
        forall recur,
          evalStateT (oZiteri
           (fold_step_opt initial_se start (synth_stmt_spec_opt c4 id_dest _ (cddddr wf)))
           (n - start)
           init) m = ret recur ->
         synth_stmt_cond c4 id_dest _ (cddddr wf)
           (SpecTree.set id_recur htp recur
             (SpecTree.set id_it int_Z32_pair n initial_se))
           m)
    | CCcall argt ret prim args => fun wf se _ =>
      fold_expr_constr_list_cond args wf se
    
    | CCyield tp _ e => fun wf se _ => oProp1 (synth_expr_ocond me tmp e wf) se
    | CCconstr _ _ _ _ el flds _ =>
      fun (wf : synth_lexpr_wellformed tmp el * fold_synth_expr_wellformed tmp flds)
          se m =>
      oProp1 (synth_lexpr_ocond me tmp el (car wf)) se /\
      oProp1 (synth_lexpr_spec me tmp el (car wf) se).(ltype_set_ocond) m /\
      fold_expr_constr_list_cond flds (cdr wf) se
    | CCassert c => fun wf se m => synth_stmt_cond c dest tmp (cdr wf) se m
    | CCdeny c => fun wf se m => synth_stmt_cond c dest tmp (cdr wf) se m
    
    | CCpanic _ _ => fun _ _ _ => True
    | CCfirst _ id_it id_end id_dest e1 e2 c3 c4 c5 =>
      fun (wf : let tmp' := AList.set id_end int_Z32_pair tmp in
                let tmp'' := AList.set id_it int_Z32_pair tmp' in
                ((tmp ! id_end ~~~ None) * ((tmp' ! id_it ~~~ None) *
                 ((tmp'' ! id_dest ~~~ None) * (dest_not_exist dest tmp'' *
                  ((synth_stmt_pure c3 ~~~ true)))))) *
                (synth_expr_wellformed tmp e1 * (synth_expr_wellformed tmp e2 *
                 (synth_stmt_wellformed c3 id_dest tmp'' *
                 (synth_stmt_wellformed c4 dest tmp'' *
                  synth_stmt_wellformed c5 dest tmp'')))))
          se m =>
      oProp1 (synth_expr_ocond me tmp e1 (cadr wf)) se /\
      oProp1 (synth_expr_ocond me tmp e2 (caddr wf)) se /\
      (let start := synth_expr_spec me tmp e1 (cadr wf) se in
       let bound := synth_expr_spec me tmp e2 (caddr wf) se in
       (forall n, start <= n < bound ->
         
         synth_stmt_cond c3 id_dest _ (cadddr wf)
           (SpecTree.set id_it int_Z32_pair n
             (SpecTree.set id_end int_Z32_pair bound se)) m) /\
       (forall n, start <= n < bound  ->
           synth_stmt_cond c4 dest _ (caddddr wf)
             (SpecTree.set id_it int_Z32_pair n
               (SpecTree.set id_end int_Z32_pair bound se)) m) /\
       (
         synth_stmt_cond c5 dest _ (cdddddr wf)
           (SpecTree.set id_it int_Z32_pair bound
                         (SpecTree.set id_end int_Z32_pair bound se)) m))
    | CCtransfer e1 e2 =>
      fun (wf : synth_expr_wellformed tmp e1 *
                 synth_expr_wellformed tmp e2)
          se m =>
      oProp1 (synth_expr_ocond me tmp e1 (car wf)) se /\ oProp1 (synth_expr_ocond me tmp e2 (cdr wf)) se
    | CCrespec _ tmp' c spec =>
      fun (wf : ((tmp ~~~ tmp') * (synth_stmt_pure c ~~~ true)) *
                synth_stmt_wellformed c dest tmp) =>
      synth_stmt_cond c dest tmp (cdr wf)
    | CCrespec_opt _ tmp' c spec =>
      fun (wf : (tmp ~~~ tmp') * synth_stmt_wellformed c dest tmp) =>
      synth_stmt_cond c dest tmp (cdr wf)
    end %alist.
  Inductive synth_stmt_CD (dest: positive) (tmp: AList.t hyper_type_pair) (se: spec_env_t tmp) (m: GetHighData):
    forall {returns} (c: cmd_constr returns) (wf: synth_stmt_wellformed c dest tmp), Prop :=
  | CDskip: forall wf, synth_stmt_CD dest tmp se m CCskip wf
  | CDlet: forall r tp hti id c1 c2 wf,
      synth_stmt_CD id tmp se m c1 (cadr wf) ->
      (forall v m1, runStateT (synth_stmt_spec_opt c1 id tmp (cadr wf) se) m = ret (v, m1) ->
                    synth_stmt_CD dest _ (SpecTree.set id (mk_hyper_type_pair tp) v se) m1 c2 (cddr wf)) ->
      synth_stmt_CD dest tmp se m (@CClet _ r tp hti id c1 c2) wf
  | CDload: forall tp hti e wf,
      oProp1 (synth_lexpr_ocond me tmp e wf) se ->
      oProp1
        (ltype_set_ocond (synth_lexpr_spec me tmp e wf se) /\
         ltype_get_extra_ocond (synth_lexpr_spec me tmp e wf se))%oprop1 m ->
      synth_stmt_CD dest tmp se m (@CCload _ tp hti e) wf
  | CDstore: forall tp hti el er wf,
      oProp1 (synth_lexpr_ocond me tmp el (car wf)) se ->
      oProp1 (synth_expr_ocond me tmp er (cdr wf)) se ->
      ht_valid_ft_cond (synth_expr_spec me tmp er (cdr wf) se) ->
      oProp1 (ltype_set_ocond (synth_lexpr_spec me tmp el (car wf) se)) m ->
      synth_stmt_CD dest tmp se m (@CCstore _ tp hti el er) wf
  | CDsequence: forall r c1 c2 wf,
      synth_stmt_CD dest tmp se m c1 (car wf) ->
      (forall m1 v1, runStateT (synth_stmt_spec_opt c1 dest tmp (car wf) se) m = ret (v1, m1) ->
                     synth_stmt_CD dest tmp se m1 c2 (cdr wf)) ->
      synth_stmt_CD dest tmp se m (@CCsequence _ r c1 c2) wf
  | CDifthenelse: forall r (e: @expr_constr _ tint_bool _) c_true c_false wf,
      oProp1 (synth_expr_ocond me tmp e (car wf)) se ->
      (synth_expr_spec me tmp e (car wf) se = true -> synth_stmt_CD dest tmp se m c_true (cadr wf)) ->
      (synth_expr_spec me tmp e (car wf) se = false -> synth_stmt_CD dest tmp se m c_false (cddr wf)) ->
      synth_stmt_CD dest tmp se m (@CCifthenelse _ r e c_true c_false) wf
  | CDfor: forall id_it id_end
                  (e1 e2: @expr_constr _ tint_Z32 _) (c3: @cmd_constr _ void_unit_pair) wf,
      oProp1 (synth_expr_ocond me tmp e1 (cadr wf)) se ->
      oProp1 (synth_expr_ocond me tmp e2 (caddr wf)) se ->
      (let start := synth_expr_spec me tmp e1 (cadr wf) se in
       let bound := synth_expr_spec me tmp e2 (caddr wf) se in
       let initial_se := SpecTree.set id_end int_Z32_pair bound se in
       forall n, start <= n < bound ->
       forall v m',
       runStateT
         (Ziteri
            (for_step_opt initial_se start
               (synth_stmt_spec_opt c3 dest
                  (AList.set id_it int_Z32_pair (AList.set id_end int_Z32_pair tmp)) 
                  (cdddr wf))) (n - start) (ret tt)) m = 
       ret (v, m') ->
       synth_stmt_CD dest _
                     (SpecTree.set id_it int_Z32_pair n (SpecTree.set id_end int_Z32_pair bound se))
                     m' c3 (cdddr wf)) ->
      synth_stmt_CD dest tmp se m (@CCfor _ id_it id_end e1 e2 c3) wf
  | CDfirst: forall r id_it id_end id_dest
                    (e1 e2: @expr_constr _ tint_Z32 _) c3 (c4 c5: @cmd_constr _ r) wf,
      oProp1 (synth_expr_ocond me tmp e1 (cadr wf)) se ->
      oProp1 (synth_expr_ocond me tmp e2 (caddr wf)) se ->
      (let start := synth_expr_spec me tmp e1 (cadr wf) se in
       let bound := synth_expr_spec me tmp e2 (caddr wf) se in
       forall n,
          start <= n < bound ->
          synth_stmt_CD id_dest _
                        (SpecTree.set id_it int_Z32_pair n (SpecTree.set id_end int_Z32_pair bound se))
                        m c3 (cadddr wf)) ->
      (let start := synth_expr_spec me tmp e1 (cadr wf) se in
       let bound := synth_expr_spec me tmp e2 (caddr wf) se in
       forall n,
          start <= n < bound ->
          synth_stmt_CD dest _
                        (SpecTree.set id_it int_Z32_pair n (SpecTree.set id_end int_Z32_pair bound se))
                        m c4 (caddddr wf)) ->
      (let bound := synth_expr_spec me tmp e2 (caddr wf) se in
       synth_stmt_CD dest _
                     (SpecTree.set id_it int_Z32_pair bound (SpecTree.set id_end int_Z32_pair bound se))
                     m c5 (cdddddr wf)) ->
      synth_stmt_CD dest tmp se m (@CCfirst _ r id_it id_end id_dest e1 e2 c3 c4 c5) wf
  | CDfold: forall tp hti id_it id_end id_recur id_dest
                   (e1 e2: @expr_constr _ tint_Z32 _) e3 (c4: @cmd_constr _ (mk_hyper_type_pair tp)) wf,
      oProp1 (synth_expr_ocond me tmp e1 (cadr wf)) se ->
      oProp1 (synth_expr_ocond me tmp e2 (caddr wf)) se ->
      oProp1 (synth_expr_ocond me tmp e3 (cadddr wf)) se ->
      (let start := synth_expr_spec me tmp e1 (cadr wf) se in
       let bound := synth_expr_spec me tmp e2 (caddr wf) se in
       let init := synth_expr_spec me tmp e3 (cadddr wf) se in
       let initial_se := SpecTree.set id_end int_Z32_pair bound se in
       forall n : Z,
         start <= n < bound ->
       forall recur m',
       runStateT
         (oZiteri
            (fold_step_opt initial_se start
               (synth_stmt_spec_opt c4 id_dest
                  (AList.set id_recur (mk_hyper_type_pair tp)
                     (AList.set id_it int_Z32_pair (AList.set id_end int_Z32_pair tmp))) 
                  (cddddr wf))) (n - start) init) m = 
       ret (recur, m') ->
       synth_stmt_CD id_dest _
                     (SpecTree.set id_recur _ recur (SpecTree.set id_it int_Z32_pair n initial_se))
                     m c4 (cddddr wf)) ->
      synth_stmt_CD dest tmp se m (@CCfold _ tp hti id_it id_end id_recur id_dest e1 e2 e3 c4) wf
  | CDcall: forall argts r prim args wf,
      fold_expr_constr_list_cond args wf se ->
      synth_stmt_CD dest tmp se m (@CCcall _ argts r prim args) wf
  
  | CDtransfer: forall e1 e2 wf,
      oProp1 (synth_expr_ocond me tmp e1 (car wf)) se ->
      oProp1 (synth_expr_ocond me tmp e2 (cdr wf)) se ->
      synth_stmt_CD dest tmp se m (CCtransfer e1 e2) wf
                    
  | CDyield: forall tp hti e wf,
      oProp1 (synth_expr_ocond me tmp e wf) se ->
      synth_stmt_CD dest tmp se m (@CCyield _ tp hti e) wf
  | CDconstr: forall tp hti fld_ids fld_tps el flds constr wf,
      oProp1 (synth_lexpr_ocond me tmp el (car wf)) se ->
      oProp1 (ltype_set_ocond (synth_lexpr_spec me tmp el (car wf) se)) m ->
      fold_expr_constr_list_cond flds (cdr wf) se ->
      synth_stmt_CD dest tmp se m (@CCconstr _ tp hti fld_ids fld_tps el flds constr) wf
  | CDassert: forall c wf,
      synth_stmt_CD dest tmp se m c (cdr wf) ->
      synth_stmt_CD dest tmp se m (@CCassert _ c) wf
  | CDdeny: forall c wf,
      synth_stmt_CD dest tmp se m c (cdr wf) ->
      synth_stmt_CD dest tmp se m (@CCdeny _ c) wf
  
  | CDpanic: forall tp hti wf, synth_stmt_CD dest tmp se m (@CCpanic _ tp hti) wf
  | CDrespec: forall r tmp' (c: cmd_constr r) (spec: spec_env_t tmp' -> DS (tp_ft r)) (wf : (tmp ~~~ tmp') * (synth_stmt_pure c ~~~ true) * synth_stmt_wellformed c dest tmp),
      synth_stmt_CD dest tmp se m c (cdr wf) ->
      synth_stmt_CD dest tmp se m (@CCrespec _ r tmp' c spec) wf
  | CDrespec_opt: forall r tmp' (c: cmd_constr r) (spec: machine_env GetHighData -> spec_env_t tmp' -> DS (tp_ft r)) (wf: (tmp ~~~ tmp') * synth_stmt_wellformed c dest tmp),
      synth_stmt_CD dest tmp se m c (cdr wf) ->
      synth_stmt_CD dest tmp se m (@CCrespec_opt _ r tmp' c spec) wf
  .
  Lemma synth_stmt_CD_cond r (c: cmd_constr r) dest tmp
        (wf: synth_stmt_wellformed c dest tmp) (se: spec_env_t tmp) m:
    synth_stmt_CD dest tmp se m c wf ->
    synth_stmt_cond c dest tmp wf se m.
  Proof. Admitted.
  
  Fixpoint synth_stmt_obligation {returns}(c : cmd_constr returns) dest tmp :
      synth_stmt_wellformed c dest tmp -> spec_env_t tmp -> GetHighData -> Prop :=
    match c as t return synth_stmt_wellformed t dest tmp -> _ with
    | CCskip => fun _ _ _ => True
    | CClet _ tp _ id c1 c2 =>
      fun (wf : let tmp' := AList.set id (mk_hyper_type_pair tp) tmp in
                ((tmp ! id ~~~ None) * dest_not_exist dest tmp') *
                (synth_stmt_wellformed c1 id tmp * synth_stmt_wellformed c2 dest tmp'))
        se m =>
        synth_stmt_obligation c1 id _ (cadr wf) se m /\
        (forall v m', runStateT (synth_stmt_spec_opt c1 id tmp (cadr wf) se) m = ret (v, m') ->
         synth_stmt_obligation c2 dest _ (cddr wf) (SpecTree.set id (mk_hyper_type_pair tp) v se) m')
    | CCload tp _ e => fun wf se _ => True
    | CCstore _ _ el er => fun _ _ _ => True
    | CCsequence _ c1 c2 =>
      fun (wf : synth_stmt_wellformed c1 dest tmp * synth_stmt_wellformed c2 dest tmp)
        se m =>
        synth_stmt_obligation c1 dest tmp (car wf) se m /\
        (forall m', execStateT (synth_stmt_spec_opt c1 dest tmp (car wf) se) m = ret m' ->
         synth_stmt_obligation c2 dest tmp (cdr wf) se m')
    | CCifthenelse _ e c_true c_false =>
      fun (wf : synth_expr_wellformed tmp e *
               (synth_stmt_wellformed c_true dest tmp * synth_stmt_wellformed c_false dest tmp))
        se m =>
        (synth_expr_spec me tmp e (car wf) se = true ->
         synth_stmt_obligation c_true dest tmp (cadr wf) se m) /\
        (synth_expr_spec me tmp e (car wf) se = false ->
         synth_stmt_obligation c_false dest tmp (cddr wf) se m)
    | CCfor id_it id_end e1 e2 c3 =>
      fun (wf : let tmp' := AList.set id_end int_Z32_pair tmp in
                let tmp'' := AList.set id_it int_Z32_pair tmp' in
                ((tmp ! id_end ~~~ None) * ((tmp' ! id_it ~~~ None) *
                 dest_not_exist dest tmp'')) *
                (synth_expr_wellformed tmp e1 * (synth_expr_wellformed tmp e2 *
                 synth_stmt_wellformed c3 dest tmp'')))
          se m =>
      (let start := synth_expr_spec me tmp e1 (cadr wf) se in
       let bound := synth_expr_spec me tmp e2 (caddr wf) se in
       let initial_se := SpecTree.set id_end int_Z32_pair bound se in
       forall n, start <= n < bound ->
         forall m', execStateT (Ziteri
             (for_step_opt initial_se start (synth_stmt_spec_opt c3 dest _ (cdddr wf)))
             (n - start)
             (ret tt)) m = ret m' ->
         synth_stmt_obligation c3 dest _ (cdddr wf)
           (SpecTree.set id_it int_Z32_pair n
             (SpecTree.set id_end int_Z32_pair bound se)) m')
    | CCfold tp _ id_it id_end id_recur id_dest e1 e2 e3 c4 =>
      let htp := mk_hyper_type_pair tp in
      fun (wf : let tmp' := AList.set id_end int_Z32_pair tmp in
                let tmp'' := AList.set id_it int_Z32_pair tmp' in
                let tmp''' := AList.set id_recur (mk_hyper_type_pair tp) tmp'' in
                ((tmp ! id_end ~~~ None) * ((tmp' ! id_it ~~~ None) *
                 ((tmp'' ! id_recur ~~~ None) * ((tmp''' ! id_dest ~~~ None) *
                 (dest_not_exist dest tmp''' * (synth_stmt_pure c4 ~~~ true)))))) *
                (synth_expr_wellformed tmp e1 * (synth_expr_wellformed tmp e2 *
                 (synth_expr_wellformed tmp e3 * synth_stmt_wellformed c4 id_dest tmp'''))))
          se m =>
      (let start := synth_expr_spec me tmp e1 (cadr wf) se in
       let bound := synth_expr_spec me tmp e2 (caddr wf) se in
       let init := synth_expr_spec me tmp e3 (cadddr wf) se in
       let initial_se := SpecTree.set id_end int_Z32_pair bound se in
       forall n, start <= n < bound ->
        forall recur,
          evalStateT (oZiteri
           (fold_step_opt initial_se start (synth_stmt_spec_opt c4 id_dest _ (cddddr wf)))
           (n - start)
           init) m = ret recur ->
         synth_stmt_obligation c4 id_dest _ (cddddr wf)
           (SpecTree.set id_recur htp recur
             (SpecTree.set id_it int_Z32_pair n initial_se))
           m)
    | CCcall argt ret prim args => fun _ _ _ => True
    
    | CCtransfer _ _ => fun _ _ _ => True
    | CCyield tp _ e => fun _ _ _ => True
    | CCconstr _ _ _ _ el flds _ =>
      fun (wf : synth_lexpr_wellformed tmp el * fold_synth_expr_wellformed tmp flds)
          se m => True
    | CCassert c =>
      fun (wf: (synth_stmt_pure c ~~~ true) *
             synth_stmt_wellformed c dest tmp) se m =>
        synth_stmt_obligation c dest tmp (cdr wf) se m
    | CCdeny c =>
      fun (wf: (synth_stmt_pure c ~~~ true) *
             synth_stmt_wellformed c dest tmp) se m =>
        synth_stmt_obligation c dest tmp (cdr wf) se m
    
    | CCpanic _ _ => fun _ _ _ => True
    | CCfirst _ id_it id_end id_dest e1 e2 c3 c4 c5 =>
      fun (wf : let tmp' := AList.set id_end int_Z32_pair tmp in
                let tmp'' := AList.set id_it int_Z32_pair tmp' in
                ((tmp ! id_end ~~~ None) * ((tmp' ! id_it ~~~ None) *
                 ((tmp'' ! id_dest ~~~ None) * (dest_not_exist dest tmp'' *
                  ((synth_stmt_pure c3 ~~~ true)))))) *
                (synth_expr_wellformed tmp e1 * (synth_expr_wellformed tmp e2 *
                 (synth_stmt_wellformed c3 id_dest tmp'' *
                 (synth_stmt_wellformed c4 dest tmp'' *
                  synth_stmt_wellformed c5 dest tmp'')))))
          se m =>
      (let start := synth_expr_spec me tmp e1 (cadr wf) se in
       let bound := synth_expr_spec me tmp e2 (caddr wf) se in
       (forall n, start <= n < bound ->
         
         synth_stmt_obligation c3 id_dest _ (cadddr wf)
           (SpecTree.set id_it int_Z32_pair n
             (SpecTree.set id_end int_Z32_pair bound se)) m) /\
       (forall n, start <= n < bound  ->
           synth_stmt_obligation c4 dest _ (caddddr wf)
             (SpecTree.set id_it int_Z32_pair n
               (SpecTree.set id_end int_Z32_pair bound se)) m) /\
       (
         synth_stmt_obligation c5 dest _ (cdddddr wf)
           (SpecTree.set id_it int_Z32_pair bound
             (SpecTree.set id_end int_Z32_pair bound se)) m))
    | CCrespec _ tmp' c spec =>
      fun (wf : ((tmp ~~~ tmp') * (synth_stmt_pure c ~~~ true)) *
                synth_stmt_wellformed c dest tmp) se m =>
      (forall se m, runStateT (synth_stmt_spec_opt c dest tmp (cdr wf) se) m =
        runStateT (spec (iso_f (caar wf) se)) m) /\
      synth_stmt_obligation c dest _ (cdr wf) se m
    | CCrespec_opt _ tmp' c spec =>
      fun (wf : (tmp ~~~ tmp') * synth_stmt_wellformed c dest tmp) se m =>
        (forall se m, runStateT (spec me (iso_f (car wf) se)) m =
        runStateT (synth_stmt_spec_opt c dest _ (cdr wf) se) m) /\
      
      synth_stmt_obligation c dest _ (cdr wf) se m
    end %alist.
  Inductive synth_stmt_RC (dest:positive) (tmp: AList.t hyper_type_pair) (se: spec_env_t tmp) (m: GetHighData):
    forall {returns} (c: cmd_constr returns) (wf: synth_stmt_wellformed c dest tmp), Prop :=
  | RCskip: forall wf,
      synth_stmt_RC dest tmp se m CCskip wf
  | RClet: forall r tp hti id c1 c2 wf,
      synth_stmt_RC id _ se m c1 (cadr wf) ->
      (forall v m',
          runStateT (synth_stmt_spec_opt c1 id tmp (cadr wf) se) m = ret (v, m') ->
          synth_stmt_RC dest _ (SpecTree.set id (mk_hyper_type_pair tp) v se) m' c2 (cddr wf)) ->
      synth_stmt_RC dest tmp se m (@CClet _ r tp hti id c1 c2) wf
  | RCload: forall tp hti e wf,
      ht_ft_cond (ltype_get (synth_lexpr_spec me tmp e wf se) m) ->
      synth_stmt_RC dest tmp se m (@CCload _ tp hti e) wf
  | RCstore: forall tp hti el er wf,
      synth_stmt_RC dest tmp se m (@CCstore _ tp hti el er) wf
  | RCsequence: forall r c1 c2 wf,
      (forall v' m', runStateT (synth_stmt_spec_opt c1 dest tmp (car wf) se) m = ret (v', m') ->
      synth_stmt_RC dest tmp se m' c2 (cdr wf)) ->
      synth_stmt_RC dest tmp se m (@CCsequence _ r c1 c2) wf
  | RCifthenelse: forall r (e: @expr_constr _ tint_bool _) c_true c_false wf,
      (synth_expr_spec me tmp e (car wf) se = true ->
       synth_stmt_RC dest tmp se m c_true (cadr wf)) ->
      (synth_expr_spec me tmp e (car wf) se = false ->
       synth_stmt_RC dest tmp se m c_false (cddr wf)) ->
      synth_stmt_RC dest tmp se m (@CCifthenelse _ r e c_true c_false) wf
  | RCfor: forall id_it id_end
                  (e1 e2: @expr_constr _ tint_Z32 _) (c3: @cmd_constr _ void_unit_pair) wf,
      synth_stmt_RC dest tmp se m (@CCfor _ id_it id_end e1 e2 c3) wf
  | RCfirst: forall r id_it id_end id_dest
                    (e1 e2: @expr_constr _ tint_Z32 _) c3 (c4 c5: @cmd_constr _ r) wf,
      oProp1 (synth_expr_ocond me tmp e1 (cadr wf)) se ->
      oProp1 (synth_expr_ocond me tmp e2 (caddr wf)) se ->
      let start := synth_expr_spec me tmp e1 (cadr wf) se in
      let bound := synth_expr_spec me tmp e2 (caddr wf) se in
      (forall n, start <= n < bound  ->
           synth_stmt_RC dest _
             (SpecTree.set id_it int_Z32_pair n
               (SpecTree.set id_end int_Z32_pair bound se))
             m c4 (caddddr wf)) ->
      (
         synth_stmt_RC dest _
           (SpecTree.set id_it int_Z32_pair bound
             (SpecTree.set id_end int_Z32_pair bound se)) m c5 (cdddddr wf)) ->
      synth_stmt_RC dest tmp se m (@CCfirst _ r id_it id_end id_dest e1 e2 c3 c4 c5) wf
  | RCfold: forall tp hti id_it id_end id_recur id_dest
                   (e1 e2: @expr_constr _ tint_Z32 _) e3 (c4: @cmd_constr _ (mk_hyper_type_pair tp)) wf,
      oProp1 (synth_expr_ocond me tmp e1 (cadr wf)) se ->
      oProp1 (synth_expr_ocond me tmp e2 (caddr wf)) se ->
      oProp1 (synth_expr_ocond me tmp e3 (cadddr wf)) se ->
      (let start := synth_expr_spec me tmp e1 (cadr wf) se in
       let bound := synth_expr_spec me tmp e2 (caddr wf) se in
       let init := synth_expr_spec me tmp e3 (cadddr wf) se in
       let initial_se := SpecTree.set id_end int_Z32_pair bound se in
       
       forall n, start <= n < bound ->
        forall recur,
          evalStateT (oZiteri
           (fold_step_opt initial_se start (synth_stmt_spec_opt c4 id_dest _ (cddddr wf)))
           (n - start)
           init) m = ret recur ->
         synth_stmt_RC id_dest _
           (SpecTree.set id_recur (mk_hyper_type_pair tp) recur
             (SpecTree.set id_it int_Z32_pair n initial_se)) m c4 (cddddr wf)) ->
      synth_stmt_RC dest tmp se m (@CCfold _ tp hti id_it id_end id_recur id_dest e1 e2 e3 c4) wf
  | RCcall: forall argts r prim args wf,
      ht_list_ft_cond (map2_synth_expr_spec args se wf) ->
      ht_list_valid_ft_cond (map2_synth_expr_spec args se wf) ->
      synth_stmt_RC dest tmp se m (@CCcall _ argts r prim args) wf
  
  | RCtransfer: forall (e1 e2: @expr_constr _ tint_U _) wf,
      synth_stmt_RC dest tmp se m (CCtransfer e1 e2) wf
                    
  | RCyield: forall tp hti e wf,
      oProp1 (synth_expr_ocond me tmp e wf) se ->
      synth_stmt_RC dest tmp se m (@CCyield _ tp hti e) wf
  | RCconstr: forall tp hti fld_ids fld_tps el flds constr wf,
      synth_stmt_RC dest tmp se m (@CCconstr _ tp hti fld_ids fld_tps el flds constr) wf
  | RCassert: forall c wf,
      synth_stmt_RC dest tmp se m (@CCassert _ c) wf
  | RCdeny: forall c wf,
      synth_stmt_RC dest tmp se m (@CCdeny _ c) wf
  | RCghost: forall c wf,
      synth_stmt_RC dest tmp se m (@CCdeny _ c) wf
  | RCpanic: forall tp hti wf,
      synth_stmt_RC dest tmp se m (@CCpanic _ tp hti) wf
  | RCrespec: forall r tmp' (c: cmd_constr r) (spec: spec_env_t tmp' -> DS (tp_ft r)) (wf : (tmp ~~~ tmp') * (synth_stmt_pure c ~~~ true) * synth_stmt_wellformed c dest tmp),
      (forall v m', runStateT (spec (iso_f (caar wf) se)) m = ret (v, m') -> ht_ft_cond v) ->
      synth_stmt_RC dest tmp se m (@CCrespec _ r tmp' c spec) wf
  | RCrespec_opt: forall r tmp' (c: cmd_constr r) (spec: machine_env GetHighData -> spec_env_t tmp' -> DS (tp_ft r)) (wf: (tmp ~~~ tmp') * synth_stmt_wellformed c dest tmp),
      (forall v m', runStateT (spec me (iso_f (car wf) se)) m = ret (v, m') -> ht_ft_cond v) ->
      synth_stmt_RC dest tmp se m (@CCrespec_opt _ r tmp' c spec) wf
  .
  Fixpoint synth_stmt_ret_cond {returns}(c : cmd_constr returns) dest tmp :
    synth_stmt_wellformed c dest tmp -> spec_env_t tmp -> GetHighData -> Prop :=
    match c as t return synth_stmt_wellformed t dest tmp -> _ with
    | CCskip => fun _ _ _ => True
    | CClet _ tp _ id c1 c2 =>
      fun (wf : let tmp' := AList.set id (mk_hyper_type_pair tp) tmp in
                ((tmp ! id ~~~ None) * dest_not_exist dest tmp') *
                (synth_stmt_wellformed c1 id tmp * synth_stmt_wellformed c2 dest tmp'))
          se m =>
        synth_stmt_ret_cond c1 id _ (cadr wf) se m /\
        forall v m', runStateT (synth_stmt_spec_opt c1 id tmp (cadr wf) se) m = ret (v, m') ->
          synth_stmt_ret_cond c2 dest _ (cddr wf) (SpecTree.set id (mk_hyper_type_pair tp) v se) m'
      
    | CCload tp _ e => fun wf se m =>
      ht_ft_cond (ltype_get (synth_lexpr_spec me tmp e wf se) m)
    | CCstore _ _ el er => fun _ _ _ => True
    | CCsequence _ c1 c2 =>
      fun (wf : synth_stmt_wellformed c1 dest tmp * synth_stmt_wellformed c2 dest tmp)
        se m =>
        forall m', execStateT (synth_stmt_spec_opt c1 dest tmp (car wf) se) m = ret m' ->
          synth_stmt_ret_cond c2 dest tmp (cdr wf) se m'
      
    | CCifthenelse _ e c_true c_false =>
      fun (wf : synth_expr_wellformed tmp e *
               (synth_stmt_wellformed c_true dest tmp * synth_stmt_wellformed c_false dest tmp))
        se m =>
      let obt := synth_stmt_ret_cond c_true dest tmp (cadr wf) se m in
      let obf := synth_stmt_ret_cond c_false dest tmp (cddr wf) se m in
      (synth_expr_spec me tmp e (car wf) se = true  -> obt) /\
      (synth_expr_spec me tmp e (car wf) se = false -> obf)
    | CCfor id_it id_end e1 e2 c3 =>
      fun (wf : let tmp' := AList.set id_end int_Z32_pair tmp in
                let tmp'' := AList.set id_it int_Z32_pair tmp' in
                ((tmp ! id_end ~~~ None) * ((tmp' ! id_it ~~~ None) *
                 dest_not_exist dest tmp'')) *
                (synth_expr_wellformed tmp e1 * (synth_expr_wellformed tmp e2 *
                 synth_stmt_wellformed c3 dest tmp'')))
          se m => True
        
    | CCfold tp _ id_it id_end id_recur id_dest e1 e2 e3 c4 =>
      let htp := mk_hyper_type_pair tp in
      fun (wf : let tmp' := AList.set id_end int_Z32_pair tmp in
                let tmp'' := AList.set id_it int_Z32_pair tmp' in
                let tmp''' := AList.set id_recur (mk_hyper_type_pair tp) tmp'' in
                ((tmp ! id_end ~~~ None) * ((tmp' ! id_it ~~~ None) *
                 ((tmp'' ! id_recur ~~~ None) * ((tmp''' ! id_dest ~~~ None) *
                 (dest_not_exist dest tmp''' * (synth_stmt_pure c4 ~~~ true)))))) *
                (synth_expr_wellformed tmp e1 * (synth_expr_wellformed tmp e2 *
                 (synth_expr_wellformed tmp e3 * synth_stmt_wellformed c4 id_dest tmp'''))))
          se m =>
      oProp1 (synth_expr_ocond me tmp e1 (cadr wf)) se /\
      oProp1 (synth_expr_ocond me tmp e2 (caddr wf)) se /\
      oProp1 (synth_expr_ocond me tmp e3 (cadddr wf)) se /\
      (let start := synth_expr_spec me tmp e1 (cadr wf) se in
       let bound := synth_expr_spec me tmp e2 (caddr wf) se in
       let init := synth_expr_spec me tmp e3 (cadddr wf) se in
       let initial_se := SpecTree.set id_end int_Z32_pair bound se in
       
       forall n, start <= n < bound ->
        forall recur,
          evalStateT (oZiteri
           (fold_step_opt initial_se start (synth_stmt_spec_opt c4 id_dest _ (cddddr wf)))
           (n - start)
           init) m = ret recur ->
         synth_stmt_ret_cond c4 id_dest _ (cddddr wf)
           (SpecTree.set id_recur htp recur
             (SpecTree.set id_it int_Z32_pair n initial_se))
           m)
    | CCcall argt ret prim args => fun wf se _ =>
     ht_list_ft_cond (map2_synth_expr_spec args se wf)
    /\ ht_list_valid_ft_cond (map2_synth_expr_spec args se wf)
    | CCtransfer e1 e2 =>
      fun (wf : synth_expr_wellformed tmp e1 * synth_expr_wellformed tmp e2)
        se m =>
        True
    | CCyield tp _ e => fun wf se _ => oProp1 (synth_expr_ocond me tmp e wf) se
    | CCconstr _ _ _ _ el flds _ => fun _ _ _ => True
    | CCassert c => fun _ _ _ => True
    | CCdeny c => fun _ _ _ => True
    
    | CCpanic _ _ => fun _ _ _ => True
    | CCfirst _ id_it id_end id_dest e1 e2 c3 c4 c5 =>
      fun (wf : let tmp' := AList.set id_end int_Z32_pair tmp in
                let tmp'' := AList.set id_it int_Z32_pair tmp' in
                ((tmp ! id_end ~~~ None) * ((tmp' ! id_it ~~~ None) *
                 ((tmp'' ! id_dest ~~~ None) * (dest_not_exist dest tmp'' *
                  ((synth_stmt_pure c3 ~~~ true)))))) *
                (synth_expr_wellformed tmp e1 * (synth_expr_wellformed tmp e2 *
                 (synth_stmt_wellformed c3 id_dest tmp'' *
                 (synth_stmt_wellformed c4 dest tmp'' *
                  synth_stmt_wellformed c5 dest tmp'')))))
          se m =>
       oProp1 (synth_expr_ocond me tmp e1 (cadr wf)) se /\
       oProp1 (synth_expr_ocond me tmp e2 (caddr wf)) se /\ 
       (let start := synth_expr_spec me tmp e1 (cadr wf) se in
       let bound := synth_expr_spec me tmp e2 (caddr wf) se in
       
       (forall n, start <= n < bound  ->
           synth_stmt_ret_cond c4 dest _ (caddddr wf)
             (SpecTree.set id_it int_Z32_pair n
               (SpecTree.set id_end int_Z32_pair bound se)) m) /\
       (
         synth_stmt_ret_cond c5 dest _ (cdddddr wf)
           (SpecTree.set id_it int_Z32_pair bound
             (SpecTree.set id_end int_Z32_pair bound se)) m))
    
    | CCrespec _ tmp' c spec =>
      fun (wf : ((tmp ~~~ tmp') * (synth_stmt_pure c ~~~ true)) *
                synth_stmt_wellformed c dest tmp) se m =>
        (forall v m', runStateT (spec (iso_f (caar wf) se)) m = ret (v, m') -> ht_ft_cond v)
    | CCrespec_opt _ tmp' c spec =>
      fun (wf : (tmp ~~~ tmp') * synth_stmt_wellformed c dest tmp) se m =>
        (forall v m', runStateT (spec me (iso_f (car wf) se)) m = ret (v, m') -> ht_ft_cond v)
    end %alist.
  Lemma synth_stmt_RC_ret_cond r (c: cmd_constr r) dest tmp
        (wf: synth_stmt_wellformed c dest tmp) (se: spec_env_t tmp) m:
      synth_stmt_RC dest tmp se m c wf ->
      synth_stmt_ret_cond c dest tmp wf se m.
  Proof. Admitted.
  Lemma map2_synth_expr_spec_satisfies_ft_cond {tmp argt}
    {arg : expr_constr_list argt}
    (argc : expr_constr_prf_conj arg)
    (wf : fold_synth_expr_wellformed tmp arg)
    (se : spec_env_t tmp) :
      fold_expr_constr_list_cond arg wf se ->
      senv_cond se ->
      ht_list_ft_cond (map2_synth_expr_spec arg se wf).
  Proof. Admitted.
  Lemma map2_synth_expr_spec_satisfies_valid_ft_cond {tmp argt}
    {arg : expr_constr_list argt}
    (argc : expr_constr_prf_conj arg)
    (wf : fold_synth_expr_wellformed tmp arg)
    (se : spec_env_t tmp) :
      fold_expr_constr_list_cond arg wf se ->
      senv_cond se ->
      ht_list_valid_ft_cond (map2_synth_expr_spec arg se wf).
  Proof. Admitted.
Lemma synth_stmt_spec_opt_pure {is_realizing returns htr}(c : cmd_constr returns) :
    synth_stmt_pure c ~~~ true ->
    cmd_constr_prf is_realizing returns htr c ->
    forall dest tmp wf se d v' d',
      runStateT (synth_stmt_spec_opt c dest tmp wf se) d = ret (v', d') ->
      d = d'.
Proof. Admitted.
  Lemma set_senv_cond tmp (se : SpecTree.t tmp) i htp v:
    ht_ft_cond v ->
    senv_cond se ->
    senv_cond (SpecTree.set i htp v se).
  Proof. Admitted.
  
  
Lemma for_mid_opt' {tmp id_it} (se : spec_env_t tmp):
  let tmp' := AList.set id_it int_Z32_pair tmp in
  forall f s0 start steps s,
    steps >= 0 ->
    runStateT (Ziteri (@for_step_opt _ id_it se start f)
                       steps
                       (ret tt)) s0 = ret (tt, s) ->
    forall n, 0%Z <= n <= steps ->
    exists s',
      runStateT (Ziteri (@for_step_opt _ id_it se start f) n (ret tt)) s0 = ret (tt, s') /\
      runStateT (Ziteri (@for_step_opt _ id_it se (start+n) f) (steps-n) (ret tt)) s' = ret (tt, s).
Proof. Admitted.
  
  
  
  
  Opaque AList.get.
  
  
Definition temp_env : Type := PTree.t val.
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  Lemma opt_bool_dec_t__f_none (x:option bool):
    {x = Some true} + {x = Some false \/ x = None}.
  Proof. Admitted.
  Lemma Z2Nat_nonpos_zero:
    forall n, n <= 0%Z -> Z.to_nat n = 0%nat.
  Proof. Admitted.
  Lemma min_ex_empty P lo hi Pn:
    lo >= hi ->
    exists no_n, LangDef.min_ex P lo hi Pn = inright no_n.
  Proof. Admitted.
  Lemma min_ex_lo_satisfy (P:Z->Prop) lo hi Pdec
        (lo_bound: lo <= lo < hi)
        (lo_pass: P lo):
    exists Q, LangDef.min_ex P lo hi Pdec = inleft (exist _ lo Q).
  Proof. Admitted.
  Lemma min_ex_skip_lo {A} {Ql: Z->A} {Qr: A} {P: Z->Prop} {lo hi Pdec}:
    ~ P lo ->
    match LangDef.min_ex P lo hi Pdec with
    | inleft (exist first_n _) => Ql first_n
    | inright _ => Qr
    end =
    match LangDef.min_ex P (Z.succ lo) hi Pdec with
    | inleft (exist first_n _) => Ql first_n
    | inright _ => Qr
    end.
  Proof. Admitted.
  Definition synth_stmt_veri_cond
      {returns}(c : cmd_constr returns) dest tmp wf se mh :=
    
    synth_stmt_cond c dest tmp wf se mh.
End STMT_FUNC.
