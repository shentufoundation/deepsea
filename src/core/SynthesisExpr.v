Require Import BinInt.
Require Import backend.AST.
Require Import backend.Values.HighValues.
Require Import cclib.Integers.
Require Import cclib.Maps.  
Require Import backend.Expressions.ExpMiniC.
Require Import backend.Expressions.SemanticsMiniC.
Require Import backend.Statements.StmtMiniC.
Require Import backend.Cop.
Require Import backend.Ctypes.
Require Import backend.MachineModel.
Require Import DeepSpec.lib.SimpleMaps.
Require Import DeepSpec.lib.SimpleIndexedMaps.
Require Import DeepSpec.lib.OProp.
Require Import DeepSpec.lib.Monad.ContOptionMonad.
Require Import DeepSpec.core.Cval.
Require Import DeepSpec.core.SEnv.
Require Import DeepSpec.core.HyperType.
Require Import DeepSpec.core.HyperTypeInst.
Require Import DeepSpec.core.MemoryModel.
Require Import DeepSpec.core.Syntax.
Require Import DeepSpec.backend.phase.MiniC.Semantics.
  
Section EXPR_FUNC.
  
  Context`{HM : HyperMem}.
  Context {ctx: int256}.
  Context (me : machine_env GetHighData).
  Let genv_t := genv.
  
  
  Fixpoint synth_expr_wellformed
      (tmp : AList.t hyper_type_pair)
     `{HyperTypeImpl}
      (e : expr_constr tp) {struct e} : Type :=
    match e with
    | ECconst_int _ _ f i => True
    | ECconst_int256 _ _ f i => True
    | ECtempvar _ _ i => (tmp ! i ~~~ Some (mk_hyper_type_pair tp))%alist
    | ECbuiltin0 _ _ _ => True
    | ECbuiltin1 _ _ _  _ _ e' => synth_expr_wellformed tmp e'
    | ECunop _ op _ tpo _ _ e' => synth_expr_wellformed tmp e'
    | ECbinop _ op _ tpl tpr _ _ _ el er =>
      synth_expr_wellformed tmp el * synth_expr_wellformed tmp er
    end%type
  with synth_lexpr_wellformed tmp `{HyperTypeImpl}(e : lexpr_constr tp) {struct e}: Type :=
    match e with
    | LCvar _ _ v => True
    | LCfield _ x _ _ _ pfieldi e' => synth_lexpr_wellformed tmp e'
    | LCindex _ _ _ _ _ e' idx =>
      synth_lexpr_wellformed tmp e' * synth_expr_wellformed tmp idx
    | LChash _ _ _ _ _  e' idx =>
      synth_lexpr_wellformed tmp e' * synth_expr_wellformed tmp idx
    end%type.
  
  Fixpoint synth_expr_expr
      (tmp : AList.t hyper_type_pair)
     `{HyperTypeImpl}
      (e : expr_constr tp) : expr :=
    match e with
    | ECconst_int _ _ _ i => Econst_int i (unpair_ty tp)
    | ECconst_int256 _ _ _ i => Econst_int256 i (unpair_ty tp)
    | ECtempvar _ _ tid => Etempvar tid (unpair_ty tp)
    | ECbuiltin0 _ _ _ => Ecall0 Hbuiltin0 (unpair_ty tp)
    | ECbuiltin1 _ _ _ _ _ e' => Ecall1 Hbuiltin1 (synth_expr_expr tmp e') (unpair_ty tp)
    | ECunop _ op _ _ _ _ e' =>
      Eunop op (synth_expr_expr tmp e') (unpair_ty tp)
    | ECbinop _ op _ _ _ _ _ _ el er =>
      Ebinop op (synth_expr_expr tmp el) (synth_expr_expr tmp er) (unpair_ty tp)
    end.
  Fixpoint synth_lexpr_expr tmp `{HyperTypeImpl}(e : lexpr_constr tp) : expr :=
    match e with
    | LCvar _ _ v => Eglob (ident_ext_base v.(ltype_ident)) (unpair_ty tp)
    | LCfield _ x _ _ _ pfieldi e' =>
      Efield (synth_lexpr_expr tmp e') x (unpair_ty tp)
    | LCindex _ _ _ _ _ e' idx =>
      Eindex (synth_lexpr_expr tmp e') (synth_expr_expr tmp idx) (unpair_ty tp)
    | LChash _ _ _ _ _  e' idx =>
      Eindex (synth_lexpr_expr tmp e') (synth_expr_expr tmp idx) (unpair_ty tp)
      
    end.
  
  Fixpoint synth_expr_spec
      (tmp : AList.t hyper_type_pair)
     `{HyperTypeImpl}
      (e : expr_constr tp)
      : synth_expr_wellformed tmp e -> spec_env_t tmp -> unpair_ft tp :=
    match e with
    | ECconst_int _ _ f _ => fun _ _ => f
    | ECconst_int256 _ _ f _ => fun _ _ => f
    | ECtempvar _ _ i => fun wf se => SpecTree.get_eq i se wf
    | ECbuiltin0 _ _ _ => fun wf se => Hquery0 me
    | ECbuiltin1 _ _ _ _ _ e' => fun wf se => Hquery1 me (synth_expr_spec tmp e' wf se)
    | ECunop _ op _ tpo _ _ e' =>
      fun wf se => Hunary (synth_expr_spec tmp e' wf se)
    | ECbinop _ op _ tpl tpr _ _ _ el er =>
      fun wf se => Hbinary (synth_expr_spec tmp el (car wf) se)
                           (synth_expr_spec tmp er (cdr wf) se)
    end.
  
  Fixpoint synth_lexpr_spec tmp `{HyperTypeImpl} (e : lexpr_constr tp)
      : synth_lexpr_wellformed tmp e -> spec_env_t tmp -> ltype_pair tp :=
    match e with
    | LCvar _ _ v => fun _ _ => v
    | LCfield _ tid _ _ _ pfieldi e' => fun wf se =>
      let ptr := synth_lexpr_spec tmp e' wf se in
      field_ltype_pair ptr tid
    | LCindex _ _ _ _ _ e_arr e_idx => fun wf se =>
      let ptr := synth_lexpr_spec tmp e_arr (car wf) se in
      indexing_ltype_pair ptr (synth_expr_spec tmp e_idx (cdr wf) se)
    | LChash _ _ _ _ _ e_arr e_idx => fun wf se =>
      let ptr := synth_lexpr_spec tmp e_arr (car wf) se in
      inthash_ltype_pair ptr (synth_expr_spec tmp e_idx (cdr wf) se)
    end.
  
  
  Fixpoint synth_expr_ocond
      (tmp : AList.t hyper_type_pair)
     `{HyperTypeImpl}
      (e : expr_constr tp)
      : synth_expr_wellformed tmp e -> OProp1 (spec_env_t tmp) :=
    match e with
    | ECconst_int _ _ f i => fun _ => otrue1
    | ECconst_int256 _ _ f i => fun _ => otrue1
    | ECtempvar _ _ t => fun _ => otrue1
    | ECbuiltin0 _ _ _ => fun _ => otrue1                                    
    | ECbuiltin1 _ _ _ _ _ e' => fun wf => synth_expr_ocond tmp e' wf
    | ECunop _ op _ _ _ _ e' => fun wf =>
      synth_expr_ocond tmp e' wf /\
      Hunary_ocond m{ synth_expr_spec tmp e' wf }
    | ECbinop _ op _ _ _ _ _ _ el er => fun wf =>
      synth_expr_ocond tmp el (car wf) /\
      synth_expr_ocond tmp er (cdr wf) /\
      Hbinary_ocond m{ synth_expr_spec tmp el (car wf) }
                    m{ synth_expr_spec tmp er (cdr wf) }
    end%oprop1
  with synth_lexpr_ocond tmp `{HyperTypeImpl}(e : lexpr_constr tp)
      : synth_lexpr_wellformed tmp e -> OProp1 (spec_env_t tmp) :=
    match e with
    | LCvar _ _ v => fun _ => otrue1
    | LCfield _ x _ _ _ pfieldi e' => fun wf =>
      synth_lexpr_ocond tmp e' wf
    | LCindex _ _ _ _ _ e_arr e_idx => fun wf =>
      synth_lexpr_ocond tmp e_arr (car wf) /\
      synth_expr_ocond tmp e_idx (cdr wf) /\
      {{ fun se => 0 <= synth_expr_spec tmp e_idx (cdr wf) se < Hindex_size }}%Z
    | LChash  _ _ _ _ _ e_arr e_idx => fun wf =>
      synth_lexpr_ocond tmp e_arr (car wf) /\
      synth_expr_ocond tmp e_idx (cdr wf)
    end%oprop1.
  
  Fixpoint synth_expr_passthrough_ocond
      (tmp : AList.t hyper_type_pair)
     `{HyperTypeImpl}
      (e : expr_constr tp)
      : synth_expr_wellformed tmp e -> OProp :=
    match e with
    | ECconst_int _ _ f i => fun _ => otrue
    | ECconst_int256 _ _ f i => fun _ => otrue
    | ECtempvar _ _ t => fun _ => otrue
    | ECbuiltin0 _ _ _ => fun _ => otrue                                
    | ECbuiltin1 _ _ _ _ _ e' => fun wf => synth_expr_passthrough_ocond tmp e' wf
    | ECunop _ op _ _ _ _ e' => fun wf => synth_expr_passthrough_ocond tmp e' wf
    | ECbinop _ op _ _ _ _ _ _ el er => fun wf =>
      synth_expr_passthrough_ocond tmp el (car wf) /\
      synth_expr_passthrough_ocond tmp er (cdr wf)
    end%oprop
  with synth_lexpr_passthrough_ocond tmp `{HyperTypeImpl}(e : lexpr_constr tp)
      : synth_lexpr_wellformed tmp e -> OProp :=
    match e with
    | LCvar _ _ v => fun _ => otrue
    | LCfield _ x _ _ _ pfieldi e' => fun wf =>
      synth_lexpr_passthrough_ocond tmp e' wf
    | LCindex _ _ _ _ _ e_arr e_idx => fun wf =>
      synth_lexpr_passthrough_ocond tmp e_arr (car wf) /\
      synth_expr_passthrough_ocond tmp e_idx (cdr wf)
    | LChash  _ _ _ _ _ e_arr e_idx => fun wf =>
      synth_lexpr_passthrough_ocond tmp e_arr (car wf) /\
      synth_expr_passthrough_ocond tmp e_idx (cdr wf)
    end%oprop.
  
  Lemma synth_expr_expr_preserves_type tmp `{HyperTypeImpl} e :
      synth_expr_wellformed tmp e ->
      typeof (synth_expr_expr tmp e) = unpair_ty tp.
      
  Proof. Admitted.
  Lemma synth_lexpr_expr_preserves_type tmp `{HyperTypeImpl} e :
      
      typeof (synth_lexpr_expr tmp e) = unpair_ty tp.
  Proof. Admitted.
  
  Theorem synth_expr_spec_satisfies_ft_cond {tmp}`{HyperType} e :
    forall wf se, expr_constr_prf e ->
      oProp1 (synth_expr_ocond tmp e wf) se -> senv_cond se ->
      ht_ft_cond (synth_expr_spec tmp e wf se).
  Proof. Admitted.
  
  
  Lemma Int256_lt_repr : forall x y,
      (0 <= x < Int256.modulus)%Z ->
      (0 <= y < Int256.modulus)%Z ->
      (Int256.lt (Int256.repr x) (Int256.repr y) = true) <-> (x < y)%Z.
  Admitted.
Require Import Omega.  
  
  Theorem synth_expr_spec_correct {tmp}`{HyperType} e :
    forall wf se le, expr_constr_prf e ->
      oProp1 (synth_expr_ocond tmp e wf) se -> lenv_cond se le ->
      exists v,
        (forall m, eval_rvalue ctx me m le (synth_expr_expr tmp e) v) /\
        ht_rel (synth_expr_spec tmp e wf se) v
  with synth_lexpr_spec_correct {tmp}`{HyperType} e :
    forall wf se le, lexpr_constr_prf e ->
      oProp1 (synth_lexpr_ocond tmp e wf) se -> lenv_cond se le ->
      let ltp := synth_lexpr_spec tmp e wf se in 
        (forall m,
          eval_lvalue ctx me m le (synth_lexpr_expr tmp e) ltp.(ltype_ident)) /\
        HyperLType ltp.
  Proof. Admitted.
  Theorem synth_lexpr_spec_is_ghost_eq {tmp}`{HyperTypeImpl} e wf se :
    (synth_lexpr_spec tmp e wf se).(ltype_ghost) = lexpr_is_ghost e.
  Proof. Admitted.
  
  
  
  
  Lemma synth_lexpr_passthrough tmp `{HyperTypeImpl} e wf :
    lexpr_constr_passthrough_prf e ->
    oProp (synth_lexpr_passthrough_ocond tmp e wf) ->
    forall se, variable_passthrough_prf (synth_lexpr_spec tmp e wf se).
  Proof. Admitted.
End EXPR_FUNC.
