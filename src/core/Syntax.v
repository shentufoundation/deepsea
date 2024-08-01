Require Import BinPosDef.
Require Import List.
Require Import Bool.
Require Import BinInt.
Require Import backend.AST.
Require Import backend.MemoryModel.
Require Import backend.Values.HighValues.
Require Import cclib.Integers.
Require Import cclib.Maps.
Require Import backend.Statements.StmtMiniC.
Require Import backend.Cop.
Require Import backend.Ctypes.
Require Import backend.MachineModel.
Require Import DeepSpec.core.Cval.
Require Import DeepSpec.core.SEnv.
Require Import DeepSpec.core.HyperType.
Require Import DeepSpec.core.HyperTypeInst.
Require Import DeepSpec.core.MemoryModel.
Require Import DeepSpec.lib.OProp.
Require Import DeepSpec.lib.Monad.ContOptionMonad.

Generalizable Variables tp tpl tpr tpo.

Section VARIABLE.
  Context `{HM : HyperMem}.

  Definition variable tp := ltype_pair tp.
  Definition offset_zero := Int256.zero.

  Record variable_prf {tp} `{HyperTypeImpl tp} (v : variable tp) : Prop :=
    mk_var_prf {
      VARoffset_zero : v.(ltype_ident) = Field Global (ident_ext_base (v.(ltype_ident)));
      VARltype : HyperLType v
    }.
  Global Arguments VARoffset_zero {_ _ _} _.
  Global Arguments VARltype {_ _ _} _.

  Class variable_passthrough_prf {tp} `{HyperTypeImpl tp} (v : variable tp) : Prop :=
    mk_var_pt_prf {
      VAR_relate_impl_eq : forall j d1 d2,
        relate_AbData j d1 d2 ->
        (v.(ltype_get) d1) = (v.(ltype_get) d2);
      VAR_relate_impl_update : forall j d1 d2 f,
        relate_AbData j d1 d2 ->
        relate_AbData j (v.(ltype_set) f d1) (v.(ltype_set) f d2);
      VAR_match_impl_update : forall j d m f,
        match_AbData d m j ->
        match_AbData (v.(ltype_set) f d) m j
    }.
End VARIABLE.

Coercion create_type_pair_marker := @create_type_marker type_pair.

Section EXPR_CONSTR.
  Context `{HM : HyperMem}.

  Inductive expr_constr : forall tp {hti : HyperTypeImpl tp}, Type :=
  | ECconst_int : forall tp `{HyperTypeImpl tp},
    unpair_ft tp -> Int.int -> expr_constr tp
  | ECconst_int256 : forall tp `{HyperTypeImpl tp},
    unpair_ft tp -> Int256.int -> expr_constr tp
  | ECtempvar : forall tp `{HyperTypeImpl tp},
    ident -> expr_constr tp
  | ECbuiltin0 : forall tp `{HyperTypeImpl tp, !HyperBuiltin0Impl tp},
    expr_constr tp
  | ECbuiltin1 : forall atp tp `{HyperTypeImpl atp, HyperTypeImpl tp, !HyperBuiltin1Impl atp tp},
    expr_constr atp -> expr_constr tp
  | ECunop : forall tp op `{HyperTypeImpl tp, HyperTypeImpl tpo, HyperUnaryImpl op tpo tp},
    expr_constr tpo -> expr_constr tp
  | ECbinop : forall tp op `{HyperTypeImpl tp, HyperTypeImpl tpl, HyperTypeImpl tpr,
                             HyperBinaryImpl op tpl tpr tp},
    expr_constr tpl -> expr_constr tpr -> expr_constr tp
  with lexpr_constr : forall tp `{hti : HyperTypeImpl tp}, Type :=
  | LCvar : forall`{HyperTypeImpl tp}(v : variable tp), lexpr_constr tp
  | LCfield : forall tp id `{HyperTypeImpl tp, HyperTypeImpl tpl, !HyperFieldImpl tpl tp id},
    lexpr_constr tpl -> lexpr_constr tp
  | LCindex : forall tp `{HyperTypeImpl tp, HyperTypeImpl tpl, !HyperIndexImpl tpl tp},
    lexpr_constr tpl -> expr_constr tint_Z32 -> lexpr_constr tp
  | LChash : forall tp `{HyperTypeImpl tp, HyperTypeImpl tpl, !HyperIntHashImpl tpl tp},
    lexpr_constr tpl -> expr_constr tint_U -> lexpr_constr tp.

  Fixpoint lexpr_is_ghost `{HyperTypeImpl}(e : lexpr_constr tp) :=
    match e with
    | LCvar _ _ v => v.(ltype_ghost)
    | LCfield _ tid _ _ _ pfieldi e' => lexpr_is_ghost e'
    | LCindex _ _ _ _ _ e_arr e_idx => lexpr_is_ghost e_arr
    | LChash _ _ _ _ _ e_arr e_idx => lexpr_is_ghost e_arr
    end.

  Inductive expr_constr_prf : forall `{HyperType tp}, expr_constr tp -> Prop :=
  | ECPconst_int256 : forall`{HyperType tp} f i (fc : ht_ft_cond f)
                            (rel : ht_cval f = CVval (Vint i)), expr_constr_prf (ECconst_int256 _ f i)
  | ECPtempvar : forall`{ht : HyperType tp} t, expr_constr_prf (ECtempvar tp t)
  | ECPbuiltin0 : forall `{HyperBuiltin0 (LayerSpec := LayerSpec) tp, !HyperType tp}, expr_constr_prf (ECbuiltin0 tp)
  | ECPbuiltin1 : forall atp `{HyperBuiltin1 (LayerSpec := LayerSpec) atp tp, !HyperType atp, !HyperType tp} e,
    expr_constr_prf e ->
    expr_constr_prf (ECbuiltin1 atp tp e)
  | ECPunop : forall op `{HyperUnaryOp op tpo tp, ht : !HyperType tp, hto : !HyperType tpo} e,
    expr_constr_prf e -> expr_constr_prf (ECunop _ op e)
  | ECPbinop : forall op `{HyperBinaryOp op tpl tpr tp, ht : !HyperType tp,
                             htl : !HyperType tpl, htr : !HyperType tpr} el er,
    expr_constr_prf el -> expr_constr_prf er ->
    expr_constr_prf (ECbinop _ op el er)
  with lexpr_constr_prf : forall`{HyperType tp}, lexpr_constr tp -> Prop :=
  | LCPvar : forall`{HyperType tp} var (v_prf : variable_prf var),
    lexpr_constr_prf (LCvar var)
  | LCPfield : forall id `{HyperField tpl tp id, !HyperType tpl, !HyperType tp} e (ec : lexpr_constr_prf e),
    lexpr_constr_prf (LCfield tp id e)
  | LCPindex : forall`{HyperIndex tpl tp, !HyperType tp, !HyperType tpl} e idx,
    lexpr_constr_prf e -> expr_constr_prf idx
