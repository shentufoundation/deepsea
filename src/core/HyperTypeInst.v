Require Import ZArith.
Require Import Znumtheory.
Require Import backend.Values.HighValues.
Require Import cclib.Integers.
Require Import backend.AST.
Require Import backend.Cop.
Require Import backend.Ctypes.
Require Import DeepSpec.core.Cval.
Require Import DeepSpec.core.HyperType.
Require Import DeepSpec.core.MemoryModel.
Require Import DeepSpec.lib.OProp.
Require Import DeepSpec.lib.Monad.ContOptionMonad.
Notation tvoid_unit := (Tpair unit Tvoid).
Section HYPER_TYPE_UNIT.
  Global Instance void_unit_impl : HyperTypeImpl tvoid_unit := {
    ht_cval := fun _ => CVany;
    ht_ft_cond := fun _ => True;
    ht_default := tt;
    ht_valid_ft_cond := fun _ => False;
    ht_valid_ft_ocond := ofalse1;
    
  }.
  Global Instance void_unit : HyperType tvoid_unit.
  Proof. Admitted.
  Definition void_unit_pair := mk_hyper_type_pair tvoid_unit.
  
End HYPER_TYPE_UNIT.
Section HYPER_LTYPE_COMPOSITION.
  Context`{HM : HyperMem}.
  Context {tp : type_pair}.
  Variable (whole : ltype_pair tp).
  Context`{hti : HyperTypeImpl tp, hlt : !HyperLType whole}.
  Section HYPER_LTYPE_FIELD.
    Context {tp_field : type_pair}.
    Variable (tid : ident).
    Context`{htif : !HyperTypeImpl tp_field,
             hfieldi : !HyperFieldImpl tp tp_field tid,
             hfield : !HyperField tp tp_field tid}.
    Definition field_ltype_pair : ltype_pair tp_field := {|
      ltype_tp_marker := create_type_marker tp_field;
      ltype_get s := Hfield_get (whole.(ltype_get) s);
      ltype_set v s :=
        whole.(ltype_set)
          (Hfield_set v (whole.(ltype_get) s))
        s;
      ltype_set_ocond :=
        (whole.(ltype_set_ocond) /\ whole.(ltype_get_extra_ocond))%oprop1;
      ltype_get_extra_ocond :=
        omap1 (fun p y => p (whole.(ltype_get) y)) ht_valid_ft_ocond;
      ltype_ghost := whole.(ltype_ghost);
      ltype_ident := (Field (whole.(ltype_ident)) tid)
    |}.
    Lemma nat_le_discrete (n m : nat) :
        (n <= m -> exists n', n' + n = m)%nat.
    Proof. Admitted.
    Context {htw : HyperType tp}.
    Global Instance field_ltype : HyperLType field_ltype_pair.
    Proof. Admitted.
  End HYPER_LTYPE_FIELD.
  
  Section HYPER_LTYPE_INDEX.
    Context {tp_elem : type_pair}.
    Context`{htif : HyperTypeImpl tp_elem,
             hidxi : !HyperIndexImpl tp tp_elem,
             hidx : !HyperIndex tp tp_elem}.
    Variable (idx : Z).
    Definition indexing_ltype_pair : ltype_pair tp_elem := {|
      ltype_tp_marker := create_type_marker tp_elem;
      ltype_get s := Hindex_get idx (whole.(ltype_get) s);
      ltype_set v s :=
        whole.(ltype_set)
          (Hindex_set idx v (whole.(ltype_get) s))
        s;
      ltype_set_ocond :=
        (whole.(ltype_set_ocond) /\ whole.(ltype_get_extra_ocond))%oprop1;
      ltype_get_extra_ocond := otrue1;
      ltype_ghost := whole.(ltype_ghost);
      ltype_ident := (Index whole.(ltype_ident) (Int256.repr idx))
    |}.
    Require Import cclib.Coqlib.
    
    Global Instance indexing_ltype :
      forall idx_in_bound : 0 <= idx < Hindex_size,
      HyperLType indexing_ltype_pair.
    Proof. Admitted.
  End HYPER_LTYPE_INDEX.
  Section HYPER_LTYPE_HASH.
    Context {tp_elem : type_pair}.
    Context`{htif : HyperTypeImpl tp_elem,
             hidxi : !HyperIntHashImpl tp tp_elem,
             hidx : !HyperIntHash tp tp_elem}.                             
    Variable (idx : int256).
    Definition inthash_ltype_pair : ltype_pair tp_elem := {|
      ltype_tp_marker := create_type_marker tp_elem;
      ltype_get s := Hhash_get idx (whole.(ltype_get) s);
      ltype_set v s :=
        whole.(ltype_set)
          (Hhash_set idx v (whole.(ltype_get) s))
        s;
      ltype_set_ocond :=
        (whole.(ltype_set_ocond) /\ whole.(ltype_get_extra_ocond))%oprop1;
      ltype_get_extra_ocond := otrue1;
      ltype_ghost := whole.(ltype_ghost);
      ltype_ident := (Index whole.(ltype_ident) idx)
   |}.
    Global Instance inthash_ltype :
      HyperLType inthash_ltype_pair.
    Proof. Admitted.
  End HYPER_LTYPE_HASH.
End HYPER_LTYPE_COMPOSITION.
Notation tint_bool := (Tpair bool tint).
Section Integer256Extra.
Lemma modulus_gt_zero : Int256.modulus > 0.
Proof. Admitted.
Theorem sub_sub : forall x y z,
    Int256.sub (Int256.sub x y) z = Int256.sub x (Int256.add y z).
Proof. Admitted.
Theorem add_repr : forall x y,
    Int256.add (Int256.repr x) (Int256.repr y) = Int256.repr (x + y).
Proof. Admitted.
Theorem sub_repr : forall x y,
    Int256.sub (Int256.repr x) (Int256.repr y) = Int256.repr (x - y).
Proof. Admitted.
Theorem mul_repr : forall x y,
    Int256.mul (Int256.repr x) (Int256.repr y) = Int256.repr (x * y).
Proof. Admitted.
Theorem add_shifted : forall x y z,
    Int256.add (Int256.sub x z) (Int256.add y z) = Int256.add x y.
Proof. Admitted.
End Integer256Extra.
Section HYPER_TYPE_BOOL.
  Local Open Scope Z_scope.
  Lemma small_modulo : 0 mod Int256.modulus = 0 /\ 1 mod Int256.modulus = 1.
  Proof. Admitted.
  Definition zero_mod_modulus := proj1 small_modulo.
  Definition one_mod_modulus := proj2 small_modulo.
  Ltac rewrite_unsigned_repr :=
    try unfold Int256.zero, Int256.one;
    try rewrite Int256.unsigned_repr_eq;
    try rewrite zero_mod_modulus;
    try rewrite one_mod_modulus.
  Local Instance int_bool_iso_impl : HyperTypeIsoImpl tint_bool := {
    ht_iso_ty_cond v := match v with
      | Vint i => Int256.unsigned i = 0 \/ Int256.unsigned i = 1
      | _ => False
      end;
    ht_iso_ft_cond f := True;
    ht_iso_default := false;
    ht_implement f := Vint (if f then Int256.one else Int256.zero);
    ht_spec v := match v with
      | Vint i => Int256.unsigned i =? 1
      | _ => false
      end
  }.
  Local Instance int_bool_iso : HyperTypeIso tint_bool.
  Proof. Admitted.
  Global Instance int_bool_impl : HyperTypeImpl tint_bool := _.
  Global Instance int_bool : HyperType tint_bool := _.
  
  Definition int_bool_pair :=
    @mk_hyper_type_pair tint_bool int_bool_impl.
  Lemma ht_ty_cond_0_1 :
      ht_ty_cond tint_bool (CVval (Vint (Int256.repr 0))) /\
      ht_ty_cond tint_bool (CVval (Vint (Int256.repr 1))).
  Proof. Admitted.
  Definition int_bool_zero := proj1 ht_ty_cond_0_1.
  Definition int_bool_one  := proj2 ht_ty_cond_0_1.
  Global Instance int_bool_notbool_impl
      : HyperUnaryImpl Onotbool tint_bool tint_bool := {
    Hunary_cond ft := True;
    Hunary_ocond := otrue1;
    Hunary := negb
  }.
  Global Instance int_bool_notbool : HyperUnaryOp Onotbool tint_bool tint_bool.
  Proof. Admitted.
  Global Instance int_bool_notbool_passthrough
      : HyperUnaryPassthrough Onotbool tint_bool tint_bool.
  
  Global Instance int_bool_or_impl
      : HyperBinaryImpl Oor tint_bool tint_bool tint_bool := {
    Hbinary_cond f f' := True;
    Hbinary_ocond := otrue2;
    Hbinary := orb
  }.
  Global Instance int_bool_or
     : HyperBinaryOp Oor tint_bool tint_bool tint_bool := {
  }.
  Proof. Admitted.
  Global Instance int_bool_or_passthrough
      : HyperBinaryPassthrough Oor tint_bool tint_bool tint_bool.
  
  Global Instance int_bool_xor_impl
      : HyperBinaryImpl Oxor tint_bool tint_bool tint_bool := {
    Hbinary_cond f f' := True;
    Hbinary_ocond := otrue2;
    Hbinary := xorb
  }.
  Global Instance int_bool_xor
      : HyperBinaryOp Oxor tint_bool tint_bool tint_bool := {
  }.
  Proof. Admitted.
  Global Instance int_bool_xor_passthrough
      : HyperBinaryPassthrough Oxor tint_bool tint_bool tint_bool.
  
  Global Instance int_bool_and_impl
      : HyperBinaryImpl Oand tint_bool tint_bool tint_bool := {
    Hbinary_cond f f' := True;
    Hbinary_ocond := otrue2;
    Hbinary := andb
  }.
  Global Instance int_bool_and
      : HyperBinaryOp Oand tint_bool tint_bool tint_bool := {
  }.
  Proof. Admitted.
  Global Instance int_bool_and_passthrough
      : HyperBinaryPassthrough Oand tint_bool tint_bool tint_bool.
  
  Global Instance int_bool_eq_impl
      : HyperBinaryImpl Oeq tint_bool tint_bool tint_bool := {
    Hbinary_cond f f' := True;
    Hbinary_ocond := otrue2;
    Hbinary f f' := negb (xorb f f')
  }.
  Global Instance int_bool_eq
      : HyperBinaryOp Oeq tint_bool tint_bool tint_bool := {
  }.
  Proof. Admitted.
  Global Instance int_bool_eq_passthrough
      : HyperBinaryPassthrough Oeq tint_bool tint_bool tint_bool.
  
  Global Instance int_bool_ne_impl
      : HyperBinaryImpl One tint_bool tint_bool tint_bool := {
    Hbinary_cond f f' := True;
    Hbinary_ocond := otrue2;
    Hbinary := xorb
  }.
  Global Instance int_bool_ne
      : HyperBinaryOp One tint_bool tint_bool tint_bool := {
  }.
  Proof. Admitted.
  Global Instance int_bool_ne_passthrough
      : HyperBinaryPassthrough One tint_bool tint_bool tint_bool.
  
  
End HYPER_TYPE_BOOL.
Section HYPER_TYPE_INT.
  Local Open Scope Z_scope.
  Class IntegerBound (bound : Z) : Prop := {
    integer_bound_within_modulus : Int256.modulus >= bound;
    integer_bound_positive : 0 < bound
  }.
  Definition Z_bounded (bound : Z){_ : IntegerBound bound} := Z.
  Typeclasses Opaque Z_bounded.
  Opaque Int256.repr.
  
Section BOUNDED.
  
  Variable bound : Z.
  Context `{bound_cond : IntegerBound bound}.
  Definition tint_Z_bounded
    := Tpair (Z_bounded bound) tint.
  Lemma unsigned_repr_eq (f : Z_bounded bound)(cond : -1 < f /\ bound > f)
      : Int256.unsigned (Int256.repr f) = f.
  Proof. Admitted.
  Lemma bounded_repr_injective  : forall (f f' : Z_bounded bound)
                                 (cond : -1 < f /\ bound > f)
                                 (cond' : -1 < f' /\ bound > f'),
      (Int256.repr f) = (Int256.repr f') -> f = f'.
  Proof. Admitted.
    
  Instance int_Z_iso_impl : HyperTypeIsoImpl tint_Z_bounded := {
    ht_iso_ty_cond v := match v with
      | Vint i => bound > Int256.unsigned i
      | _ => False
      end;
    ht_iso_ft_cond f := -1 < f /\ bound > f;
    ht_iso_default := 0;
    ht_implement f := (Vint (Int256.repr f));
    ht_spec v := match v with
      | Vint i => Int256.unsigned i
      | _ => 0
      end
   }.
  
  Instance int_Z_iso : HyperTypeIso tint_Z_bounded.
  Proof. Admitted.
  Instance int_Z_impl : HyperTypeImpl tint_Z_bounded := _.
  Instance int_Z : HyperType tint_Z_bounded := _.
  
  
  Instance int_Z_bounded_add_impl
      : HyperBinaryImpl Oadd tint_Z_bounded tint_Z_bounded tint_Z_bounded := {
    Hbinary_cond f f' := f + f' < bound;
    Hbinary_ocond := oprop2 (fun f f' => f + f' < bound);
    Hbinary := Z.add
  }.
  Instance int_Z_bounded_add
      : HyperBinaryOp Oadd tint_Z_bounded tint_Z_bounded tint_Z_bounded := {
  }.
  Proof. Admitted.
  Instance int_Z_bounded_add_passthrough
      : HyperBinaryPassthrough Oadd tint_Z_bounded tint_Z_bounded tint_Z_bounded.
  
  Instance int_Z_bounded_sub_impl
      : HyperBinaryImpl Osub tint_Z_bounded tint_Z_bounded tint_Z_bounded := {
    Hbinary_cond f f' := f >= f';
    Hbinary_ocond := oprop2 (fun f f' => f >= f');
    Hbinary := Z.sub
  }.
  Instance int_Z_bounded_sub
      : HyperBinaryOp Osub tint_Z_bounded tint_Z_bounded tint_Z_bounded := {
  }.
  Proof. Admitted.
  Instance int_Z_bounded_sub_passthrough
      : HyperBinaryPassthrough Osub tint_Z_bounded tint_Z_bounded tint_Z_bounded.
  
  Instance int_Z_bounded_mul_impl
      : HyperBinaryImpl Omul tint_Z_bounded tint_Z_bounded tint_Z_bounded := {
    Hbinary_cond f f' := f * f' < bound;
    Hbinary_ocond := oprop2 (fun f f' => f * f' < bound);
    Hbinary := Z.mul
  }.
  Instance int_Z_bounded_mul
      : HyperBinaryOp Omul tint_Z_bounded tint_Z_bounded tint_Z_bounded := {
  }.
  Proof. Admitted.
  Instance int_Z_bounded_mul_passthrough
      : HyperBinaryPassthrough Omul tint_Z_bounded tint_Z_bounded tint_Z_bounded.
  
  Instance int_Z_bounded_mod_impl
      : HyperBinaryImpl Omod tint_Z_bounded tint_Z_bounded tint_Z_bounded := {
    Hbinary_cond f f' := f' <> 0;
    Hbinary_ocond := oprop2 (fun f f' => f' <> 0);
    Hbinary := Z.modulo
  }.
  Instance int_Z_bounded_mod
      : HyperBinaryOp Omod tint_Z_bounded tint_Z_bounded tint_Z_bounded := {
  }.
  Proof. Admitted.
  Instance int_Z_bounded_mod_passthrough
      : HyperBinaryPassthrough Omod tint_Z_bounded tint_Z_bounded tint_Z_bounded.
  
  
  Instance int_Z_bounded_div_impl
      : HyperBinaryImpl Odiv tint_Z_bounded tint_Z_bounded tint_Z_bounded := {
    Hbinary_cond f f' := f' <> 0;
    Hbinary_ocond := oprop2 (fun f f' => f' <> 0);
    Hbinary := Z.div
  }.
  Instance int_Z_bounded_div
      : HyperBinaryOp Odiv tint_Z_bounded tint_Z_bounded tint_Z_bounded := {
  }.
  Proof. Admitted.
  Instance int_Z_bounded_div_passthrough
      : HyperBinaryPassthrough Odiv tint_Z_bounded tint_Z_bounded tint_Z_bounded.
  
  Instance int_Z_bounded_eq_impl
      : HyperBinaryImpl Oeq tint_Z_bounded tint_Z_bounded tint_bool := {
    Hbinary_cond f f' := True;
    Hbinary_ocond := otrue2;
    Hbinary := Z.eqb
  }.
  Instance int_Z_bounded_eq
      : HyperBinaryOp Oeq tint_Z_bounded tint_Z_bounded tint_bool := {
  }.
  Proof. Admitted.
  Instance int_Z_bounded_eq_passthrough
      : HyperBinaryPassthrough Oeq tint_Z_bounded tint_Z_bounded tint_bool.
  
  Instance int_Z_bounded_ne_impl
      : HyperBinaryImpl One tint_Z_bounded tint_Z_bounded tint_bool := {
    Hbinary_cond f f' := True;
    Hbinary_ocond := otrue2;
    Hbinary f f' := negb (Z.eqb f f')
  }.
  Instance int_Z_bounded_ne
      : HyperBinaryOp One tint_Z_bounded tint_Z_bounded tint_bool := {
  }.
  Proof. Admitted.
  Instance int_Z_bounded_ne_passthrough
      : HyperBinaryPassthrough One tint_Z_bounded tint_Z_bounded tint_bool.
  
  Instance int_Z_bounded_lt_impl
      : HyperBinaryImpl Olt tint_Z_bounded tint_Z_bounded tint_bool := {
    Hbinary_cond f f' := True;
    Hbinary_ocond := otrue2;
    Hbinary := Z.ltb
  }.
  Instance int_Z_bounded_lt
      : HyperBinaryOp Olt tint_Z_bounded tint_Z_bounded tint_bool := {
  }.
  Proof. Admitted.
  Instance int_Z_bounded_lt_passthrough
      : HyperBinaryPassthrough Olt tint_Z_bounded tint_Z_bounded tint_bool.
  
  Instance int_Z_bounded_gt_impl
      : HyperBinaryImpl Ogt tint_Z_bounded tint_Z_bounded tint_bool := {
    Hbinary_cond f f' := True;
    Hbinary_ocond := otrue2;
    Hbinary := Z.gtb
  }.
  Instance int_Z_bounded_gt
      : HyperBinaryOp Ogt tint_Z_bounded tint_Z_bounded tint_bool := {
  }.
  Proof. Admitted.
  Instance int_Z_bounded_gt_passthrough
      : HyperBinaryPassthrough Ogt tint_Z_bounded tint_Z_bounded tint_bool.
  
  Instance int_Z_bounded_le_impl
      : HyperBinaryImpl Ole tint_Z_bounded tint_Z_bounded tint_bool := {
    Hbinary_cond f f' := True;
    Hbinary_ocond := otrue2;
    Hbinary := Z.leb
  }.
  Instance int_Z_bounded_le
      : HyperBinaryOp Ole tint_Z_bounded tint_Z_bounded tint_bool := {
  }.
  Proof. Admitted.
  Instance int_Z_bounded_le_passthrough
      : HyperBinaryPassthrough Ole tint_Z_bounded tint_Z_bounded tint_bool.
  
  Instance int_Z_bounded_ge_impl
      : HyperBinaryImpl Oge tint_Z_bounded tint_Z_bounded tint_bool := {
    Hbinary_cond f f' := True;
    Hbinary_ocond := otrue2;
    Hbinary := Z.geb
  }.
  Instance int_Z_bounded_ge
      : HyperBinaryOp Oge tint_Z_bounded tint_Z_bounded tint_bool := {
  }.
  Proof. Admitted.
  Instance int_Z_bounded_ge_passthrough
      : HyperBinaryPassthrough Oge tint_Z_bounded tint_Z_bounded tint_bool.
  
End BOUNDED.
  Global Instance modulus_bound : IntegerBound Int256.modulus := {
    integer_bound_within_modulus := Z.le_ge _ _ (Zle_refl Int256.modulus);
    integer_bound_positive := Z.gt_lt _ _ (Int256.modulus_pos)
  }.
  Definition tint_Z32 := tint_Z_bounded Int256.modulus.
  Typeclasses Transparent tint_Z32 tint_Z_bounded.
  Definition int_Z32_impl := int_Z_impl Int256.modulus.
  Definition int_Z32 := int_Z Int256.modulus.
  
  Definition int_Z32_pair := @mk_hyper_type_pair tint_Z32 int_Z32_impl .
  
  Existing Instance int_Z32_impl.
  Lemma int_Z32_ty_cond z
      : ht_ty_cond tint_Z32 (CVval (HighValues.Vint (Int256.repr z))).
  Proof. Admitted.
  Definition int_Z32_add_impl := int_Z_bounded_add_impl Int256.modulus.
  Definition int_Z32_sub_impl := int_Z_bounded_sub_impl Int256.modulus.
  Definition int_Z32_mul_impl := int_Z_bounded_mul_impl Int256.modulus.
  Definition int_Z32_mod_impl := int_Z_bounded_mod_impl Int256.modulus.
  Definition int_Z32_div_impl := int_Z_bounded_div_impl Int256.modulus.
  Definition int_Z32_add := int_Z_bounded_add Int256.modulus.
  Definition int_Z32_sub := int_Z_bounded_sub Int256.modulus.
  Definition int_Z32_mul := int_Z_bounded_mul Int256.modulus.
  Definition int_Z32_mod := int_Z_bounded_mod Int256.modulus.
  Definition int_Z32_div := int_Z_bounded_div Int256.modulus.
  Definition int_Z32_add_passthrough := int_Z_bounded_add_passthrough Int256.modulus.
  Definition int_Z32_sub_passthrough := int_Z_bounded_sub_passthrough Int256.modulus.
  Definition int_Z32_mul_passthrough := int_Z_bounded_mul_passthrough Int256.modulus.
  Definition int_Z32_mod_passthrough := int_Z_bounded_mod_passthrough Int256.modulus.
  Definition int_Z32_div_passthrough := int_Z_bounded_div_passthrough Int256.modulus.  
  
  Definition int_Z32_eq_impl := int_Z_bounded_eq_impl Int256.modulus.
  Definition int_Z32_ne_impl := int_Z_bounded_ne_impl Int256.modulus.
  Definition int_Z32_lt_impl := int_Z_bounded_lt_impl Int256.modulus.
  Definition int_Z32_gt_impl := int_Z_bounded_gt_impl Int256.modulus.
  Definition int_Z32_le_impl := int_Z_bounded_le_impl Int256.modulus.
  Definition int_Z32_ge_impl := int_Z_bounded_ge_impl Int256.modulus.
  Definition int_Z32_eq := int_Z_bounded_eq Int256.modulus.
  Definition int_Z32_ne := int_Z_bounded_ne Int256.modulus.
  Definition int_Z32_lt := int_Z_bounded_lt Int256.modulus.
  Definition int_Z32_gt := int_Z_bounded_gt Int256.modulus.
  Definition int_Z32_le := int_Z_bounded_le Int256.modulus.
  Definition int_Z32_ge := int_Z_bounded_ge Int256.modulus.
  Definition int_Z32_eq_passthrough := int_Z_bounded_eq_passthrough Int256.modulus.
  Definition int_Z32_ne_passthrough := int_Z_bounded_ne_passthrough Int256.modulus.
  Definition int_Z32_lt_passthrough := int_Z_bounded_lt_passthrough Int256.modulus.
  Definition int_Z32_gt_passthrough := int_Z_bounded_gt_passthrough Int256.modulus.
  Definition int_Z32_le_passthrough := int_Z_bounded_le_passthrough Int256.modulus.
  Definition int_Z32_ge_passthrough := int_Z_bounded_ge_passthrough Int256.modulus.
End HYPER_TYPE_INT.
Notation Z32 := (Z_bounded Int256.modulus).
Existing Instances int_Z32_impl int_Z32 .
Existing Instances int_Z32_add_impl int_Z32_sub_impl int_Z32_mul_impl
                   int_Z32_mod_impl int_Z32_div_impl.
Existing Instances int_Z32_add int_Z32_sub int_Z32_mul int_Z32_mod int_Z32_div.
Existing Instances int_Z32_add_passthrough int_Z32_sub_passthrough
                   int_Z32_mul_passthrough int_Z32_mod_passthrough
                   int_Z32_div_passthrough.
Existing Instances int_Z32_eq_impl int_Z32_ne_impl int_Z32_lt_impl
                   int_Z32_gt_impl int_Z32_le_impl int_Z32_ge_impl.
Existing Instances int_Z32_eq int_Z32_ne int_Z32_lt int_Z32_gt
                   int_Z32_le int_Z32_ge.
Existing Instances int_Z32_eq_passthrough int_Z32_ne_passthrough
                   int_Z32_lt_passthrough int_Z32_gt_passthrough
                   int_Z32_le_passthrough int_Z32_ge_passthrough.
Require Import Omega.
Section HYPER_TYPE_UNSIGNED.
  
  Definition tint_U := Tpair int256 tint.
  Instance int_U_iso_impl : HyperTypeIsoImpl tint_U :=
    {
      ht_iso_ft_cond f := True;
      ht_iso_ty_cond v := match v with
                            | Vint i => Int256.modulus > Int256.unsigned i
                            | _ => False
                          end;
      ht_iso_default := Int256.zero;
      ht_implement f := (Vint f);
      ht_spec v := match v with
                     | Vint i => i
                     | _ => Int256.zero
                   end
    }.
  Instance int_U_iso : HyperTypeIso tint_U.
  Proof. Admitted.
  Instance int_U_impl : HyperTypeImpl tint_U := _.
  Instance int_U : HyperType tint_U := _.
  
  
  Instance int_U_add_impl : HyperBinaryImpl Oadd tint_U tint_U tint_U :=
    {
      Hbinary_cond f f' := True;
      Hbinary_ocond := otrue2;
      Hbinary x y := Int256.add x y
    }.
  Instance int_U_add : HyperBinaryOp Oadd tint_U tint_U tint_U :=
    {
    }.
  Proof. Admitted.
  Instance int_U_add_passthrough : HyperBinaryPassthrough Oadd tint_U tint_U tint_U.
  
  
  Instance int_U_sub_impl : HyperBinaryImpl Osub tint_U tint_U tint_U :=
    {
      Hbinary_cond f f' := True;
      Hbinary_ocond := otrue2;
      Hbinary x y := Int256.sub x y
    }.
  Instance int_U_sub : HyperBinaryOp Osub tint_U tint_U tint_U :=
    {
    }.
  Proof. Admitted.
  Instance int_U_sub_passthrough : HyperBinaryPassthrough Osub tint_U tint_U tint_U.
  
  
  Instance int_U_mul_impl : HyperBinaryImpl Omul tint_U tint_U tint_U :=
    {
      Hbinary_cond f f' := True;
      Hbinary_ocond := otrue2;
      Hbinary x y := Int256.mul x y
    }.
  Instance int_U_mul : HyperBinaryOp Omul tint_U tint_U tint_U :=
    {
    }.
  Proof. Admitted.
  Instance int_U_mul_passthrough : HyperBinaryPassthrough Omul tint_U tint_U tint_U.
  
  
  Instance int_U_mod_impl : HyperBinaryImpl Omod tint_U tint_U tint_U :=
    {
      Hbinary_cond f f' := f' <> Int256.zero;
      Hbinary_ocond := oprop2 (fun f f' => f' <> Int256.zero);
      Hbinary := Int256.modu
    }.
    
  Instance int_U_mod : HyperBinaryOp Omod tint_U tint_U tint_U :=
    {
    }.
  Proof. Admitted.
  Instance int_U_mod_passthrough : HyperBinaryPassthrough Omod tint_U tint_U tint_U.
  
  
  Instance int_U_div_impl : HyperBinaryImpl Odiv tint_U tint_U tint_U :=
    {
      Hbinary_cond f f' := f' <> Int256.zero;
      Hbinary_ocond := oprop2 (fun f f' => f' <> Int256.zero);
      Hbinary := Int256.divu
    }.
  Instance int_U_div : HyperBinaryOp Odiv tint_U tint_U tint_U :=
    {
    }.
  Proof. Admitted.
  Instance int_U_div_passthrough : HyperBinaryPassthrough Odiv tint_U tint_U tint_U.
  
  
  Instance int_U_eq_impl : HyperBinaryImpl Oeq tint_U tint_U tint_bool :=
    {
      Hbinary_cond f f' := True;
      Hbinary_ocond := otrue2;
      Hbinary := Int256.cmpu Ceq
    }.
  Opaque val_eq_dec.
  
  Instance int_U_eq : HyperBinaryOp Oeq tint_U tint_U tint_bool :=
    {
    }.
  Proof. Admitted.
  Instance int_U_eq_passthrough : HyperBinaryPassthrough Oeq tint_U tint_U tint_bool .
  
  
  Instance int_U_ne_impl : HyperBinaryImpl One tint_U tint_U tint_bool :=
    {
      Hbinary_cond f f' := True;
      Hbinary_ocond := otrue2;
      Hbinary := Int256.cmpu Cne
    }.
  Instance int_U_ne : HyperBinaryOp One tint_U tint_U tint_bool :=
    {
    }.
  Proof. Admitted.
  Instance int_U_ne_passthrough : HyperBinaryPassthrough One tint_U tint_U tint_bool.
  
  
  
  Instance int_U_lt_impl : HyperBinaryImpl Olt tint_U tint_U tint_bool :=
    {
      Hbinary_cond f f' := True;
      Hbinary_ocond := otrue2;
      Hbinary := Int256.cmpu Clt
    }.
  Instance int_U_lt : HyperBinaryOp Olt tint_U tint_U tint_bool :=
    {
    }.
  Proof. Admitted.
  Instance int_U_lt_passthrough : HyperBinaryPassthrough Olt tint_U tint_U tint_bool.
  
  
  Instance int_U_gt_impl : HyperBinaryImpl Ogt tint_U tint_U tint_bool :=
    {
      Hbinary_cond f f' := True;
      Hbinary_ocond := otrue2;
      Hbinary := Int256.cmpu Cgt
    }.
  Instance int_U_gt : HyperBinaryOp Ogt tint_U tint_U tint_bool :=
    {
    }.
  Proof. Admitted.
  Instance int_U_gt_passthrough : HyperBinaryPassthrough Ogt tint_U tint_U tint_bool.
  
  
  Instance int_U_le_impl : HyperBinaryImpl Ole tint_U tint_U tint_bool :=
    {
      Hbinary_cond f f' := True;
      Hbinary_ocond := otrue2;
      Hbinary := Int256.cmpu Cle
    }.
  Instance int_U_le : HyperBinaryOp Ole tint_U tint_U tint_bool :=
    {
    }.
  Proof. Admitted.
  Instance int_U_le_passthrough : HyperBinaryPassthrough Ole tint_U tint_U tint_bool.
  
  
  Instance int_U_ge_impl : HyperBinaryImpl Oge tint_U tint_U tint_bool :=
    {
      Hbinary_cond f f' := True;
      Hbinary_ocond := otrue2;
      Hbinary := Int256.cmpu Cge
    }.
  Instance int_U_ge : HyperBinaryOp Oge tint_U tint_U tint_bool :=
    {
    }.
  Proof. Admitted.
  Instance int_U_ge_passthrough : HyperBinaryPassthrough Oge tint_U tint_U tint_bool.
  
  
  Instance int_U_notint_impl : HyperUnaryImpl Onotint tint_U tint_U :=
    {
      Hunary_cond ft := True;
      Hunary_ocond := otrue1;
      Hunary := Int256.not
    }.
  Instance int_U_notint : HyperUnaryOp Onotint tint_U tint_U.
  Proof. Admitted.
  Instance int_U_notint_passthrough : HyperUnaryPassthrough Onotint tint_U tint_U.
  
  
  Instance int_U_and_impl : HyperBinaryImpl Oand tint_U tint_U tint_U :=
    {
      Hbinary_cond f f' := True;
      Hbinary_ocond := otrue2;
      Hbinary := Int256.and
    }.
  Instance int_U_and : HyperBinaryOp Oand tint_U tint_U tint_U :=
    {
    }.
  Proof. Admitted.
  Instance int_U_and_passthrough : HyperBinaryPassthrough Oand tint_U tint_U tint_U.
  
  
  
  Instance int_U_or_impl : HyperBinaryImpl Oor tint_U tint_U tint_U :=
    {
      Hbinary_cond f f' := True;
      Hbinary_ocond := otrue2;
      Hbinary := Int256.or
    }.
  Instance int_U_or : HyperBinaryOp Oor tint_U tint_U tint_U :=
    {
    }.
  Proof. Admitted.
  Instance int_U_or_passthrough : HyperBinaryPassthrough Oor tint_U tint_U tint_U.
  
  
  Instance int_U_shl_impl : HyperBinaryImpl Oshl tint_U tint_Z32 tint_U :=
    {
      Hbinary_cond f f' := f' < Int256.zwordsize;
      Hbinary_ocond := oprop2 (fun f f' => f' < Int256.zwordsize);
      Hbinary x y := Int256.shl x (Int256.repr y)
    }.
  Lemma lt_zwordsize_lt_iwordsize :
    forall f,
      -1 < f < Int256.zwordsize ->
      Int256.ltu (Int256.repr f) Int256.iwordsize = true.
  Proof. Admitted.
  
  Instance int_U_shl : HyperBinaryOp Oshl tint_U tint_Z32 tint_U :=
    {
    }.
  Proof. Admitted.
  Instance int_U_shl_passthrough : HyperBinaryPassthrough Oshl tint_U tint_Z32 tint_U.
  
  
  Instance int_U_shr_impl : HyperBinaryImpl Oshr tint_U tint_Z32 tint_U :=
    {
      Hbinary_cond f f' := f' < Int256.zwordsize;
      Hbinary_ocond := oprop2 (fun f f' => f' < Int256.zwordsize);
      Hbinary x y := Int256.shru x (Int256.repr y)
    }.
  Instance int_U_shr : HyperBinaryOp Oshr tint_U tint_Z32 tint_U :=
    {
    }.
  Proof. Admitted.
  Instance int_U_shr_passthrough : HyperBinaryPassthrough Oshr tint_U tint_Z32 tint_U.
  
  
  Instance int_U_xor_impl : HyperBinaryImpl Oxor tint_U tint_U tint_U :=
    {
      Hbinary_cond f f' := True;
      Hbinary_ocond := otrue2;
      Hbinary := Int256.xor
    }.
  Instance int_U_xor : HyperBinaryOp Oxor tint_U tint_U tint_U :=
    {
    }.
  Proof. Admitted.
  Instance int_U_xor_passthrough : HyperBinaryPassthrough Oxor tint_U tint_U tint_U.
  
  
  Typeclasses Transparent tint_U.
  Definition int_U_pair := @mk_hyper_type_pair tint_U int_U_impl.
  Existing Instance int_U_impl.
  Lemma int_U_ty_cond z :
    ht_ty_cond tint_U (CVval (HighValues.Vint z)).
  Proof. Admitted.
End HYPER_TYPE_UNSIGNED.
Existing Instances int_U_impl int_U .
Existing Instances
         int_U_add_impl int_U_sub_impl int_U_mul_impl
         int_U_mod_impl int_U_div_impl.
Existing Instances
         int_U_add int_U_sub
         int_U_mul int_U_mod int_U_div.
Existing Instances
         int_U_add_passthrough int_U_sub_passthrough
         int_U_mul_passthrough int_U_mod_passthrough int_U_div_passthrough.
Existing Instances
         int_U_eq_impl int_U_ne_impl
         int_U_lt_impl int_U_gt_impl int_U_le_impl int_U_ge_impl.
Existing Instances
         int_U_eq int_U_ne
         int_U_lt int_U_gt int_U_le int_U_ge.
Existing Instances
         int_U_eq_passthrough int_U_ne_passthrough
         int_U_lt_passthrough int_U_gt_passthrough
         int_U_le_passthrough int_U_ge_passthrough.
Existing Instances
         int_U_notint_impl int_U_and_impl int_U_or_impl
         int_U_shl_impl int_U_shr_impl int_U_xor_impl.
Existing Instances
         int_U_notint int_U_and int_U_or
         int_U_shl int_U_shr int_U_xor.
Existing Instances
         int_U_notint_passthrough int_U_and_passthrough int_U_or_passthrough
         int_U_shl_passthrough int_U_shr_passthrough int_U_xor_passthrough.
Require Import backend.MachineModel.
Section WITH_DATA.
Context `{LayerSpec : LayerSpecClass}.
Local Arguments HyperBuiltin0 {_} {tp} {hti} (HyperBuiltin0Impl0).
Local Arguments HyperBuiltin1 {_} {arg_tp} {tp} {arg_hti} {hti} (HyperBuiltin1Impl0).
Instance builtin0_address_impl : HyperBuiltin0Impl tint_U
  := Build_HyperBuiltin0Impl tint_U me_address Baddress.
Instance builtin0_address : HyperBuiltin0 builtin0_address_impl.
constructor; reflexivity.
Qed.
Instance builtin0_origin_impl : HyperBuiltin0Impl tint_U
  := Build_HyperBuiltin0Impl tint_U me_origin Borigin.
Instance builtin0_origin : HyperBuiltin0 builtin0_origin_impl.
constructor; reflexivity.
Qed.
Instance builtin0_caller_impl : HyperBuiltin0Impl tint_U
  := Build_HyperBuiltin0Impl tint_U me_caller Bcaller.
Instance builtin0_caller : HyperBuiltin0 builtin0_caller_impl.
constructor; reflexivity.
Qed.
Instance builtin0_callvalue_impl : HyperBuiltin0Impl tint_U
  := Build_HyperBuiltin0Impl tint_U me_callvalue Bcallvalue.
Instance builtin0_callvalue : HyperBuiltin0 builtin0_callvalue_impl.
constructor; reflexivity.
Qed.
Instance builtin0_coinbase_impl : HyperBuiltin0Impl tint_U
  := Build_HyperBuiltin0Impl tint_U me_coinbase Bcoinbase.
Instance builtin0_coinbase : HyperBuiltin0 builtin0_coinbase_impl.
constructor; reflexivity.
Qed.
Instance builtin0_timestamp_impl : HyperBuiltin0Impl tint_U
  := Build_HyperBuiltin0Impl tint_U me_timestamp Btimestamp.
Instance builtin0_timestamp : HyperBuiltin0 builtin0_timestamp_impl.
constructor; reflexivity.
Qed.
Instance builtin0_number_impl : HyperBuiltin0Impl tint_U
  := Build_HyperBuiltin0Impl tint_U me_number Bnumber.
Instance builtin0_number : HyperBuiltin0 builtin0_number_impl.
constructor; reflexivity.
Qed.
Instance builtin1_balance_impl : HyperBuiltin1Impl tint_U tint_U
  := Build_HyperBuiltin1Impl tint_U tint_U me_balance Bbalance.
Instance builtin1_balance : HyperBuiltin1 builtin1_balance_impl.
constructor.
- 
  constructor.
- 
  intros.
  simpl in H0.
  exists (Vint f).
  split.
  + symmetry. apply H0.
  + reflexivity.
Qed.
Instance builtin1_blockhash_impl : HyperBuiltin1Impl tint_U tint_U
  := Build_HyperBuiltin1Impl tint_U tint_U me_blockhash Bblockhash.
Instance builtin1_blockhash : HyperBuiltin1 builtin1_blockhash_impl.
constructor.
- 
  constructor.
- 
  intros.
  simpl in H0.
  exists (Vint f).
  split.
  + symmetry. apply H0.
  + reflexivity.
Qed.
End WITH_DATA.
Require Import backend.SymbolicKeccak.
Inductive hashvalue : Set :=
| hashval_int256 : int256 -> hashvalue
| hashval_hash1 : hashvalue -> hashvalue
| hashval_hash2 : hashvalue -> hashvalue -> hashvalue.
Fixpoint val_of_hashvalue hv :=
  match hv with
  | hashval_int256 i => Vint i
  | hashval_hash1 hv1 => sha_1 (val_of_hashvalue hv1)
  | hashval_hash2 hv1 hv2 => sha_2 (val_of_hashvalue hv1) (val_of_hashvalue hv2)
  end.
Lemma val_of_hashvalue_injective : forall hv hv',
    val_of_hashvalue hv = val_of_hashvalue hv' ->
    hv = hv'.
Proof. Admitted.
  
Definition tint_hashvalue := Tpair hashvalue tint.
Instance int_hashvalue_impl : HyperTypeImpl tint_hashvalue := {
  ht_cval hv := CVval (val_of_hashvalue hv);
  ht_default := hashval_int256 Int256.zero;
  ht_ft_cond hv := True;
  ht_valid_ft_cond hv := True;
  ht_valid_ft_ocond := otrue1;
  
}.
Instance int_hashvalue : HyperType tint_hashvalue.
 constructor.
 - intros hv _.   
   eexists. reflexivity.
 - simpl. tauto.
 - simpl. tauto.
Qed.
Definition int_hashvalue_pair :=
    @mk_hyper_type_pair tint_hashvalue int_hashvalue_impl.
Global Instance int_hashvalue_hash1_impl
  : HyperUnaryImpl Osha_1 tint_hashvalue tint_hashvalue := {
 Hunary_cond ft := True;
 Hunary_ocond := otrue1;
 Hunary := hashval_hash1
}.
Global Instance int_hashvalue_hash1 : HyperUnaryOp Osha_1 tint_hashvalue tint_hashvalue.
Proof. Admitted.
Global Instance int_hashvalue_hash1_passthrough : HyperUnaryPassthrough  Osha_1 tint_hashvalue tint_hashvalue.
Global Instance int_hashvalue_hash1_U_impl
  : HyperUnaryImpl Osha_1 tint_U tint_hashvalue := {
 Hunary_cond ft := True;
 Hunary_ocond := otrue1;
 Hunary f := (hashval_hash1 (hashval_int256 f))
}.
             
Global Instance int_hashvalue_hash1_U : HyperUnaryOp Osha_1 tint_U tint_hashvalue.
Proof. Admitted.
Global Instance int_hashvalue_hash1_U_passthrough : HyperUnaryPassthrough Osha_1 tint_U tint_hashvalue.
Global Instance int_hashvalue_hash2_impl :
  HyperBinaryImpl Osha_2 tint_hashvalue tint_hashvalue tint_hashvalue := {
    Hbinary_cond f f' := True;
    Hbinary_ocond := otrue2;
    Hbinary := hashval_hash2
  }.
  Global Instance int_hashvalue_hash2 
     : HyperBinaryOp Osha_2 tint_hashvalue tint_hashvalue tint_hashvalue := {
  }.
  Proof. Admitted.
  
  Global Instance int_hashvalue_hash2_passthrough
     : HyperBinaryPassthrough Osha_2 tint_hashvalue tint_hashvalue tint_hashvalue := {
                                                                                    }.
  
Global Instance int_hashvalue_hash2_U1impl :
  HyperBinaryImpl Osha_2 tint_U tint_hashvalue tint_hashvalue := {
    Hbinary_cond f f' := True;
    Hbinary_ocond := otrue2;
    Hbinary x y := hashval_hash2 (hashval_int256 x) y
  }.
  Global Instance int_hashvalue_hash2_U1 
     : HyperBinaryOp Osha_2 tint_U tint_hashvalue tint_hashvalue := {
  }.
  Proof. Admitted.
  
  Global Instance int_hashvalue_hash2_U1_passthrough
     : HyperBinaryPassthrough Osha_2 tint_U tint_hashvalue tint_hashvalue := {
                                                                                    }.
  
Global Instance int_hashvalue_hash2_U2_impl :
  HyperBinaryImpl Osha_2 tint_hashvalue tint_U tint_hashvalue := {
    Hbinary_cond f f' := True;
    Hbinary_ocond := otrue2;
    Hbinary x y := hashval_hash2 x (hashval_int256 y)
  }.
  Global Instance int_hashvalue_hash2_U2
     : HyperBinaryOp Osha_2 tint_hashvalue tint_U tint_hashvalue := {
  }.
  Proof. Admitted.
  
  Global Instance int_hashvalue_hash2_U2_passthrough
     : HyperBinaryPassthrough Osha_2 tint_hashvalue tint_U tint_hashvalue := {
                                                                                    }.
  
Global Instance int_hashvalue_hash2_U12_impl :
  HyperBinaryImpl Osha_2 tint_U tint_U tint_hashvalue := {
    Hbinary_cond f f' := True;
    Hbinary_ocond := otrue2;
    Hbinary x y := hashval_hash2 (hashval_int256 x) (hashval_int256 y)
  }.
  Global Instance int_hashvalue_hash2_U12
     : HyperBinaryOp Osha_2 tint_U tint_U tint_hashvalue := {
  }.
  Proof. Admitted.
  
  Global Instance int_hashvalue_hash2_U12_passthrough
     : HyperBinaryPassthrough Osha_2 tint_U tint_U tint_hashvalue := {
                                                                                    }.
  
  Definition hashvalue_eq_dec : forall (hv hv' : hashvalue), 
      {hv = hv'} + {hv <> hv'}.
    decide equality.
    apply Int256.eq_dec.
  Defined.
  
  Definition hashvalue_eqb (hv hv' : hashvalue) : bool :=
    if hashvalue_eq_dec hv hv' then true else false.    
  Global Instance hashvalue_bool_eq_impl
    : HyperBinaryImpl Oeq tint_hashvalue tint_hashvalue tint_bool := {
    Hbinary_cond f f' := True;
    Hbinary_ocond := otrue2;
    Hbinary f f' := hashvalue_eqb f f'
  }.
  Global Instance hashvalue_bool_eq
      : HyperBinaryOp Oeq tint_hashvalue tint_hashvalue tint_bool := {
  }.
  Proof. Admitted.
  Global Instance hashvalue_bool_eq_passthrough
      : HyperBinaryPassthrough Oeq tint_hashvalue tint_hashvalue tint_bool.
  
  Global Instance hashvalue_bool_ne_impl
      : HyperBinaryImpl One tint_hashvalue tint_hashvalue tint_bool := {
    Hbinary_cond f f' := True;
    Hbinary_ocond := otrue2;
    Hbinary f f' := negb (hashvalue_eqb f f')
  }.
  Global Instance hashvalue_bool_ne
      : HyperBinaryOp One tint_hashvalue tint_hashvalue tint_bool := {
  }.
  Proof. Admitted.
  Global Instance int_hashvalue_ne_passthrough
      : HyperBinaryPassthrough One tint_hashvalue tint_hashvalue tint_bool.
  
