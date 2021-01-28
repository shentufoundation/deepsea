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

(** Abstract syntax trees for DeepSpec expressions and statements.  *)

(* Standard library modules *)
Require Import BinPosDef.  (* positive_scope *)
Require Import List.
Require Import Bool.
Require Import BinInt.

(* CompCert modules *)
Require Import backend.AST.
(*Require Import compcert.common.Events. *)
Require Import backend.MemoryModel.
Require Import backend.Values.HighValues.
Require Import cclib.Integers.
Require Import cclib.Maps.
Require Import backend.Statements.StmtMiniC.
Require Import backend.Cop.
Require Import backend.Ctypes.
Require Import backend.MachineModel.

(* DeepSpec modules *)
Require Import DeepSpec.core.Cval.
Require Import DeepSpec.core.SEnv.
Require Import DeepSpec.core.HyperType.
Require Import DeepSpec.core.HyperTypeInst.
Require Import DeepSpec.core.MemoryModel.
Require Import DeepSpec.lib.OProp.
Require Import DeepSpec.lib.Monad.ContOptionMonad.

Generalizable Variables tp tpl tpr tpo.  (* Enabling `{ } and `( ) *)

(* DeepSpec variables are represented as lenses to the abstract state + a block number.
  This is a special case of ltype_pair. *)
Section VARIABLE.
  (* Context `{LayerSpec : LayerSpecClass}. *)
  Context `{HM : HyperMem}.
  Definition variable tp := ltype_pair tp.

  (* TODO: move this to the backend director: *)
  Definition offset_zero := Int256.zero.
  
  Record variable_prf {tp}`{HyperTypeImpl tp}(v : variable tp) : Prop
      := mk_var_prf {
    (* This is the only difference from the general ltype_pair: we require the offset to be zero. *)
    VARoffset_zero : v.(ltype_ident) = Field Global (ident_ext_base (v.(ltype_ident)));
    VARltype : HyperLType v
  }.
  Global Arguments VARoffset_zero {_ _ _} _.
  Global Arguments VARltype       {_ _ _} _.

  (* This should better be named HyperLTypePassthrough.
     For each operation (here, variable reads and writes) we define a Class with the side conditions that are needed to prove that
     passthrough methods using that operation are correct.

    Then for each variable in the DeepSpec source program, Edsger will generate an instance.

    These three conditions are better seen as conditions of the refinement relation than on the lens itself.
   *)
  (* TODO: Sadly, we will probably have to fix this sooner rather than later. *)
  Class variable_passthrough_prf {tp}`{HyperTypeImpl tp}(v : variable tp) : Prop
    := mk_var_pt_prf {
    (* When reading values from related ds, the results are equal modolo injection. *)
    VAR_relate_impl_eq j d1 d2 :
      relate_AbData j d1 d2 ->
      (v.(ltype_get) d1) = (v.(ltype_get) d2);

    (* When writing equal values, the relation still holds. *)
    VAR_relate_impl_update j d1 d2 :
      relate_AbData j d1 d2 ->
      forall f,
        relate_AbData j (v.(ltype_set) f d1) (v.(ltype_set) f d2);

    (* Intuitively, this says that writing using the lens does not change the ++++ part of the abstract data.
       (Or even if it did change, at least it would still be related.)

       [ raw memory ][++++  abstract data]
       [ raw memory  :::::][abstract data]
     *)
    VAR_match_impl_update j d m :
      match_AbData d m j ->
      forall f,
        match_AbData (v.(ltype_set) f d) m j;
    (*
    (* When writing values to underlay's private variable, overlay's validStateHigh
       should be presrved, since it should never metion underlay's private variables. *)
    VAR_valid_update d:
      validStateHigh d ->
      forall f,
        validStateHigh (v.(ltype_set)f d)
*)
  }.
End VARIABLE.

Coercion create_type_pair_marker := @create_type_marker type_pair.

(** * Expressions *)
(** Clight expression is a clone of Clight expression aiming for synthesizing
    CertiKOS.  It started as a subset of Clight, but diverged in several key
    points making it more suitable for synthesizing and proving properties.
    - Separated L-value and R-value expressions.
    - No addressable local variables; all local variables are temporaries.
    - Evaluation of either expression never requires memory access.
    - R-value can no longer return structures or unions ([access_mode ty =
      By_copy]); arrays and functions are still possible ([access_mode ty =
      By_reference]), but this is subject to change in the future.
    - No floating point constants as they are not used in kernels.
    - Flattened address-of operator ([&]): inlining all potential L-value
      expressions.

    The two inductively defined data types, [expr_constr] (expression
    construct) and [lexpr_constr] (L-value expression construct), just like
    temporaries, are also annotated with the [type_pair] indicating the type
    of their evaluation results.  They are heavily annotated with type classes
    defined in {{DeepSpec.HyperType.html}HyperType}.  The type classes play
    a huge role in [synth_expr].

    All branches in [expr_constr] are pretty much the same to their Clight
    counterparts, except for [ECaddrof_var], which is the inlined version of
    [Eaddrof (Evar i t) t'], and [ECaddrof_pfield], which is the inlined
    version of [Eaddrof (Efield (Ederef e t) x t') t''].
*)
Section EXPR_CONSTR.
  Context `{HM : HyperMem}.
    (* For instance, lexpr variable definitions contain a lens, so they need to know the type of
       the abstract data, which is part of LayerSpecClass, which is implicitly
       part of the context because it is a parameter of HM. *)

  (* Annotating (indexing) [ht] instead of parametrizing it to make declaring
     subclasses in constructors easier. *)
  Inductive expr_constr : forall tp {hti : HyperTypeImpl tp}, Type :=
  | ECconst_int : forall tp `{HyperTypeImpl tp},
    unpair_ft tp -> Int.int -> expr_constr tp          (**r integer literal *)
  | ECconst_int256 : forall tp `{HyperTypeImpl tp},
    unpair_ft tp -> Int256.int -> expr_constr tp          (**r integer literal *)
  | ECtempvar : forall tp `{HyperTypeImpl tp},
      ident -> expr_constr tp                         (**r temporary variable *)
  | ECbuiltin0 : forall tp `{HyperTypeImpl tp, !HyperBuiltin0Impl tp},
      expr_constr tp  (**r Ethereum EVM builtin *)
  | ECbuiltin1 : forall atp tp `{HyperTypeImpl atp, HyperTypeImpl tp, !HyperBuiltin1Impl atp tp},
      expr_constr atp ->  expr_constr tp  (**r Ethereum EVM builtin *)
  | ECunop : forall tp op `{HyperTypeImpl tp, HyperTypeImpl tpo, HyperUnaryImpl op tpo tp},
    expr_constr tpo -> expr_constr tp
                                                       (**r unary operation *)
  | ECbinop : forall tp op `{HyperTypeImpl tp, HyperTypeImpl tpl, HyperTypeImpl tpr,
                             HyperBinaryImpl op tpl tpr tp},
    expr_constr tpl -> expr_constr tpr -> expr_constr tp
  (**r binary operation *)
(* Note: this is never generated by Edsger. *)
(*  | ECcast : forall tp `{HyperTypeImpl tp, HyperTypeImpl tpo, HyperCastImpl tpo tp},
    expr_constr tpo -> expr_constr tp
                                                  (**r type cast ([(ty) e]) *) *)
(* Note: this is never generated by Edsger. *)
(*  | ECaddrof : forall {tp}`{HyperTypeImpl tp},
    lexpr_constr tp -> expr_constr tchar_pointer_globalpointer 
     (* Currently Edsger never generates ECaddrof, but it may be useful in future work. So expr and lexpr are mutually recursive. *) *)
(* Note: this is never generated by Edsger. *)
(*  | ECrespec : forall {tp}`{HyperTypeImpl tp} tmp',
    expr_constr tint_Z32 ->
    forall spec : spec_env_t tmp' -> unpair_ft tp,
    expr_constr tp *)

  (** There are only three acceptable L-value expressions and they are
      represented by the three constructors of [lexpr_constr].  [LCvar]
      and [LCderef] are exactly lixe [Evar] and [Ederef] (note that
      [LCderef] takes [expr_constr] as an argument, not [lexpr_constr]:
      [lexpr_constr] is not recursive).

      [LCpfield] is equivalent to [Efield (Ederef e t) x t'], which is
      inlined in [ECaddrof_pfield].
  *)
  with lexpr_constr : forall tp `{hti : HyperTypeImpl tp}, Type :=
  | LCvar : forall`{HyperTypeImpl tp}(v : variable tp), lexpr_constr tp
                                                              (**r variable *)
  | LCfield : forall tp id `{HyperTypeImpl tp, HyperTypeImpl tpl, !HyperFieldImpl tpl tp id},
    lexpr_constr tpl -> lexpr_constr tp
                               (**r access to a member of a struct or union *)
  | LCindex : forall tp `{HyperTypeImpl tp, HyperTypeImpl tpl, !HyperIndexImpl tpl tp},
      lexpr_constr tpl -> expr_constr tint_Z32 -> lexpr_constr tp
                               (**r access to an element of an array. *)
  | LChash : forall tp `{HyperTypeImpl tp, HyperTypeImpl tpl, !HyperIntHashImpl tpl tp},
      lexpr_constr tpl -> expr_constr tint_U -> lexpr_constr tp
                               (**r access to an element of a hashtable. *)
  .

  Fixpoint lexpr_is_ghost `{HyperTypeImpl}(e : lexpr_constr tp) :=
    match e with
    | LCvar _ _ v => v.(ltype_ghost)
    | LCfield _ tid _ _ _ pfieldi e' => lexpr_is_ghost e'
    | LCindex _ _ _ _ _ e_arr e_idx => lexpr_is_ghost e_arr
    | LChash _ _ _ _ _ e_arr e_idx => lexpr_is_ghost e_arr
    end.

  (* This inductive type contains the verification conditions needed for the corresponding
     expression type. Each DeepSpec expression will generate one expr_constr (in LayerFoo.v)
     and a tactic will generate an expr_constr_prf (in ObjBarCode.v), where Foo is the layer
     containing the object Bar.

     The philosophy is that verification conditions that can be discharged fully automatically
     are put into the _prf type, while those that need manual work are put into the _cond .
  *)
  (* TODO fill in other constructors *)
  Inductive expr_constr_prf : forall `{HyperType tp}, expr_constr tp -> Prop :=
  (* | ECPconst_int : forall`{HyperType tp} f i (fc : ht_ft_cond f)
    (rel : ht_cval f = CVval (Vint i)), expr_constr_prf (ECconst_int _ f i) *)
  | ECPconst_int256 : forall`{HyperType tp} f i (fc : ht_ft_cond f)
    (rel : ht_cval f = CVval (Vint i)), expr_constr_prf (ECconst_int256 _ f i)
  | ECPtempvar : forall`{ht : HyperType tp} t, expr_constr_prf (ECtempvar tp t)

  | ECPbuiltin0 : forall `{HyperBuiltin0 (LayerSpec := LayerSpec) tp, !HyperType tp}, expr_constr_prf (ECbuiltin0 tp)
  | ECPbuiltin1 : forall atp `{HyperBuiltin1 (LayerSpec := LayerSpec) atp tp, !HyperType atp, !HyperType tp} e,
    expr_constr_prf e ->
    expr_constr_prf (ECbuiltin1 atp tp e)

  | ECPunop : forall op `{HyperUnaryOp op tpo tp, ht : !HyperType tp,
                          hto : !HyperType tpo} e,
    expr_constr_prf e -> expr_constr_prf (ECunop _ op e)
  | ECPbinop : forall op `{HyperBinaryOp op tpl tpr tp, ht : !HyperType tp,
                           htl : !HyperType tpl, htr : !HyperType tpr} el er,
    expr_constr_prf el -> expr_constr_prf er ->
    expr_constr_prf (ECbinop _ op el er)

  with lexpr_constr_prf : forall`{HyperType tp}, lexpr_constr tp -> Prop :=
  | LCPvar : forall`{HyperType tp} var (v_prf : variable_prf var),
    lexpr_constr_prf (LCvar var)
  | LCPfield : forall id `{HyperField tpl tp id, !HyperType tpl, !HyperType tp}
                      e (ec : lexpr_constr_prf e),
    lexpr_constr_prf (LCfield tp id e)
  | LCPindex : forall`{HyperIndex tpl tp, !HyperType tp, !HyperType tpl} e idx,
    lexpr_constr_prf e -> expr_constr_prf idx ->
    lexpr_constr_prf (LCindex tp e idx)
  | LCPhash : forall`{HyperIntHash tpl tp, !HyperType tp, !HyperType tpl} e idx,
    lexpr_constr_prf e -> expr_constr_prf idx ->
    lexpr_constr_prf (LChash tp e idx)
  .

  (* Each DeepSpec thing also defines a corresponding _passthrough_prf.

    The _prf is used for proving correctness when a primitive is first added to a layer, and
    the _passthrough_prf is used when proving correctness of higher layers when the primitive is
    passed through.
  *)

  Inductive expr_constr_passthrough_prf :
      forall`{HyperTypeImpl tp}, expr_constr tp -> Prop :=
  | ECPPconst_int : forall`{hti : HyperTypeImpl tp} f i,
    expr_constr_passthrough_prf (ECconst_int tp f i)
  | ECPPtempvar : forall`{hti : HyperTypeImpl tp} t,
    expr_constr_passthrough_prf (ECtempvar tp t)
  | ECPPunop : forall tp op `{htu : HyperUnaryPassthrough op tpo tp} e,
    expr_constr_passthrough_prf e -> expr_constr_passthrough_prf (ECunop tp op e)
  | ECPPbinop : forall tp op `{htb : HyperBinaryPassthrough op tpl tpr tp} el er,
    expr_constr_passthrough_prf el -> expr_constr_passthrough_prf er ->
    expr_constr_passthrough_prf (ECbinop tp op el er)

  with lexpr_constr_passthrough_prf :
      forall`{HyperTypeImpl tp}, lexpr_constr tp -> Prop :=
  | LCPPvar : forall`{hti : HyperTypeImpl tp} var (v_prf : variable_passthrough_prf var),
    lexpr_constr_passthrough_prf (LCvar var)
  | LCPPfield : forall id `{htf : HyperFieldPassthrough tpl tp id} e,
    lexpr_constr_passthrough_prf e ->
    lexpr_constr_passthrough_prf (LCfield tp id e)
  | LCPPindex : forall `{htd : HyperIndexPassthrough tpl tp} e idx,
    lexpr_constr_passthrough_prf e -> expr_constr_passthrough_prf idx ->
    lexpr_constr_passthrough_prf (LCindex tp e idx)
  .
End EXPR_CONSTR.

Section STMT_CONSTR.
  (** * Commands *)
  (** Again, Clight statement is a clone of Clight statement but wildly
      modified.  Key differences include:
      - Each statement can either read, write, or do not access the memory,
        but never read _and_ write.
      - Memory accessing has been limited to one primitive data type at a time.
        (No whole structure or union assignments.)
      - The statements are indexed by wheather they will [return] or not.
        This is a must analysis, not a may analysis.
      - Single assignments on all temporaries.  (Future versions may have
        special forms of assignments for loops and branching.)

      In the fine-grain abstract layer model, function calls are restricted
      to calls to lower layers marked as external function calls.  Intra-layer
      function calls indicate the need to split a layer into multiple ones,
      and recursion is (semi-)forbidden for stack size concern.
  *)
  (* Context `{LayerSpec : LayerSpecClass}. *)
  Context `{HM : HyperMem}.

  Definition function_return_dec returns
    := match tp_ty returns with
       | Tvoid => false
       | _ => true
       end.

  Require Import DeepSpec.lib.Monad.StateMonadOption.
  Import MonadNotation.
  Open Scope monad_scope.
  Definition DS := osT GetHighData.
  Instance Monad_DS : Monad DS := MosT GetHighData.

  Instance MonadLaws_DS : MonadLaws (Monad_stateT GetHighData Monad_option).
  Proof.
    apply MonadLaws_MosT.
  Qed.

  Instance MonadState_DS : MonadState GetHighData DS.
  apply MonadState_stateT.
  apply Monad_option.
  Defined.

  Instance MonadZero_DS : MonadZero DS.
  apply MonadZero_stateT.
  apply Monad_option.
  apply Zero_option.
  Defined.


  (*  The underlay interface for a primitive is a Coq Prop, a relation from memory etc to the final value.
      But we want a Coq function. So the primitive record contains the function.

      Then the class primitive_exec_prf says that the primitive is equivalent to the specification in the
      underlay interface.
   *)
  Record primitive (argt : list hyper_type_pair)(ret : hyper_type_pair)
      : Type := mk_prim {
    PRIMident : ident;
    (* Todo: maybe we need this for the C backend. If so, need to parameterize it as another opaque type in MiniC.v *)                
    (*PRIMcc    : calling_convention;   (* Either C or Assembly calling convention. This is not the same as the [[semantic]] in the source file! *)*)
    PRIMghost : bool;                 (* These track the source file. *)
    PRIMpure  : bool;                 (* This is called "const" in the source file. *)

    PRIMargt_marker : type_marker argt;
    PRIMret_marker : type_marker ret;


    (* PRIMsem_opt, the "monadic" version, combines PRIMcond and PRIMsem into one thing.
       There is a verification condition below saying that they must be equivalent;
       the synthesis use them interchangably for convenience. *)
    PRIMcond : HList tp_ft argt -> machine_env GetHighData -> GetHighData -> Prop;

    PRIMsem_opt : HList tp_ft argt -> machine_env GetHighData -> DS (tp_ft ret)
(*    PRIMsem : HList tp_ft argt -> GetHighData ->
              GetHighData * (* trace * *) tp_ft ret;
    PRIMsem_opt : HList tp_ft argt -> GetHighData ->
              ContOpt (GetHighData * (* trace * *) tp_ft ret) *)
  }.
  (* begin hide *)
  Global Arguments PRIMident {_ _} _.
  (* Global Arguments PRIMcc    {_ _} _. *)
  Global Arguments PRIMghost {_ _} _.
  Global Arguments PRIMpure  {_ _} _.
  Global Arguments PRIMcond {_ _} _ _ _.
  (* Global Arguments PRIMsem   {_ _} _ _ _. *)
  (* Global Arguments PRIMsem_opt {_ _} _ _ _ _ _. *)
  Global Arguments PRIMsem_opt {_ _} _ _.
  (* end hide *)

  Fixpoint to_typelist argt :=
    match argt with
    | nil => Ctypes.Tnil
    | cons x xs => Ctypes.Tcons (tp_ty x) (to_typelist xs)
    end.

  (* This class has verification conditions for all primitives (ghost and non-ghost). *)
  Class primitive_prf {argt returns}(prim : primitive argt returns) := mk_prim_prf {
    PRIMret_cond :
      forall args me,
        ht_list_ft_cond args ->
        ht_list_valid_ft_cond args ->
      forall (s : GetLowData) v,
        evalStateT (prim.(PRIMsem_opt) args me) s = ret v ->
        high_level_invariant (CompatDataOps := cdataOpsLow) s ->
        (* validStateLow d -> *)
        (* (exists j m, match_AbData d m j) -> *)
        ht_ft_cond v /\
        (function_return_dec returns = true -> ht_valid_ft_cond v);
    PRIMis_pure :
      prim.(PRIMpure) = true ->
      forall args me d,
      forall d', execStateT (prim.(PRIMsem_opt) args me) d = ret d' ->
                 d = d';

    PRIMinv :
      prim.(PRIMpure) = false ->
      forall args me,
        ht_list_ft_cond args ->
        ht_list_valid_ft_cond args ->
      forall (s s' : GetLowData),
        execStateT (prim.(PRIMsem_opt) args me) s = ret s' ->
        high_level_invariant (CompatDataOps := cdataOpsLow) s ->
        high_level_invariant (CompatDataOps := cdataOpsLow) s'
   }.

  (* This class contains verification conditions specific to non-ghost primitives. *)
  Class primitive_exec_prf {argt ret}(prim : primitive argt ret) := mk_prim_exec_prf {
  }.

  Class primitive_passthrough_prf {argt ret}(prim : primitive argt ret)
    := mk_prim_pt_prf {
  }.

  Definition expr_constr_list :=
    HList (fun htp => expr_constr (tp_type_pair htp)).
  Definition expr_constr_prf_conj {ls}(es : expr_constr_list ls) :=
    HList_fold_right_nodep (fun htp e p =>
        p /\ { ht : HyperType (tp_type_pair htp) | expr_constr_prf (H := ht) e })
      True es.

  Definition expr_constr_passthrough_prf_conj {ls}(es : expr_constr_list ls) :=
    HList_fold_right_nodep (fun htp e p => p /\ expr_constr_passthrough_prf e)
      True es.

  Inductive cmd_constr : forall (returns : hyper_type_pair), Type :=
  | CCskip : cmd_constr void_unit_pair                      (**r do nothing *)
  | CClet : forall {r}`{HyperTypeImpl tp}(id : ident),
    cmd_constr (mk_hyper_type_pair tp) -> cmd_constr r -> cmd_constr r
                                            (**r binding [let x = s1 in s2] *)
  | CCload : forall`{HyperTypeImpl tp},
    lexpr_constr tp -> cmd_constr (mk_hyper_type_pair tp)
                                                           (**r memory read *)
  | CCstore : forall`{HyperTypeImpl tp},
    lexpr_constr tp -> expr_constr tp -> cmd_constr void_unit_pair
                                        (**r memory write [lvalue = rvalue] *)
  | CCsequence : forall {r},
    cmd_constr void_unit_pair -> cmd_constr r -> cmd_constr r (**r sequence *)
  | CCifthenelse : forall {r},
    expr_constr tint_bool -> cmd_constr r -> cmd_constr r -> cmd_constr r
                                                           (**r conditional *)
  | CCfor : forall id_it id_end : ident,
    expr_constr tint_Z32 -> expr_constr tint_Z32 -> cmd_constr void_unit_pair ->
    cmd_constr void_unit_pair                             (**r bounded loop *)
  | CCfirst : forall {r}(id_it id_end id_dest : ident),
    expr_constr tint_Z32 -> expr_constr tint_Z32 ->
    cmd_constr int_bool_pair ->
    cmd_constr r -> cmd_constr r -> cmd_constr r         (**r linear search *)
  | CCfold : forall `{HyperTypeImpl tp}(id_it id_end id_recur id_dest : ident),
    expr_constr tint_Z32 -> expr_constr tint_Z32 -> expr_constr tp ->
    cmd_constr (mk_hyper_type_pair tp) ->
    cmd_constr (mk_hyper_type_pair tp)                     (**r linear scan *)
  | CCcall : forall {argt ret},
    primitive argt ret -> expr_constr_list argt -> cmd_constr ret
                                              (**r (internal object) function call *)
  (* | CCcall_ext : forall {argt ret},
    int256 -> (* the address of the external object/contract to call *)
    primitive argt ret -> expr_constr_list argt -> cmd_constr ret
                                              (**r (external contract) function call *) *)
  | CCyield : forall`{HyperTypeImpl tp},
    expr_constr tp -> cmd_constr (mk_hyper_type_pair tp)
                            (**r yield or return a value depends on context *)

  (**r Belows are DeepSpec specific *)
  | CCconstr : forall`{HyperTypeImpl tp}(fld_ids : list ident) fld_tps,
    lexpr_constr tp -> expr_constr_list fld_tps ->
    list_curried_type fld_tps (unpair_ft tp) ->
    cmd_constr void_unit_pair
  | CCassert : cmd_constr int_bool_pair -> cmd_constr void_unit_pair
                                          (**r specification only assertion *)
  | CCdeny : cmd_constr int_bool_pair -> cmd_constr void_unit_pair
                                          (**r specification only assertion *)
  (* | CCghost : cmd_constr void_unit_pair -> cmd_constr void_unit_pair *)
                                            (**r specification only command *)
  | CCpanic : forall tp`{HyperTypeImpl tp},
    cmd_constr (mk_hyper_type_pair tp)
                         (**r specification only failure (w/ a dummy value) *)
  (* There are two versions of of respec, which take a monadic or a non-monadic functional specification.
     The difference is that CCrespec only can encode pure specifications (it doesn't return a new memory).
     So if you want a non-pure specification you have to use CCrespec_opt,
      and if you want to use a respec in a loop test you have to use CCrespec.
  *)
  | CCrespec : forall {r} tmp', cmd_constr r ->
    forall spec : spec_env_t tmp' -> DS (tp_ft r),
    cmd_constr r                         (**r manually specify the behavior *)
  | CCrespec_opt : forall {r} tmp', cmd_constr r ->
    forall spec :  machine_env GetHighData -> spec_env_t tmp' -> DS (tp_ft r), (* GetHighData -> *)
      (* ContOpt (GetHighData * (* trace * *) tp_ft r), *)
    cmd_constr r                         (**r manually specify the behavior *)
  .

  Inductive cmd_constr_passthrough_prf : forall ret, cmd_constr ret -> Prop :=
  | CCPPskip : cmd_constr_passthrough_prf _ CCskip
  | CCPPlet : forall {r}`{hti : HyperTypeImpl tp}(id : ident) c1 c2,
    cmd_constr_passthrough_prf (mk_hyper_type_pair tp) c1 ->
    cmd_constr_passthrough_prf r c2 ->
    cmd_constr_passthrough_prf r (CClet id c1 c2)
  | CCPPload : forall`{hti : HyperTypeImpl tp} e,
    lexpr_constr_passthrough_prf e ->
    cmd_constr_passthrough_prf (mk_hyper_type_pair tp) (CCload e)
  | CCPPstore : forall`{hti : HyperTypeImpl tp} el er,
    lexpr_constr_passthrough_prf el -> expr_constr_passthrough_prf er ->
    cmd_constr_passthrough_prf _ (CCstore el er)
  | CCPPsequence : forall {r} c1 c2,
    cmd_constr_passthrough_prf _ c1 -> cmd_constr_passthrough_prf r c2 ->
    cmd_constr_passthrough_prf r (CCsequence c1 c2)
  | CCPPifthenelse : forall {r} e c1 c2,
    expr_constr_passthrough_prf e ->
    cmd_constr_passthrough_prf r c1 -> cmd_constr_passthrough_prf r c2 ->
    cmd_constr_passthrough_prf r (CCifthenelse e c1 c2)
  | CCPPfor : forall id_it id_end e1 e2 c,
    expr_constr_passthrough_prf e1 -> expr_constr_passthrough_prf e2 ->
    cmd_constr_passthrough_prf _ c ->
    cmd_constr_passthrough_prf _ (CCfor id_it id_end e1 e2 c)
  | CCPPfirst : forall {r} id_it id_end id_dest e1 e2 c3 c4 c5,
    expr_constr_passthrough_prf e1 -> expr_constr_passthrough_prf e2 ->
    cmd_constr_passthrough_prf _ c3 ->
    cmd_constr_passthrough_prf r c4 -> cmd_constr_passthrough_prf r c5 ->
    cmd_constr_passthrough_prf r (CCfirst id_it id_end id_dest e1 e2 c3 c4 c5)
  | CCPPfold : forall `{hti : HyperTypeImpl tp} id_it id_end id_recur id_dest e1 e2 e3 c,
    expr_constr_passthrough_prf e1 -> expr_constr_passthrough_prf e2 ->
    expr_constr_passthrough_prf e3 ->
    cmd_constr_passthrough_prf (mk_hyper_type_pair tp) c ->
    cmd_constr_passthrough_prf (mk_hyper_type_pair tp)
                               (CCfold id_it id_end id_recur id_dest e1 e2 e3 c)
  | CCPPcall : forall {argt ret} prim arg,
    expr_constr_passthrough_prf_conj arg ->
    primitive_passthrough_prf prim ->
    cmd_constr_passthrough_prf _ (@CCcall argt ret prim arg)
  (* external call *)
  | CCPPcall_ext : forall {argt ret} prim arg,
    expr_constr_passthrough_prf_conj arg ->
    primitive_passthrough_prf prim ->
    cmd_constr_passthrough_prf _ (@CCcall argt ret prim arg)
  | CCPPyield : forall`{hti : HyperTypeImpl tp} e,
    expr_constr_passthrough_prf e ->
    cmd_constr_passthrough_prf (mk_hyper_type_pair tp) (CCyield e)
  | CCPPconstr : forall`{HyperTypeImpl tp} fld_ids fld_tps el flds constr
      `{hcp : !HyperConstructorPassthrough tp fld_tps constr},
    lexpr_constr_passthrough_prf el -> expr_constr_passthrough_prf_conj flds ->
    cmd_constr_passthrough_prf _ (CCconstr fld_ids fld_tps el flds constr)
  | CCPPassert : forall c,
    cmd_constr_passthrough_prf _ c -> cmd_constr_passthrough_prf _ (CCassert c)
  | CCPPdeny : forall c,
    cmd_constr_passthrough_prf _ c -> cmd_constr_passthrough_prf _ (CCdeny c)
(*  | CCPPghost : forall c,
    cmd_constr_passthrough_prf _ c -> cmd_constr_passthrough_prf _ (CCghost c) *)
  | CCPPpanic : forall tp`{HyperTypeImpl tp},
    cmd_constr_passthrough_prf (mk_hyper_type_pair tp) (CCpanic tp)
  | CCPPrespec : forall {r} tmp' c spec,
    cmd_constr_passthrough_prf r (CCrespec tmp' c spec)
  | CCPPrespec_opt: forall {r} tmp' c spec,
    cmd_constr_passthrough_prf r (CCrespec_opt tmp' c spec)
  .

  (* Local identifier allocation:

       [FC_ident_start]: first valid local ident -- reserved for return value;
       [FC_param_ident_start]: ident for the first function parameter (or
                               the first local variable if there is none).

     All [SClet]-bindings in [FC_body] should have idents being at least
     [FC_param_ident_start + length FC_params].
  *)
  Record function_constr : Type := mk_function_constr {
    FC_ident_start : ident;
    FC_param_ident_start := Pos.succ FC_ident_start;

    FC_params : list hyper_type_pair;
    FC_returns : hyper_type_pair;

    FC_body : cmd_constr FC_returns
  }.

  Definition function_constr_passthrough_prf f : Prop :=
    cmd_constr_passthrough_prf f.(FC_returns) f.(FC_body).

End STMT_CONSTR.

(** For primitive type markers *)
Coercion create_list_marker := fun A => @create_type_marker (list A).
Coercion create_hyper_type_pair_marker := @create_type_marker hyper_type_pair.

Section CONSTR_PRF.
  Context `{HM : HyperMem}.

  (* The cmd_constr_prf is parametrized by the is_realizing bit.
     If this is true, the _prf condition is stronger, since it also includes
     conditions needed to prove the synthesized code correct. *)
  Inductive cmd_constr_prf (is_realizing : bool) :
      forall ret (htr : HyperType (tp_type_pair ret)), cmd_constr ret -> Prop :=
  | CCPskip : cmd_constr_prf is_realizing _ _ CCskip
  | CCPlet : forall r `{ht : HyperType tp} htr id c1 c2,
    function_return_dec (mk_hyper_type_pair tp) = true ->
    cmd_constr_prf is_realizing (mk_hyper_type_pair tp) ht c1 ->
    cmd_constr_prf is_realizing r htr c2 ->
    cmd_constr_prf is_realizing r htr (CClet id c1 c2)
  | CCPload : forall `{ht : HyperType tp, hbv : !HyperByValueType tp} e
                (* {hltrc: HyperLTypeRetCond} *)
                     (ec : lexpr_constr_prf e),
    (* To be realizable, must not read from a ghost value:
         is_realizing => ~ is_ghost
       <=>
         ~ is_realizing \/ ~ is_ghost
       <=>
         ~ (is_realizing /\ is_ghost)
    *)
    (is_realizing && lexpr_is_ghost e = false) ->
    cmd_constr_prf is_realizing (mk_hyper_type_pair tp) ht (CCload e)
  | CCPstore : forall `{ht : HyperType tp, hbv : !HyperByValueType tp} el er
                      (ecl : lexpr_constr_prf el)(ecr : expr_constr_prf er),
    (* To skip realization, must act ghostly not touching real data:
         ~ is_realizing => is_ghost
       <=>
         ~ ~ is_realizing \/ is_ghost
    *)
    (is_realizing || lexpr_is_ghost el = true) ->
    cmd_constr_prf is_realizing _ _ (CCstore el er)
  | CCPsequence : forall r htr c1 c2,
    cmd_constr_prf is_realizing _ _ c1 ->
    cmd_constr_prf is_realizing r htr c2 ->
    cmd_constr_prf is_realizing r htr (CCsequence c1 c2)
  | CCPifthenelse : forall r htr e c_true c_false
                           (ec : expr_constr_prf e),
    cmd_constr_prf is_realizing r htr c_true ->
    cmd_constr_prf is_realizing r htr c_false ->
    cmd_constr_prf is_realizing r htr (CCifthenelse e c_true c_false)
  | CCPfor : forall id_it id_end e1 e2 c
                   (ec1 : expr_constr_prf e1)(ec2 : expr_constr_prf e2),
    cmd_constr_prf is_realizing _ _ c ->
    cmd_constr_prf is_realizing _ _ (CCfor id_it id_end e1 e2 c)
  | CCPfirst : forall r htr id_it id_end id_dest e1 e2 c3 c4 c5,
    expr_constr_prf e1 -> expr_constr_prf e2 ->
    cmd_constr_prf is_realizing int_bool_pair int_bool c3 ->
    cmd_constr_prf is_realizing r htr c4 ->
    cmd_constr_prf is_realizing r htr c5 ->
    cmd_constr_prf is_realizing r htr
                   (CCfirst id_it id_end id_dest e1 e2 c3 c4 c5)
  | CCPfold : forall `{ht : HyperType tp} id_it id_end id_recur id_dest
      e1 e2 e3 c (ec1 : expr_constr_prf e1)(ec2 : expr_constr_prf e2)
      (ec3 : expr_constr_prf e3),
    function_return_dec (mk_hyper_type_pair tp) = true ->
    cmd_constr_prf is_realizing (mk_hyper_type_pair tp) ht c ->
    cmd_constr_prf is_realizing (mk_hyper_type_pair tp) ht
                   (CCfold id_it id_end id_recur id_dest e1 e2 e3 c)
  | CCPcall : forall argt ret htr prim arg
      (IHprim : @primitive_prf _ _ argt ret prim)
      (IHprim_exec : primitive_exec_prf prim)
      (IHprim_b : primitive_passthrough_prf prim)
      (ecs : expr_constr_prf_conj arg),
    (* To be realizable, must not receive value from a ghost primitive call:
         is_realizing /\ ghost => ~ return
       <=>
         ~ (is_realizing /\ ghost) \/ ~ return
       <=>
         ~ (is_realizing /\ return /\ ghost)

       To skip realization, must act ghostly and only call ghost or pure
       primitives:
         ~ is_realizing => ghost \/ pure
       <=>
         ~ ~ is_realizing \/ (ghost \/ pure)
    *)
    (* (is_realizing && function_return_dec ret && prim.(PRIMghost) = false) -> *)
      (* (is_realizing || prim.(PRIMghost) || prim.(PRIMpure) = true) -> *)
      is_realizing || prim.(PRIMpure) = true ->
    cmd_constr_prf is_realizing ret htr (CCcall prim arg)
  (* | CCPcall_ext : forall argt ret htr addr prim arg
      (IHprim : @primitive_prf _ _ argt ret prim)
      (IHprim_exec : primitive_exec_prf prim)
      (IHprim_b : primitive_passthrough_prf prim)
      (ecs : expr_constr_prf_conj arg),
    (* To be realizable, must not receive value from a ghost primitive call:
         is_realizing /\ ghost => ~ return
       <=>
         ~ (is_realizing /\ ghost) \/ ~ return
       <=>
         ~ (is_realizing /\ return /\ ghost)

       To skip realization, must act ghostly and only call ghost or pure
       primitives:
         ~ is_realizing => ghost \/ pure
       <=>
         ~ ~ is_realizing \/ (ghost \/ pure)
    *)
    (* (is_realizing && function_return_dec ret && prim.(PRIMghost) = false) -> *)
      (* (is_realizing || prim.(PRIMghost) || prim.(PRIMpure) = true) -> *)
      is_realizing || prim.(PRIMpure) = true ->
    cmd_constr_prf is_realizing ret htr (CCcall_ext addr prim arg) *)
  | CCPyield : forall `{ht : HyperType tp} e (ec : expr_constr_prf e),
    cmd_constr_prf is_realizing (mk_hyper_type_pair tp) ht (CCyield e)
  | CCPconstr : forall `{ht : HyperType tp} fld_ids fld_tps el flds constr
      `{hc : !HyperConstructor tp fld_ids fld_tps constr}
      (ecl : lexpr_constr_prf el)(fldc : expr_constr_prf_conj flds)
      (flds_byvalue : HList
        (fun htp : hyper_type_pair =>
           HyperByValueType (tp_type_pair htp)) fld_tps),
    (* See [CCPstore] *)
    (is_realizing || lexpr_is_ghost el = true) ->
    (length fld_ids <= Z.to_nat Int256.modulus)%nat ->
    cmd_constr_prf is_realizing _ _ (CCconstr fld_ids fld_tps el flds constr)

  (* These three constructors is where the is_realizing bit changes, their subexpressions are ghost. *)
  | CCPassert : forall c, cmd_constr_prf is_realizing _ _ c -> cmd_constr_prf is_realizing _ _ (CCassert c)
  | CCPdeny : forall c, cmd_constr_prf is_realizing _ _ c -> cmd_constr_prf is_realizing _ _ (CCdeny c)
(*  | CCPghost : forall c, cmd_constr_prf false _ _ c -> cmd_constr_prf is_realizing _ _ (CCghost c) *)
  | CCPpanic : forall `{ht : HyperType} (*f*), cmd_constr_prf is_realizing _ _ (CCpanic tp (*f*))
  | CCPrespec : forall r htr tmp' c spec,
    (* Not sure if necessary
    function_return_dec r = true ->
    *)
    cmd_constr_prf is_realizing r htr c ->
    cmd_constr_prf is_realizing r htr (CCrespec tmp' c spec)
  | CCPrespec_opt : forall r htr tmp' c spec,
    (* Do not know if [spec] acts on the abstract state stealthily, restrict
       it to be realizing. *)
    is_realizing = true ->
    cmd_constr_prf is_realizing r htr c ->
    cmd_constr_prf is_realizing r htr (CCrespec_opt tmp' c spec)
  .

  Record function_constr_prf (is_realizing : bool) (f : function_constr) : Prop := mk_function_constr_prf {
    (* FC_returns_cast : if function_return_dec f.(FC_returns)
                        then HyperArgRet (tp_type_pair f.(FC_returns))
                        else True; *)

    FC_returns_ht : HyperType (tp_type_pair f.(FC_returns));
    FC_body_prf : cmd_constr_prf is_realizing f.(FC_returns) FC_returns_ht f.(FC_body)
  }.
  (* begin hide *)
  (* Global Arguments FC_returns_cast _ {_} _.
  Global Arguments FC_returns_ht   _ {_} _.
  Global Arguments FC_body_prf     _ {_} _. *)
  (* end hide *)
End CONSTR_PRF.
