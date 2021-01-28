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

(** Synthesizing Clight statements, functional statements, and
    specifications from DeepSpec along with the proofs.  *)
(* Standard library modules *)
Require Import BinIntDef.
Require Import BinPosDef.
Require Import Bool.

(* CompCert modules *)
Require Import backend.AST.
(*Require Import compcert.common.Events.
Require Import compcert.common.Globalenvs.  (* Genv *) *)
Require Import backend.MemoryModel.
Require Import backend.Values.HighValues.
Require Import cclib.Coqlib.
Require Import cclib.Integers.
Require Import cclib.Maps.  (* PTree *)
Require Import backend.Expressions.ExpMiniC.
Require Import backend.Statements.StmtMiniC.
(* Require Import compcert.cfrontend.ClightBigstep. *)
Require Import backend.Cop.  (* bool_val *)
Require Import backend.Ctypes.
Require Import backend.MachineModel.

(* DeepSpec modules *)
Require Import DeepSpec.lib.OProp.
(* Require Import DeepSpec.lib.Monad.ContOptionMonad. *)
Require Import DeepSpec.core.Cval.
Require Import DeepSpec.core.SEnv.
Require Import DeepSpec.core.HyperType.
Require Import DeepSpec.core.HyperTypeInst.
(*Require Import DeepSpec.core.HyperMem.*)
Require Import DeepSpec.core.MemoryModel.
Require Import DeepSpec.core.Syntax.
Require Import DeepSpec.core.SynthesisExpr.
Require        DeepSpec.lib.LangDef.

Require Import DeepSpec.lib.SimpleMaps.
Require Import DeepSpec.lib.SimpleIndexedMaps.
Require Import backend.phase.MiniC.Semantics.
Require Import backend.phase.MiniC.BigstepSemantics.
(* Require Import backend.Events. *)

Local Open Scope Z.
Opaque Monad.bind Monad.ret (* OptionMonad.guard *) MonadState.get MonadState.put MonadState.gets MonadState.modify StateMonad.runStateT StateMonad.execStateT StateMonad.evalStateT MonadZero.mzero.

Section STMT_FUNC.
  Context`{HM : HyperMem}.
  Context (me : machine_env GetHighData).

  (*Variable m0 : MemLow.*)

  (* This is the type of what the desugaring returns: new abstract data, and a return value. *)
  Record stmt_spec_record returns : Type
      := mk_stmt_spec_record{
    ss_mem : GetHighData;
    (*ss_trace : trace;*)
    ss_return : tp_ft returns   (* If the DeepSpec function does not return a value, the specification returns unit. *)
  }.
  (* begin hide *)
  Global Arguments mk_stmt_spec_record _ _ (*_*) _.
  Global Arguments ss_mem    {_} _.
  (* Global Arguments ss_trace  {_} _. *)
  Global Arguments ss_return {_} _.
  (* end hide *)

  Lemma stmt_spec_record_surjective {returns} r :
      r = mk_stmt_spec_record returns r.(ss_mem) (*r.(ss_trace)*) r.(ss_return).
  Proof.
    destruct r; reflexivity.
  Qed.

  (* This is the output of the correctness theorem. *)
  Record stmt_output_equivalent {tmp returns}
      (j : meminj)     (* memory injection, we thread this through because we use it for mem_match. *)
      dest  (* identifier for the temporary variable used to store the return value of the statement. *)
      (ml : MemLow)    (* low memory after the end of statement execution.. *)
      (se : spec_env_t tmp)   (* This is the specification environment (immutable, so same both before and after statement execution) *)
      le                      (* Temporary environment after the execution. *)
      (r : stmt_spec_record returns) : Prop := mk_stmt_output_equivalent{
    so_mem : mem_match j r.(ss_mem) ml;   (* The resulting low memory matches the resulting abstract data. *)
    so_se  : lenv_cond se le;             (* The values in le all refine the values in the se. *)
    so_return : function_return_dec returns = true -> ht_rel_some r.(ss_return) (le ! dest);            (* If the function returns a value, then it is stored in dest. *)
    so_ft_cond : ht_ft_cond r.(ss_return)
  }.
  (* begin hide *)
  Global Arguments so_mem     {_ _ _ _ _ _ _ _} _.
  Global Arguments so_se      {_ _ _ _ _ _ _ _} _ _ _ _ _.
  Global Arguments so_return  {_ _ _ _ _ _ _ _} _ _.
  Global Arguments so_ft_cond {_ _ _ _ _ _ _ _} _.
  (* end hide *)


  (* Auxillary functions about isomorphisms. These are used for looking things up in the Indexed Trees. *)
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

  (* These are auxilliary definitions used for desugaring loops. (I.e. the desugared function will contain calls to these.) *)
  Definition for_start m (start_idx : Z) :=
    (@mk_stmt_spec_record void_unit_pair m (*E0*) tt, start_idx).
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
        (*ss_trace := r.(ss_trace) ** r'.(ss_trace);*)
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
  Proof.
    unfold dest_not_exist.
    intros dest_not_exist_set.
    destruct (Pos.eq_dec dest id) as [ dest_eq | dest_ne ].
    - exfalso.
      rewrite dest_eq in dest_not_exist_set.
      rewrite AList.gss in dest_not_exist_set.
      set (is_some (o : option hyper_type_pair) :=
             match o with Some _ => true | _ => false end).
      apply (isomorphism_decidable_neq_absurd is_some dest_not_exist_set);
        reflexivity.
    - rewrite AList.gso in dest_not_exist_set; assumption.
  Qed.

  Definition yield_expr dest e :=
    Sset dest e.

  (* This is used as hypothesis for some of the later theorems.
     Compare with figure 4.9 "Command synthesis" in the thesis:
     some elaboration rule has a precondition that e.g. the test
     expression in a loop has to be pure. If it's not, we can't
     synthesize, or at least not prove any theorems about the code.  *)
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
      (* for loops are assumed to always have side effects. (otherwise, why use the loop?) *)
    | CCfirst _ id_it id_end id_dest e1 e2 c3 c4 c5 =>
      synth_stmt_pure c3 && synth_stmt_pure c4 && synth_stmt_pure c5
    | CCfold tp _ id_it id_end id_recur id_dest e1 e2 e3 c4 => true
    | CCcall argt ret prim args => prim.(PRIMpure)
    (* | CCcall_ext argt ret addr prim args => prim.(PRIMpure) *)
    | CCyield tp _ e => true
    | CCconstr _ _ _ _ _ _ _ => false
    | CCassert c => synth_stmt_pure c
    (* | CCghost c => synth_stmt_pure c *)
    | CCdeny c => synth_stmt_pure c
    | CCpanic _ _ => true
    | CCrespec _ _ c _ => true
    | CCrespec_opt _ _ c _ => false
    end.


  (* More typechecking judgements. This requires
       -again, that temporaries are welltyped, see e.g. the case for CClet.
       -and the pure conditions, see e.g. CCfold.
     *)
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
       ((tmp'' ! id_recur ~~~ None) * ((tmp''' ! id_dest ~~~ None) *                (* we use ~~~ because it has to be computable, we will use it as argument to the tree get function. *)
       (dest_not_exist dest tmp''' * (synth_stmt_pure c4 ~~~ true)))))) *           (* we could probably use = instead of ~~~ in this one case. *)
      (synth_expr_wellformed tmp e1 * (synth_expr_wellformed tmp e2 *
       (synth_expr_wellformed tmp e3 * synth_stmt_wellformed c4 id_dest tmp''')))
    | CCcall argt ret prim args => fold_synth_expr_wellformed tmp args
    (* | CCcall_ext argt ret addr prim args => fold_synth_expr_wellformed tmp args *)
    | CCconstr _ _ _ _ el flds _ =>
      synth_lexpr_wellformed tmp el * fold_synth_expr_wellformed tmp flds

    | CCyield tp _ e => synth_expr_wellformed tmp e
    | CCassert c => (synth_stmt_pure c ~~~ true) * synth_stmt_wellformed c dest tmp
    | CCdeny c   => (synth_stmt_pure c ~~~ true) * synth_stmt_wellformed c dest tmp
    (* | CCghost c => synth_stmt_wellformed c dest tmp *)
    | CCpanic _ _ => True
    | CCrespec _ tmp' c spec =>
      ((tmp ~~~ tmp') * (synth_stmt_pure c ~~~ true)) *
      synth_stmt_wellformed c dest tmp
    | CCrespec_opt _ tmp' c spec => (tmp ~~~ tmp') * synth_stmt_wellformed c dest tmp
    end%type%alist.

  (* Collect all the local variables the synthesized C statment will contain.
     This is used in SynthesisFunction, we need to give a list of all the
     locals of the function body.  *)
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
    (* | CCcall_ext argt ret addr prim args => nil *)
    | CCconstr _ _ _ _ _ _ _ => nil
    | CCyield tp _ e => nil
    | CCassert _ => nil
    | CCdeny _ => nil
    (* | CCghost _ => nil *)
    | CCpanic _ _ => nil
    | CCrespec _ tmp' c cpec => synth_stmt_locals c dest tmp
    | CCrespec_opt _ tmp' c spec => synth_stmt_locals c dest tmp
    end.

  Fixpoint synth_CCconstr_stmt tmp target fld_ids {fld_tps}
      (flds : expr_constr_list fld_tps){struct flds} : statement :=
    match flds with
    | HNil => Sskip
    | HCons htp _ fld_e res_es => match fld_ids with
      | nil => Sskip  (* Impossible, but let's not bother *)
      | fld_id :: res_ids =>
        Ssequence
          (Sassign (Efield target fld_id (tp_ty htp))
                   (synth_expr_expr tmp fld_e))
          (synth_CCconstr_stmt tmp target res_ids res_es)
      end
    end.

  (* This builds the Clight statement for a statement (see Figure 4.9). *)
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
      let prim_type := Tfunction arg_ty (tp_ty ret) (*prim.(PRIMcc)*) in
      let synth_expr_htp htp := synth_expr_expr tmp (tp := tp_type_pair htp) in
      let return_tmp := if function_return_dec ret then
                          Some dest
                        else
                          None in
      Scall return_tmp prim.(PRIMident) 
                       (HList_map_nodep synth_expr_htp args)
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
    (* | CCghost _ => Sskip *)
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

(* The induction principle in the Coq standard library is much more awkward. *)
Lemma Z_nonneg_peano_ind (P : Z -> Prop) :
  P 0%Z ->
  (forall z, 0 <= z -> P z -> P (Z.succ z)) ->
  (forall p, P (Z.neg p)) ->
  forall z, P z.
Proof.
  intros P0 Pind Pneg z.
  destruct z.
  - assumption.
  - apply (Pos.peano_ind (fun p => P (Z.pos p))).
    + change 1 with (Z.succ 0).
      apply Pind.
      * apply Z.le_refl.
      * assumption.
    + intros p' Pposp'.
      rewrite Pos2Z.inj_succ.
      apply Pind.
      * apply Pos2Z.is_nonneg.
      * assumption.
  - apply Pneg.
Qed.

Lemma Z_pos_peano_ind (P : Z -> Prop) :
  (forall z, z <= 0 -> P z) ->
  (forall z, 0 <= z -> P z -> P (Z.succ z)) ->
  forall z, P z.
Proof.
  intros Pnonpos Pind z.
  destruct z.
  - apply Pnonpos. omega.
  - apply (Pos.peano_ind (fun p => P (Z.pos p))).
    + change 1 with (Z.succ 0).
      apply Pind; [|apply Pnonpos]; omega.
    + intros p' Pposp'.
      rewrite Pos2Z.inj_succ.
      apply Pind; [apply Pos2Z.is_nonneg | assumption].
  - apply Pnonpos. apply Pos2Z.neg_is_nonpos.
Qed.

(** Why we define a new iterator: Ziteri instead of Z.iter?

    Otherwise, when executing loop with runStateT, it will look like:
    runStateT (fst (Z.iter ...(init_monad, start))) m.
    We need to carry a number (ranges from start to bound) during iteration,
    it can only be stored with the actual monad and form a pair.
    This form is hard for applying MonadLaws.
    So we let Ziteri itself to provide iteration value.
*)

Fixpoint Niteri {A:Type} (f:Z->A->A) (n:nat) (x:A) {struct n} : A :=
  match n with
  | O => x
  | S n' => f (Z.of_nat n') (Niteri f n' x)
  end.

Lemma Niteri_swap:
  forall (n:nat) (A:Type) (f:Z->A->A) (x:A),
    Niteri (fun z => f (Z.succ z)) n (f 0 x) = f (Z.of_nat n) (Niteri f n x).
Proof.
  induction n; intros.
  - tauto.
  - simpl.
    rewrite IHn.
    rewrite Zpos_P_of_succ_nat. reflexivity.
Qed.

Lemma Niteri_succ:
  forall n (A:Type) (f:Z->A->A) (x:A),
    Niteri f (S n) x = f (Z.of_nat n) (Niteri f n x).
Proof.
  reflexivity.
Qed.
  
Lemma Niteri_add:
  forall p q (A:Type) (f:Z->A->A) (x:A),
    Niteri f (p+q) x = Niteri (fun z => f (z+(Z.of_nat q))) p (Niteri f q x).
Proof.
  induction p; intros.
  - reflexivity.
  - (*now rewrite plus_Sn_m, !Niteri_succ, IHp. *)
    rewrite plus_Sn_m. rewrite !Niteri_succ. rewrite IHp.
    replace (Z.of_nat (p+q)) with (Z.of_nat p + Z.of_nat q).
    reflexivity.
    rewrite <- Nat2Z.inj_add. reflexivity.
Qed.

Lemma Niteri_add1:
  forall p (A:Type) (f:Z->A->A) (x:A),
    Niteri f (S p) x = Niteri (fun z => f (Z.succ z)) p (Niteri f 1 x).
Proof.
  intros.
  replace (S p) with (p + 1)%nat by (rewrite Plus.plus_comm; reflexivity).
  rewrite Niteri_add. reflexivity.
Qed.

Definition Ziteri {A} (f:Z->A->A) (steps:Z) (x:A) :=
    Niteri f (Z.to_nat steps) x.
(* TODO: change all _ > _ or _ >= _ into _ < _ and _ <= _ *)
Lemma Ziteri_base':
  forall z (A:Type) (f:Z->A->A) (x:A),
    z < 0 ->
    Ziteri f z x = x.
Proof.
  intros z A f x lt0.
  destruct z.
  - omega.
  - inversion lt0.
  - unfold Ziteri. rewrite Z2Nat.inj_neg. reflexivity.
Qed.

Lemma Ziteri_base:
  forall z (A:Type) (f:Z->A->A) (x:A),
    z <= 0 ->
    Ziteri f z x = x.
Proof.
  intros z A f x le0.
  destruct z.
  - reflexivity.
  - destruct le0. reflexivity.
  - unfold Ziteri. rewrite Z2Nat.inj_neg. reflexivity.
Qed.

Lemma Ziteri_neg:
  forall p (A:Type) (f:Z->A->A) (x:A),
    Ziteri f (Zneg p) x = x.
Proof.
  intros.
  apply Ziteri_base.
  specialize (Zlt_neg_0 p).
  omega.
Qed.

Lemma Ziteri_0:
  forall (A:Type) (f:Z->A->A) (x:A),
    Ziteri f 0 x = x.
Proof.
  intros.
  apply Ziteri_base. omega.
Qed.

Lemma Ziteri_swap:
  forall z (A:Type) (f:Z->A->A) (x:A),
    0 <= z ->
    Ziteri (fun z => f (Z.succ z)) z (f 0 x) = f z (Ziteri f z x).
Proof.
  intro z.
  intros. unfold Ziteri.
  rewrite (Niteri_swap (Z.to_nat z)).
  rewrite Z2Nat.id. reflexivity.
  assumption.
Qed.

Lemma Ziteri_succ:
  forall z (A:Type) (f:Z->A->A) (x:A),
    0 <= z ->
    Ziteri f (Z.succ z) x = f z (Ziteri f z x).
Proof.
  intros z A f x z_le0.
  unfold Ziteri.
  rewrite Z2Nat.inj_succ by assumption.
  rewrite <- (Z2Nat.id z) at 2 by assumption.
  apply Niteri_succ.
Qed.

Lemma Ziteri_add1:
  forall p (A:Type) (f:Z->A->A) (x:A),
    0 <= p ->
    Ziteri f (Z.succ p) x = Ziteri (fun z => f (Z.succ z)) p (Ziteri f 1 x).
Proof.
  intros.
  unfold Ziteri.
  rewrite Z2Nat.inj_succ by assumption.
  apply Niteri_add1.
Qed.


Definition failure_passing {A} (h: DS A -> DS A) : Prop :=
  forall m x,
  runStateT x m = mzero ->
  runStateT (h x) m = mzero.

Lemma Niteri_from_none {A} (f: Z -> DS A -> DS A) (fp: forall n, failure_passing (f n)) n:
  forall x m,
  runStateT x m = mzero ->
  runStateT (Niteri f n x) m = mzero.
Proof.
  induction n; intros x m opt_x.
  - (* O *)
    simpl. exact opt_x.
  - (* S *)
    simpl.
    apply fp.
    apply IHn. exact opt_x.
Qed.

Lemma Niteri_cut_0 {A} {f: Z -> DS A -> DS A} (fp: forall n, failure_passing (f n)) {n x m v s}:
  runStateT (Niteri f n x) m = ret (v, s) ->
  exists v' s', runStateT x m = ret (v', s').
Proof.
  intros opt_n.
  destruct (runStateT x m) eqn:opt_x.
  - (* run x m = Some p *)
    destruct p as [v' s']. exists v', s'. reflexivity.
  - (* run x m = None *)
    apply (Niteri_from_none f fp n) in opt_x.
    rewrite opt_x in opt_n.
    discriminate.
Qed.

Lemma Niteri_Ziteri {A} (f: Z -> DS A -> DS A) {P}:
  forall x n,
  P (Niteri f (Z.to_nat n) x) ->
  P (Ziteri f n x).
Proof.
  intros x n Pn.
  unfold Ziteri.
  exact Pn.
Qed.

Lemma Ziteri_from_none {A} (f: Z -> DS A -> DS A) (fp: forall n, failure_passing (f n)) n:
  forall x m,
    runStateT x m = mzero ->
    runStateT (Ziteri f n x) m = mzero.
Proof.
  intros x m opt_x.
  apply (Niteri_from_none f fp (Z.to_nat n)) in opt_x.
  unfold Ziteri.
  exact opt_x.
Qed.

Lemma Ziteri_cut_0 {A} {f: Z -> DS A -> DS A} (fp: forall n, failure_passing (f n)) {n x m v s}:
  runStateT (Ziteri f n x) m = ret (v, s) ->
  exists v' s', runStateT x m = ret (v', s').
Proof.
  unfold Ziteri.
  intros opt_n.
  apply Niteri_Ziteri in opt_n.
  apply (Niteri_cut_0 fp) in opt_n.
  exact opt_n.
Qed.

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
Proof.
  reflexivity.
Qed.

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
Proof.
  revert x m v s.
  induction n; intros x m v s opt_1.
  - (* O *)
    unfold oNiteri at 1. simpl. rewrite opt_1. reflexivity.
  - (* S n *)
    specialize (IHn _ _ _ _ opt_1).
    unfold oNiteri at 1.
    simpl.
    unfold oZiteri_wrapper at 1.
    rewrite <- oNiteri_unfolder.
    bind_rewrite_osT IHn.
    rewrite <- Nat2Z.inj_succ.
    reflexivity.
Qed.

Lemma oZiteri_link_head {A} {f: Z -> A -> DS A} {n x m v s}:
  0 <= n ->
  runStateT (oZiteri f 1 x) m = ret (v, s) ->
  runStateT (oZiteri (fun z => f (Z.succ z)) n v) s =
  runStateT (oZiteri f (Z.succ n) x) m.
Proof.
  intros n_range.
  repeat rewrite oZiteri_oNiteri.
  replace (Z.to_nat 1) with 1%nat by reflexivity.
  replace (Z.to_nat (Z.succ n)) with (S (Z.to_nat n)).
  exact oNiteri_link_head.
  symmetry. apply Z2Nat.inj_succ. assumption.
Qed.

Lemma failure_passing_oZiteri_wrapper {A} (f: Z -> A -> DS A):
  forall n, failure_passing ((oZiteri_wrapper f) n).
Proof.
  intros n.
  unfold oZiteri_wrapper, failure_passing.
  intros m x opt_x.
  apply bind_rewrite_osT_mzero. exact opt_x.
Qed.

Lemma oZiteri_0 {A} {f: Z -> A -> DS A} {x:A}:
  oZiteri f 0 x = ret x.
Proof.
  unfold oZiteri. apply Ziteri_0.
Qed.

Lemma oZiteri_base {A} {f: Z -> A -> DS A} {x:A} n:
  n <= 0 ->
  oZiteri f n x = ret x.
Proof.
  intros n_range.
  unfold oZiteri. apply Ziteri_base. assumption.
Qed.

Lemma oZiteri_cut_head {A} {f: Z -> A -> DS A} {n x m v s}:
  0 <= n ->
  runStateT (oZiteri f (Z.succ n) x) m = ret (v, s) ->
  exists v' s', runStateT (oZiteri f 1 x) m = ret (v', s') /\
                runStateT (oZiteri (fun z => f (Z.succ z)) n v') s' = ret (v, s).
Proof.
  intros n_range opt_succ_n.
  unfold oZiteri in opt_succ_n |-*.
  rewrite Ziteri_add1 in opt_succ_n by assumption.
  assert (fp: forall n0 : Z, failure_passing (oZiteri_wrapper f (Z.succ n0))).
  { apply failure_passing_oZiteri_wrapper. }
  destruct (Ziteri_cut_0 fp opt_succ_n) as (v' & s' & opt_1).
  exists v', s'.
  split; [assumption|].
  rewrite <- oZiteri_unfolder.
  rewrite <- Ziteri_add1 in opt_succ_n by assumption.
  rewrite <- oZiteri_unfolder in opt_succ_n, opt_1.
  erewrite oZiteri_link_head.
  exact opt_succ_n.
  omega.
  exact opt_1.
Qed.

Lemma Ziteri_1 {A} {f: Z -> A -> A} {x:A}:
  Ziteri f 1 x = f 0 x.
Proof.
  reflexivity.
Qed.

Lemma oZiteri_1 {A} {f: Z -> A -> DS A} {x m}:
  runStateT (oZiteri f 1 x) m = runStateT (f 0 x) m.
Proof.
  unfold oZiteri.
  rewrite Ziteri_1.
  unfold oZiteri_wrapper.
  destruct (runStateT (f 0 x) m) eqn:opt_0.
  - destruct p as [v s].
    erewrite bind_osT.
    reflexivity.
    apply ret_osT. split; reflexivity.
    exact opt_0.
  - rewrite bind_of_return. exact opt_0.
    typeclasses eauto.
Qed.

Lemma oZiteri_neg {A} {f: Z -> A -> DS A} {x:A} {p}:
  oZiteri f (Z.neg p) x = ret x.
Proof.
  reflexivity.
Qed.

Lemma oZiteri_cut_tail {A} {f: Z -> A -> DS A} {n x m v s}:
  0 <= n ->
  runStateT (oZiteri f (Z.succ n) x) m = ret (v, s) ->
  exists v' s', runStateT (oZiteri f n x) m = ret (v', s') /\
                runStateT (oZiteri (fun z => f (n + z)) 1 v') s' = ret (v, s).
Proof.
  intros n_range opt_succ_n.
  unfold oZiteri in opt_succ_n |-*.
  rewrite Ziteri_succ in opt_succ_n by assumption.
  unfold oZiteri_wrapper at 1 in opt_succ_n.
  split_run opt_succ_n as (v' & s' & opt_n & opt_rest).
  exists v', s'.
  split. exact opt_n.
  rewrite Ziteri_1.
  unfold oZiteri_wrapper.
  replace (n + 0) with n by apply Zplus_0_r_reverse.
  erewrite bind_osT.
  - reflexivity.
  - apply ret_osT. split; reflexivity.
  - exact opt_rest.
Qed.
Lemma oZiteri_mid {A} {f: Z -> A -> DS A} n0 n1 {x m v s}:
  0 <= n0 ->
  0 <= n1 ->
  runStateT (oZiteri f (n0+n1) x) m = ret (v, s) ->
  exists v' s', runStateT (oZiteri f n0 x) m = ret (v', s') /\
                runStateT (oZiteri (fun z => f (n0 + z)) n1 v') s' = ret (v, s).
Proof.
  revert n1 n0 x m v s.
  refine (Z_nonneg_peano_ind _ _ _ _).
  - (* n1 = 0 *)
    intros n0 x m v s n0_range n1_range opt. (* 0 = n1 *)
    replace (n0 + 0) with n0 in opt by apply Zplus_0_r_reverse.
    exists v, s.
    split. assumption.
    reflexivity.
  - (* n1 > 0 *)
    intros z z_range Hind n0 x m v s n0_range n1_range opt. (* Z.succ z = n1 *)
    replace (n0 + Z.succ z) with (Z.succ (n0 + z)) in opt by omega.
    apply oZiteri_cut_tail in opt. 2:omega.
    destruct opt as (v1 & s1 & opt_n0_z & opt_rest).
    destruct (Hind _ _ _ _ _ n0_range z_range opt_n0_z) as (v' & s' & opt_n0 & opt_z).
    exists v', s'.
    split; [assumption|].
    (* NOTE: unfold oZiteri in opt_rest *)
    (* unfold oZiteri in opt_rest. *)
    (* rewrite Ziteri_1 in opt_rest. *)
    unfold oZiteri.
    rewrite Ziteri_succ by assumption.
    rewrite <- oZiteri_unfolder.
    unfold oZiteri_wrapper.
    erewrite bind_osT.
    + reflexivity.
    + exact opt_z.
    + rewrite oZiteri_1 in opt_rest.
      replace (n0 + z + 0) with (n0 + z) in opt_rest by omega.
      exact opt_rest.
  - (* n1 < 0 *)
    intros p n0 x m v s n0_range n1_range opt. (* Z.neg p = n1 *)
    pose (Pos2Z.neg_is_neg p) as n1_neg.
    omega.
Qed.

Lemma oZiteri_pure {A} {f: Z -> A -> DS A}:
  forall n m,
  (forall z, 0 <= z < n -> forall r v s, runStateT (f z r) m = ret (v, s) -> m = s) ->
  forall x v s, runStateT (oZiteri f n x) m = ret (v, s) ->
                m = s.
Proof.
  refine (Z_nonneg_peano_ind _ _ _ _).
  - (* n = 0 *)
    intros m pure_step x v s opt.
    rewrite oZiteri_0 in opt.
    apply ret_osT in opt. tauto.
  - (* n > 0 *)
    intros z z_range Hind m pure_step x v s opt.
    apply oZiteri_cut_tail in opt. 2:assumption.
    destruct opt as (v' & s' & opt_z & opt_last).
    assert (m = s').
    { eapply Hind. 2:exact opt_z.
      intros z0 z0_range. apply pure_step. omega. }
    subst s'.
    rewrite oZiteri_1 in opt_last.
    replace (z + 0) with z in opt_last by omega.
    eapply (pure_step z). omega.
    exact opt_last.
  - (* n < 0 *)
    intros p m pure_step x v s opt.
    rewrite oZiteri_neg in opt.
    apply ret_osT in opt. tauto.
Qed.

(* connect multiple individual steps into n steps *)
Lemma oZiteri_connect {A} {f: Z -> A -> DS A}:
  forall n x m,
    (forall z, 0 <= z < n ->
     forall v' s', runStateT (oZiteri f z x) m = ret (v', s') ->
                   exists v s, runStateT (f z v') s' = ret (v, s)) ->
  exists v s, runStateT (oZiteri f n x) m = ret (v, s).
Proof.
  intros n. revert n f.
  refine (Z_nonneg_peano_ind _ _ _ _).
  - (* n = 0 *)
    intros f x m opt_step.
    exists x, m.
    reflexivity.
  - (* n > 0 *)
    intros z z_range Hind f x m opt_step.
    assert (opt_1: exists v1 s1, runStateT (f 0 x) m = ret (v1, s1)).
    { eapply opt_step. omega.
      rewrite oZiteri_0. reflexivity. }
    destruct opt_1 as (v1 & s1 & opt_1).
    assert (opt_rest: exists v s, runStateT (oZiteri (fun z => f (Z.succ z)) z v1) s1 = ret (v, s)).
    { apply Hind.
      intros z0 z0_range v' s' opt_z0.
      apply opt_step. omega.
      erewrite <- oZiteri_link_head.
      - exact opt_z0.
      - apply z0_range.
      - rewrite oZiteri_1. exact opt_1. }
    destruct opt_rest as (v & s & opt_rest).
    exists v, s.
    erewrite <- oZiteri_link_head.
    + exact opt_rest.
    + assumption.
    + rewrite oZiteri_1. exact opt_1.
  - (* n < 0 *)
    intros p f x m opt_step.
    exists x, m.
    reflexivity.
Qed.

(* TODO: This proof is not quite elegant, should have been proven by oZiteri_pure and oZiteri_connect. *)
Lemma oZiteri_pure_connect {A} {f: Z -> A -> DS A}:
  forall n x m,
    (forall z, 0 <= z < n ->
     forall v', runStateT (oZiteri f z x) m = ret (v', m) ->
                   exists v, runStateT (f z v') m = ret (v, m)) ->
    exists v, runStateT (oZiteri f n x) m = ret (v, m).
Proof.
  intros n. revert n f.
  refine (Z_nonneg_peano_ind _ _ _ _).
  - (* n = 0 *)
    intros f x m opt_step.
    exists x.
    reflexivity.
  - (* n > 0 *)
    intros z z_range Hind f x m opt_step.
    assert (opt_1: exists v1, runStateT (f 0 x) m = ret (v1, m)).
    { eapply opt_step. omega.
      rewrite oZiteri_0. reflexivity. }
    destruct opt_1 as [v1 opt_1].
    assert (opt_rest: exists v, runStateT (oZiteri (fun z => f (Z.succ z)) z v1) m = ret (v, m)).
    { apply Hind.
      intros z0 z0_range v' opt_z0.
      apply opt_step. omega.
      erewrite <- oZiteri_link_head.
      - exact opt_z0.
      - apply z0_range.
      - rewrite oZiteri_1. exact opt_1. }
    destruct opt_rest as [v opt_rest].
    exists v.
    erewrite <- oZiteri_link_head.
    + exact opt_rest.
    + assumption.
    + rewrite oZiteri_1. exact opt_1.
  - (* n < 0 *)
    intros p f x m opt_step.
    exists x.
    reflexivity.
Qed.

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
Proof.
  induction p; intros.
  - tauto.
  - simpl.
    unfold for_step_opt at 1.
    apply bind_rewrite_osT_mzero.
    apply IHp. assumption.
Qed.

Lemma for_from_none' {tmp} id_it (se: spec_env_t tmp):
  forall p f s0 start init,
    runStateT init s0 = mzero ->
    runStateT (Niteri (fun z => @for_step_opt _ id_it se start f (Z.succ z))
                      p
                      init) s0 = mzero.
Proof.
  induction p; intros.
  - tauto.
  - simpl.
    unfold for_step_opt at 1.
    apply bind_rewrite_osT_mzero.
    apply IHp. assumption.
Qed.

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
Proof.
  induction d; intros.
  - simpl.
    unfold for_step_opt at 1.
    rewrite Z.add_0_r. apply H.
  - simpl. simpl in IHd.
    unfold for_step_opt at 1.
    unfold for_step_opt at 3.
    specialize (IHd _ _ _ H).
    bind_rewrite_osT IHd.
    replace (start + Z.succ (Z.of_nat d)) with (start + 1 + Z.of_nat d) by omega.
    reflexivity.
Qed.

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
Proof.
  unfold Ziteri.
  intros.
  Transparent execStateT.
  unfold execStateT.
  Opaque execStateT.
  rewrite (for_n_plus_1__runStateT _ _ _ _ _ start mh m').
  reflexivity.
  exec2run H as v.
  destruct v; auto.
Qed.

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
Proof.
  intros until s. intros steps_nonneg Hiter.
  destruct (runStateT (f (SpecTree.set id_it int_Z32_pair start se)) s0) eqn:Hrun.
  - destruct p as [v' s']; exists s'; destruct v'; split; [reflexivity|].
    rewrite Ziteri_add1 in Hiter by assumption.
    unfold Ziteri in Hiter.
    rewrite (for_n_plus_1__runStateT _ _ _ _ _ _ _ _ Hrun) in Hiter.
    apply Hiter.

  - simpl in Hiter.
    unfold Ziteri in Hiter.
    rewrite Z2Nat.inj_succ in Hiter by assumption.
    rewrite Niteri_add1 in Hiter.
    rewrite for_from_none' in Hiter. discriminate.
    simpl. unfold for_step_opt at 1.
    rewrite bind_of_return by apply MonadLaws_DS.
    rewrite Z.add_0_r. assumption.
Qed.

Lemma oZiteri_fail_at_1 {A} {f: Z -> A -> DS A} {n x m}:
  0 <= n ->
  runStateT (oZiteri f 1 x) m = mzero ->
  runStateT (oZiteri f (Z.succ n) x) m = mzero.
Proof.
  intros n_range opt_1.
  rewrite oZiteri_1 in opt_1.
  unfold oZiteri.
  rewrite Ziteri_add1 by assumption.
  apply Ziteri_from_none.
  { (* failure_passing *)
    unfold failure_passing, oZiteri_wrapper.
    intros n0 m0 x0 opt_x0.
    apply bind_rewrite_osT_mzero. assumption. }
  rewrite Ziteri_1. unfold oZiteri_wrapper.
  erewrite bind_rewrite_osT_ret. exact opt_1. reflexivity.
Qed.

Lemma oZiteri_replace {A} {f: Z -> A -> DS A} {g: Z -> A -> DS A}:
  (forall z r s, runStateT (f z r) s = runStateT (g z r) s) ->
  forall n x m, runStateT (oZiteri f n x) m = runStateT (oZiteri g n x) m.
Proof.
  intros Heq n. revert n f g Heq.
  refine (Z_nonneg_peano_ind _ _ _ _).
  - (* n = 0 *)
    intros f g Heq x m. reflexivity.
  - (* n > 0 *)
    intros z z_range Hind f g Heq x m.
    destruct (runStateT (oZiteri f 1 x) m) eqn:opt_1.
    + destruct p as [v s].
      erewrite <- (oZiteri_link_head z_range opt_1).
      rewrite oZiteri_1 in opt_1.
      rewrite Heq in opt_1.
      rewrite <- oZiteri_1 in opt_1.
      erewrite <- (oZiteri_link_head z_range opt_1).
      eapply Hind.
      intros z0 r s0.
      apply Heq.
    + rewrite oZiteri_fail_at_1 by assumption.
      symmetry.
      apply oZiteri_fail_at_1. assumption.
      rewrite oZiteri_1 in opt_1. rewrite Heq in opt_1.
      exact opt_1.
  - (* n < 0 *)
    intros p f g Heq x m.
    do 2 rewrite oZiteri_neg.
    reflexivity.
Qed.

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
Proof.
  intros until s'. intros opt_f n n_range.
  unfold fold_step_opt.
  erewrite <- oZiteri_link_head.
  - apply oZiteri_replace.
    intros z r s.
    replace (start + Z.succ z) with (Z.succ start + z) by omega. reflexivity.
  - assumption.
  - rewrite oZiteri_1. replace (start + 0) with start by apply Zplus_0_r_reverse. apply opt_f.
Qed.

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
Proof.
  intros until s. intros steps_nonneg opt_succ_steps.
  apply oZiteri_cut_head in opt_succ_steps. 2:assumption.
  destruct opt_succ_steps as (v' & s' & opt_1 & opt_steps).
  exists v', s'.
  rewrite oZiteri_1 in opt_1. unfold fold_step_opt in opt_1.
  replace (start + 0) with start in opt_1 by apply Zplus_0_r_reverse.
  split. assumption.
  erewrite oZiteri_replace. exact opt_steps.
  intros z r s0.
  unfold fold_step_opt.
  rewrite Z.add_succ_comm. reflexivity.
Qed.


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
Proof.
  intros ? ? ? ? ? ? steps_nonneg Hiter.
  simpl in *.
  exec2run Hiter as [].
  apply for_cut_opt' in Hiter; [|assumption].
  destruct Hiter as (s' & Hiter_1 & Hiter_n).
  apply runStateT_execStateT in Hiter_1.
  apply runStateT_execStateT in Hiter_n.
  exists s'. split.
  - exact Hiter_1.
  - exact Hiter_n.
Qed.

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
Proof.
  intro z_nonneg.
  unfold Zfind.
  rewrite Z2Nat.inj_succ by assumption.
  replace (Z.succ z) with (Z.of_nat (S (Z.to_nat z))) by
      (rewrite <- (Z2Nat.id z) at 2 by assumption;
       apply Nat2Z.inj_succ).
  reflexivity.
Qed.

(* Transparent ret. *)
Lemma Zfind_pure (check:Z -> DS bool):
  (forall z m v s, runStateT (check z) m = ret (v, s) -> m = s) ->
  forall z m v s, runStateT (Zfind check z) m = ret (v, s) ->
             m = s.
Proof.
  intros Hstep.
  refine (Z_nonneg_peano_ind _ _ _ _).
  - (* z = 0 *)
    intros ? ? ? Hrun.
    unfold Zfind in Hrun. simpl in Hrun.
    apply ret_osT in Hrun. apply Hrun.
  - (* z > 0 *)
    intros ? Hz Hind ? ? ? Hrun.
    rewrite Zfind_succ in Hrun by omega.
    dest_opt Hrun as Hrun' [v' s'].
    specialize (Hstep (Z.succ z) m v' s').
    apply Hstep in Hrun; subst s'; clear Hstep.
    destruct v'.
    + (* check = true  *)
      apply ret_osT in Hrun'. apply Hrun'.
    + (* check = false *)
      eapply Hind. exact Hrun'.
  - (* z < 0 *)
    intros ? ? ? ? Hrun.
    unfold Zfind in Hrun. simpl in Hrun.
    apply ret_osT in Hrun. apply Hrun.
Qed.

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
  me_callvalue := me_callvalue me; (* FIXME: the callvalue modeling is wrong *)
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
           (SpecTree.set id_it int_Z32_pair bound (* make more sense to swap n and bound here,
                                                     but I did not change it to consist with legacy code. *)
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
  (* | CCcall_ext argt ret addr prim args =>
    fun wf se =>
      let argv := map2_synth_expr_spec args se wf in
      prim.(PRIMsem_opt) argv (ext_call_me addr) *)
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
(*          m <- get;;
        put (l.(ltype_set)
                 (apply_curried_func
                    constr
                    (map2_synth_expr_spec flds se (cdr wf))) m) *)
  | CCassert c =>
    fun (wf: (synth_stmt_pure c ~~~ true) * synth_stmt_wellformed c dest tmp) se =>
      v <- synth_stmt_spec_opt c dest tmp (cdr wf) se;;
      guard v
  | CCdeny c =>
    fun (wf: (synth_stmt_pure c ~~~ true) * synth_stmt_wellformed c dest tmp) se =>
      v <- synth_stmt_spec_opt c dest tmp (cdr wf) se;;
      guard (negb v)
  (* | CCghost c =>
    synth_stmt_spec_opt c dest tmp *)
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
  (* | _ => fun wf se => mzero *)
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
Proof.
  intros pure_step m v s se opt_steps.
  eapply oZiteri_pure.
  2:exact opt_steps.
  intros z z_range r v0 s0 opt_z.
  unfold fold_step_opt in opt_z.
  apply pure_step in opt_z. assumption.
Qed.

Lemma get_put_pure {A} {s v d} {f: DS A}:
  runStateT (m <- get;; f ;; put m) d = ret (v, s) ->
  d = s.
Proof.
  intros opt.
  dest_opt opt as opt1 [v1 s1].
  apply get_osT in opt. destruct opt; subst v1 s1.
  dest_opt opt1 as opt2 [v2 s2].
  apply put_osT in opt2. destruct opt2; subst v s.
  reflexivity.
Qed.

Lemma first_map_pure' (check: Z -> DS bool) start steps:
  (* (forall i m v s, 0 <= i < steps -> (runStateT (check (start+i)) m = ret (v, s) -> m = s)) -> *)
  forall m l s, runStateT (first_map check start (start + steps)) m = ret (l, s) ->
           m = s.
Proof.
  unfold first_map. replace (start + steps - start) with steps by omega.
  intros (* step_pure *) m l s opt.
  eapply oZiteri_pure. 2:exact opt.
  intros z z_range r v s0 step_pure'.
  simpl in step_pure'.
  apply get_put_pure in step_pure'. assumption.
(*  split_run step_pure' as (v' & s' & opt_check & opt_ret).
  rewrite ret_osT in opt_ret.
  destruct opt_ret; subst v s0.
  eapply (step_pure z). assumption.
  exact opt_check. *)
Qed.

Lemma first_map_pure (check: Z -> DS bool) start bound:
  (* (forall i m v s, 0 <= i < (bound-start) -> runStateT (check (start+i)) m = ret (v, s) -> m = s) -> *)
  forall m l s, runStateT (first_map check start bound) m = ret (l, s) ->
           m = s.
Proof.
  intros Hstep ? ? Hrun.
  eapply first_map_pure'.
  replace bound with (start + (bound - start)) in Hrun by omega.
  exact Hrun.
(*  - apply Hstep.
  - replace bound with (start + (bound - start)) in Hrun by omega.
    exact Hrun. *)
Qed.

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
Proof.
  intros Himpl [x HQ].
  exists x. apply Himpl. exact HQ.
Qed.

Lemma exists_impl2 {A B} (P:A->B->Prop) (Q:A->B->Prop):
  (forall x y, Q x y -> P x y) ->
  (exists x y, Q x y) ->
  exists x y, P x y.
Proof.
  intros Himpl (x & y & HQ).
  exists x, y. apply Himpl. exact HQ.
Qed.

Lemma first_map_osT' (check: Z -> DS bool) start steps m:
  (forall i, 0 <= i < steps -> exists v, runStateT (check (start+i)) m = ret (v, m)) ->
  exists l, runStateT (first_map check start (start+steps)) m = ret (l, m).
Proof.
  intros opt_step.
  assert (opt_steps: exists v,
             runStateT (oZiteri (fun i l => m <- get;; h <- check (start + i);; put m) steps tt) m = ret (v, m)).
  { eapply oZiteri_pure_connect.
    intros z z_range l' opt_z.
    destruct (opt_step z z_range) as [h opt_check].
    exists tt.
    eapply bind_osT. apply get_osT. tauto.
    eapply bind_osT. exact opt_check.
    apply put_osT. tauto.
  }
  destruct opt_steps as [v opt_steps].
  exists v.
  unfold first_map.
  replace (start + steps - start) with steps by omega.
  exact opt_steps.
Qed.

Lemma first_map_osT (check: Z -> DS bool) start bound m:
  (forall i, start <= i < bound -> exists v, runStateT (check i) m = ret (v, m)) ->
  exists l, runStateT (first_map check start bound) m = ret (l, m).
Proof.
  intros Hstep.
  replace bound with (start + (bound - start)) by omega.
  eapply first_map_osT'.
  intros j j_range.
  eapply Hstep. omega.
Qed.

Lemma first_map_osT_weak' (check: Z -> DS bool) start steps m:
  (forall i, 0 <= i < steps -> exists v m', runStateT (check (start+i)) m = ret (v, m')) ->
  exists l, runStateT (first_map check start (start+steps)) m = ret (l, m).
Proof.
  intros opt_step.
  assert (opt_steps: exists v,
             runStateT (oZiteri (fun i l => m <- get;; h <- check (start + i);; put m) steps tt) m = ret (v, m)).
  { eapply oZiteri_pure_connect.
    intros z z_range l' opt_z.
    destruct (opt_step z z_range) as [h [m' opt_check]].
    exists tt.
    eapply bind_osT. apply get_osT. tauto.
    eapply bind_osT. exact opt_check.
    apply put_osT. tauto.
  }
  destruct opt_steps as [v opt_steps].
  exists v.
  unfold first_map.
  replace (start + steps - start) with steps by omega.
  exact opt_steps.
Qed.

Lemma first_map_osT_weak (check: Z -> DS bool) start bound m:
  (forall i, start <= i < bound -> exists v m', runStateT (check i) m = ret (v, m')) ->
  exists l, runStateT (first_map check start bound) m = ret (l, m).
Proof.
  intros Hstep.
  replace bound with (start + (bound - start)) by omega.
  eapply first_map_osT_weak'.
  intros j j_range.
  eapply Hstep. omega.
Qed.

  Lemma first_map_mid n (check: Z -> DS bool) start bound:
    start <= n < bound ->
    forall m v s, runStateT (first_map check start bound) m = ret (v, s) ->
    exists v' s', runStateT (check n) m = ret (v', s').
  Proof.
    intros n_range. intros m v s opt_map.
    unfold first_map in opt_map.
    replace (bound - start) with ((n - start) + (bound - n)) in opt_map by omega.
    apply oZiteri_mid in opt_map; [|omega|omega].
    destruct opt_map as (v_mid & s_mid & opt_left & opt_right).
    assert (m = s_mid).
    { apply first_map_pure in opt_left. assumption. }
    subst s_mid.
    replace (bound - n) with (Z.succ (bound - n - 1)) in opt_right by omega.
    apply oZiteri_cut_head in opt_right; [|omega].
    destruct opt_right as (v_l & s_l & opt_n & opt_rest).
    rewrite oZiteri_1 in opt_n.
    split_run opt_n as (v' & s' & opt_get & opt_n). apply get_osT in opt_get. destruct opt_get; subst v' s'.
    split_run opt_n as (v' & s' & opt_n & opt_put). apply put_osT in opt_put. destruct opt_put; subst v_l s_l.
    replace (start + (n - start + 0)) with n in opt_n by omega.
    exists v', s'.
    exact opt_n.
  Qed.

  Lemma some_injective_isomorphism {A}{a b : A} : Some a ~~~ Some b -> a ~~~ b.
  Proof.
    intros some_iso.
    split.
    - intros F fa.
      set (F' x := match x with None => F a | Some x => F x end).
      apply (iso_f some_iso (F := F')), fa.
    - intros F fb.
      set (F' x := match x with None => F a | Some x => F x end).
      apply (iso_g some_iso (F := F')), fb.
  Qed.

  Lemma set_lenv_cond tmp (se : SpecTree.t tmp) le i htp f v :
    let set_se := SpecTree.set i htp f se in
    let set_le := PTree.set i v le in
      ht_ft_cond f ->
      ht_rel f v ->
      lenv_cond se le ->
      lenv_cond set_se set_le.
  Proof.
    intros set_se set_le rel f_cond le_cond i' tp' hti'.
    set (htp' := @mk_hyper_type_pair tp' hti').
    intros eq; generalize eq.  (* save it so that we can change it *)
    unfold set_se, set_le.

    destruct (peq i' i) as [ i_eq | i_ne ].
    - rewrite i_eq in eq |- *; rewrite AList.gss in eq.
      intros eq'.
      rewrite (SpecTree.gess' i _ se _ eq), PTree.gss.
      generalize eq.
      set (F x := forall eq0 : Some htp ~~~ Some x,
        ht_ft_cond (eq0 SpecTree.OptionalType f) /\
        ht_rel_some (eq0 SpecTree.OptionalType f) (Some v)).
      apply (some_injective_isomorphism eq F).
      intros eq_htp.
      rewrite <- functor_identity_paramatricity.
      split; [| constructor ]; assumption.
    - rewrite AList.gso in eq; try assumption.
      intros eq'.
      rewrite (SpecTree.geso i' i _ _ eq' eq i_ne), PTree.gso; try assumption.
      apply le_cond.
  Qed.

  Lemma unset_lenv_cond {tmp i htp f}{se : SpecTree.t tmp}{le} :
      (tmp ! i ~~~ None)%alist ->
      lenv_cond (SpecTree.set i htp f se) le -> lenv_cond se le.
  Proof.
    intros iso le_cond i' tp' hti'.
    set (htp' := mk_hyper_type_pair tp').
    intros eq.

    destruct (peq i' i) as [ i_eq | i_ne ].
    - exfalso.
      rewrite i_eq in eq.
      set (is_some (o : option hyper_type_pair) :=
             match o with None => false | _ => true end).
      apply (isomorphism_decidable_neq_absurd is_some (!eq @ iso));
        reflexivity.
    - assert (iso' : (AList.set i htp tmp) ! i' %alist ~~~ Some htp')
        by (rewrite AList.gso; assumption).
      specialize (le_cond i' tp' hti' iso').
      rewrite (SpecTree.geso _ _ _ _ iso' eq i_ne) in le_cond.
      apply le_cond.
  Qed.

  (* The le is allowed to have more mappings than the se. *)
  Lemma set_le_lenv_cond tmp (se : SpecTree.t tmp) le i v :
      (tmp ! i)%alist ~~~ None -> lenv_cond se le -> lenv_cond se (PTree.set i v le).
  Proof.
    intros iso le_cond i' tp' hti'.
    set (htp' := @mk_hyper_type_pair tp' hti').
    intros eq; generalize eq.  (* save it so that we can change it *)

    destruct (peq i' i) as [ i_eq | i_ne ].
    - exfalso.
      rewrite i_eq in eq.
      set (is_some (o : option hyper_type_pair) :=
             match o with None => false | _ => true end).
      apply (isomorphism_decidable_neq_absurd is_some (!eq @ iso));
        reflexivity.
    - intros eq'.
      rewrite PTree.gso; try assumption.
      apply le_cond.
  Qed.

  (* If the le has more mappings than se, then we can extend the se correspondingly. *)
  Lemma set_se_lenv_cond tmp (se : SpecTree.t tmp) le i htp f v :
    let set_se := SpecTree.set i htp f se in
      ht_ft_cond f ->
      ht_rel f v ->
      le ! i = Some v ->
      lenv_cond se le ->
      lenv_cond set_se le.
  Proof.
    intros set_se f_cond rel le_i_v le_cond i' tp' hti'.
    set (htp' := @mk_hyper_type_pair tp' hti').
    intros eq; generalize eq.  (* save it so that we can change it *)
    unfold set_se.

    destruct (peq i' i) as [ i_eq | i_ne ].
    - rewrite i_eq in eq |- *; rewrite AList.gss in eq.
      intros eq'.
      rewrite (SpecTree.gess' i _ se _ eq), le_i_v.
      generalize eq.
      set (F x := forall eq0 : Some htp ~~~ Some x,
        ht_ft_cond (eq0 SpecTree.OptionalType f) /\
        ht_rel_some (eq0 SpecTree.OptionalType f) (Some v)).
      apply (some_injective_isomorphism eq F).
      intros eq_htp.
      rewrite <- functor_identity_paramatricity.
      split; [| constructor ]; assumption.
    - rewrite AList.gso in eq by assumption.
      intros eq'.
      rewrite (SpecTree.geso i' i _ _ eq' eq i_ne); try assumption.
      apply le_cond.
  Qed.

  Lemma get_neq_idx_neq {tmp i j}{htp : hyper_type_pair} :
    (tmp ! i)%alist ~~~ Some htp -> (tmp ! j)%alist ~~~ None -> i <> j.
  Proof.
    intros iso_i iso_j i_eq.
    rewrite i_eq in iso_i.
    set (is_some (o : option hyper_type_pair) :=
           match o with None => false | _ => true end).
    apply (isomorphism_decidable_neq_absurd is_some (!iso_i @ iso_j));
      reflexivity.
  Qed.

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
  Proof.
    intros not_Plo.
    destruct
      (LangDef.min_ex P lo hi Pdec) as [ (n & n_range & Pn & n_least) | no_n ],
      (LangDef.min_ex P (Z.succ lo) hi Pdec)
                                     as [ (m & m_range & Pm & m_least) | no_m ].
    - (* n = m *)
      destruct (Ztrichotomy n m) as [ lt | [ eql | gt ]].
      + (* if n < m *)
        contradiction (m_least n); try split; try assumption.
        (* Z.succ lo <= n (excluding lo = n b/c not_Plo) *)
        destruct (proj1 (Z.lt_eq_cases _ _) (proj1 n_range)) as [ lt' | eql ].
        * apply Z.le_succ_l, lt'.
        * rewrite eql in not_Plo; contradiction (not_Plo Pn).
      + (* if n = n *)
        f_equal; exact eql.
      + (* if n > m *)
        apply Z.gt_lt in gt.
        contradiction (n_least m); try split; try assumption.
        (* lo <= m (weaker than Z.succ lo <= m) *)
        apply Z.lt_le_incl, Z.le_succ_l, m_range.
    - (* n = default *)
      contradiction (no_m n); split; try apply n_range.
      (* Z.succ lo <= n (excluding lo = n b/c not_Plo) *)
      destruct (proj1 (Z.lt_eq_cases _ _) (proj1 n_range)) as [ lt' | eql ].
      + apply Z.le_succ_l, lt'.
      + rewrite eql in not_Plo; contradiction (not_Plo Pn).
    - (* default = m *)
      contradiction (no_n m); split; try apply m_range.
      (* lo <= m (weaker than Z.succ lo <= m) *)
      apply Z.lt_le_incl, Z.le_succ_l, m_range.
    - reflexivity.
  Qed.

  (** [synth_stmt_cond] is the verification condition for
      [synth_stmt_spec_opt] to refine the Clight semantics of [synth_stmt_stmt],
      i.e. the one that the user has to manually prove.
      This is Figure 4.11 in the thesis.

      Naming convention: _cond is verification conditions.

        [forall se mh, oProp2 (synth_stmt_ocond c dest tmp wf) se mh ->
         forall B k b,
         synth_stmt_spec_opt c dest tmp wf se mh B k = Some b ->
         forall le ml ge, [[se and mh corresponding to le, ml, and ge]] ->
         exists r,
           (forall B' k', synth_stmt_spec_opt c dest tmp wf se mh B' k' = k' r) /\
           exists le' ml', [[r correspond to le' and ml']] /\
           LangDef.exec_stmt m0 ge env le ml (synth_stmt_stmt c dest tmp)
                             E0 le' ml' Out_normal]

      as shown in [synth_stmt_correct] below. *)


  Fixpoint fold_expr_constr_list_cond {tmp tps} es
    : @fold_synth_expr_wellformed tmp tps es ->
      spec_env_t tmp -> Prop :=
    match es with
    | HNil => fun _ _ => True
    | HCons x ls e es => fun wf se =>
      let (wf, IHwf) := wf in
      oProp1 (synth_expr_ocond me tmp e wf /\
(*
              ht_valid_ft_ocond .{ synth_expr_spec tmp e wf }
*)
(* The second conjunct is syntax for this: *)
    (omap1 (fun (p : unpair_ft (tp_type_pair x) -> Prop)
                (y : spec_env_t tmp) =>
              p
                (synth_expr_spec me tmp e wf y))
           ht_valid_ft_ocond) 
             )%oprop1 se /\ (* NOTE: implement synth_expr_cond? *)
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
(*    | CClet _ tp _ id c1 c2 =>
      fun (wf : let tmp' := AList.set id (mk_hyper_type_pair tp) tmp in
                ((tmp ! id ~~~ None) * dest_not_exist dest tmp') *
                (synth_stmt_wellformed c1 id tmp * synth_stmt_wellformed c2 dest tmp'))
        se m =>
        forall v m1, runStateT (synth_stmt_spec_opt c1 id tmp (cadr wf) se) m = ret (v, m1) ->
      synth_stmt_cond c1 id _ (cadr wf) se m /\
      synth_stmt_cond c2 dest _ (cddr wf)
        (SpecTree.set id (mk_hyper_type_pair tp) v se) m1 *)
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
(*    | CCsequence _ c1 c2 =>
      fun (wf : synth_stmt_wellformed c1 dest tmp * synth_stmt_wellformed c2 dest tmp)
        se m =>
        forall m1, execStateT (synth_stmt_spec_opt c1 dest tmp (car wf) se) m = ret m1 ->
      synth_stmt_cond c1 dest tmp (car wf) se m /\
      synth_stmt_cond c2 dest tmp (cdr wf) se m1 *)
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
    (* | CCcall_ext argt ret prim args => fun wf se _ =>
      fold_expr_constr_list_cond args wf se *)
    | CCyield tp _ e => fun wf se _ => oProp1 (synth_expr_ocond me tmp e wf) se
    | CCconstr _ _ _ _ el flds _ =>
      fun (wf : synth_lexpr_wellformed tmp el * fold_synth_expr_wellformed tmp flds)
          se m =>
      oProp1 (synth_lexpr_ocond me tmp el (car wf)) se /\
      oProp1 (synth_lexpr_spec me tmp el (car wf) se).(ltype_set_ocond) m /\
      fold_expr_constr_list_cond flds (cdr wf) se
    | CCassert c => fun wf se m => synth_stmt_cond c dest tmp (cdr wf) se m
    | CCdeny c => fun wf se m => synth_stmt_cond c dest tmp (cdr wf) se m
    (* | CCghost _ => fun _ _ _ => True *)
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
(*       let P m n :=
          evalStateT (synth_stmt_spec_opt c3 id_dest _ (cadddr wf)
                       (SpecTree.set id_it int_Z32_pair n
                         (SpecTree.set id_end int_Z32_pair bound se))) m
          = Some true
(*         is_true (synth_stmt_spec_ret id_dest _ _ (synth_stmt_spec_opt c3) (cadddr wf)
                   (SpecTree.set id_it int_Z32_pair n
                                 (SpecTree.set id_end int_Z32_pair bound se)) m) *)
       in *)
       (forall n, start <= n < bound ->
         (* ???: /\ (forall n', start <= n' < n -> ~ P m n') *)
         synth_stmt_cond c3 id_dest _ (cadddr wf)
           (SpecTree.set id_it int_Z32_pair n
             (SpecTree.set id_end int_Z32_pair bound se)) m) /\
       (forall n, start <= n < bound (* /\ P m n /\
         (forall n', start <= n' < n -> ~ P m n') *) ->
           synth_stmt_cond c4 dest _ (caddddr wf)
             (SpecTree.set id_it int_Z32_pair n
               (SpecTree.set id_end int_Z32_pair bound se)) m) /\
       ((* (forall n, start <= n < bound -> ~ P m n) -> *)
         synth_stmt_cond c5 dest _ (cdddddr wf)
           (SpecTree.set id_it int_Z32_pair bound
             (SpecTree.set id_end int_Z32_pair bound se)) m))
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
  (* | CDcall_ext: forall argts r addr prim args wf,
      fold_expr_constr_list_cond args wf se ->
      synth_stmt_CD dest tmp se m (@CCcall_ext _ argts r addr prim args) wf *)
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
  (* | CDghost: forall c wf, synth_stmt_CD dest tmp se m (@CCghost _ c) wf *)
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
  Proof.
    intros CD.
    induction CD; simpl; eauto.
    - (* CDsequence *)
      split. eauto.
      intros m1 opt.
      exec2run opt as v1. eauto.
    - (* CDfor *)
      repeat split; eauto.
      intros n n_bound m' opt.
      exec2run opt as v. eauto.
    - (* CDfold *)
      repeat split; eauto.
      intros n n_bound recur opt.
      eval2run opt as m'. eauto.
  Qed.

  (* The synth_stmt_obligation is the part of the verification condition to do with CCrespec.
     This could be merged into the definition of _cond above, but we treat it seperately
     because the respecs are generated by Edsger, so in future works Edsger could maybe also
     generate the respec proofs.
   *)

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
    (* | CCcall_ext argt ret addr prim args => fun _ _ _ _ => True *)
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
    (* | CCghost c =>
      fun (wf: synth_stmt_wellformed c dest tmp) se m =>
        synth_stmt_obligation c dest tmp wf se m *)
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
(*       let P m n :=
          evalStateT (synth_stmt_spec_opt c3 id_dest _ (cadddr wf)
                       (SpecTree.set id_it int_Z32_pair n
                         (SpecTree.set id_end int_Z32_pair bound se))) m
          = Some true
(*         is_true (synth_stmt_spec_ret id_dest _ _ (synth_stmt_spec_opt c3) (cadddr wf)
                   (SpecTree.set id_it int_Z32_pair n
                                 (SpecTree.set id_end int_Z32_pair bound se)) m) *)
       in *)
       (forall n, start <= n < bound ->
         (* ???: /\ (forall n', start <= n' < n -> ~ P m n') *)
         synth_stmt_obligation c3 id_dest _ (cadddr wf)
           (SpecTree.set id_it int_Z32_pair n
             (SpecTree.set id_end int_Z32_pair bound se)) m) /\
       (forall n, start <= n < bound (* /\ P m n /\
         (forall n', start <= n' < n -> ~ P m n') *) ->
           synth_stmt_obligation c4 dest _ (caddddr wf)
             (SpecTree.set id_it int_Z32_pair n
               (SpecTree.set id_end int_Z32_pair bound se)) m) /\
       ((* (forall n, start <= n < bound -> ~ P m n) -> *)
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
      (* synth_stmt_side_cond c dest _ (cdr wf) se m /\ *)
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
(*      (let start := synth_expr_spec tmp e1 (cadr wf) se in
       let bound := synth_expr_spec tmp e2 (caddr wf) se in
       let initial_se := SpecTree.set id_end int_Z32_pair bound se in
       forall n, start <= n < bound ->
         forall m', execStateT (Ziteri
             (for_step_opt initial_se start (synth_stmt_spec_opt c3 dest _ (cdddr wf)))
             (n - start)
             (ret tt)) m = ret m' ->
         synth_stmt_RC dest _
           (SpecTree.set id_it int_Z32_pair n
             (SpecTree.set id_end int_Z32_pair bound se))
           m' c3 (cdddr wf)) -> *)
      synth_stmt_RC dest tmp se m (@CCfor _ id_it id_end e1 e2 c3) wf

  | RCfirst: forall r id_it id_end id_dest
                    (e1 e2: @expr_constr _ tint_Z32 _) c3 (c4 c5: @cmd_constr _ r) wf,
      oProp1 (synth_expr_ocond me tmp e1 (cadr wf)) se ->
      oProp1 (synth_expr_ocond me tmp e2 (caddr wf)) se ->
      let start := synth_expr_spec me tmp e1 (cadr wf) se in
      let bound := synth_expr_spec me tmp e2 (caddr wf) se in
      (forall n, start <= n < bound (* /\ P m n /\
         (forall n', start <= n' < n -> ~ P m n') *) ->
           synth_stmt_RC dest _
             (SpecTree.set id_it int_Z32_pair n
               (SpecTree.set id_end int_Z32_pair bound se))
             m c4 (caddddr wf)) ->
      ((* (forall n, start <= n < bound -> ~ P m n) -> *)
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
       (* ht_ft_cond  *)
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
  (* | RCcall_ext: forall argts r addr prim args wf,
      ht_list_ft_cond (map2_synth_expr_spec args se wf) ->
      ht_list_valid_ft_cond (map2_synth_expr_spec args se wf) ->
      synth_stmt_RC dest tmp se m (@CCcall_ext _ argts r addr prim args) wf *)
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
(*
      (runStateT (synth_stmt_spec_opt c dest tmp (cdr wf) se) m =
       runStateT (spec (iso_f (caar wf) se)) m) ->
      synth_stmt_RC dest _ se m c (cdr wf) ->
*)
      synth_stmt_RC dest tmp se m (@CCrespec _ r tmp' c spec) wf
  | RCrespec_opt: forall r tmp' (c: cmd_constr r) (spec: machine_env GetHighData -> spec_env_t tmp' -> DS (tp_ft r)) (wf: (tmp ~~~ tmp') * synth_stmt_wellformed c dest tmp),
      (forall v m', runStateT (spec me (iso_f (car wf) se)) m = ret (v, m') -> ht_ft_cond v) ->
(*
      (runStateT (synth_stmt_spec_opt c dest _ (cdr wf) se) m =
       runStateT (spec (iso_f (car wf) se)) m) ->
      synth_stmt_RC dest _ se m c (cdr wf) ->
*)
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
      (* let p1 := synth_stmt_ret_cond c1 id _ (cadr wf) se m in
      let p2 := synth_stmt_ret_cond c2 dest _ (cddr wf) (SpecTree.set id (mk_hyper_type_pair tp) v se) m' in
      (p1 /\ p2) *)
    | CCload tp _ e => fun wf se m =>
      ht_ft_cond (ltype_get (synth_lexpr_spec me tmp e wf se) m)
    | CCstore _ _ el er => fun _ _ _ => True
    | CCsequence _ c1 c2 =>
      fun (wf : synth_stmt_wellformed c1 dest tmp * synth_stmt_wellformed c2 dest tmp)
        se m =>
        forall m', execStateT (synth_stmt_spec_opt c1 dest tmp (car wf) se) m = ret m' ->
          synth_stmt_ret_cond c2 dest tmp (cdr wf) se m'
      (*
      let p1 := synth_stmt_ret_cond c1 dest tmp (car wf) se m in
      let p2 := synth_stmt_ret_cond c2 dest tmp (cdr wf) se m' in
      (p1 /\ p2) *)
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
        (*
      (let start := synth_expr_spec tmp e1 (cadr wf) se in
       let bound := synth_expr_spec tmp e2 (caddr wf) se in
       let initial_se := SpecTree.set id_end int_Z32_pair bound se in
       forall n, start <= n < bound ->
         forall m', execStateT (Ziteri
             (for_step_opt initial_se start (synth_stmt_spec_opt c3 dest _ (cdddr wf)))
             (n - start)
             (ret tt)) m = ret m' ->
         synth_stmt_ret_cond c3 dest _ (cdddr wf)
           (SpecTree.set id_it int_Z32_pair n
             (SpecTree.set id_end int_Z32_pair bound se)) m') *)
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
       (* ht_ft_cond  *)
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
    (* fold_expr_constr_list_cond args wf se (* /\ senv_cond se *)
    /\ *) ht_list_ft_cond (map2_synth_expr_spec args se wf)
    /\ ht_list_valid_ft_cond (map2_synth_expr_spec args se wf)
    | CCyield tp _ e => fun wf se _ => oProp1 (synth_expr_ocond me tmp e wf) se
    | CCconstr _ _ _ _ el flds _ => fun _ _ _ => True
    | CCassert c => fun _ _ _ => True
    | CCdeny c => fun _ _ _ => True
    (* | CCghost c => fun _ _ _ => True *)
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
       (* forall n, start <= n < bound ->
         (* ???: /\ (forall n', start <= n' < n -> ~ P m n') *)
         synth_stmt_ret_cond c3 id_dest _ (cadddr wf)
           (SpecTree.set id_it int_Z32_pair n
             (SpecTree.set id_end int_Z32_pair bound se)) m *)
       (forall n, start <= n < bound (* /\ P m n /\
         (forall n', start <= n' < n -> ~ P m n') *) ->
           synth_stmt_ret_cond c4 dest _ (caddddr wf)
             (SpecTree.set id_it int_Z32_pair n
               (SpecTree.set id_end int_Z32_pair bound se)) m) /\
       ((* (forall n, start <= n < bound -> ~ P m n) -> *)
         synth_stmt_ret_cond c5 dest _ (cdddddr wf)
           (SpecTree.set id_it int_Z32_pair bound
             (SpecTree.set id_end int_Z32_pair bound se)) m))
    | CCrespec _ tmp' c spec =>
      fun (wf : ((tmp ~~~ tmp') * (synth_stmt_pure c ~~~ true)) *
                synth_stmt_wellformed c dest tmp) se m =>
        (forall v m', runStateT (spec (iso_f (caar wf) se)) m = ret (v, m') -> ht_ft_cond v)
(*
      runStateT (synth_stmt_spec_opt c dest tmp (cdr wf) se) m =
      runStateT (spec (iso_f (caar wf) se)) m /\
      synth_stmt_ret_cond c dest _ (cdr wf) se m
*)
    | CCrespec_opt _ tmp' c spec =>
      fun (wf : (tmp ~~~ tmp') * synth_stmt_wellformed c dest tmp) se m =>
        (forall v m', runStateT (spec me (iso_f (car wf) se)) m = ret (v, m') -> ht_ft_cond v)
(*
        runStateT (synth_stmt_spec_opt c dest _ (cdr wf) se) m =
        runStateT (spec (iso_f (car wf) se)) m /\
      (* synth_stmt_side_cond c dest _ (cdr wf) se m /\ *)
      synth_stmt_ret_cond c dest _ (cdr wf) se m
*)
    end %alist.

  Lemma synth_stmt_RC_ret_cond r (c: cmd_constr r) dest tmp
        (wf: synth_stmt_wellformed c dest tmp) (se: spec_env_t tmp) m:
      synth_stmt_RC dest tmp se m c wf ->
      synth_stmt_ret_cond c dest tmp wf se m.
  Proof.
    intros RC.
    induction RC; simpl; auto.
    intros m' opt_c1.
    exec2run opt_c1 as v.
    eauto.
  Qed.

  Lemma map2_synth_expr_spec_satisfies_ft_cond {tmp argt}
    {arg : expr_constr_list argt}
    (argc : expr_constr_prf_conj arg)
    (wf : fold_synth_expr_wellformed tmp arg)
    (se : spec_env_t tmp) :
      fold_expr_constr_list_cond arg wf se ->
      senv_cond se ->
      ht_list_ft_cond (map2_synth_expr_spec arg se wf).
  Proof.
    intros cd se_cond.
    induction arg.
    - constructor.
    - simpl in argc, wf, cd |- *.
      destruct wf as [ wf arg_wf ].
      destruct argc as [ argc [ ht ec ] ].
      destruct cd as [ ecd cd ].
      rewrite oand1_distr in ecd.
      destruct ecd as [ ecd e_valid ].
      constructor;
        [ apply (synth_expr_spec_satisfies_ft_cond me)
        | apply IHarg ];
        assumption.
  Qed.

  Lemma map2_synth_expr_spec_satisfies_valid_ft_cond {tmp argt}
    {arg : expr_constr_list argt}
    (argc : expr_constr_prf_conj arg)
    (wf : fold_synth_expr_wellformed tmp arg)
    (se : spec_env_t tmp) :
      fold_expr_constr_list_cond arg wf se ->
      senv_cond se ->
      ht_list_valid_ft_cond (map2_synth_expr_spec arg se wf).
  Proof.
    intros cd se_cond.
    induction arg.
    - constructor.
    - simpl in argc, wf, cd |- *.
      destruct wf as [ wf arg_wf ].
      destruct argc as [ argc [ ht ec ] ].
      destruct cd as [ ecd cd ].
      rewrite oand1_distr in ecd.
      destruct ecd as [ ecd e_valid ].
      constructor;
        [ apply ht_valid_ft_ocond_same;
          rewrite OProp1map1 in e_valid by tauto
        | apply IHarg ];
        assumption.
  Qed.

Lemma synth_stmt_spec_opt_pure {is_realizing returns htr}(c : cmd_constr returns) :
    synth_stmt_pure c ~~~ true ->
    cmd_constr_prf is_realizing returns htr c ->
    forall dest tmp wf se d v' d',
      runStateT (synth_stmt_spec_opt c dest tmp wf se) d = ret (v', d') ->
      d = d'.
Proof.
  intros X cc.
  assert (is_pure := iso_g X (F := fun b => b = true) eq_refl); clear X.
  intros dest tmp wf se d v' d' Hrun.
  revert dest tmp wf se d v' d' Hrun.
  induction cc; try discriminate is_pure; intros; simpl in Hrun.
  - (* CCskip *)
    apply ret_osT in Hrun. apply Hrun.
  - (* CClet *)
    split_run Hrun as (v_ & d_ & Hrun1 & Hrun2).
    simpl in is_pure. apply andb_prop in is_pure.
    transitivity d_.
    + eapply IHcc1. apply is_pure. exact Hrun1.
    + eapply IHcc2. apply is_pure. exact Hrun2.
  - (* CCload *)
    apply gets_osT in Hrun. apply Hrun.
  - (* CCsequence *)
    split_run Hrun as (v_ & d_ & Hrun1 & Hrun2).
    simpl in is_pure. apply andb_prop in is_pure.
    transitivity d_.
    + eapply IHcc1. apply is_pure. exact Hrun1.
    + eapply IHcc2. apply is_pure. exact Hrun2.
  - (* CCifthenelse *)
    simpl in is_pure. apply andb_prop in is_pure.
    match type of Hrun with
      context [if ?cond then _ else _] => destruct cond
    end.
    + eapply IHcc1. apply is_pure. exact Hrun.
    + eapply IHcc2. apply is_pure. exact Hrun.
  - (* CCfirst *)
    simpl in is_pure. apply andb_prop in is_pure.
    destruct is_pure as [is_pure1 is_pure3].
    apply andb_prop in is_pure1.
    destruct is_pure1 as [is_pure1 is_pure2].
    split_run Hrun as (v1 & d1 & Hrun1 & Hrun2).
    split_run Hrun2 as (v2 & d2 & Hrun2 & Hrun3).
    transitivity d1.
    { apply get_osT in Hrun1. apply Hrun1. }
    transitivity d2.
    { apply first_map_pure in Hrun2. exact Hrun2. }
    clear Hrun2.
    dest_first Hrun3.
    + eapply IHcc2. apply is_pure2. exact Hrun3.
    + eapply IHcc3. apply is_pure3. exact Hrun3.
  - (* CCfold *)
    eapply fold_pure.
    2: exact Hrun.
    intros m v_ m_ se_.
    apply IHcc. simpl in wf. apply wf. reflexivity.
  - (* CCcall *) (* TODO *)
    eapply PRIMis_pure. apply is_pure.
    apply (runStateT_execStateT Hrun).
  (* - CCcall_ext (* TODO *)
    eapply PRIMis_pure. apply is_pure.
    apply (runStateT_execStateT Hrun). *)
  - (* CCyield *)
    apply ret_osT in Hrun. apply Hrun.
  - (* CCconstr *)
    split_run Hrun as (v_ & d_ & Hrun1 & Hrun2).
    apply guard_osT in Hrun2.
    transitivity d_.
    + eapply IHcc. apply is_pure. exact Hrun1.
    + apply Hrun2.
  - (* CCassert *)
    split_run Hrun as (v_ & d_ & Hrun1 & Hrun2).
    apply guard_osT in Hrun2.
    transitivity d_.
    + eapply IHcc. apply is_pure. exact Hrun1.
    + apply Hrun2.
  (* - (* CCghost *)
    eapply IHcc. apply is_pure. exact Hrun. *)
  - (* CCPdummy *)
    admit.
  - (* CCpanic *)
    admit.
Admitted.
  Lemma set_senv_cond tmp (se : SpecTree.t tmp) i htp v:
    ht_ft_cond v ->
    senv_cond se ->
    senv_cond (SpecTree.set i htp v se).
  Proof.
    unfold senv_cond.
    intros ft_v se_cond.
    intros i' tp hti eq.
    generalize eq.
    destruct (peq i' i) as [i_eq | i_ne].
    - (* i' = i *)
      subst i'. rewrite AList.gss in eq.
      intros eq'.
      rewrite (SpecTree.gess' i _ se _ eq).
      generalize eq.
      set (F x := forall eq0 : Some htp ~~~ Some x, ht_ft_cond (eq0 SpecTree.OptionalType v)).
      apply (some_injective_isomorphism eq F). unfold F.
      intros eq_htp.
      rewrite <- functor_identity_paramatricity. exact ft_v.
    - (* i' != i *)
      rewrite AList.gso in eq; [|assumption].
      intros eq'.
      rewrite (SpecTree.geso i' i _  _ eq' eq i_ne).
      apply se_cond.
  Qed.

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
Proof.
  intros tmp' f s0 start steps s steps_range Hrun.
  refine (Z_nonneg_peano_ind _ _ _ _).
  - (* n = 0 *)
    intros n_range.
    rewrite Ziteri_base by omega.
    exists s0.
    split. reflexivity.
    replace (start + 0) with start by omega.
    replace (steps - 0) with steps by omega.
    exact Hrun.
  - (* n > 0 *)
    intros z z_nonneg Hind Succ_z_range.
    assert (z_range: 0 <= z <= steps) by omega.
    specialize (Hind z_range).
    destruct Hind as (s' & opt_z & opt_rest).
    set (rest := (steps - (Z.succ z))).
    replace (steps - z) with (Z.succ rest) in opt_rest by (unfold rest; omega).
    apply for_cut_opt' in opt_rest; [|unfold rest; omega].
    destruct opt_rest as (s'0 & opt_succ_z & opt_rest).
    exists s'0.
    split.
    + rewrite Ziteri_succ by omega.
      unfold for_step_opt at 1.
      eapply bind_osT. exact opt_z.
      exact opt_succ_z.
    + replace (start + Z.succ z) with (start + z + 1) by omega.
      exact opt_rest.
  - (* n < 0 *)
    intros p p_range.
    pose (Pos2Z.neg_is_neg p) as n_neg.
    omega.
Qed.

  Opaque AList.get.
  (* This is the statement of correctness. The following lemmas will prove it for each synthesis function.

     Note that the statement is a little tricky, because it is parametrized by a type of wellformedness condition.
     In the cases below, wellformed, spec_cond, ... will be instantiated to synth_stmt_wellformed, synth_stmt_spec_cond, etc.
     (See Theorem synth_stmt_spec_correct for an example.)
  *)
  (* Variable m0 : MemLow. *)
Definition temp_env : Type := PTree.t val.

  Lemma opt_bool_dec_t__f_none (x:option bool):
    {x = Some true} + {x = Some false \/ x = None}.
  Proof.
    destruct x as [b|]; [destruct b|]; tauto.
  Qed.

  Lemma Z2Nat_nonpos_zero:
    forall n, n <= 0%Z -> Z.to_nat n = 0%nat.
  Proof.
    destruct n; [reflexivity| |reflexivity].
    specialize (Pos2Z.is_pos p); omega.
  Qed.

  Lemma min_ex_empty P lo hi Pn:
    lo >= hi ->
    exists no_n, LangDef.min_ex P lo hi Pn = inright no_n.
  Proof.
    intros range.
    destruct (LangDef.min_ex P lo hi Pn) as [(n & n_range) | no_n].
    - omega.
    - exists no_n. reflexivity.
  Qed.

  Lemma min_ex_lo_satisfy (P:Z->Prop) lo hi Pdec
        (lo_bound: lo <= lo < hi)
        (lo_pass: P lo):
    exists Q, LangDef.min_ex P lo hi Pdec = inleft (exist _ lo Q).
  Proof.
    destruct (LangDef.min_ex P lo hi Pdec) as [[n (n_range & Pn & n_least)] | no_n].
    - assert (n = lo).
      { destruct (Z_dec n lo) as [[Hlt | Hgt] | Heq].
        - omega.
        - specialize (n_least lo). assert (~ P lo) by (apply n_least; omega).
          contradiction.
        - assumption. }
      subst n.
      eexists. reflexivity.
    - exfalso. specialize (no_n _ lo_bound). contradiction.
  Qed.

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
  Proof.
    intros fail_lo.
    destruct (LangDef.min_ex P lo hi Pdec) as [(n & n_range & Pn & n_least)|no_n]; destruct (LangDef.min_ex P (Z.succ lo) hi Pdec) as [(m & m_range & Pm & m_least) | no_m]; [|exfalso|exfalso|].
    - (* Ql n = Ql m *)
      destruct (Z_dec n m) as [[Hlt | Hgt] | Heq].
      + (* n < m *)
        assert (Z.succ lo <= n).
        { destruct (Z_dec lo n) as [[H'lt | H'gt] | H'eq]; [omega|omega|].
          subst n. contradiction. }
        specialize (m_least n (conj H Hlt)). contradiction.
      + (* n > m *)
        exfalso. refine (n_least m _ Pm).
        omega.
      + (* n = m *)
        f_equal. assumption.
    - (* Ql n = Qr *)
      destruct (Z_dec lo n) as [[Hlt | Hgt] | Heq].
      + (* lo < n *)
        refine (no_m n _ Pn). omega.
      + (* lo > n *)
        omega.
      + (* lo = n *)
        subst n. contradiction.
    - (* Qr = Ql m *)
      refine (no_n m _ Pm). omega.
    - (* Qr = Qr *)
      reflexivity.
  Qed.

  Definition synth_stmt_veri_cond
      {returns}(c : cmd_constr returns) dest tmp wf se mh :=
    (* synth_stmt_side_cond c dest tmp wf se mh /\ *)
    synth_stmt_cond c dest tmp wf se mh.

End STMT_FUNC.
