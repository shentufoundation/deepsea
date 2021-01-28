Require Import backend.MachineModel.
Require Import backend.Environments.AllEnvironments.
Require Import backend.Environments.ExtEnv.
Require Import backend.TempModel.
Require Import backend.AST.
Require Import cclib.Coqlib.
Require Import backend.Expressions.ExpMiniC.
Require Import cclib.Maps.
Require Import cclib.Integers.
Require Import backend.Cop.
Require Import backend.Ctypes.
Require Import backend.Options.
Require Import backend.Values.HighValues.
Require Import backend.AbstractData.

Section SEMANTICS.

  Variable adata: Type.
  
  Variable ctx: int256.

  (* machine environment *)
  Variable me: machine_env adata.

  (* storage/memory environment *)
  Variable ee: ext_env.

  Variable le: temp_env.

  (* Calculate the size of the offset we need, in words. *)
  (*Definition sem_field_offset (ty: type) (field: ident) : option int256 :=
    match ty with
    | Tstruct _ fld =>
    unpack offset, _ <- struct_field fld field ;; Some (Int256.repr (Z.of_nat offset))
    | _ => None
    end.*)

  (* rvalues always evaluate to ints.  *)
  Inductive eval_rvalue: expr -> val -> Prop :=
  | eval_Econst_int256: forall i ty,
      eval_rvalue (Econst_int256 i ty) (Vint i)
  | eval_Etempvar: forall id val ty,
      PTree.get id le = Some val ->
      eval_rvalue (Etempvar id ty) val
  | eval_Elvalue: forall e lv v,
      (* This case is tricky, it relies on the set of things that eval_lvalue
         can evaluate is disjoint from the other ones.  
         The effect is that any lexpr which is embedded in an rexpr implicitly involves an lookup in ee. *)      
      eval_lvalue e lv ->
      read lv ee = Some v ->
      eval_rvalue e v
  | eval_Eaddr: forall e lv ty,
      eval_lvalue e lv ->
      eval_rvalue (Eaddr e ty) (Vptr (LVeid lv))
  | eval_Eunop: forall op a ty v1 v,
      eval_rvalue a v1 ->
      sem_unary_operation op v1 Tvoid = Some v ->
      eval_rvalue (Eunop op a ty) v
  | eval_Ebinop: forall op a1 a2 ty1 ty2 ty  v1 v2 v,
      eval_rvalue a1 v1 ->
      eval_rvalue a2 v2 ->
      (* For the ethereum backend we mostly ignore types, so to be compatible
         with the middle end it is useful to be able to put in arbitrary ty1/ty2. *)
      sem_binary_operation op v1 ty1 v2 ty2 = Some v ->
      eval_rvalue (Ebinop op a1 a2 ty) v
  | eval_Ecall0: forall b ty v,
      (me_query me) (Qcall0 b) = v ->
      eval_rvalue (Ecall0 b ty) v
  | eval_Ecall1: forall b a av ty v,
      eval_rvalue a av ->
      (me_query me) (Qcall1 b av) = v ->
      eval_rvalue (Ecall1 b a ty) v

(* lvalues always evaluate to extended identifiers.
   The `val` type is mutually defined with an `lval` type,
   but that is not actually used until in later compiler phases *)
with eval_lvalue: expr -> ident_ext -> Prop :=
| eval_Evar: forall id ty lv,
    var ctx id ee = Some lv ->
    eval_lvalue (Evar id ty) lv
| eval_Eglob: forall id ty lv,
    glob id ee = Some lv ->
    eval_lvalue (Eglob id ty) lv
| eval_Efield: forall e id ty base lv,
    eval_lvalue e base ->
    access_field base (typeof e) id = Some lv ->
    eval_lvalue (Efield e id ty) lv
| eval_Eindex: forall e1 e2 ty base ind lv,
    eval_lvalue e1 base ->
    eval_rvalue e2 ind ->
    index_array base (typeof e1) ind = Some lv ->
    eval_lvalue (Eindex e1 e2 ty) lv
| eval_Ederef: forall e ty v lv,
    eval_rvalue e v ->
    deref v = Some lv ->
    eval_lvalue (Ederef e ty) lv
.

Inductive eval_rvalues: list expr -> list val -> Prop :=
  | eval_Enil:
    eval_rvalues nil nil
  | eval_Econs: forall rv val rvs vals,
    eval_rvalue rv val ->
    eval_rvalues rvs vals ->
    eval_rvalues (rv :: rvs) (val :: vals).

Remark eval_rvalues_len: forall es vs,
  eval_rvalues es vs -> length es = length vs.
Proof. induction es; intros; inv H. auto. simpl.
replace (length vals) with (length es). auto. apply IHes; auto.
Qed.

(* evaluates to Vunit for None. *)
Inductive eval_optvalue: option expr -> val -> Prop :=
  | eval_Enone:
    eval_optvalue None Vunit
  | eval_Esome: forall e v,
    eval_rvalue e v ->
    eval_optvalue (Some e) v.

End SEMANTICS.

Arguments eval_lvalue {adata}.
Arguments eval_rvalue {adata}.
Arguments eval_rvalues {adata}.
Arguments eval_optvalue {adata}.
