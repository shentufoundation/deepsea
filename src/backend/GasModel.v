
Require Import cclib.Coqlib.
Require Import backend.Expressions.ExpStacked.
Require Import backend.Expressions.ExpMiniC.
Require Import backend.Cop.
Require Import backend.Ctypes.
Require Import backend.MachineModel.
Require Import Environments.Globalenvs.

Local Open Scope nat_scope.

Ltac REORDER order := assert order as NEWORDER by (intros; omega); rewrite NEWORDER; clear NEWORDER.
Ltac REORDERIN order hyp := assert order as NEWORDER by (intros; omega); rewrite NEWORDER in hyp; clear NEWORDER.

Opaque Peano.plus.

(* constants from yellow paper *)
Definition g_base : nat := 2.
Definition g_verylow : nat := 3.
Definition g_low : nat := 5.
Definition g_mid : nat := 8.
Definition g_high : nat := 10.
Definition g_jumpdest : nat := 1.

Definition g_sload : nat := 200.
Definition g_sset : nat := 20000.  (* upper bound SSTORE by assuming it's always zero -> nonzero *)


(** Derived constants *)

Definition gas_mload : nat := g_verylow.
Definition gas_push : nat := g_verylow.
Definition gas_pop : nat := g_base.
Definition gas_calldataload : nat := g_verylow.
Definition gas_codecopy : nat := g_verylow.
Definition gas_dup : nat := g_verylow.
Definition gas_swap : nat := g_verylow.

(* jumps and labels are inexpensive,
and it's hard to keep track at all levels of the number of each required at each level *)
Definition gas_label : nat := g_jumpdest.
Definition gas_jump : nat := g_mid.
Definition gas_jumpi : nat := g_high.
Definition gas_jump_arg : nat := gas_push + gas_jump.
Definition gas_jumpi_arg : nat := gas_push + gas_jumpi.
(* either fallthrough or jump or conditional jump *)
Definition gas_branch : nat := Nat.max gas_label (Nat.max gas_jump_arg gas_jumpi_arg).


Remark gas_jump_le_branch: gas_jump_arg <= gas_branch.
Proof. unfold gas_branch. apply Nat.le_trans with (Nat.max gas_jump_arg gas_jumpi_arg).
apply Nat.le_max_l. apply Nat.le_max_r.
Qed.

Remark gas_jumpi_le_branch: gas_jumpi_arg <= gas_branch.
Proof. unfold gas_branch. apply Nat.le_trans with (Nat.max gas_jump_arg gas_jumpi_arg).
apply Nat.le_max_r. apply Nat.le_max_r.
Qed.

Remark gas_label_le_branch: gas_label <= gas_branch.
Proof. unfold gas_branch. apply Nat.le_max_l.
Qed.

Fixpoint gas_branches (count: nat) : nat :=
  match count with 0 => 0 | S n => gas_branch + gas_branches n end.

Lemma gas_branches_split: forall b1 b2,
  gas_branches (b1 + b2) = gas_branches b1 + gas_branches b2.
Proof. induction b1; simpl; intros; repeat rewrite Nat.add_0_l.
auto. rewrite Nat.add_succ_l. simpl. rewrite IHb1. omega.
Qed.

Definition gas_eval_builtin0 (b: builtin0) : nat :=
  match b with
  | Baddress => g_base
  | Borigin => g_base
  | Bcaller => g_base
  | Bcallvalue => g_base
  | Bcoinbase => g_base
  | Btimestamp => g_base
  | Bnumber => g_base
  | Bchainid => g_base
  | Bselfbalance => g_low
  end.

Definition gas_eval_builtin1 (b: builtin1) : nat :=
  match b with
  (* these are from the yellow paper *)
  | Bbalance => 400
  | Bblockhash => 20
  end.

Definition gas_sha_1 : nat := 30 + 6.  (* 30 flat rate plus 6 per word *)
Definition gas_sha_2 : nat := 30 + 12. (* 30 flat rate plus 6 per word *)

Definition gas_binop (o: binary_operation) : nat :=
  match o with
  | Oadd => g_verylow
  | Osub => g_verylow
  | Omul => g_low
  | Osha_2 => gas_sha_2
  | _ => g_verylow
  end.

Definition gas_unop (o: unary_operation) : nat :=
  match o with
  | Onotbool => g_verylow
  | Onotint => g_verylow
  | Oneg => gas_push + gas_binop Osub
  | Osha_1 => gas_sha_1
  end.

(* TODO: look up the actual values *)

(* deref determines whether the expression is automatically dereferenced *)
Fixpoint gas_eval_expr (deref: bool) (e: expr) : nat :=
  match e with
  | Econst_int _ _ => 0
  | Econst_int256 _ _ => gas_push
  | Evar _ _ => gas_push + match deref with true => g_sload | false => 0 end
  | Eglob _ _ => gas_push + match deref with true => g_sload | false => 0 end
  | Etempvar _ _ => gas_dup
  | Eaddr _ _ => 0
  | Ederef e _ => (gas_eval_expr deref e) + g_sload
  | Eunop o e _ => (gas_eval_expr deref e) + gas_unop o
  | Ebinop o e1 e2 _ => (gas_eval_expr deref e1) + (gas_eval_expr deref e2) + gas_binop o
  | Efield e _ _ => (gas_eval_expr deref e) + gas_push + gas_sha_2 + g_sload
  | Eindex e1 e2 _ => (gas_eval_expr deref e1) + (gas_eval_expr deref e2) + gas_sha_2 + g_sload
  | Ecall0 b _ => gas_eval_builtin0 b
  | Ecall1 b e _ => gas_eval_expr deref e + gas_eval_builtin1 b
  end.

Fixpoint gas_eval_exprs (deref: bool) (args: list expr) : nat :=
  match args with
  | nil => 0
  | arg::rest => (gas_eval_expr deref arg) + (gas_eval_exprs deref rest)
  end.

Definition gas_eval_optexpr (deref: bool) (optexpr: option expr) : nat :=
  match optexpr with None => gas_push | Some e => gas_eval_expr deref e end.

Definition gas_assign (deref: bool) (lv: expr) (rv: expr) (b: nat) : nat :=
  (gas_eval_expr deref lv) + (gas_eval_expr deref rv) + g_sset + (gas_branches b).

Definition gas_set_stacked : nat := gas_swap + gas_pop.

Definition gas_set (deref: bool) (rv: expr) (b: nat) : nat :=
  (gas_eval_expr deref rv) + gas_set_stacked + (gas_branches b).

(* TODO: find if gas depends on amount of args and other stuff *)
Definition g_call : nat := 700.
Definition g_callvalue : nat := 9000.

Definition gas_transfer_base : nat :=
  g_call + g_callvalue.

Definition gas_callmethod_base : nat :=
  g_call + g_callvalue.

Definition gas_transfer (deref: bool) (a: expr) (v: expr) (b: nat) : nat :=
  gas_jumpi + gas_push + gas_unop Onotbool + gas_transfer_base + gas_push + gas_push + gas_push + gas_push + gas_push +
    (gas_eval_expr deref a) + (gas_eval_expr deref v) + (gas_branches b).

Definition gas_log_base (ntopics nargs : nat) : nat :=
  nargs * (2 * g_verylow)   (* Write arguments to memory. This is not quite right, because MSTORE can take more than verylow if it extends memory. *)
  + 2 * g_verylow           (* push memory base/size *)
  + 375 * (1 + ntopics).    (* the LOG0/.../LOG4 instruction *)


Definition gas_log (deref: bool) (topics args : list expr) (b: nat) : nat :=
  gas_log_base (length topics) (length args)
  + (gas_eval_exprs deref topics)
  + (gas_eval_exprs deref args)
  + (gas_branches b).

Definition gas_saveretval : nat := gas_set_stacked.

Definition gas_saveretval' (b: nat) : nat := gas_saveretval + gas_branches b.

Fixpoint gas_saveretvals (c: nat) : nat :=
  match c with 0 => 0 | S n => gas_saveretval + gas_saveretvals n end.

(* read from memory *)
Definition gas_extractretval : nat := 30.
Fixpoint gas_extractretvals (c: nat) : nat :=
  match c with 0 => 0 | S n => gas_extractretval + gas_extractretvals n end.

(* put in memory *)
Definition gas_stasharg : nat := 22.
Fixpoint gas_stashargs (c: nat) : nat :=
  match c with 0 => 0 | S n => gas_stasharg + gas_stashargs n end.

Definition gas_return_var (deref: bool) (rv: positive) (b: nat) : nat :=
  (gas_eval_expr deref (Etempvar rv Tvoid)) + (gas_branches b).

Definition gas_return_none (deref: bool) (b: nat) : nat :=
  (gas_branches b).

Definition gas_pops (garbage: nat) : nat :=
  garbage * gas_pop.

Definition gas_cleanup (garbage: nat) : nat :=
  match garbage with
  | O => O
  | _ => gas_pops garbage + gas_swap
  end.

Definition gas_done_simple (garbage: nat) : nat :=
  gas_cleanup garbage + 100.

Definition gas_done (garbage: nat) (b: nat) : nat :=
  gas_done_simple garbage + gas_branches b.

Definition gas_return_done (deref: bool) (rv: option positive) (garbage: nat) (b: nat) : nat :=
  match rv with
  | Some rv => gas_return_var deref rv 0 + gas_done garbage 0 + gas_branches b
  | None => gas_return_none deref 0 + gas_done garbage 0 + gas_branches b
  end.

(*Lemma gas_return_done_split: forall d r g b1 b2,
  gas_return_done d r g (b1 + b2) = gas_done g b1 + gas_return d r b2.
Proof.
intros. unfold gas_return_done. unfold gas_done. unfold gas_return.
rewrite gas_branches_split. simpl. omega.
Qed.*)

Definition gas_callmethod_stacked (rv_count: nat) (args: nat) : nat :=
  gas_callmethod_base
  + (gas_extractretvals rv_count)
  + (gas_stashargs args).

Definition gas_callmethod (deref: bool) (a: expr) (rv_count: nat) (v: expr) (args: list expr) (b: nat) : nat :=
  gas_push + gas_push + gas_push + gas_push + gas_push + gas_push + gas_jumpi + gas_unop Onotbool
  + gas_eval_expr deref a
  + gas_saveretvals rv_count
  + gas_eval_expr deref v
  + gas_eval_exprs deref args
  + gas_callmethod_stacked rv_count (length args)
  + gas_branches b.

Definition gas_call (deref: bool) (args: list expr) (b: nat) : nat :=
  (gas_eval_exprs deref args) + gas_jump_arg + gas_push + (gas_branches b).

(* pushing empty temps onto the stack *)
Definition gas_temps (temps: nat) : nat := gas_push * temps.

(* Push args onto the stack. For internal functions, the args are
  already on the stack. We reserve the gas because methods and the
  constructor need it, and it's difficult to know in the semantics
  whether we're entering a method. So there are three cases:
  - Internal function, zero gas.
  - Method, gas_calldataload + gas_push.
  - Constructor, 3*gas_push + gas_codecopy + gas_mload.
  gas_arg is an upper bound of all of them.
 *)

Definition gas_arg_method :nat := gas_calldataload + gas_push.
Definition gas_arg_constr :nat := 3*gas_push + gas_codecopy + gas_mload.

Definition gas_arg : nat := gas_arg_constr.
Definition gas_args (args: nat) : nat := gas_arg * args.

Remark gas_arg_bound_method: gas_arg >= gas_arg_method.
Proof. compute. omega. Qed.
Remark gas_arg_bound_constr: gas_arg >= gas_arg_constr.
Proof. compute. omega. Qed.

Definition gas_intro (temps: nat) (args: nat) : nat :=
  gas_temps temps + gas_args args.

Definition gas_callenter (temps: nat) (args: nat) (b: nat) : nat :=
  (gas_intro temps args) + (gas_branches b).

Definition gas_eval_cond (deref: bool) (e: expr) (b: nat) : nat :=
  (gas_eval_expr deref e) + (gas_branches b).

Definition gas_branching (f: nat -> nat) : Prop :=
  forall x, f x = f 0 + gas_branches x.

Ltac gas_branching_auto FUN := unfold gas_branching; intros; simpl;
  unfold FUN; simpl; repeat rewrite Nat.add_0_r; auto.

Remark transfer_branching: forall a b c, gas_branching (gas_transfer a b c).
gas_branching_auto gas_transfer. Qed.

Remark log_branching: forall a b c, gas_branching (gas_log a b c).
gas_branching_auto gas_log. Qed.

Remark callmethod_branching: forall a b c d e, gas_branching (gas_callmethod a b c d e).
gas_branching_auto gas_callmethod. Qed.

Remark callenter_branching: forall a b, gas_branching (gas_callenter a b).
gas_branching_auto gas_callenter. Qed.

Remark assign_branching: forall a b c, gas_branching (gas_assign a b c).
gas_branching_auto gas_assign. Qed.

Remark set_branching: forall a b, gas_branching (gas_set a b).
gas_branching_auto gas_set. Qed.

Remark call_branching: forall a b, gas_branching (gas_call a b).
gas_branching_auto gas_call. Qed.

Remark cond_branching: forall a b, gas_branching (gas_eval_cond a b).
gas_branching_auto gas_eval_cond. Qed.

(*Remark return_branching: forall a b, gas_branching (gas_return a b).
gas_branching_auto gas_return. Qed.*)

Remark done_branching: forall a, gas_branching (gas_done a).
gas_branching_auto gas_done. Qed.

Remark retval_branching: gas_branching gas_saveretval'.
gas_branching_auto gas_saveretval'. Qed.


Lemma succ_pred: forall x, 0 < x -> S (Nat.pred x) = x.
Proof.
destruct x; intros.
omega.
simpl. reflexivity.
Qed.

Lemma gas_add_sub: forall c g g',
  g + c <= g' ->
  exists g'', g' = g'' + c /\ g <= g''.
Proof.
induction c; simpl; intros.
exists g'. rewrite Nat.add_0_r in H. split; auto.
rewrite Nat.add_comm in H. rewrite Nat.add_succ_l in H.
rewrite Nat.add_comm in H. rewrite <- Nat.add_succ_l in H.
destruct (IHc (S g) g' H) as (g_0 & g_0_eq & g_0_lt).
exists (pred g_0).
assert (S (Init.Nat.pred g_0) = g_0).
{ rewrite succ_pred; auto. unfold lt.
  apply Nat.le_trans with (S g); auto. omega. }
split.

rewrite Nat.add_comm. rewrite Nat.add_succ_l.
rewrite Nat.add_comm. rewrite <- Nat.add_succ_l.
rewrite H0; auto.
apply le_S_n. rewrite H0. auto.
Qed.

Lemma le_add: forall g c,
  g <= g + c.
Proof. induction c; intros.
omega. apply Nat.le_trans with (g + c); auto.
rewrite Nat.add_succ_r. omega.
Qed.

Lemma le_pad: forall g c c',
  c' <= c -> c' <= c + g.
Proof. intros. apply Nat.le_trans with c; auto. apply le_add.
Qed.

Lemma le_diff: forall g c c',
  c' <= c -> g + c' <= g + c.
Proof. induction g; intros.
repeat rewrite Nat.add_0_l. assumption.
repeat rewrite Nat.add_succ_l.
apply le_n_S. apply IHg. assumption.
Qed.

Lemma le_diff2: forall g c c',
  c' <= c -> c' + g <= c + g.
Proof. intros.
rewrite Nat.add_comm. rewrite (Nat.add_comm c g).
apply le_diff. auto.
Qed.

Lemma le_pad': forall g c c',
  c' + g <= c -> c' <= c.
Proof. intros. apply Nat.le_trans with (c' + g); auto. apply le_pad. auto.
Qed.

Lemma le_add2: forall g g' c c',
  c' <= c -> g' <= g -> c' + g' <= c + g.
Proof. intros.
apply Nat.le_trans with (c' + g).
apply le_diff; auto.
apply le_diff2; auto.
Qed.

Lemma gas_partial_add_sub: forall c g g',
  g + c <= g' ->
  forall c',
  c' <= c ->
  exists g'', g' = g'' + c' /\ g <= g''.
Proof.
intros. apply gas_add_sub. apply Nat.le_trans with (g + c); auto.
apply le_diff. auto.
Qed.



Lemma gas_branch_label: forall g g',
  g + gas_branch <= g' ->
  exists g'', g' = g'' + gas_label /\ g <= g''.
Proof.
intros. eapply gas_partial_add_sub; eauto. apply gas_label_le_branch.
Qed.

Lemma gas_branch_jump: forall g g',
  g + gas_branch <= g' ->
  exists g'', g' = g'' + gas_jump_arg /\ g <= g''.
Proof.
intros. eapply gas_partial_add_sub; eauto. apply gas_jump_le_branch.
Qed.

Lemma gas_branch_jumpi: forall g g',
  g + gas_branch <= g' ->
  exists g'', g' = g'' + gas_jumpi_arg /\ g <= g''.
Proof.
intros. eapply gas_partial_add_sub; eauto. (* apply gas_jumpi_le_branch. *)
Qed.


Ltac same_gas_used GAS g' := apply gas_add_sub in GAS; destruct GAS as (g'' & g_eq & GAS); subst g'.


Lemma gas_split_branches: forall g f,
  gas_branching f ->
  g + f 2 = g + gas_branch + f 0 + gas_branch.
Proof.
unfold gas_branching. intros. assert (A := H 2).
rewrite A.
simpl.
omega.
Qed.

Lemma gas_extract_branch: forall g f,
  gas_branching f ->
  g + f 1 = g + f 0 + gas_branch.
Proof. unfold gas_branching. intros. assert (A := H 1).
rewrite A; simpl; omega.
Qed.

Ltac gas_use_2 GAS g' g_0 :=
  same_gas_used GAS g'; rename g'' into g_0;
  same_gas_used GAS g_0.

Ltac gas_use_3 GAS g' g_0 g_1 :=
  same_gas_used GAS g'; rename g'' into g_0;
  same_gas_used GAS g_0; rename g'' into g_1;
  same_gas_used GAS g_1.

Ltac gas_use_4 GAS g' g_0 g_1 g_2 :=
  same_gas_used GAS g'; rename g'' into g_0;
  same_gas_used GAS g_0; rename g'' into g_1;
  same_gas_used GAS g_1; rename g'' into g_2;
  same_gas_used GAS g_2.

(* Stacked expression *)

Definition gas_eval_stacked_expr (e: ExpStacked.expr) : nat :=
  match e with
  | ExpStacked.Econst_int256 _ => gas_push
  | ExpStacked.Eglob _ => gas_push
  | ExpStacked.Etempvar _ => gas_dup
  | ExpStacked.Esload => g_sload
  | ExpStacked.Emload => g_sload
  | ExpStacked.Eunop o => gas_unop o
  | ExpStacked.Ebinop o _ => gas_binop o
  | ExpStacked.Ecall0 b => gas_eval_builtin0 b
  | ExpStacked.Ecall1 b => gas_eval_builtin1 b
  end.

Fixpoint gas_eval_stacked_exprs (args: list ExpStacked.expr) : nat :=
  match args with
  | nil => 0
  | arg::rest => (gas_eval_stacked_expr arg) + (gas_eval_stacked_exprs rest)
  end.

Definition gas_eval_stacked_optexpr (optexpr: option ExpStacked.expr) : nat :=
  match optexpr with None => 0 | Some e => gas_eval_stacked_expr e end.

Definition gas_assign_stacked : nat := g_sset.

Definition gas_eval_stacked_cond (cond: ExpStacked.expr) (b: nat) : nat :=
  (gas_eval_stacked_expr cond) + (gas_branches b).

(* Method multiplexer *)

(* TODO: make it depend on the number of methods *)
Definition gas_multiplex (methods: nat) : nat := 500.

