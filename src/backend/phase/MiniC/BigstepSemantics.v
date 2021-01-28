Require Import backend.TempModel.
Require Import backend.AST.
Require Import cclib.Coqlib.
Require Import backend.Values.HighValues.
Require Import cclib.Maps.
Require Import cclib.Integers.
Require Import backend.Cop.
Require Import backend.Ctypes.
Require Import backend.Options.
Require Import backend.MemoryModel.
Require Import backend.MachineModel.
Require Import backend.Environments.Globalenvs.
Require Import backend.Environments.AllEnvironments.
Require Import backend.Environments.ExtEnv.
Require Import backend.AbstractData.
Require Import backend.Events.
Require Import backend.GasModel.
Require Import backend.Statements.StmtMiniC.
Require Import backend.Expressions.SemanticsMiniC.
Require Import backend.phase.MiniC.Semantics.

Section WITH_DATA.
  Context (D : compatdata).
                                   
Section BIGSTEP.

Variable ge: genv.
Variable me: machine_env (cdata_type D).

(** ** Big-step semantics for terminating statements and functions *)

Inductive outcome: Type :=
| Out_break: outcome                 (* terminated by [break] *)
| Out_normal: outcome                (* terminated normally *)
| Out_return: val  -> outcome.       (* terminated by [return] *)

Inductive out_normal : outcome -> Prop :=
| Out_normal_N: out_normal Out_normal.

Inductive out_break_or_return : outcome -> outcome -> Prop :=
| Out_break_or_return_B: out_break_or_return Out_break Out_normal
| Out_break_or_return_R: forall ov,
    out_break_or_return (Out_return ov) (Out_return ov).

Definition outcome_switch (out: outcome) : outcome :=
  match out with
  | Out_break => Out_normal
  | o => o
  end.  

(* the second log and the nat is the *new* events, and the *additional* gas used. *)
(* Inductive exec_stmt: int256 -> temp_env -> ext_env -> log -> statement -> int256 -> temp_env -> ext_env -> log -> nat -> outcome -> Prop := *)
(* | exec_Sskip: forall ctx le m lg, *)
(*     exec_stmt ctx le m lg Sskip *)
(*               ctx le m nil 0 Out_normal *)
(* | exec_Sassign: forall ctx le se lg lv lv_ident rv rv_int, *)
(*     eval_lvalue ctx me se le lv lv_ident -> *)
(*     eval_rvalue ctx me se le rv rv_int -> *)
(*     True *)
(*     (* exec_stmt ctx le se lg (Sassign lv rv) *) *)
(*     (*           ctx le (IdentExtMap.set lv_ident rv_int se) nil (gas_assign true lv rv 2) Out_normal *) *)
(* | exec_Sset: forall id rv rv_int le se prev lg, *)
(*     PTree.get id le = Some prev -> *)
(*     eval_rvalue me se le rv rv_int -> *)
(*     True *)
(*     (* exec_stmt le se lg (Sset id rv) *) *)
(*     (*           (PTree.set id rv_int le) se nil (gas_set true rv 2) Out_normal *) *)
(* | exec_Scall: forall optid lab fd args arg_ints vres le se se' lg lg' g, *)
(*     eval_rvalues me se le args arg_ints -> *)
(*     lookup_function ge lab = Some fd -> *)
(*     eval_funcall se lg fd arg_ints se' lg' g vres -> *)
(*     exec_stmt le se lg (Scall optid lab args) (optset optid vres le) se' lg' (g + gas_saveretval' 1 + gas_call true args 1) Out_normal *)
(*  *)
(*  *)
(* | exec_Sseq_1: forall le le1 le2 se se1 se2 s1 s2 lg lg1 lg2 g1 g2 out, *)
(*     exec_stmt le  se  lg s1 le1 se1 lg1 g1 Out_normal-> *)
(*     exec_stmt le1 se1 (lg1++lg) s2 le2 se2 lg2 g2 out -> *)
(*     exec_stmt le  se  lg (Ssequence s1 s2) le2 se2 (lg2++lg1) (g1+g2) out *)
(*  *)
(* | exec_Sseq_2: forall le le1 se se1 s1 s2 lg lg1 g1 out, *)
(*     exec_stmt le  se  lg s1 le1 se1 lg1 g1 out -> *)
(*     out <> Out_normal -> *)
(*     exec_stmt le  se  lg (Ssequence s1 s2) le1 se1 lg1 g1 out *)
(*  *)
(* | exec_Sifthenelse: forall le le1 se se1 cond b s1 s2 lg lg1 g1 out, *)
(*     eval_rvalue me se le cond (Vint b) ->       *)
(*     exec_stmt le se lg (if (Int256.eq b Int256.zero) then s2 else s1) *)
(*               le1 se1 lg1 g1 out -> *)
(*     exec_stmt le se lg (Sifthenelse cond s1 s2) *)
(*               le1 se1 lg1 (g1+gas_eval_cond true cond 3) out *)
(*  *)
(* | exec_Sreturn_some: forall se le lg a v, *)
(*     eval_rvalue me se le a v -> *)
(*     exec_stmt le se lg (Sreturn (Some a)) *)
(*               le se nil (gas_return true (Some a) 0) (Out_return v) *)
(*  *)
(* | exec_Sbreak:   forall se le lg, *)
(*     exec_stmt le se lg Sbreak *)
(*               le se nil 0  Out_break *)
(*  *)
(* | exec_Sloop_stop: forall le le' se se' lg lg' s g out out', *)
(*     exec_stmt le se lg s le' se' lg' g out' -> *)
(*     out_break_or_return out' out -> *)
(*     exec_stmt le se lg (Sloop s) *)
(*               le' se' lg' (g+ gas_branches 2) out *)
(*  *)
(* | exec_Sloop_loop: forall le le1 le2 se se1 se2 lg lg1 lg2 s g1 g2 out1 out2, *)
(*     exec_stmt le se lg s le1 se1 lg1 g1  out1 -> *)
(*     out_normal out1 -> *)
(*     exec_stmt le1 se1 (lg1++lg) (Sloop s) le2 se2 lg2 g2 out2 -> *)
(*     exec_stmt le se lg (Sloop s) *)
(*               le2 se2 (lg2++lg1) (g1 + g2 + gas_branches 2) out2 *)
(*  *)
(* | exec_Slog : forall le se topics topics' args args' lg, *)
(*     eval_rvalues me se le topics topics' -> *)
(*     eval_rvalues me se le args args' -> *)
(*     exec_stmt le se lg (Slog topics args) le se (Elog topics' args' :: nil) (gas_log true topics args 2) Out_normal *)
(*                *)
(*                  *)
(*    (* todo: we should also have a case for Stranfer, but right now  *)
(*       the small-step semantics are not quite right (the relation *)
(*       should also act on the storage environment), and in any *)
(*       case we are not emitting it in Edsger yet, so leave it for later. *) *)
(*                  *)
(* (** [eval_funcall m1 fd args t m2 res] describes the invocation of *)
(*   function [fd] with arguments [args].  [res] is the value returned *)
(*   by the call.  *) *)
(*  *)
(* with eval_funcall: ext_env -> log -> function -> list val -> ext_env -> log -> nat -> val -> Prop := *)
(* | eval_funcall_internal: forall le le' se se' lg lg' f args_int g vres, *)
(*     function_initial_temps ftype f args_int = Some le -> *)
(*     exec_stmt le se lg f.(fn_body) le' se' lg' g (Out_return vres) -> *)
(*     eval_funcall se lg f args_int se' lg' (g + gas_done (length (all_temps ftype f)) 3 + gas_callenter (length (some_temps ftype f)) (length (some_args ftype f)) 1) vres *)
(*   (*                  *)
(*   | eval_funcall_external: ... *). *)
(*  *)
(*  *)
(* (* In Clight, there is a distinction between internal and external function *)
(*    calls, and the DeepSEA C backend generally uses external calls. *)
(*    But it doesn't make a big difference, because the primitive-proofs make use of *)
(*    eval_funcall, rather than eval_funcall_internal or eval_funcall_external directly. *) *)
(*  *)
(* Scheme exec_stmt_ind2 := Minimality for exec_stmt Sort Prop *)
(*   with eval_funcall_ind2 := Minimality for eval_funcall Sort Prop. *)
(* Combined Scheme exec_stmt_funcall_ind from exec_stmt_ind2, eval_funcall_ind2. *)


End BIGSTEP.

(** Big-step execution of a whole program.  *)
(*
Section WHOLE_PROGRAM.
Local Existing Instance writable_block_always_ops.

Inductive bigstep_program_terminates' (p: program): trace -> int -> Prop :=
  | bigstep_program_terminates_intro: forall b f m0 m1 t r,
      let ge := Genv.globalenv p in 
      Genv.init_mem p = Some m0 ->
      Genv.find_symbol ge p.(prog_main) = Some b ->
      Genv.find_funct_ptr ge b = Some f ->
      type_of_fundef f = Tfunction Tnil type_int32s cc_default ->
      eval_funcall ge  m0 f nil t m1 (Vint r) ->
      bigstep_program_terminates' p t r.

Inductive bigstep_program_diverges' (p: program): traceinf -> Prop :=
  | bigstep_program_diverges_intro: forall b f m0 t,
      let ge := Genv.globalenv p in 
      Genv.init_mem p = Some m0 ->
      Genv.find_symbol ge p.(prog_main) = Some b ->
      Genv.find_funct_ptr ge b = Some f ->
      type_of_fundef f = Tfunction Tnil type_int32s cc_default ->
      evalinf_funcall ge  m0 f nil t ->
      bigstep_program_diverges' p t.

Definition bigstep_semantics' (p: program) :=
  Bigstep_semantics (bigstep_program_terminates' p) (bigstep_program_diverges' p).

Definition bigstep_semantics1 := bigstep_semantics' function_entry1.

Definition bigstep_semantics2 := bigstep_semantics' (fun _ => function_entry2).

End WHOLE_PROGRAM.
*)

(** * Implication from big-step semantics to transition semantics *)

Section BIGSTEP_TO_TRANSITIONS.

Variable ge: genv.
Variable me: machine_env (cdata_type D).

Open Scope nat_scope.
  
(* Inductive outcome_state_match *)
(*           (le: temp_env) (d: cdata_type D) (se : storage_env) (f: function) (k: cont) (g:nat): *)
(*   outcome -> state -> Prop := *)
(*   | osm_normal: forall g', *)
(*       g = g' ->   *)
(*       outcome_state_match le d se f k g Out_normal (State f Sskip  k le d se g') *)
(*   | osm_break: forall g', *)
(*       g = g' ->  *)
(*       outcome_state_match le d se f k g Out_break  (State f Sbreak k le d se g') *)
(* (*  | osm_return_none: forall  g' k', *)
(*       lg = lg' -> *)
(*       g = g' ->  *)
(*       call_cont k' = call_cont k -> *)
(*       outcome_state_match le m f k  g  *)
(*         (Out_return None) (State f (Sreturn None) k' le m g')  *) *)
(*   | osm_return_some: forall a v g' k', *)
(*       g + (gas_return true (Some a) 0) = g' ->        *)
(*       call_cont k' = call_cont k -> *)
(*       eval_rvalue me se le a v -> *)
(*       outcome_state_match le d se f k g *)
(*                           (Out_return v) *)
(*                           (State f (Sreturn (Some a)) k' le d se g'). *)
(*  *)
Lemma is_call_cont_call_cont:
  forall k, is_call_cont k -> call_cont k = k.
Proof.
  destruct k; simpl; intros; contradiction || auto.
Qed.

(*Notation step := (fun ge => Clight.step ge (function_entry ge)).*)

Require Import Smallstep.


Lemma gt_plus : forall G g,
  (G >= g -> exists g0, G = g0 + g )%nat.
Proof.
  intros.
  exists (G-g).
  omega.
Qed.

Ltac to_plus :=
  match goal with
    [ H : (_ >= _)%nat |- _] => (destruct (gt_plus _ _ H); subst)
  end.


Lemma loop_inversion1 : forall f s k le d se g S2,
  step me ge
       (State f (Sloop s) k le d se g) S2 ->
  exists g', 
    star (step me) ge
         (State f s (Kloop s k) le d se g') S2
         /\ g' = g - gas_branches 2
         /\ g = g-gas_branches 2+gas_branches 2.
Proof.
  intros.
  inversion H.
  subst.
  eexists.
  split.
  - apply star_refl.
  - simpl. omega.
Qed.

(* Lemma loop_inversion : forall f s k le se lg g S2 *)
(*                               f' k' le' se' lg' g' out', *)
(*     star (step me) ge (State f (Sloop s) k le se lg g) S2 -> *)
(*     outcome_state_match le' se' f' k' lg' g' out' S2 -> *)
(*     exists g', *)
(*       star (step me) ge (State f s (Kloop s k) le se lg g') S2 *)
(*       /\ g' = g-gas_branches 2 *)
(*       /\ g = g-gas_branches 2+gas_branches 2. *)
(* Proof. *)
(*   intros f s k le se lg g S2 *)
(*          f' k' le' se' lg' g' out' Hstar Hosm. *)
(*   inv Hstar. *)
(*   + inv Hosm. *)
(*   + destruct (loop_inversion1 _ _ _ _ _ _ _ _ H) as [g1 [Hstep' [Heq_g1 Heq_g]]]. *)
(*     exists g1. *)
(*     split; auto. *)
(*     apply star_trans with s2; assumption. *)
(* Qed. *)

(* Lemma exec_stmt_eval_funcall_steps:  *)
(*   (forall le m s le' m' g out, *)
(*    exec_stmt ge me le m s le' m' g out -> *)
(*    forall f k G, *)
(*      (G >= g) -> *)
(*      exists S, *)
(*        star (step me) ge (State f s k le (snd m) (fst m) G) S *)
(*        /\ outcome_state_match le' (snd m') (fst m') f k (G-g) out S) *)
(* /\ *)
(*   (forall m f args m' g res, *)
(*       eval_funcall ge me m f args m' g res -> *)
(*    forall k G, *)
(*      (G >= g) -> *)
(*      is_call_cont k -> *)
(*      star (step me) ge (Callstate f args k (snd m) (fst m) G) *)
(*                        (Returnstate res k (snd m') (fst m') (G - g))). *)
(* Proof. *)
(*   apply exec_stmt_funcall_ind; intros. *)
(*  *)
(* (* skip *) *)
(*   -  econstructor; split. apply star_refl. *)
(*      replace (G-0)%nat with G by omega. *)
(*      constructor; simpl; auto. *)
(*  *)
(* (* assign *) *)
(*   - intros. *)
(*     to_plus. *)
(*     econstructor; split. apply star_one. *)
(*     econstructor; eauto. *)
(*     constructor; auto; omega. *)
(* (* set *) *)
(*   - intros. *)
(*     to_plus. *)
(*     econstructor; split. apply star_one. *)
(*     econstructor; eauto. constructor; auto; omega. *)
(*  *)
(* (* call *) *)
(*   - intros. *)
(*     to_plus. *)
(*     econstructor; split. *)
(*     eapply star_step. *)
(*     rewrite !plus_assoc. *)
(*     econstructor; eauto.  *)
(*     eapply star_right. *)
(*     + eapply H2. *)
(*       * omega. *)
(*       * exact I. *)
(*     + replace (x + g + gas_saveretval' 1 - g) with (x + gas_saveretval' 1) by omega. *)
(*       apply step_returnstate. *)
(*     + constructor; auto; omega. *)
(*        *)
(* (* sequence 1 *) *)
(*   - to_plus. rewrite !plus_assoc.     *)
(*     destruct (H0 f (Kseq s2 k) (x+g1+g2)) as [S1 [A1 B1]]. *)
(*       { omega. } *)
(*     inv B1. *)
(*     destruct (H2 f k (x+g2)) as [S2 [A2 B2]]. *)
(*     { omega. } *)
(*     econstructor; split. *)
(*     eapply star_left. econstructor. *)
(*     eapply star_trans. eexact A1.  *)
(*     eapply star_left. constructor. *)
(*     replace (x + g1 + g2 - g1) with (x+g2) by omega. eexact A2. *)
(*     replace (x + g1 + g2 - (g1 + g2)) with (x + g2 - g2) by omega. *)
(*     exact B2. *)
(*  *)
(* (* sequence 2 *) *)
(*   - to_plus.  *)
(*     destruct (H0 f (Kseq s2 k) (x+g1)) as [S1 [A1 B1]]. *)
(*     { omega. } *)
(*   set (S2 := *)
(*     match out with *)
(*     | Out_break => State f Sbreak k le1 (snd m1) (fst m1) x *)
(*     | _ => S1 *)
(*     end). *)
(*   exists S2; split. *)
(*   + eapply star_left. econstructor. *)
(*     eapply star_trans. eexact A1. *)
(*     unfold S2; inv B1. *)
(*     congruence. *)
(*     * apply star_one. *)
(*       replace (x + g1 - g1) with x by omega. *)
(*       apply step_break_seq. *)
(*     * apply star_refl. *)
(*   + unfold S2; inv B1; congruence || econstructor; eauto; omega. *)
(*  *)
(* (* ifthenelse *) *)
(*   - to_plus. *)
(*     destruct (H1 f k (x+g1)) as [S1 [A1 B1]]. *)
(*     { omega. } *)
(*     exists S1; split. *)
(*     + eapply star_left. 2: eexact A1. *)
(*       rewrite plus_assoc. *)
(*       eapply step_ifthenelse; eauto. *)
(*     + replace(x + (g1 + gas_eval_cond true cond 3) - (g1 + gas_eval_cond true cond 3)) *)
(*         with (x + g1 - g1) by omega. *)
(*       exact B1. *)
(*  *)
(*   (* return none *) *)
(*   (*       *)
(*   -  econstructor; split. apply star_refl. *)
(*      apply osm_return_none; simpl; auto; omega. *)
(*    *) *)
(*  *)
(*  *)
(* (* return some *) *)
(*   -econstructor; split. apply star_refl. *)
(*    apply osm_return_some; simpl; auto; omega. *)
(*  *)
(*  *)
(* (* break *) *)
(*   - econstructor; split. apply star_refl. *)
(*     apply osm_break; simpl; auto; omega. *)
(*  *)
(* (* loop stop *) *)
(*   - to_plus. rewrite !plus_assoc. *)
(*     destruct (H0 f (Kloop s k) (x+g)) as [S1 [A1 B1]]. *)
(*      { omega. } *)
(*      set (S2 := *)
(*             match out' with *)
(*             | Out_break => State f Sskip k le' (snd m') (fst m') (x) *)
(*             | _ => S1 *)
(*             end). *)
(*      exists S2; split. *)
(*   + eapply star_left. eapply step_loop.  *)
(*      eapply star_trans. eexact A1. *)
(*      unfold S2. inversion H1; subst. *)
(*      inv B1. apply star_one. *)
(*      replace (x+g-g) with x by omega. *)
(*      constructor.     *)
(*      apply star_refl. *)
(*   + replace (x + g + gas_branches 2 - (g + gas_branches 2)) *)
(*             with (x + g - g) by omega. *)
(*     unfold S2. inversion H1; subst. *)
(*     * constructor; auto; omega. *)
(*     * inversion B1; apply osm_return_some; simpl in *; auto. *)
(*        *)
(* (* loop loop *) *)
(*   - to_plus. *)
(*     rewrite !plus_assoc. *)
(*     destruct (H0 f (Kloop s k) (x+g1+g2)) as [S1 [A1 B1]]. *)
(*     { omega. } *)
(*     destruct (H3 f k (x+g2)) as [S2 [A2 B2]]. *)
(*     { omega. } *)
(*     exists S2; split. *)
(*     + eapply star_left. eapply step_loop.  *)
(*       eapply star_trans. *)
(*       exact A1. *)
(*  *)
(*       eapply loop_inversion in A2; [|eassumption]. *)
(*       destruct A2 as [g' [A2_steps [A2_eq1 A2_eq2]]].  *)
(*       inv H1. *)
(*       inv B1. *)
(*       eapply star_left. *)
(*       replace (x + g1 + g2 - g1) with (x + g2) by omega. *)
(*       rewrite A2_eq2. *)
(*       apply step_skip_loop. *)
(*       exact A2_steps. *)
(*     + replace (x + g1 + g2  + gas_branches 2  - (g1 + g2 + gas_branches 2)) *)
(*       with (x + g2 - g2) by omega. *)
(*       inv B2; constructor; auto. *)
(*  *)
(*   (* log *) *)
(*   - to_plus. *)
(*     eexists. split. *)
(*  *)
(*     +eapply star_step. *)
(*      econstructor. *)
(*      eauto. *)
(*      eauto. *)
(*      econstructor. *)
(*     + constructor; simpl; auto. *)
(*       omega. *)
(*      *)
(*   (* call internal *) *)
(*   - to_plus. *)
(*     rewrite !plus_assoc. *)
(*     destruct (H1 f k (x + g + gas_done (length (all_temps ftype f)) 3)) as [S1 [A1 B1]]. *)
(*     { omega. } *)
(*     eapply star_left. eapply step_internal_function; eauto. *)
(*     eapply star_right. eexact A1. *)
(*     inv B1.  *)
(*     (* Out_return Some *) *)
(*     + rewrite <- (is_call_cont_call_cont k H3). rewrite <- H6. *)
(*       replace *)
(*         (x + g + gas_done (length (all_temps ftype f)) 3 + *)
(*         gas_callenter (length (some_temps ftype f)) (length (some_args ftype f)) 1 - *)
(*         (g + gas_done (length (all_temps ftype f)) 3 + *)
(*          gas_callenter (length (some_temps ftype f)) (length (some_args ftype f)) 1)) *)
(*         with x by omega. *)
(*       replace (x + g + gas_done (length (all_temps ftype f)) 3 - g + gas_return true (Some a) 0) *)
(*       with (x + (gas_done (length (all_temps ftype f)) 3 + gas_return true (Some a) 0)) *)
(*              by omega. *)
(*       replace (gas_done (length (all_temps ftype f)) 3 + gas_return true (Some a) 0) *)
(*       with  (gas_return_done true (Some a) (length (all_temps ftype f)) 3). *)
(*  *)
(*       Focus 2. *)
(*       rewrite <- gas_return_done_split; f_equal; omega. *)
(*       eapply step_return; eauto. *)
(*       constructor; assumption. *)
(*  (* call external *) *)
(*  (* - apply star_one. apply step_external_function; auto. *) *)
(* Qed.       *)


(* Lemma exec_stmt_steps: *)
(*   forall le m s le' m' g out, *)
(*   exec_stmt ge me le m s le' m' g out -> *)
(*   forall f k G, *)
(*     (G >= g) -> *)
(*     exists S, *)
(*        star (step me) ge (State f s k le (snd m) (fst m) G) S *)
(*        /\ outcome_state_match le' (snd m') (fst m') f k  (G-g) out S. *)
(* Proof. *)
(*   destruct exec_stmt_eval_funcall_steps; assumption. *)
(* Qed. *)
(*  *)
(* Lemma eval_funcall_steps: *)
(*   forall m f args m' g res, *)
(*     eval_funcall ge me m f args m' g res -> *)
(*     forall k G, *)
(*       (G >= g) -> *)
(*       is_call_cont k -> *)
(*       star (step me) ge (Callstate f args k (snd m) (fst m) G) *)
(*                         (Returnstate res k (snd m') (fst m') (G-g)). *)
(* Proof. *)
(*   destruct exec_stmt_eval_funcall_steps; assumption. *)
(* Qed. *)

End BIGSTEP_TO_TRANSITIONS.

End WITH_DATA.
