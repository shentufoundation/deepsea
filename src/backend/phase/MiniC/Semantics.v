(* Semantics for MiniC expressions and statements. *)

Require Import backend.TempModel.
Require Import backend.AST.
Require Import cclib.Coqlib.
Require Import backend.Expressions.ExpMiniC.
Require Import backend.Expressions.SemanticsMiniC.
Require Import backend.Statements.StmtMiniC.
Require Import backend.Values.HighValues.
Require Import cclib.Maps.
Require Import cclib.Integers.
Require Import backend.Cop.
Require Import backend.Ctypes.
Require Import backend.Options.
Require Import backend.MemoryModel.
Require Import backend.AbstractData.
Require Import backend.MachineModel.
Require Import backend.Environments.Globalenvs.
Require Import backend.Environments.AllEnvironments.
Require Import backend.Environments.ExtEnv.
Require Import backend.GasModel.
Require Import backend.SymbolicKeccak.
Require Import List. Import ListNotations.

Definition ftype : ftemps := mkftemps function fn_temps fn_params.

Section WITH_DATA.


(* machine environment *)
Context {adata} {data_ops: CompatDataOps adata}.  
Variable me: machine_env adata.

(** ** Transition semantics for statements and functions *)

(** Continuations *)

(* from a given backtrace / line number in the program, where does it go from here?
forms a linked list of continuations, ending at a stop. *)

Inductive cont: Type :=
| Kstop: cont
| Kseq: statement -> cont -> cont       (**r [Kseq s2 k] = after [s1] in [s1;s2] *)
| Kloop: statement -> cont -> cont      (**r [Kloop s k] = inside [Sloop s] *)
| Kcall: option ident ->                  (**r where to store result *)
         function ->                      (**r calling function *)
         temp_env ->                      (**r temporary env of calling function *)
         cont -> cont.

(** Pop continuation until a call or stop.
    It used in the semantics on return, to pop out of all sequences and loops.
 *)
Fixpoint call_cont (k: cont) : cont :=
  match k with
  | Kseq _ k => call_cont k
  | Kloop _ k => call_cont k
  | _ => k
  end.

(* Used in theorem statements. *)
Definition is_call_cont (k: cont) : Prop :=
  match k with
  | Kstop => True
  | Kcall _ _ _ _ => True
  | _ => False
  end.

Lemma call_cont_idempotent: forall k,
  is_call_cont k -> k = call_cont k.
Proof.
intros. destruct k; simpl.
reflexivity.
simpl in H. contradiction.
simpl in H. contradiction.
reflexivity.
Qed.

Fixpoint call_cont_index (k:cont) : int256 :=
  match k with
  | Kstop => Int256.repr 0
  | Kcall _ _ _ k => Int256.add (call_cont_index k) (Int256.repr 1)
  | Kseq _ k => call_cont_index k
  | Kloop _ k => call_cont_index k
  end.

(** States *)

Inductive state: Type :=
  | State
      (f: function)
      (s: statement)
      (k: cont)
      (le: temp_env)
      (ee: ext_env)    (* EVM specific *)
      (d: adata)
      (gas: nat) : state   (* EVM specific. This is the amount of gas _remaining_.  *)
  | Callstate        (* these are used for internal function calls, but currently not for method calls. *)
      (fd: function)
      (args: list val)
      (k: cont)
      (ee: ext_env)
      (d: adata)
      (gas: nat) : state
  | Initialstate
      (fd: function)
      (args: list val)
      (ee: ext_env)
      (d: adata)
      (gas: nat) : state
  | Returnstate
      (res: val)
      (k: cont)
      (ee: ext_env)
      (d: adata)
      (gas: nat) : state.

Section STEP.

  Local Open Scope nat_scope.
  
  Variable ge: genv.
  
  Definition count_methods := Genv.count_methods function type ge.

  Definition lookup_function (lab: label) : option function :=
    PTree.get lab (Genv.genv_fundefs function type ge).

  (* This models sucessful runs. In particular, note that there is no rule for revert, so a call for revert is a stuck state. *)
  Inductive step: state -> state -> Prop :=
  | step_assign: forall f e1 lv e2 rv k ee le d g ee',
      eval_lvalue (call_cont_index k) me ee le e1 lv ->
      eval_rvalue (call_cont_index k) me ee le e2 rv ->
      (* only assign to locations that are already defined *)
      write lv rv ee = Some ee' ->
      step (State f (Sassign e1 e2) k le ee d (g + gas_assign true e1 e2 2))
           (* See GasModel.v for the definitions of gas costs. *)
           (State f Sskip k le ee' d g)
           
  | step_set: forall f id rv rv_int k le ee prev d g,
      PTree.get id le = Some prev -> (* Can only Sset an existing temp *)
      eval_rvalue (call_cont_index k) me ee le rv rv_int ->
      step (State f (Sset id rv) k le ee d (g + gas_set true rv 2))
           (State f Sskip k (PTree.set id rv_int le) ee d g)

  | step_call: forall f optid lab fd args arg_ints k le ee d g,
      eval_rvalues (call_cont_index k) me ee le args arg_ints ->
      lookup_function lab = Some fd ->
      (* gas for returning is reserved in step_returnstate *)
      step (State f (Scall optid lab args) k le ee d (g + gas_call true args 1))
           (Callstate fd arg_ints (Kcall optid f le k) ee d g)

  | step_seq: forall f s1 s2 k le ee d g,
      step (State f (Ssequence s1 s2) k le ee d g)
           (State f s1 (Kseq s2 k) le ee d g)
  | step_skip_seq: forall f s k le ee d g,
      step (State f Sskip (Kseq s k) le ee d g)
           (State f s k le ee d g)
  | step_break_seq: forall f s k le ee d g,
      step (State f Sbreak (Kseq s k) le ee d g)
           (State f Sbreak k le ee d g)

  | step_ifthenelse: forall f cond b s1 s2 k le ee d g,
      eval_rvalue (call_cont_index k) me ee le cond (Vint b) ->
      step (State f (Sifthenelse cond s1 s2) k le ee d (g + gas_eval_cond true cond 3))
           (State f (if (Int256.eq b Int256.zero) then s2 else s1) k le ee d g)

  | step_loop: forall f s k le ee d g,
      (* enter loop *)
      step (State f (Sloop s) k le ee d (g + gas_branches 2))
           (State f s (Kloop s k) le ee d g)
  | step_break_loop: forall f s k le ee d g,
      (* break from any part of the loop. doesn't use gas because it doesn't compile to any steps in Cgraph *)
      step (State f Sbreak (Kloop s k) le ee d g)
           (State f Sskip k le ee d g)
  | step_skip_loop: forall f s k le ee d g,
      (* reach the end of s, go back to restart the loop *)
      step (State f Sskip (Kloop s k) le ee d (g + gas_branches 2))
           (State f s (Kloop s k) le ee d g)

  | step_return_var: forall f retvar val k le ee ee' ctx' d g,
      PTree.get retvar le = Some val ->
      push_func function fn_locals (call_cont_index k) f ee = Some (ee',ctx') ->
      step (State f (Sreturn (Some retvar)) k le ee d (g + gas_return_done true (Some retvar) (length (all_temps ftype f)) 3))
           (* TODO: gas for pushing the retval *)
           (Returnstate val (call_cont k) ee' d g)
  | step_return_none: forall f k le ee ee' ctx' d g,
      push_func function fn_locals (call_cont_index k) f ee = Some (ee',ctx') ->
      step (State f (Sreturn None) k le ee d (g + gas_return_done true None (length (all_temps ftype f)) 3))
           (* TODO: gas for pushing the retval *)
           (Returnstate Vunit (call_cont k) ee' d g)

           
  | step_internal_function: forall f arg_ints k le ee ee' ctx' d g,
      function_initial_temps ftype f arg_ints = Some le ->
      push_func function fn_locals (call_cont_index k) f ee = Some (ee',ctx') ->
      step (Callstate f arg_ints k ee d (g + gas_callenter (length (some_temps ftype f)) (length (some_args ftype f)) 1))
           (State f (fn_body f) k le ee d g)
           
  | step_returnstate: forall retval id f k le ee d g,
      step (Returnstate retval (Kcall id f le k) ee d (g + gas_saveretval' 1))
         (State f Sskip k (optset id retval le) ee d g)

  | step_transfer: forall f a a' v v' le d d' ee k g,
      eval_rvalue (call_cont_index k) me ee le a (Vint a') ->
      eval_rvalue (call_cont_index k) me ee le v (Vint v') ->
      (me_transfer me) a' v' d = (Int256.one, d') ->
      step (State f (Stransfer a v) k le ee d (g + gas_transfer true a v 2))
           (State f Sskip k le ee d' g)
           
  | step_log: forall f topics topics' args args' le d ee k g,
      eval_rvalues (call_cont_index k) me ee le topics topics' ->
      eval_rvalues (call_cont_index k) me ee le args args' ->
      step (State f (Slog topics args) k le ee d (g + gas_log true topics args 2))
           (State f Sskip k le ee (me_log me topics' args' d) g)
           
  (* This currently only defines what happens if the method succeeds.

     Whether this is a problem depends on what we do in the source
     language: if we don't provide a way to catch failures, then it
     makes sense to only model succeeding runs (and in practice
     Solidity programs may not catch failures.)

     In our compiler, we have a test for a failing method call, and if so we call revert. 
 *)
  | step_callmethod: forall f a a' sg v v' args args' le le' ee ee' d d' k rvs rv_keys g gas,
      eval_rvalue (call_cont_index k) me ee le a a' ->
      eval_rvalue (call_cont_index k) me ee le v v' ->
      eval_rvalues (call_cont_index k) me ee le args args' ->
      (me_callmethod me) a' sg v' args' d ee d' ee' Int256.one rvs ->
      listset rv_keys rvs le = Some le' ->
      step (State f (Scallmethod a rv_keys sg v gas args) k le ee d (g + gas_callmethod true a (length rv_keys) v args 2))
           (State f Sskip k le' ee d' g).

  Print step.
  (* Whole-program semantics 
     A simulation proof consists of three parts: the steps match, the initial states match, and the final states match.
     Here we define the initial and final states. 
   *)

  (* Note that these take more arguments than Clight. *)

  Inductive initial_state (sg: int) (*our method signature, this is provided to us by the Solidity ABI.*)
                          (args: list val)
                          (d: adata)
                          (ee: ext_env)
                          (g: nat): state -> Prop :=
  | initial_state_intro: forall f,
    In sg (Genv.genv_methods function type ge) ->
    IntMap.get sg (Genv.genv_methoddefs function type ge) = Some f ->
    initial_state sg args d ee g (Initialstate f args ee d g).

  (* note we may care how much gas is left, if this is called from another contract *)
  Inductive final_state: state -> val -> adata -> ext_env -> nat -> Prop :=
  | final_state_intro: forall r ee d g,
      final_state (Returnstate r Kstop d ee g) r ee d g.

End STEP.

End WITH_DATA.

Arguments step {adata}.
Arguments state {adata}.
Arguments initial_state {adata}.
Arguments final_state {adata}.
