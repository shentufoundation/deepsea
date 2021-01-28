Require Import token.DataTypes.
Require Import token.DataTypeOps.
Require Import lib.Monad.StateMonadOption.
Require Import cclib.Maps.
Require Import cclib.Integers.
Require Import ZArith.
Require Import core.HyperTypeInst.
Require Import backend.MachineModel.
Require Import token.LayerFIXEDSUPPLYTOKEN.

Definition state := global_abstract_data_type.

Definition init_state := init_global_abstract_data.

Definition wei := Z.

Definition addr := int256.

Definition blocknumber := int256.

Existing Instance GlobalLayerSpec.

Section step.

  Context (contract_address : addr).

  Section mstep.
  (* These are the parameters which are constant within a given block. *)
  Context (coinbase : int256)
          (timestamp : int256)
          (number : int256)
          (balance : int256 -> int256)
          (blockhash : int256 -> int256)
          (prev_contract_state : state).

  Definition make_machine_env (caller: addr)
                              : machine_env state
    := {| me_address := contract_address;
          me_origin := caller;
          me_caller := caller; (* need update after every control-flow transfer *)
          me_callvalue := Int256.repr (0);
          me_coinbase := coinbase; 
          me_timestamp := timestamp;
          me_number := number;
          me_balance := balance;
          me_blockhash := blockhash;
          (* not implemented *)
          me_transfer _ _ _ _ _ := False;
          me_callmethod _ _ _ _ _ _ _ _ _ _ := False;
          me_log _ _ _ := prev_contract_state;
        |}.

  Import MonadNotation.

  Definition lif {A:Type}
                 (caller : addr)
                 (cmd : machine_env state -> osT global_abstract_data_type A)
    : osT state A :=
    st  <- get;;
    let me := make_machine_env caller in
    match runStateT (cmd me) st with
    | None => mzero
    | Some (v, st') => put st' ;; ret v
    end.

  (* osT state int256 = 
    state transformer with option monad, state is state, and value is int256 *)
  Print osT. (* = fun D : Type => stateT D option *)
  Print stateT. (* (S : Type) (m : Type -> Type) (t : Type) : Type := mkStateT
  { runStateT : S -> m (t * S)%type } *)

  Print runStateT. (* takes monad transformer, state, returns m (t * S) *)

  (* How the state of the system changes in a single method call made by player p. *)
  Inductive mstep u (st st' : state) : Prop :=
  | FixedSupplyToken_constructor_step : forall r , runStateT (FixedSupplyToken_constructor_opt  (make_machine_env u)) st = Some (r, st') -> mstep u st st' 
  | FixedSupplyToken_totalSupply_step : forall r , runStateT (FixedSupplyToken_totalSupply_opt  (make_machine_env u)) st = Some (r, st') -> mstep u st st' 
  | FixedSupplyToken_balanceOf_step : forall r tokenOwner, runStateT (FixedSupplyToken_balanceOf_opt tokenOwner (make_machine_env u)) st = Some (r, st') -> mstep u st st' 
  | FixedSupplyToken_transfer_step : forall r toA tokens, runStateT (FixedSupplyToken_transfer_opt toA tokens (make_machine_env u)) st = Some (r, st') -> mstep u st st' 
  | FixedSupplyToken_approve_step : forall r spender tokens, runStateT (FixedSupplyToken_approve_opt spender tokens (make_machine_env u)) st = Some (r, st') -> mstep u st st' 
  | FixedSupplyToken_transferFrom_step : forall r fromA toA tokens, runStateT (FixedSupplyToken_transferFrom_opt fromA toA tokens (make_machine_env u)) st = Some (r, st') -> mstep u st st' 
.

  (* We can compute a new block by letting the players call the contract 
     in some arbitrary order. *)
     Inductive multi_mstep : state -> state -> Prop := 
     | multi_mstep_reflexive : forall (st : state), multi_mstep st st
     | multi_mstep_transitive : forall (st st' st'' : state) u,
         multi_mstep st st' -> mstep u st' st'' -> multi_mstep st st''.


     (* A block is sufficiently synchronous if every player got a chance to submit a 
        transaction to to. *)
     (* TODO, I think this definition is fine for now, but it seems little too clever,
        should probably re-state this using some straightforward tracking of the states
        we pass through. *)
     Definition multi_mstep_synchronous st1 st2 :=
       forall u, exists st st',
                   multi_mstep st1 st /\ mstep u st st' /\ multi_mstep st' st2.

     (* Here are a bunch of induction principles inspired by linear temporal logic. *)

     (* Prove that some property P holds "globally", i.e. for each state along a 
        path.
        You can also specify a property Pprev which is known to hold for the
        prev_contract_state. If not needed, it can just be True.
    *)
     Lemma multi_mstep_Global_ind : forall (Pprev P : state -> Prop),
         (forall u st st', Pprev prev_contract_state -> P st -> mstep u st st' -> P st') ->
          Pprev prev_contract_state -> forall st st', P st -> multi_mstep st st' -> P st'.
     Proof.
       induction 4; eauto.
     Qed.

     (*
     (* Prove that P holds "until" Q  along a path. 
        "Until" is a liveness assertion, so we need the synchronicity assumption. *)
     Lemma multi_mstep_Until_ind : forall (Pprev P Q : state -> Prop),
         (forall p st st', Pprev prev_contract_state -> P st -> In p players
                           -> mstep p st st' -> (P st' \/ Q st')) ->
                           Pprev prev_contract_state ->
                           forall st,
                             P st -> exists st',  multi_mstep st st' -> (P st' \/ Q st').
     Proof.
       induction 4; eauto *)

     End mstep.


     Definition Int256_incr x := Int256.add x Int256.one.

     Inductive bstep (n : blocknumber) : state -> state -> Prop :=
     | bstep_step : forall coinbase timestamp balance blockhash st st',
         multi_mstep coinbase timestamp n balance blockhash st st st' ->
         bstep n st st'.

     Inductive multi_bstep : blocknumber -> state -> blocknumber -> state -> Prop := 
     | multi_bstep_reflexive : forall n (st : state), multi_bstep n st n st
     | multi_bstep_transitive : forall n n'  (st st' st'' : state),
       multi_bstep n st n' st' -> bstep (Int256_incr n') st' st'' -> multi_bstep n st (Int256_incr n') st''.

     (* multi_bstep is the step relation without any synchronicity assumption.
        This is sufficient to prove some safety properties, but for most interesting 
        theorems we instead need to use this synchronous version: *)

     Inductive bstep_synch (n : blocknumber) : state -> state -> Prop :=
     | bstep_synch_step : forall coinbase timestamp balance blockhash st st',
         multi_mstep_synchronous coinbase timestamp n balance blockhash st st st' ->
         bstep_synch n st st'.

     Inductive multi_bstep_synch : blocknumber -> state -> blocknumber -> state -> Prop := 
     | multi_bstep_synch_reflexive : forall n (st : state), multi_bstep_synch n st n st
     | multi_bstep_synch_transitive : forall n n'  (st st' st'' : state),
       multi_bstep_synch n st n' st' -> bstep_synch (Int256_incr n') st' st'' -> multi_bstep_synch n st (Int256_incr n') st''.

     Lemma multi_bstep_Global_ind : forall (P : state -> Prop),
         (forall coinbase timestamp number balance blockhash prev_block u st st',
             P prev_block
             -> P st
             -> mstep coinbase timestamp number balance blockhash prev_block u st st'
             -> P st')
         -> forall n st n' st',
           P st -> multi_bstep n st n' st' -> P st'.
     Proof.
       induction 3.
       - auto.
       - inversion H2; subst.
         eapply multi_mstep_Global_ind with (st:=st') (prev_contract_state := st').
         + intros.
           refine (H _ _ _ _ _ _ _ _ _ _ _ H6); auto.
         + apply IHmulti_bstep; assumption.
         + apply IHmulti_bstep; assumption.
         + exact H3.
     Qed.
   End step.

   Section DeepSEAGenericProof.

     Lemma Int256Tree_reduce : forall (i: int256) (v: Z) (t: Int256Tree.t Z), Int256Tree.get_default 0%Z i (Int256Tree.set i v t) = v.
     Proof.
       intros.
       unfold Int256Tree.get_default.
       rewrite Int256Tree.gss .
       reflexivity.
     Qed.

     Lemma Int256Tree_mreduce : forall (i j : int256) (v v': Z) (t: Int256Tree.t Z), 
       i <> j ->
       Int256Tree.get_default 0%Z i (Int256Tree.set j v' (Int256Tree.set i v t)) = v.
     Proof.
       intros.
       unfold Int256Tree.get_default.
       rewrite Int256Tree.gso.
       rewrite Int256Tree.gss.
       reflexivity.
       exact H.
     Qed.

     Lemma Int256Tree_mireduce : forall (i j k : int256) (v v': Z) (t: Int256Tree.t Z), 
       i <> j ->
       i <> k ->
       j <> k ->
       Int256Tree.get_default 0%Z i (Int256Tree.set j v' (Int256Tree.set k v t)) = 
       Int256Tree.get_default 0%Z i t.
     Proof.
       intros.
       unfold Int256Tree.get_default.
       rewrite Int256Tree.gso.
       rewrite Int256Tree.gso.
       reflexivity.
       exact H0.
       exact H.
     Qed.

     Lemma add_sub_inv : forall (i j : Z32), (i + j - i)%Z = j.
     Proof.
       intros.
       omega.
     Qed.
   End DeepSEAGenericProof.

   Section Proof.   
     Context (* (strategies : strategy_tuple) *)
             (initial_balances : addr -> Z)
             (contract_address : int256).

     Context (init_bt init_rt : int256)
             (init_coinbase : int256)
             (init_timestamp : int256)
             (init_number : int256)
             (init_blockhash : int256 -> int256)
             (pre_init_state init_state : state).

     (* These are the parameters which are constant within a given block. *)
     Context (coinbase : int256)
             (timestamp : int256)
             (number : int256)
             (balance : int256 -> int256)
             (blockhash : int256 -> int256)
             (prev_contract_state : state).

     Require Import lib.Monad.RunStateTInv.
     Require Import lib.ArithInv.

     Definition make_machine_env_wrapped prev_st user :=
      make_machine_env contract_address coinbase timestamp number balance blockhash prev_st user.

     Lemma make_machine_env_caller_eq : forall st caller, me_caller (make_machine_env_wrapped st caller) = caller.
      Proof. auto. Qed.

     Lemma make_machine_env_address_eq : forall st caller, me_address (make_machine_env_wrapped st caller) = contract_address.
      Proof. auto. Qed.
     
Transparent FixedSupplyToken_constructor_opt.
Transparent FixedSupplyToken_totalSupply_opt.
Transparent FixedSupplyToken_balanceOf_opt.
Transparent FixedSupplyToken_transfer_opt.
Transparent FixedSupplyToken_approve_opt.
Transparent FixedSupplyToken_transferFrom_opt.

Ltac rds :=
unfold FixedSupplyToken_constructor_opt in *;
unfold FixedSupplyToken_totalSupply_opt in *;
unfold FixedSupplyToken_balanceOf_opt in *;
unfold FixedSupplyToken_transfer_opt in *;
unfold FixedSupplyToken_approve_opt in *;
unfold FixedSupplyToken_transferFrom_opt in *;
inv_runStateT;
subst;
inv_arith;
simpl;
try rewrite make_machine_env_caller_eq in *;
try rewrite make_machine_env_address_eq in *;
try rewrite Int256Tree_reduce in *;
try rewrite Int256Tree_mreduce in *;
try rewrite Int256Tree_mireduce in *;
auto.

Theorem sample : forall n, n = n.

End Proof.
