Require Import amm.DataTypes.
Require Import amm.DataTypeOps.
Require Import amm.LayerAMM.
Require Import lib.Monad.StateMonadOption.
Require Import cclib.Maps.
Require Import cclib.Integers.
Require Import ZArith.
Require Import core.HyperTypeInst. 
(* for `hashvalue`, maybe should move that to some different file. *)
Require Import backend.MachineModel.

(* PROOFS: 
   2. balance tracks reserve
   3. increasing k -> algebra
   4. no path independence, splitting trades is strictly more expensive -> algebra, appendix D
   5. liquidity token economics is right, i.e., UNI-V2 tracks the actual share of pool --> actually, where is the trading of UNI-V2 Pair tokens? What is its value?
   6. MOD: slippage control
   7. ... cost of manipulation (hard)
*)

(* global_abstract_data_type *)

Definition state := global_abstract_data_type.

Definition init_state := init_global_abstract_data.

Definition wei := Z.

Definition addr := int256.

Definition blocknumber := int256.

Existing Instance GlobalLayerSpec.

Section step.

  Context (token0_address token1_address amm_address : addr).

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
    := {| me_address := amm_address;
          me_origin := caller;
          me_caller := caller;
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

  Definition lifted_simpleSwap0 :=
    fun (caller: addr) (callvalue: wei) (toA: addr) => lif caller
      (AutomatedMarketMaker_simpleSwap0_opt toA).

  Definition lifted_LiquidityToken_mint :=
    fun (caller: addr) (callvalue: wei) (toA: addr) (value: Z) => lif caller
      (LiquidityToken_mint_opt toA value).

  (* osT state int256 = 
    state transformer with option monad, state is state, and value is int256 *)
  Print osT. (* = fun D : Type => stateT D option *)
  Print stateT. (* (S : Type) (m : Type -> Type) (t : Type) : Type := mkStateT
  { runStateT : S -> m (t * S)%type } *)

  Print runStateT. (* takes monad transformer, state, returns m (t * S) *)

  (* How the state of the system changes in a single method call made by player p. *)
  (* for simplicity we only model functions that modify states and can be called by user *)
  Inductive mstep u (st st' : state) : Prop :=
  (* fixed supply tokens *)
  | FixedSupplyToken0_transfer_step : 
      forall toA tokens r, runStateT (FixedSupplyToken_transfer_opt toA tokens (make_machine_env u)) st = Some (r, st')
      -> mstep u st st'
  | FixedSupplyToken1_transfer_step : 
      forall toA tokens r, runStateT (FixedSupplyToken1_transfer_opt toA tokens (make_machine_env u)) st = Some (r, st')
      -> mstep u st st'
  | FixedSupplyToken0_approve_step : 
      forall spender tokens r, runStateT (FixedSupplyToken_approve_opt spender tokens (make_machine_env u)) st = Some (r, st')
      -> mstep u st st'
  | FixedSupplyToken1_approve_step : 
      forall spender tokens r, runStateT (FixedSupplyToken1_approve_opt spender tokens (make_machine_env u)) st = Some (r, st')
      -> mstep u st st'
  | FixedSupplyToken0_transferFrom_step : 
      forall fromA toA tokens r, runStateT (FixedSupplyToken_transferFrom_opt fromA toA tokens (make_machine_env u)) st = Some (r, st')
      -> mstep u st st'
  | FixedSupplyToken1_transferFrom_step : 
      forall fromA toA tokens r, runStateT (FixedSupplyToken1_transferFrom_opt fromA toA tokens (make_machine_env u)) st = Some (r, st')
      -> mstep u st st'
  (* amm *)
  | AutomatedMarketMaker_mint_step : 
      forall toA r, runStateT (AutomatedMarketMaker_mint_opt toA (make_machine_env u)) st = Some (r, st')
      -> mstep u st st'
  | AutomatedMarketMaker_burn_step : 
      forall toA r, runStateT (AutomatedMarketMaker_burn_opt toA (make_machine_env u)) st = Some (r, st')
      -> mstep u st st'
  | AutomatedMarketMaker_simpleSwap0_step : 
      forall toA r, runStateT (AutomatedMarketMaker_simpleSwap0_opt toA (make_machine_env u)) st = Some (r, st')
      -> mstep u st st'
  .

  (* How the state of the system changes in a single method call made by player p. *)
  (* for simplicity we only model functions that modify states and can be called by user *)
  Inductive mstep_amm u (st st' : state) : Prop :=
  (* amm *)
  | AutomatedMarketMaker_mint_ammstep : 
      forall toA r, runStateT (AutomatedMarketMaker_mint_opt toA (make_machine_env u)) st = Some (r, st')
      -> mstep_amm u st st'
  | AutomatedMarketMaker_burn_ammstep : 
      forall toA r, runStateT (AutomatedMarketMaker_burn_opt toA (make_machine_env u)) st = Some (r, st')
      -> mstep_amm u st st'
  | AutomatedMarketMaker_simpleSwap0_ammstep : 
      forall toA r, runStateT (AutomatedMarketMaker_simpleSwap0_opt toA (make_machine_env u)) st = Some (r, st')
      -> mstep_amm u st st'
  | AutomatedMarketMaker_skim_ammstep : 
      forall toA r, runStateT (AutomatedMarketMaker_skim_opt toA (make_machine_env u)) st = Some (r, st')
      -> mstep_amm u st st'
  | AutomatedMarketMaker_sync_ammstep : 
      forall r, runStateT (AutomatedMarketMaker_sync_opt (make_machine_env u)) st = Some (r, st')
      -> mstep_amm u st st'
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

  (* Lemmas on Inequalities over Z *)
  Lemma ZInEq_ladd : (forall (i j k : Z32), (i > j + k <-> i - k > j)%Z).
  Proof.
  intros. omega.
  Qed.
  Hint Rewrite ZInEq_ladd : ZInEq.

  Axiom ZInEq_multpos : (forall (a b c : Z32), c > 0 -> a > b <-> a * c > b * c)%Z.
  Hint Rewrite ZInEq_multpos : ZInEq.

  Axiom ZInEq_ldiv : (forall (a b c : Z32), b > 0 -> a / b > c <-> a > b * c)%Z.

  Axiom ZInEq_denadd : (forall (a b c : Z32), c > 0 -> a / b > a / (b + c))%Z.

  Lemma increasing_k_math : forall (i j k : Z), (i > 0 -> j > 0 -> k > 0 -> 
    i * j < (i + k) * (j - (k * 997 * j) / (i * 1000 + k * 997)))%Z.
  Proof.
    From Coq Require Import ZArith Psatz.
    Open Scope Z_scope.
    simpl. Fail lia. Print Lia.lia. Print Lia.zchecker_no_abstract.
    intros.
    (*
    psatz Z 100.
    lia.

    assert (forall (v v' v'' : Z), v'' > 0 -> v < v' -> v * v'' < v' * v'').
      intros. 
      pose (H19 := H16 (v * v'') (v' * v'')); clearbody H19.
      apply H19.
      unfold Z.sub.
      apply mul_sub_distr.
    pose (H17 := H16 (i * j) ((i + k) * (j - k * 997 * j / (i * 1000 + k * 997))) (i * 1000 + k * 997)); clearbody H17.
    *)
    assert (0 < (i * 1000 + k * 997)). omega.
    (* multiplied both sides by the denominator of the divisor and needed to use comparisons between "(W / V) * V" and W *)
    pose (Zmult_nc:=(Zmult_lt_compat_r (i * j) ((i + k) * (j - k * 997 * j / (i * 1000 + k * 997))) (i * 1000 + k * 997) H2)); clearbody Zmult_nc.
  Admitted.
End DeepSEAGenericProof.

Section AMMProof.
  Context (token0_address token1_address amm_address : addr).

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

  Definition compute_k (s: state) := 
    Z.mul (AutomatedMarketMaker__reserve0 s) (AutomatedMarketMaker__reserve1 s).

  Definition get_balance0 (s: state) (a: addr) :=
    (Int256Tree.get_default (0%Z)
      a (FixedSupplyToken_balances s)).

  Definition get_balance1 (s: state) (a: addr) :=
    (Int256Tree.get_default 0%Z
      a (FixedSupplyToken1_balances s)).

  Definition make_machine_env_wrapped prev_st user :=
    make_machine_env amm_address coinbase timestamp number balance blockhash prev_st user.

  Lemma make_machine_env_caller_eq : forall st caller, me_caller (make_machine_env_wrapped st caller) = caller.
  Proof. auto. Qed.

  Lemma make_machine_env_address_eq : forall st caller, me_address (make_machine_env_wrapped st caller) = amm_address.
  Proof. auto. Qed.

Transparent FixedSupplyToken_constructor_opt.
Transparent FixedSupplyToken_totalSupply_opt.
Transparent FixedSupplyToken_balanceOf_opt.
Transparent FixedSupplyToken_transfer_opt.
Transparent FixedSupplyToken_approve_opt.
Transparent FixedSupplyToken_transferFrom_opt.
Transparent FixedSupplyToken1_constructor_opt.
Transparent FixedSupplyToken1_totalSupply_opt.
Transparent FixedSupplyToken1_balanceOf_opt.
Transparent FixedSupplyToken1_transfer_opt.
Transparent FixedSupplyToken1_approve_opt.
Transparent FixedSupplyToken1_transferFrom_opt.
Transparent LiquidityToken_constructor_opt.
Transparent LiquidityToken_mint_opt.
Transparent LiquidityToken_burn_opt.
Transparent LiquidityToken_totalSupply_opt.
Transparent LiquidityToken_balanceOf_opt.
Transparent LiquidityToken_transfer_opt.
Transparent LiquidityToken_approve_opt.
Transparent LiquidityToken_transferFrom_opt.
Transparent AutomatedMarketMakerLib_constructor_opt.
Transparent AutomatedMarketMakerLib_getAmountIn_opt.
Transparent AutomatedMarketMakerLib_getBalanceAdjusted_opt.
Transparent AutomatedMarketMakerLib_min_opt.
Transparent AutomatedMarketMaker_constructor_opt.
Transparent AutomatedMarketMaker_mint_opt.
Transparent AutomatedMarketMaker_burn_opt.
Transparent AutomatedMarketMaker_simpleSwap0_opt.
Transparent AutomatedMarketMaker_swap_opt.
Transparent AutomatedMarketMaker_skim_opt.
Transparent AutomatedMarketMaker_sync_opt.
Transparent AutomatedMarketMaker_k_opt.
Transparent AutomatedMarketMaker_quote0_opt.
Transparent AutomatedMarketMaker_getAmountOut0_opt.
Transparent AutomatedMarketMaker_getAmountIn0_opt.
Definition FixedSupplyToken_address := (Int256.repr 65587).
Definition FixedSupplyToken1_address := (Int256.repr 65586).
Definition LiquidityToken_address := (Int256.repr 65585).
Definition AutomatedMarketMaker_address := (Int256.repr 65584).

Ltac rds :=
unfold FixedSupplyToken_constructor_opt in *;
unfold FixedSupplyToken_totalSupply_opt in *;
unfold FixedSupplyToken_balanceOf_opt in *;
unfold FixedSupplyToken_transfer_opt in *;
unfold FixedSupplyToken_approve_opt in *;
unfold FixedSupplyToken_transferFrom_opt in *;
unfold FixedSupplyToken1_constructor_opt in *;
unfold FixedSupplyToken1_totalSupply_opt in *;
unfold FixedSupplyToken1_balanceOf_opt in *;
unfold FixedSupplyToken1_transfer_opt in *;
unfold FixedSupplyToken1_approve_opt in *;
unfold FixedSupplyToken1_transferFrom_opt in *;
unfold LiquidityToken_constructor_opt in *;
unfold LiquidityToken_mint_opt in *;
unfold LiquidityToken_burn_opt in *;
unfold LiquidityToken_totalSupply_opt in *;
unfold LiquidityToken_balanceOf_opt in *;
unfold LiquidityToken_transfer_opt in *;
unfold LiquidityToken_approve_opt in *;
unfold LiquidityToken_transferFrom_opt in *;
unfold AutomatedMarketMakerLib_constructor_opt in *;
unfold AutomatedMarketMakerLib_getAmountIn_opt in *;
unfold AutomatedMarketMakerLib_getBalanceAdjusted_opt in *;
unfold AutomatedMarketMakerLib_min_opt in *;
unfold AutomatedMarketMaker_constructor_opt in *;
unfold AutomatedMarketMaker_mint_opt in *;
unfold AutomatedMarketMaker_burn_opt in *;
unfold AutomatedMarketMaker_simpleSwap0_opt in *;
unfold AutomatedMarketMaker_swap_opt in *;
unfold AutomatedMarketMaker_skim_opt in *;
unfold AutomatedMarketMaker_sync_opt in *;
unfold AutomatedMarketMaker_k_opt in *;
unfold AutomatedMarketMaker_quote0_opt in *;
unfold AutomatedMarketMaker_getAmountOut0_opt in *;
unfold AutomatedMarketMaker_getAmountIn0_opt in *;
inv_runStateT;
subst;
inv_arith;
simpl in *;
try rewrite make_machine_env_caller_eq in *;
try rewrite make_machine_env_address_eq in *;
try rewrite Int256Tree_reduce in *;
try rewrite Int256Tree_mreduce in *;
try rewrite Int256Tree_mireduce in *;
auto.

  (* need to transfer balance first, assume token0 transfer first *)
  (* should have been approve and then transferFrom, here we are hacking a bit *)
  (* prove that after every possible function call, whichever layers, the 
  k = _reserve0 * _reserve1 is strictly increasing for simpleSwap0:

  newState = simpleSwap0 oldState /\ compute_k newState > compute_k oldState *)
  Theorem increasing_k_simpleSwap0 : (forall r r' 
    (s s' s'' : state) 
    (trader : addr) 
    (swapAmount0 : Z32), 
    (swapAmount0 > 0)%Z -> (* trasferring an amount > 0 *)
    ((AutomatedMarketMaker__reserve0 s) > 0)%Z -> 
    ((AutomatedMarketMaker__reserve1 s) > 0)%Z ->
    amm_address <> trader -> 
    get_balance0 s amm_address = (AutomatedMarketMaker__reserve0 s) -> (* assumes that the reserve is the same as balance at s *)
    get_balance1 s amm_address = (AutomatedMarketMaker__reserve1 s) ->
    runStateT (FixedSupplyToken_transfer_opt amm_address swapAmount0 (make_machine_env_wrapped s trader)) s = Some (r, s') ->
    runStateT (AutomatedMarketMaker_simpleSwap0_opt trader (make_machine_env_wrapped s amm_address)) s' = Some (r', s'') -> 
    Z.lt (compute_k s) (compute_k s'')).
  Proof.
  intros.
  unfold get_balance0 in H0.
  unfold get_balance1 in H1.
  unfold AutomatedMarketMaker_simpleSwap0_opt in *.
  unfold FixedSupplyToken1_transfer_opt in *.
  unfold FixedSupplyToken_balanceOf_opt in *.
  unfold FixedSupplyToken1_balanceOf_opt in *.
  unfold FixedSupplyToken_transfer_opt in *.
  inv_runStateT.
  rewrite -> make_machine_env_caller_eq in *.
  rewrite -> make_machine_env_address_eq in *.
  subst.
  inv_arith.
  unfold compute_k.
  simpl.

  rewrite Int256Tree_reduce in *.
  rewrite Int256Tree_mreduce in *.
  repeat match goal with
  | [ |- context[AutomatedMarketMaker__reserve1 ?X]] => remember (AutomatedMarketMaker__reserve1 X) as R1
  end.
  repeat match goal with
  | [ |- context[AutomatedMarketMaker__reserve0 ?X]] => remember (AutomatedMarketMaker__reserve0 X) as R0
  end.
  match goal with 
  | [ |- context[Z.mul (Z.add ?X _) _]] => replace X with R0 (* rewrite -> H1 *)
  end.
  match goal with 
  | [ |- context[Z.sub ?X _]] => replace X with R1
  end.
  remember swapAmount0 as delta0.

  pose (IKM := increasing_k_math R0 R1 delta0 H0 H1 H); clearbody IKM.
  rewrite add_sub_inv.
  exact IKM.
  exact H2.
  Qed.

Lemma balance_tracks_reserve_inv : forall u s s', 
  u <> amm_address -> 
  get_balance0 s amm_address = (AutomatedMarketMaker__reserve0 s) ->
  get_balance1 s amm_address = (AutomatedMarketMaker__reserve1 s) -> 
  mstep_amm amm_address coinbase timestamp number balance blockhash prev_contract_state u s s' ->
  get_balance0 s amm_address = (AutomatedMarketMaker__reserve0 s') /\
  get_balance1 s amm_address = (AutomatedMarketMaker__reserve1 s').
Proof.
  intros u s s' Hun Hps0 Hps1 Hstp.
  unfold get_balance0 in *.
  unfold get_balance1 in *.
  destruct Hstp; rds. (* FIXME: rds should be able to automatically solve all the goals if we have 
                          machine_env updating implemented *)
Admitted.

Axiom anything : False.

(* splitting trades is strictly more expensive, if you split one trade into two trades with combined
    same amount, then you swapped for less alternative tokens *)
Theorem path_dependency : forall r r' rd rd' rd'' rd''' 
    (s s' s'' sd' sd'' sd''' sd'''' : state) 
    (trader : addr) 
    (swapAmount0 : Z32)
    (swapAmount0D0 swapAmount0D1 : Z32), 
    (swapAmount0D0 > 0)%Z ->
    (swapAmount0D1 > 0)%Z ->
    (swapAmount0D0 + swapAmount0D1 = swapAmount0)%Z ->
    (swapAmount0 > 0)%Z -> (* trasferring an amount > 0 *)
    ((AutomatedMarketMaker__reserve0 s) > 0)%Z -> 
    ((AutomatedMarketMaker__reserve1 s) > 0)%Z ->
    amm_address <> trader -> 
    get_balance0 s amm_address = (AutomatedMarketMaker__reserve0 s) -> (* assumes that the reserve is the same as balance at s *)
    get_balance1 s amm_address = (AutomatedMarketMaker__reserve1 s) ->
    (* swapping in one trade *)
    runStateT (FixedSupplyToken0_transfer_opt amm_address swapAmount0 (make_machine_env_wrapped s trader)) s = Some (r, s') ->
    runStateT (AutomatedMarketMaker_simpleSwap0_opt trader (make_machine_env_wrapped s amm_address)) s' = Some (r', s'') -> 
    (* swapping in two trades *)
    runStateT (FixedSupplyToken0_transfer_opt amm_address swapAmount0D0 (make_machine_env_wrapped s trader)) s = Some (rd, sd') ->
    runStateT (AutomatedMarketMaker_simpleSwap0_opt trader (make_machine_env_wrapped s amm_address)) sd' = Some (rd', sd'') -> 
    runStateT (FixedSupplyToken0_transfer_opt amm_address swapAmount0D1 (make_machine_env_wrapped s trader)) sd'' = Some (rd'', sd''') ->
    runStateT (AutomatedMarketMaker_simpleSwap0_opt trader (make_machine_env_wrapped s amm_address)) sd''' = Some (rd''', sd'''') -> 
    (* the ending balance is different *)
    (r' > rd' + rd''')%Z.
Proof.
  (* generic tactic *)
  intros.
  unfold get_balance0 in *.
  unfold get_balance1 in *.
  Time rds. (* finish in 182 secs, or 3m2s *)
  rewrite !H6. rewrite !H7. 
  remember (AutomatedMarketMaker__reserve0 m121) as a.
  remember (AutomatedMarketMaker__reserve1 m121) as b.
  remember (swapAmount0D0) as c.
  remember (swapAmount0D1) as d.
  rewrite !add_sub_inv. (* need inequality rewriting rules *)

  assert (d * 997 * R1 / (R0 * 1000 + d * 997) > d * 997 * R1 / (R0 * 1000 + d * 997 + d' * 997))%Z.
  assert (d' * 997 > 0)%Z. omega.
  match goal with
  | [ |- context[Z.gt (Z.div ?A ?B) (Z.div ?A (Z.add ?B ?C))]] => pose (HZInEq_denadd := ZInEq_denadd A B C H31); clearbody HZInEq_denadd
  end.
  exact HZInEq_denadd.
  apply H31.
  rewrite Zgt_trans.
  nia.
  unfold AutomatedMarketMaker_simpleSwap0_opt in *.
  unfold FixedSupplyToken1_transfer_opt in *.
  unfold FixedSupplyToken0_balanceOf_opt in *.
  unfold FixedSupplyToken1_balanceOf_opt in *.
  unfold FixedSupplyToken0_transfer_opt in *.

  Opaque bind ret get gets put modify guard mzero.

  Import MonadNotation.
  inv_runStateT1 H13.
  subst.
  inv_arith.
  simpl in *.

  inv_runStateT1 H12.
  subst.
  autorewrite with updates in *.
  rewrite !Int256Tree_reduce in *.
  simpl in *.
  inv_arith.

  inv_runStateT1  H11.
  subst.
  simpl in *.
  autorewrite with updates in *.
  inv_arith.
  rewrite !(@Int256Tree_Properties.get_default_so _ (0%Z) a25 a) in * by (exact H22).
  rewrite !Int256Tree_Properties.get_default_ss in *.

  inv_runStateT1 H9.
  subst.
  simpl in *.
  autorewrite with updates in *.
  inv_arith.

  inv_runStateT1 H8.
  subst.
  simpl in *.
  autorewrite with updates in *.
  rewrite !Int256Tree_Properties.get_default_ss in *.

  inv_runStateT1   H10.
  subst.
  simpl in *.
  autorewrite with updates in *.
  rewrite !Int256Tree_Properties.get_default_ss in *.

  (* These lines are to test if the resulting proof term can be checked reasonably quickly, but it seems it works. *)
  (* destruct anything.
  Time Qed. *)
  
Admitted.
