(*Add Rec LoadPath "../../cclib" as DeepSpec.cclib.
Add Rec LoadPath "../../lib" as DeepSpec.lib.
Add Rec LoadPath "../../core" as DeepSpec.core.
Add Rec LoadPath "../../backend" as DeepSpec.backend.*)

Require Import spblind.DataTypes.
Require Import spblind.DataTypeOps.
Require Import spblind.LayerBLINDAUCTION.
Require Import lib.Monad.StateMonadOption.
Require Import cclib.Maps.
Require Import cclib.Integers.
Require Import ZArith.
Require Import core.HyperTypeInst. 
(* for `hashvalue`, maybe should move that to some different file. *)
Require Import backend.MachineModel.


Section WithMem. (* This seems ugly, 
        it would be better to structure the compiler so we didn't 
        need a MemoryModelOps here? *)
Import core.MemoryModel.
(* Context {HmemOps: MemoryModelOps mem}.*)

Existing Instance GlobalLayerSpec.

Definition player_addr := int256.
Definition wei := Z.          (* for payments and balances. *)
Definition blocknumber := int256.

Record state : Type := {
 contract_state : global_abstract_data_type;
 player_transfers : list (player_addr * wei); (* others' transfer to contract *)
}.

(* For a given block, we know what the state of the contract was in the previous block,
   and we can call methods on the contract (in some state which we don't know exactly.
   It's possible to make a transaction of several calls that operate atomicall (this can
   be done by uploading a temporary contract.
   One thing that's currently not modelled is how to deal with multiple addresses under
   the control of a single principal, we should probably modify this to have a 
   "controlled by" relation. *)
Definition strategy : Type :=
  forall (prev_block: state)
         (n : blocknumber)
         (A:Type)
         (bid : wei -> hashvalue -> osT A unit)
         (reveal : wei -> int256 -> hashvalue -> osT A unit)
         (withdraw : wei -> osT A unit)
         (auctionEnd : wei -> osT A unit),
   osT A unit.

(* The entire proof is done with respect to a fixed finite set of players, who each
   have a private opinion about how much the item is worth. 
   We also give each player a nonce and an amount of capital to use when interacting
   with the contract; the exact values of these will not affect the correctness. *)
Context (players : list player_addr).
Record player : Type := mk_Player 
  { valuation: int256; secret: hashvalue; capital_proof: wei}.
Context (player_info : player_addr -> player).

(* The strategy of all players; in practice we will only call it on addresses in `players`. *)
Definition strategy_tuple := player_addr -> strategy.

Instance MosT_osT_state : Monad (osT state) := MosT state.
Instance MonadState_osT_state :MonadState state (osT state) := MonadState_osT state.
Instance MonadZero_osT_state : MonadZero (osT state) := MonadZero_osT state.

Section step.
  (* These are the parameters which are constant for the entire 
     execution. *)
  Context (strategies : strategy_tuple)
          (initial_balances : player_addr -> Z)
          (contract_address : int256).
  

  (* The amount that is transferred from the auction contract
     to the player p. *)
  Definition outgoing_transfers (p : player_addr)
                                (ts: list Transfer) :=
    List.fold_left (fun z t =>
                      if Int256.eq (_to t) p
                      then (Int256.intval (_amount t) + z)%Z
                      else z)
                   ts 0%Z.

  (* The amount that is transferred from player p to the auction contract. *)
  Definition incoming_transfers (p : player_addr)
                                (ts: list (player_addr * wei)) :=
    List.fold_left (fun z t =>
                      if Int256.eq (fst t) p
                      then ((snd t) + z)%Z
                      else z)
                   ts 0%Z.

  Definition current_balances (incoming : list (player_addr * wei))
                              (outgoing : list Transfer)
                              (p : player_addr) : int256 :=
    Int256.repr (initial_balances p
                 - incoming_transfers p incoming
                 + outgoing_transfers p outgoing).

  Section mstep.
  (* These are the parameters which are constant within a given block. *)
  Context (coinbase : int256)
          (timestamp : int256)
          (number : blocknumber)
          (blockhash : int256 -> int256)
          (prev_contract_state : state).

  Definition make_machine_env (addr : int256)
                              (callvalue : wei)
                              (incoming : list (player_addr * wei))
                              (outgoing : list Transfer)
       : machine_env GetHighData                             
    := {| me_address := contract_address;
          me_origin := addr;
          me_caller := addr;
          me_callvalue := Int256.repr (callvalue);
          me_coinbase := coinbase;
          me_timestamp := timestamp;
          me_number := number;
          me_balance := current_balances incoming outgoing;
          me_blockhash := blockhash;
          me_transfer _ _ _ _ _ := False;
          me_callmethod _ _ _ _ _ _ _ _ _ _ := False;
          me_log _ _ d := d
       |}.

  Print machine_env.

  (* "lifting" the semantics of a method to act on the the entire 
     world state. 
     Perhaps this could be better done by using some monad combinators.
   *)

  Require Import lib.Monad.RunStateTInv.
  Import lib.Monad.Monad.MonadNotation.
  
  Require Import List.

  Definition lif {A:Type}
                 (addr : player_addr)
                 (callvalue : wei) 
                 (cmd : machine_env GetHighData -> osT global_abstract_data_type A)
    : osT state A :=
    st  <- get;;
    let incoming' := (addr, callvalue) :: (player_transfers st) in
    let me := make_machine_env addr callvalue incoming' (_events (contract_state st)) in 
    match runStateT (cmd me) (contract_state st) with
    | None => mzero
    | Some (v, st') => put (Build_state st' incoming');; ret v
    end.

  Definition lifted_init :=
    fun (p_addr: player_addr) (w: wei) (bt rt: int256) => lif p_addr w 
      (BlindAuction_initialize_opt bt rt).

  Lemma lif_inv : forall A addr callvalue (cmd : machine_env GetHighData -> osT global_abstract_data_type A)
                                             st v st',
      runStateT (lif addr callvalue cmd) st = Some (v, st') ->
      exists cst',
       let incoming' := (addr, callvalue) :: (player_transfers st) in
       let me := make_machine_env addr callvalue incoming' (_events (contract_state st)) in
       runStateT (cmd me) (contract_state st) = Some (v, cst')
       /\ st' = (Build_state cst' incoming').
  Proof.
    unfold lif.
    intros.
    inv_runStateT; subst.
    destruct (runStateT
             (cmd
                (make_machine_env addr callvalue
                   ((addr, callvalue) :: player_transfers m)
                   (_events (contract_state m)))) (contract_state m));
      [ | simpl in *; congruence].
    destruct p.
    inv_runStateT.
    subst.
    eexists; split; reflexivity.
  Qed.
  
  (* How the state of the system changes in a single method call made by player p. *)
  Definition mstep p (st st' : state) : Prop := 
    exists retval,
      runStateT ((strategies p prev_contract_state number state
                             (fun w hv => lif p w (BlindAuction_bid_opt hv))
                             (fun w v hv => lif p w (BlindAuction_reveal_opt v hv))
                             (fun w => lif p w BlindAuction_withdraw_opt)
                             (fun w => lif p w BlindAuction_auctionEnd_opt))
                         : osT state unit) st
      = Some (retval, st').

  (* We can compute a new block by letting the players call the contract 
     in some arbitrary order. *)
  Inductive multi_mstep : state -> state -> Prop := 
  | multi_mstep_reflexive : forall (st : state), multi_mstep st st
  | multi_mstep_transitive : forall (st st' st'' : state) p,
      In p players ->
      multi_mstep st st' -> mstep p st' st'' -> multi_mstep st st''.


  (* A block is sufficiently synchronous if every player got a chance to submit a 
     transaction to to. *)
  (* TODO, I think this definition is fine for now, but it seems little too clever,
     should probably re-state this using some straightforward tracking of the states
     we pass through. *)
  Definition multi_mstep_synchronous st1 st2 :=
    forall p, In p players ->
              exists st st',
                multi_mstep st1 st /\ mstep p st st' /\ multi_mstep st' st2.

  (* Here are a bunch of induction principles inspired by linear temporal logic. *)

  (* Prove that some property P holds "globally", i.e. for each state along a 
     path.
     You can also specify a property Pprev which is known to hold for the
     prev_contract_state. If not needed, it can just be True.
 *)
  Lemma multi_mstep_Global_ind : forall (Pprev P : state -> Prop),
      (forall p st st', Pprev prev_contract_state -> P st -> In p players -> mstep p st st' -> P st') ->
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
  | bstep_step : forall coinbase timestamp blockhash st st',
      multi_mstep coinbase timestamp n blockhash st st st' ->
      bstep n st st'.

  Inductive multi_bstep : blocknumber -> state -> blocknumber -> state -> Prop := 
  | multi_bstep_reflexive : forall n (st : state), multi_bstep n st n st
  | multi_bstep_transitive : forall n n'  (st st' st'' : state),
    multi_bstep n st n' st' -> bstep (Int256_incr n') st' st'' -> multi_bstep n st (Int256_incr n') st''.
  
  (* multi_bstep is the step relation without any synchronicity assumption.
     This is sufficient to prove some safety properties, but for most interesting 
     theorems we instead need to use this synchronous version: *)

  Inductive bstep_synch (n : blocknumber) : state -> state -> Prop :=
  | bstep_synch_step : forall coinbase timestamp blockhash st st',
      multi_mstep_synchronous coinbase timestamp n blockhash st st st' ->
      bstep_synch n st st'.

  Inductive multi_bstep_synch : blocknumber -> state -> blocknumber -> state -> Prop := 
  | multi_bstep_synch_reflexive : forall n (st : state), multi_bstep_synch n st n st
  | multi_bstep_synch_transitive : forall n n'  (st st' st'' : state),
    multi_bstep_synch n st n' st' -> bstep_synch (Int256_incr n') st' st'' -> multi_bstep_synch n st (Int256_incr n') st''.

  Lemma multi_bstep_Global_ind : forall (P : state -> Prop),
      (forall coinbase timestamp number blockhash prev_block p st st',
          P prev_block
          -> P st
          -> In p players
          -> mstep coinbase timestamp number blockhash prev_block p st st'
          -> P st')
      -> forall n st n' st',
        P st -> multi_bstep n st n' st' -> P st'.
  Proof.
    induction 3.
    - auto.
    - inversion H2; subst.
      eapply multi_mstep_Global_ind with (st:=st') (prev_contract_state := st').
      + intros.
        refine (H _ _ _ _ _ _ _ _ _ _ _ H7); auto.
      + apply IHmulti_bstep; assumption.
      + apply IHmulti_bstep; assumption.
      + exact H3.
  Qed.
End step.

Section Transactions.
  Context (p: player_addr) (st: state).

  Definition player_outgoing : Z :=
    outgoing_transfers p (_events (contract_state st)).

  Definition player_incoming : Z :=
    incoming_transfers p (player_transfers st).

  (* negative if player payed others, positive if received more *)
  Definition net_gain : int256 := 
    Int256.repr (
      player_outgoing - 
      player_incoming).
End Transactions.

Definition is_where_player_won (p: player_addr) (o: state) : bool :=
  Int256.eq p (_highestBidder (contract_state o)).

Definition outcome_spec (p : player_addr) (st : state) : Prop :=
  (is_where_player_won p st = true -> 
    (Int256.intval (valuation (player_info p)) + Int256.intval (net_gain p st) = 0)%Z)
  /\
  (is_where_player_won p st = false -> 
    (Int256.intval (net_gain p st) = 0)%Z).

(* what we wish to show is to first provide the mechanics of the game, then prove the 
   following `main lemma`:
   "if the user follows that strategy (this specification of his behavior)
   , then he either wins and pays at most that value,
   or he gets money back".
   Then we define a comparison function for states, saying which states the user
   prefers, i.e., defining the utility function for each player, thus proving that
   the strategy we chose lead to the set of outcomes that is weakly dominating.
*)

Definition gt := fun (a : int256) (b : int256) => 
                  match (Int256.ltu a b) with
                  | true => false
                  | false => true
                  end.

Definition keccak : hashvalue -> hashvalue -> hashvalue := hashval_hash2.

(* The phases of the contract *)
Definition is_bidding_phase (n : blocknumber) (b: state) :=
  Int256.lt n (_biddingEnd b.(contract_state)).

Definition already_bidded (p: player_addr) (b: state) := 
  match Int256Tree.get p (_bids b.(contract_state)) 
  with
  | None => false
  | Some _ => true
  end.

Definition is_reveal_phase (n:blocknumber) (b: state) :=
  andb
    (Int256.lt n (_revealEnd b.(contract_state)))
    (gt n (_biddingEnd b.(contract_state))).

Definition is_auction_finished_phase (n : blocknumber) (b: state) :=
  (gt n (_revealEnd b.(contract_state))).

(* prove the safety of this contract, i.e. prove that the benificiary always get payed *)
(* we need an assumption that `capital_proof > p.(valuation)` *)
Definition bid_own_valuation (p : player_addr) : strategy := 
  (fun (prev_block : state)
       (n : blocknumber)
       (A : Type)
       (bid : wei -> hashvalue -> osT A unit)
       (reveal : wei -> int256 -> hashvalue -> osT A unit)
       (withdraw : wei -> osT A unit)
       (auctionEnd : wei -> osT A unit) =>  
  if andb (is_bidding_phase n prev_block) (negb (already_bidded p prev_block))
  then
    bid (player_info p).(capital_proof)
        (keccak (hashval_int256 (player_info p).(valuation)) (player_info p).(secret))
  else if is_reveal_phase n prev_block
  then
    reveal 0%Z (player_info p).(valuation) (player_info p).(secret)
  else if is_auction_finished_phase n prev_block
  then
    withdraw 0%Z
  else
    ret tt).

(* The equilibrium strategy tuple. We will prove that this is a Nash equilibrium. *)
Definition ess : strategy_tuple :=
  fun p => bid_own_valuation p.

Definition sufficient_capital_proof (p: player) : Prop := 
  eq_true (gt (Int256.repr p.(capital_proof)) p.(valuation)).

Definition bounded_valuation (p: player) : Prop := 
  Z.lt 0%Z (Int256.intval p.(valuation)) /\ 
  Z.lt (Int256.intval p.(valuation)) Int256.max_signed.


(* The idea is to say for any player, if the contract starts at the initial state,
   then operate according to the `multi-bstep` relation, then in the final state, 
   which is when the auction is over, the list generated by the `transfer` must give
   an outcome that satisfy a predicate "player wins and pay at most value, or get 
   money back" 
*)

Transparent BlindAuction_initialize_opt
            BlindAuction_transferb_opt
            BlindAuction_reveal_opt
            BlindAuction_bid_opt
            BlindAuction_withdraw_opt
            BlindAuction_auctionEnd_opt

            builtin0_address_impl builtin0_origin_impl builtin0_caller_impl builtin0_callvalue_impl builtin0_coinbase_impl builtin0_timestamp_impl builtin0_number_impl builtin1_balance_impl  builtin1_blockhash_impl.



Section outcome_proof.
  Context (* (strategies : strategy_tuple) *)
          (initial_balances : player_addr -> Z)
          (contract_address : int256).

  Context (init_bt init_rt : int256)
          (init_coinbase : int256)
          (init_timestamp : int256)
          (init_number : int256)
          (init_blockhash : int256 -> int256)
          (pre_init_state init_state : state).
 

  Definition contract_owner_addr : player_addr := Int256.zero.
  Definition contract_init_price : wei := 0%Z.
  Definition outcome := state.
  
  Definition initial_state :=
     {|
    contract_state := {|
                      _beneficiary := contract_owner_addr;
                      _biddingEnd := Int256.add init_number init_bt;
                      _revealEnd := Int256.add (Int256.add init_number init_bt)
                                      init_rt;
                      _ended := false;
                      _bids := Maps.Int256Tree.empty Bid;
                      _highestBidder := contract_owner_addr;
                      _highestBid := Int256.repr 0;
                      _secondBid := Int256.repr 0;
                      _revealed := Maps.Int256Tree.set contract_owner_addr true
                                     (Maps.Int256Tree.empty bool);
                      _amountOf := Maps.Int256Tree.empty int256;
                      _events := nil |};
    player_transfers := (contract_owner_addr, 0%Z) :: nil |}.

  (* We note that the above is indeed the state produced by the contract constructor. *)
  Remark inital_state_correct :
    runStateT (lifted_init 
                 initial_balances 
                 contract_address 
                 init_coinbase init_timestamp init_number init_blockhash 
                 contract_owner_addr contract_init_price init_bt init_rt)
              (Build_state init_global_abstract_data nil)
    = Some (tt, initial_state).
  Proof.
    reflexivity.
  Qed.  

  Require Import lib.Monad.RunStateTInv.
  Import lib.Monad.Monad.MonadNotation.
  
  Ltac inv_ess :=
    repeat match goal with
           | [H : runStateT (if ?b then _ else _) _ = Some _ |- _] => destruct b eqn:?
           | [H : runStateT (lif _ _ _ _ _ _ _ _ ?cmd) _ = Some (_, _) |- _] =>
             apply lif_inv in H; destruct H as [? [? ?]]
           end.

  Require Import MonadState.

  
  (* We reason entirely in terms of monad lemmas. *)
  Opaque bind ret get gets put modify guard mzero.
  Transparent  BlindAuction_bid_opt BlindAuction_reveal_opt EVMOpcode_transfer_opt BlindAuction_withdraw_opt.
  
  (* Functional correctness proofs. *)
  Definition No_extra_bids (st : state) : Prop :=
    forall p, ~ In p players ->
              Int256Tree.get p st.(contract_state).(_bids) = None.

  Lemma no_extra_bids : forall n o, 
      multi_bstep ess initial_balances contract_address init_number initial_state n o ->
      No_extra_bids o.
  Proof.
    assert (No_extra_bid_initial : No_extra_bids initial_state).
    {
      unfold No_extra_bids, initial_state; simpl.
      intros; rewrite Int256Tree.gempty; reflexivity.
    }
    intros; eapply multi_bstep_Global_ind with (strategies:=ess).
    - intros coinbase timestamp number blockhash prev_bloc p st st' IHprev IHst Hin Hmstep.
      inversion Hmstep as [retval Hrun]; clear Hmstep.
      unfold No_extra_bids.
      intros p' Hnot_in.
      destruct (Int256.eq_dec p p') as [Heq | Hneq]; [subst; contradiction |].      
      unfold ess, bid_own_valuation in Hrun.
      inv_ess.
      + (* bid method. *)
        unfold BlindAuction_bid_opt, EVMOpcode_transfer_opt in *.
        simpl in H0.
        inv_runStateT_branching.
        {
          subst. simpl.
          rewrite Int256Tree.gso by congruence.
          rewrite Int256Tree.gso by congruence.
          apply IHst.
          auto.
        }
        {
          subst. simpl.
          rewrite Int256Tree.gso by congruence.            
          apply IHst.
          auto.
        }
      + (* reveal method *)
        unfold BlindAuction_reveal_opt in H0.
        simpl in H0.
        inv_runStateT_branching.
        {
          subst. simpl.
          apply IHst; auto.
        }
        { subst. simpl.
          apply IHst; auto. }
        { subst. simpl.
          apply IHst; auto. } 
        { subst. simpl.
          apply IHst; auto. }
        { subst. simpl.
          apply IHst; auto. }
     +  unfold BlindAuction_withdraw_opt, EVMOpcode_transfer_opt in H0.
        simpl in H0.
        inv_runStateT_branching.
        subst. simpl.
        apply IHst; auto.
     + (*  some left-over thing from the the inv_ess... *)
       inv_runStateT.
       subst.
       apply IHst; auto.
    - exact No_extra_bid_initial.
    - eassumption.
  Qed.
  
  Definition has_placed_bid (p : player_addr) (st : state) : Prop :=
    Int256Tree.get p st.(contract_state).(_bids)
    = Some {| _blindedBid := (keccak (hashval_int256 (valuation (player_info p)))
                                     (secret (player_info p)));
              _deposit := Int256.repr (capital_proof (player_info p)) |}.

          Require Import lib.ArithInv.
  
  Lemma has_placed_bid_preserved : forall p n_i i n_o o,
      has_placed_bid p i ->
      multi_bstep ess initial_balances contract_address n_i i n_o o ->
      has_placed_bid p o.
  Proof.     
    intros; eapply multi_bstep_Global_ind with (strategies:=ess).
    - intros coinbase timestamp number blockhash prev_bloc p0 st st' IHprev IHst Hin Hmstep.
      inversion Hmstep as [retval Hrun]; clear Hmstep.
      unfold ess, bid_own_valuation in Hrun.
      inv_ess.
      + (* bid method. *)
        destruct (Int256.eq_dec p p0).
        * (* a method call by player p. *)
          unfold BlindAuction_bid_opt, EVMOpcode_transfer_opt in *.
          simpl in H1.
          inv_runStateT_branching.
          {
            (* The case where we place a bid. *)
            subst. simpl.
            unfold has_placed_bid.
            simpl.
            (* p is placing a bid, so _bids[p] has the right value. *)
            subst.
            rewrite Int256Tree.gss.
            rewrite Int256Tree_Properties.get_default_ss.
            reflexivity.
          }
          {
            (* The already_bidded test was false, which contradicts our IH. *)
            subst. simpl.
            inv_arith.
            contradict H2.
            unfold already_bidded.
            rewrite IHprev.
            discriminate.
          }
        * (* a method call by some other player p0. *)
          unfold BlindAuction_bid_opt, EVMOpcode_transfer_opt in *.
          simpl in H1.
          inv_runStateT_branching;
            subst;
            unfold has_placed_bid; simpl;
            rewrite !Int256Tree.gso by congruence;
            apply IHst.
      + (* reveal method *)
        unfold BlindAuction_reveal_opt in H1.
        simpl in H1.
        inv_runStateT_branching;
          subst; simpl; apply IHst.
      + (* withdraw method. *)
        unfold BlindAuction_withdraw_opt, EVMOpcode_transfer_opt in H1.
        simpl in H1.
        inv_runStateT_branching;
          subst; simpl; apply IHst.
      + (*  some left-over thing from the the inv_ess... *)
        inv_runStateT.
        subst.
        apply IHst; auto.
    - eassumption.
    - eassumption.
  Qed. 


(***********************************************************************
  Work in progress, the rest of the file does not compile from here on.
 ***********************************************************************)  

  Lemma has_placed_bid_phase1 :
   multi_mstep_synchronous ess inital_balances constract_address 
    
  
Lemma has_placed_bid_preserved : forall p n_i i n_o o,
      has_placed_bid p i ->
      multi_bstep ess initial_balances contract_address n_i i n_o o ->
      has_placed_bid p o.
  
  
(* Proof of Nash equilibrium *)

  
  Definition outcome_from_strategies (o: outcome) (stg: strategy_tuple)
    :=
      exists n, 
      multi_bstep stg initial_balances contract_address init_number initial_state n o
      /\ is_true (is_auction_finished_phase n o).
    
  Definition use_stg (p: player) (s: player -> strategy) 
    (ss: strategy_tuple): Prop :=
      ss p.(addr) = s p.

  Definition sound_player (p: player): Prop :=
    sufficient_capital_proof p /\ bounded_valuation p.

  (* say which state the player prefers, ordinal *)
  Definition selfish_greedy_utility (p : player) (o: outcome) : Z := 
    (* if outcome is p get object, then payoff is p.(valuation) - final_price *)
    if Int256.eq (addr p) (_highestBidder (contract_state o)) then
      Int256.intval p.(valuation) + Int256.intval (net_gain p o)
    (* if outcome is p not getting object, then payoff is zero *)
    else
      (* since we already prove refund_failed_bidders, no 
         need for additional substraction of net_gain *)
      0%Z.

  (* lemma refund_failed_bidders: saying that failed people get their money back *)
  (* fold both incoming and outcoming lists, assert both directions p have 
      same amount transferred *)

  Require Import DeepSpec.lib.Monad.RunStateTInv.

  (* prove this for bid_own_valuation only *)
  (* mstep with the runStateT, say that the state when we call bid, P st, then mstep st st;
      P2 st', and st' satisfy p2 for runStateT *)
  Lemma refund_failed_bidders : forall p o ss,
      sound_player p -> 
      use_stg p bid_own_valuation ss ->
      outcome_from_strategies o ss -> 
      is_where_player_won p o = false -> 
      player_outgoing p o = player_incoming p o.
  Proof.
    intros.
    Print is_where_player_won.
    Print outcome_from_strategies.
    unfold outcome_from_strategies in  H1.
    Print inv_runStateT.
    (************************************************************************************************)
    (* decompose the functions to individual prepost by runStateT, specialize for theorems to prove *)
    (************************************************************************************************)
    Admitted.

  (* lemma consume_succ_bidders: saying that succed people never get refunded *)
  (* fold on outcoming_list of transactions, assert there is none to p *)
  Lemma consume_succ_bidder_money : forall p o ss,
      sound_player p -> 
      outcome_from_strategies o ss -> 
      is_where_player_won p o = true -> 
      player_outgoing p o = 0%Z.
  Proof.
    intros.
    Admitted.

    Lemma bid_own_valuation_win_case:
      forall (o: outcome) (p: player) (ss: strategy_tuple),
      use_stg p bid_own_valuation ss ->
      outcome_from_strategies o ss ->
      is_true (is_where_player_won p o) -> 
      (Int256.intval (_highestBid o.(contract_state))) = Int256.intval (valuation p).
    Proof.
      intros.
      Admitted.

  (* destruct outcome_spec *)
    (* p fails: refund_failed_bidders, unfold net_gain definition, 
        net_gain = 0 *)
    (* p succ: consume_succ_bidder_money, with bid_own_valuation, 
        he wins, and get no money back, net_gain = -valuation *)
  Lemma bid_own_valuation_outcome (p: player) (o : outcome) (ss: strategy_tuple): 
      sound_player p -> 
      use_stg p bid_own_valuation ss ->
      outcome_from_strategies o ss -> 
      outcome_spec p o.
  Proof.
  intros.
  unfold use_stg in H0.
  unfold outcome_spec.
  split;
  intros;
  unfold net_gain.
  (* p wins *)
  + pose (OZ:=consume_succ_bidder_money p o ss H H1 H2); clearbody OZ.
    rewrite -> OZ.
    simpl.
    (* prove that with H0, o will only have player_incoming being valuation of p *)
    admit.
  (* p does not win *)
  + pose (OZ:=refund_failed_bidders p o ss H H0 H1 H2); clearbody OZ.
    rewrite -> OZ.
    assert (ZL:forall f, Z.eq (f - f) 0%Z).
    - intros. exact (Z.sub_diag f).
    - pose (Z:=ZL (player_incoming p o)); clearbody Z.
      rewrite -> Z. constructor.
  Admitted.
  
  (* just apply selfish_utility to bid_own_valuation_outcome *)
  (* lemma never_overpays: following stg, p's utility = 0 *)
    (* p fails: utility = 0 *)
    (* p succ: consume_succ_bidder_money, with bid_own_valuation, then 
        utility = valuation + net_gain = 0 *)
  Lemma bid_own_valuation_never_overpays: forall p o ss,
      sound_player p -> 
      use_stg p bid_own_valuation ss ->
      (* strategies p.(addr) = bid_own_valuation p ->  *)
      outcome_from_strategies o ss -> 
      (0 = (selfish_greedy_utility p o))%Z.
  Proof.
    intros.
    unfold selfish_greedy_utility.
    pose (OT:=bid_own_valuation_outcome p o ss H H0 H1); clearbody OT.
    unfold outcome_spec in OT.
    destruct OT.
    destruct (is_where_player_won p o) eqn:?;
    unfold is_where_player_won in Heqb;
    rewrite -> Heqb. 
    + pose (H4:=H2 eq_refl); clearbody H4.
      rewrite -> H4.
      constructor.
    + compute.
      constructor.
  Qed.

  Section equilibrium_proof.

    Print outcome_from_strategies.
    (* non-determinism of states, included in bstep, assumed miner fairness *)
    (* Lemma: all non-deterministic outcomes by bstep have same payoff *)

    Definition weakly_dominant (stg: player -> strategy) (p: player)
                               (utility_func: player -> outcome -> Z) :=
      forall (o o': outcome) (ss ss': strategy_tuple) (p': player),
        p' <> p ->
        ss p'.(addr) = ss' p'.(addr) -> 
        use_stg p stg ss' -> 
        outcome_from_strategies o ss -> 
        outcome_from_strategies o' ss' ->
        ((utility_func p o) <= (utility_func p o'))%Z.

    Check fun o => (_bids (contract_state o)).

    (* since the bid amounts are hashed, we essentially cannot tell how much
        other people bidded easily *)

    Definition gmapv {A} (p: player) (b: state) 
      (fd: global_abstract_data_type -> Maps.Int256Tree.t A) := 
      Maps.Int256Tree.get p.(addr) (fd b.(contract_state)).

    (* Print keccak.

    (* Define the reverse hash function, just to make expressing bid
        values easier *)
    Definition keccak_i := (fun a b : hashvalue => hashval_hash2 a b) :
      hashvalue -> hashvalue -> hashvalue.

    Axiom inverse_func_keccak : forall a b, keccak_i (keccak a b)

    Definition keccak_r (bid_hashed p o).(_blindedBid) secret

    Definition  keccak value secret = (bid_hashed p o).(_blindedBid). *)

    (* prove that at one point inside o',
              some other player put a bid, then we use the syncronicity assumption 
              on the assumption of o plus the assumption about bids being blind to 
              show that the same player also put a bid in the execution leading to o, 
              and then we have some "lemma" saying that with that bid being there, 
              that player will either win or we will pay to much.
              prove that the only way to win in this scenario is overpay!
              - "if a player places a bid such that ... then the outcome will be 
              ..." (a bid_outcome lemma, no strategies involved) *)

    (* bid is blind, so that your new bid will not affect other player's bids 
        for every player playing according to a certain strategy, even if 
        everybody else's strategy changed, he will always perform the same
        kind of action (function calls), his call to bid_function will be same *)
    Definition blind_bids (p p': player) (o o': outcome) (ss ss': strategy_tuple)
        (s s': player -> strategy)
      :=
      p <> p' ->
      ss p'.(addr) = ss' p'.(addr) -> 
      use_stg p s' ss' -> 
      use_stg p s ss -> 
      outcome_from_strategies o ss -> 
      outcome_from_strategies o' ss' ->
      gmapv p' o _trueBids = gmapv p' o' _trueBids /\
      gmapv p' o _secrets = gmapv p' o' _secrets /\ 
      gmapv p' o _bids = gmapv p' o' _bids.

    Definition bid_amount := fun p o => match gmapv p o _trueBids with 
      | Some (v) => v
      | None => Int256.zero
      end.

    (* given an outcome where player p wins, we extract the winning bid *)
    (* a bid function call b' on state, defined using mstep *)
    (* Definition exists_winning_bid: is_where_player_won p o ->  . *)
    (* predicate on state *)


    (* If you won, it would be because you paid more than others *)
    Lemma bidding_pv_functional_win:
      forall (o: outcome) (p p': player) (ss: strategy_tuple),
      p <> p' ->
      use_stg p bid_own_valuation ss ->
      outcome_from_strategies o ss ->
      is_true (is_where_player_won p o) -> 
      Z.ge
        (Int256.intval (bid_amount p o))
        (Int256.intval (bid_amount p' o)).
    Proof.
      intros.
    Admitted.

    Lemma bidding_pv_functional_lose:
      forall (o: outcome) (p: player) (ss: strategy_tuple),
      use_stg p bid_own_valuation ss ->
      outcome_from_strategies o ss ->
      not (is_true (is_where_player_won p o)) -> 
      exists (p': player),
      p <> p' ->
      Z.ge
      (Int256.intval (bid_amount p' o))
      (Int256.intval (bid_amount p o)).
    Proof.
      intros.
    Admitted.

    (* prove that if given exists_winning_bid b' p', the only way to win the 
        auction for player p <> p' is to place bid b >= b' *)
    Lemma winning_bid_monotonicity: 
      forall 
        (o o': outcome) 
        (p p': player) 
        (ss ss': strategy_tuple)
        (s': player -> strategy), 
      p <> p' ->
      ss p'.(addr) = ss' p'.(addr) -> 
      use_stg p s' ss' -> 
      use_stg p bid_own_valuation ss -> 
      outcome_from_strategies o ss -> 
      outcome_from_strategies o' ss' ->
      bid_amount p' o = bid_amount p' o' ->
      is_true (is_where_player_won p o) -> 
      is_true (is_where_player_won p o') -> 
      blind_bids p p' o o' ss ss' bid_own_valuation s' ->
      Z.ge
      (Int256.intval (_highestBid o'.(contract_state)))
      (Int256.intval (_highestBid o.(contract_state))).
    Proof.
      intros.
      unfold is_where_player_won in H.
      pose (HB:=(bid_own_valuation_win_case o p ss H2 H3 H6)); clearbody HB.
      rewrite -> HB.
      unfold blind_bids in H8.
      pose (HBD:=(H8 H H0 H1 H2 H3 H4)); clearbody HBD.
      clear H8.
      (* need runStateT and bidding_pv_functional and HBD, HBD is for o and o' cases, and the 
        bidding_pv_functional is for one case *)
      (* bidding_pv_functional that in o' you bid higher than everybody *)
      (* bidding_pv_functional that in o you bid higher than everybody *)
      (* HBD that everybody bidded the same value *)
      Admitted.

    (* assuming player is sound, then *)
    Theorem second_price_auction_equilibrium: forall (p: player), 
      sound_player p -> 
      weakly_dominant bid_own_valuation p selfish_greedy_utility.
    Proof.
      intros.
      unfold weakly_dominant.
      (* given another player's strategy, the corresponding outcome 
         from bid_own_valuation is better than outcome from other 
         strategies. *)
      intros.
      destruct (is_where_player_won p o') eqn:?;
      unfold is_where_player_won in Heqb;
      unfold selfish_greedy_utility;
      rewrite -> Heqb;
      destruct (is_where_player_won p o) eqn:?;
      unfold is_where_player_won in Heqb0;
      rewrite -> Heqb0.
      (* case analysis on (o'-o) *)
      - (* "succ-succ":
          if o is success, then 
          we prove that o overpays (restrict other player's
          behavior so we get corresponding outcomes), 
          direct apply lemma overpaied_win_given_bid *)
        (* if p win in o and p win in o', then as p plays according to
            bid_own_valuation, exists winning_bid p o' = p.(valuation).fa
            and there also exists winning_bid p o, then we assert that
            for every player p' <> p, he will have same action in o as in o',
            "actually this part is false, there exists strategies that 
              are better than bid_own_valuation, while also wins"
            so we assume blind_bids (ss p), then ??? *)

        (* need winning_bid_monotonicity lemma, but that lemma cannot be proven! *)
        admit.
      - (* "succ-fail":
          apply the outcome lemma that following stg give =0 utility,
            then it is trivially true *)
        pose (UZ:=bid_own_valuation_never_overpays p o' ss' H H2 H4).
        clearbody UZ.
        unfold selfish_greedy_utility in UZ.
        rewrite -> Heqb in UZ.
        left.
        exact UZ.
      (* we prove that for whatever
        other strategy we could have used, we would still either lose or 
        overpay (use utility function to formulate this). *)
      - (* "fail-succ":
            we prove that the succ case will overpay, thus have negative utility.
            apply lemma overpaied_win_given_bid
        *)
        (* apply overpaied_win_given_bid. *)
        (* if p did not win in o', we must have (assume)
            since p played according to bid_own_valuation and failed,
            the winning_bid by pw must >= p.(valuation), and
            exists_winning_bid pw o' o
            for player pw <> p who won the auction in o', he had same action in o,
            given those assumptions, if p wants to win
            in o, he must bid more (by monotonicity lemma)
            than pw's bid in o, which is the same 
            as pw's bid in o', which is larger than p.(valuation), which 
            causes overpay *)
        (* since by definition in o' we have a bid pw > p.(valuation), and since 
            there is also this pw bid in o, then winning bid in o > pw, then according to
            selfish_greedy_utility, the utility is negative, omega *)

        (*  *)

        admit.
      - (* "fail-fail":
          trivial, 0 leq 0 *)
        omega.
    Admitted.
    
  End equilibrium_proof.

End outcome_proof.
