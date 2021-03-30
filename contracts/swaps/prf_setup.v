Require Export swaps_threeway.DataTypes.
Require Export swaps_threeway.DataTypeOps.
Require Export swaps_threeway.LayerSWAPCONTRACT.
Require Export cclib.Integers. 
Require Export cclib.Maps.
Require Export ZArith.
Require Export core.HyperTypeInst.
Require Import core.MemoryModel.
Require Export lib.Monad.StateMonadOption.
Require Export backend.MachineModel.
Require Export lib.ArithInv.

Existing Instance GlobalLayerSpec.

Definition player_addr := int256.
Definition wei := Z.

Record contract := (* represented by a weighted edge *)
  { party: player_addr; 
    counter_party: player_addr;
    amount: int256;}.


(* three players, A -> B -> C -> A, where A is the leader. 
  The behaviors of the players are not symmetric. *)
(* Inductive player_type := player_A | player_B | player_C. *)

(* Record player : Type := 
  { addr: player_addr; secret (* private key *): int256; capital_proof: wei;
    neighbors_incoming: list player_addr; neighbors_outgoing: list player_addr;
    tp: player_type}. *)

Record player : Type := 
  { addr: player_addr; secret (*private key*): int256;
    neighbors_incoming: list (player_addr * int256);
    neighbors_outgoing: list (player_addr * int256);}.

Record state : Type := Build_state {
	contract_state : global_abstract_data_type;
  blocktimestamp : int256; (* check *)
  player_transfers : list (player_addr * wei);
  contracts_published: list contract;
  secrets_revealed: list (player_addr * int256);
}.

(* outcome of all possible choices at a given time, 
  where the player knows both the current state and the previous block *)
Definition strategy : Type :=
  forall (prev_block:state)
  (A:Type)
  (refined_prev_block:A)
  (unlock : wei -> int256 -> osT A unit)
  (claim : wei -> osT A unit)
  (refund : wei -> osT A unit)
  (publish_contracts: wei -> list (player_addr * int256) -> osT A unit), osT A unit.


Definition strategy_tuple := player -> strategy.

(* Relevant definitions pasted; delete after complete.
    Record stateT (t : Type) : Type := mkStateT
    { runStateT : S -> m (t * S)%type }.
    Definition osT := @stateT D option.

  (osT state A) -> state monad with state type state and return value type A.
  available methods: put, ret, get (with the types as implied from the following instances.
*)
Global Instance MosT_osT_state : Monad (osT state) := MosT state.
Global Instance MonadState_osT_state :MonadState state (osT state) := MonadState_osT state.
Global Instance MonadZero_osT_state : MonadZero (osT state) := MonadZero_osT state.

(* Basic step simulations. *)
Section step.

  Context (strategies: strategy_tuple)
          (initial_balances: player_addr -> Z)
          (contract_address: int256).

  (* The amount that is transferred from contract to player *)
  Definition transfer_con2player (p: player_addr)
                                (ts: list Transfer) := 
  List.fold_left (fun z t => 
                    if Int256.eq (_to t) p
                    then (Int256.intval (_amount t) + z)%Z
                    else z) 
                 ts 0%Z.

  (* The amount that is transferred from player to contract *)
  Definition transfer_player2con (p: player_addr)
                                (ts: list (player_addr * wei)) := 
  List.fold_left (fun z t => 
                    if Int256.eq (fst t) p
                    then ((snd t) +z)%Z
                    else z)
                 ts 0%Z.

  Definition compute_balances_player (player2con: list (player_addr * wei))
                              (con2player: list Transfer)
                              (p: player_addr) : int256 :=
  Int256.repr (initial_balances p
               - transfer_player2con p player2con
               + transfer_con2player p con2player).

  Section mstep.
  (* These are the parameters that will stay constant for the lifetime a given block *)
  Context (coinbase: int256)
          (timestamp: int256)
          (number: int256)
          (blockhash: int256 -> int256)
          (prev_contract_state: state).

  Definition make_machine_env (addr: int256)
                              (callvalue: wei)
                              (player2con: list (player_addr * wei))
                              (con2player: list Transfer)
       : machine_env GetHighData
    := {| me_address := contract_address;
          me_origin := addr;
          me_caller := addr;
          me_callvalue := Int256.repr (callvalue);
          me_coinbase := coinbase;
          me_timestamp := timestamp;
          me_number := number;
          me_balance := compute_balances_player player2con con2player;
          me_blockhash := blockhash;
          me_transfer _ _ _ _ _ := False;
          me_callmethod _ _ _ _ _ _ _ _ _ _ := False;
          me_log _ _ d := d
       |}.

  Import MonadNotation.
  Require Import List.

  (* embed the change `cmd` has on contract_state into
    the entire world state. *)
  Import ListNotations.
  Definition lif {A:Type}
                 (addr : player_addr)
                 (callvalue : wei) 
                 (cmd : machine_env GetHighData -> osT global_abstract_data_type A)
                 (f: state -> state) (* allowing for effects on the state *)
    : osT state A :=
    st  <- get;;
      let incoming' := (addr, callvalue) :: (player_transfers st) in
      let s := st.(secrets_revealed) in
      let me := make_machine_env addr callvalue incoming' st.(contract_state).(_events) in
      match runStateT (cmd me) (contract_state st) with
      | None => mzero
      | Some (v, st') => put (f (Build_state st' st.(blocktimestamp) incoming' st.(contracts_published) s)) ;; ret v
      end.

  Definition lif_simpl {A:Type}
                 (addr : player_addr)
                 (callvalue : wei) 
                 (cmd : machine_env GetHighData -> osT global_abstract_data_type A)
                 (*f: state -> state*) (* allowing for effects on the state *)
    : osT state A :=
    st  <- get;;
      let incoming' := (addr, callvalue) :: (player_transfers st) in
      let s := st.(secrets_revealed) in
      let me := make_machine_env addr callvalue incoming' st.(contract_state).(_events) in
      match runStateT (cmd me) (contract_state st) with
      | None => mzero
      | Some (v, st') => put (Build_state st' st.(blocktimestamp) incoming' st.(contracts_published) s) ;; ret v
      end.


  Definition f_unlock (pt: player_addr) (s: int256) (st: state) : state :=
  let s' := (pt, s) :: (secrets_revealed st) in
  Build_state (contract_state st) (blocktimestamp st) (player_transfers st) (contracts_published st) s'.

  Definition f_id (st: state) : state :=
  st.

  Definition append_contract (pt c_pt: player_addr) (amt: int256) (st: state): state :=
  let new_contract := Build_contract pt c_pt amt in
  Build_state
  st.(contract_state) st.(blocktimestamp) st.(player_transfers)
  (new_contract :: st.(contracts_published)) st.(secrets_revealed).
  (* change to `state` should include new contract published, as well as 
     transfer amount made to the contract. *)
  Definition lifted_init := fun (pt: player_addr) (w: wei) (ld op c_pt: player_addr) (hl: hashvalue) (st dt: int256) 
  => lif pt w (SwapContract_initialize_opt ld op c_pt hl st dt) (* f_id *) (append_contract pt c_pt (Int256.repr w)).

  Definition lifted_init_id := fun (pt: player_addr) (w: wei) (ld op c_pt: player_addr) (hl: hashvalue) (st dt: int256) 
  => lif pt w (SwapContract_initialize_opt ld op c_pt hl st dt) f_id.

  Definition lifted_unlock := fun (pt: player_addr) (w: wei) (s: int256)
  => lif pt w (SwapContract_unlock_opt s) (f_unlock pt s).

  Definition lifted_claim := fun (pt: player_addr) (w: wei) 
  => lif pt w (SwapContract_claim_opt) f_id.

  Definition lifted_refund := fun (pt: player_addr) (w: wei)
  => lif pt w (SwapContract_refund_opt) f_id.

  (* change both player_transfers list and the contracts list *)
  Definition pub_contracts (pt: player_addr) (c_pt: list (player_addr * int256)) (callvalue: wei) 
  : osT state unit := 
  st <- get ;;
  let incoming' := (pt, callvalue) :: (player_transfers st) in
  let new_contracts := List.map (fun x => Build_contract pt (fst x) (snd x)) c_pt in
  let c' :=  new_contracts ++ (contracts_published st) in
  put (Build_state st.(contract_state) st.(blocktimestamp) incoming' c' st.(secrets_revealed)).


  (* State transition by applying the strategy *)
  Inductive mstep : state -> state -> Prop :=
  | mstep_call : forall p st retval st' (*rst*),
      (* assume that `rst = prev_contract_state` for our purposes for now. *)
      runStateT ((strategies p prev_contract_state state (* rst *) prev_contract_state
                    (fun w secret => lif p.(addr) w (SwapContract_unlock_opt secret) (f_unlock p.(addr) secret))
                    (fun w => lif p.(addr) w SwapContract_claim_opt f_id)
                    (fun w => lif p.(addr) w SwapContract_refund_opt f_id)
                    (fun w c_pt => pub_contracts p.(addr) c_pt w))
                         : osT state unit) st
      = Some (retval, st') ->
      mstep st st'.

  Inductive multi_mstep : state -> state -> Prop := 
  | multi_mstep_reflexive : forall (st : state), multi_mstep st st
  | multi_mstep_transitive : forall (st st' st'' : state), 
    mstep st st' -> multi_mstep st' st'' -> multi_mstep st st''.
  (* | multi_mstep_synchronicity : forall (stg : strategy) (s s' a a': state),
    multi_mstep s a /\ mstep a a' /\ multi_mstep a' s'. *)
  End mstep.

  Definition construct_incremented_state := 
  fun st => 
  (Build_state st.(contract_state) (Int256.add (blocktimestamp st) Int256.one) 
        (player_transfers st) (contracts_published st) (secrets_revealed st)).

  Inductive bstep : state -> state -> Prop :=
  | step_blocktimestamp : forall coinbase timstamp number blockhash p st st',
      construct_incremented_state st = st' -> 
      multi_mstep coinbase timstamp number blockhash p st st' -> 
      bstep st st'.

  Inductive multi_bstep : state -> state -> Prop := 
  | multi_bstep_reflexive : forall (st : state), multi_bstep st st
  | multi_bstep_transitive : forall (st st' st'' : state), 
    bstep st st' -> multi_bstep st' st'' -> multi_bstep st st''.

  Example prop1 : Prop := forall coinbase timstamp number blockhash p (st : state),
  mstep coinbase timstamp number blockhash p st st.

End step.

Section Transactions.
  Context (p: player)
          (st: state).

  Definition player_paid : Z :=
  (transfer_player2con p.(addr) (player_transfers st)).

  Definition player_gained : Z :=
  (transfer_con2player p.(addr) st.(contract_state).(_events)).


  Definition player_netgain : int256 :=
  Int256.repr (player_gained - player_paid).

  Definition player_bestpayoff : int256 :=
  let incoming := List.fold_left (fun x y => Int256.add x (snd y)) p.(neighbors_incoming) Int256.zero in
  let outgoing := List.fold_left (fun x y => Int256.add x (snd y)) p.(neighbors_outgoing) Int256.zero in
  (Int256.sub incoming outgoing).

End Transactions.

Definition is_nil {A:Type} (l: list A) : bool :=
  match l with
  | nil => true
  | _ => false
  end.
(* There are two phases to the transaction: 
  * phase1: populating the contracts.
    we are doing a case specific modeling here - not in the general sense.
  * phase2: populating the secrets. *)
Section properties.
  Context (st: state).
  Import ListNotations.

  (* validity of contracts? *)
  Fixpoint edge_contract_valid (pt c_pt: player_addr) (amt: int256) (c: list contract) : bool :=
  match c with
  | [] => false
  | {|party := pt'; counter_party := c_pt'; amount := amt' |} :: rest => 
    if andb (Int256.eq pt pt') (andb (Int256.eq c_pt c_pt') (Int256.eq amt' amt))
    then true else edge_contract_valid pt c_pt amt rest
  end.

  (* all the incoming contracts are placed and have the right amount *)
  Definition incoming_contracts_complete (p: player) : bool :=
  let neighs := p.(neighbors_incoming) in
  let c := st.(contracts_published) in
    (List.forallb (fun x => (edge_contract_valid (fst x) p.(addr) (snd x) c)) neighs).

  Definition outgoing_contracts_complete (p: player) : bool :=
  let neighs := p.(neighbors_outgoing) in
  let c := st.(contracts_published) in
    (List.forallb (fun x => (edge_contract_valid p.(addr) (fst x) (snd x) c)) neighs).

  (* contracts not yet published, p as the party *)
  Definition unpublished_contracts (p: player) : list (player_addr * int256) :=
  let neighs := p.(neighbors_outgoing) in
  let c := st.(contracts_published) in
  List.concat
   (List.map (fun x => if (edge_contract_valid p.(addr) (fst x) (snd x) c) then [] else [x]) neighs).

  (* lemma: outgoing_contracts_complete if and only if of unpublished contracts length 0 *)

  (* Note that this is not the same as pre-publication phase. *)
  Definition is_pre_initialization_phase :bool :=
  andb (is_nil st.(secrets_revealed)) 
        (andb (is_nil st.(player_transfers)) (is_nil st.(contracts_published))).

  Definition hashlock_unlocked : bool :=
  st.(contract_state).(_unlocked).

  Definition contract_ended : bool :=
  st.(contract_state).(_ended).

  Definition is_leader (p: player) : bool :=
  Int256.eq st.(contract_state).(_leader) p.(addr).


  (* secret learned from any one of its outgoing neighbors *)
  Definition has_learned_secret (p: player) : bool :=
  let neighs := p.(neighbors_outgoing) in
  let s := st.(secrets_revealed) in 
  List.existsb (fun x => List.existsb (fun k => Int256.eq (fst k) (fst x)) s) neighs.

  Definition get_secret (p: player) : option (player_addr * int256) :=
  let neighs := p.(neighbors_outgoing) in
  let s := st.(secrets_revealed) in
  List.find (fun x => List.existsb (fun k => Int256.eq (fst k) (fst x)) neighs) s.

End properties.

Definition mtt {A: Type} (m: option (unit * A)): osT A unit :=
  match m with
  | None => ret tt
  | Some (v, st) => put st
  end.

(* Differentiate between leader and follower
  Assumption needed: player's capital proof is >= sum of its outgoing contracts 
                     who can call these functions (unlock, claim, refund)
  FIXME: not yet included `refund`. It relies on time, tricky.... *)
Definition protocol_implementation (p: player): strategy :=
(fun (prev_block: state)
     (A: Type)
     (refined_prev_block: A)
     (unlock : wei -> int256 -> osT A unit)
     (claim : wei -> osT A unit)
     (refund : wei -> osT A unit) 
     (publish_contracts : wei -> list (player_addr * int256) -> osT A unit) =>
  (* pre-initialized -> no actions taken. *)
if is_pre_initialization_phase prev_block
then put refined_prev_block
else
(* leader *)
if is_leader prev_block p
then
  if negb (outgoing_contracts_complete prev_block p)
  then let unpublished_neighs := unpublished_contracts prev_block p in
       let z_neighs := List.map (fun x => (fst x, Int256.intval (snd x))) unpublished_neighs in
       let total_cost := List.fold_left (fun x z => (x + (snd z))%Z) z_neighs 0%Z in
       mtt (runStateT (publish_contracts total_cost unpublished_neighs) refined_prev_block)
  else
  if (incoming_contracts_complete prev_block p)
  then 
    if (is_nil (secrets_revealed prev_block))
    then mtt (runStateT (unlock 0%Z p.(secret)) refined_prev_block)
    else mtt (runStateT (claim 0%Z) refined_prev_block)
  else put refined_prev_block
(* follower *)
else 
  if andb (incoming_contracts_complete prev_block p)
        (negb (outgoing_contracts_complete prev_block p))
  then let unpublished_neighs := unpublished_contracts prev_block p in
     let z_neighs := List.map (fun x => (fst x, Int256.intval (snd x))) unpublished_neighs in
     let total_cost := List.fold_left (fun x z => (x + (snd z))%Z) z_neighs 0%Z in
     (* Important: need to assert that capital_proof >= sum of outgoing contracts *)
     mtt (runStateT (publish_contracts total_cost unpublished_neighs) refined_prev_block)
  else
  if (has_learned_secret prev_block p)
  then if hashlock_unlocked prev_block
     then (* claim *)
          mtt (runStateT (claim 0%Z) refined_prev_block)
     else match get_secret prev_block p with
          | None => put refined_prev_block (* should never happen *)
          | Some (_, v) => mtt (runStateT (unlock 0%Z v) refined_prev_block)
          end
  else
  put refined_prev_block
).
