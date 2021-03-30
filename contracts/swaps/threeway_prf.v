Require Import prf_setup.
Require Import List.
Import ListNotations.
Require Import lib.Monad.RunStateTInv.
Require Import BinPos.
Require Import DeepSpec.Runtime.
Require Import SymbolicKeccak.

Require Import lib.Monad.Monad.
Import MonadNotation.

  (* The following variables don't meaningfully affect the transaction. *)
  Context (initial_balances: player_addr -> Z)
          (contract_address_AB contract_address_BC contract_address_CA : int256)
          (coinbase_AB coinbase_BC coinbase_CA : int256)
          (number_AB number_BC number_CA : int256)
          (blockhash_AB blockhash_BC blockhash_CA : int256 -> int256).

  (************************************************************)
  (*       context setup: machine_env and initial state       *)
  (************************************************************)

  Context (A_to_B B_to_C C_to_A: int256).

  (* we will bind these two with desired properties and init_global_abstract_data *)
  Context (pre_init_state init_state : state).

  (* The following variables matter, and we set these to specific values
     so that the assertions in .ds will succeed.

     While we only care the general properties they satisfy rather than what
     exact values they take, it is not possible to just bound them by predicates, 
     since the actual methods in `LayerSWAPCONTRACT.v` only do computations over
     the actual values, and we don't want to modify these methods. *)

  Definition delta : int256 := Int256.repr 2.

  Definition init_timestamp_AB : int256 := Int256.repr 0.

  Definition init_timestamp_BC : int256 := Int256.repr 5.

  Definition init_timestamp_CA : int256 := Int256.repr 10.


  (* FIXME: generalize to just three distinct addresses *)
  Definition A_address : int256 := Int256.repr 0. (* A *)
  Definition B_address : int256 := Int256.repr 1. (* B *)
  Definition C_address : int256 := Int256.repr 2. (* C *)

  Definition A_secret := Int256.repr 10.
  Definition B_secret := Int256.repr 30.
  Definition C_secret := Int256.repr 60.

  Definition player_A : player :=
  Build_player A_address A_secret [(C_address, C_to_A)] [(B_address, A_to_B)].

  Definition player_B : player :=
  Build_player B_address B_secret [(A_address, A_to_B)] [(C_address, B_to_C)].

  Definition player_C : player :=
  Build_player C_address C_secret [(B_address, B_to_C)] [(A_address, C_to_A)].

  (* single hashlock in the 3-way swap case *)
  Definition hashlock : hashvalue := hashval_hash1 (hashval_int256 A_secret).

  (************************************************************)
  (*                definitions, lemmas, ltacs                *)
  (************************************************************)

  Definition filled_init_AB
  :=
  (lifted_init initial_balances contract_address_AB
   coinbase_AB init_timestamp_AB number_AB blockhash_AB
   A_address (Int256.intval A_to_B) (* callvalue *)).

  (* they are published, hence, use lifted_init_id *)
  Definition filled_init_BC
  :=
  (lifted_init_id initial_balances contract_address_BC
   coinbase_BC init_timestamp_BC number_BC blockhash_BC
   B_address (Int256.intval B_to_C) (* callvalue *)).

  Definition filled_init_CA
  :=
  (lifted_init_id initial_balances contract_address_CA
   coinbase_CA init_timestamp_CA number_CA blockhash_CA
   C_address (Int256.intval C_to_A) (* callvalue *)).


  Definition runstate_unlock
  con_addr cb ts num bh (p: player_addr)
  :=
  (lifted_unlock initial_balances con_addr cb ts num bh
    p 0%Z).

  Definition runstate_claim
  con_addr cb ts num bh (p: player_addr)
  :=
  (lifted_claim initial_balances con_addr cb ts num bh
    p 0%Z).

  (* all three players use this strategy *)
  Definition strategies (p: player) : strategy :=
  protocol_implementation p.

  Definition use_stg (p: player) (s: player -> strategy)
    (ss: strategy_tuple): Prop :=
  ss p = s p.

  Definition is_preinit_state (st : state): Prop :=
  (* states external to contract state conditions *)
  is_nil st.(secrets_revealed) = true
  /\ is_nil st.(player_transfers) = true
  /\ is_nil st.(contracts_published) = true.

  Axiom player_exclusivity :
  forall (p: player),
  p = player_A \/ p = player_B \/ p = player_C.

  Axiom player_congruence :
  forall (p: player),
  (p.(addr) = A_address
    -> p.(secret) = player_A.(secret)
       /\ p.(neighbors_incoming) = player_A.(neighbors_incoming)
       /\ p.(neighbors_outgoing) = player_A.(neighbors_outgoing)
       /\ p = player_A)
  /\
  (p.(addr) = B_address
    -> p.(secret) = player_B.(secret)
       /\ p.(neighbors_incoming) = player_B.(neighbors_incoming)
       /\ p.(neighbors_outgoing) = player_B.(neighbors_outgoing)
       /\ p = player_B)
  /\
  (p.(addr) = C_address
    -> p.(secret) = player_C.(secret)
       /\ p.(neighbors_incoming) = player_C.(neighbors_incoming)
       /\ p.(neighbors_outgoing) = player_C.(neighbors_outgoing)
       /\ p = player_C).


  Remark match_osT_bind {A S: Type} {e: option (A*S)} {d s: S} {v: A} {k: A -> S -> osT S A}:
  runStateT
    match e with
    | Some (a, st) => k a st
    | None => mzero
    end d = Some (v, s) ->
  exists a st, e = Some (a, st) /\ (k a st).(runStateT) d = Some (v, s).
  Proof.
  destruct e.
  - destruct p. intro.
    exists a, s0. split; auto.
  - simpl. intro. discriminate.
  Qed.

  (* helper functions that serve as `remember` or `destruct` *)
  Remark runstate_extract: forall A S (e: osT S A) s r,
    runStateT e s = r -> exists e', e = e' /\ runStateT e' s = r.
  Proof.
  intros. exists e. auto.
  Qed.

  Remark runstate_inj: forall A S (f: S -> option (A * S)) g,
    {| runStateT := f |} = {| runStateT := g |} -> f = g.
  Proof.
  intros. injection H. auto.
  Qed.

  Remark runstate_match_extract {A S S'}
  (e: option (A*S)) (s_1 s_2: osT S' A) (v: A) (st: S) v' (x: S') (prev_st: S')
  : runStateT
  match e with 
  | Some (v, st) => s_1
  | None => s_2
  end prev_st = Some (v', x)
  -> exists e', e = e' /\ 
  runStateT
  match e' with 
  | Some (v, st) => s_1
  | None => s_2
  end prev_st = Some (v', x).
  Proof.
  intros. exists e. auto.
  Qed.

  Definition bounded_int256 (v: int256) : Prop :=
    Z.le 0%Z (Int256.intval v) /\ Z.le (Int256.intval v) Int256.max_unsigned.

  Remark intval_to_repr :
    forall v, bounded_int256 v ->
    exists d, Int256.intval v = d /\ Int256.repr d = v.
  Proof.
  intros.
  exists (Int256.intval v). split; try reflexivity.
  unfold bounded_int256 in H.
  remember (Int256.intval v) as z.
  apply Int256.unsigned_repr in H.
  rewrite Heqz. apply Int256.repr_unsigned.
  Qed.

  Remark preinit_equiv : forall st, 
  is_pre_initialization_phase st = true
  <-> is_preinit_state st.
  Proof.
  intros. unfold is_pre_initialization_phase.
  unfold is_preinit_state. split.
  - intro. apply andb_prop in H. destruct H. 
    apply andb_prop in H0. destruct H0.
    repeat split; assumption.
  - intro. destruct H. destruct H0.
    rewrite H. rewrite H0. rewrite H1. auto.
  Qed.

  Remark eq_eq : forall (x y :int256), 
  Int256.eq x y = true -> x = y.
  Proof.
  intros.
  assert (A := Int256.eq_spec x y).
  rewrite H in A. assumption.
  Qed.

  Remark nil_nil : forall {A} (x: list A),
  is_nil x = true <-> x = [].
  Proof.
  intros. split.
  - unfold is_nil. destruct x; auto.
    intro. discriminate.
  - intro. unfold is_nil. rewrite H. auto.
  Qed.

  Ltac match_runStateT :=
  match goal with
    [H: @runStateT ?st ?m ?a match @runStateT ?s ?m ?a ?x ?y 
        with | Some _ => _ | None => _ end _ = _ |- _]
    => remember (@runStateT s m a x y) as t
  end.

  Ltac if_runStateT :=
  match goal with
    [H: @runStateT ?s ?m ?a (if ?e then _ else _) _ = _ |- _]
    => destruct e eqn:?
  end.

  Ltac split_conj :=
  repeat match goal with
    [H : ?a /\ ?b |- _] => destruct H as [? ?]
  end.

  Ltac forward_replace :=
  repeat match goal with
    | [H1: ?a = ?b, H2 : ?b = ?c |- _] => rewrite H2 in H1; clear H2
    | [H1: ?a = _ :: ?b, H2 : ?b = ?c |- _] => rewrite H2 in H1; clear H2
  end.

  Ltac unfold_updates H :=
  unfold update__assetAmount in H; simpl in H;
  unfold update__leader in H; simpl in H;
  unfold update__hashlock in H; simpl in H;
  unfold update__timelock in H; simpl in H;
  unfold update__counterparty in H; simpl in H;
  unfold update__otherparty in H; simpl in H;
  unfold update__party in H; simpl in H;
  unfold update__start in H; simpl in H;
  unfold update__delta in H; simpl in H;
  unfold update__ended in H; simpl in H;
  unfold update__events in H; simpl in H.

  Ltac unfold_updates_goal :=
  unfold update__assetAmount; simpl;
  unfold update__leader; simpl;
  unfold update__hashlock; simpl;
  unfold update__timelock; simpl;
  unfold update__counterparty; simpl;
  unfold update__otherparty; simpl;
  unfold update__party; simpl;
  unfold update__start; simpl;
  unfold update__delta; simpl;
  unfold update__ended; simpl;
  unfold update__events; simpl.


  (************************************************************)
  (*        Properties of methods acting on the state         *)
  (************************************************************)

  (* append doesn't change contract state *)
  Lemma append_contract_invariant :
  forall pt c_pt amt prev_st st,
  append_contract pt c_pt amt prev_st = st
  -> (contract_state prev_st) = (contract_state st).
  Proof.
  intros. unfold contract_state.
  unfold append_contract in H. subst. 
  reflexivity.
  Qed.

  (* lif + append doesn't change contract state *)
  Lemma lif_lifsimpl_invariant :
  forall A bal con_addr cbase ts num bh a_addr amt cmd addr_1 addr_2 amt'
      prev_state (v1: A) st1,
  runStateT (lif bal con_addr cbase ts num bh a_addr amt
        cmd
       (append_contract addr_1 addr_2 amt')) prev_state = 
  Some (v1, st1)
  -> exists (v2:A) st2, runStateT (lif_simpl bal con_addr cbase ts num bh a_addr amt
        cmd) prev_state = 
  Some (v2, st2) /\ (contract_state st1) = (contract_state st2).
  Proof.
  intros. simpl in *.
  destruct (runStateT
          (cmd
             (make_machine_env bal con_addr cbase ts num bh
                a_addr amt
                ((a_addr, amt) :: player_transfers prev_state)
                (_events (contract_state prev_state))))
          (contract_state prev_state)) eqn:?; try congruence.
  - destruct p.
    exists a.
    exists {| contract_state := g;
              blocktimestamp := blocktimestamp
                                   prev_state;
              player_transfers := (a_addr, amt) :: player_transfers
                                   prev_state;
              contracts_published := contracts_published
                                   prev_state;
              secrets_revealed := secrets_revealed
                                   prev_state |}.
    split; try reflexivity.
    simpl in H. injection H. intros. subst. reflexivity.
  - simpl in H. discriminate.
  Qed.


  Definition start_AB : int256 := Int256.repr 3. (* ts: 0 *)
  Definition start_BC : int256 := Int256.repr 8. (* ts: 5 *)
  Definition start_CA : int256 := Int256.repr 13. (* ts: 10 *)


  Definition init_state_AB (o: state) : Prop :=
  o.(contracts_published) = 
    [(Build_contract A_address B_address A_to_B)]
  /\ o.(player_transfers) = [(A_address, (Int256.intval A_to_B))]
  /\ o.(secrets_revealed) = nil.

  Definition init_state_BC (o: state) : Prop :=
  o.(contracts_published) = nil
  /\ o.(player_transfers) = nil
  /\ o.(secrets_revealed) = nil.

  Definition init_state_CA (o: state) : Prop :=
  o.(contracts_published) = nil
  /\ o.(player_transfers) = nil
  /\ o.(secrets_revealed) = nil.

  Definition init_constate_AB (st: global_abstract_data_type) : Prop :=
  st.(_assetAmount) = A_to_B
    /\ st.(_leader) = A_address
    /\ st.(_hashlock) = hashlock
    /\ st.(_party) = A_address
    /\ st.(_counterparty) = B_address
    /\ st.(_otherparty) = C_address
    /\ st.(_start) = start_AB
    /\ st.(_delta) = delta
    /\ st.(_unlocked) = false
    /\ st.(_ended) = false
    /\ st.(_events) = nil
    /\ st.(_timelock) = (Int256.mul (Int256.repr 6) delta).

  Definition init_constate_BC (st: global_abstract_data_type) : Prop :=
  st.(_assetAmount) = B_to_C
    /\ st.(_leader) = A_address
    /\ st.(_hashlock) = hashlock
    /\ st.(_party) = B_address
    /\ st.(_counterparty) = C_address
    /\ st.(_otherparty) = A_address
    /\ st.(_start) = start_BC
    /\ st.(_delta) = delta
    /\ st.(_unlocked) = false
    /\ st.(_ended) = false
    /\ st.(_events) = nil
    /\ st.(_timelock) = (Int256.mul (Int256.repr 5) delta).

  Definition init_constate_CA (st: global_abstract_data_type) : Prop :=
  st.(_assetAmount) = C_to_A
    /\ st.(_leader) = A_address
    /\ st.(_hashlock) = hashlock
    /\ st.(_party) = C_address
    /\ st.(_counterparty) = A_address
    /\ st.(_otherparty) = B_address
    /\ st.(_start) = start_CA
    /\ st.(_delta) = delta
    /\ st.(_unlocked) = false
    /\ st.(_ended) = false
    /\ st.(_events) = nil
    /\ st.(_timelock) = (Int256.mul (Int256.repr 4) delta).

  Lemma initialize_properties :
  forall prev_state b new_state,
  bounded_int256 A_to_B
  -> init_global_abstract_data = prev_state.(contract_state)
  -> runStateT
      (SwapContract_initialize_opt A_address C_address B_address
             hashlock start_AB delta
      (make_machine_env initial_balances contract_address_AB coinbase_AB
      init_timestamp_AB number_AB blockhash_AB (* caller address, which is party *)
      A_address (Int256.intval A_to_B)
      ((A_address, (Int256.intval A_to_B))::(player_transfers prev_state))
      (_events (contract_state prev_state))))
      (contract_state prev_state) = Some (b, new_state)
  -> init_constate_AB new_state.
  Proof.
  intros.
  Transparent SwapContract_initialize_opt.
  remember (Int256.intval A_to_B) as z.
  assert (A_to_B = Int256.repr z).
  { rewrite Heqz. unfold bounded_int256 in H.
    symmetry. apply Int256.repr_unsigned. }
  rewrite H2 in *.
  unfold SwapContract_initialize_opt in H1.
  rewrite <- H0 in H1.
  inv_runStateT.
  repeat split; subst; try auto.
  Qed.

Section outcome_proof.
  (************************************************************)
  (*      Init(0th) step: Initialize AB, BC, CA               *)
  (* prove on *AB*: contract state and external state correct *)
  (* prove for all: all contract state correctly initialized  *)
  (************************************************************)

  (* Important existence lemma.
     runStateT _ _ = Some means that the method call didn't fail. *)

  Lemma init_constate_exists_AB :
  forall amt (* A_to_B *) (prev_state: state),
  bounded_int256 amt
  -> init_global_abstract_data = prev_state.(contract_state)
  -> exists b (new_state : global_abstract_data_type), 
      runStateT
      (SwapContract_initialize_opt
             A_address (* leader *) C_address (* otherparty *) B_address (* counterparty *)
             hashlock start_AB delta
      (make_machine_env initial_balances contract_address_AB coinbase_AB
      init_timestamp_AB number_AB blockhash_AB
      A_address (* party *) (Int256.intval amt)
      ((A_address, (Int256.intval amt))::(player_transfers prev_state))
      (_events (contract_state prev_state))))
      (contract_state prev_state) = Some (b, new_state).
  Proof.
  intros. exists tt.
  remember (Int256.mul (Int256.repr 6) delta) as timelock.
  exists {| _assetAmount := amt;
            _leader := A_address;
            _hashlock := hashlock;
            _party := A_address;
            _counterparty := B_address;
            _otherparty := C_address;
            _start := start_AB;
            _delta := delta;
            _timelock := timelock;
            _unlocked := false;
            _ended := false;
            _events := nil; |}.
  rewrite <- H0. 
  Transparent SwapContract_initialize_opt.
  unfold SwapContract_initialize_opt.
  simpl.
    Transparent _assetAmount update__assetAmount _leader update__leader _hashlock update__hashlock _timelock update__timelock _party update__party _counterparty update__counterparty _otherparty update__otherparty _start update__start _delta update__delta _unlocked update__unlocked _ended update__ended _events update__events.

    unfold_updates_goal.
    replace ({|
       Int256.intval := 6;
       Int256.intrange := Int256.Z_mod_modulus_range' 6 |}) with (Int256.repr 6)
       by reflexivity.
    replace (Int256.repr (Int256.intval amt)) with amt by (symmetry;  apply Int256.repr_unsigned).
    subst timelock.
    auto.
  Qed.

  (* contract correctly initialized *)
  Lemma init_constate_correct_AB :
  bounded_int256 A_to_B
  -> is_true (is_pre_initialization_phase pre_init_state)
  -> init_global_abstract_data = pre_init_state.(contract_state)
  -> runStateT (filled_init_AB A_address C_address B_address hashlock start_AB delta)
               pre_init_state = Some (tt, init_state)
  -> init_constate_AB (contract_state init_state).
  Proof.
  intros.
  unfold filled_init_AB in H2. unfold lifted_init in H2.
  unfold init_constate_AB.
  apply lif_lifsimpl_invariant in H2.
  destruct H2. destruct H2. destruct H2.
  rewrite H3.
  unfold lif_simpl in H2.
  Opaque SwapContract_initialize_opt.
  simpl in H2.
  match_runStateT.
  destruct t eqn:?.
  - (* assertion succeeds, stuff happens *)
    destruct p.
    rewrite <- H1 in *.
    Transparent SwapContract_initialize_opt.
    symmetry in Heqt.
    unfold SwapContract_initialize_opt in Heqt.
    (* inv_runStateT. *)  (* This part didn't work -- probably it matches the wrong hyptohesis. *)
    inv_runStateT1 Heqt.
    subst.
    unfold_updates H2.
    inversion H2. simpl.
    repeat split; try auto.
    apply Int256.repr_unsigned.
  - (* assertion fails, nothing happens *)
    simpl in H2. discriminate.
  Qed.

  (* external state - "observations" - correctly initialized *)
  Lemma init_state_correct_AB : 
  bounded_int256 A_to_B
  -> is_preinit_state pre_init_state
  -> init_global_abstract_data = pre_init_state.(contract_state)
  -> runStateT (filled_init_AB A_address C_address B_address hashlock start_AB delta)
               pre_init_state = Some (tt, init_state)
  -> init_state_AB init_state.
  Proof.
  intros.
  unfold init_state_AB.
  unfold filled_init_AB in H2.
  unfold lifted_init in H2.
  unfold append_contract in H2.
  unfold lif in H2.
  simpl in H2.
  unfold is_preinit_state in H0.
  split_conj.
  inversion H2. simpl.
  apply nil_nil in H4; rewrite H4 in *.
  apply nil_nil in H0; rewrite H0 in *.
  apply nil_nil in H3; rewrite H3 in *.
  repeat split; try assumption; try reflexivity.
  replace (Int256.repr (Int256.intval A_to_B)) with (A_to_B).
  reflexivity.
  symmetry; apply Int256.repr_unsigned.
  Qed.


  Lemma init_constate_exists_BC :
  forall amt (* B_to_C *) (prev_state: state),
  bounded_int256 amt
  -> init_global_abstract_data = prev_state.(contract_state)
  -> exists b (new_state : global_abstract_data_type), 
      runStateT
      (SwapContract_initialize_opt
             A_address (* leader *) A_address (* otherparty *) C_address (* counterparty *)
             hashlock start_BC delta
      (make_machine_env initial_balances contract_address_BC coinbase_BC
      init_timestamp_BC number_BC blockhash_BC (* caller address, which is party *)
      B_address (Int256.intval amt)
      ((B_address, (Int256.intval amt))::(player_transfers prev_state))
      (_events (contract_state prev_state))))
      (contract_state prev_state) = Some (b, new_state).
  Proof.
  intros. exists tt.
  remember (Int256.mul (Int256.repr 5) delta) as timelock.
  exists {| _assetAmount := amt;
            _leader := A_address;
            _hashlock := hashlock;
            _party := B_address;
            _counterparty := C_address;
            _otherparty := A_address;
            _start := start_BC;
            _delta := delta;
            _timelock := timelock;
            _unlocked := false;
            _ended := false;
            _events := nil; |}.
  rewrite <- H0.
  Transparent SwapContract_initialize_opt.
  unfold SwapContract_initialize_opt.
  Transparent _assetAmount update__assetAmount _leader update__leader _hashlock update__hashlock _timelock update__timelock _party update__party _counterparty update__counterparty _otherparty update__otherparty _start update__start _delta update__delta _unlocked update__unlocked _ended update__ended _events update__events.
  unfold_updates_goal.
  replace ({|
       Int256.intval := 5;
       Int256.intrange := Int256.Z_mod_modulus_range' 5 |}) with (Int256.repr 5)
       by reflexivity.
  replace (Int256.repr (Int256.intval amt)) with amt
    by (symmetry; apply Int256.repr_unsigned).
  subst timelock.
  reflexivity.
  Qed.

  Lemma init_constate_correct_BC :
  bounded_int256 B_to_C
  -> is_true (is_pre_initialization_phase pre_init_state)
  -> init_global_abstract_data = pre_init_state.(contract_state)
  -> runStateT (filled_init_BC A_address A_address C_address hashlock start_BC delta)
               pre_init_state = Some (tt, init_state)
  -> init_constate_BC (contract_state init_state).
  Proof.
  intros.
  unfold init_constate_BC.
  unfold filled_init_BC in H2;
  unfold lifted_init_id in H2.
  unfold f_id in H2. simpl in H2.
  inversion H2. simpl.
  repeat split; try auto.
  apply Int256.repr_unsigned.
  unfold_updates H4.
  rewrite <- H1. auto.
  Qed.

  Lemma init_constate_exists_CA :
  forall amt (* C_to_A *) (prev_state: state),
  bounded_int256 amt
  -> init_global_abstract_data = prev_state.(contract_state)
  -> exists b (new_state : global_abstract_data_type), 
      runStateT
      (SwapContract_initialize_opt
             A_address (* leader *) B_address (* otherparty *) A_address (* counterparty *)
             hashlock start_CA delta
      (make_machine_env initial_balances contract_address_CA coinbase_CA
      init_timestamp_CA number_CA blockhash_CA (* caller address, which is party *)
      C_address (Int256.intval amt)
      ((C_address, (Int256.intval amt))::(player_transfers prev_state))
      (_events (contract_state prev_state))))
      (contract_state prev_state) = Some (b, new_state).
  Proof.
  intros. exists tt.
  remember (Int256.mul (Int256.repr 4) delta) as timelock.
  exists {| _assetAmount := amt;
            _leader := A_address;
            _hashlock := hashlock;
            _party := C_address;
            _counterparty := A_address;
            _otherparty := B_address;
            _start := start_CA;
            _delta := delta;
            _timelock := timelock;
            _unlocked := false;
            _ended := false;
            _events := nil; |}.
  rewrite <- H0.
  Transparent SwapContract_initialize_opt.
  unfold SwapContract_initialize_opt.
  Transparent _assetAmount update__assetAmount _leader update__leader _hashlock update__hashlock _timelock update__timelock _party update__party _counterparty update__counterparty _otherparty update__otherparty _start update__start _delta update__delta _unlocked update__unlocked _ended update__ended _events update__events.
  unfold_updates_goal.
  replace ({|
       Int256.intval := 4;
       Int256.intrange := Int256.Z_mod_modulus_range' 4 |}) with (Int256.repr 4).
    replace (Int256.repr (Int256.intval amt)) with amt.
  subst timelock.
  reflexivity.
  symmetry. apply Int256.repr_unsigned.
  unfold Int256.repr. reflexivity.
  Qed.


  Lemma init_constate_correct_CA :
  bounded_int256 C_to_A
  -> is_true (is_pre_initialization_phase pre_init_state)
  -> init_global_abstract_data = pre_init_state.(contract_state)
  -> runStateT (filled_init_CA A_address B_address A_address hashlock start_CA delta)
               pre_init_state = Some (tt, init_state)
  -> init_constate_CA (contract_state init_state).
  Proof.
  intros.
  unfold init_constate_CA.
  unfold filled_init_CA in H2; unfold lifted_init_id in H2.
  unfold f_id in H2. simpl in H2.
  inversion H2. simpl.
  repeat split; try auto.
  apply Int256.repr_unsigned.
  unfold_updates H4.
  rewrite <- H1. auto.
  Qed.


  (************************************************************)
  (*                First step: AB live                       *)
  (*        prove on contract *AB*: B publishes BC            *)
  (************************************************************)

  (* the external fields of state is what all three contracts would agree on,
     since they correspond to observations and event log. *)

  (* State changes after first step: 
   * contract state of AB doesn't change, contract BC gets published, 
   * so that now both contracts are live.
   *)
  Definition mstep1_state (o i: state) : Prop :=
  o.(contracts_published) =
    (Build_contract B_address C_address B_to_C) :: (contracts_published i)
  /\ o.(player_transfers) = (B_address, Int256.intval B_to_C) :: (player_transfers i)
  /\ o.(secrets_revealed) = (secrets_revealed i).

  Definition mstep1_constate (c_st init_st: global_abstract_data_type) : Prop :=
  c_st.(_assetAmount) = init_st.(_assetAmount)
  /\ c_st.(_leader) = init_st.(_leader)
  /\ c_st.(_hashlock) = init_st.(_hashlock)
  /\ c_st.(_timelock) = init_st.(_timelock)
  /\ c_st.(_party) = init_st.(_party)
  /\ c_st.(_counterparty) = init_st.(_counterparty)
  /\ c_st.(_otherparty) = init_st.(_otherparty)
  /\ c_st.(_start) = init_st.(_start)
  /\ c_st.(_delta) = init_st.(_delta)
  /\ c_st.(_unlocked) = init_st.(_unlocked)
  /\ c_st.(_ended) = init_st.(_ended)
  /\ c_st.(_events) = init_st.(_events).


  (* after init_state, next step is publish contract B -> C *)
  Lemma mstep1_correct_AB (o: state) : 
  bounded_int256 A_to_B
  -> is_preinit_state pre_init_state
  -> init_global_abstract_data = pre_init_state.(contract_state)
  -> runStateT (filled_init_AB A_address C_address B_address hashlock start_AB delta) (* determines the machine env. *)
               pre_init_state = Some (tt, init_state)
  (* after init_state *)
  -> mstep strategies initial_balances contract_address_AB
           coinbase_AB init_timestamp_AB number_AB blockhash_AB
     (* prev contract state *)
     init_state init_state o
  -> o = init_state
     \/
     ((mstep1_state o init_state)
     /\ (mstep1_constate o.(contract_state) init_state.(contract_state))).
  Proof.
  intros. simpl in *.
  pose (Hi_state := init_state_correct_AB H H0 H1 H2);
    clearbody Hi_state.
  destruct Hi_state as [H4 [H5 H6]].
  assert (is_true (is_pre_initialization_phase pre_init_state)).
  { apply preinit_equiv. assumption. }
  pose (Hi_constate := init_constate_correct_AB H H7 H1 H2);
    clearbody Hi_constate.
  unfold init_constate_AB in Hi_constate.
  split_conj.
  inversion H3.
  unfold mstep1_state.
  unfold mstep1_constate.
  unfold strategies in H20.
  unfold protocol_implementation in H20.
  destruct (is_pre_initialization_phase init_state) eqn:?.
  - (* preinit phase - contradiction *)
    apply preinit_equiv in Heqb. unfold is_preinit_state in Heqb.
    split_conj.  rewrite H4 in *. inversion H25.
  - (* not preinit phase *)
    destruct (is_leader init_state p) eqn:?.
    + (* leader *)
      unfold is_leader in Heqb0.
      apply eq_eq in Heqb0. rewrite H9 in Heqb0. 
      destruct (negb (outgoing_contracts_complete init_state p)) eqn:?.
      * (* outgoing not complete: contradiction *)
        unfold outgoing_contracts_complete in Heqb1.
        unfold edge_contract_valid in Heqb1.
        assert (neighbors_outgoing p = neighbors_outgoing player_A).
        { apply player_congruence. auto. }
        rewrite H4 in Heqb1. rewrite H23 in Heqb1.
        simpl in Heqb1. rewrite <- Heqb0 in Heqb1.
        replace (Int256.eq A_to_B A_to_B) with true in Heqb1.
        replace (Int256.eq A_address A_address) with true in Heqb1.
        inversion Heqb1.
        symmetry; apply Int256.eq_true.
        symmetry; apply Int256.eq_true.
      * (* outgoing complete *)
        destruct (incoming_contracts_complete init_state p) eqn:?.
        -- (* incoming complete: contradiction *)
          unfold incoming_contracts_complete in Heqb2.
          unfold edge_contract_valid in Heqb2.
          assert (neighbors_incoming p = neighbors_incoming player_A).
          { apply player_congruence. auto. }
          rewrite H4 in Heqb2. rewrite H23 in Heqb2.
          simpl in Heqb2. inversion Heqb2.
        -- (* incoming incomplete : NO ACTION *)
          left. simpl in H20. inversion H20. auto.
    + (* follower *)
      pose (He := player_exclusivity p); clearbody He.
      inversion He. rewrite H23 in Heqb0.
      unfold is_leader in Heqb0. simpl in Heqb0.
      replace (_leader (contract_state init_state)) with A_address in Heqb0.
      inversion Heqb0. clear He.
      destruct H23.
      * (* player B *)
        rewrite H23 in *. subst.
        destruct (incoming_contracts_complete init_state player_B &&
          negb (outgoing_contracts_complete init_state player_B)) eqn:?.
        -- (* can publish: ACTION *)
          right.
          repeat split;
          unfold unpublished_contracts in H20;
          rewrite H4 in *; simpl in H20; inversion H20; try reflexivity.
          (* that gets rid of all but contracts_published case *)
          rewrite H4. reflexivity.
        -- (* can't publish: contradiction *)
          clear H20.
          unfold outgoing_contracts_complete in Heqb1.
          unfold incoming_contracts_complete in Heqb1.
          rewrite H4 in *. simpl in Heqb1.
          replace (Int256.eq A_to_B A_to_B) with true in Heqb1.
          inversion Heqb1.
          symmetry. apply Int256.eq_true.
      * (* player C *)
        rewrite H23 in *. subst.
        destruct (incoming_contracts_complete init_state player_C &&
          negb (outgoing_contracts_complete init_state player_C)) eqn:?.
        -- (* can publish: contradiction *)
          clear H20.
          unfold incoming_contracts_complete in Heqb1.
          simpl in Heqb1.
          rewrite H4 in Heqb1.
          unfold edge_contract_valid in Heqb1.
          inversion Heqb1.
        -- (* can't publish: NO ACTION *)
          left.
          replace (has_learned_secret init_state player_C) with false in H20.
          simpl in H20. inversion H20. 
          auto.
          unfold has_learned_secret.
          rewrite H6.
          reflexivity.
  Qed.

  (************************************************************)
  (*               Second step: AB, BC live                   *)
  (*      prove on contract *AB*: C publishes contract CA     *)
  (*     (prove on contract *BC*: C publishes contract CA)    *)
  (************************************************************)

  (* same for AB and BC -> new contract and transfer by C *)
  Definition mstep2_state (o i: state) : Prop :=
  o.(contracts_published) =
    (Build_contract C_address A_address C_to_A) :: (contracts_published i)
  /\ o.(player_transfers) = (C_address, Int256.intval C_to_A) :: (player_transfers i)
  /\ o.(secrets_revealed) = (secrets_revealed i).

  (* same for AB and BC -> no change in contract state. *)
  Definition mstep2_constate (next_st st: global_abstract_data_type) : Prop :=
  next_st.(_assetAmount) = st.(_assetAmount)
  /\ next_st.(_leader) = st.(_leader)
  /\ next_st.(_hashlock) = st.(_hashlock)
  /\ next_st.(_timelock) = st.(_timelock)
  /\ next_st.(_party) = st.(_party)
  /\ next_st.(_counterparty) = st.(_counterparty)
  /\ next_st.(_otherparty) = st.(_otherparty)
  /\ next_st.(_start) = st.(_start)
  /\ next_st.(_delta) = st.(_delta)
  /\ next_st.(_unlocked) = st.(_unlocked)
  /\ next_st.(_ended) = st.(_ended)
  /\ next_st.(_events) = st.(_events).


  Definition mstep1_since_init_AB (o i: state) :Prop :=
  (* external state changed since init_state *)
  (contracts_published o) =
      [(Build_contract B_address C_address B_to_C)]
      ++ (contracts_published i)
  /\
  (player_transfers o) = 
      [(B_address, Int256.intval B_to_C)]
      ++ (player_transfers i)
  (* /\ (blocktimestamp o) = (blocktimestamp i) *)
  /\ (secrets_revealed o) = (secrets_revealed i)
  (* contract state is not changed since init_state *)
  /\ (contract_state o) = (contract_state i).

  Definition mstep1_since_init_BC (o i: state) :Prop :=
  (* external state changed since init_state *)
  (contracts_published o) =
      [(Build_contract B_address C_address B_to_C);
       (Build_contract A_address B_address A_to_B)]
      ++ (contracts_published i)
  /\
  (player_transfers o) = 
      [(B_address, Int256.intval B_to_C);
       (A_address, Int256.intval A_to_B)]
      ++ (player_transfers i)
  /\ (secrets_revealed o) = (secrets_revealed i)
  (* contract state is not changed since init_state *)
  /\ (contract_state o) = (contract_state i).


  Lemma mstep2_correct_AB (st_1 st_2: state) : 
  bounded_int256 A_to_B
  -> is_preinit_state pre_init_state
  -> init_global_abstract_data = pre_init_state.(contract_state)
  -> runStateT (filled_init_AB A_address C_address B_address hashlock start_AB delta) (* determines the me. *)
               pre_init_state = Some (tt, init_state)
  -> mstep1_since_init_AB st_1 init_state
  -> mstep strategies initial_balances contract_address_AB
           coinbase_AB init_timestamp_AB number_AB blockhash_AB
     st_1 st_1 st_2
  -> st_2 = st_1
     \/
     ((mstep2_state st_2 st_1)
     /\ (mstep2_constate st_2.(contract_state) st_1.(contract_state))).
  Proof.
  intros.
  unfold mstep1_since_init_AB in H3.
  assert (is_true (is_pre_initialization_phase pre_init_state)).
  { apply preinit_equiv. assumption. }
  unfold is_preinit_state in H0.
  split_conj.
  apply nil_nil in H0;
  apply nil_nil in H9;
  apply nil_nil in H10.
  pose (Hi_constate := init_constate_correct_AB H H5 H1 H2);
    clearbody Hi_constate.
  unfold init_constate_AB in Hi_constate.
  unfold mstep2_state; unfold mstep2_constate.
  inversion H2.
  unfold append_contract in H12;
    simpl in H12.
  split_conj. forward_replace.
  unfold_updates H13. (* now, we've brought init_state to the env *)
  inversion H4. (* main proof starts here *)
  unfold strategies in *.
  unfold protocol_implementation in *.
  destruct (is_pre_initialization_phase st_1) eqn:?.
  subst st st'.
  - (* preinit phase: contradiction *)
    apply preinit_equiv in Heqb.
    unfold is_preinit_state in Heqb.
    split_conj.
    rewrite H3 in H27. unfold is_nil in H27.
    discriminate.
  - (* not preinit phase *)
    destruct (is_leader st_1 p) eqn:?.
    + (* leader *)
      unfold is_leader in Heqb0.
      apply eq_eq in Heqb0.
      rewrite H8 in Heqb0; rewrite <- H12 in Heqb0;
        simpl in Heqb0.
      assert (p = player_A).
      { apply player_congruence. auto. }
      subst p.
      destruct (negb (outgoing_contracts_complete st_1 player_A)) eqn:?.
      * (* outgoing not complete: contradiction *)
        unfold outgoing_contracts_complete in Heqb1.
        unfold edge_contract_valid in Heqb1.
        rewrite H3 in Heqb1; rewrite <- H12 in Heqb1;
          simpl in Heqb1.
        replace (Int256.repr (Int256.intval A_to_B)) with A_to_B in Heqb1.
        replace (Int256.eq A_to_B A_to_B) with true in Heqb1.
        inversion Heqb1.
        symmetry; apply Int256.eq_true.
        symmetry; apply Int256.repr_unsigned.
      * (* outgoing complete *)
        destruct (incoming_contracts_complete st_1 player_A) eqn:?.
        -- (* incoming complete: contradiction *)
          unfold incoming_contracts_complete in Heqb2.
          unfold edge_contract_valid in Heqb2.
          rewrite H3 in Heqb2; rewrite <- H12 in Heqb2;
            rewrite H10 in Heqb2;
            simpl in Heqb2.
         inversion Heqb2.
        -- (* incoming incomplete : NO ACTION *)
          left.
          simpl in H24. inversion H24. auto.
    + (* follower *)
      pose (He := player_exclusivity p); clearbody He.
      inversion He.
      unfold is_leader in Heqb0. simpl in Heqb0.
      rewrite H8 in Heqb0; rewrite  H13 in Heqb0.
      subst p. simpl in Heqb0.
      inversion Heqb0. clear He.
      destruct H27.
      * (* player B *)
        destruct (incoming_contracts_complete st_1 p &&
         negb (outgoing_contracts_complete st_1 p)) eqn:?.
        -- (* can publish: contradiction *)
          subst p.
          unfold incoming_contracts_complete in Heqb1.
          unfold outgoing_contracts_complete in Heqb1.
          unfold edge_contract_valid in Heqb1.
          rewrite H3 in Heqb1; rewrite <- H12 in Heqb1;
            rewrite H10 in Heqb1;
            simpl in Heqb1.
          replace (Int256.repr (Int256.intval A_to_B)) with A_to_B in Heqb1.
          replace (Int256.eq A_to_B A_to_B) with true in Heqb1.
          replace (Int256.eq B_to_C B_to_C) with true in Heqb1.
          inversion Heqb1.
          symmetry; apply Int256.eq_true.
          symmetry; apply Int256.eq_true.
          symmetry; apply Int256.repr_unsigned.
        -- (* can't publish: NO ACTION *)
          left.
          subst p.
          replace (has_learned_secret st_1 player_B) with false in H24.
          simpl in H24. inversion H24. auto.
          unfold has_learned_secret.
          rewrite H7; rewrite <- H12; rewrite H0;
            reflexivity.
      * (* player C *)
        destruct (incoming_contracts_complete st_1 p &&
         negb (outgoing_contracts_complete st_1 p)) eqn:?.
        -- (* can publish: ACTION *)
          right.
          subst.
          simpl in H24.
          inversion H24. simpl.
          (* gets rid of all but the meaningful publish contract case *)
          repeat split; try reflexivity.
          unfold unpublished_contracts.
          rewrite H3; rewrite <- H12; rewrite H10;
            reflexivity.
          unfold unpublished_contracts.
          rewrite H3; rewrite <- H12; rewrite H10;
            reflexivity.
        -- (* can't publish: contradiction *)
          subst p.
          unfold outgoing_contracts_complete in Heqb1.
          unfold incoming_contracts_complete in Heqb1.
          rewrite H3 in Heqb1; rewrite <- H12 in Heqb1;
            rewrite H10 in Heqb1;
            simpl in Heqb1.
          replace (Int256.eq B_to_C B_to_C) with true in Heqb1.
          inversion Heqb1.
          symmetry; apply Int256.eq_true.
  Qed.

  Lemma mstep2_correct_BC (st_1 st_2: state) : 
  bounded_int256 B_to_C
  -> is_preinit_state pre_init_state
  -> init_global_abstract_data = pre_init_state.(contract_state)
  -> runStateT (filled_init_BC A_address A_address C_address hashlock start_BC delta) (* determines the me. *)
               pre_init_state = Some (tt, init_state)
  -> mstep1_since_init_BC st_1 init_state
  -> mstep strategies initial_balances contract_address_BC
           coinbase_BC init_timestamp_BC number_BC blockhash_BC
     st_1 st_1 st_2
  -> st_2 = st_1
     \/
     ((mstep2_state st_2 st_1)
     /\ (mstep2_constate st_2.(contract_state) st_1.(contract_state))).
  Proof.
  intros.
  unfold mstep1_since_init_BC in H3.
  assert (is_true (is_pre_initialization_phase pre_init_state)).
  { apply preinit_equiv. assumption. }
  unfold is_preinit_state in H0.
  split_conj.
  apply nil_nil in H0;
  apply nil_nil in H9;
  apply nil_nil in H10.
  pose (Hi_constate := init_constate_correct_BC H H5 H1 H2);
    clearbody Hi_constate.
  unfold init_constate_BC in Hi_constate.
  unfold mstep2_state; unfold mstep2_constate.
  inversion H2.
  unfold append_contract in H12;
    simpl in H12.
  split_conj. forward_replace.
  unfold_updates H13. (* now, we've brought init_state to the env *)
  inversion H4. (* main proof starts here *)
  unfold strategies in *.
  unfold protocol_implementation in *.
  destruct (is_pre_initialization_phase st_1) eqn:?.
  subst st st'.
  - (* preinit phase: contradiction *)
    apply preinit_equiv in Heqb.
    unfold is_preinit_state in Heqb.
    split_conj.
    rewrite H3 in H27. unfold is_nil in H27.
    discriminate.
  - (* not preinit phase *)
    destruct (is_leader st_1 p) eqn:?.
    + (* leader *)
      unfold is_leader in Heqb0.
      apply eq_eq in Heqb0.
      rewrite H8 in Heqb0; rewrite <- H12 in Heqb0;
        simpl in Heqb0.
      assert (p = player_A).
      { apply player_congruence. auto. }
      subst p.
      destruct (negb (outgoing_contracts_complete st_1 player_A)) eqn:?.
      * (* outgoing not complete: contradiction *)
        unfold outgoing_contracts_complete in Heqb1.
        unfold edge_contract_valid in Heqb1.
        rewrite H3 in Heqb1; rewrite <- H12 in Heqb1;
          simpl in Heqb1.
        replace (Int256.repr (Int256.intval A_to_B)) with A_to_B in Heqb1.
        replace (Int256.eq A_to_B A_to_B) with true in Heqb1.
        inversion Heqb1.
        symmetry; apply Int256.eq_true.
        symmetry; apply Int256.repr_unsigned.
      * (* outgoing complete *)
        destruct (incoming_contracts_complete st_1 player_A) eqn:?.
        -- (* incoming complete: contradiction *)
          unfold incoming_contracts_complete in Heqb2.
          unfold edge_contract_valid in Heqb2.
          rewrite H3 in Heqb2; rewrite <- H12 in Heqb2;
            rewrite H10 in Heqb2;
            simpl in Heqb2.
         inversion Heqb2.
        -- (* incoming incomplete : NO ACTION *)
          left.
          simpl in H24. inversion H24. auto.
    + (* follower *)
      pose (He := player_exclusivity p); clearbody He.
      inversion He.
      unfold is_leader in Heqb0. simpl in Heqb0.
      rewrite H8 in Heqb0; rewrite  H13 in Heqb0.
      subst p. simpl in Heqb0.
      inversion Heqb0. clear He.
      destruct H27.
      * (* player B *)
        destruct (incoming_contracts_complete st_1 p &&
         negb (outgoing_contracts_complete st_1 p)) eqn:?.
        -- (* can publish: contradiction *)
          subst p.
          unfold incoming_contracts_complete in Heqb1.
          unfold outgoing_contracts_complete in Heqb1.
          unfold edge_contract_valid in Heqb1.
          rewrite H3 in Heqb1; rewrite <- H12 in Heqb1;
            rewrite H10 in Heqb1;
            simpl in Heqb1.
          replace (Int256.repr (Int256.intval A_to_B)) with A_to_B in Heqb1.
          replace (Int256.eq A_to_B A_to_B) with true in Heqb1.
          replace (Int256.eq B_to_C B_to_C) with true in Heqb1.
          inversion Heqb1.
          symmetry; apply Int256.eq_true.
          symmetry; apply Int256.eq_true.
          symmetry; apply Int256.repr_unsigned.
        -- (* can't publish: NO ACTION *)
          left.
          subst p.
          replace (has_learned_secret st_1 player_B) with false in H24.
          simpl in H24. inversion H24. auto.
          unfold has_learned_secret.
          rewrite H7; rewrite <- H12; rewrite H0;
            reflexivity.
      * (* player C *)
        destruct (incoming_contracts_complete st_1 p &&
         negb (outgoing_contracts_complete st_1 p)) eqn:?.
        -- (* can publish: ACTION *)
          right.
          subst.
          simpl in H24.
          inversion H24. simpl.
          (* gets rid of all but the meaningful publish contract case *)
          repeat split; try reflexivity.
          unfold unpublished_contracts.
          rewrite H3; rewrite <- H12; rewrite H10;
            reflexivity.
          unfold unpublished_contracts.
          rewrite H3; rewrite <- H12; rewrite H10;
            reflexivity.
        -- (* can't publish: contradiction *)
          subst p.
          unfold outgoing_contracts_complete in Heqb1.
          unfold incoming_contracts_complete in Heqb1.
          rewrite H3 in Heqb1; rewrite <- H12 in Heqb1;
            rewrite H10 in Heqb1;
            simpl in Heqb1.
          replace (Int256.eq B_to_C B_to_C) with true in Heqb1.
          inversion Heqb1.
          symmetry; apply Int256.eq_true.
  Qed.

  (************************************************************)
  (*              Third step: AB, BC, CA live,                *)
  (*   prove on contract *CA*: A reveals secret + unlocks CA  *)
  (************************************************************)


  Definition mstep3_state (o i: state) : Prop :=
  o.(contracts_published) = (contracts_published i)
  /\ o.(player_transfers) = (A_address, 0%Z) :: (player_transfers i)
  /\ o.(secrets_revealed) = (A_address, A_secret) :: (secrets_revealed i).

  (* this is in regards to contract CA state; others unchanged *)
  Definition mstep3_constate (next_st st: global_abstract_data_type) : Prop :=
  next_st.(_assetAmount) = st.(_assetAmount)
  /\ next_st.(_leader) = st.(_leader)
  /\ next_st.(_hashlock) = st.(_hashlock)
  /\ next_st.(_timelock) = st.(_timelock)
  /\ next_st.(_party) = st.(_party)
  /\ next_st.(_counterparty) = st.(_counterparty)
  /\ next_st.(_otherparty) = st.(_otherparty)
  /\ next_st.(_start) = st.(_start)
  /\ next_st.(_delta) = st.(_delta)
  /\ next_st.(_unlocked) = true (* unlocked *)
  /\ next_st.(_ended) = st.(_ended)
  /\ next_st.(_events) = st.(_events).

  Definition mstep2_since_init_CA (o i: state) :Prop :=
  (* external state changed since init_state *)
  (contracts_published o) =
      [(Build_contract C_address A_address C_to_A);
       (Build_contract B_address C_address B_to_C);
       (Build_contract A_address B_address A_to_B)]
      ++ (contracts_published i)
  /\
  (player_transfers o) = 
      [(C_address, Int256.intval C_to_A);
       (B_address, Int256.intval B_to_C);
       (A_address, Int256.intval A_to_B)]
      ++ (player_transfers i)
  /\ (secrets_revealed o) = (secrets_revealed i)
  (* contract state is not changed since init_state *)
  /\ (contract_state o) = (contract_state i).


  Lemma mstep3_correct_CA (st_2 st_3: state) : 
  bounded_int256 C_to_A
  -> is_preinit_state pre_init_state
  -> init_global_abstract_data = pre_init_state.(contract_state)
  -> runStateT (filled_init_CA A_address B_address A_address hashlock start_CA delta)
               pre_init_state = Some (tt, init_state)
  (* after init_state *)
  -> mstep2_since_init_CA st_2 init_state
  -> mstep strategies initial_balances contract_address_CA
           coinbase_CA init_timestamp_CA number_CA blockhash_CA
     st_2 st_2 st_3
  -> st_3 = st_2
     \/
     ((mstep3_state st_3 st_2)
     /\ (mstep3_constate st_3.(contract_state) st_2.(contract_state))).
  Proof.
  intros.
  unfold mstep2_since_init_CA in H3.
  assert (is_true (is_pre_initialization_phase pre_init_state)).
  { apply preinit_equiv. assumption. }
  unfold is_preinit_state in H0.
  split_conj.
  apply nil_nil in H10; apply nil_nil in H9;
  apply nil_nil in H0.
  pose (H_1constate := init_constate_correct_CA H H5 H1 H2);
    clearbody H_1constate.
  unfold init_constate_CA in H_1constate.
  unfold mstep3_state. unfold mstep3_constate.
  inversion H2. 
  split_conj. forward_replace.
  unfold f_id in H12; simpl in H12.
  (* now all the desired properties from st_2 are in the env. *)
  inversion H4. (* main proof starts here, where we go into cases *)
  unfold strategies in *.
  unfold protocol_implementation in *.
  destruct (is_pre_initialization_phase st_2) eqn:?.
  subst st. subst st'.
  - (* preinit phase: contradiction *)
    apply preinit_equiv in Heqb.
    unfold is_preinit_state in Heqb.
    split_conj. rewrite H3 in H27.
    unfold is_nil in H27.
    discriminate.
  - (* not preinit phase *)
    subst st. subst st'.
    destruct (is_leader st_2 p) eqn:?.
    + (* leader *)
      unfold is_leader in Heqb0.
      apply eq_eq in Heqb0. rewrite H8 in *.
      rewrite H13 in Heqb0.
      assert (p = player_A).
        { apply player_congruence. auto. }
      subst.
      destruct (negb (outgoing_contracts_complete st_2 player_A)) eqn:?.
      * (* outgoing not complete: contradiction *)
        unfold outgoing_contracts_complete in Heqb1.
        unfold edge_contract_valid in Heqb1.
        rewrite <- H12 in H3; rewrite H10 in H3; simpl in H3.
        rewrite H3 in Heqb1.
        simpl in Heqb1.
        replace (Int256.eq A_to_B A_to_B) with true in Heqb1.
        inversion Heqb1.
        symmetry. apply Int256.eq_true.
      * (* outgoing complete *)
        destruct (incoming_contracts_complete st_2 player_A) eqn:?.
        -- (* incoming complete: ACTION *)
          right.
          rewrite H7 in H24.
          assert (secrets_revealed init_state = nil).
          { rewrite <- H12. simpl. assumption. }
          rewrite H25 in H24.
          simpl in H24.
          rewrite H8 in H24.
          rewrite <- H12 in H24.
          Transparent SwapContract_unlock_opt Hquery0 builtin0_timestamp_impl
                      builtin0_caller_impl.
          simpl in H24.
          inversion H24. simpl.
          repeat split; try auto.
          rewrite H11. apply Int256.repr_unsigned.
          rewrite <- H12; simpl.
          reflexivity. (* success! state successfully gets updated!! *)
        -- (* incoming incomplete : contradiction *)
          unfold incoming_contracts_complete in Heqb2.
          rewrite H3 in Heqb2.
          assert (contracts_published init_state = nil).
          { rewrite <- H12. simpl. assumption. }
          rewrite H25 in Heqb2.
          simpl in Heqb2.
          replace (Int256.eq C_to_A C_to_A) with true in Heqb2.
          inversion Heqb2.
          symmetry. apply Int256.eq_true.
    + (* follower *)
      pose (He := player_exclusivity p); clearbody He.
      inversion He.
      unfold is_leader in Heqb0. simpl in Heqb0.
      rewrite H8 in Heqb0. rewrite H13 in Heqb0.
      rewrite H25 in Heqb0.
      inversion Heqb0. clear He.
      destruct H25.
      * (* player B *)
        destruct (incoming_contracts_complete st_2 p &&
         negb (outgoing_contracts_complete st_2 p)) eqn:?.
        -- (* can publish: contradiction *)
          subst p.
          unfold incoming_contracts_complete in Heqb1.
          unfold outgoing_contracts_complete in Heqb1.
          unfold edge_contract_valid in Heqb1.
          rewrite H3 in Heqb1.
          assert (contracts_published init_state = nil).
          { rewrite <- H12. simpl. assumption. }
          rewrite H25 in Heqb1. simpl in Heqb1.
          replace (Int256.eq A_to_B A_to_B) with true in Heqb1.
          replace (Int256.eq B_to_C B_to_C) with true in Heqb1.
          inversion Heqb1.
          symmetry; apply Int256.eq_true.
          symmetry; apply Int256.eq_true.
        -- (* can't publish + no secrets revealed : NO ACTION *)
          left.
          subst p.
          replace (has_learned_secret st_2 player_B) with false in H24.
          simpl in H24. inversion H24. auto.
          unfold has_learned_secret.
          rewrite H7. rewrite <- H12.
          simpl. rewrite H0. reflexivity.
      * (* player C *)
        destruct (incoming_contracts_complete st_2 p &&
         negb (outgoing_contracts_complete st_2 p)) eqn:?.
        -- (* can publish: contradiction *)
          subst p.
          unfold incoming_contracts_complete in Heqb1.
          unfold outgoing_contracts_complete in Heqb1.
          unfold edge_contract_valid in Heqb1.
          rewrite H3 in Heqb1.
          assert (contracts_published init_state = nil).
          { rewrite <- H12. simpl. assumption. }
          rewrite H25 in Heqb1. simpl in Heqb1.
          replace (Int256.eq B_to_C B_to_C) with true in Heqb1.
          replace (Int256.eq C_to_A C_to_A) with true in Heqb1.
          inversion Heqb1.
          symmetry; apply Int256.eq_true.
          symmetry; apply Int256.eq_true.
        -- (* can't publish + no secrets revealed : NO ACTION *)
          left.
          subst p.
          replace (has_learned_secret st_2 player_C) with false in H24.
          simpl in H24. inversion H24. auto.
          unfold has_learned_secret.
          rewrite H7. rewrite <- H12.
          simpl. rewrite H0. reflexivity.
  Qed.


  (************************************************************)
  (*              Fourth step: AB, BC, CA live,               *)
  (*   prove on contract *CA*: A claims `C_to_A` from CA      *)
  (*   prove on contract *BC*: C unlocks BC + reveals secret  *)
  (************************************************************)

  Definition mstep4_state_CA (o i: state) : Prop :=
  o.(contracts_published) = (contracts_published i)
  /\ o.(player_transfers) = (A_address, 0%Z) :: (player_transfers i)
  /\ o.(secrets_revealed) = (secrets_revealed i).

  Definition mstep4_state_BC (o i: state) : Prop :=
  o.(contracts_published) = (contracts_published i)
  /\ o.(player_transfers) = (C_address, 0%Z) :: (player_transfers i)
  /\ o.(secrets_revealed) = (C_address, A_secret) :: (secrets_revealed i).

  Definition mstep4_constate_CA (next_st st: global_abstract_data_type) : Prop :=
  next_st.(_assetAmount) = st.(_assetAmount)
  /\ next_st.(_leader) = st.(_leader)
  /\ next_st.(_hashlock) = st.(_hashlock)
  /\ next_st.(_timelock) = st.(_timelock)
  /\ next_st.(_party) = st.(_party)
  /\ next_st.(_counterparty) = st.(_counterparty)
  /\ next_st.(_otherparty) = st.(_otherparty)
  /\ next_st.(_start) = st.(_start)
  /\ next_st.(_delta) = st.(_delta)
  /\ next_st.(_unlocked) = st.(_unlocked)
  /\ next_st.(_ended) = true (* ended *)
  /\ next_st.(_events) = (Build_Transfer C_to_A A_address) :: (_events st).

  Definition mstep4_constate_BC (next_st st: global_abstract_data_type) : Prop :=
  next_st.(_assetAmount) = st.(_assetAmount)
  /\ next_st.(_leader) = st.(_leader)
  /\ next_st.(_hashlock) = st.(_hashlock)
  /\ next_st.(_timelock) = st.(_timelock)
  /\ next_st.(_party) = st.(_party)
  /\ next_st.(_counterparty) = st.(_counterparty)
  /\ next_st.(_otherparty) = st.(_otherparty)
  /\ next_st.(_start) = st.(_start)
  /\ next_st.(_delta) = st.(_delta)
  /\ next_st.(_unlocked) = true (* unlocked *)
  /\ next_st.(_ended) = st.(_ended)
  /\ next_st.(_events) = (_events st).

  Definition mstep3_since_init_CA (o i: state) :Prop :=
  (* external state changed since init_state *)
  (contracts_published o) =
      [(Build_contract C_address A_address C_to_A);
       (Build_contract B_address C_address B_to_C);
       (Build_contract A_address B_address A_to_B)]
      ++ (contracts_published i)
  /\
  (player_transfers o) = 
      [(A_address, 0%Z);
       (C_address, Int256.intval C_to_A);
       (B_address, Int256.intval B_to_C);
       (A_address, Int256.intval A_to_B)]
      ++ (player_transfers i)
  /\ (secrets_revealed o) = (A_address, A_secret) :: (secrets_revealed i)
  (* convenient way to decouple contract state and general state. *)
  /\ mstep3_constate (contract_state o) (contract_state i).

  Definition mstep3_since_init_BC (o i: state) :Prop :=
    (* external state changed since init_state *)
  (contracts_published o) =
      [(Build_contract C_address A_address C_to_A);
       (Build_contract B_address C_address B_to_C);
       (Build_contract A_address B_address A_to_B)]
      ++ (contracts_published i)
  /\
  (player_transfers o) = 
      [(A_address, 0%Z); (* claim *)
       (A_address, 0%Z); (* unlock *)
       (C_address, Int256.intval C_to_A);
       (B_address, Int256.intval B_to_C);
       (A_address, Int256.intval A_to_B)]
      ++ (player_transfers i)
  /\ (secrets_revealed o) = (A_address, A_secret) :: (secrets_revealed i)
  (* convenient way to decouple contract state and general state. *)
  /\ (contract_state o)  = (contract_state i).

  Lemma mstep4_correct_CA (st_3 st_4: state) : 
  bounded_int256 C_to_A
  -> is_preinit_state pre_init_state
  -> init_global_abstract_data = pre_init_state.(contract_state)
  -> runStateT (filled_init_CA A_address B_address A_address hashlock start_CA delta)
               pre_init_state = Some (tt, init_state)
  (* after init_state *)
  -> mstep3_since_init_CA st_3 init_state
  -> mstep strategies initial_balances contract_address_CA
           coinbase_CA init_timestamp_CA number_CA blockhash_CA
     st_3 st_3 st_4
  -> st_4 = st_3
     \/
     ((mstep4_state_CA st_4 st_3)
     /\ (mstep4_constate_CA st_4.(contract_state) st_3.(contract_state))).
  Proof.
  intros.
  unfold mstep3_since_init_CA in H3.
  unfold mstep3_constate in H3.
  assert (is_true (is_pre_initialization_phase pre_init_state)).
  { apply preinit_equiv. assumption. }
  unfold is_preinit_state in H0.
  split_conj.
  apply nil_nil in H20;
  apply nil_nil in H21;
  apply nil_nil in H0.
  pose (H_1constate := init_constate_correct_CA H H5 H1 H2);
    clearbody H_1constate.
  unfold init_constate_CA in H_1constate.
  unfold mstep4_state_CA. unfold mstep4_constate_CA.
  inversion H2.
  split_conj. forward_replace.
  unfold f_id in H23; simpl in H23.
  (* now all the desired properties from st_2 are in the env. *)
  inversion H4. (* main proof starts here, where we go into cases *)
  unfold strategies in *.
  unfold protocol_implementation in *.
  destruct (is_pre_initialization_phase st_3) eqn:?.
  subst st. subst st'.
  - (* preinit phase: contradiction *)
    apply preinit_equiv in Heqb.
    unfold is_preinit_state in Heqb.
    split_conj. rewrite H3 in H26.
    unfold is_nil in H26.
    discriminate.
  - (* not preinit phase *)
    subst st. subst st'.
    destruct (is_leader st_3 p) eqn:?.
    + (* leader *)
      unfold is_leader in Heqb0.
      apply eq_eq in Heqb0.
      rewrite H9 in *.
      assert (p = player_A).
        { apply player_congruence. auto. }

      destruct (negb (outgoing_contracts_complete st_3 player_A)) eqn:?.
      * (* outgoing not complete: contradiction *)
        unfold outgoing_contracts_complete in Heqb1.
        unfold edge_contract_valid in Heqb1.
        rewrite <- H23 in H3; rewrite H21 in H3; simpl in H3.
        rewrite H3 in Heqb1.
        simpl in Heqb1.
        replace (Int256.eq A_to_B A_to_B) with true in Heqb1.
        inversion Heqb1.
        symmetry. apply Int256.eq_true.
      * (* outgoing complete *)
        subst p.
        destruct (incoming_contracts_complete st_3 player_A) eqn:?.
        -- (* incoming complete: ACTION - claim *)
          right.
          replace (negb (outgoing_contracts_complete st_3 player_A)) with false in H22.
          Transparent SwapContract_claim_opt SwapContract_unlock_opt EVMOpcode_transfer_opt
                      Hquery0 builtin0_timestamp_impl
                      builtin0_caller_impl.
          rewrite H7 in H22 (* secrets revealed *);
          rewrite <- H23 in H22 (* init_state *);
          simpl in H22;
          rewrite H18 in H22 (* ended *);
          rewrite H13 in H22 (* counterparty *);
          rewrite H17 in H22 (* unlocked *);
          simpl in H22.
          inversion H22. simpl.
          repeat split; try auto.
          rewrite H8. reflexivity.  (* success! state successfully gets updated!! *)
        -- (* incoming incomplete : contradiction *)
          unfold incoming_contracts_complete in Heqb2.
          rewrite H3 in Heqb2.
          assert (contracts_published init_state = nil).
          { rewrite <- H23. simpl. assumption. }
          rewrite H24 in Heqb2.
          simpl in Heqb2.
          replace (Int256.eq C_to_A C_to_A) with true in Heqb2.
          inversion Heqb2.
          symmetry. apply Int256.eq_true.
    + (* follower *)
      pose (He := player_exclusivity p); clearbody He.
      inversion He.
      unfold is_leader in Heqb0. simpl in Heqb0.
      rewrite H9 in Heqb0. rewrite H24 in Heqb0.
      simpl in Heqb0.
      replace (Int256.eq A_address A_address) with true in Heqb0;
        inversion Heqb0.
      clear He.
      destruct H24.
      * (* player B *)
        destruct (incoming_contracts_complete st_3 p &&
         negb (outgoing_contracts_complete st_3 p)) eqn:?.
        -- (* can publish: contradiction *)
          subst p.
          unfold incoming_contracts_complete in Heqb1.
          unfold outgoing_contracts_complete in Heqb1.
          unfold edge_contract_valid in Heqb1.
          rewrite H3 in Heqb1.
          assert (contracts_published init_state = nil).
          { rewrite <- H23. simpl. assumption. }
          rewrite H24 in Heqb1. simpl in Heqb1.
          replace (Int256.eq A_to_B A_to_B) with true in Heqb1.
          replace (Int256.eq B_to_C B_to_C) with true in Heqb1.
          inversion Heqb1.
          symmetry. apply Int256.eq_true.
          symmetry. apply Int256.eq_true.
        -- (* can't publish + no secrets from C revealed : NO ACTION *)
          left.
          subst p.
          replace (has_learned_secret st_3 player_B) with false in H22.
          simpl in H22. inversion H22. auto.
          unfold has_learned_secret.
          rewrite H7. rewrite <- H23.
          simpl. rewrite H0. reflexivity.
      * (* player C *)
        destruct (incoming_contracts_complete st_3 p &&
         negb (outgoing_contracts_complete st_3 p)) eqn:?.
        -- (* can publish: contradiction *)
          subst p.
          unfold incoming_contracts_complete in Heqb1.
          unfold outgoing_contracts_complete in Heqb1.
          unfold edge_contract_valid in Heqb1.
          rewrite H3 in Heqb1.
          assert (contracts_published init_state = nil).
          { rewrite <- H23. simpl. assumption. }
          rewrite H24 in Heqb1. simpl in Heqb1.
          replace (Int256.eq B_to_C B_to_C) with true in Heqb1.
          replace (Int256.eq C_to_A C_to_A) with true in Heqb1.
          inversion Heqb1.
          symmetry. apply Int256.eq_true.
          symmetry. apply Int256.eq_true.
        -- (* can't publish, with secrets revealed, but WRONG ENVIRONMENT -> FAILED ACTION *)
          left. (* left due to failed unlock action -> wrong contract environment *)
          subst p.
          Transparent SwapContract_claim_opt.
          replace (has_learned_secret st_3 player_C) with true in H22.
          replace (hashlock_unlocked st_3) with true in H22.
          unfold get_secret in H22; simpl in H22.
          rewrite H13 in H22. (* counterparty address <> C_address *)
          replace (Int256.eq C_address A_address) with false in H22.
          simpl in H22.
          rewrite H7 in H22; (* secrets_revealed *)
          rewrite <- H23 in H22;
          rewrite H0 in H22;
          rewrite H18 in H22; (* _ended *)
          simpl in H22.
          inversion H22. auto.

          auto.
          unfold has_learned_secret.
          rewrite H7; rewrite <- H23; rewrite H0; reflexivity.
  Qed.

  Lemma mstep4_correct_BC (st_3 st_4: state) : 
  bounded_int256 B_to_C
  -> is_preinit_state pre_init_state
  -> init_global_abstract_data = pre_init_state.(contract_state)
  -> runStateT (filled_init_BC A_address A_address C_address hashlock start_BC delta)
               pre_init_state = Some (tt, init_state)
  (* after init_state *)
  -> mstep3_since_init_BC st_3 init_state
  -> mstep strategies initial_balances contract_address_BC
           coinbase_BC init_timestamp_BC number_BC blockhash_BC
     st_3 st_3 st_4
  -> st_4 = st_3
     \/
     ((mstep4_state_BC st_4 st_3)
     /\ (mstep4_constate_BC st_4.(contract_state) st_3.(contract_state))).
  Proof.
  intros.
  unfold mstep3_since_init_BC in H3.
  assert (is_true (is_pre_initialization_phase pre_init_state)).
  { apply preinit_equiv. assumption. }
  unfold is_preinit_state in H0.
  split_conj.
  apply nil_nil in H10;
  apply nil_nil in H9;
  apply nil_nil in H0.
  pose (H_1constate := init_constate_correct_BC H H5 H1 H2);
    clearbody H_1constate.
  unfold init_constate_BC in H_1constate.
  unfold mstep4_state_BC. unfold mstep4_constate_BC.
  inversion H2.
  split_conj. forward_replace.
  unfold f_id in H12; simpl in H12.
  (* now all the desired properties from st_2 are in the env. *)
  inversion H4. (* main proof starts here, where we go into cases *)
  unfold strategies in *.
  unfold protocol_implementation in *.
  destruct (is_pre_initialization_phase st_3) eqn:?.
  subst st. subst st'.
  - (* preinit phase: contradiction *)
    apply preinit_equiv in Heqb.
    unfold is_preinit_state in Heqb.
    split_conj. rewrite H3 in H27.
    unfold is_nil in H27.
    discriminate.
  - (* not preinit phase *)
    subst st. subst st'.
    destruct (is_leader st_3 p) eqn:?.
    + (* leader *)
      unfold is_leader in Heqb0.
      apply eq_eq in Heqb0.
      rewrite H8 in Heqb0; rewrite H13 in Heqb0.
      assert (p = player_A).
        { apply player_congruence. auto. }
      subst p.
      destruct (negb (outgoing_contracts_complete st_3 player_A)) eqn:?.
      * (* outgoing not complete: contradiction *)
        unfold outgoing_contracts_complete in Heqb1.
        unfold edge_contract_valid in Heqb1.
        (* contracts_published st_3 *)
        rewrite <- H12 in H3; simpl in H3.
        rewrite H3 in Heqb1.
        simpl in Heqb1.
        replace (Int256.eq A_to_B A_to_B) with true in Heqb1.
        inversion Heqb1.
        symmetry. apply Int256.eq_true.
      * (* outgoing complete *)
        destruct (incoming_contracts_complete st_3 player_A) eqn:?.
        -- (* incoming complete : STATE ENVIRONMENT WRONG -> ACTION FAILS *)
          left. (* left due to failed claim action -> wrong contract environment *)
          Transparent SwapContract_claim_opt.
          rewrite H7 in H24; rewrite <- H12 in H24; rewrite H0 in H24; (* secrets *)
          simpl in H24;
          rewrite H8 in H24; (* constate equal *)
          rewrite H16 in H24; (* counterparty *)
          rewrite H21 in H24; (* ended *)
          simpl in H24.
          inversion H24. auto.
        -- (* incoming incomplete : contradiction *)
          unfold incoming_contracts_complete in Heqb2.
          rewrite H3 in Heqb2.
          assert (contracts_published init_state = nil).
          { rewrite <- H12. simpl. assumption. }
          rewrite H25 in Heqb2.
          simpl in Heqb2.
          replace (Int256.eq C_to_A C_to_A) with true in Heqb2.
          inversion Heqb2.
          symmetry. apply Int256.eq_true.
    + (* follower *)
      pose (He := player_exclusivity p); clearbody He.
      inversion He.
      unfold is_leader in Heqb0. simpl in Heqb0.
      rewrite H8 in Heqb0; rewrite H13 in Heqb0; rewrite H25 in Heqb0.
      simpl in Heqb0.
      replace (Int256.eq A_address A_address) with true in Heqb0;
        inversion Heqb0.
      clear He.
      destruct H25.
      * (* player B *)
        destruct (incoming_contracts_complete st_3 p &&
         negb (outgoing_contracts_complete st_3 p)) eqn:?.
        -- (* can publish: contradiction *)
          subst p.
          unfold incoming_contracts_complete in Heqb1.
          unfold outgoing_contracts_complete in Heqb1.
          unfold edge_contract_valid in Heqb1.
          rewrite H3 in Heqb1.
          assert (contracts_published init_state = nil).
          { rewrite <- H12. simpl. assumption. }
          rewrite H25 in Heqb1. simpl in Heqb1.
          replace (Int256.eq A_to_B A_to_B) with true in Heqb1.
          replace (Int256.eq B_to_C B_to_C) with true in Heqb1.
          inversion Heqb1.
          symmetry. apply Int256.eq_true.
          symmetry. apply Int256.eq_true.
        -- (* can't publish + no secrets from C revealed : NO ACTION *)
          left.
          subst p.
          replace (has_learned_secret st_3 player_B) with false in H24.
          simpl in H24. inversion H24. auto.
          unfold has_learned_secret.
          rewrite H7; rewrite <- H12; rewrite H0;
          reflexivity.
      * (* player C *)
        destruct (incoming_contracts_complete st_3 p &&
         negb (outgoing_contracts_complete st_3 p)) eqn:?.
        -- (* can publish: contradiction *)
          subst p.
          unfold incoming_contracts_complete in Heqb1.
          unfold outgoing_contracts_complete in Heqb1.
          unfold edge_contract_valid in Heqb1.
          rewrite H3 in Heqb1.
          assert (contracts_published init_state = nil).
          { rewrite <- H12. simpl. assumption. }
          rewrite H25 in Heqb1. simpl in Heqb1.
          replace (Int256.eq B_to_C B_to_C) with true in Heqb1.
          replace (Int256.eq C_to_A C_to_A) with true in Heqb1.
          inversion Heqb1.
          symmetry. apply Int256.eq_true.
          symmetry. apply Int256.eq_true.
        -- (* can unlock: ACTION *)
          right. subst p.
          replace (has_learned_secret st_3 player_C) with true in H24.
          replace (hashlock_unlocked st_3) with false in H24.
          unfold get_secret in H24;
          rewrite H7 in H24;
          simpl in H24;
          rewrite H8 in H24; (* con_st equal *)
          rewrite <- H12 in H24;
          rewrite <- H1 in H24; (* contract_state of pre_init_state *)
          unfold f_unlock in H24;
          simpl in H24.
          unfold_updates H24. (* process all the updates, magic! *)
          inversion H24. simpl.
          repeat split; rewrite H8; try auto.
          rewrite <- H12; reflexivity.
          unfold hashlock_unlocked; rewrite H8; rewrite H20;
            reflexivity.
          unfold has_learned_secret; rewrite H7;
            rewrite <- H12; reflexivity.
  Qed.


  (************************************************************)
  (*          Fifth step: AB, BC live; CA ended               *)
  (*   prove on contract *BC*: C claims `B_to_C` from BC      *)
  (*   prove on contract *AB*: B unlocks AB + reveals secret  *)
  (************************************************************)


  Definition mstep5_state_BC (o i: state) : Prop :=
  o.(contracts_published) = (contracts_published i)
  /\ o.(player_transfers) = (C_address, 0%Z) :: (player_transfers i)
  /\ o.(secrets_revealed) = (secrets_revealed i).

  Definition mstep5_state_AB (o i: state) : Prop :=
  o.(contracts_published) = (contracts_published i)
  /\ o.(player_transfers) = (B_address, 0%Z) :: (player_transfers i)
  /\ o.(secrets_revealed) = (B_address, A_secret) :: (secrets_revealed i).

  Definition mstep5_constate_BC (next_st st : global_abstract_data_type) : Prop :=
  next_st.(_assetAmount) = st.(_assetAmount)
  /\ next_st.(_leader) = st.(_leader)
  /\ next_st.(_hashlock) = st.(_hashlock)
  /\ next_st.(_timelock) = st.(_timelock)
  /\ next_st.(_party) = st.(_party)
  /\ next_st.(_counterparty) = st.(_counterparty)
  /\ next_st.(_otherparty) = st.(_otherparty)
  /\ next_st.(_start) = st.(_start)
  /\ next_st.(_delta) = st.(_delta)
  /\ next_st.(_unlocked) = st.(_unlocked)
  /\ next_st.(_ended) = true (* ended *)
  /\ next_st.(_events) = (Build_Transfer B_to_C C_address) :: (_events st).

  Definition mstep5_constate_AB (next_st st: global_abstract_data_type) : Prop :=
  next_st.(_assetAmount) = st.(_assetAmount)
  /\ next_st.(_leader) = st.(_leader)
  /\ next_st.(_hashlock) = st.(_hashlock)
  /\ next_st.(_timelock) = st.(_timelock)
  /\ next_st.(_party) = st.(_party)
  /\ next_st.(_counterparty) = st.(_counterparty)
  /\ next_st.(_otherparty) = st.(_otherparty)
  /\ next_st.(_start) = st.(_start)
  /\ next_st.(_delta) = st.(_delta)
  /\ next_st.(_unlocked) = true (* unlocked *)
  /\ next_st.(_ended) = st.(_ended)
  /\ next_st.(_events) = (_events st).

  Definition mstep4_since_init_BC (o i: state) :Prop :=
    (* external state changed since init_state *)
  (contracts_published o) =
      [(Build_contract C_address A_address C_to_A);
       (Build_contract B_address C_address B_to_C);
       (Build_contract A_address B_address A_to_B)]
      ++ (contracts_published i)
  /\
  (player_transfers o) = 
      [(C_address, 0%Z); (* unlock *)
       (A_address, 0%Z); (* claim *)
       (A_address, 0%Z); (* unlock *)
       (C_address, Int256.intval C_to_A);
       (B_address, Int256.intval B_to_C);
       (A_address, Int256.intval A_to_B)]
      ++ (player_transfers i)
  /\ (secrets_revealed o) = [(C_address, A_secret);
                             (A_address, A_secret)] ++ (secrets_revealed i)
  (* convenient way to decouple contract state and general state. *)
  /\ mstep4_constate_BC (contract_state o) (contract_state i).

  Definition mstep4_since_init_AB (o i: state) :Prop :=
    (* external state changed since init_state *)
  (contracts_published o) =
      [(Build_contract C_address A_address C_to_A);
       (Build_contract B_address C_address B_to_C)
       (*Build_contract A_address B_address A_to_B <- lifted_init published con *)]
      ++ (contracts_published i)
  /\
  (player_transfers o) = 
      [(C_address, 0%Z); (* claim *)
       (C_address, 0%Z); (* unlock *)
       (A_address, 0%Z); (* claim *)
       (A_address, 0%Z); (* unlock *)
       (C_address, Int256.intval C_to_A);
       (B_address, Int256.intval B_to_C)
       (* A_address, Int256.intval A_to_B <- init state has transfer *)]
      ++ (player_transfers i)
  /\ (secrets_revealed o) = [(C_address, A_secret);
                             (A_address, A_secret)] ++ (secrets_revealed i)
  (* convenient way to decouple contract state and general state. *)
  /\ (contract_state o)  = (contract_state i).



  Lemma mstep5_correct_BC (st_4 st_5: state) :
  bounded_int256 B_to_C
  -> is_preinit_state pre_init_state
  -> init_global_abstract_data = pre_init_state.(contract_state)
  -> runStateT (filled_init_BC A_address A_address C_address hashlock start_BC delta)
               pre_init_state = Some (tt, init_state)
  -> mstep4_since_init_BC st_4 init_state
  -> mstep strategies initial_balances contract_address_BC
           coinbase_BC init_timestamp_BC number_BC blockhash_BC
     st_4 st_4 st_5
  -> st_4 = st_5
     \/
     ((mstep5_state_BC st_5 st_4)
      /\ (mstep5_constate_BC st_5.(contract_state) st_4.(contract_state))).
  Proof.
  intros. unfold mstep4_since_init_BC in H3.
  unfold mstep4_constate_BC in H3.
  assert (is_true (is_pre_initialization_phase pre_init_state)).
  { apply preinit_equiv. assumption. }
  unfold is_preinit_state in H0.
  split_conj.
  apply nil_nil in H20;
  apply nil_nil in H21;
  apply nil_nil in H0.
  pose (H_1constate := init_constate_correct_BC H H5 H1 H2);
    clearbody H_1constate.
  unfold init_constate_BC in H_1constate.
  unfold mstep4_state_BC. unfold mstep4_constate_BC.
  inversion H2.
  split_conj. forward_replace.
  unfold f_id in H23; simpl in H23.
  (* now all the desired properties from st_2 are in the env. *)
  inversion H4. (* main proof starts here, where we go into cases *)
  unfold strategies in *.
  unfold protocol_implementation in *.
  destruct (is_pre_initialization_phase st_4) eqn:?.
  subst st. subst st'.
  - (* preinit phase: contradiction *)
    apply preinit_equiv in Heqb.
    unfold is_preinit_state in Heqb.
    split_conj. rewrite H3 in H26.
    unfold is_nil in H26.
    discriminate.
  - (* not preinit phase *)
    subst st. subst st'.
    destruct (is_leader st_4 p) eqn:?.
    + (* leader *)
      unfold is_leader in Heqb0.
      apply eq_eq in Heqb0.
      rewrite H9 in *.
      assert (p = player_A).
        { apply player_congruence. auto. }

      destruct (negb (outgoing_contracts_complete st_4 player_A)) eqn:?.
      * (* outgoing not complete: contradiction *)
        unfold outgoing_contracts_complete in Heqb1.
        unfold edge_contract_valid in Heqb1.
        rewrite <- H23 in H3; rewrite H21 in H3; simpl in H3.
        rewrite H3 in Heqb1.
        simpl in Heqb1.
        replace (Int256.eq A_to_B A_to_B) with true in Heqb1.
        inversion Heqb1.
        symmetry. apply Int256.eq_true.
      * (* outgoing complete *)
        subst p.
        destruct (incoming_contracts_complete st_4 player_A) eqn:?.
        -- (* incoming complete: wrong environment -> NO ACTION *)
          left.
          replace (negb (outgoing_contracts_complete st_4 player_A)) with false in H22.
          Transparent SwapContract_claim_opt SwapContract_unlock_opt EVMOpcode_transfer_opt
                      Hquery0 builtin0_timestamp_impl
                      builtin0_caller_impl.
          rewrite H7 in H22 (* secrets revealed *);
          rewrite <- H23 in H22 (* init_state *);
          simpl in H22;
          rewrite H18 in H22 (* ended *);
          rewrite H13 in H22 (* counterparty *);
          rewrite H17 in H22 (* unlocked *);
          simpl in H22.
          inversion H22; reflexivity.
        -- (* incoming incomplete : contradiction *)
          unfold incoming_contracts_complete in Heqb2.
          rewrite H3 in Heqb2.
          assert (contracts_published init_state = nil).
          { rewrite <- H23. simpl. assumption. }
          rewrite H24 in Heqb2.
          simpl in Heqb2.
          replace (Int256.eq C_to_A C_to_A) with true in Heqb2.
          inversion Heqb2.
          symmetry. apply Int256.eq_true.
    + (* follower *)
      pose (He := player_exclusivity p); clearbody He.
      inversion He.
      unfold is_leader in Heqb0. simpl in Heqb0.
      rewrite H9 in Heqb0. rewrite H24 in Heqb0.
      simpl in Heqb0.
      replace (Int256.eq A_address A_address) with true in Heqb0;
        inversion Heqb0.
      clear He.
      destruct H24.
      * (* player B *)
        destruct (incoming_contracts_complete st_4 p &&
         negb (outgoing_contracts_complete st_4 p)) eqn:?.
        -- (* can publish: contradiction *)
          subst p.
          unfold incoming_contracts_complete in Heqb1.
          unfold outgoing_contracts_complete in Heqb1.
          unfold edge_contract_valid in Heqb1.
          rewrite H3 in Heqb1.
          assert (contracts_published init_state = nil).
          { rewrite <- H23. simpl. assumption. }
          rewrite H24 in Heqb1. simpl in Heqb1.
          replace (Int256.eq A_to_B A_to_B) with true in Heqb1.
          replace (Int256.eq B_to_C B_to_C) with true in Heqb1.
          inversion Heqb1.
          symmetry. apply Int256.eq_true.
          symmetry. apply Int256.eq_true.
        -- (* can't publish, can unlock, but wrong contract env -> FAILED ACTION *)
          left.
          subst p.
          replace (has_learned_secret st_4 player_B) with true in H22.
          replace (hashlock_unlocked st_4) with true in H22.
          simpl in H22;
          rewrite H18 in H22; (* ended *)
          rewrite H13 in H22; (* counterparty *)
          simpl in H22.
          inversion H22; auto.
          unfold has_learned_secret.
          rewrite H7. rewrite <- H23.
          simpl. reflexivity.
      * (* player C *)
        destruct (incoming_contracts_complete st_4 p &&
         negb (outgoing_contracts_complete st_4 p)) eqn:?.
        -- (* can publish: contradiction *)
          subst p.
          unfold incoming_contracts_complete in Heqb1.
          unfold outgoing_contracts_complete in Heqb1.
          unfold edge_contract_valid in Heqb1.
          rewrite H3 in Heqb1.
          assert (contracts_published init_state = nil).
          { rewrite <- H23. simpl. assumption. }
          rewrite H24 in Heqb1. simpl in Heqb1.
          replace (Int256.eq B_to_C B_to_C) with true in Heqb1.
          replace (Int256.eq C_to_A C_to_A) with true in Heqb1.
          inversion Heqb1.
          symmetry. apply Int256.eq_true.
          symmetry. apply Int256.eq_true.
        -- (* claim B_to_C from contract B->C : ACTION *)
          right.
          subst p.
          Transparent SwapContract_claim_opt.
          replace (has_learned_secret st_4 player_C) with true in H22.
          replace (hashlock_unlocked st_4) with true in H22.
          unfold get_secret in H22; simpl in H22.
          rewrite H13 in H22.
          replace (Int256.eq C_address C_address) with true in H22.
          simpl in H22.
          rewrite H7 in H22; (* secrets_revealed *)
          rewrite <- H23 in H22;
          rewrite H0 in H22;
          rewrite H18 in H22; (* _ended *)
          rewrite H17 in H22; (* _unlocked *)
          simpl in H22.
          unfold mstep5_state_BC. unfold mstep5_constate_BC.
          inversion H22; simpl.
          repeat split; try assumption; try auto.
          rewrite H7; rewrite <- H23; rewrite H0;
          reflexivity. (* secrets_revealed *)
          rewrite H8; reflexivity.
          symmetry; apply Int256.eq_true.
          unfold has_learned_secret.
          rewrite H7; rewrite <- H23; rewrite H0;
          reflexivity.
  Qed.


  Lemma mstep5_correct_AB (st_4 st_5: state) : 
  bounded_int256 A_to_B
  -> is_preinit_state pre_init_state
  -> init_global_abstract_data = pre_init_state.(contract_state)
  -> runStateT (filled_init_AB A_address C_address B_address hashlock start_AB delta)
               pre_init_state = Some (tt, init_state)
  (* after init_state *)
  -> mstep4_since_init_AB st_4 init_state
  -> mstep strategies initial_balances contract_address_AB
           coinbase_AB init_timestamp_AB number_AB blockhash_AB
     st_4 st_4 st_5
  -> st_5 = st_4
     \/
     ((mstep5_state_AB st_5 st_4)
     /\ (mstep5_constate_AB st_5.(contract_state) st_4.(contract_state))).
  Proof.
  intros.
  unfold mstep4_since_init_AB in H3.
  assert (is_true (is_pre_initialization_phase pre_init_state)).
  { apply preinit_equiv. assumption. }
  unfold is_preinit_state in H0.
  split_conj.
  apply nil_nil in H10;
  apply nil_nil in H9;
  apply nil_nil in H0.
  pose (H_1constate := init_constate_correct_AB H H5 H1 H2);
    clearbody H_1constate.
  unfold init_constate_AB in H_1constate.
  unfold mstep5_state_AB. unfold mstep5_constate_AB.
  inversion H2.
  split_conj. forward_replace.
  unfold f_id in H12; simpl in H12.
  (* now all the desired properties from st_2 are in the env. *)
  inversion H4. (* main proof starts here, where we go into cases *)
  unfold strategies in *.
  unfold protocol_implementation in *.
  destruct (is_pre_initialization_phase st_4) eqn:?.
  subst st. subst st'.
  - (* preinit phase: contradiction *)
    apply preinit_equiv in Heqb.
    unfold is_preinit_state in Heqb.
    split_conj. rewrite H3 in H27.
    unfold is_nil in H27.
    discriminate.
  - (* not preinit phase *)
    subst st. subst st'.
    destruct (is_leader st_4 p) eqn:?.
    + (* leader *)
      unfold is_leader in Heqb0.
      apply eq_eq in Heqb0.
      rewrite H8 in Heqb0; rewrite H13 in Heqb0.
      assert (p = player_A).
        { apply player_congruence. auto. }
      subst p.
      destruct (negb (outgoing_contracts_complete st_4 player_A)) eqn:?.
      * (* outgoing not complete: contradiction *)
        unfold outgoing_contracts_complete in Heqb1.
        unfold edge_contract_valid in Heqb1.
        (* contracts_published st_3 *)
        rewrite <- H12 in H3; simpl in H3.
        rewrite H3 in Heqb1; rewrite H10 in Heqb1.
        simpl in Heqb1.
        replace (Int256.repr (Int256.intval A_to_B)) with A_to_B in Heqb1.
        replace (Int256.eq A_to_B A_to_B) with true in Heqb1.
        inversion Heqb1.
        symmetry; apply Int256.eq_true.
        symmetry; apply Int256.repr_unsigned.
      * (* outgoing complete *)
        destruct (incoming_contracts_complete st_4 player_A) eqn:?.
        -- (* incoming complete : STATE ENVIRONMENT WRONG -> ACTION FAILS *)
          left. (* left due to failed claim action -> wrong contract environment *)
          Transparent SwapContract_claim_opt.
          rewrite H7 in H24; rewrite <- H12 in H24; rewrite H0 in H24; (* secrets *)
          simpl in H24;
          rewrite H8 in H24; (* constate equal *)
          rewrite H16 in H24; (* counterparty *)
          rewrite H21 in H24; (* ended *)
          simpl in H24.
          inversion H24. auto.
        -- (* incoming incomplete : contradiction *)
          unfold incoming_contracts_complete in Heqb2.
          rewrite H3 in Heqb2.
          rewrite <- H12 in Heqb2;
          rewrite H10 in Heqb2;
          simpl in Heqb2.
          replace (Int256.eq C_to_A C_to_A) with true in Heqb2.
          inversion Heqb2.
          symmetry; apply Int256.eq_true.
    + (* follower *)
      pose (He := player_exclusivity p); clearbody He.
      inversion He.
      unfold is_leader in Heqb0. simpl in Heqb0.
      rewrite H8 in Heqb0; rewrite H13 in Heqb0; rewrite H25 in Heqb0.
      simpl in Heqb0.
      replace (Int256.eq A_address A_address) with true in Heqb0;
        inversion Heqb0.
      clear He.
      destruct H25.
      * (* player B *)
        destruct (incoming_contracts_complete st_4 p &&
         negb (outgoing_contracts_complete st_4 p)) eqn:?.
        -- (* can publish: contradiction *)
          subst p.
          unfold incoming_contracts_complete in Heqb1.
          unfold outgoing_contracts_complete in Heqb1.
          unfold edge_contract_valid in Heqb1.
          rewrite H3 in Heqb1.
          rewrite <- H12 in Heqb1;
          rewrite H10 in Heqb1;
          simpl in Heqb1.
          replace (Int256.repr (Int256.intval A_to_B)) with A_to_B in Heqb1.
          replace (Int256.eq A_to_B A_to_B) with true in Heqb1.
          replace (Int256.eq B_to_C B_to_C) with true in Heqb1.
          inversion Heqb1.
          symmetry; apply Int256.eq_true.
          symmetry; apply Int256.eq_true.
          symmetry; apply Int256.repr_unsigned.
        -- (* can unlock : ACTION *)
          right.
          subst p.
          replace (has_learned_secret st_4 player_B) with true in H24.
          replace (hashlock_unlocked st_4) with false in H24.
          unfold get_secret in H24;
          rewrite H7 in H24;
          simpl in H24;
          rewrite H8 in H24; (* con_st equal *)
          rewrite <- H12 in H24;
          rewrite <- H1 in H24; (* contract_state of pre_init_state *)
          unfold f_unlock in H24;
          simpl in H24.
          unfold_updates H24. (* process all the updates, magic! *)
          inversion H24. simpl.
          repeat split; rewrite H8; try auto.
          rewrite H11; apply Int256.repr_unsigned.
          unfold hashlock_unlocked; rewrite H8; rewrite H20;
            reflexivity.
          unfold has_learned_secret; rewrite H7;
            rewrite <- H12; reflexivity.
      * (* player C *)
        destruct (incoming_contracts_complete st_4 p &&
         negb (outgoing_contracts_complete st_4 p)) eqn:?.
        -- (* can publish: contradiction *)
          subst p.
          unfold incoming_contracts_complete in Heqb1.
          unfold outgoing_contracts_complete in Heqb1.
          unfold edge_contract_valid in Heqb1.
          rewrite H3 in Heqb1.
          rewrite <- H12 in Heqb1;
          rewrite H10 in Heqb1;
          simpl in Heqb1.
          replace (Int256.eq B_to_C B_to_C) with true in Heqb1.
          replace (Int256.eq C_to_A C_to_A) with true in Heqb1.
          inversion Heqb1.
          symmetry. apply Int256.eq_true.
          symmetry. apply Int256.eq_true.
        -- (* can claim: wrong environment -> FAILED ACTION *)
          left.
          subst p.
          replace (has_learned_secret st_4 player_C) with true in H24.
          replace (hashlock_unlocked st_4) with false in H24.
          unfold get_secret in H24;
          rewrite H7 in H24;
          simpl in H24;
          rewrite H8 in H24; (* con_st equal *)
          rewrite <- H12 in H24;
          rewrite <- H1 in H24; (* contract_state of pre_init_state *)
          unfold f_unlock in H24;
          simpl in H24.
          inversion H24; auto.
          unfold hashlock_unlocked; rewrite H8; rewrite H20;
            reflexivity.
          unfold has_learned_secret; rewrite H7;
            rewrite <- H12; reflexivity.
  Qed.


  (************************************************************)
  (*           Sixth step: AB live, BC, CA ended              *)
  (*   prove on contract *AB*: B claims `A_to_B` from AB      *)
  (************************************************************)

  Definition mstep6_state_AB (o i: state) : Prop :=
  o.(contracts_published) = (contracts_published i)
  /\ o.(player_transfers) = (B_address, 0%Z) :: (player_transfers i)
  /\ o.(secrets_revealed) = (secrets_revealed i).

  Definition mstep6_constate_AB (next_st st: global_abstract_data_type) : Prop :=
  next_st.(_assetAmount) = st.(_assetAmount)
  /\ next_st.(_leader) = st.(_leader)
  /\ next_st.(_hashlock) = st.(_hashlock)
  /\ next_st.(_timelock) = st.(_timelock)
  /\ next_st.(_party) = st.(_party)
  /\ next_st.(_counterparty) = st.(_counterparty)
  /\ next_st.(_otherparty) = st.(_otherparty)
  /\ next_st.(_start) = st.(_start)
  /\ next_st.(_delta) = st.(_delta)
  /\ next_st.(_unlocked) = st.(_unlocked)
  /\ next_st.(_ended) = true (* ended *)
  /\ next_st.(_events) = (Build_Transfer A_to_B B_address) :: (_events st).

  Definition mstep5_since_init_AB (o i: state) :Prop :=
  (* external state changed since init_state *)
  (contracts_published o) =
      [(Build_contract C_address A_address C_to_A);
       (Build_contract B_address C_address B_to_C)
       (*Build_contract A_address B_address A_to_B <- lifted_init published contract *)]
      ++ (contracts_published i)
  /\
  (player_transfers o) = 
      [(B_address, 0%Z); (* unlock *)
       (C_address, 0%Z); (* claim *)
       (C_address, 0%Z); (* unlock *)
       (A_address, 0%Z); (* claim *)
       (A_address, 0%Z); (* unlock *)
       (C_address, Int256.intval C_to_A);
       (B_address, Int256.intval B_to_C)]
      ++ (player_transfers i)
  /\ (secrets_revealed o) = [(B_address, A_secret);
                             (C_address, A_secret);
                             (A_address, A_secret)] ++ (secrets_revealed i)
  (* convenient way to decouple contract state and general state. *)
  /\ mstep5_constate_AB (contract_state o) (contract_state i).

  Lemma mstep6_correct_AB (st_5 st_6 :state) :
  bounded_int256 A_to_B
  -> is_preinit_state pre_init_state
  -> init_global_abstract_data = pre_init_state.(contract_state)
  -> runStateT (filled_init_AB A_address C_address B_address hashlock start_AB delta)
               pre_init_state = Some (tt, init_state)
  -> mstep5_since_init_AB st_5 init_state
  -> mstep strategies initial_balances contract_address_AB
           coinbase_AB init_timestamp_AB number_AB blockhash_AB
     st_5 st_5 st_6
  -> st_5 = st_6
     \/
     ((mstep6_state_AB st_6 st_5)
      /\ (mstep6_constate_AB st_6.(contract_state) st_5.(contract_state))).
  Proof.
  intros. unfold mstep5_since_init_AB in H3.
  unfold mstep5_constate_AB in H3.
  assert (is_true (is_pre_initialization_phase pre_init_state)).
  { apply preinit_equiv. assumption. }
  unfold is_preinit_state in H0.
  split_conj.
  apply nil_nil in H20;
  apply nil_nil in H21;
  apply nil_nil in H0.
  pose (H_1constate := init_constate_correct_AB H H5 H1 H2);
    clearbody H_1constate.
  unfold init_constate_AB in H_1constate.
  unfold mstep6_state_AB. unfold mstep6_constate_AB.
  inversion H2.
  split_conj. forward_replace.
  unfold f_id in H23; simpl in H23.
  (* now all the desired properties from st_2 are in the env. *)
  inversion H4. (* main proof starts here, where we go into cases *)
  unfold strategies in *.
  unfold protocol_implementation in *.
  destruct (is_pre_initialization_phase st_5) eqn:?.
  subst st. subst st'.
  - (* preinit phase: contradiction *)
    apply preinit_equiv in Heqb.
    unfold is_preinit_state in Heqb.
    split_conj. rewrite H3 in H26.
    unfold is_nil in H26.
    discriminate.
  - (* not preinit phase *)
    subst st. subst st'.
    destruct (is_leader st_5 p) eqn:?.
    + (* leader *)
      unfold is_leader in Heqb0.
      apply eq_eq in Heqb0.
      rewrite H9 in *.
      assert (p = player_A).
        { apply player_congruence. auto. }
      subst p.
      destruct (negb (outgoing_contracts_complete st_5 player_A)) eqn:?.
      * (* outgoing not complete: contradiction *)
        unfold outgoing_contracts_complete in Heqb1.
        unfold edge_contract_valid in Heqb1.
        rewrite <- H23 in H3; rewrite H21 in H3; simpl in H3.
        rewrite H3 in Heqb1.
        simpl in Heqb1.
        replace (Int256.repr (Int256.intval A_to_B)) with A_to_B in Heqb1.
        replace (Int256.eq A_to_B A_to_B) with true in Heqb1.
        inversion Heqb1.
        symmetry; apply Int256.eq_true.
        symmetry; apply Int256.repr_unsigned.
      * (* outgoing complete *)
        destruct (incoming_contracts_complete st_5 player_A) eqn:?.
        -- (* incoming complete: wrong environment -> NO ACTION *)
          left.
          replace (is_nil (secrets_revealed st_5)) with false in H22.
          simpl in H22.
          Transparent SwapContract_claim_opt SwapContract_unlock_opt EVMOpcode_transfer_opt
                      Hquery0 builtin0_timestamp_impl
                      builtin0_caller_impl.
          rewrite H7 in H22 (* secrets revealed *);
          rewrite <- H23 in H22 (* init_state *);
          simpl in H22;
          rewrite H18 in H22 (* ended *);
          rewrite H13 in H22 (* counterparty *);
          rewrite H17 in H22 (* unlocked *);
          simpl in H22.
          inversion H22; reflexivity.
          rewrite H7; auto.
        -- (* incoming incomplete : contradiction *)
          unfold incoming_contracts_complete in Heqb2.
          rewrite H3 in Heqb2.
          rewrite <- H23 in Heqb2;
          rewrite <- H21 in Heqb2;
          simpl in Heqb2.
          replace (Int256.eq C_to_A C_to_A) with true in Heqb2.
          inversion Heqb2.
          symmetry; apply Int256.eq_true.
    + (* follower *)
      pose (He := player_exclusivity p); clearbody He.
      inversion He.
      unfold is_leader in Heqb0. simpl in Heqb0.
      rewrite H9 in Heqb0. rewrite H24 in Heqb0.
      simpl in Heqb0.
      replace (Int256.eq A_address A_address) with true in Heqb0;
        inversion Heqb0.
      clear He.
      destruct H24.
      * (* player B *)
        destruct (incoming_contracts_complete st_5 p &&
         negb (outgoing_contracts_complete st_5 p)) eqn:?.
        -- (* can publish: contradiction *)
          subst p.
          unfold incoming_contracts_complete in Heqb1.
          unfold outgoing_contracts_complete in Heqb1.
          unfold edge_contract_valid in Heqb1.
          rewrite H3 in Heqb1.
          rewrite <- H23 in Heqb1;
          rewrite H21 in Heqb1;
          simpl in Heqb1.
          replace (Int256.repr (Int256.intval A_to_B)) with A_to_B in Heqb1.
          replace (Int256.eq A_to_B A_to_B) with true in Heqb1.
          replace (Int256.eq B_to_C B_to_C) with true in Heqb1.
          inversion Heqb1.
          symmetry; apply Int256.eq_true.
          symmetry; apply Int256.eq_true.
          symmetry; apply Int256.repr_unsigned.
        -- (* claim: ACTION *)
          right.
          subst p.
          replace (has_learned_secret st_5 player_B) with true in H22.
          replace (hashlock_unlocked st_5) with true in H22.
          simpl in H22;
          rewrite H18 in H22; (* ended *)
          rewrite H13 in H22; (* counterparty *)
          rewrite H17 in H22; (* _unlocked *)
          simpl in H22.
          inversion H22. simpl.
          repeat split; try assumption; try auto.
          rewrite H8; reflexivity.
          unfold has_learned_secret.
          rewrite H7; rewrite <- H23; rewrite H0;
          reflexivity.
      * (* player C *)
        destruct (incoming_contracts_complete st_5 p &&
         negb (outgoing_contracts_complete st_5 p)) eqn:?.
        -- (* can publish: contradiction *)
          subst p.
          unfold incoming_contracts_complete in Heqb1.
          unfold outgoing_contracts_complete in Heqb1.
          unfold edge_contract_valid in Heqb1.
          rewrite H3 in Heqb1.
          rewrite <- H23 in Heqb1;
          rewrite H21 in Heqb1;
          simpl in Heqb1.
          replace (Int256.eq B_to_C B_to_C) with true in Heqb1.
          replace (Int256.eq C_to_A C_to_A) with true in Heqb1.
          inversion Heqb1.
          symmetry. apply Int256.eq_true.
          symmetry. apply Int256.eq_true.
        -- (* wrong contract env -> FAILED ACTION -> SAME STATE *)
          left.
          subst p.
          Transparent SwapContract_claim_opt.
          replace (has_learned_secret st_5 player_C) with true in H22.
          replace (hashlock_unlocked st_5) with true in H22.
          unfold get_secret in H23; simpl in H22.
          rewrite H13 in H22;
          rewrite H7 in H22; (* secrets_revealed *)
          rewrite <- H23 in H22;
          rewrite H0 in H22;
          rewrite H18 in H22; (* _ended *)
          rewrite H17 in H22; (* _unlocked *)
          simpl in H22.
          inversion H22; auto.
          unfold has_learned_secret.
          rewrite H7; rewrite <- H23; rewrite H0;
          reflexivity.
  Qed.

  (* final external state *)
  Definition final_since_init (o i: state) :Prop :=
  (* external state changed since init_state *)
  (contracts_published o) =
      [(Build_contract C_address A_address C_to_A);
       (Build_contract B_address C_address B_to_C)
       (*Build_contract A_address B_address A_to_B <- lifted_init published contract *)]
      ++ (contracts_published i)
  /\
  (player_transfers o) = 
      [(B_address, 0%Z); (* claim *)
       (B_address, 0%Z); (* unlock *)
       (C_address, 0%Z); (* claim *)
       (C_address, 0%Z); (* unlock *)
       (A_address, 0%Z); (* claim *)
       (A_address, 0%Z); (* unlock *)
       (C_address, Int256.intval C_to_A);
       (B_address, Int256.intval B_to_C)]
      ++ (player_transfers i)
  /\ (secrets_revealed o) = [(B_address, A_secret);
                             (C_address, A_secret);
                             (A_address, A_secret)] ++ (secrets_revealed i).

  Definition final_constate_since_init_AB (o i: global_abstract_data_type): Prop:=
  exists (st: global_abstract_data_type),
  mstep5_constate_AB st i
  /\ mstep6_constate_AB o st.

  Definition final_constate_since_init_BC (o i: global_abstract_data_type): Prop :=
  exists (st: global_abstract_data_type),
  mstep4_constate_BC st i
  /\ mstep5_constate_BC o st.

  Definition final_constate_since_init_CA (o i: global_abstract_data_type): Prop :=
  exists (st: global_abstract_data_type),
  mstep3_constate st i
  /\ mstep4_constate_CA o st.


  (************************************************************)
  (*                State transition proofs                   *)
  (************************************************************)

  (*  At the start, we have the following: 
     { init_state_AB, init_state_BC, init_state_CA }
     { init_constate_AB, init_constate_BC, init_constate_CA }

     The main idea is that external state (transfers, published
      contracts, and secrets revealed) is common knowledge, and
     it is directly inherited at each step from the previous step,
     regardless of which contract.

      So that `mstep*_since_init_*` is well defined if it correctly
      reflects the external state as well as the contract state
      since initialization.
   *)

  (* Lemma: mstep1_since_init_AB correctly defined. *)
  Lemma transition_correct_1 :
    forall (o i_st: state),
    mstep1_state o i_st
    /\ mstep1_constate (contract_state o) (contract_state i_st)
    -> mstep1_since_init_AB o i_st.
  Proof.
  intros.
  unfold mstep1_state in H; unfold mstep1_constate in H.
  unfold mstep1_since_init_AB.
  split_conj.
  repeat split; try assumption; try auto.
  destruct o; destruct i_st; simpl in *.
  destruct contract_state; destruct contract_state0; simpl in *.
  subst.
  reflexivity.
  Qed.

  (* Lemma: mstep1_since_init_BC correctly defined. *)
  Lemma transition_correct_2 :
  forall (o st i_st i_st': state),
  mstep1_since_init_AB st i_st'
  (* inherits external state from previous state *)
  /\ (contracts_published st) = (contracts_published o)
  /\ (player_transfers st) = (player_transfers o)
  /\ (secrets_revealed st) = (secrets_revealed o)
  (* comparing init_state_AB with init_state_BC *)
  /\ (contracts_published i_st') = (Build_contract A_address B_address A_to_B) :: (contracts_published i_st)
  /\ (player_transfers i_st') = (A_address, Int256.intval A_to_B) :: (player_transfers i_st)
  /\ (secrets_revealed i_st) = (secrets_revealed i_st')
  (* contract state of BC didn't change since it was published *)
  /\ (contract_state o) = (contract_state i_st)
  -> mstep1_since_init_BC o i_st.
  Proof.
  intros.
  unfold mstep1_since_init_AB in H;
  split_conj.
  unfold mstep1_since_init_BC.
  repeat split.
  - rewrite <- H0; rewrite H; rewrite H3; reflexivity.
  - rewrite <- H1; rewrite H7; rewrite H4; reflexivity.
  - rewrite <- H2; rewrite H8; rewrite H5; reflexivity.
  - assumption.
  Qed.

  (* Lemma: mstep2_since_init_CA correctly defined. *)
  Lemma transition_correct_3 :
  forall (o st i_st i_st' :state),
  mstep1_since_init_BC st i_st'
  (* initial state *)
  /\ (contracts_published i_st') = (contracts_published i_st)
  /\ (player_transfers i_st') = (player_transfers i_st)
  /\ (secrets_revealed i_st') = (secrets_revealed i_st)
  (* contract state of CA didn't change since it was published *)
  /\ (contract_state o) = (contract_state i_st)
  (* external state change *)
  /\ mstep2_state o st
  -> mstep2_since_init_CA o i_st.
  Proof.
  intros.
  unfold mstep1_since_init_BC in H;
  unfold mstep2_state in H.
  split_conj.
  unfold mstep2_since_init_CA.
  repeat split.
  - rewrite H4; rewrite H; rewrite H0; reflexivity.
  - rewrite H5; rewrite H7; rewrite H1; reflexivity.
  - rewrite H6; rewrite H8; rewrite H2; reflexivity.
  - rewrite H3; reflexivity.
  Qed.

  (* Lemma: mstep3_since_init_CA correctly defined. *)
  Lemma transition_correct_4 :
  forall (o st i_st: state),
  mstep2_since_init_CA st i_st
  (* how state & contract state changed A reveals secret+unlocks contract CA *)
  /\ mstep3_state o st
  /\ mstep3_constate (contract_state o) (contract_state st)
  -> mstep3_since_init_CA o i_st.
  Proof.
  intros.
  unfold mstep2_since_init_CA in H;
  unfold mstep3_state in H;
  unfold mstep3_constate in H.
  unfold mstep3_since_init_CA.
  split_conj.
  unfold mstep3_constate.
  rewrite H17 in *.
  repeat split; try assumption.
  - rewrite H0; rewrite H; reflexivity.
  - rewrite H13; rewrite H15; reflexivity.
  - rewrite H14; rewrite H16; reflexivity.
  Qed.

  (* Lemma: mstep3_since_init_BC correctly defined. *)
  Lemma transition_correct_5 :
  forall (o st i_st i_st': state),
  mstep3_since_init_CA st i_st'
  (* initial state *)
  /\ (contracts_published i_st') = (contracts_published i_st)
  /\ (player_transfers i_st') = (player_transfers i_st)
  /\ (secrets_revealed i_st') = (secrets_revealed i_st)
  (* contract state of BC didn't change since it was published *)
  /\ (contract_state o) = (contract_state i_st)
  (* how state changed after A claims `C_to_A` from CA *)
  /\ mstep4_state_CA o st
  -> mstep3_since_init_BC o i_st.
  Proof.
  intros.
  unfold mstep3_since_init_CA in H;
  unfold mstep4_state_CA in H.
  unfold mstep3_since_init_BC.
  split_conj.
  repeat split; try assumption.
  - rewrite H4; rewrite H; rewrite H0; reflexivity.
  - rewrite H5; rewrite H7; rewrite H1; reflexivity.
  - rewrite H6; rewrite H8; rewrite H2; reflexivity.
  Qed.

  (* Lemma: mstep4_since_init_BC correctly defined. *)
  Lemma transition_correct_6 :
  forall (o st i_st: state),
  (* external state at last step *)
  mstep3_since_init_BC st i_st
  /\ mstep4_constate_BC (contract_state o) (contract_state st)
  /\ mstep4_state_BC o st
  -> mstep4_since_init_BC o i_st.
  Proof.
  intros.
  unfold mstep3_since_init_BC in H;
  unfold mstep4_constate_BC in H;
  unfold mstep4_state_BC in H.
  unfold mstep4_since_init_BC;
  unfold mstep4_constate_BC.
  split_conj. rewrite H1 in *.
  repeat split; try (rewrite <- H17; assumption).
  - rewrite H; reflexivity.
  - rewrite H2; rewrite H15; reflexivity.
  - rewrite H3; rewrite H16; reflexivity.
  - assumption.
  Qed.


  (* Lemma: mstep4_since_init_AB correctly defined. *)
  Lemma transition_correct_7 :
  forall (o st i_st i_st' st': state),
  mstep4_since_init_BC st i_st
  (* initial state *)
  /\ (contracts_published i_st') = (Build_contract A_address B_address A_to_B) :: (contracts_published i_st)
  /\ (player_transfers i_st') = (A_address, Int256.intval A_to_B) :: (player_transfers i_st)
  /\ (secrets_revealed i_st') = (secrets_revealed i_st)
  (* lastest contract state for AB *)
  /\ mstep1_since_init_AB st' i_st'
  /\ (contract_state o) = (contract_state st')
  (* how external state changes *)
  /\ mstep5_state_BC o st
  -> mstep4_since_init_AB o i_st'.
  Proof.
  intros.
  unfold mstep4_since_init_BC in H;
  unfold mstep1_since_init_AB in H;
  unfold mstep5_state_BC in H.
  unfold mstep4_since_init_AB.
  split_conj.
  repeat split.
  - rewrite H5; rewrite H; rewrite H0; reflexivity.
  - rewrite H6; rewrite H11; rewrite H1; reflexivity.
  - rewrite H7; rewrite H12; rewrite H2; reflexivity.
  - rewrite H4; rewrite H10; reflexivity.
  Qed.

  (* Lemma: mstep5_since_init_AB correctly defined. *)
  Lemma transition_correct_8 :
  forall (o st i_st: state),
  mstep4_since_init_AB st i_st
  /\ mstep5_state_AB o st
  /\ mstep5_constate_AB (contract_state o) (contract_state st)
  -> mstep5_since_init_AB o i_st.
  Proof.
  intros.
  unfold mstep4_since_init_AB in H;
  unfold mstep5_state_AB in H;
  unfold mstep5_constate_AB in H.
  unfold mstep5_since_init_AB.
  split_conj.
  unfold mstep5_constate_AB.
  rewrite H17 in *.
  repeat split; try assumption.
  - rewrite H0; rewrite H; reflexivity.
  - rewrite H13; rewrite H15; reflexivity.
  - rewrite H14; rewrite H16; reflexivity.
  Qed.

  (* Lemma: final_since_init correctly defined *)
  Lemma transition_correct_9 :
  forall (o st st' i_st: state),
  mstep5_since_init_AB st i_st
  /\ mstep6_state_AB o st
  /\ mstep6_constate_AB (contract_state o) (contract_state st)
  -> final_since_init o i_st
     /\ final_constate_since_init_AB (contract_state o) (contract_state i_st).
  Proof.
  intros.
  unfold mstep5_since_init_AB in H;
  unfold mstep6_state_AB in H.
  split_conj.
  unfold final_since_init;
  unfold final_constate_since_init_AB.
  repeat split.
  - rewrite H0; rewrite H; reflexivity.
  - rewrite H2; rewrite H4; reflexivity.
  - rewrite H3; rewrite H5; reflexivity.
  - exists (contract_state st). split; try assumption.
  Qed.

(* 
  Theorem transaction_success (o: state) :
    is_true (is_pre_initialization_phase pre_init_state)
    -> init_global_abstract_data = pre_init_state.(contract_state)
    (* contract state is given by running initialize_opt *)
    -> runStateT (filled_init A_to_B A_address C_address B_address init_hashlock init_start init_delta)
       pre_init_state = Some (tt, init_state)
    -> init_state.(player_transfers) = nil
    -> init_state.(contract_state).(_events) = nil
    -> init_state.(secrets_revealed) = nil
    -> is_true (contract_ended o)
    -> multi_bstep strategies initial_balances contract_address init_state o
    (* transactions go as planned *)
    -> player_bestpayoff player_A = player_netgain player_A o.
  Admitted.
*)

End outcome_proof.
