Require Import backend.MachineModel.
Require Import cclib.Integers.
Require Import LayerAUCTION.
Require Import backend.Values.
Require Import backend.BuiltinSemantics.
Require Import DeepSpec.lib.Monad.StateMonadOption.
Require Import DataTypeOps.



Obligation Tactic := try eexists; simpl; eauto;  try congruence.
Program Definition simple_machine_env
        (address origin caller callvalue
         coinbase timestamp number : int256) : machine_env :=
  {| me_query q :=
       match q with
       | Qcall0 Baddress => Vint address
       | Qcall0 Borigin => Vint origin
       | Qcall0 Bcaller => Vint caller
       | Qcall0 Bcallvalue => Vint callvalue
       | Qcall0 Bcoinbase => Vint coinbase
       | Qcall0 Btimestamp => Vint timestamp
       | Qcall0 Bnumber => Vint number
       | Qcall1 Bsha_1 v => Vunit (* todo. *)
       | Qcall1 Bbalance v => Vunit (* todo. *)
       | Qcall1 Bblockhash v => Vunit (* todo. *)
       | Qcall2 Bsha_2 (Vptr a1p) (Vint a2i) => Vptr (Ihash a1p a2i)
       | Qcall2 Bsha_2 _ _ => Vunit (* todo. *)
       end ;
     me_transfer _ _ _ _ := True;
     me_callmethod _ _ _ _ _ _ _ := True
  |}.

Definition from_option {A:Type} (default:A) (x:option A) :A :=
  match x with
  | None => default
  | Some y => y
  end.

Require Import ZArith.
Require Import cclib.Maps.

Section WithMem.

Import core.MemoryModel.
  
Context {HmemOps: MemoryModelOps mem}.

Section AuctionVerification.

  Parameter owner : int256.
  Parameter contract_address : int256.
  Parameter deadline : int256.

  Context (init_coinbase
           init_timestamp
           init_number : int256).  

  Transparent OpenAuction_initialize_opt.
  Transparent  HyperTypeInst.builtin0_caller_impl HyperTypeInst.builtin0_callvalue_impl HyperTypeInst.builtin0_coinbase_impl HyperTypeInst.builtin0_timestamp_impl HyperTypeInst.builtin0_number_impl.
  
  Definition OpenAuction_initial :=
    Eval cbv -[Int256Tree.empty Int256.zero]
    in
      from_option init_global_abstract_data
                  (execStateT (OpenAuction_initialize_opt
                                 deadline
                                 (simple_machine_env contract_address owner owner Int256.zero
                                                     init_coinbase init_timestamp init_number))
                              init_global_abstract_data).

  