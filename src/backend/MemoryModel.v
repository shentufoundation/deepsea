
Require Import cclib.Coqlib.
Require Import cclib.Maps.
Require Import cclib.Integers.
Require Import backend.AST.
Require Import backend.IndexLib.

(* For the ethereum backend we do not use memory injections, but we
still define a dummy datatype in order to make the middle-end more
portable between C and Ethereum. *)
Definition meminj : Set := unit.

(* The type of offsets into structs etc. It will be int in the C backend and int256 in the Ethereum backend. *)
Definition offset_t := int256.

(* after hashing, the rest of memory is for retvals and args *)
Definition public_retval_base : Z := 96%Z.
(* according to ABI, this is how big each arg/retval is *)
Definition abi_word_size : Z := 32%Z.

(* move funsig over to left-align the 4 bytes to 256 bytes *)
Definition funsig_aligned (funsig: int) : int256 :=
  let funsig := Int.unsigned funsig in
  Int256.repr (Z.shiftl funsig 224).

(* locations in memory for CALL args *)
Definition public_funsig_pos (retval_count: nat) : Z :=
  public_retval_base + (abi_word_size * (Z.of_nat retval_count)).
Definition public_arg_pos (index: nat) (retval_count: nat) : Z :=
  (public_funsig_pos retval_count) + ((Z.of_nat index) * abi_word_size) + 4%Z.
Definition public_arg_size (arg_count: nat) : Z :=
  ((Z.of_nat arg_count) * abi_word_size) + 4%Z.
Definition retval_pos (index: nat) : Z :=
  public_retval_base + (abi_word_size * (Z.of_nat index)).

(* arguments to CALL *)
Definition argpos (retval_count: nat) : Z := public_funsig_pos retval_count.
Definition arglen (arg_count: nat) : Z := public_arg_size arg_count.
Definition retpos : Z := public_retval_base.
Definition retlen (retval_count: nat) : Z := abi_word_size * (Z.of_nat retval_count).

(* according to the ABI specification,
Calldata locations *)
Definition call_data_arg_location (argi: nat) : int256 :=
  Int256.repr (((Z.of_nat argi) * 32%Z) + 4%Z).

(* add two consts for codecopy *)
Definition bytes_to_fetch : int256 := Int256.repr 32%Z.

Definition MemoryLocation : int256 := Int256.repr 128%Z.  