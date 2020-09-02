open AST
open Ascii
open BinNums
open Compiled
open Cop
open Datatypes
open EVM
open Integers
open MachineModel
open Maps0
open Monad
open OptErrMonad
open PeanoNat
open String0

(** val dup_ident : nat -> compiled **)

let dup_ident required_index =
  match Nat.leb required_index (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
          O))))))))))))))) with
  | Coq_true -> command_compiled (Coq_evm_dup (S required_index))
  | Coq_false ->
    error_compiled (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
      Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
      Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
      Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true,
      Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
      Coq_true, Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
      Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
      Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
      Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
      Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
      (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_true,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
      Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
      (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
      Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
      Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
      (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
      Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
      (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
      Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
      ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
      Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
      Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
      Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
      (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
      Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
      ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
      Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
      Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
      Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
      Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
      Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
      EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

(** val global_address : coq_Z PTree.t -> ident -> compiled **)

let global_address ge id =
  let lookup = PTree.get id ge in
  (match lookup with
   | Some addr -> command_compiled (Coq_evm_push (Int256.repr addr))
   | None ->
     error_compiled (String ((Ascii (Coq_true, Coq_true, Coq_false,
       Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
       (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
       Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
       Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
       (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
       Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
       Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
       (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
       Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
       Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
       (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_false, Coq_false,
       Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
       Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
       (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
       Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
       Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
       Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
       Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
       ((Ascii (Coq_true, Coq_true, Coq_true, Coq_false, Coq_false, Coq_true,
       Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
       Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
       (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
       Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
       Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
       (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
       Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
       Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
       Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
       Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
       ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
       Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
       Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
       Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
       Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
       (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
       Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
       Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
       (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
       Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
       Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
       (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
       Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
       Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
       (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
       Coq_true, Coq_true, Coq_false)),
       EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

(** val sha_base : Int256.int **)

let sha_base =
  Int256.repr (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))

(** val sha_arg2 : Int256.int **)

let sha_arg2 =
  Int256.repr (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
    Coq_xH)))))))

(** val sha_size1 : Int256.int **)

let sha_size1 =
  Int256.repr (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))

(** val sha_size2 : Int256.int **)

let sha_size2 =
  Int256.repr (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
    Coq_xH)))))))

(** val sha_1_compiled : compiled **)

let sha_1_compiled =
  append_compiled Coq_evm_sha3
    (append_compiled (Coq_evm_push sha_base)
      (append_compiled (Coq_evm_push sha_size1)
        (append_compiled Coq_evm_mstore
          (command_compiled (Coq_evm_push sha_base)))))

(** val sha_2_compiled : compiled **)

let sha_2_compiled =
  append_compiled Coq_evm_sha3
    (append_compiled (Coq_evm_push sha_base)
      (append_compiled (Coq_evm_push sha_size2)
        (append_compiled Coq_evm_mstore
          (append_compiled (Coq_evm_push sha_arg2)
            (append_compiled Coq_evm_mstore
              (command_compiled (Coq_evm_push sha_base)))))))

(** val binop_compiled : binary_operation -> bool -> compiled **)

let binop_compiled op signed =
  match op with
  | Oadd -> ret (Obj.magic coq_Monad_optErr) (Coq_cons (Coq_evm_add, Coq_nil))
  | Osub -> ret (Obj.magic coq_Monad_optErr) (Coq_cons (Coq_evm_sub, Coq_nil))
  | Omul -> ret (Obj.magic coq_Monad_optErr) (Coq_cons (Coq_evm_mul, Coq_nil))
  | Odiv ->
    ret (Obj.magic coq_Monad_optErr) (Coq_cons
      ((match signed with
        | Coq_true -> Coq_evm_sdiv
        | Coq_false -> Coq_evm_div), Coq_nil))
  | Omod ->
    ret (Obj.magic coq_Monad_optErr) (Coq_cons
      ((match signed with
        | Coq_true -> Coq_evm_smod
        | Coq_false -> Coq_evm_mod), Coq_nil))
  | Oexp -> ret (Obj.magic coq_Monad_optErr) (Coq_cons (Coq_evm_exp, Coq_nil))
  | Oand -> ret (Obj.magic coq_Monad_optErr) (Coq_cons (Coq_evm_and, Coq_nil))
  | Oor -> ret (Obj.magic coq_Monad_optErr) (Coq_cons (Coq_evm_or, Coq_nil))
  | Oxor -> ret (Obj.magic coq_Monad_optErr) (Coq_cons (Coq_evm_xor, Coq_nil))
  | Oshl ->
    ret (Obj.magic coq_Monad_optErr) (Coq_cons (Coq_evm_mul, (Coq_cons
      (Coq_evm_exp, (Coq_cons ((Coq_evm_push
      (Int256.repr (Zpos (Coq_xO Coq_xH)))), (Coq_cons ((Coq_evm_swap (S O)),
      Coq_nil))))))))
  | Oshr ->
    ret (Obj.magic coq_Monad_optErr) (Coq_cons (Coq_evm_div, (Coq_cons
      ((Coq_evm_swap (S O)), (Coq_cons (Coq_evm_exp, (Coq_cons ((Coq_evm_push
      (Int256.repr (Zpos (Coq_xO Coq_xH)))), (Coq_cons ((Coq_evm_swap (S O)),
      Coq_nil))))))))))
  | Oeq -> ret (Obj.magic coq_Monad_optErr) (Coq_cons (Coq_evm_eq, Coq_nil))
  | One ->
    ret (Obj.magic coq_Monad_optErr) (Coq_cons (Coq_evm_iszero, (Coq_cons
      (Coq_evm_eq, Coq_nil))))
  | Olt ->
    ret (Obj.magic coq_Monad_optErr) (Coq_cons
      ((match signed with
        | Coq_true -> Coq_evm_slt
        | Coq_false -> Coq_evm_lt), Coq_nil))
  | Ogt ->
    ret (Obj.magic coq_Monad_optErr) (Coq_cons
      ((match signed with
        | Coq_true -> Coq_evm_sgt
        | Coq_false -> Coq_evm_gt), Coq_nil))
  | Ole ->
    ret (Obj.magic coq_Monad_optErr) (Coq_cons (Coq_evm_iszero, (Coq_cons
      ((match signed with
        | Coq_true -> Coq_evm_sgt
        | Coq_false -> Coq_evm_gt), Coq_nil))))
  | Oge ->
    ret (Obj.magic coq_Monad_optErr) (Coq_cons (Coq_evm_iszero, (Coq_cons
      ((match signed with
        | Coq_true -> Coq_evm_slt
        | Coq_false -> Coq_evm_lt), Coq_nil))))
  | Osha_2 -> sha_2_compiled

(** val unop_compiled : unary_operation -> compiled **)

let unop_compiled = function
| Onotbool -> command_compiled Coq_evm_iszero
| Onotint -> command_compiled Coq_evm_not
| Oneg ->
  append_compiled Coq_evm_sub (command_compiled (Coq_evm_push Int256.zero))
| Osha_1 -> sha_1_compiled

(** val builtin0_compiled : builtin0 -> evm **)

let builtin0_compiled = function
| Baddress -> Coq_evm_address
| Borigin -> Coq_evm_origin
| Bcaller -> Coq_evm_caller
| Bcallvalue -> Coq_evm_callvalue
| Bcoinbase -> Coq_evm_coinbase
| Btimestamp -> Coq_evm_timestamp
| Bnumber -> Coq_evm_number

(** val builtin1_compiled : builtin1 -> evm **)

let builtin1_compiled = function
| Bbalance -> Coq_evm_balance
| Bblockhash -> Coq_evm_blockhash
