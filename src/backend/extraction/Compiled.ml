open Datatypes
open EVM
open Monad
open OptErrMonad
open String0

type compiled = evm list optErr

(** val command_compiled : evm -> compiled **)

let command_compiled cmd =
  ret (Obj.magic coq_Monad_optErr) (Coq_cons (cmd, Coq_nil))

(** val error_compiled : string -> compiled **)

let error_compiled msg =
  Error msg

(** val empty_compiled : compiled **)

let empty_compiled =
  ret (Obj.magic coq_Monad_optErr) Coq_nil

(** val append_compiled : evm -> compiled -> compiled **)

let append_compiled cmd rest =
  bind (Obj.magic coq_Monad_optErr) rest (fun code ->
    ret (Obj.magic coq_Monad_optErr) (Coq_cons (cmd, code)))

(** val concatenate_compiled : compiled -> compiled -> compiled **)

let concatenate_compiled chunk1 chunk2 =
  bind (Obj.magic coq_Monad_optErr) chunk1 (fun code1 ->
    bind (Obj.magic coq_Monad_optErr) chunk2 (fun code2 ->
      ret (Obj.magic coq_Monad_optErr) (app code2 code1)))
