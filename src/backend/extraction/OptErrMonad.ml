open Monad
open String0

type __ = Obj.t

type 'a optErr =
| Success of 'a
| Error of string

(** val coq_Monad_optErr : __ optErr coq_Monad **)

let coq_Monad_optErr =
  { ret = (fun _ v -> Success v); bind = (fun _ _ c1 c2 ->
    match c1 with
    | Success v -> c2 v
    | Error msg -> Error msg) }
