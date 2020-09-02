open Datatypes
open Monad

type __ = Obj.t

(** val coq_Monad_option : __ option coq_Monad **)

let coq_Monad_option =
  { ret = (Obj.magic (fun _ x -> Some x)); bind = (fun _ _ c1 c2 ->
    match c1 with
    | Some v -> c2 v
    | None -> None) }
