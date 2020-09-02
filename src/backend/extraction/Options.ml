open Datatypes
open OptErrMonad
open String0

(** val fromOption : 'a1 option -> string -> 'a1 optErr **)

let fromOption ov msg =
  match ov with
  | Some v -> Success v
  | None -> Error msg

(** val optional_filter : 'a1 option -> 'a1 list **)

let optional_filter = function
| Some a' -> Coq_cons (a', Coq_nil)
| None -> Coq_nil
