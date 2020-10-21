open AST
open Datatypes
open Globalenvs
open Labels
open List0
open Maps0
open Specif
open StmtStacked

(** val label_code : code -> label list **)

let rec label_code = function
| Coq_nil -> Coq_nil
| Coq_cons (s, rest) ->
  let lr = label_code rest in
  (match s with
   | Slabel l -> Coq_cons (l, lr)
   | _ -> lr)

(** val label_function : coq_function -> bool **)

let label_function f =
  match label_norepet_dec (label_code (fn_code f)) with
  | Coq_left -> Coq_true
  | Coq_right -> Coq_false

(** val xlabel_functions : coq_function option list -> bool **)

let rec xlabel_functions = function
| Coq_nil -> Coq_true
| Coq_cons (o, rest) ->
  (match o with
   | Some f ->
     (match label_function f with
      | Coq_true -> xlabel_functions rest
      | Coq_false -> Coq_false)
   | None -> xlabel_functions rest)

(** val label_functions : coq_function PTree.t -> bool **)

let label_functions fns =
  xlabel_functions (map (fun x -> Some x) (map snd (PTree.elements fns)))

(** val label_methods : coq_function option IntMap.t -> bool **)

let label_methods fns =
  xlabel_functions (Coq_cons ((fst fns),
    (map snd (PTree.elements (snd fns)))))
