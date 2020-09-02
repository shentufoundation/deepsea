open AST
open Ascii
open BinNums
open BinPos
open Datatypes
open IndexLib
open Integers
open Maps0
open Monad
open OptErrMonad
open OptionMonad
open Options
open Specif
open String0

type label_map = { lm_map : label PTree.t; lm_max : label }

(** val lm_map : label_map -> label PTree.t **)

let lm_map x = x.lm_map

(** val lm_max : label_map -> label **)

let lm_max x = x.lm_max

(** val allocate_label : positive -> label_map -> label_map **)

let allocate_label n base =
  let max = base.lm_max in
  let new_label = Pos.succ max in
  let m = base.lm_map in
  { lm_map = (PTree.set n new_label m); lm_max = new_label }

(** val xallocate_labels : label list -> label_map -> label_map **)

let rec xallocate_labels elts base =
  match elts with
  | Coq_nil -> base
  | Coq_cons (n, rest) ->
    let allocate_rest = xallocate_labels rest base in
    allocate_label n allocate_rest

(** val empty_label_map : label_map **)

let empty_label_map =
  { lm_map = PTree.empty; lm_max = Coq_xH }

type fd_label =
| Lfun of label
| Lconstructor
| Lmethod of Int.int
| Lmultiplexer of Int.int
| Lbody

type node_label =
| Lfun_node of label * label * positive
| Lconstructor_node of label * positive
| Lmethod_node of Int.int * label * positive

type label_type = (fd_label, node_label) sum

(** val node_within' : fd_label -> label -> positive -> label_type **)

let node_within' lt n p =
  Coq_inr
    (match lt with
     | Lfun f -> Lfun_node (f, n, p)
     | Lmethod i -> Lmethod_node (i, n, p)
     | Lmultiplexer i -> Lmethod_node (i, n, p)
     | _ -> Lconstructor_node (n, p))

(** val node_within : fd_label -> label -> label_type **)

let node_within lt n =
  node_within' lt n Coq_xH

(** val call_node_within : fd_label -> label -> label_type **)

let call_node_within lt n =
  node_within' lt n (Coq_xO Coq_xH)

(** val key : label_type -> positive **)

let key = function
| Coq_inl f0 ->
  (match f0 with
   | Lfun f ->
     inject_positive (inject_positive Coq_xH Coq_xH)
       (inject_positive Coq_xH f)
   | Lconstructor ->
     inject_positive (inject_positive Coq_xH Coq_xH)
       (inject_positive (Coq_xI Coq_xH) Coq_xH)
   | Lmethod i ->
     inject_positive (inject_positive Coq_xH Coq_xH)
       (inject_positive (Coq_xI (Coq_xO Coq_xH)) (int32_index i))
   | Lmultiplexer i ->
     inject_positive (inject_positive Coq_xH Coq_xH)
       (inject_positive (Coq_xI (Coq_xI Coq_xH)) (int32_index i))
   | Lbody ->
     inject_positive (inject_positive Coq_xH Coq_xH)
       (inject_positive (Coq_xO (Coq_xO (Coq_xO Coq_xH))) Coq_xH))
| Coq_inr n0 ->
  (match n0 with
   | Lfun_node (f, n, p) ->
     inject_positive (inject_positive n p) (inject_positive (Coq_xO Coq_xH) f)
   | Lconstructor_node (n, p) ->
     inject_positive (inject_positive n p)
       (inject_positive (Coq_xO (Coq_xO Coq_xH)) Coq_xH)
   | Lmethod_node (i, n, p) ->
     inject_positive (inject_positive n p)
       (inject_positive (Coq_xO (Coq_xI Coq_xH)) (int32_index i)))

(** val label_key : label_map -> label_type -> label optErr **)

let label_key lm lt =
  fromOption (PTree.get (key lt) lm.lm_map) (String ((Ascii (Coq_false,
    Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false,
    Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
    Coq_true, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
    Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
    Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
    Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_false,
    Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
    (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
    Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
    Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
    ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
    Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
    Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
    (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
    Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
    Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
    (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_false,
    Coq_true, Coq_true, Coq_false)), EmptyString))))))))))))))))))))))))))))))

(** val elt_key : fd_label -> label -> positive **)

let elt_key fn x =
  key (node_within fn x)

(** val call_elt_key : fd_label -> label -> positive **)

let call_elt_key fn x =
  key (call_node_within fn x)

(** val labels_no_dups : label list -> coq_unit PTree.t option **)

let rec labels_no_dups = function
| Coq_nil -> Some PTree.empty
| Coq_cons (lbl, rest) ->
  bind (Obj.magic coq_Monad_option) (labels_no_dups rest)
    (fun rest_no_dups ->
    match PTree.get lbl rest_no_dups with
    | Some _ -> None
    | None -> Some (PTree.set lbl Coq_tt rest_no_dups))

(** val label_norepet_dec : label list -> sumbool **)

let label_norepet_dec labels =
  let tree = labels_no_dups labels in
  (match tree with
   | Some _ -> Coq_left
   | None -> Coq_right)

(** val decide_label_norepet : label list -> bool **)

let decide_label_norepet labels =
  match label_norepet_dec labels with
  | Coq_left -> Coq_true
  | Coq_right -> Coq_false
