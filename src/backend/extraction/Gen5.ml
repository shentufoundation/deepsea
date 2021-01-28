open AST
open Ascii
open Datatypes
open Globalenvs
open Integers
open Labels
open List0
open Maps0
open Monad
open OptErrMonad
open Options
open Semantics0
open StmtClinear
open String0
open Trees

(** val nodes_in_code : code -> label list **)

let rec nodes_in_code = function
| Coq_nil -> Coq_nil
| Coq_cons (s, rest) ->
  let more_nodes = nodes_in_code rest in
  (match s with
   | Slabel lbl -> Coq_cons (lbl, more_nodes)
   | _ -> more_nodes)

(** val allocate_labels : fd_label -> code -> label_map -> label_map **)

let allocate_labels fn c base =
  let nodes = nodes_in_code c in
  xallocate_labels (map (elt_key fn) nodes)
    (xallocate_labels (map (call_elt_key fn) nodes) base)

(** val allocate_labels_fundef :
    fd_label -> coq_function -> label_map -> label_map **)

let allocate_labels_fundef fn_l f base =
  allocate_label (key (Coq_inl fn_l)) (allocate_labels fn_l f.fn_code base)

(** val xallocate_labels_fun :
    (label, coq_function) prod list -> label_map -> label_map **)

let rec xallocate_labels_fun elts base =
  match elts with
  | Coq_nil -> base
  | Coq_cons (p, rest) ->
    let Coq_pair (fn, fd) = p in
    let rest_allocated = xallocate_labels_fun rest base in
    allocate_labels_fundef (Lfun fn) fd rest_allocated

(** val allocate_labels_fun :
    coq_function PTree.t -> label_map -> label_map **)

let allocate_labels_fun defs base =
  xallocate_labels_fun (PTree.elements defs) base

(** val allocate_labels_methods :
    Int.int list -> coq_function option IntMap.t -> label_map -> label_map **)

let rec allocate_labels_methods sigs defs base =
  match sigs with
  | Coq_nil -> base
  | Coq_cons (sig0, rest) ->
    let rest_allocated = allocate_labels_methods rest defs base in
    (match IntMap.get sig0 defs with
     | Some fd ->
       allocate_label (key (Coq_inl (Lmultiplexer sig0)))
         (allocate_labels_fundef (Lmethod sig0) fd rest_allocated)
     | None -> rest_allocated)

(** val allocate_labels_constructor :
    coq_function option -> label_map -> label_map **)

let allocate_labels_constructor optc base =
  match optc with
  | Some fd -> allocate_labels_fundef Lconstructor fd base
  | None -> base

(** val allocate_labels_ge : genv -> label_map **)

let allocate_labels_ge ge =
  let sigs = ge.Genv.genv_methods in
  let fundefs = ge.Genv.genv_fundefs in
  let methoddefs = ge.Genv.genv_methoddefs in
  let constructor = ge.Genv.genv_constructor in
  allocate_label (key (Coq_inl Lbody))
    (allocate_labels_constructor constructor
      (allocate_labels_methods sigs methoddefs
        (allocate_labels_fun fundefs empty_label_map)))

(** val clabeled_stm :
    fd_label -> label_map -> statement -> statement optErr **)

let clabeled_stm fn lm s = match s with
| Scall (rv, dst, args, retlbl) ->
  bind (Obj.magic coq_Monad_optErr)
    (Obj.magic label_key lm (Coq_inl (Lfun dst))) (fun dst' ->
    bind (Obj.magic coq_Monad_optErr)
      (Obj.magic label_key lm (call_node_within fn retlbl)) (fun retlbl' ->
      ret (Obj.magic coq_Monad_optErr) (Scall (rv, dst', args, retlbl'))))
| Slabel lbl ->
  bind (Obj.magic coq_Monad_optErr)
    (Obj.magic label_key lm (node_within fn lbl)) (fun l ->
    ret (Obj.magic coq_Monad_optErr) (Slabel l))
| Sjump lbl ->
  bind (Obj.magic coq_Monad_optErr)
    (Obj.magic label_key lm (node_within fn lbl)) (fun lbl' ->
    ret (Obj.magic coq_Monad_optErr) (Sjump lbl'))
| Sjumpi (e, lbl) ->
  bind (Obj.magic coq_Monad_optErr)
    (Obj.magic label_key lm (node_within fn lbl)) (fun lbl' ->
    ret (Obj.magic coq_Monad_optErr) (Sjumpi (e, lbl')))
| Stransfer (a, v, fail) ->
  bind (Obj.magic coq_Monad_optErr)
    (Obj.magic label_key lm (node_within fn fail)) (fun f ->
    ret (Obj.magic coq_Monad_optErr) (Stransfer (a, v, f)))
| Scallmethod (a, r, s0, v, g, args, fail) ->
  bind (Obj.magic coq_Monad_optErr)
    (Obj.magic label_key lm (node_within fn fail)) (fun f ->
    ret (Obj.magic coq_Monad_optErr) (Scallmethod (a, r, s0, v, g, args, f)))
| _ -> ret (Obj.magic coq_Monad_optErr) s

(** val clabeled_code : fd_label -> label_map -> code -> code optErr **)

let clabeled_code fn lm c =
  map_error (clabeled_stm fn lm) c

(** val fn_is_method : fd_label -> bool **)

let fn_is_method = function
| Lfun _ -> Coq_false
| _ -> Coq_true

(** val clabeled_function_body : label -> fd_label -> code -> code **)

let clabeled_function_body l fn c =
  Coq_cons ((Slabel l), (Coq_cons ((Sfetchargs (fn_is_method fn)), (Coq_cons
    (Sintro, c)))))

(** val clabeled_function :
    label_map -> fd_label -> coq_function -> coq_function optErr **)

let clabeled_function lm fn f =
  bind (Obj.magic coq_Monad_optErr) (Obj.magic clabeled_code fn lm f.fn_code)
    (fun c ->
    bind (Obj.magic coq_Monad_optErr) (Obj.magic label_key lm (Coq_inl fn))
      (fun l ->
      let body = clabeled_function_body l fn c in
      ret (Obj.magic coq_Monad_optErr) { fn_return = f.fn_return; fn_params =
        f.fn_params; fn_temps = f.fn_temps; fn_locals = f.fn_locals;
        fn_code = body }))

(** val clabeled_fundef :
    label_map -> fd_label -> coq_function -> coq_function optErr **)

let clabeled_fundef =
  clabeled_function

(** val clabeled_fundef_f :
    label_map -> label -> coq_function -> (label, coq_function) prod optErr **)

let clabeled_fundef_f lm name fd =
  bind (Obj.magic coq_Monad_optErr)
    (Obj.magic clabeled_fundef lm (Lfun name) fd) (fun f ->
    bind (Obj.magic coq_Monad_optErr)
      (Obj.magic label_key lm (Coq_inl (Lfun name))) (fun l ->
      ret (Obj.magic coq_Monad_optErr) (Coq_pair (l, f))))

(** val clabeled_fundefs :
    label_map -> coq_function PTree.t -> coq_function PTree.t optErr **)

let clabeled_fundefs lm t0 =
  transl_tree_keys_move (clabeled_fundef_f lm) t0

(** val xclabeled_methods :
    label_map -> Int.int list -> coq_function option IntMap.t -> coq_function
    option IntMap.t optErr **)

let rec xclabeled_methods lm sigs methods =
  match sigs with
  | Coq_nil -> ret (Obj.magic coq_Monad_optErr) (IntMap.init None)
  | Coq_cons (sig0, rest) ->
    bind (Obj.magic coq_Monad_optErr) (xclabeled_methods lm rest methods)
      (fun crest ->
      bind (Obj.magic coq_Monad_optErr)
        (fromOption (IntMap.get sig0 (Obj.magic methods)) (String ((Ascii
          (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
          Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
          Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
          (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true,
          Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
          (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
          Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
          Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
          (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false,
          Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
          (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
          Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
          Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
          (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true,
          Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
          (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
          Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
          Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
          (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
          Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
          (Coq_true, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
          Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
          Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
          (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false,
          Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
          (Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
          Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
          Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
          (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false,
          Coq_false, Coq_true, Coq_true, Coq_false)),
          EmptyString))))))))))))))))))))))))))))))))))))) (fun meth ->
        bind (Obj.magic coq_Monad_optErr)
          (Obj.magic clabeled_fundef lm (Lmethod sig0) meth) (fun b ->
          ret (Obj.magic coq_Monad_optErr) (IntMap.set sig0 (Some b) crest))))

(** val clabeled_methods :
    label_map -> Int.int list -> coq_function option IntMap.t -> coq_function
    option IntMap.t optErr **)

let clabeled_methods =
  xclabeled_methods

(** val clabeled_constructor :
    label_map -> coq_function option -> coq_function optErr **)

let clabeled_constructor lm = function
| Some c ->
  bind (Obj.magic coq_Monad_optErr) (clabeled_fundef lm Lconstructor c)
    (fun f -> ret (Obj.magic coq_Monad_optErr) f)
| None ->
  Error (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false,
    Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
    Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
    ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
    Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
    Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
    (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false,
    Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
    Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
    Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
    Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true,
    Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
    Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
    Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true,
    Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
    Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
    Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
    Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true,
    Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
    Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
    Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
    Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
    Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
    Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
    Coq_false)), EmptyString))))))))))))))))))))))))))))))))))))))))))

(** val clabeled_genv : label_map -> genv -> genv optErr **)

let clabeled_genv lm ge =
  let vars = ge.Genv.genv_vars in
  let funcs = ge.Genv.genv_funcs in
  let methods = ge.Genv.genv_methods in
  let defs = ge.Genv.genv_defs in
  let fundefs = ge.Genv.genv_fundefs in
  let methoddefs = ge.Genv.genv_methoddefs in
  let constructor = ge.Genv.genv_constructor in
  (match label_functions fundefs with
   | Coq_true ->
     (match label_methods methoddefs with
      | Coq_true ->
        bind (Obj.magic coq_Monad_optErr)
          (Obj.magic clabeled_fundefs lm fundefs) (fun fundefs0 ->
          bind (Obj.magic coq_Monad_optErr)
            (Obj.magic clabeled_methods lm methods methoddefs)
            (fun methoddefs0 ->
            bind (Obj.magic coq_Monad_optErr)
              (Obj.magic clabeled_constructor lm constructor)
              (fun constructor0 ->
              ret (Obj.magic coq_Monad_optErr) { Genv.genv_vars = vars;
                Genv.genv_funcs = funcs; Genv.genv_methods = methods;
                Genv.genv_defs = defs; Genv.genv_fundefs = fundefs0;
                Genv.genv_methoddefs = methoddefs0; Genv.genv_constructor =
                (Some constructor0) })))
      | Coq_false ->
        Error (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true,
          Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
          (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
          Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
          Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
          (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
          Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
          (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
          Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
          Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
          (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_false,
          Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
          (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
          Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
          Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
          (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true,
          Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
          (Coq_false, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
          Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
          Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
          (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false,
          Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
          (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
          Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
          Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
          (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true,
          Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
          (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
          Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
          Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
          (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false,
          Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
          (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
          Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
          Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
          (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
          Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
          (Coq_false, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
          Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
          Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
          (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true,
          Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
          (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
          Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
          Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
          (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true,
          Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
          (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
          Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
          Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
          (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
          Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
          (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
          Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
          Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_false)),
          (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_false,
          Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
          (Coq_true, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
          Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
          Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
          (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
          Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
          (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
          Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
          Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
          (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
          Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
          Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
          Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
          Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
          EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
   | Coq_false ->
     Error (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true,
       Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
       (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
       Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
       Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
       (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_true,
       Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
       Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
       Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
       Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
       ((Ascii (Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
       Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
       Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
       (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false,
       Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
       Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
       (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_false,
       Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
       Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
       (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
       Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
       Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
       (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
       Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
       Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
       (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
       Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
       (Coq_false, Coq_true, Coq_true, Coq_false, Coq_false, Coq_true,
       Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
       Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
       (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
       Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
       Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
       Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
       Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
       (Coq_false, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
       Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
       Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
       (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
       Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
       Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
       (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
       Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
       (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
       Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
       Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
       (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false, Coq_false,
       Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
       Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
       (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false,
       Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
       Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_false)),
       (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_false, Coq_false,
       Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
       Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
       (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
       Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
       Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
       (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
       Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
       Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
       (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
       Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
       Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
       (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true,
       Coq_true, Coq_true, Coq_false)),
       EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

(** val clabeled_program : genv -> program optErr **)

let clabeled_program ge =
  let lm = allocate_labels_ge ge in
  bind (Obj.magic coq_Monad_optErr) (Obj.magic clabeled_genv lm ge)
    (fun cge ->
    bind (Obj.magic coq_Monad_optErr)
      (Obj.magic label_key lm (Coq_inl Lbody)) (fun body ->
      ret (Obj.magic coq_Monad_optErr) (Coq_pair (cge, body))))
