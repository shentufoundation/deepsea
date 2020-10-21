open AST
open BinNums
open Datatypes
open Integers
open List0
open Maps0
open Options
open Specif

module IntIndexed =
 struct
  type t = Int.int

  (** val index : Int.int -> positive **)

  let index = function
  | Z0 -> Coq_xH
  | Zpos p -> Coq_xO p
  | Zneg p -> Coq_xI p

  (** val index_inv : positive -> Int.int **)

  let index_inv = function
  | Coq_xI p' -> Int.repr (Zneg p')
  | Coq_xO p' -> Int.repr (Zpos p')
  | Coq_xH -> Int.zero

  (** val eq : Int.int -> Int.int -> sumbool **)

  let eq =
    Int.eq_dec
 end

module IntMap = IMap(IntIndexed)

module Genv =
 struct
  type ('f, 'v) t = { genv_vars : ident list; genv_funcs : ident list;
                      genv_methods : Int.int list; genv_defs : 'v PTree.t;
                      genv_fundefs : 'f PTree.t;
                      genv_methoddefs : 'f option IntMap.t;
                      genv_constructor : 'f option }

  (** val genv_vars : ('a1, 'a2) t -> ident list **)

  let genv_vars x = x.genv_vars

  (** val genv_funcs : ('a1, 'a2) t -> ident list **)

  let genv_funcs x = x.genv_funcs

  (** val genv_methods : ('a1, 'a2) t -> Int.int list **)

  let genv_methods x = x.genv_methods

  (** val genv_defs : ('a1, 'a2) t -> 'a2 PTree.t **)

  let genv_defs x = x.genv_defs

  (** val genv_fundefs : ('a1, 'a2) t -> 'a1 PTree.t **)

  let genv_fundefs x = x.genv_fundefs

  (** val genv_methoddefs : ('a1, 'a2) t -> 'a1 option IntMap.t **)

  let genv_methoddefs x = x.genv_methoddefs

  (** val genv_constructor : ('a1, 'a2) t -> 'a1 option **)

  let genv_constructor x = x.genv_constructor

  (** val empty_genv : ('a1, 'a2) t **)

  let empty_genv =
    { genv_vars = Coq_nil; genv_funcs = Coq_nil; genv_methods = Coq_nil;
      genv_defs = PTree.empty; genv_fundefs = PTree.empty; genv_methoddefs =
      (IntMap.init None); genv_constructor = None }

  (** val add_genv_funcs :
      (ident, 'a1) prod list -> ('a1, 'a2) t -> ('a1, 'a2) t **)

  let rec add_genv_funcs funcs ge =
    match funcs with
    | Coq_nil -> ge
    | Coq_cons (p, rest) ->
      let Coq_pair (id, fundef) = p in
      let r = add_genv_funcs rest ge in
      { genv_vars = r.genv_vars; genv_funcs = (Coq_cons (id, r.genv_funcs));
      genv_methods = r.genv_methods; genv_defs = r.genv_defs; genv_fundefs =
      (PTree.set id fundef r.genv_fundefs); genv_methoddefs =
      r.genv_methoddefs; genv_constructor = r.genv_constructor }

  (** val add_genv_vars :
      (ident, 'a2) prod list -> ('a1, 'a2) t -> ('a1, 'a2) t **)

  let rec add_genv_vars vars ge =
    match vars with
    | Coq_nil -> ge
    | Coq_cons (p, rest) ->
      let Coq_pair (id, def) = p in
      let r = add_genv_vars rest ge in
      { genv_vars = (Coq_cons (id, r.genv_vars)); genv_funcs = r.genv_funcs;
      genv_methods = r.genv_methods; genv_defs =
      (PTree.set id def r.genv_defs); genv_fundefs = r.genv_fundefs;
      genv_methoddefs = r.genv_methoddefs; genv_constructor =
      r.genv_constructor }

  (** val add_genv_methods :
      (Int.int, 'a1) prod list -> ('a1, 'a2) t -> ('a1, 'a2) t **)

  let rec add_genv_methods methods ge =
    match methods with
    | Coq_nil -> ge
    | Coq_cons (p, rest) ->
      let Coq_pair (sig0, fundef) = p in
      let r = add_genv_methods rest ge in
      { genv_vars = r.genv_vars; genv_funcs = r.genv_funcs; genv_methods =
      (Coq_cons (sig0, r.genv_methods)); genv_defs = r.genv_defs;
      genv_fundefs = r.genv_fundefs; genv_methoddefs =
      (IntMap.set sig0 (Some fundef) r.genv_methoddefs); genv_constructor =
      r.genv_constructor }

  (** val add_genv_constructor :
      'a1 option -> ('a1, 'a2) t -> ('a1, 'a2) t **)

  let add_genv_constructor constructor ge =
    { genv_vars = ge.genv_vars; genv_funcs = ge.genv_funcs; genv_methods =
      ge.genv_methods; genv_defs = ge.genv_defs; genv_fundefs =
      ge.genv_fundefs; genv_methoddefs = ge.genv_methoddefs;
      genv_constructor = constructor }

  (** val new_genv :
      (ident, 'a2) prod list -> (ident, 'a1) prod list -> (Int.int, 'a1) prod
      list -> 'a1 option -> ('a1, 'a2) t **)

  let new_genv vars funcs methods constructor =
    add_genv_constructor constructor
      (add_genv_methods methods
        (add_genv_funcs funcs (add_genv_vars vars empty_genv)))

  (** val all_functions : ('a1, 'a2) t -> 'a1 list **)

  let all_functions ge =
    app (map snd (PTree.elements ge.genv_fundefs))
      (flat_map optional_filter
        (map snd (PTree.elements (snd ge.genv_methoddefs))))
 end
