open AST
open BinNums
open Datatypes
open Integers
open List0
open Maps0
open Options
open Specif

module IntIndexed :
 sig
  type t = Int.int

  val index : Int.int -> positive

  val index_inv : positive -> Int.int

  val eq : Int.int -> Int.int -> sumbool
 end

module IntMap :
 sig
  type elt = IntIndexed.t

  val elt_eq : IntIndexed.t -> IntIndexed.t -> sumbool

  type 'x t = 'x PMap.t

  val init : 'a1 -> ('a1, 'a1 PTree.t) prod

  val get : IntIndexed.t -> 'a1 t -> 'a1

  val set : IntIndexed.t -> 'a1 -> 'a1 t -> ('a1, 'a1 PTree.t) prod

  val map : ('a1 -> 'a2) -> 'a1 t -> 'a2 t

  val union : 'a1 t -> 'a1 t -> 'a1 t
 end

module Genv :
 sig
  type ('f, 'v) t = { genv_vars : ident list; genv_funcs : ident list;
                      genv_methods : Int.int list; genv_defs : 'v PTree.t;
                      genv_fundefs : 'f PTree.t;
                      genv_methoddefs : 'f option IntMap.t;
                      genv_constructor : 'f option }

  val genv_vars : ('a1, 'a2) t -> ident list

  val genv_funcs : ('a1, 'a2) t -> ident list

  val genv_methods : ('a1, 'a2) t -> Int.int list

  val genv_defs : ('a1, 'a2) t -> 'a2 PTree.t

  val genv_fundefs : ('a1, 'a2) t -> 'a1 PTree.t

  val genv_methoddefs : ('a1, 'a2) t -> 'a1 option IntMap.t

  val genv_constructor : ('a1, 'a2) t -> 'a1 option

  val empty_genv : ('a1, 'a2) t

  val add_genv_funcs : (ident, 'a1) prod list -> ('a1, 'a2) t -> ('a1, 'a2) t

  val add_genv_vars : (ident, 'a2) prod list -> ('a1, 'a2) t -> ('a1, 'a2) t

  val add_genv_methods :
    (Int.int, 'a1) prod list -> ('a1, 'a2) t -> ('a1, 'a2) t

  val add_genv_constructor : 'a1 option -> ('a1, 'a2) t -> ('a1, 'a2) t

  val new_genv :
    (ident, 'a2) prod list -> (ident, 'a1) prod list -> (Int.int, 'a1) prod
    list -> 'a1 option -> ('a1, 'a2) t

  val all_functions : ('a1, 'a2) t -> 'a1 list
 end
