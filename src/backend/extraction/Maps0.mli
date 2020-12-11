open BinNums
open Coqlib
open Datatypes
open List0
open Specif

module PTree :
 sig
  type elt = positive

  type 'a tree =
  | Leaf
  | Node of 'a tree * 'a option * 'a tree

  type 'a t = 'a tree

  val empty : 'a1 t

  val get : positive -> 'a1 t -> 'a1 option

  val set : positive -> 'a1 -> 'a1 t -> 'a1 t

  val remove : positive -> 'a1 t -> 'a1 t

  val bempty : 'a1 t -> bool

  val beq : ('a1 -> 'a1 -> bool) -> 'a1 t -> 'a1 t -> bool

  val prev_append : positive -> positive -> positive

  val prev : positive -> positive

  val xmap : (positive -> 'a1 -> 'a2) -> 'a1 t -> positive -> 'a2 t

  val map : (positive -> 'a1 -> 'a2) -> 'a1 t -> 'a2 t

  val map1 : ('a1 -> 'a2) -> 'a1 t -> 'a2 t

  val xelements :
    'a1 t -> positive -> (positive, 'a1) prod list -> (positive, 'a1) prod
    list

  val elements : 'a1 t -> (positive, 'a1) prod list

  val xkeys : 'a1 t -> positive -> positive list

  val xfold :
    ('a2 -> positive -> 'a1 -> 'a2) -> positive -> 'a1 t -> 'a2 -> 'a2

  val fold : ('a2 -> positive -> 'a1 -> 'a2) -> 'a1 t -> 'a2 -> 'a2

  val fold1 : ('a2 -> 'a1 -> 'a2) -> 'a1 t -> 'a2 -> 'a2

  val union : 'a1 t -> 'a1 t -> 'a1 t

  val get_default : 'a1 -> elt -> 'a1 t -> 'a1
 end

module PMap :
 sig
  type 'a t = ('a, 'a PTree.t) prod

  val init : 'a1 -> ('a1, 'a1 PTree.t) prod

  val get : positive -> 'a1 t -> 'a1

  val set : positive -> 'a1 -> 'a1 t -> ('a1, 'a1 PTree.t) prod

  val map : ('a1 -> 'a2) -> 'a1 t -> 'a2 t
 end

module type INDEXED_TYPE =
 sig
  type t

  val index : t -> positive

  val index_inv : positive -> t

  val eq : t -> t -> sumbool
 end

module IMap :
 functor (X:INDEXED_TYPE) ->
 sig
  type elt = X.t

  val elt_eq : X.t -> X.t -> sumbool

  type 'x t = 'x PMap.t

  val init : 'a1 -> ('a1, 'a1 PTree.t) prod

  val get : X.t -> 'a1 t -> 'a1

  val set : X.t -> 'a1 -> 'a1 t -> ('a1, 'a1 PTree.t) prod

  val map : ('a1 -> 'a2) -> 'a1 t -> 'a2 t

  val union : 'a1 t -> 'a1 t -> 'a1 t
 end
