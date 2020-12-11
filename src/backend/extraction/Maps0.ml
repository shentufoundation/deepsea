open BinNums
open Coqlib
open Datatypes
open List0
open Specif

module PTree =
 struct
  type elt = positive

  type 'a tree =
  | Leaf
  | Node of 'a tree * 'a option * 'a tree

  type 'a t = 'a tree

  (** val empty : 'a1 t **)

  let empty =
    Leaf

  (** val get : positive -> 'a1 t -> 'a1 option **)

  let rec get i = function
  | Leaf -> None
  | Node (l, o, r) ->
    (match i with
     | Coq_xI ii -> get ii r
     | Coq_xO ii -> get ii l
     | Coq_xH -> o)

  (** val set : positive -> 'a1 -> 'a1 t -> 'a1 t **)

  let rec set i v = function
  | Leaf ->
    (match i with
     | Coq_xI ii -> Node (Leaf, None, (set ii v Leaf))
     | Coq_xO ii -> Node ((set ii v Leaf), None, Leaf)
     | Coq_xH -> Node (Leaf, (Some v), Leaf))
  | Node (l, o, r) ->
    (match i with
     | Coq_xI ii -> Node (l, o, (set ii v r))
     | Coq_xO ii -> Node ((set ii v l), o, r)
     | Coq_xH -> Node (l, (Some v), r))

  (** val remove : positive -> 'a1 t -> 'a1 t **)

  let rec remove i m =
    match i with
    | Coq_xI ii ->
      (match m with
       | Leaf -> Leaf
       | Node (l, o, r) ->
         (match l with
          | Leaf ->
            (match o with
             | Some _ -> Node (l, o, (remove ii r))
             | None ->
               (match remove ii r with
                | Leaf -> Leaf
                | Node (t0, o0, t1) -> Node (Leaf, None, (Node (t0, o0, t1)))))
          | Node (_, _, _) -> Node (l, o, (remove ii r))))
    | Coq_xO ii ->
      (match m with
       | Leaf -> Leaf
       | Node (l, o, r) ->
         (match o with
          | Some _ -> Node ((remove ii l), o, r)
          | None ->
            (match r with
             | Leaf ->
               (match remove ii l with
                | Leaf -> Leaf
                | Node (t0, o0, t1) -> Node ((Node (t0, o0, t1)), None, Leaf))
             | Node (_, _, _) -> Node ((remove ii l), o, r))))
    | Coq_xH ->
      (match m with
       | Leaf -> Leaf
       | Node (l, _, r) ->
         (match l with
          | Leaf ->
            (match r with
             | Leaf -> Leaf
             | Node (_, _, _) -> Node (l, None, r))
          | Node (_, _, _) -> Node (l, None, r)))

  (** val bempty : 'a1 t -> bool **)

  let rec bempty = function
  | Leaf -> Coq_true
  | Node (l, o, r) ->
    (match o with
     | Some _ -> Coq_false
     | None ->
       (match bempty l with
        | Coq_true -> bempty r
        | Coq_false -> Coq_false))

  (** val beq : ('a1 -> 'a1 -> bool) -> 'a1 t -> 'a1 t -> bool **)

  let rec beq beqA m1 m2 =
    match m1 with
    | Leaf -> bempty m2
    | Node (l1, o1, r1) ->
      (match m2 with
       | Leaf -> bempty m1
       | Node (l2, o2, r2) ->
         (match match match o1 with
                      | Some y1 ->
                        (match o2 with
                         | Some y2 -> beqA y1 y2
                         | None -> Coq_false)
                      | None ->
                        (match o2 with
                         | Some _ -> Coq_false
                         | None -> Coq_true) with
                | Coq_true -> beq beqA l1 l2
                | Coq_false -> Coq_false with
          | Coq_true -> beq beqA r1 r2
          | Coq_false -> Coq_false))

  (** val prev_append : positive -> positive -> positive **)

  let rec prev_append i j =
    match i with
    | Coq_xI i' -> prev_append i' (Coq_xI j)
    | Coq_xO i' -> prev_append i' (Coq_xO j)
    | Coq_xH -> j

  (** val prev : positive -> positive **)

  let prev i =
    prev_append i Coq_xH

  (** val xmap : (positive -> 'a1 -> 'a2) -> 'a1 t -> positive -> 'a2 t **)

  let rec xmap f m i =
    match m with
    | Leaf -> Leaf
    | Node (l, o, r) ->
      Node ((xmap f l (Coq_xO i)),
        (match o with
         | Some x -> Some (f (prev i) x)
         | None -> None), (xmap f r (Coq_xI i)))

  (** val map : (positive -> 'a1 -> 'a2) -> 'a1 t -> 'a2 t **)

  let map f m =
    xmap f m Coq_xH

  (** val map1 : ('a1 -> 'a2) -> 'a1 t -> 'a2 t **)

  let rec map1 f = function
  | Leaf -> Leaf
  | Node (l, o, r) -> Node ((map1 f l), (option_map f o), (map1 f r))

  (** val xelements :
      'a1 t -> positive -> (positive, 'a1) prod list -> (positive, 'a1) prod
      list **)

  let rec xelements m i k =
    match m with
    | Leaf -> k
    | Node (l, o, r) ->
      (match o with
       | Some x ->
         xelements l (Coq_xO i) (Coq_cons ((Coq_pair ((prev i), x)),
           (xelements r (Coq_xI i) k)))
       | None -> xelements l (Coq_xO i) (xelements r (Coq_xI i) k))

  (** val elements : 'a1 t -> (positive, 'a1) prod list **)

  let elements m =
    xelements m Coq_xH Coq_nil

  (** val xkeys : 'a1 t -> positive -> positive list **)

  let xkeys m i =
    List0.map fst (xelements m i Coq_nil)

  (** val xfold :
      ('a2 -> positive -> 'a1 -> 'a2) -> positive -> 'a1 t -> 'a2 -> 'a2 **)

  let rec xfold f i m v =
    match m with
    | Leaf -> v
    | Node (l, o, r) ->
      (match o with
       | Some x ->
         let v1 = xfold f (Coq_xO i) l v in
         let v2 = f v1 (prev i) x in xfold f (Coq_xI i) r v2
       | None -> let v1 = xfold f (Coq_xO i) l v in xfold f (Coq_xI i) r v1)

  (** val fold : ('a2 -> positive -> 'a1 -> 'a2) -> 'a1 t -> 'a2 -> 'a2 **)

  let fold f m v =
    xfold f Coq_xH m v

  (** val fold1 : ('a2 -> 'a1 -> 'a2) -> 'a1 t -> 'a2 -> 'a2 **)

  let rec fold1 f m v =
    match m with
    | Leaf -> v
    | Node (l, o, r) ->
      (match o with
       | Some x -> let v1 = fold1 f l v in let v2 = f v1 x in fold1 f r v2
       | None -> let v1 = fold1 f l v in fold1 f r v1)

  (** val union : 'a1 t -> 'a1 t -> 'a1 t **)

  let union m1 m2 =
    fold (fun m k v -> set k v m) m1 m2

  (** val get_default : 'a1 -> elt -> 'a1 t -> 'a1 **)

  let get_default default k m =
    match get k m with
    | Some v -> v
    | None -> default
 end

module PMap =
 struct
  type 'a t = ('a, 'a PTree.t) prod

  (** val init : 'a1 -> ('a1, 'a1 PTree.t) prod **)

  let init x =
    Coq_pair (x, PTree.empty)

  (** val get : positive -> 'a1 t -> 'a1 **)

  let get i m =
    match PTree.get i (snd m) with
    | Some x -> x
    | None -> fst m

  (** val set : positive -> 'a1 -> 'a1 t -> ('a1, 'a1 PTree.t) prod **)

  let set i x m =
    Coq_pair ((fst m), (PTree.set i x (snd m)))

  (** val map : ('a1 -> 'a2) -> 'a1 t -> 'a2 t **)

  let map f m =
    Coq_pair ((f (fst m)), (PTree.map1 f (snd m)))
 end

module type INDEXED_TYPE =
 sig
  type t

  val index : t -> positive

  val index_inv : positive -> t

  val eq : t -> t -> sumbool
 end

module IMap =
 functor (X:INDEXED_TYPE) ->
 struct
  type elt = X.t

  (** val elt_eq : X.t -> X.t -> sumbool **)

  let elt_eq =
    X.eq

  type 'x t = 'x PMap.t

  (** val init : 'a1 -> ('a1, 'a1 PTree.t) prod **)

  let init =
    PMap.init

  (** val get : X.t -> 'a1 t -> 'a1 **)

  let get i m =
    PMap.get (X.index i) m

  (** val set : X.t -> 'a1 -> 'a1 t -> ('a1, 'a1 PTree.t) prod **)

  let set i v m =
    PMap.set (X.index i) v m

  (** val map : ('a1 -> 'a2) -> 'a1 t -> 'a2 t **)

  let map =
    PMap.map

  (** val union : 'a1 t -> 'a1 t -> 'a1 t **)

  let union m1 m2 =
    Coq_pair ((fst m1), (PTree.union (snd m1) (snd m2)))
 end
