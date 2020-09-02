open Datatypes

type __ = Obj.t
let __ = let rec f _ = Obj.repr f in Obj.repr f

type 'm coq_Monad = { ret : (__ -> __ -> 'm);
                      bind : (__ -> __ -> 'm -> (__ -> 'm) -> 'm) }

(** val ret : 'a1 coq_Monad -> 'a2 -> 'a1 **)

let ret monad x =
  let { ret = ret0; bind = _ } = monad in Obj.magic ret0 __ x

(** val bind : 'a1 coq_Monad -> 'a1 -> ('a2 -> 'a1) -> 'a1 **)

let bind monad x x0 =
  let { ret = _; bind = bind0 } = monad in Obj.magic bind0 __ __ x x0

(** val bind2 : 'a1 coq_Monad -> 'a1 -> ('a2 -> 'a3 -> 'a1) -> 'a1 **)

let bind2 m ma f =
  bind m ma (fun a -> let Coq_pair (x, y) = a in f x y)
