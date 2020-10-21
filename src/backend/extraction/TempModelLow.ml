open BinNums
open Ctypes
open Datatypes
open List0
open Specif

type __ = Obj.t

type typed_idents = (positive, coq_type) prod list

(** val my_peq : positive -> positive -> sumbool **)

let rec my_peq p x0 =
  match p with
  | Coq_xI p0 -> (match x0 with
                  | Coq_xI p1 -> my_peq p0 p1
                  | _ -> Coq_right)
  | Coq_xO p0 -> (match x0 with
                  | Coq_xO p1 -> my_peq p0 p1
                  | _ -> Coq_right)
  | Coq_xH -> (match x0 with
               | Coq_xH -> Coq_left
               | _ -> Coq_right)

(** val unique_temps_excl : typed_idents -> typed_idents -> typed_idents **)

let rec unique_temps_excl t excl =
  match t with
  | Coq_nil -> Coq_nil
  | Coq_cons (p, rest) ->
    let Coq_pair (tmp, ty) = p in
    let utemps = unique_temps_excl rest excl in
    (match in_dec my_peq tmp (map fst utemps) with
     | Coq_left -> utemps
     | Coq_right ->
       (match in_dec my_peq tmp (map fst excl) with
        | Coq_left -> utemps
        | Coq_right -> Coq_cons ((Coq_pair (tmp, ty)), utemps)))

(** val unique_temps : typed_idents -> typed_idents **)

let unique_temps t =
  unique_temps_excl t Coq_nil

(** val unique_all_temps : typed_idents -> typed_idents -> typed_idents **)

let unique_all_temps t t' =
  app (unique_temps_excl t t') (unique_temps t')

type ftemps = { f_temps : (__ -> typed_idents); f_args : (__ -> typed_idents) }

type f_t = __

(** val f_temps : ftemps -> f_t -> typed_idents **)

let f_temps x = x.f_temps

(** val f_args : ftemps -> f_t -> typed_idents **)

let f_args x = x.f_args

type coq_function = f_t

(** val fn_temps : ftemps -> f_t -> typed_idents **)

let fn_temps =
  f_temps

(** val fn_params : ftemps -> f_t -> typed_idents **)

let fn_params =
  f_args

(** val temps : ftemps -> coq_function -> typed_idents **)

let temps =
  fn_temps

(** val params : ftemps -> coq_function -> typed_idents **)

let params =
  fn_params

(** val some_temps : ftemps -> coq_function -> typed_idents **)

let some_temps ftype f =
  unique_temps_excl (temps ftype f) (params ftype f)

(** val some_args : ftemps -> coq_function -> typed_idents **)

let some_args ftype f =
  unique_temps (params ftype f)

(** val all_temps : ftemps -> coq_function -> typed_idents **)

let all_temps ftype f =
  unique_all_temps (temps ftype f) (params ftype f)
