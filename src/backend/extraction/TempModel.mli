open BinNums
open Ctypes
open Datatypes
open List0
open Specif

type __ = Obj.t

type typed_idents = (positive, coq_type) prod list

val my_peq : positive -> positive -> sumbool

val unique_temps_excl : typed_idents -> typed_idents -> typed_idents

val unique_temps : typed_idents -> typed_idents

val unique_all_temps : typed_idents -> typed_idents -> typed_idents

type ftemps = { f_temps : (__ -> typed_idents); f_args : (__ -> typed_idents) }

type f_t = __

val f_temps : ftemps -> f_t -> typed_idents

val f_args : ftemps -> f_t -> typed_idents

type coq_function = f_t

val fn_temps : ftemps -> f_t -> typed_idents

val fn_params : ftemps -> f_t -> typed_idents

val temps : ftemps -> coq_function -> typed_idents

val params : ftemps -> coq_function -> typed_idents

val some_temps : ftemps -> coq_function -> typed_idents

val some_args : ftemps -> coq_function -> typed_idents

val all_temps : ftemps -> coq_function -> typed_idents
