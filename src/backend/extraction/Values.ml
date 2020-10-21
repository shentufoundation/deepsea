open Datatypes
open Float
open Int0
open Types

type byte = nat

type char = nat

type name = char list

type ('i32, 'i64, 'f32, 'f64) op =
| Coq_i32 of 'i32
| Coq_i64 of 'i64
| Coq_f32 of 'f32
| Coq_f64 of 'f64

type coq_val = (I32.t, I64.t, F32.t, F64.t) op

(** val type_of : ('a1, 'a2, 'a3, 'a4) op -> valtype **)

let type_of = function
| Coq_i32 _ -> T_i32
| Coq_i64 _ -> T_i64
| Coq_f32 _ -> T_f32
| Coq_f64 _ -> T_f64

(** val value_of_val : coq_val -> nat option **)

let value_of_val = function
| Coq_i32 v -> Some (I32.to_nat v)
| Coq_i64 v -> Some (I64.to_nat v)
| _ -> None
