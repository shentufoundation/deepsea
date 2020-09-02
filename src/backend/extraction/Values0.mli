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

val type_of : ('a1, 'a2, 'a3, 'a4) op -> valtype

val value_of_val : coq_val -> nat option
