open Datatypes
open Int0

type __ = Obj.t

type valtype =
| T_i32
| T_i64
| T_f32
| T_f64

type functype =
| FT of valtype list * valtype list

type limits = { coq_L_min : I32.t; coq_L_max : I32.t option }

type memtype = limits

type tabletype = (limits, __) prod

type mut =
| GT_const
| GT_var

type globaltype = (mut, valtype) prod
