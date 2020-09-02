open Monad
open String0

type __ = Obj.t

type 'a optErr =
| Success of 'a
| Error of string

val coq_Monad_optErr : __ optErr coq_Monad
