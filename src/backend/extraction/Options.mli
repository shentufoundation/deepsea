open Datatypes
open OptErrMonad
open String0

val fromOption : 'a1 option -> string -> 'a1 optErr

val optional_filter : 'a1 option -> 'a1 list
