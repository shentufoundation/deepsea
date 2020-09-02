(*
this module defines extensions to the generated L module
 *)

module L = Language

val show_genv : L.genv -> string
val fn_params' : L.coq_function -> (AST.ident * Ctypes.coq_type) list
val fn_temps' : L.coq_function -> (AST.ident * Ctypes.coq_type) list
