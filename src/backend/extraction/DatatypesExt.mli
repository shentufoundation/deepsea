(*
this module defines extensions to the generated Datatypes module
 *)

module D = Datatypes

val eval_nat : D.nat -> int
val caml_list : 'a D.list -> 'a list
val caml_prod : ('a, 'b) D.prod -> 'a * 'b 
val caml_bool : D.bool -> bool
val caml_option : 'a D.option -> 'a option
val coq_option: 'a option -> 'a D.option
val caml_string : String0.string -> string                                          
val coqlist_of_list : 'a list -> 'a D.list
