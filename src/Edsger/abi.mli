open Ast

val function_selector_of_method  : a_method_definition -> string
val function_selector_intval_of_method  : a_method_definition -> int

val event_topic_of_event_type : a_event_type -> Backend.BinNums.coq_Z
 
val json_of_layer : a_layer -> a_event_type list -> a_method_definition -> string
