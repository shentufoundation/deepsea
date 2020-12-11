type ident = string

type builtin_type =
  | Tint
  | Tuint
  | Tbool
  | Tunit
  | Taddress
  (* EVM specifics *)
  | Thashvalue
  (* CertiKOS specifics *)
  | Tglobalpointer
  (*
  | Tval
  | Tflatmem
  *)

type method_kind =
  | MKnormal
  | MKlogical
  | MKrefined
  | MKconst
  | MKghost
  | MKconstghost
  | MKconstructor

type constant =
  | CONint of int
  | CONuint of int
  | CONbool of bool
  | CONunit
  | CONaddress of string
  | CONhashvalue
  (*| CONarray_init (*XXX remove -> a_big_expr in translation *)*)
  (* CertiKOS specifics *)
  | CONglobalpointer_undef
  (*
  | CONval_undef
  | CONflatmem_empty
  *)

type unop =
  | OPneg
  | OPnot
  | OPbitnot
  | OPbitneg  (* ??? *)
  | OPsha_1

type binop =
  | OPplus
  | OPminus
  | OPtimes
  | OPdivide
  | OPremainder
  | OPand
  | OPor
  | OPeq
  | OPne
  | OPlt
  | OPle
  | OPgt
  | OPge
  | OPshl
  | OPshr
  | OPxor
  | OPbitand  (* ??? *)
  | OPbitor  (* ??? *)
  | OPsha_2

val string_of_builtin_type : builtin_type -> string
val string_of_method_kind : method_kind -> string
val string_of_constant : constant -> string
val string_of_unop : unop -> string
val string_of_binop : binop -> string
val string_of_method_kind : method_kind -> string
