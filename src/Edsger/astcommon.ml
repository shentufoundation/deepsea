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
  | CONhashvalue (* default value 0u0 *)
  (*| CONarray_init*)
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

let string_of_builtin_type = function
  | Tint -> "int"
  | Tuint -> "uint"
  | Tbool -> "bool"
  | Tunit -> "unit"
  | Thashvalue -> "hashvalue"
  | Taddress -> "address"
  | Tglobalpointer -> "globalpointer"
  (*
  | Tval -> "val"
  | Tflatmem -> "flatmem"
  *)

let string_of_method_kind = function
  | MKnormal -> ""
  | MKlogical -> "logical "
  | MKconst -> "const "
  | MKghost -> "ghost "
  | MKconstghost -> "const ghost "
  | MKconstructor -> "constructor "
  | MKrefined -> "refined "

let string_of_constant = function
  | CONint n -> string_of_int n
  | CONuint n -> "(Int256.repr " ^ string_of_int n ^ ")"
  | CONbool b -> if b then "true" else "false"
  | CONunit -> "()"
  (* initial constant is the same as uint 0 *)
  | CONaddress addr -> "(Int256.repr " ^ Backend.BinNumsExt.numstring2decimalstring addr ^ ")"
  | CONhashvalue -> "(hashval_int256 Int256.zero)"
  (*| CONarray_init -> "array_init"*)
  | CONglobalpointer_undef -> "GLOBUndef"
  (*
  | CONval_undef -> "Vundef"
  | CONflatmem_empty -> "empty_flatmem"
  *)

let string_of_unop = function
  | OPneg -> "-"
  | OPnot -> "!"
  | OPbitnot -> "~"
  | OPbitneg -> "~"
  | OPsha_1 -> "keccak256 "

let string_of_binop = function
  | OPplus -> "+"
  | OPminus -> "-"
  | OPtimes -> "*"
  | OPdivide -> "/"
  | OPremainder -> "%"
  | OPand -> "/\\"
  | OPor -> "\\/"
  | OPeq -> "="
  | OPne -> "<>"
  | OPlt -> "<"
  | OPle -> "<="
  | OPgt -> ">"
  | OPge -> ">="
  | OPshl -> "<<"
  | OPshr -> ">>"
  | OPxor -> "^"
  | OPbitand -> "&"
  | OPbitor -> "|"
  | OPsha_2 -> "keccak256"
