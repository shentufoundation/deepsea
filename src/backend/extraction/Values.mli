open AST
open BinNums
open Integers
open Specif

type ident_ext =
| Iident of ident
| Ihash of ident_ext * Int256.int

module IdentExtIndexed :
 sig
  val eq : ident_ext -> ident_ext -> sumbool
 end

type coq_val =
| Vunit
| Vint of Int256.int
| Vptr of ident_ext
| Vhash of coq_val
| Vhash2 of coq_val * coq_val
