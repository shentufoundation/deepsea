(*
this module defines extensions to the generated BinNums module
 *)

val show_pos : BinNums.positive -> string
val show_nat : BinNums.coq_N -> string
val show_int : BinNums.coq_Z -> string

val z_of_bytestring : string -> BinNums.coq_Z
val z_of_numstring : string -> BinNums.coq_Z
val decimalstring2binarystring : string -> string
val numstring2decimalstring: string -> string