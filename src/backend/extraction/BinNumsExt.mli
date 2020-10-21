(*
this module defines extensions to the generated BinNums module
 *)

val show_pos : BinNums.positive -> string
val show_nat : BinNums.coq_N -> string
val show_coq_int : BinNums.coq_Z -> string

val int_of_pos : BinNums.positive -> Z.t
val int_of_coq_int : BinNums.coq_Z -> Z.t

val z_of_bytestring : string -> BinNums.coq_Z
val z_of_numstring : string -> BinNums.coq_Z
val decimalstring2binarystring : string -> string
val numstring2decimalstring: string -> string

val positive_of_int : int -> BinNums.positive
val coq_Z_of_int: int -> BinNums.coq_Z
val coq_Z_of_Z: Z.t -> BinNums.coq_Z
