open Integers
(* open WasmPre *)

type asm (* representation corresponding directly to real EVM instructions *)
type asm_program  = asm list
type evm_program  = EVM.evm list
type intermediate = {
    constructor : evm_program ;
    body        : evm_program
  }

(* It returns both the entire program (including deployment part) 
   and also just the runtime part.  *)
val transform : evm_program -> AST.label -> (asm_program * asm_program)

(* final bytecode output *)
val assemble  : asm_program -> string

(* representation for humans *)
val mnemonics : asm_program  -> string
val mnemonics_wasm : Structure.coq_module -> bool -> Int.int list -> bool -> string

val size_of_program : asm list -> int
