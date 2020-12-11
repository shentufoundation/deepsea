open Integers
open StmtCGraph

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
val mnemonics_cgraph : (BinNums.positive * StmtCGraph.statement) list -> BinNums.positive -> string
val mnemonics_clash : (BinNums.positive * (BinNums.positive list)) list -> string
