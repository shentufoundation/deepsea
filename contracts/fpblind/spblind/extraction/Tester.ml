open LSrcEVMOPCODE
open LSrcBLINDAUCTION

let usage () = 
  Printf.eprintf "usage: extract (bytecode | debug)\n" ;
  exit 1

let () = if Array.length Sys.argv != 2 then usage ()

let debug_flag = match Array.get Sys.argv 1 with
  | "bytecode" -> false
  | "debug"    -> true
  | _          -> usage ()

let ge_compiled = Glue.full_compile_genv ge

let rec convert_list = function
  | Datatypes.Coq_nil -> []
  | Datatypes.Coq_cons (x, xs) -> x :: convert_list xs

let (program, label) =
  match ge_compiled with
  | None -> (Printf.eprintf "error in compilation\n" ; exit 1)
  | Some Coq_pair (p, l) -> (List.rev (convert_list p), l)

let intermediate = ASM.split program label
let asm = ASM.transform intermediate
let bytecode = ASM.assemble asm
let human_readable = ASM.show_asm asm

let () =
  if debug_flag then
    Printf.printf "%s\n" human_readable
  else
    Printf.printf "%s\n" bytecode
