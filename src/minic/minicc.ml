open Minic
open Backend.BytecodeExt

let usage () = print_endline
    "usage: minicc file (print(-verbose)? | assembly(-runtime)? | (bytecode | ewasm)(-runtime)?)?";
    ignore (exit 1);

type modes = PRINT | VERBOSE | ASSEMBLY | ASSEMBLY_RUNTIME | BYTECODE | BYTECODE_RUNTIME | EWASM | EWASM_RUNTIME

let mode_of_string = function
  | "print" -> PRINT
  | "print-verbose" -> VERBOSE
  | "assembly" -> ASSEMBLY
  | "assembly-runtime" -> ASSEMBLY_RUNTIME
  | "bytecode" -> BYTECODE
  | "bytecode-runtime" -> BYTECODE_RUNTIME
  | "ewasm" -> EWASM
  | "ewasm-runtime" -> EWASM_RUNTIME
  | _ -> usage (); exit 1

let minic mode filename =
  let chan = open_in filename in
  let lexbuf = Lexing.from_channel chan in
  let parsed = try
    Parser.file Lexer.token lexbuf
  with Failure _ | Parser.Error ->
    let curr = lexbuf.Lexing.lex_curr_p in
    let line = curr.Lexing.pos_lnum in
    let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
    let tok = Lexing.lexeme lexbuf in
    print_endline (String.concat ""
      [ filename; ":"; string_of_int line; ":"; string_of_int cnum;
      ": Syntax error at token \""; tok; "\"." ]);
    exit 1
  in
  let name_tbls, genv = Typecheck.typecheck parsed in
  match mode with
  | PRINT ->
    print_endline (Backend.LanguageExt.show_genv false name_tbls genv)
  | VERBOSE ->
    print_endline (Backend.LanguageExt.show_genv true name_tbls genv)
  | ASSEMBLY -> print_endline (assembly false genv)
  | ASSEMBLY_RUNTIME -> print_endline (assembly true genv)
  | BYTECODE -> print_endline (bytecode false genv)
  | BYTECODE_RUNTIME -> print_endline (bytecode true genv)
  | EWASM -> print_endline (ewasm false genv)
  | EWASM_RUNTIME -> print_endline (ewasm true genv)

let main () =
  if (Array.length Sys.argv) = 1 then usage ();
  let mode =
    if (Array.length Sys.argv) >= 3 then mode_of_string Sys.argv.(2)
    else PRINT in
  minic mode Sys.argv.(1)

let () = main ()
