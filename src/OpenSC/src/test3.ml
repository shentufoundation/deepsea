open Sast
open TranslateMinic

let _ =
  let open LanguageExt in
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Scanner.token lexbuf in
  let sprogram = Semant.check program in
(*   print_endline (string_of_sprogram sprogram) in *)
  let minicAST = TranslateMinic.minicgen sprogram in
  print_endline (show_genv minicAST)




  (* | MINIC -> let ge = minicgen filename ast_structure in print_endline (Backend.LanguageExt.show_genv ge)			      *)


(* ocamlbuild  -I backend test3.native *)
