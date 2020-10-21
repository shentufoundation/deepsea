open Sast
open TranslateMinic

let _ =
  let open LanguageExt in
  let open Datatypes in 
  let open Glue in
  let open ASM in
  let open DatatypesExt in
  let ch = open_in filename in
  let lexbuf = Lexing.from_channel ch in
  let program = Parser.program Scanner.token lexbuf in
  let sprogram = Semant.check program in
(*   print_endline (string_of_sprogram sprogram) in *)
  let minicAST = TranslateMinic.minicgen sprogram in
     match full_compile_genv minicAST with
     | None -> print_endline "Compilation failed"; exit 1
     | Some (Coq_pair (program, entrypoint)) ->
        let asm =
          transform
            (List.rev (caml_list program))
            entrypoint in
            print_endline (show_genv minicAST); 
            print_endline (assemble asm)


  (* | MINIC -> let ge = minicgen filename ast_structure in print_endline (Backend.LanguageExt.show_genv ge)			      *)


(* ocamlbuild -pkg cryptokit -I backend test4.native *)
