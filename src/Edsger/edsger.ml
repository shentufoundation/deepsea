#include "config.h"

open Ast
open Backend.BytecodeExt
open Minicgen
open Parser
open Parsetree
open Typecheck
open Metadata

#ifdef ANT
let deepsea_basename = "DeepSEA/AntChain-EVM"
#else
let deepsea_basename = "DeepSEA/EVM"
#endif
let deepsea_version_num = "1.1.0"

let deepsea_version = deepsea_basename ^ " " ^ deepsea_version_num

type mode = ABI | BYTECODE | BYTECODE_RUNTIME | COMBINED_JSON | ASSEMBLY | MINIC | MINIC_VERBOSE | COQ | EWASM | EWASM_RUNTIME | METADATA

let string_of_token = function
  | ARRAY        -> "ARRAY"
  | MAPPING      -> "MAPPING"
  | CONST        -> "CONST"
  | CONSTRUCTOR  -> "CONSTRUCTOR"
  | EOF          -> "EOF"
  | EXTERNAL     -> "EXTERNAL"
  | GHOST        -> "GHOST"
  | IDENT s      -> "IDENT \"" ^ s ^ "\""
  | INT i        -> "INT \"" ^ i ^ "\""
  | UINT i       -> "UINT \"" ^ i ^ "\""
  | LOGICAL      -> "LOGICAL"
  | LAYER        -> "LAYER"
  | OBJECT       -> "OBJECT"
  (* | OF        -> "OF" *)
  | SIGNATURE    -> "SIGNATURE"
  | STRING s     -> "STRING(\"" ^ String.escaped s ^ "\")"
  | TRUSTED      -> "TRUSTED"
  | TYPE         -> "TYPE"
  | EVENT        -> "EVENT"
#ifdef ANT
  | IDENTITY      -> "IDENTITY"
#else
  | ADDRESS      -> "ADDRESS"
#endif
  | INDEXED      -> "INDEXED"
  | EMIT         -> "EMIT"
  | ASSERT       -> "ASSERT"
  | BEGIN        -> "BEGIN"
  | DENY         -> "DENY"
  | DO           -> "DO"
  | ELSE         -> "ELSE"
  | END          -> "END"
  | FAIL         -> "FAIL"
  | FIRST        -> "FIRST"
  | FOLD         -> "FOLD"
  | FOR          -> "FOR"
  | IF           -> "IF"
  | IN           -> "IN"
  | LET          -> "LET"
  | MATCH        -> "MATCH"
  | MOD          -> "MOD"
  (* | SKIP      -> "SKIP" *)
  | THEN         -> "THEN"
  | TO           -> "TO"
  | WITH         -> "WITH"
  | AT           -> "AT"
  | ARROW        -> "ARROW"
  | ASSIGN       -> "ASSIGN"
  | BAR          -> "BAR"
  | BARBAR       -> "BARBAR"
  | BANG         -> "BANG"
  | COLON        -> "COLON"
  | COLONCOLON   -> "COLONCOLON"
  | COLONGREATER -> "COLONGREATER"
  | COMMA        -> "COMMA"
  | CONJUNCTION  -> "CONJUNCTION"
  | DISJUNCTION  -> "DISJUNCTION"
  | DOT          -> "DOT"
  | DOUBLEARROW  -> "DOUBLEARROW"
  | EQUAL        -> "EQUAL"
  | GREATER      -> "GREATER"
  | GREATEREQ    -> "GREATEREQ"
  | LBRACE       -> "LBRACE"
  | LBRACKET     -> "LBRACKET"
  | LESS         -> "LESS"
  | LESSEQ       -> "LESSEQ"
  | LPAREN       -> "LPAREN"
  | MINUS        -> "MINUS"
  | PLUS         -> "PLUS"
  | RBRACE       -> "RBRACE"
  | RBRACKET     -> "RBRACKET"
  | RPAREN       -> "RPAREN"
  | SEMICOLON    -> "SEMICOLON"
  | STAR         -> "STAR"
  | SLASH        -> "SLASH"
  | UNEQUAL      -> "UNEQUAL"
  | BITNOT       -> "BITNOT"
  | BITAND       -> "BITAND"
  | XOR          -> "XOR"
  | SHL          -> "SHL"
  | SHR          -> "SHR"
  | LIST         -> "LIST"

let rec read_tokens buf =
  let token = Lexer.token buf in
  print_endline (string_of_token token);
  if token <> EOF then read_tokens buf

let rec string_of_declaration d =
  (string_of_declaration_desc d.p_declaration_desc) (* ^ (string_of_location d.p_declaration_loc) *)
and string_of_declaration_desc = function
  | PDtype(t) -> "string_of_p_type_FO: " ^ string_of_p_type_FO t
  | PDevent (lsy) -> "XD"
  | PDsignature(s) -> string_of_p_signature s
  | PDlayer_sig(f) -> string_of_p_layer_signature f
  | PDobject(d) -> string_of_p_object_definition d
  | PDlayer(c) -> string_of_p_layer_definition c
  | PDexternal_with(s, ann) -> "external" ^ string_of_p_annotations ann ^
         " with \"" ^ String.escaped s ^ "\""
  | PDexternal_type (s, lli_opt, ann) -> (string_of_p_annotations ann ^
         " = \"" ^ String.escaped s ^ "\"" ^
         match lli_opt with None -> "" | Some lli -> " \"" ^ lli ^ "\"")
  | PDexternal_const (s, t, ann) -> (string_of_p_annotations ann ^
         " : " ^ string_of_p_type_FO t ^
         " = \"" ^ String.escaped s ^ "\"")
  | PDexternal_function (s, arg, ret, ann) -> (string_of_p_annotations ann ^
         " : " ^ string_of_p_type_FO arg ^
         " -> " ^ string_of_p_type_FO ret ^ " = \"" ^ String.escaped s ^ "\"")
  | PDexternal_prop (s, t) -> (
         " : " ^ string_of_p_type_FO t ^
         " = \"" ^ String.escaped s ^ "\"")

let print_parse_file_structure structure =
  List.iter
    (function (i, d) -> print_endline (i ^ (string_of_declaration d)))
    structure

let print_ast_file_structure ast =
  List.iter
    (function
    | i, ADtype t ->
       print_endline ("AType " ^ i ^ " ::= " ^ string_of_a_type true t)
    | i, ADevent e ->
       print_endline ("AEvent " ^ i ^ " ::= [todo]")

    (*
    | i, ADobject o ->
      print_endline (string_of_a_object o)
    *)
    | i, ADlayer c ->
      print_endline ("ALayer " ^ i ^ " ::= " ^ string_of_a_layer c)
    (*
    | i, ADexternal_with (s, except) ->
      print_endline ("AExternal with \"" ^ String.escaped s ^ "\"" ^
        if except = []
          then ""
          else " except: " ^ String.concat ", " except)
    *)
    )
    ast.aFileDeclarations;
  List.iter (fun (s, except) ->
    print_endline ("AExternal with \"" ^ String.escaped s ^ "\"" ^
      if except = []
        then ""
        else " except: " ^ String.concat ", " except)
    ) ast.aFileExternalVerbatim


let usage () =
  prerr_endline ( "usage: dsc program.ds (bytecode(-runtime)? | abi | combined-json | assembly | minic(-verbose)? | coq | ewasm(-runtime)? | metadata)\n"
		  ^"or     dsc --version\n");
  exit 1

let combined_json filename abi bytecode =
  Printf.sprintf "{\"contracts\":{\"%s\":{\"abi\":\"%s\", \"bin\":\"%s\"}}, \"version\":\"%s\"}"
  		 filename
		 (Str.global_replace (Str.regexp "\"") "\\\"" abi)
		 bytecode
		 deepsea_version

let main argv =
  (if (Array.length argv = 2 && argv.(1) = "--version")
     then begin print_endline deepsea_version; exit 0 end);
  (if (Array.length argv <> 3) then usage());
  let filename = argv.(1) in
  let mode_flag = match Array.get argv 2 with
    | "bytecode"         -> BYTECODE
    | "bytecode-runtime" -> BYTECODE_RUNTIME
    | "abi"              -> ABI
    | "combined-json"    -> COMBINED_JSON
    | "assembly"         -> ASSEMBLY
    | "minic"            -> MINIC
    | "minic-verbose"    -> MINIC_VERBOSE
    | "coq"              -> COQ
    | "ewasm"            -> EWASM
    | "ewasm-runtime"    -> EWASM_RUNTIME
    | "metadata"         -> METADATA
    | _                  -> usage () in
  (* print_endline ("reading from \"" ^ filename ^ "\""); *)
  let ch = open_in filename in
  let buf = Lexing.from_channel ch in
  (*let _ = Location.init buf filename in*)
  let parse_structure = try
    Parser.file
      Lexer.token
      (*fun buf -> let t = Lexer.token buf
                  in print_endline ("TOK: " ^ string_of_token t); t*)
      buf
     with Failure _
        | Parser.Error  ->
	  let curr = buf.Lexing.lex_curr_p in
	  let line = curr.Lexing.pos_lnum in
	  let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
	  let tok = Lexing.lexeme buf in
	  print_endline (filename ^":"^ string_of_int line
			 ^":"^ string_of_int cnum
			 ^": Syntax error at token \"" ^ tok ^ "\".");
	  exit 1
   in
  (* let _ = print_parse_file_structure parse_structure in *)
  let has_error, ast_structure = typecheck parse_structure filename
  in
  if has_error then
    begin print_endline "typecheck failed"; exit 1 end
  else
    let abi = abigen ast_structure in
    match mode_flag with
    (*  COQ -> Coqgen.coqgen filename ast_structure *)
    | COQ -> print_endline "Coq output is not supported in this preview release."; exit 1
    | ABI -> print_endline abi
    | METADATA -> print_endline (metadata deepsea_version_num argv.(0) filename abi)
    | _ ->
      let name_tables, ge = minicgen ast_structure in
      match mode_flag with
      | MINIC -> print_endline (Backend.LanguageExt.show_genv false name_tables ge)
      | MINIC_VERBOSE -> let name_tables, ge = minicgen ast_structure in print_endline (Backend.LanguageExt.show_genv true name_tables ge)
      | EWASM -> print_endline (ewasm false ge)
      | EWASM_RUNTIME -> print_endline (ewasm true ge)
      | BYTECODE -> print_endline (bytecode false ge)
      | BYTECODE_RUNTIME -> print_endline (bytecode true ge)
      | ASSEMBLY -> print_endline (assembly ge)
      | COMBINED_JSON ->
        let asm, _ , programsize = get_bytecode_params ge in
        print_endline (combined_json filename abi (Backend.ASM.assemble asm))
      | COQ | ABI | METADATA -> (print_endline "unreachable"; exit 1)

let _ = main Sys.argv
