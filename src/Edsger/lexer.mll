{
#include "config.h"

open Parser
open Parsetree

let keyword_table =
  let tbl = Hashtbl.create 20 in
  List.iter (fun (key, data) -> Hashtbl.add tbl key data) [
    "array", ARRAY;
    "mapping", MAPPING;
    "external", EXTERNAL;
    "const", CONST;
    "ghost", GHOST;
    "constructor", CONSTRUCTOR;
    "layer", LAYER;
    "logical", LOGICAL;
    "coarsed", REFINED;
    "object", OBJECT;
    (* "of", OF; *)
    "signature", SIGNATURE;
    "trusted", TRUSTED;
    "type", TYPE;
    "event", EVENT;
#ifdef ANT
    "identity", IDENTITY;
#else
    "address", ADDRESS;
#endif
    "emit", EMIT;
    "indexed", INDEXED;
    "assert", ASSERT;
    "begin", BEGIN;
    "deny", DENY;
    "do", DO;
    "else", ELSE;
    "end", END;
    "fail", FAIL;
    "first", FIRST;
    "fold", FOLD;
    "for", FOR;
    "if", IF;
    "in", IN;
    "let", LET;
    "match", MATCH;
    "mod", MOD;
    (* "skip", SKIP; *)
    "then", THEN;
    "to", TO;
    "with", WITH;
    "list", LIST;
    "clone", CLONE;
    "transferEth", TRANSFERETH;
  ];
  tbl

let comment_level = ref 0

let string_buf = Buffer.create 256


let incr_linenum lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  lexbuf.Lexing.lex_curr_p <- { pos with
    Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
    Lexing.pos_bol = pos.Lexing.pos_cnum;
  }

let sub_from_2nd st =
  String.sub st 2 ((String.length st)-2)

}

let newline = ('\010' | "\013\010" )
let identstart = ['A'-'Z' 'a'-'z' '_']
let identchar = ['A'-'Z' 'a'-'z' '_' '\'' '0'-'9']

let decimal_literal =
  ['1'-'9'] ['0'-'9' '_']* | '0'
let hex_literal =
  '0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f' '_']*
let oct_literal_C =
  '0' ['0'-'7' '_']+  (* NOTE: C syntax, different from OCaml *)
let oct_literal_OCaml =
  '0' ['o' 'O'] ['0'-'7' '_']+
let bin_literal =
  '0' ['b' 'B'] ['0'-'1' '_']+
let int_literal =
  decimal_literal | hex_literal | oct_literal_OCaml | bin_literal

rule token = parse
  | newline {  incr_linenum lexbuf; token lexbuf }
  | [' ' '\009' '\012'] +  { token lexbuf }

  | newline "__END__" newline { EOF }
  | eof { EOF }

  | "(*" { comment lexbuf; token lexbuf }
  | "\"" { Buffer.clear string_buf; string_ lexbuf }

  | identstart identchar*
      { let s = Lexing.lexeme lexbuf in
        try Hashtbl.find keyword_table s
        with Not_found -> IDENT s }

  | "0u" int_literal { UINT (sub_from_2nd (Lexing.lexeme lexbuf)) }
  | int_literal { INT ( Lexing.lexeme lexbuf) }
  | oct_literal_C { INT ("0o" ^ Lexing.lexeme lexbuf) }

  | "@" { AT }
  | "->" { ARROW }
  | ":=" { ASSIGN }
  | "||" { BARBAR }
  | "|" { BAR }
  | "!" { BANG }
  | "~" { BITNOT }
  | "::" { COLONCOLON }
  | ":" { COLON }
  | ":>" { COLONGREATER }
  | "<:" { COLONLESS }
  | "," { COMMA }
  | "/\\" { CONJUNCTION }
  | "\\/" { DISJUNCTION }
  | "." { DOT }
  | "=>" { DOUBLEARROW }
  | "=" { EQUAL }
  | ">" { GREATER }
  | ">=" { GREATEREQ }
  | "{" { LBRACE }
  | "[" { LBRACKET }
  | "<" { LESS }
  | "<=" { LESSEQ }
  | "(" { LPAREN }
  | "-" { MINUS }
  | "+" { PLUS }
  | "]" { RBRACKET }
  | "}" { RBRACE }
  | ")" { RPAREN }
  | ";" { SEMICOLON }
  | "*" { STAR }
  | "/" { SLASH }
  | "<>" { UNEQUAL }
  | "&" { BITAND }
  | "^" { XOR }
  | "<<" { SHL }
  | ">>" { SHR }

and comment = parse
  | "(*" { incr comment_level; comment lexbuf }
  | "*)" { if !comment_level > 0 then (decr comment_level; comment lexbuf) }
  | newline { incr_linenum lexbuf; comment lexbuf }
  | _ { comment lexbuf }

and string_ = parse
  | "\"" { STRING(Buffer.contents string_buf) }
  | "\\\"" { Buffer.add_char string_buf '"'; string_ lexbuf }
  | "\\n"  { Buffer.add_char string_buf '\n'; string_ lexbuf }
  | "\\\\" { Buffer.add_char string_buf '\\'; string_ lexbuf }
  | _ { Buffer.add_char string_buf (Lexing.lexeme_char lexbuf 0);
        string_ lexbuf }
  | eof { raise (Failure "Unterminated string") }
