
(* The type of tokens. *)

type token = 
  | XOR
  | WITH
  | UNEQUAL
  | UINT of (string)
  | TYPE
  | TRUSTED
  | TRANSFERETH
  | TO
  | THEN
  | STRING of (string)
  | STAR
  | SLASH
  | SIGNATURE
  | SHR
  | SHL
  | SEMICOLON
  | RPAREN
  | REFINED
  | RBRACKET
  | RBRACE
  | PLUS
  | OBJECT
  | MOD
  | MINUS
  | MATCH
  | MAPPING
  | LPAREN
  | LOGICAL
  | LIST
  | LET
  | LESSEQ
  | LESS
  | LBRACKET
  | LBRACE
  | LAYER
  | INT of (string)
  | INDEXED
  | IN
  | IF
  | IDENT of (Astcommon.ident)
  | GREATEREQ
  | GREATER
  | GHOST
  | FOR
  | FOLD
  | FIRST
  | FAIL
  | EXTERNAL
  | EVENT
  | EQUAL
  | EOF
  | END
  | EMIT
  | ELSE
  | DOUBLEARROW
  | DOT
  | DO
  | DISJUNCTION
  | DENY
  | CONSTRUCTOR
  | CONST
  | CONJUNCTION
  | COMMA
  | COLONLESS
  | COLONGREATER
  | COLONCOLON
  | COLON
  | CLONE
  | BITNOT
  | BITAND
  | BEGIN
  | BARBAR
  | BAR
  | BANG
  | AT
  | ASSIGN
  | ASSERT
  | ARROW
  | ARRAY
  | ADDRESS

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val file: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Parsetree.p_file_structure)
