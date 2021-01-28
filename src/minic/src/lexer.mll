{
open Parser
open Backend.AST

let trim s n =
  String.sub s n ((String.length s) - n)

}

let letter = ['A'-'Z' 'a'-'z' '_']
let digit = ['0'-'9']
let whitespace = [' ' '\t' '\r']
let decimal = ['1'-'9'] digit* | '0'

rule token = parse
  | "\n" { Lexing.new_line lexbuf; token lexbuf }
  | whitespace+ { token lexbuf }
  | decimal as d { NUM d }
  | ";" { SEMICOLON }
  | "," { COMMA }
  | "[" { LBRACKET }
  | "]" { RBRACKET }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "#" { HASH }
  | "=" { EQUAL }
  | "<-" { LARROW }
  | "//" { comment lexbuf }
  | "/*" { multi_comment 0 lexbuf }
  | "+" { ADD }
  | "-" { SUB }
  | "*" { MUL }
  | "/" { DIV }
  | "%" { MOD }
  | "**" { EXP }
  | "&" { AND }
  | "|" { OR }
  | "^" { XOR }
  | "<<" { SHL }
  | ">>" { SHR }
  | "==" { EQ }
  | "!=" { NE }
  | "<" { LT }
  | ">" { GT }
  | "<=" { LE }
  | ">=" { GE }
  | "!" { NOTBOOL }
  | "~" { NOTINT }
  | "." { DOT }
  | "sha1" { SHA1 }
  | "sha2" { SHA2 }
  | "if" { IF }
  | "else" { ELSE }
  | "void" { VOID }
  | "int" decimal? as s { INT (trim s 3) }
  | "bool" { BOOL }
  | "public" { PUBLIC }
  | "private" { PRIVATE }
  | "while" { WHILE }
  | "break" { BREAK }
  | "return" { RETURN }
  | "address" { ADDRESS }
  | "origin" { ORIGIN }
  | "caller" { CALLER }
  | "callvalue" { CALLVALUE }
  | "coinbase" { COINBASE }
  | "timestamp" { TIMESTAMP }
  | "number" { NUMBER }
  | "balance" { BALANCE }
  | "blockhash" { BLOCKHASH }
  | "transfer" { TRANSFER }
  | "revert" { REVERT }
  | "struct" { STRUCT }
  | "union" { UNION }
  | "emit" { EMIT }
  | "callmethod" { CALLMETHOD }
  | "memory" { MEMORY }
  | "chainid" { CHAINID }
  | "selfbalance" { SELFBALANCE }
  | letter (letter | digit)* as s { IDENT s }
  | eof { EOF }

and comment = parse
  | '\n' { Lexing.new_line lexbuf; token lexbuf }
  | _    { comment lexbuf }

and multi_comment count = parse
  | "/*" { multi_comment (count+1) lexbuf }
  | "*/" { if count > 0 then multi_comment (count-1) lexbuf else token lexbuf }
  | '\n' { Lexing.new_line lexbuf; multi_comment count lexbuf }
  | _    { multi_comment count lexbuf }
