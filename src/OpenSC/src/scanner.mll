{open Parser}

let digits = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']

rule token = parse
   [' ' '\t' '\r' '\n'] 	{ token lexbuf }
  |"/-"				 					{multicomment lexbuf} (* multiple comment *)
  | '('             	 { LPAREN }
	| ')'             	 { RPAREN }
	| '{'             	 { LBRACE }
	| '}'             	 { RBRACE }
	| '['             	 { LBRACK }
	| ']'             	 { RBRACK }
	(*  General op *)
	| "=="				 			 { EQ }
	| "!="				 			 { NEQ }
	| ">"				 				 { LGT } 
	| ">="							 { LGTEQ }
	| "<="							 { RGTEQ }
	| "<"								 { RGT} 
	| "+"				 				 { ADD }
	| "-"				 				 { SUB }
	| "*"				 				 { MUL }
	| "/"				 				 { DIVIDE }
	| "and"				 				 { AND }
	| "or"				 				 { OR }			
	(* end of general ops *)
	(*  Types *)
	| "UInt"			 			 { UINTTYPE("uint") }
	| "True"          	 { BooLit(true)  }
	| "False"         	 { BooLit(false) }
	| "Bool"          	 { BOOL }
	| "Address"			 		 { ADDRESSTYPE("ADDRESS") }
	| "map"				 			 { MAP } (* as hash table *)
	| "voidlit"					 {VOID("voidlit")} (* void is a literal type ... *)
	| "void"				 			 { UNIT("void") } (* instead of () use void *)
	(* end of types *)
	(* type of assignement*)
	| "->"				 			 { ARROW }
	| "|->"				 			 { PASSIGN }
	| "=>"							 { MAPASSIGN }
	| '='				 				 { ASSIGN }
	| ':'             	 { COLON } (* Type declaration *)
	(* end of type of assignments *)
	| '.'				 				 { POINT } (* Point for extract information *)
	| ';'								 { SEMI }
	| ','								 { COMMA } 
	(*  ==========================================================  *)
	| "signature"		 		 { SIGNATURE }		
	(* | "end"				       { END("END") }	separation op *)
	| "storage"			     { STORAGE }
	| "event"			 			 { EVENT }
	| "of"				 			 { OF }
	| "method"			 		 { METHOD }
	| "constructor"		 	 { CONSTRUCTOR }
	| "Env"				 			 { ENVIRONMENT("Env") }
	| "guard"			 			 { GUARD }
	| "effects"				 	 { EFFECTS }
	| "logs"			  		 { LOGS }
	| "returns"			 		 { RETURNS }
	(* NEED more type *)
	| '"' (([^'"']*) as s) '"'  { STRLIT(s) }
	| "int"    					{ INT }
	| digits+ as lem  { NUMLITERAL(int_of_string lem) }
	| letter (digits | letter | '_')* as lem { ID(lem) }
	| eof { EOF }

	and multicomment = parse
	  "-/"  { token lexbuf }
	| _     { multicomment lexbuf }
