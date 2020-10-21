
let digits = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']

rule token = parse
   [' ' '\t' '\r' '\n'] 	{ token lexbuf }
  |'-' 				 					{comment 1 lexbuf} (* comment *)
  |"/-"				 					{multicomment 1 lexbuf} (* multiple comment *)
  | '('             	 { "LPAREN" }
	| ')'             	 { "RPAREN" }
	| '{'             	 { "LBRACE" }
	| '}'             	 { "RBRACE" }
	| '['             	 { "LBRACK" }
	| ']'             	 { "RBRACK" }
	(*  General op *)
	| "=="				 			 { "EQ" }
	| "!="				 			 { "NEQ" }
	| ">"				 				 { "LGT" } 
	| "+"				 				 { "ADD" }
	| "-"				 				 { "SUB" }
	| "*"				 				 { "MUL" }
	| "/"				 				 { "DIVIDE" }
	| "and"				 				 { "AND" }
	| "or"				 				 { "OR" }			
	(* end of general ops *)
	(*  Types *)
	| "UInt"			 			 { "UINTType" }
	| "True"          	 { "BooLit(true)"  }
	| "False"         	 { "BooLit(false)" }
	| "Bool"          	 { "BOOL" }
	| "Address"			 		 { "ADDRESS" }
	| "map"				 			 { "MAP" } (* as hash table *)
	| "()"				 			 { "UNIT" }
	(* end of types *)
	(* type of assignement*)
	| "->"				 			 { "ARROW" }
	| "|->"				 			 { "PASSIGN" }
	| "=>"							 { "MAPASSIGN" }
	| '='				 				 { "ASSIGN" }
	| ':'             	 { "COLON" } (* Type declaration *)
	(* end of type of assignments *)
	| '.'				 				 { "POINT" } (* Point for extract information *)
	| ';'								 { "SEMI" }
	(*  ==========================================================  *)
	| "signature"		 		 { "SIGNATURE" }		
	| "end"				       { "END" }	(* separation op *)
	| "storage"			     { "STROAGE" }
	| "event"			 			 { "EVENT" }
	| "of"				 			 { "OF" }
	| "method"			 		 { "METHOD" }
	| "constructor"		 	 { "CONSTRUCTOR" }
	| "Env"				 			 { "ENVIRONMENT" }
	| "guard"			 			 { "GUARD" }
	| "effects"				 	 { "EFFECTS" }
	| "logs"			  		 { "LOGS" }
	| "returns"			 		 { "RETURNS" }
	| digits+ as lem    { "please"}
	| letter (digits | letter | '_')* as lem { lem }

	and comment lvl = parse
	  "\n"  { if lvl = 1 then token lexbuf else comment (lvl - 1) lexbuf  }
	| _     { comment lvl lexbuf }

	and multicomment lvl = parse
	  "-/"  { if lvl = 1 then token lexbuf else comment (lvl - 1) lexbuf  }
	| _     { multicomment lvl lexbuf }


{
  let _ = 
  let buf = Lexing . from_channel stdin in
  let f = token buf in
  print_endline f
}