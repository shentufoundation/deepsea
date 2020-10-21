open Ast 


(* type sparam =  *)
type varscope = 
	| Sglobal 
	| Slocal

type sexpr = typ * sx
and sx = 
	| SNumLit of int  (* number literal *)
	| SBoolLit of bool
	| SStrLit of string
	| SId of varscope * string
	(* | SVar of sexpr * typ *)
	| SEnvLit of string * string
	| SMapexpr of sexpr * sexpr list 
	| SBinop of sexpr * op * sexpr
	| SLogexpr of sexpr * sexpr list
	| SStorageassign of sexpr * sexpr 
	| SComparsion of sexpr * op * sexpr
	| SVoidlit of string


type sconsturctor_def ={
	sname: sexpr;
	sparams: decls list;
	sconsturctor_body: sexpr list;
	sreturn_type: typ;
}


type smethod_def = {
	smethodname: sexpr;
	sparams: decls list;
	sguard_body: sexpr list;
	sstorage_body: sexpr list;
	seffects_body: sexpr list;
	sreturns: sexpr;
}

type sinterface_def = {
	ssignaturename: sexpr;
	sinterfacebody: decls list;
}

type simplementation_def = {
	sconsturctor: sconsturctor_def;
	smethods: smethod_def list;
}

type sprogram = sinterface_def * simplementation_def

let string_of_scope s = match s with
	| Slocal -> ":(local)"
	| Sglobal -> ":(global)"

let rec string_of_sexpr (t, e) = 
  "(" ^ string_of_typ t ^ ": " ^ 
  (match e with 
		SNumLit(x) -> string_of_int x
	| SBoolLit(x) -> string_of_bool x
	| SStrLit(x) -> "SStrLit(" ^ x ^ ")"
  | SId(s, x) -> "SId(" ^ x ^ string_of_scope s ^ ")"
  | SEnvLit(l, l2) -> "EnvLit(" ^ l ^ (l2) ^ ")"
	| SMapexpr (l1, l2) -> "Mapexpr(" ^ string_of_sexpr l1 ^ String.concat " " (List.map string_of_sexpr l2) ^ ")"
	| SBinop(e1, op, e2) ->  "Binop(" ^ (string_of_sexpr e1) ^ " " ^ (string_of_op op) ^ " " ^ (string_of_sexpr e2) ^ ")"
	| SLogexpr(e, el) -> "Log(" ^ string_of_sexpr e ^ " " ^ String.concat " " (List.map string_of_sexpr el) ^ ")"
	| SStorageassign(e1, e2) -> "Assign: " ^ (string_of_sexpr e1) ^ " <- " ^ (string_of_sexpr e2)
	| SComparsion(e1, op, e2) ->  "Comparsion: " ^ (string_of_sexpr e1) ^ " " ^ (string_of_op op) ^ " " ^ (string_of_sexpr e2)
	| SVoidlit(s) -> "Void: " ^ s
	(* | Svar(x, t) -> x ^ string_of_typ t *)
	(* | StypeAssign(x, y)-> "Type Assign: " ^ string_of_sexpr x  ^ " " ^ string_of_typ y ^ "\n" *)
	(* | SmapAssign(x, t1, t2) -> "Map assign: " ^ string_of_sexpr x ^ " " ^ (string_of_typ t1) ^ (string_of_typ t2) ^ "\n" *)
	(* | SpointAssign(x, e) -> "pointer assign: " ^string_of_sexpr x ^ " " ^ (string_of_sexpr e) ^ "\n" *)
	(* | Sevent(x, ty) -> x ^ "Event: " ^ String.concat " " (List.map string_of_typ ty) ^ "\n" *)
	(* | Sconstructorexpr(x, ty1, ty2) -> "constructor expr: " ^ " " ^ x ^ " " ^ string_of_typ ty1 ^ " " ^  string_of_typ ty2 ^ "\n" *)
	(* | Smethodexpr(x, ty1, ty2) -> "Method expr: " ^ x ^ " "  ^ string_of_typ ty1 ^ " " ^ string_of_typ ty2 ^ " " ^ "\n" *)
  ) ^ ")"

let string_of_sinterfacedef sinterfacedecl =
  "--interface\n\n" ^
  "signature " ^
  string_of_sexpr sinterfacedecl.ssignaturename ^ "\n " ^
  String.concat "\n " (List.map string_of_decl sinterfacedecl.sinterfacebody)
  

let string_of_sconstructordef constructordecl = 
  "constructor " ^
	string_of_sexpr constructordecl.sname ^ 
	"(" ^ String.concat " \n " (List.map string_of_decl constructordecl.sparams) ^ ")\n " ^
	String.concat " \n " (List.map string_of_sexpr constructordecl.sconsturctor_body) ^
	"\n returns " ^ string_of_typ constructordecl.sreturn_type ^ "\n\n"

let string_of_smethoddef methoddecl = 
  "method " ^
	string_of_sexpr methoddecl.smethodname ^ 
	"(" ^  String.concat "\n  " (List.map string_of_decl methoddecl.sparams) ^ ")" ^
	"\n guard\n  " ^ String.concat "\n  " (List.map string_of_sexpr methoddecl.sguard_body) ^
	"\n storage\n  " ^ String.concat "\n  " (List.map string_of_sexpr methoddecl.sstorage_body) ^
	"\n effects\n  " ^ String.concat "\n  " (List.map string_of_sexpr methoddecl.seffects_body) ^
	"\n returns " ^ string_of_sexpr methoddecl.sreturns ^ "\n\n"

let string_of_simplementation implementdecl =
  "--implementation\n\n" ^
	string_of_sconstructordef implementdecl.sconsturctor ^ 
	String.concat "\n" (List.map string_of_smethoddef implementdecl.smethods)

let string_of_sprogram (interfaces, implementations) =
	"\n\n--------------------------------\n  Semantically checked program \n--------------------------------\n\n" ^
	"" ^ ( string_of_sinterfacedef interfaces) ^ "\n"  ^
	"\n" ^ (string_of_simplementation implementations) ^ "\n\n***Yeah!***"
