type op = Add | Sub | Times | Divide | Equal | Neq |  And | Or | LGT | RGT | LGTEQ | RGTEQ | PASSIGN

type typ = 
	| Bool 
	| Int
	| Uint of string 
	| Address of string
	| Void of string
	| Mapstruct of typ list * typ


(* Need change *)
(* type ocamlbuiltin = Int | Bool | String *)

(* type param =  *)


type expr =
	| NumLit of int  (* number literal *)
	| BoolLit of bool
	| StrLit of string
	| Id of string
	| EnvLit of string * string
	| Mapexpr of expr * expr list 
	| Binop of expr * op * expr
	| Logexpr of expr * expr list
	| Storageassign of expr * expr 
	| Comparsion of expr * op * expr
	| Voidlit of string

type decls = 
	| Var of expr * typ
	| TypeAssigndecl of expr * typ
	| MapAssigndecl of expr * typ 
	| Eventdecl of expr * typ list
	| Constructordecl of expr * typ list * typ 
	| Methodecls of expr * typ list * typ

(* control flow statement: if, while ?? *)
(* type stmt =
	  Block of stmt list
	| Expr of expr
	| Return of expr *)

type consturctor_def ={
	name: expr;
	params: decls list;
	consturctor_body: expr list;
	return_type: typ;
}


type method_def = {
	methodname: expr;
	params: decls list;
	guard_body: expr list;
	storage_body: expr list;
	effects_body: expr list;
	returns: expr;
}

type interface_def = {
	signaturename: expr;
	interfacebody: decls list;
}

type implementation_def = {
	consturctor: consturctor_def;
	methods: method_def list;
}

(* type program = interface_def list * consturctor_def list *)
(* consturctor list is bad ! *)
type program = interface_def * implementation_def
(* type program = interface_def list * consturctor_def list * method_def list  *)

(* pretty printing *)
let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Times -> "*"
  | Divide -> "/"
  | Equal -> "=="
  | Neq -> "!="
  | And -> "&&"
  | Or -> "||"
	| LGT -> ">"
	| RGT -> "<"
	| LGTEQ -> ">="
	| RGTEQ -> "<="
	| PASSIGN -> "|->"

(* let string_of_builtin = function
		Int -> "int"
	|	Bool -> "boolean"
	|	String -> "string" *)
let unit_to_string () = "unit"	

let rec string_of_typ = function
		Bool-> "bool"
	| Int -> "int"
	| Uint(x) ->  "uint(" ^ x ^ ")"
	| Address(x) ->  "address(" ^ x ^ ")"
	| Void(x) ->  "void(" ^ x ^ ")"
	| Mapstruct(x, y) ->  "Mapstruct(" ^ String.concat " " (List.map string_of_typ x) ^ string_of_typ y ^ ")"

let rec string_of_expr = function
	| NumLit(l) -> "NumLit(" ^ string_of_int l ^ ")"
	| BoolLit(l) -> "BoolLit(" ^ string_of_bool l ^ ")"
	| StrLit(l) -> "StrLit(" ^ l ^ ")"
	| Id(x) -> x
	| EnvLit(l, l2) -> "EnvLit(" ^ l ^ (l2) ^ ")"
	| Mapexpr (l1, l2) -> "Mapexpr(" ^ string_of_expr l1 ^ " elements:" ^ String.concat " " (List.map string_of_expr l2) ^ ")"
	| Binop(e1, op, e2) ->  "Binop(" ^ (string_of_expr e1) ^ " "  ^ (string_of_op op) ^ " " ^ (string_of_expr e2) ^ ")"
	| Logexpr(e, el) -> "Logexpr(" ^ " " ^ string_of_expr e ^ " " ^ String.concat " " (List.map string_of_expr el) ^ ")"
	| Storageassign (e1, e2) -> "StorageAssign: " ^ string_of_expr e1 ^ " PASSIGN: |->" ^ string_of_expr e2 ^ ")"
	| Comparsion (e1, op, e2) ->" Comparsion: " ^ string_of_expr e1 ^ " " ^ string_of_op op ^ " " ^ string_of_expr e2
	| Voidlit(s) -> "Void: " ^ s


let rec string_of_decl = function
	| Var(x , t) -> "Var(" ^ string_of_expr x ^ ": " ^ string_of_typ t ^ ")"
	| TypeAssigndecl(l, t) -> "TypeAssign(" ^ string_of_expr l  ^ ": " ^ string_of_typ t ^ ")\n"
	| MapAssigndecl (l, t) -> "map " ^ string_of_expr l ^ " " ^ (string_of_typ t) ^ "\n"
	| Eventdecl(l ,t) ->  "event(" ^ (string_of_expr l) ^  String.concat " " (List.map string_of_typ t) ^ ")\n"
	| Constructordecl(l, t1, t2) ->"constructor " ^ " " ^ string_of_expr l ^  String.concat " " (List.map string_of_typ t1) ^ " " ^  string_of_typ t2 ^ "\n"
	| Methodecls (l, t1, t2) -> "method " ^ string_of_expr l ^ " "  ^ String.concat " " (List.map string_of_typ t1)  ^ (string_of_typ t2) ^ " " ^ "\n"


let string_of_interfacedef interfacedecl =
	"--interface\n\n" ^
	"signature " ^
	string_of_expr interfacedecl.signaturename ^ "\n " ^
	String.concat "\n " (List.map string_of_decl interfacedecl.interfacebody)

let string_of_constructordef constructordecl = 
	"constructor " ^ 
	string_of_expr constructordecl.name ^ 
	"(" ^ String.concat " \n " (List.map string_of_decl constructordecl.params) ^ ")\n " ^
	String.concat " \n " (List.map string_of_expr constructordecl.consturctor_body) ^
	"\n returns " ^ string_of_typ constructordecl.return_type ^ "\n\n"

let string_of_methoddef methoddecl = 
	"method " ^
	string_of_expr methoddecl.methodname ^ 
	"(" ^  String.concat ", " (List.map string_of_decl methoddecl.params) ^ ")" ^
	"\n guard\n  " ^ String.concat "\n  " (List.map string_of_expr methoddecl.guard_body) ^
	"\n storage\n  " ^ String.concat "\n  " (List.map string_of_expr methoddecl.storage_body) ^
	"\n effects\n  " ^ String.concat "\n  " (List.map string_of_expr methoddecl.effects_body) ^
	"\n returns " ^ string_of_expr methoddecl.returns ^ "\n\n"


let string_of_implementation implementdecl =
	"--implementation\n\n" ^
	string_of_constructordef implementdecl.consturctor ^ 
	String.concat "\n" (List.map string_of_methoddef implementdecl.methods)

let string_of_program (interfaces, implementations) =
	"\n\n-------------------\n  Parsed program \n-------------------\n\n" ^
	string_of_interfacedef interfaces ^ "\n"  ^
	string_of_implementation implementations ^ "\n\n***Yeah!***"
