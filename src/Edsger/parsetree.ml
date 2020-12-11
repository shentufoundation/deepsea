open Astcommon
open Lexing

type location = {loc_start: position; loc_end: position;};;

let in_file name =
  let loc = { dummy_pos with pos_fname = name } in
  { loc_start = loc; loc_end = loc; }
;;

let none = in_file "__none__";;


type p_expression = 
  {
    p_expression_desc:  p_expression_desc;
    p_expression_loc: location;
  }
and p_expression_desc =
  | PEglob of ident
  | PEconstant of constant
  | PEun of unop * p_expression
  | PEbin of binop * p_expression * p_expression
  | PEpair of p_expression * p_expression
  | PEapp of p_expression * p_expression list
  | PEstruct of (ident * p_expression) list
  | PEfield of p_expression * ident
  | PEindex of p_expression * p_expression


type p_command = 
  {
    p_command_desc: p_command_desc;
    p_command_loc: location;
  }
and p_command_desc =
  (* | PCskip *)

  (* How do we distinguish pure expressions from effectful (object variable
     reading) ones?  To do so some type information is needed, hence they
     are clumped together when parsing. *)
  | PCyield of p_expression

  | PClet of ident * p_command * p_command
  | PCsequence of p_command * p_command
  (* | PCcall of ident * ident * p_expression *)
  (* premitive calls now parsed as
       PCyield (PEapp (PEfield (PEglob "slot", PEglob "method"),
                       [argument]))
  *)
  | PCcond of p_expression * p_command * p_command option
  | PCfor of ident * p_expression * p_expression * p_command * p_annotations
      (* FOR i     = 0           TO n           DO s.run i *)
  | PCmatch of p_expression * p_clause list

  (* Emitting ethereum events. For now the expression has to be an
     event constructor directly applied to arguments, but
     hypothetically you could pass around events as a first-class data type. *)
  | PCemit of p_expression

  (* extension for certified programming *)
  | PCstore of p_expression * p_expression
  | PCfail
  | PCassert of p_command
  | PCdeny of p_command
  | PCghost of p_command
  | PCfirst of ident * p_expression * p_expression * p_command *
      (* FIRST i     = 0           TO n           DO s.test i  *)
               p_command option * p_command option * p_annotations
       (* THEN i             ELSE FAIL *)
  | PCfold of ident * p_expression * p_expression *
      (* FOLD i     = 0           TO n            *)
              ident * p_expression * p_command    * p_annotations
         (* | sum   = 0           DO sum + i      *)

and p_clause = ident * ident list * p_command

and p_annotation =
  | PAexpr of p_command
  | PAclause of ident * p_annotation list
  (*| PArecord of (ident * p_annotation) list*)
and p_annotations = p_annotation list

(* This is just a hack to factor the grammar. *)
type p_pattern_tail =
  | PPTcons of ident 
  | PPTother of ident list

type p_type_FO = 
  {
    p_type_FO_desc: p_type_FO_desc;
    p_type_FO_loc: location;
  }
and p_type_FO_desc =
  | PTname of ident
  | PTbuiltin of builtin_type
  | PTdata of p_type_data * p_annotations
  | PTprod of p_type_FO * p_type_FO
  | PTarray of int * p_type_FO
  | PTmapping of p_type_FO * p_type_FO
  | PTlist of p_type_FO
           
and p_type_data =
  | PTsingleton of (ident * p_type_FO) list
  | PTbranches of p_type_constr list

and p_type_constr = {
  pTypeConstrName : ident;
  pTypeConstrArgs : (ident * p_type_FO) list;
  pTypeConstrAnnotations : p_annotations;
  pTypeConstrLoc: location
}

type p_signature =
  {
    p_signature_desc: p_signature_desc;
    p_signature_loc: location;
  }
and p_signature_desc =
  | PSname of ident
  | PSconstr of (ident * p_type_FO * p_type_FO * method_kind) list
  | PSghostize of p_signature * ident list
  | PSlogicize of p_signature * ident list
  | PSminus of p_signature * ident list

type p_layer_signature =
  {
    p_layer_signature_desc: p_layer_signature_desc;
    p_layer_signature_loc: location;
  }
and p_layer_signature_desc =
  | PLSname of ident
  | PLSconstr of (ident * p_signature) list

type p_object_type = {
  pObjectBase : p_layer_signature;
  pObjectSignature : p_signature;
  pObjectTypLoc: location;
}

type p_layer_type = {
  pLayerBase : p_layer_signature;
  pLayerSignature : p_layer_signature;
  pLayerLoc: location
}

type p_method_definition = {
  pMethodArguments : (ident * p_type_FO option) list;
  pMethodReturnType : p_type_FO option;
  pMethodKind : method_kind;
  pMethodBody : p_command;
  pMethodAnnotations : p_annotations;
  pMethodLoc: location;
}

type p_object_kind =
  | POnormal
  | POlogical
  | POtrusted

type p_object_construct = {
  pObjType : p_object_type;
  pObjKind : p_object_kind;
  pObjFields : (ident * p_type_FO * p_expression * bool (* is_ghost *)) list;
  pObjMethods : (ident * p_method_definition) list;
  pObjLoc: location;
}


type p_object = 
  {
    p_object_desc: p_object_desc;
    p_object_loc: location
  }
and p_object_desc =
  | POname of ident
  | POclone of ident
  | POconstr of p_object_construct
  | POrelax of p_object * p_layer_signature

type p_object_definition = {
  pObjectType : p_object_type option;
  pObjectDesc : p_object;
  pObjectLoc: location;
}

type p_object_inst =
  {
    p_object_inst_desc: p_object_inst_desc;
    p_object_inst_loc: location
  }
and p_object_inst_desc = 
  | POinternal of p_object
  | POexternal of constant * p_object

type p_layer_construct = (ident * p_object_inst) list

type p_proposition =
  {
    p_proposition_desc: p_proposition_desc;
    p_proposition_loc: location;
  }
and p_proposition_desc =
  | PPident of ident
  | PPexternal of string

type p_layer = 
  {
    p_layer_desc: p_layer_desc;
    p_layer_loc: location;
  }
and p_layer_desc =
  | PLname of ident
  | PLconstr of p_layer_construct
  | PLrelax of p_layer * p_layer_type
  | PLinst of p_layer * p_layer
  | PLrefine of p_layer * p_layer * p_proposition

type p_invariant = ident * location

type p_layer_definition = {
  (* pLayerType : p_layer_type option; *)
  pLayerDesc : p_layer;
  pLayerInvariant : p_proposition option;
  pLayerAnnotations : p_annotations;
  pLayerLoc: location;
}

type p_declaration = 
  {
    p_declaration_desc: p_declaration_desc;
    p_declaration_loc: location;
  }
and p_declaration_desc =
  | PDtype of p_type_FO
  | PDevent of (ident * p_type_FO * bool) list
  | PDsignature of p_signature
  | PDlayer_sig of p_layer_signature
  | PDobject of p_object_definition
  | PDlayer of p_layer_definition
  | PDexternal_with of string * p_annotations
  | PDexternal_type of string * string option * p_annotations
  | PDexternal_const of string * p_type_FO * p_annotations
  | PDexternal_function of string * (* arg : *) p_type_FO * (* ret : *) p_type_FO
                         * p_annotations
  | PDexternal_prop of string * p_type_FO

type p_file_structure = (ident * p_declaration) list

let rec int_of_p_expression = function
  | PEconstant (CONint n) -> n
  | PEun (OPneg, e) -> - int_of_p_expression e.p_expression_desc
  | PEun (OPbitnot, e) -> lnot (int_of_p_expression e.p_expression_desc)
  | PEbin (op, e1, e2) ->
    let n1 = int_of_p_expression e1.p_expression_desc in
    let n2 = int_of_p_expression e2.p_expression_desc in
    begin match op with
    | OPplus -> n1 + n2
    | OPminus -> n1 - n2
    | OPtimes -> n1 * n2
    | OPdivide -> n1 / n2
    | OPremainder -> n1 mod n2
    | OPbitand -> n1 land n2
    | OPbitor -> n1 lor n2
    | OPxor -> n1 lxor n2
    | OPshl -> n1 lsl n2
    | OPshr -> n1 lsr n2
    | _ -> raise (Invalid_argument "int_of_p_expression")
    end
  | _ -> raise (Invalid_argument "int_of_p_expression")


let string_of_location loc =
  let sloc_start_posfname = loc.loc_start.pos_fname in
  let sloc_satart_poslnum = loc.loc_start.pos_lnum in
  let sloc_satart_posbol = loc.loc_start.pos_bol in
  let sloc_satart_poscnum = loc.loc_start.pos_cnum in
  let eloc_end_posfname = loc.loc_end.pos_fname in
  let eloc_end_poslnum = loc.loc_end.pos_lnum in
  let eloc_end_posbol = loc.loc_end.pos_bol in
  let eloc_end_poscnum = loc.loc_end.pos_cnum in
  "\n" ^"filename1: " ^ sloc_start_posfname  ^ "pos_lnum1: " ^( string_of_int sloc_satart_poslnum) ^ "posbol1: " ^ (string_of_int sloc_satart_posbol)
  ^ "poscnum1: " ^ (string_of_int sloc_satart_poscnum) ^ "filename2: " ^
  eloc_end_posfname ^ "poslnum2: " ^ (string_of_int eloc_end_poslnum) ^ "posbo2: " ^ (string_of_int eloc_end_posbol) 
   ^ "poscnum2: " ^ (string_of_int eloc_end_poscnum) ^ "\n"

let rec string_of_p_expression pexp =
  (string_of_p_expression_desc pexp.p_expression_desc) (* ^ (string_of_location pexp.p_expression_loc) *)
and string_of_p_expression_desc = function
  | PEglob i -> "global(" ^ i ^ ")"
  | PEconstant c -> string_of_constant c
  | PEun (op, e) -> string_of_unop op ^ (string_of_p_expression e)
  | PEbin (op, e1, e2) ->
    "(" ^ string_of_p_expression e1 ^ " " ^ string_of_binop op ^ " " ^ string_of_p_expression e2 ^ ")"
  | PEpair (e1, e2) ->
    "(" ^ string_of_p_expression e1 ^ ", " ^ string_of_p_expression e2 ^  ")"
  | PEapp (e, es) ->
    string_of_p_expression_desc e.p_expression_desc ^ "location info: " ^ string_of_location e.p_expression_loc ^
    " (" ^ String.concat ") (" (List.map string_of_p_expression es) ^ ")"
  | PEstruct ls -> "{" ^
    String.concat "; "
      (List.map (fun (i, e) -> i ^ " = " ^ string_of_p_expression e) ls) ^ "}"
  | PEfield (e, i) -> string_of_p_expression e ^ ".{" ^ i ^ "}"
  | PEindex (e1, e2) -> string_of_p_expression e1 ^ "[" ^ string_of_p_expression e2 ^ "]"

let rec string_of_p_command pcmd =
    (string_of_p_command_desc pcmd.p_command_desc) (* ^ (string_of_location pcmd.p_command_loc) *) 
and string_of_p_command_desc = function
  (* | PCskip -> "skip" *)
  | PCyield e -> "val (" ^ string_of_p_expression e ^ ")"
  | PClet (x, c1, c2) -> "let " ^ x ^ " = " ^ string_of_p_command c1 ^
                         "\nin " ^ string_of_p_command c2
  | PCsequence (c1, c2) -> string_of_p_command c1 ^ ";\n" ^
                           string_of_p_command c2
  (* | PCcall (s, f, e) -> "call " ^ s ^ "." ^ f ^ " " ^ string_of_p_expression e *)
  | PCcond (e, c, None) -> "if " ^ string_of_p_expression e ^
                           "\n  then " ^ string_of_p_command c
  | PCcond (e, c1, Some c2) -> "if " ^ string_of_p_expression e ^
                          "\n  then " ^ string_of_p_command c1 ^
                          "\n  else " ^ string_of_p_command c2
  | PCfor (i, l, h, c, annos) ->
    "for" ^ string_of_p_annotations annos ^ " " ^ i ^ " = " ^ string_of_p_expression l ^
    " to " ^ string_of_p_expression h ^
    "\ndo " ^ string_of_p_command c
  | PCmatch (e, cls) -> "match " ^ string_of_p_expression e ^ " with\n" ^
                          string_of_p_clause_list cls ^ "\nend"
  | PCemit e -> "emit (" ^ string_of_p_expression e ^ ")"
  | PCstore (v, e) -> string_of_p_expression v ^ " := " ^
                      string_of_p_expression e
  | PCfail -> "fail"
  | PCassert c -> "assert (" ^ string_of_p_command c ^ ")"
  | PCdeny c -> "deny (" ^ string_of_p_command c ^ ")"
  | PCghost c -> "ghost (" ^ string_of_p_command c ^ ")"
  | PCfirst (i, l, h, c1, c2o, c3o, annos) ->
    "first" ^ string_of_p_annotations annos ^ " " ^ i ^ " = " ^
    string_of_p_expression l ^ " to " ^ string_of_p_expression h ^
    "\ndo " ^ string_of_p_command c1 ^
    begin match c2o with
    | Some c2 -> "\nthen " ^ string_of_p_command c2
    | None -> ""
    end ^
    begin match c3o with
    | Some c3 -> "\nelse " ^ string_of_p_command c3
    | None -> ""
    end
  | PCfold (i, l, h, a, s, c, annos) ->
    "fold" ^ string_of_p_annotations annos ^ " " ^ i ^ " = " ^
    string_of_p_expression l ^ " to " ^ string_of_p_expression h ^
    "\n| " ^ a ^ " = " ^ string_of_p_expression s ^
    "\ndo " ^ string_of_p_command c
and string_of_p_clause_list cls = String.concat "\n"
      (List.map (fun (i, vs, c) -> "| " ^ i ^ " " ^ String.concat " " vs ^
                                   " => " ^ string_of_p_command c) cls)

and string_of_p_annotations a =
  let rec anno = function
        | PAexpr c -> string_of_p_command c
        | PAclause (i, []) -> i
        | PAclause (i, a) -> i ^ " (" ^ annos a ^ ")"
  (*
        | PArecord lst -> " {" ^
          String.concat "; " (List.map (fun (i, a) -> i ^ " = " ^ anno a) lst) ^
          " }"
        *)
      and annos a = String.concat ", " (List.map anno a)
  in if a = [] then ""
               else " [[" ^ annos a ^ "]] "

let rec string_of_p_type_FO foptyp=
  (string_of_p_type_FO_desc foptyp.p_type_FO_desc) (* ^ (string_of_location foptyp.p_type_FO_loc) *)
and string_of_p_type_FO_desc = function
  | PTname i -> "TN (" ^ i ^ ")"
  | PTbuiltin t -> string_of_builtin_type t
  | PTdata (d, a) -> string_of_p_type_data d ^ string_of_p_annotations a
  | PTprod (t1, t2) -> "(" ^ string_of_p_type_FO t1 ^ " * " ^ string_of_p_type_FO t2 ^ ")"
  | PTarray (n, t) -> "array[" ^ string_of_int n ^ "] (" ^ string_of_p_type_FO t ^ ")"
  | PTmapping (t1, t2) -> "mapping[" ^ string_of_p_type_FO t1 ^ ", " ^ string_of_p_type_FO t2 ^ "]"
  | PTlist t -> "list[" ^ string_of_p_type_FO t ^ "]"


and string_of_p_type_constr c =
  c.pTypeConstrName ^
  String.concat " "
    (List.map (fun (i, t) -> "(" ^ i ^ " : " ^ string_of_p_type_FO t ^ ")")
              c.pTypeConstrArgs) ^
  string_of_p_annotations c.pTypeConstrAnnotations ^ string_of_location c.pTypeConstrLoc

and string_of_p_type_data = function
  | PTsingleton lst -> "{T{{ " ^
      String.concat ";\n  "
        (List.map (fun (i, t) -> i ^ " : " ^ string_of_p_type_FO t) lst) ^
    " }}}"
  | PTbranches lst -> "[T[[ " ^
      String.concat "\n  | " (List.map string_of_p_type_constr lst) ^
    " ]]]"

let rec string_of_p_signature_aux indent psig =
  (string_of_p_signature_aux_desc indent psig.p_signature_desc) (* ^ (string_of_location psig.p_signature_loc) *)
and string_of_p_signature_aux_desc indent = function
  | PSname i ->  i 
  | PSconstr lst -> "{R{{ " ^
      String.concat (";\n  " ^ indent)
        (List.map (fun (i, t1, t2, k) ->
    string_of_method_kind k ^ i ^
      " : " ^ string_of_p_type_FO t1 ^
      " -> " ^ string_of_p_type_FO t2) lst) ^
    " }}}"
  | PSghostize (s, lst) ->
    string_of_p_signature_aux indent s ^ " with ghost {" ^
      String.concat "; " lst ^ "}"
  | PSlogicize (s, lst) ->
    string_of_p_signature_aux indent s ^ " with logical {" ^
      String.concat "; " lst ^ "}"
  | PSminus (s, lst) ->
    string_of_p_signature_aux indent s ^ " minus {" ^
      String.concat "; " lst ^ "}"

let string_of_p_signature = string_of_p_signature_aux ""

let rec string_of_p_layer_signature pls =
  string_of_p_layer_signature_desc pls.p_layer_signature_desc (* ^ string_of_location pls.p_layer_signature_loc *)
and string_of_p_layer_signature_desc = function
  | PLSname i -> i
  | PLSconstr lst -> "{I{{ " ^
      String.concat ";\n  "
        (List.map (fun (i, r) -> i ^ " : " ^ string_of_p_signature_aux "  " r) lst) ^
    " }}}"

let string_of_p_object_type t =
  (if t.pObjectBase.p_layer_signature_desc = (PLSconstr [])
     then "[]"
     else "[" ^ string_of_p_layer_signature t.pObjectBase ^ "]") ^
  " " ^ string_of_p_signature t.pObjectSignature
    (* ^ string_of_location t.pObjectTypLoc *)

let string_of_p_layer_type t =
  (if t.pLayerBase.p_layer_signature_desc = PLSconstr []
     then "[]"
     else "[" ^ string_of_p_layer_signature t.pLayerBase ^ "]") ^
  " " ^ string_of_p_layer_signature t.pLayerSignature (* ^ string_of_location t.pLayerLoc *)

let string_of_pMethodArguments = function
  | [] -> "pMethodArguments[unexpected empty list]"
  | [i, None] -> i
  | [i, Some t] -> "(" ^ i ^ " : " ^ string_of_p_type_FO t ^ ")"
  | lst -> "(" ^ String.concat ", " (List.map (function
         | i, None -> i
         | i, Some t -> i ^ " : " ^ string_of_p_type_FO t) lst) ^ ")"

let string_of_p_object_kind = function
  | POnormal -> ""
  | POlogical -> "logical "
  | POtrusted -> "trusted "

let rec string_of_p_object pobj =
  string_of_p_object_desc pobj.p_object_desc (* ^ string_of_location pobj.p_object_loc *)
and string_of_p_object_desc = function
  | POname i -> (* begin match d.pObjectType with
    | None -> *) "ON (" ^ i ^ ")" (*
    | Some t -> "ON (" ^ i ^ " : " ^ string_of_p_object_type t ^ ")"
    end *)
  | POclone i -> 
    "CLONE ON (" ^ i ^ ")"
  | POconstr c -> "{O{" ^ string_of_p_object_type c.pObjType ^
      string_of_p_object_kind c.pObjKind ^ "{ " ^
      String.concat ";\n  "
        (List.map (fun (i, t, e, is_ghost) -> (if is_ghost then "ghost " else "") ^
                 i ^ " : " ^ string_of_p_type_FO t ^
                       " := " ^ string_of_p_expression e) c.pObjFields @
         List.map (fun (i, m) -> string_of_p_annotations m.pMethodAnnotations ^
                       i ^ " " ^ string_of_pMethodArguments m.pMethodArguments ^
                       " = " ^ string_of_p_command m.pMethodBody) c.pObjMethods) ^
    "}}}"
  | POrelax (o, t) -> string_of_p_object o ^ " :> " ^
                      string_of_p_layer_signature t

let string_of_p_object_definition d = string_of_p_object d.pObjectDesc (* ^ string_of_location d.pObjectLoc *)

(*
let string_of_p_collection_type_option = function
  | None -> ""
  | Some t -> string_of_p_collection_type t
*)

let rec string_of_p_proposition prop = 
  string_of_p_proposition_desc prop.p_proposition_desc (* ^ string_of_location prop.p_proposition_loc *)
and string_of_p_proposition_desc = function
  | PPident i -> i
  | PPexternal s -> "\"" ^ String.escaped s ^ "\""

let rec string_of_p_layer p_layer = 
  string_of_p_layer_desc p_layer.p_layer_desc (* ^ string_of_location p_layer.p_layer_loc *)
and string_of_p_layer_desc = function
  | PLname i -> (* begin match d.pLayerType with
    | None -> *) "CN (" ^ i ^ ")" (*
    | Some t -> "CN (" ^ i ^ " : " ^ string_of_p_layer_type t ^ ")"
    end *)
  | PLconstr c -> 
    "{C{" ^ (* string_of_p_layer_type_option d.pLayerType ^ *)
      (* string_of_p_annotations d.pLayerAnnotations ^ *) "{ " ^
      String.concat ";\n  "
        (List.map (fun (i, o) -> 
          let o = match o.p_object_inst_desc with
          | POinternal io -> io
          | POexternal (_, eo) -> eo
          in
          i ^ " = " ^ string_of_p_object o) c) ^
    "}}}"
  | PLrelax (c, t) -> "(" ^ string_of_p_layer c ^ " : " ^
                      string_of_p_layer_type t ^ ")"
  | PLinst (c1, c2) -> "(" ^ string_of_p_layer c1 ^ " @ " ^
                       string_of_p_layer c2 ^ ")"
  | PLrefine (c1, c2, p) -> "(" ^ string_of_p_layer c1 ^ " :> " ^
    string_of_p_layer c2 ^ " with " ^ string_of_p_proposition p ^ ")"

let string_of_p_layer_definition d =
 (* string_of_location d.pLayerLoc ^ *) string_of_p_annotations d.pLayerAnnotations ^
  string_of_p_layer d.pLayerDesc ^
  match d.pLayerInvariant with
  | None -> ""
  | Some p -> " assert " ^ string_of_p_proposition p 


