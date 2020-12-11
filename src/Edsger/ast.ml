open Astcommon

type a_ctype =
  | ACtint
  | ACtchar
  | ACtvoid
  | ACtpointer of a_ctype
  | ACtarray of int * a_ctype
  | ACtmapping of a_ctype * a_ctype			
  | ACtstruct of ident * (ident * a_ctype) list

(* Inspired by Ocaml internals, most types are split into a_foo, and a_foo_desc.
   The description has the branching structure, while the a_foo is a record with extra information. *)

(* This is a description of how a DeepSpec value of an algebraic datatype is mapped to a C value. *)
type a_cimpl = {
  aImplDesc : a_cimpl_desc;
  aImplType : a_ctype
}

and a_cimpl_desc =
  | ACdefault
  | ACint of int
  | ACvar of ident
  | ACplus of a_cimpl * a_cimpl  (* or binop? *)
  | ACtimes of a_cimpl * a_cimpl  (* or binop? *)
  | ACcond of a_cimpl * a_cimpl * a_cimpl
  | ACarray of a_cimpl  (* do we really need this? *)
  | ACstruct of (ident * a_cimpl) list

type a_external_type = {
  aEXTypeName : ident;

  aEXTypeString : string;
  aEXTypeLowLevelInv : string option;
  mutable aEXTypeDefault : (string * ident) option  (* String & Name *)
}

type a_type = {
  aTypeDesc : a_type_desc;
  aTypeCtype : a_ctype;
  aTypePairIdent : ident;
  aTypeProvable : bool
    (* If  provable is true, the type has a reasonable C implementation.
       (Otherwise, there is a dummy "int" implementation, but this can not be proved correct.)
       External types are not provable, nor types without synthesis hints, except arrays. *)
}

and a_type_desc =
  | ATbuiltin of builtin_type
  | ATdata of ident * a_type_data
  | ATprod of a_type * a_type
  | ATarray of int * a_type
  | ATmapping of a_type * a_type
  | ATlist of a_type
  | ATexternal of a_external_type

and a_type_data =
  | ATsingleton of a_type_constr
  | ATbranches of a_type_constr list

and a_type_constr = {
  aTypeConstrName : ident;
  aTypeConstrArgs : (ident * a_type) list;
  aTypeConstrImpl : a_cimpl option
}

type a_event_type = {
  aEventName : ident;
  aEventArgs : (ident * a_type * bool) list;
}

type a_method_type = {
  aMethodArgumentTypes : a_type list;
  aMethodReturnType : a_type;
  aMethodKind : method_kind
}

type a_signature = {
  aSignatureName : ident;
  aSignatureMethods : (ident * a_method_type) list;
  aSignatureSuppressError : bool
}

type a_layer_signature = {
  aLayerSignatureName : ident;
  aLayerSignatureSlots : (ident * a_signature) list;
  aLayerSignatureSuppressError : bool
}

type a_object_type = {
  aObjectBase : a_layer_signature;
  aObjectSignature : a_signature
}

type a_layer_type = {
  aLayerBase : a_layer_signature;
  aLayerSignature : a_layer_signature
}

type tmp_id_t = int

type a_rexpr = {
  aRexprDesc : a_rexpr_desc;
  aRexprType : a_type
}

and a_rexpr_desc =
  | AEconst of constant
  | AEconstr_val of a_type_constr * (ident * a_rexpr) list
  | AEtemp of tmp_id_t * ident
  | AEunop of unop * a_rexpr
  | AEbinop of binop * a_rexpr * a_rexpr
  (* builtins in the EVM backend: *)
  | AEbuiltin of string * a_rexpr list

type a_lexpr = {
  aLexprDesc : a_lexpr_desc;
  aLexprType : a_type;

  aLexprIsGhost : bool
}

and a_lexpr_desc =
  | AEglob of ident
  | AEfield of a_lexpr * ident
  | AEindex of a_lexpr * a_rexpr
			   

(* "big" expressions are those that produce results that do not fit in a C 32-bit int. *)
type a_big_expr = {
  aBigExprDesc : a_big_expr_desc;
  aBigExprType : a_type
}

and a_big_expr_desc =
  | AErexpr of a_rexpr
  | AEstruct of a_type_constr * (ident * a_big_expr) list
  | AEconstr of a_type_constr * (ident * a_big_expr) list
  | AEexternal_const of string * ident                     (* things defined by "external let x = " *)
  (*| AEexternal_call of string * ident * a_big_expr list*)

type a_matchable = {
  aMatchableDesc : a_matchable_desc;
  aMatchableType : a_type
}

and a_matchable_desc =
  | AMtemp of tmp_id_t * ident
  | AMlexpr of a_lexpr
   (* Why is this an lexpr instead of an rexpr?
      Suppose we have a single-branch datatype, then we want to compile matches
          match e with
            | C x y
      into C code like
         x := e.A;
         y := e.B;
      When typechecking, we could make e either an lexpr or an rexp, but if we
      made it an rexpr we wouldn't have enough information to compile the field access. *)

type a_external_call_arg =
  | AEXrexer of a_rexpr
  | AEXlexpr of a_lexpr

type a_compile_time_constant =
  | ACONconst of constant
  | ACONstruct of (ident * a_compile_time_constant) list
  | ACONconstr of a_type_constr * (ident * a_compile_time_constant) list
  | ACONexternal_const of string * ident
  (*| ACONexternal_call of string * ident * a_compile_time_constant list*)

type a_external_const = {
  aExtConstName : string;
  aExtConstUnfolding : bool;
  aExtConstPartial : bool
}
			   
type a_object_field = {
  aObjectFieldName : ident;
  aObjectFieldType : a_type;
  aObjectFieldInitial : a_compile_time_constant;   (* the initializer. *)
  aObjectFieldIsLogical : bool  (* aka aObjectFieldNoImplementation *)
}

type tmp_env_t = (ident * (tmp_id_t * a_type)) list

type side_effect_t = {
  affectsAbstraction : bool;    (* Does it ever change the abstract state part? *)
  affectsImplementation : bool; (* Does it ever change a non-ghost variable? *)
  dependsOnAbstraction : bool;  (* Does it ever e.g. read a ghost variable? (So can not be compiled.) *)
  invokesLogical : bool         (* We don't remember what this is. *)
}

(* Loops can be annotated by an external name, if so they will be generated as separate global Coq symbol.
   For example, see the fist loop in object PMM, which captures MM_kern_valid_dec. *)
type a_captured_command = {
  aCapturedName : ident;
  aCapturedTemp : tmp_env_t
}

type a_command = {
  aCmdDesc : a_command_desc;
  aCmdType : a_type;
  aCmdEnv : tmp_env_t;

  aCmdEffect : side_effect_t
}

and a_command_desc =
  | ACskip
  | ACyield of a_rexpr
  | AClet of tmp_id_t * ident * a_command * a_command
  | ACsequence of a_command * a_command
  | ACcall of ident * ident * a_rexpr list
  | ACcond of a_rexpr * a_command * a_command
  | ACfor of tmp_id_t * ident * a_rexpr * tmp_id_t * a_rexpr * a_command
      (* FOR (*NUM*)    i     = 0      TO (*HOLDER*) n      DO s.run i *)
           * a_captured_command option
  | ACmatch of a_matchable * a_clause list
  | ACemit of a_event_type * a_rexpr list
             
  (* extension for certified programming *)
  | ACload of a_lexpr
  | ACstore of a_lexpr * a_rexpr
  | ACconstr of a_lexpr * a_big_expr
  | ACfail
  | ACassert of a_command
  | ACdeny of a_command
  | ACghost of a_command
  | ACfirst of tmp_id_t * ident * a_rexpr * tmp_id_t * a_rexpr *
      (* FIRST (*NUM*)    i     = 0      TO (*HOLDER*) n       *)
               tmp_id_t * a_command * a_command * a_command
         (* DO (*HOLDER*) s.test i THEN i    ELSE FAIL *)
             * a_captured_command option
  | ACfold of tmp_id_t * ident * a_rexpr * tmp_id_t * a_rexpr *
      (* FOLD (*NUM*)    i     = 0      TO (*HOLDER*) n       *)
              tmp_id_t * ident * a_rexpr * tmp_id_t * a_command
         (* | (*NUM*)    sum   = 0      DO (*HOLDER*) sum + i *)
            * a_captured_command option
  | ACexternal of a_lexpr option * string * ident *  a_external_call_arg list

and a_clause = a_type_constr option * tmp_env_t * a_command

type a_method_semantics =
  | ASdefault
  | ASsetCR3
  | ASassembly
  | ASassembly'  (* Actually defined in assembly? Used by [pt_in] and [pt_out] *)
  | AStrap_info_get
  | AStrap_info_ret

type a_accessor =
  | AAnone
  | AALoadStoreSem1
  | AALoadStoreSem2
  | AALoadStoreSem3

(* Assertions and refinement relations. *)
type a_proposition = string

type a_method_definition = {
  aMethodName : ident;
  aMethodArguments : ident list;
  aMethodType : a_method_type;
  aMethodSemantics : a_method_semantics;
  aMethodBody : a_command;
  aMethodParamEnv : tmp_env_t
}

type a_object = {
  aObjectName : ident;
  aObjectAddress : string option;
  (*aObjectSerial : int;*)
  aObjectCoreType : a_object_type;
  aObjectType : a_object_type;
  aObjectRequireImpl : bool;
  aObjectIsTrusted : bool;
  aObjectFields : a_object_field list;
  aObjectMethods : a_method_definition list;

  aObjectProcedures : a_method_definition list;
  aObjectFunctions : a_method_definition list
}

module IdentSet = Set.Make(String)

type a_checked_layer = {
  aCheckedLayerName : ident;
  (*aCheckedLayerSerial : int;*)
  (*aCheckedLayerCoreType : a_layer_type;*)
  aCheckedLayerType : a_layer_type;
  aCheckedLayerAccessor : a_accessor;
  aCheckedLayerInvariant : a_proposition option;

  aCheckedLayerPassthroughs : a_layer_signature;

  aCheckedLayerTipObjectSet : IdentSet.t;  (* object names *)
  aCheckedLayerKeelObjectSet : IdentSet.t; (* object names *)

  aCheckedLayerDesc : a_checked_layer_desc
}

and a_checked_layer_desc =
  | ALconstr of (ident * a_object) list  (* slot id -> object *)
  | ALrelax of a_checked_layer
  | ALinst of a_checked_layer * a_checked_layer
  | ALrefine of a_checked_layer * a_checked_layer * a_proposition

type a_layer = {
  aLayerName : ident;
  (*aLayerSerial : int;*)
  (*aLayerCoreType : a_layer_type;*)
  aLayerType : a_layer_type;
  aLayerAccessor : a_accessor;
  aLayerInvariant : a_proposition option;

  aLayerFreshObjects : (ident * a_object) list;  (* slot id -> object *)
  aLayerPassthroughObjects : (ident * a_object) list;
  aLayerAllObjects : (ident * a_object) list;

  aLayerTipObjectSet : IdentSet.t;  (* object names *)
  aLayerKeelObjectSet : IdentSet.t; (* object names *)

  aLayerDesc : a_layer_desc
}

and a_layer_desc =
  | ALbottom
  | ALontop of a_layer                   (* { s |-> w } : I[I] @ L *)
  | ALrefine_bottom of a_layer * a_proposition
                                         (* { s |-> w }        :> L *)
  (*
  | ALrefine_ontop of a_layer * a_layer  (* ({ s |-> w } @ L1) :> L2 *)
  *)

type a_declaration =
  | ADtype of a_type
  | ADevent of a_event_type
  (*
  | ADobject of a_object
  *)
  | ADlayer of a_layer
  (*
  | ADexternal_with of string * string list
  *)

type a_file = {
  aFileDeclarations : (ident * a_declaration) list;
  aFileGlobalAbstractDataType : a_external_type option;
  aFileExternalVerbatim : (string * string list) list;
  aFileExternalSymbols : a_external_const list;
}

(* Implementation of these three match the respecting definition in CompCert
   almost line by line (with omission as we only support a small subset of
   types). *)
let calign addr alignment = (addr + alignment - 1) / alignment * alignment
let rec calignof = function
  | ACtint -> 4
  | ACtchar -> 1
  | ACtvoid -> 1
  | ACtpointer _ -> 4
  | ACtarray (_, ct) -> calignof ct
  | ACtmapping (_, ct) -> calignof ct (* doesn't actually matter for EVM backend.*)
  | ACtstruct (_, ls) ->
    List.fold_left (fun curr_max (_, ct) -> max curr_max (calignof ct))
                   1 ls
let rec csizeof = function
  | ACtint -> 4
  | ACtchar -> 1
  | ACtvoid -> 1
  | ACtpointer _ -> 4
  | ACtarray (n, ct) -> csizeof ct * max 0 n
  | ACtmapping (_, ct) -> 1 (* doesn't actually matter for EVM backend. *)
  | ACtstruct (_, ls) as ct ->
    calign (List.fold_left
             (fun pos (_, ct) -> calign pos (calignof ct) + csizeof ct)
             0 ls)
           (calignof ct)

let rec a_type_desc_to_ident = function
  | ATbuiltin Tint -> "Z32"
  | ATbuiltin Tuint -> "Z" (* TODO: use clight builtin type *)
  | ATbuiltin Tbool -> "bool"
  | ATbuiltin Tunit -> "unit"
  | ATbuiltin Taddress -> "address"
  | ATbuiltin Thashvalue -> "hashvalue"
  | ATbuiltin Tglobalpointer -> "globalpointer"
  (*
  | ATbuiltin Tval -> "val"
  | ATbuiltin Tflatmem -> "flatmem"
  *)
  | ATdata (i, _) -> i
  | ATprod (t1, t2) -> "PR_" ^ a_type_desc_to_ident t1.aTypeDesc ^ "_"
                             ^ a_type_desc_to_ident t2.aTypeDesc
  | ATarray (n, t) -> "AR" ^ string_of_int n ^ "_" ^ a_type_desc_to_ident t.aTypeDesc
  | ATmapping (t1, t2) -> "HASH_" ^ a_type_desc_to_ident t1.aTypeDesc ^ "_" ^ a_type_desc_to_ident t2.aTypeDesc
  | ATlist t -> "list_" ^ a_type_desc_to_ident t.aTypeDesc
  | ATexternal extype -> extype.aEXTypeName

let rec a_type_contains_raw_type t = match t.aTypeDesc with
  (*
  | ATbuiltin Tval -> true
  *)
  | ATbuiltin Tglobalpointer -> true
  | ATbuiltin _ -> false
  | ATdata (_, ATsingleton c) -> a_type_constr_contains_raw_type c
  | ATdata (_, ATbranches cs) -> List.exists a_type_constr_contains_raw_type cs
  | ATprod (t1, t2) -> a_type_contains_raw_type t1 || a_type_contains_raw_type t2
  | ATarray (_, t') -> a_type_contains_raw_type t'
  | ATmapping (t1, t2) -> a_type_contains_raw_type t1 || a_type_contains_raw_type t2
  | ATlist _ -> true						
  | ATexternal _ -> true
and a_type_constr_contains_raw_type c =
  List.exists (fun (_, t) -> a_type_contains_raw_type t) c.aTypeConstrArgs

let string_of_a_ctype =
  let rec sf name prefix trailer = function
    | ACtint -> "int " ^ prefix ^ name ^ trailer
    | ACtchar -> "char " ^ prefix ^ name ^ trailer
    | ACtvoid -> "void " ^ prefix ^ name ^ trailer
    | ACtpointer ct -> sf name ("*" ^ prefix) trailer ct
    | ACtarray (n, ct) ->
      if prefix <> ""
        then sf name ("(" ^ prefix) (trailer ^ ")[" ^ string_of_int n ^ "]") ct
        else sf name prefix (trailer ^ "[" ^ string_of_int n ^ "]") ct
    | ACtmapping (kt, ct) -> "(mapping [stuff])"
    | ACtstruct (i, flds) -> "struct " ^ i ^ " {\n  " ^
      String.concat "\n  " (List.map (fun (f, ct) -> sf f "" "" ct ^ ";") flds) ^
      "\n} " ^ prefix ^ name ^ trailer
  in sf "_" "" ""

let rec string_of_a_cimpl c = match c.aImplDesc with
  | ACdefault -> "default"
  | ACint n -> string_of_int n
  | ACvar i -> "CN (" ^ i ^ ")"
  | ACplus (c1, c2) -> "(" ^ string_of_a_cimpl c1 ^ " + " ^ string_of_a_cimpl c2 ^ ")"
  | ACtimes (c1, c2) -> string_of_a_cimpl c1 ^ " * " ^ string_of_a_cimpl c2
  | ACcond (cond, c1, c2) ->
    "if " ^ string_of_a_cimpl cond ^ " then " ^ string_of_a_cimpl c1 ^
    " else " ^ string_of_a_cimpl c2
  | ACarray c -> "array[] (" ^ string_of_a_cimpl c ^ ")"
  | ACstruct lst -> "{ " ^
    String.concat "; " (List.map (fun (i, c) ->
      i ^ " = " ^ string_of_a_cimpl c) lst) ^ " }"

let rec string_of_a_type print_impl t =
  let body = match t.aTypeDesc with
    | ATbuiltin t -> string_of_builtin_type t
    | ATdata (i, d) -> "TN (" ^ i ^ ")" ^
      if print_impl
        then match d with
        | ATsingleton c ->" {\n  " ^
          String.concat ";\n  " (List.map (fun (f, t) ->
            f ^ " : " ^ string_of_a_type false t) c.aTypeConstrArgs) ^
          "\n}"
        | ATbranches lst -> " =\n  " ^
          String.concat "\n  " (List.map (fun c ->
            "| " ^ c.aTypeConstrName ^ String.concat ""
              (List.map (fun (f, t) ->
                " (" ^ f ^ " : " ^ string_of_a_type false t ^ ")")
                c.aTypeConstrArgs) ^ match c.aTypeConstrImpl with
                | None -> ""
                | Some impl -> " [[" ^ string_of_a_cimpl impl ^ "]]")
            lst)
        else ""
    | ATprod (t1, t2) -> "(" ^ string_of_a_type false t1 ^ " * " ^
                         string_of_a_type false t2 ^ ")"
    | ATarray (n, t) -> "array[" ^ string_of_int n ^ "] (" ^
                        string_of_a_type false t ^ ")"
    | ATmapping (t1, t2) -> "mapping[" ^ string_of_a_type false t1 ^ "] (" ^
                        string_of_a_type false t2 ^ ")"
    | ATlist t -> "list (" ^
                        string_of_a_type false t ^ ")"
    | ATexternal extype -> "EXT (\"" ^ String.escaped extype.aEXTypeString ^
        "\", " ^ extype.aEXTypeName ^ ")" ^
        (match extype.aEXTypeLowLevelInv with
         | None -> ""
         | Some s -> " LL(\"" ^ String.escaped s ^ "\"") ^
        if print_impl
          then match extype.aEXTypeDefault with
            | None -> "*no-default"
            | Some (s, i) -> "*def:(\"" ^ String.escaped s ^ "\", " ^ i ^ ")"
          else ""
  in body ^
     (if print_impl
      then " [[" ^ string_of_a_ctype t.aTypeCtype ^
             (if t.aTypeProvable then "" else "*") ^ "]]"
      else "")

let string_of_tmp_env tmp_env =
  String.concat "; " (List.map (fun (x,(i,t)) -> x
						 ^ " " ^ (string_of_int i)
						 ^ " : " ^ (string_of_a_type false t))
			        tmp_env)

let string_of_method_arg_types = function
  | [] -> "()"
  | [t] -> string_of_a_type false t
  | lst -> "[" ^ String.concat "; " (List.map (string_of_a_type false) lst) ^ "]"

let string_of_a_signature r =
  "<<" ^ r.aSignatureName ^ ">> {\n  " ^
  String.concat ";\n  " (List.map
    (fun (i, m) -> string_of_method_kind m.aMethodKind ^ i ^ " : " ^
      string_of_method_arg_types m.aMethodArgumentTypes ^ " -> " ^
      string_of_a_type false m.aMethodReturnType)
    r.aSignatureMethods) ^
  "\n}"

let string_of_a_layer_signature layer_sig =
  "<<" ^ layer_sig.aLayerSignatureName ^ ">> {\n  " ^
  String.concat ";\n  "
    (List.map (fun (i, s) -> i ^ " : " ^ string_of_a_signature s (*s.aSignatureName*)) layer_sig.aLayerSignatureSlots) ^
  "\n}"


let string_of_a_object_type t =
  "[" ^ string_of_a_layer_signature t.aObjectBase ^ "] " ^
  string_of_a_signature t.aObjectSignature

let string_of_a_layer_type t =
  "[" ^ string_of_a_layer_signature t.aLayerBase ^ "] " ^
  string_of_a_layer_signature t.aLayerSignature

let rec string_of_a_rexpr e = match e.aRexprDesc with
  | AEconst c -> string_of_constant c
  | AEconstr_val (c, ls) -> "CONSTR (" ^ c.aTypeConstrName ^
    String.concat " "
      (List.map (fun (i, e) -> "(" ^ i ^ " := " ^ string_of_a_rexpr e ^ ")") ls) ^
    ")"
  | AEtemp (n, i) -> i ^ "<" ^ string_of_int n ^ ">"
  | AEunop (op, e) -> string_of_unop op ^ string_of_a_rexpr e
  | AEbinop (op, e1, e2) ->
    "(" ^ string_of_a_rexpr e1 ^ " " ^ string_of_binop op ^ " " ^
    string_of_a_rexpr e2 ^ ")"
  | AEbuiltin (be,args) -> ("BUILTIN"^be)


let rec string_of_a_lexpr e = match e.aLexprDesc with
  | AEglob i -> "global (" ^ i ^ ")"
  | AEfield (e, i) -> string_of_a_lexpr e ^ ".{" ^ i ^ "}"
  | AEindex (e1, e2) -> string_of_a_lexpr e1 ^ "[" ^ string_of_a_rexpr e2 ^ "]"

let rec string_of_a_big_expr e = match e.aBigExprDesc with
  | AErexpr e -> "B(" ^ string_of_a_rexpr e ^ ")"
  | AEstruct (c, ls) -> "((" ^ c.aTypeConstrName ^ ")){" ^
    String.concat "; "
      (List.map (fun (i, e) -> i ^ " = " ^ string_of_a_big_expr e) ls) ^ "}"
  | AEconstr (c, ls) -> c.aTypeConstrName ^ " (" ^
    String.concat ") ("
      (List.map (fun (i, e) -> i ^ " := " ^ string_of_a_big_expr e) ls) ^
    ")"
  | AEexternal_const (s, i) -> "EXC(\"" ^ String.escaped s ^ "\", " ^ i ^ ")"
  (*
  | AEexternal_call (s, i, args) ->
    "EXF(\"" ^ String.escaped s ^ "\", " ^ i ^ ") (" ^
      String.concat ", " (List.map string_of_a_big_expr args) ^ ")"
  *)

let string_of_a_matchable e = match e.aMatchableDesc with
  | AMtemp (n, i) -> i ^ "<" ^ string_of_int n ^ ">"
  | AMlexpr e' -> string_of_a_lexpr e'

let rec string_of_a_compile_time_constant = function
  | ACONconst c -> string_of_constant c
  | ACONstruct ls -> "{" ^
    String.concat "; "
      (List.map (fun (i, e) -> i ^ " = " ^ string_of_a_compile_time_constant e) ls) ^ "}"
  | ACONconstr (c, ls) -> c.aTypeConstrName ^ " (" ^
    String.concat ") ("
      (List.map (fun (i, e) -> i ^ " := " ^ string_of_a_compile_time_constant e) ls) ^
    ")"
  | ACONexternal_const (s, i) -> "EXC(\"" ^ String.escaped s ^ "\", " ^ i ^ ")"
  (*
  | ACONexternal_call (s, i, args) ->
    "EXF(\"" ^ String.escaped s ^ "\", " ^ i ^ ") (" ^
      String.concat ", " (List.map string_of_a_compile_time_constant args) ^ ")"
  *)

let string_of_a_captured_command = function
  | None -> ""
  | Some cc -> " [[" ^ cc.aCapturedName ^
      (if cc.aCapturedTemp = [] then
         ""
       else
         "(" ^ String.concat ", " (List.map (fun a -> fst a) cc.aCapturedTemp)
             ^ ")") ^ "]]"

let rec string_of_a_command c = match c.aCmdDesc with
  | ACskip -> "skip"
  | ACyield e -> "val (" ^ string_of_a_rexpr e ^ ")"
  | AClet (n, x, c1, c2) ->
    "let " ^ x ^ "<" ^ string_of_int n ^ "> = " ^ string_of_a_command c1 ^
    "\nin " ^ string_of_a_command c2
  | ACsequence (c1, c2) -> string_of_a_command c1 ^ ";\n" ^
                           string_of_a_command c2
  | ACcall (s, f, es) -> "call " ^ s ^ "." ^ f ^ " (" ^
    String.concat ", " (List.map string_of_a_rexpr es) ^ ")"
  | ACcond (e, c1, c2) -> "if " ^ string_of_a_rexpr e ^
                          "\n  then " ^ string_of_a_command c1 ^
                          "\n  else " ^ string_of_a_command c2
  | ACfor (ni, i, l, nh, h, c, occ) ->
    "for" ^ string_of_a_captured_command occ ^
      " " ^ i ^ "<" ^ string_of_int ni ^ "> = " ^ string_of_a_rexpr l ^
    " to <" ^ string_of_int nh ^ ">" ^ string_of_a_rexpr h ^
    "\ndo " ^ string_of_a_command c
  | ACmatch (e, cls) -> "match " ^ string_of_a_matchable e ^ " with\n" ^
                          string_of_a_clause_list cls ^ "\nend"
  | ACemit (et, es) -> "emit("
                     ^ String.concat "," (List.map string_of_a_rexpr  es) ^ ")"
  | ACload e -> "load (" ^ string_of_a_lexpr e ^ ")"
  | ACstore (v, e) -> string_of_a_lexpr v ^ " := " ^ string_of_a_rexpr e
  | ACconstr (v, e) -> string_of_a_lexpr v ^ " ::= " ^ string_of_a_big_expr e
  | ACfail -> "fail"
  | ACassert c -> "assert (" ^ string_of_a_command c ^ ")"
  | ACdeny c -> "deny (" ^ string_of_a_command c ^ ")"
  | ACghost c -> "ghost (" ^ string_of_a_command c ^ ")"
  | ACfirst (ni, i, l, nh, h, nc, c1, c2, c3, occ) ->
    "first" ^ string_of_a_captured_command occ ^
        " " ^ i ^ "<" ^ string_of_int ni ^ "> = " ^
    string_of_a_rexpr l ^ " to <" ^ string_of_int nh ^ ">" ^ string_of_a_rexpr h ^
    "\ndo <" ^ string_of_int nc ^ ">" ^ string_of_a_command c1 ^
    "\nthen " ^ string_of_a_command c2 ^
    "\nelse " ^ string_of_a_command c3
  | ACfold (ni, i, l, nh, h, na, a, s, nc, c, occ) ->
    "fold" ^ string_of_a_captured_command occ ^
       " " ^ i ^ "<" ^ string_of_int ni ^ "> = " ^
    string_of_a_rexpr l ^ " to <" ^ string_of_int nh ^ ">" ^ string_of_a_rexpr h ^
    "\n| " ^ a ^ "<" ^ string_of_int na ^ "> = " ^ string_of_a_rexpr s ^
    "\ndo<" ^ string_of_int nc ^ "> " ^ string_of_a_command c
  | ACexternal (dest, s, i, []) ->
    (match dest with None -> "" | Some e -> string_of_a_lexpr e ^ " := ") ^
    "EXC(\"" ^ String.escaped s ^ "\", " ^ i ^ ")"
  | ACexternal (dest, s, i, args) ->
    (match dest with None -> "" | Some e -> string_of_a_lexpr e ^ " := ") ^
    "EXF(\"" ^ String.escaped s ^ "\", " ^ i ^ ") (" ^
      String.concat ", " (List.map (function
        | AEXrexer e -> string_of_a_rexpr e
        | AEXlexpr e -> string_of_a_lexpr e) args) ^ ")"
and string_of_a_clause_list cls = String.concat "\n"
  (List.map (fun (con_opt, vs, c) -> "| " ^
    (match con_opt with None -> "_" | Some con -> con.aTypeConstrName) ^ " " ^
    String.concat " "
      (List.map (fun (i, (n, _)) -> i ^ "<" ^ string_of_int n ^ ">") vs) ^
    " => " ^ string_of_a_command c) cls)

let string_of_aMethodArguments = function
  | [], [] -> "aMethodArguments[unexpected empty list]"
  | [i], [t] -> "(" ^ i ^ " : " ^ string_of_a_type false t ^ ")"
  | idents, types when List.length idents <> List.length types ->
    "aMethodArguments[unexpected uneven list]"
  | idents, types ->
    "(" ^ String.concat ", " (List.map2 (fun i t ->
            i ^ " : " ^ string_of_a_type false t) idents types) ^ ")"

let string_of_a_method_definition prefix m =
  prefix ^
  (match m.aMethodSemantics with
   | ASdefault -> ""
   | ASsetCR3 -> "[[semantics (setCR3)]]"
   | ASassembly -> "[[semantics (assembly)]]"
   | ASassembly' -> "[[semantics (assembly')]]"
   | AStrap_info_get -> "[[semantics (trap_info_get)]]"
   | AStrap_info_ret -> "[[semantics (trap_info_ret)]]") ^
  m.aMethodName ^ " " ^
  string_of_aMethodArguments (m.aMethodArguments, m.aMethodType.aMethodArgumentTypes) ^
  " = " ^ string_of_a_command m.aMethodBody

let rec string_of_a_object c = (*match d.pObjectDesc with
  | POname i -> begin match d.pObjectType with
    | None -> "ON (" ^ i ^ ")"
    | Some t -> "ON (" ^ i ^ " : " ^ string_of_p_object_type t ^ ")"
    end
  | POconstr c ->*) "Object " ^ (*string_of_int c.aObjectSerial ^ "> " ^*) c.aObjectName ^ " : " ^
      string_of_a_object_type c.aObjectType ^
      (if c.aObjectCoreType <> c.aObjectType
         then " (core: " ^ string_of_a_object_type c.aObjectCoreType ^ ")"
         else "") ^
      (if c.aObjectIsTrusted then
         " [[trusted]]{ "
       else
         if not c.aObjectRequireImpl then " [[logical]]{ " else "{ ") ^
      String.concat ";\n  "
        (List.map (fun f -> (if f.aObjectFieldIsLogical then "ghost " else "") ^
             f.aObjectFieldName ^ " : " ^
             string_of_a_type true f.aObjectFieldType ^ " := " ^
             string_of_a_compile_time_constant f.aObjectFieldInitial)
           c.aObjectFields @
         List.map (string_of_a_method_definition "") c.aObjectMethods @
         List.map (string_of_a_method_definition "[[procedure]] ")
           c.aObjectProcedures @
         List.map (string_of_a_method_definition "[[function]] ")
           c.aObjectFunctions
         ) ^
    "}"
  (*| POrelax (o, t) -> string_of_p_object o ^ " :> " ^
                      string_of_p_layer_type t*)

let string_of_a_accessor = function
  | AAnone          -> ""
  | AALoadStoreSem1 -> "LoadStoreSem1"
  | AALoadStoreSem2 -> "LoadStoreSem2"
  | AALoadStoreSem3 -> "LoadStoreSem3"

let rec string_of_a_checked_layer_aux succinct l =
  match l.aCheckedLayerDesc with
  | ALconstr objs ->
    (*if succinct then c.aCheckedLayerName else*)
    "{L" ^ string_of_a_accessor l.aCheckedLayerAccessor ^ (* string_of_int l.aLayerSerial ^ *) "{" ^
    string_of_a_layer_type l.aCheckedLayerType ^ " " ^
      String.concat ";\n  "
        (List.map (fun (s, o) -> s ^ " = " ^ o.aObjectName (*string_of_a_object o*))
                  objs) ^
    "\nPassthrough: " ^ String.concat ", "
      (List.map fst l.aCheckedLayerPassthroughs.aLayerSignatureSlots) ^
    "\nTip obj: " ^ String.concat ", " (IdentSet.elements l.aCheckedLayerTipObjectSet) ^
    "\nKeel obj: " ^ String.concat ", " (IdentSet.elements l.aCheckedLayerKeelObjectSet) ^
    "}}}"
  | _ ->
    "{L" ^ string_of_a_accessor l.aCheckedLayerAccessor ^ (* string_of_int l.aLayerSerial ^ *) "{" ^
    string_of_a_layer_type l.aCheckedLayerType ^ " " ^
    "\nPassthrough: " ^ String.concat ", "
      (List.map fst l.aCheckedLayerPassthroughs.aLayerSignatureSlots) ^
    "\nTip obj: " ^ String.concat ", " (IdentSet.elements l.aCheckedLayerTipObjectSet) ^
    "\nKeel obj: " ^ String.concat ", " (IdentSet.elements l.aCheckedLayerKeelObjectSet) ^
    "}}}" ^ match l.aCheckedLayerDesc with
  | ALconstr _ -> assert false
  | ALrelax l' -> " RELAXED " ^ string_of_a_checked_layer_aux true l'
  | ALinst (l1, l2) -> " INST " ^ string_of_a_checked_layer_aux true l1 ^
                       " AT " ^ string_of_a_checked_layer_aux true l2
  | ALrefine (l1, l2, p) -> " REFINED " ^ string_of_a_checked_layer_aux true l1 ^
                             " OVER " ^ string_of_a_checked_layer_aux true l2 ^
                             " WITH \"" ^ String.escaped p ^ "\""

let string_of_a_checked_layer = string_of_a_checked_layer_aux false

let rec string_of_a_layer l =
  "{L" ^ string_of_a_accessor l.aLayerAccessor ^ (* string_of_int l.aLayerSerial ^ *) "{" ^
  string_of_a_layer_type l.aLayerType ^ " " ^
  String.concat ";\n  "
    (List.map (fun (s, o) -> s ^ " = " ^ o.aObjectName ^ string_of_a_object o)
              l.aLayerFreshObjects) ^
  "\nPassthrough: " ^ String.concat ", " (List.map fst l.aLayerPassthroughObjects) ^
  "\nTip obj: " ^ String.concat ", " (IdentSet.elements l.aLayerTipObjectSet) ^
  "\nKeel obj: " ^ String.concat ", " (IdentSet.elements l.aLayerKeelObjectSet) ^
  "}}}" ^
  match l.aLayerDesc with
  | ALbottom -> ""
  | ALontop l' -> " INST " ^ string_of_a_layer l'
  | ALrefine_bottom (l', p) -> " REFINED " ^ string_of_a_layer l' ^
                               " WITH \"" ^ String.escaped p ^ "\""
  (*
  | ALrefine_ontop _ -> " @:> XXX"
  *)

let rec string_of_a_layer_desc ld =
  match ld with
  | ALbottom   -> "ALBottom"
  | ALontop l -> "(ALontop " ^ string_of_a_layer l ^")"
  | ALrefine_bottom (l, p) -> "(ALrefine_bottom " ^ string_of_a_layer l ^ " [some prop])"
							       
								 
