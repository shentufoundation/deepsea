#include "config.h"

open Ast
open Astcommon
open Parsetree


(* Like a boolean, but "false" carries an error message. *)
type check =
 | OK
 | Error of string

let check_and c1 c2 = match c1 with
  | Error _ -> c1
  | OK -> c2

let rec check_for_all f xs = match xs with
  | [] -> OK
  | x::xs -> check_and (f x) (check_for_all f xs)

let fresh_serial = ref 0 (* I know this is an ugly hack *)

let fresh id = 
  fresh_serial := !fresh_serial + 1;
  id ^ string_of_int !fresh_serial

(* These are "static" because they remain constant when checking an
   entire function body (although they are different for different functions). *)
type cmd_static_env_t = {
  variable_env : (ident, a_lexpr) Hashtbl.t;
  base_layer_signature : (ident * a_signature) list;
  function_env : a_method_definition list;
  procedure_env : a_method_definition list
}

let locationinfo_print loc filename =
    if loc == none then "File " ^ "'" ^ filename ^ "'" ^ ":"
    else if loc.loc_start.pos_lnum == loc.loc_end.pos_lnum 
      then "File \"" ^ filename ^ "\"" ^ "," ^ " line " ^ (string_of_int (loc.loc_end.pos_lnum)) ^ "," ^ " characters " ^ (string_of_int (loc.loc_start.pos_cnum - loc.loc_start.pos_bol)) ^ "-" ^ (string_of_int (loc.loc_end.pos_cnum - loc.loc_end.pos_bol)) ^ ":"
    else "File \"" ^ filename ^ "\"" ^ "," ^ " line " ^ ((string_of_int loc.loc_start.pos_lnum) ^ " between " ^ (string_of_int loc.loc_end.pos_lnum)) ^ ":" ;;

(* The length of an address, this is 20 bytes in Ethereum and 32 bytes in Ant Chain. *)
#ifdef ANT
let address_length = 32
#else
let address_length = 20
#endif

(* Does not belong here *)
let function_start_id_num = 10
let constructor_start_id_num = 10

(* We will combine code from all the constructors in a program into a
   single EVM constructor, so parameter names must be globally unique, 
   this variable tracks which ids have been allocated. *)
let constructor_current_id_num = ref constructor_start_id_num

(* The temp identifiers inside the constructor must not clash with the param names,
   so start from a high number.  *)
let constructor_temp_identifier_start = 100

let debug_endline _ = () (*print_endline*)
let catch_not_found f v = try Some (f v) with Not_found -> None
let default_not_found f v d = try f v with Not_found -> d ()

let mk_cmd_static_env v i f p = {
  variable_env = v; base_layer_signature = i; function_env = f; procedure_env = p
}

let find_method env s m =
  match catch_not_found (List.assoc s) env.base_layer_signature with
  | None -> `Not_found (s ^ " is not a defined slot")
  | Some sg -> match catch_not_found (List.find (fun (i, _) -> i = m))
                       sg.aSignatureMethods with
  | None -> `Not_found ("Signature " ^ sg.aSignatureName ^ " (bound to " ^ s ^
                        ") does not contain method " ^ m)
  | Some (_, mt) -> `Found mt

let globalpointer_GLOBP =
  { aTypeConstrName = "GLOBP";
    aTypeConstrArgs = [
      "b", { aTypeDesc = ATbuiltin Tint; aTypeCtype = ACtvoid;
             aTypePairIdent = "tint_ident"; aTypeProvable = false };
      "ofs", { aTypeDesc = ATbuiltin Tint; aTypeCtype = ACtvoid;
             aTypePairIdent = "tint_Z32"; aTypeProvable = false } ];
    aTypeConstrImpl = Some {
      aImplDesc = ACint 0;
      aImplType = ACtpointer ACtchar
    }
  }

let globalpointer_GLOBUndef =
  { aTypeConstrName = "GLOBUndef";
    aTypeConstrArgs = [];
    aTypeConstrImpl = Some {
      aImplDesc = ACdefault;
      aImplType = ACtpointer ACtchar
    }
  }

let tchar_pointer_globalpointer =
  { aTypeDesc = ATdata ("globalpointer", ATbranches [
      globalpointer_GLOBP; globalpointer_GLOBUndef
    ]);
    aTypeCtype = ACtpointer ACtchar;
    aTypePairIdent = "tchar_pointer_globalpointer";
    aTypeProvable = true
  }
  
let builtin_type_a_type t =
  match t with
  | Tglobalpointer -> tchar_pointer_globalpointer
  | _ ->
  let ct, tp, pf = match t with
    | Tint -> ACtint, "tint_Z32", true
    | Tuint -> ACtint, "tint_U", true
    | Tbool -> ACtint, "tint_bool", true
    | Taddress -> ACtint, "tint_U", true
    | Tunit -> ACtvoid, "tvoid_unit", true
    | Thashvalue -> ACtint, "tint_hashvalue", true
    | Tglobalpointer -> assert false
    (*
    | Tval -> ACtint, "tint_val", true
    | Tflatmem -> ACtint, "tint_flatmem", false
    *)
  in { aTypeDesc = ATbuiltin t; aTypeCtype = ct;
       aTypePairIdent = tp; aTypeProvable = pf }

let tvoid_unit = builtin_type_a_type Tunit
let tint_bool = builtin_type_a_type Tbool
let tint_Z32 = builtin_type_a_type Tint
let tint_U = builtin_type_a_type Tuint
let tint_address = builtin_type_a_type Taddress
let tint_hashvalue = builtin_type_a_type Thashvalue           

let rec type_pair_ident_ctype = function
  | ACtint -> "int"
  | ACtchar -> "char"
  | ACtvoid -> "void"
  | ACtpointer ct -> "pointer_" ^ type_pair_ident_ctype ct
  | ACtarray (_, ct) -> "array_" ^ type_pair_ident_ctype ct
  | ACtmapping (_, ct) -> "hash_" ^ type_pair_ident_ctype ct
  | ACtstruct (i, _) -> i

exception ConstAddressOverflow

let constant_type = function
  | CONint _ -> tint_Z32
  | CONuint _ -> tint_U
  | CONbool _ -> tint_bool
  | CONaddress _ -> tint_address
  | CONunit -> tvoid_unit
  | CONhashvalue ->  tint_hashvalue
  | CONglobalpointer_undef -> builtin_type_a_type Tglobalpointer

let unop_type op t = match op, t.aTypeDesc with
  | OPneg, ATbuiltin Tint -> Some t
  | OPneg, ATbuiltin Tuint -> Some t
  | OPnot, ATbuiltin Tbool -> Some t
  | OPbitnot, ATbuiltin Tuint -> Some t
  | OPsha_1, ATbuiltin Thashvalue -> Some tint_hashvalue
  | OPsha_1, ATbuiltin Tuint -> Some tint_hashvalue
  | OPsha_1, ATbuiltin Taddress -> Some tint_hashvalue
  | _ -> None

let binop_type op t1 t2 = match op with
  | OPplus | OPminus | OPtimes | OPdivide | OPremainder ->
    if t1.aTypeDesc = ATbuiltin Tint && t2.aTypeDesc = ATbuiltin Tint || t1.aTypeDesc = ATbuiltin Tuint && t2.aTypeDesc = ATbuiltin Tuint
      then Some t1 else None
  | OPbitand | OPbitor | OPxor ->
    if t1.aTypeDesc = ATbuiltin Tuint && t2.aTypeDesc = ATbuiltin Tuint
      then Some t1 else None
  | OPshl | OPshr ->
    if t1.aTypeDesc = ATbuiltin Tuint && t2.aTypeDesc = ATbuiltin Tint
      then Some t1 else None
  | OPand | OPor ->
    if t1.aTypeDesc = ATbuiltin Tbool && t2.aTypeDesc = ATbuiltin Tbool
      then Some t1 else None
  | OPeq | OPne ->
    if t1.aTypeDesc = t2.aTypeDesc then Some tint_bool else None
  | OPlt | OPle | OPgt | OPge ->
    if (t1.aTypeDesc = ATbuiltin Tint && t2.aTypeDesc = ATbuiltin Tint) ||
       (t1.aTypeDesc = ATbuiltin Tuint && t2.aTypeDesc = ATbuiltin Tuint)
    then Some tint_bool else None
  | OPsha_2 ->
     let is_hashable t = match t.aTypeDesc with
       | ATbuiltin Tuint -> true
       | ATbuiltin Taddress -> true
       | ATbuiltin Thashvalue -> true
       | _ -> false
     in
    if (is_hashable t1 && is_hashable t2)
    then Some tint_hashvalue else None

let dummy_type i =
  { aTypeDesc = ATdata (i, ATbranches []); aTypeCtype = ACtint;
    aTypePairIdent = "tint_unit"; aTypeProvable = false }
let dummy_signature i =
  { aSignatureName = i; aSignatureMethods = []; aSignatureSuppressError = true }
let dummy_layer_signature i =
  { aLayerSignatureName = i; aLayerSignatureSlots = []; aLayerSignatureSuppressError = true }
let dummy_layer_type i =
  { aLayerBase = dummy_layer_signature (i ^ "_base");
    aLayerSignature = dummy_layer_signature (i ^ "_inter") }
let dummy_rexpr =
  { aRexprDesc = AEtemp (-1, "*DUMMY*"); aRexprType = dummy_type "*DUMMY*" }
let dummy_lexpr =
  { aLexprDesc = AEglob "*DUMMY*"; aLexprType = dummy_type "*DUMMY*"; aLexprIsGhost = false }
let dummy_big_expr =
  { aBigExprDesc = AErexpr dummy_rexpr; aBigExprType = dummy_rexpr.aRexprType }
let dummy_object i =
  let t = { aObjectBase = dummy_layer_signature (i ^ "_base");
            aObjectSignature = dummy_signature (i ^ "_rec") }
  in { aObjectName = i; (*aObjectSerial = -1;*)
       aObjectAddress = None;
       aObjectCoreType = t; aObjectType = t;
       aObjectRequireImpl = true; aObjectIsTrusted = false;
       aObjectFields = []; aObjectMethods = [];
       aObjectProcedures = []; aObjectFunctions = [] }

let empty_var_env () = (Hashtbl.create 10 : (ident, a_lexpr) Hashtbl.t)
let empty_tmp_env () = ([] : tmp_env_t)
let empty_layer_signature =
  { aLayerSignatureName = "LSEmpty"; aLayerSignatureSlots = []; aLayerSignatureSuppressError = false }

let error_string_struct_unimplementable i name lst =
  "Structure type " ^ i ^ (if i = name then "" else " (struct " ^ name ^ ")") ^
  " contains unimplementable field(s):\n  " ^
  String.concat "\n  "
    (List.map (fun (f, t) -> f ^ " : " ^ string_of_a_type false t) lst)

let parse_type_annotations ctype_env =
  let rec f = function
    | [] -> None
    | (PAclause ("struct", [PAclause (cname, [])])) :: _ ->
      Some (ACtstruct (cname, []))
    | (PAclause ("int", [])) :: _ -> Some ACtint
    | (PAclause ("char", [])) :: _ -> Some ACtchar
    | (PAclause (name, [])) :: rest ->
      default_not_found (fun _ -> Some (Hashtbl.find ctype_env name)) ()
                        (fun _ -> f rest)
    | _ :: rest -> f rest
  in f

exception UnsupportedConstrAnnotation

let parse_constr_annotations ctype_env annos =
  let rec parse_expression e = match e.p_expression_desc with
    | PEglob i ->
      begin match Hashtbl.find_opt ctype_env i with
      | None -> raise UnsupportedConstrAnnotation
      | Some ct -> { aImplDesc = ACvar i; aImplType = ct }
      end
    | PEconstant (CONint n) -> { aImplDesc = ACint n; aImplType = ACtint }
    | PEbin (OPplus, e1, e2) ->
      let e1' = parse_expression e1 in
      let e2' = parse_expression e2 in
      if e1'.aImplType = ACtint && e2'.aImplType = ACtint
        then { aImplDesc = ACplus (e1', e2'); aImplType = ACtint }
        else raise UnsupportedConstrAnnotation
    | PEbin (OPtimes, e1, e2) ->
      let e1' = parse_expression e1 in
      let e2' = parse_expression e2 in
      if e1'.aImplType = ACtint && e2'.aImplType = ACtint
        then { aImplDesc = ACtimes (e1', e2'); aImplType = ACtint }
        else raise UnsupportedConstrAnnotation
    | PEapp ({p_expression_desc = (PEglob "array_init"); p_expression_loc = _}, [e]) -> (* PEglob "array_init"  *)
      let e' = parse_expression e in
      { aImplDesc = ACarray e'; aImplType = ACtarray (0, e'.aImplType) }
    | PEstruct lst ->
      let str_impl, str_flds = List.split @@ List.map (fun (i, e) ->
          let e' = parse_expression e in (i, e'), (i, e'.aImplType)
        ) lst
      in { aImplDesc = ACstruct str_impl; aImplType = ACtstruct ("", str_flds) }
    | _ -> raise UnsupportedConstrAnnotation in
  let rec parse_command c = match c.p_command_desc with
    | PCyield e -> parse_expression e
    | PCcond (e, c1, Some c2) ->
      let e' = parse_expression e in
      let c1' = parse_command c1 in
      let c2' = parse_command c2 in
      if e'.aImplType = ACtint && c1'.aImplType = c2'.aImplType
        then { aImplDesc = ACcond (e', c1', c2'); aImplType = c1'.aImplType }
        else raise UnsupportedConstrAnnotation
    | _ -> raise UnsupportedConstrAnnotation in
  let rec f = function
    | [] -> raise UnsupportedConstrAnnotation
    | (PAexpr c) :: _ -> parse_command c
    (*
    | (PAint n) :: _ -> Some { aImplDesc = ACint n; aImplType = ACtint }
    *)
    | (PAclause ("default", [])) :: _ ->
      { aImplDesc = ACdefault; aImplType = ACtvoid }
    (*
    | (PAclause (i, [])) :: rest ->
      begin match catch_not_found (Hashtbl.find ctype_env) i with
      | None -> f rest
      | Some ct -> Some { aImplDesc = ACvar i; aImplType = ct }
      end
    | (PAclause ("plus", [ a1; a2 ])) :: _ -> begin
      match f [a1], f [a2] with
      | Some c1, Some c2 ->
        if c1.aImplType = ACtint && c2.aImplType = ACtint
          then Some { aImplDesc = ACplus (c1, c2);
                      aImplType = ACtint }
          else None
      | _ -> None
      end
    | (PAclause ("cond", [ a_cond; a_then; a_else ])) :: _ -> begin
      match f [a_cond], f [a_then], f [a_else] with
      | Some c_cond, Some c_then, Some c_else ->
        if c_cond.aImplType = ACtint && c_then.aImplType = c_else.aImplType
          then Some { aImplDesc = ACcond (c_cond, c_then, c_else);
                      aImplType = c_then.aImplType }
          else None
      | _ -> None
      end
    | (PAclause ("array_init", [a])) :: _ -> begin
      match f [a] with
      | Some c -> Some { aImplDesc = ACarray c;
                         aImplType = ACtarray (0, c.aImplType) }
      | None -> None
      end
    | (PArecord lst) :: _ ->
      let impl_flds = List.fold_right (fun (i, a) flds ->
        match flds with None -> None | Some flds ->
        match f [a] with
        | None -> None
        | Some c -> Some ((i, c) :: fst flds,
                          (i, c.aImplType) :: snd flds)
        ) lst (Some ([], []))
      in begin match impl_flds with
         | None -> None
         | Some (impl, flds) -> Some { aImplDesc = ACstruct impl;
                                       aImplType = ACtstruct ("", flds) }
         end
    *)
    | _ :: rest -> f rest
  in try Some (f annos)
     with UnsupportedConstrAnnotation -> None

let rec cimpl_respects_ctype i ct = match i.aImplDesc with
  | ACdefault -> Some { aImplDesc = ACdefault; aImplType = ct }
  | ACarray elem -> begin match ct with
    | ACtarray (n, telem) -> begin match cimpl_respects_ctype elem telem with
      | Some elem' -> Some { aImplDesc = ACarray elem';
                             aImplType = ACtarray (n, elem'.aImplType) }
      | None -> None
      end
    | _ -> None
    end
  | ACstruct lst -> begin match ct with
    | ACtstruct (name, tlst) ->
      let tlst_map = Hashtbl.create (List.length tlst) in
      let _ = List.iter (fun (i, ct) -> Hashtbl.add tlst_map i ct) tlst in
      let lst_extra = List.fold_right (fun (f, elem) acc ->
        match acc with None -> None | Some (lst, extra) ->
        match Hashtbl.find_opt tlst_map f with
        | Some ct -> begin match cimpl_respects_ctype elem ct with
          | Some elem' -> Hashtbl.replace tlst_map f elem'.aImplType;
                          Some ((f, elem') :: lst, extra)
          | None -> None
          end
        | None -> Some ((f, elem) :: lst, (f, elem.aImplType) :: extra)
        ) lst (Some ([], [])) in
      begin match lst_extra with
      | Some (lst', extra) ->
        Some { aImplDesc = ACstruct lst';
               aImplType = ACtstruct (name, extra @
                 List.map (fun (i, _) -> i, Hashtbl.find tlst_map i) tlst) }
      | None -> None
      end
    | _ -> None
    end
  | _ -> if i.aImplType = ct then Some i else None

let ctype_is_small = function
  | ACtstruct _ -> false
  | _ -> true  (* ACtarray considered small because there is no construction *)

let type_is_big t = match t.aTypeDesc with
  | ATbuiltin _ -> false
  | _ -> true

let rexpr_respects_type e t =
  if e.aRexprType = t then Some e
  else match t.aTypeDesc, e.aRexprDesc with
    | ATarray (_, t'),
      AEconstr_val ({ aTypeConstrName = "array_init" } as cstr, []) ->
      let cstr' = { cstr with aTypeConstrArgs = ["_", t'] } in
      Some { aRexprDesc = AEconstr_val (cstr', []); aRexprType = t }
    | ATmapping (_, t'),
      AEconstr_val ({ aTypeConstrName = "mapping_init" } as cstr, []) ->
      let cstr' = { cstr with aTypeConstrArgs = ["_", t'] } in
      Some { aRexprDesc = AEconstr_val (cstr', []); aRexprType = t }   
    | _ -> None

let big_expr_respects_type e t =
  if e.aBigExprType = t then Some e
  else match e.aBigExprDesc with
    | AErexpr e' -> begin match rexpr_respects_type e' t with
      | Some e'' ->
        Some { aBigExprDesc = AErexpr e''; aBigExprType = e''.aRexprType }
      | _ -> None
      end
    | _ -> None

let side_effect_pure = {
  affectsAbstraction = false;
  affectsImplementation = false;
  dependsOnAbstraction = false;
  invokesLogical = false
}

let side_effect_store ghost = {
  affectsAbstraction = ghost;
  affectsImplementation = not ghost;
  dependsOnAbstraction = false;
  invokesLogical = false
}

let side_effect_join e1 e2 = {
  affectsAbstraction = e1.affectsAbstraction || e2.affectsAbstraction;
  affectsImplementation = e1.affectsImplementation || e2.affectsImplementation;
  dependsOnAbstraction = e1.dependsOnAbstraction || e2.dependsOnAbstraction;
  invokesLogical = e1.invokesLogical || e2.invokesLogical
}

let method_side_effect mt =
  let has_return_value = mt.aMethodReturnType <> tvoid_unit in
  match mt.aMethodKind with
  | MKnormal ->
    { affectsAbstraction = true; affectsImplementation = true;
      dependsOnAbstraction = false; invokesLogical = false }
  | MKrefined ->
    { affectsAbstraction = true; affectsImplementation = true;
      dependsOnAbstraction = false; invokesLogical = false }
  | MKlogical ->
    { affectsAbstraction = true; affectsImplementation = true;
      dependsOnAbstraction = false; invokesLogical = true }
  | MKconst -> side_effect_pure
  | MKconstructor -> 
    { affectsAbstraction = true; affectsImplementation = true;
      dependsOnAbstraction = false; invokesLogical = true }
  | MKghost ->
    { affectsAbstraction = true; affectsImplementation = false;
      dependsOnAbstraction = has_return_value; invokesLogical = false }
  | MKconstghost ->
    { affectsAbstraction = false; affectsImplementation = false;
      dependsOnAbstraction = has_return_value; invokesLogical = false }

(*
let rec command_is_pure c = match c.aCmdDesc with
  | ACyield _ -> true
  | AClet (_, _, c1, c2) -> command_is_pure c1 && command_is_pure c2
  | ACcond (_, c1, c2) -> command_is_pure c1 && command_is_pure c2
  | _ -> false
*)

let rec commandList_helper cmd_list typ env effect = match cmd_list with
    | [] -> {aCmdDesc = ACskip; aCmdType = typ; aCmdEnv = env; aCmdEffect = effect}
    | hd :: tl -> {aCmdDesc = ACsequence(hd, (commandList_helper tl typ env effect)); aCmdType = typ; aCmdEnv = env; aCmdEffect = effect}

let rec command_polymorphic c = match c.aCmdDesc with
  | ACfail -> true
  | AClet (_, _, _, c2) -> command_polymorphic c2
  | ACsequence (_, c2) -> command_polymorphic c2
  | ACcond (_, c1, c2) -> command_polymorphic c1 && command_polymorphic c2
  | ACmatch (_, cls) ->
    List.for_all (fun (_, _, c) -> command_polymorphic c) cls
  | ACfirst (_, _, _, _, _, _, _, c2, c3, _) ->
    command_polymorphic c2 && command_polymorphic c3
  | _ -> false

let rec command_poly_retype c t = match c.aCmdDesc with
  | ACfail -> { c with aCmdType = t }
  | AClet (n, i, c1, c2) ->
    { c with aCmdDesc = AClet (n, i, c1, command_poly_retype c2 t); aCmdType = t }
  | ACsequence (c1, c2) ->
    { c with aCmdDesc = ACsequence (c1, command_poly_retype c2 t); aCmdType = t }
  | ACcond (e, c1, c2) ->
    { c with aCmdDesc = ACcond (e, command_poly_retype c1 t, command_poly_retype c2 t);
             aCmdType = t }
  | ACmatch (e, cls) ->
    { c with aCmdDesc = ACmatch (e,
               List.map (fun (i, v, c) -> i, v, command_poly_retype c t) cls);
             aCmdType = t }
  | _ -> c

let command_respects_type c t =
  if c.aCmdType = t
    then Some c
    else if command_polymorphic c
      then Some (command_poly_retype c t)
      else None

let unify_commands cs =
  match catch_not_found (List.find (fun c -> not (command_polymorphic c))) cs with
  | None -> Some (cs, tvoid_unit)
  | Some c0 ->
    List.fold_right (fun c res -> match res with
      | None -> None
      | Some (res', t) -> match command_respects_type c t with
      | None -> None
      | Some c' -> Some (c' :: res', t)
    ) cs (Some ([], c0.aCmdType))

let command_remove_negation =
  let rec rec_rexpr e = match e.aRexprDesc with
    | AEunop (OPneg, e1) -> let rev, e1' = rec_rexpr e1 in not rev, e1'
    | _ -> false, e in
  let rec rec_cmd c = match c.aCmdDesc with
    | ACyield e ->
      let rev, e' = rec_rexpr e in rev, { c with aCmdDesc = ACyield e' }
    | AClet (n, i, c1, c2) ->
      let rev, c2' = rec_cmd c2 in rev, { c with aCmdDesc = AClet (n, i, c1, c2') }
    | ACsequence (c1, c2) ->
      let rev, c2' = rec_cmd c2 in rev, { c with aCmdDesc = ACsequence (c1, c2') }
    | _ -> false, c
  in rec_cmd

let expression_to_compile_time_constant =
  let rec from_rexpr e = match e.aRexprDesc with
    | AEconst c -> Some (ACONconst c)
    | AEconstr_val (c, ls) ->
      let ls' = List.fold_right (fun (i, e) res ->
        match res with None -> None
        | Some rest -> match from_rexpr e with None -> None
        | Some e' -> Some ((i, e') :: rest)
      ) ls (Some []) in
      begin match ls' with
      | None -> None
      | Some ls'' -> Some (ACONconstr (c, ls''))
      end
    | _ -> None in
  let rec from_big_expr e = match e.aBigExprDesc with
    | AErexpr e' -> from_rexpr e'
    | AEstruct (_, ls) ->
      let ls' = List.fold_right (fun (i, e') res ->
        match res with None -> None
        | Some rest -> match from_big_expr e' with None -> None
        | Some e'' -> Some ((i, e'') :: rest)
      ) ls (Some []) in
      begin match ls' with
      | None -> None
      | Some ls'' -> Some (ACONstruct ls'')
      end
    | AEconstr (c, ls) ->
      let ls' = List.fold_right (fun (i, e) res ->
        match res with None -> None
        | Some rest -> match from_big_expr e with None -> None
        | Some e' -> Some ((i, e') :: rest)
      ) ls (Some []) in
      begin match ls' with
      | None -> None
      | Some ls'' -> Some (ACONconstr (c, ls''))
      end
    | AEexternal_const (s, i) -> Some (ACONexternal_const (s, i))
    (*
    | AEexternal_call (s, i, args) ->
      let args' = List.fold_right (fun e res ->
        match res with None -> None
        | Some rest -> match from_big_expr e with None -> None
        | Some e' -> Some (e' :: rest)
      ) args (Some []) in
      begin match args' with
      | None -> None
      | Some args'' -> Some (ACONexternal_call (s, i, args''))
      end
    *)
  in from_big_expr

let method_kind_realizing k = k = MKnormal || k = MKconst || k = MKrefined

let method_kind_realized_by k1 k2 =
  match k1, k2 with
  | _,            (MKlogical | MKghost | MKconstghost) -> k1
  | MKlogical,    _        -> k2
  | MKghost,      _        -> MKnormal
  | MKconstghost, MKnormal -> MKnormal
  | MKconstghost, MKconst  -> MKconst
  | _,            _        -> k1

let method_type_weakereq i m1 m2 =
  if  m1.aMethodArgumentTypes <> m2.aMethodArgumentTypes 
    then Error ("For method " ^ i ^", srgument types " ^ string_of_method_arg_types m1.aMethodArgumentTypes
                ^ " and " ^ string_of_method_arg_types m2.aMethodArgumentTypes ^ " do not match")
  else if  m1.aMethodReturnType <> m2.aMethodReturnType
    then Error ("For method " ^i ^", return types " ^ string_of_a_type false m1.aMethodReturnType
                ^ " and " ^ string_of_a_type false m2.aMethodReturnType ^ " do not match")
  else match m1.aMethodKind, m2.aMethodKind with
       | MKlogical, _ -> OK
       | MKnormal, (MKnormal | MKconst) -> OK
       | MKghost, (MKghost | MKconst | MKconstghost) -> OK
       | MKconstghost, (MKconst | MKconstghost) -> OK
       | MKconst, MKconst -> OK
       | MKconstructor, MKconstructor -> OK
       | MKrefined, MKrefined -> OK
       | _ -> Error ("For method " ^ i ^ ", incompatible method qualifiers \"" ^ string_of_method_kind m1.aMethodKind
                     ^ "\" and \"" ^ string_of_method_kind m2.aMethodKind ^ "\"")

let method_type_realized_by m1 m2 =
  if m1.aMethodArgumentTypes = m2.aMethodArgumentTypes &&
     m1.aMethodReturnType = m2.aMethodReturnType then
    { m1 with
      aMethodKind = method_kind_realized_by m1.aMethodKind m2.aMethodKind }
  else
    m1

let signature_subseteq s1 s2 =
  if s1.aSignatureSuppressError || s2.aSignatureSuppressError
    then OK
    else check_for_all (fun (i1, m1) ->
           match catch_not_found (List.assoc i1) s2.aSignatureMethods with
           | Some m2 -> method_type_weakereq i1 m1 m2
           | None -> Error ("Method " ^ i1 ^ " not found.")
         ) s1.aSignatureMethods

let signature_join sg1 sg2 = {
  aSignatureName = sg1.aSignatureName ^ "_\\/_" ^ sg2.aSignatureName;
  aSignatureMethods =
    List.map
      (fun (i, m) ->
        match catch_not_found (List.assoc i) sg2.aSignatureMethods with
        | None -> i, m
        | Some m' -> i, method_type_realized_by m m'
      )
      sg1.aSignatureMethods;
  aSignatureSuppressError = sg1.aSignatureSuppressError || sg2.aSignatureSuppressError
}

let layer_signature_subseteq i1 i2 =
  if i1.aLayerSignatureSuppressError || i2.aLayerSignatureSuppressError
    then OK
    else check_for_all (fun (s, sg) ->
      match catch_not_found (List.assoc s) i2.aLayerSignatureSlots with
      | None -> Error ("Object " ^ s ^ " not found.")
      | Some sg' -> signature_subseteq sg sg') i1.aLayerSignatureSlots

let layer_signature_disjoint i1 i2 =
  if i1.aLayerSignatureSuppressError || i2.aLayerSignatureSuppressError
    then true
    else not (List.exists (fun (s, _) -> List.mem_assoc s i2.aLayerSignatureSlots)
      i1.aLayerSignatureSlots)

let layer_signature_union i1 i2 =
  { aLayerSignatureName = i1.aLayerSignatureName ^ "_U_" ^ i2.aLayerSignatureName;
    aLayerSignatureSlots =
      List.map
        (fun (s, sg) ->
          match catch_not_found (List.assoc s) i2.aLayerSignatureSlots with
          | None -> s, sg
          | Some sg' -> s, signature_join sg sg'
        )
        i1.aLayerSignatureSlots @
      List.filter
        (fun (s, _) -> not (List.mem_assoc s i1.aLayerSignatureSlots))
        i2.aLayerSignatureSlots;
    aLayerSignatureSuppressError =
      i1.aLayerSignatureSuppressError || i2.aLayerSignatureSuppressError }

let layer_signature_minus i1 i2 =
  { aLayerSignatureName = i1.aLayerSignatureName ^ "_\\_" ^ i2.aLayerSignatureName;
    aLayerSignatureSlots = List.filter
      (fun (s, _) -> not (List.mem_assoc s i2.aLayerSignatureSlots))
      i1.aLayerSignatureSlots;
    aLayerSignatureSuppressError =
      i1.aLayerSignatureSuppressError || i2.aLayerSignatureSuppressError }

let object_type_equal t1 t2 = true  (* XXX *)
let layer_signature_equal t1 t2 = true  (* XXX *)
let layer_type_equal t1 t2 = true  (* XXX *)
let default_constructor_signatureMethod = 
    [("constructor", {
                      aMethodArgumentTypes = [tvoid_unit];
                      aMethodReturnType = tvoid_unit;
                      aMethodKind = MKconstructor
                     })]

let typecheck parsed filename =
  let has_failure = ref false in
  let report_error s loc = print_endline ((locationinfo_print loc filename) ^ "\n" ^ "Error: " ^ s ^ "\n"); has_failure := true in
  let report_warning s loc = print_endline ((locationinfo_print loc filename) ^ "\n" ^ "Warning: " ^ s ^ "\n") in

  let check c msg loc = match c with
    | OK -> ()
    | Error msg' -> report_error (msg ^ " " ^ msg') loc in

  let declarations = ref [] in
  let add_declaration i d = declarations := (i, d) :: !declarations in
  let get_declarations _ = List.rev !declarations in

  let global_abstract_data_type_store = ref None in

  let external_verbatim = ref [] in
  let get_external_verbatim _ = List.rev !external_verbatim in
  let add_external_verabtim s exc =
        external_verbatim := (s, exc) :: !external_verbatim in

  let external_symbols = ref [] in
  let get_external_symbols _ = List.rev !external_symbols in

  let add_folded_unfolding_symbol ann s loc =
    let part = List.mem (PAclause ("partial", [])) ann in
    if List.mem (PAclause ("unfold", [PAclause ("disallowed", [])])) ann then
      external_symbols := { aExtConstName=s; aExtConstUnfolding=false; aExtConstPartial=part} :: !external_symbols
    else if List.mem (PAclause ("unfold", [PAclause ("always", [])])) ann then
      external_symbols := { aExtConstName=s; aExtConstUnfolding=true; aExtConstPartial=part} :: !external_symbols
    else
      report_error ("external symbol " ^ s ^": must specify either [[unfold disallowed]] or [[unfold always]].") loc in 

  let declaration_num = List.length parsed in
  let type_environment = Hashtbl.create declaration_num in
  let struct_field_environment = Hashtbl.create declaration_num in
  let type_constr_environment = Hashtbl.create declaration_num in
  let ctype_environment = Hashtbl.create declaration_num in
  let event_environment = Hashtbl.create declaration_num in
  let signature_environment = Hashtbl.create declaration_num in
  let layer_signature_environment = Hashtbl.create declaration_num in
  let object_environment = Hashtbl.create declaration_num in
  let layer_environment = Hashtbl.create declaration_num in
  let layer_declarations = Hashtbl.create declaration_num in

  let external_const_environment = Hashtbl.create declaration_num in
  let external_function_environment = Hashtbl.create declaration_num in
  let external_prop_environment = Hashtbl.create declaration_num in
  let ethbuiltins_environment = Hashtbl.create declaration_num in

  let _ = List.iter (fun (constr_name, data_type, constr) ->
      Hashtbl.add type_constr_environment constr_name (data_type, constr)) [
    "array_init",
    { aTypeDesc = ATarray (-1, tvoid_unit); aTypeCtype = ACtarray (-1, ACtvoid);
      aTypePairIdent = ""; aTypeProvable = true },
    { aTypeConstrName = "array_init"; aTypeConstrArgs = [];
       aTypeConstrImpl = None };
    "mapping_init",
    { aTypeDesc = ATmapping (tvoid_unit, tvoid_unit); aTypeCtype = ACtmapping (ACtvoid, ACtvoid);
      aTypePairIdent = ""; aTypeProvable = true },
    { aTypeConstrName = "mapping_init"; aTypeConstrArgs = [];
       aTypeConstrImpl = None };
    "GLOBP", tchar_pointer_globalpointer, globalpointer_GLOBP;
    "GLOBUndef", tchar_pointer_globalpointer, globalpointer_GLOBUndef ] in

  
  let _ = List.iter (fun (name, defn) ->
      Hashtbl.add ethbuiltins_environment name defn) [
                    "this_address",
                    ([], { aRexprDesc = AEbuiltin ("address", []); aRexprType = builtin_type_a_type Taddress });
                    "tx_origin",
                    ([], { aRexprDesc = AEbuiltin ("origin", []); aRexprType = builtin_type_a_type Taddress });
                    "msg_sender",
                    ([], { aRexprDesc = AEbuiltin ("caller", []); aRexprType = builtin_type_a_type Taddress });
                    "msg_value",
                    ([], { aRexprDesc = AEbuiltin ("callvalue", []) ; aRexprType = builtin_type_a_type Tuint });
                    "block_coinbase",
                    ([], { aRexprDesc = AEbuiltin ("coinbase", []); aRexprType = builtin_type_a_type Tuint });
                    "block_timestamp",
                    ([], { aRexprDesc = AEbuiltin ("timestamp", []); aRexprType = builtin_type_a_type Tuint });
                    "block_number",
                    ([],{ aRexprDesc = AEbuiltin ("number", []); aRexprType = builtin_type_a_type Tuint });
                    "chain_id",
                    ([],{ aRexprDesc = AEbuiltin ("chainid", []); aRexprType = builtin_type_a_type Tuint });
                    "self_balance",
                    ([],{ aRexprDesc = AEbuiltin ("selfbalance", []); aRexprType = builtin_type_a_type Tuint });
                    "balance",
                    ([builtin_type_a_type Taddress],{ aRexprDesc = AEbuiltin ("balance", []); aRexprType = builtin_type_a_type Tuint });
                    "blockhash",
                    ([builtin_type_a_type Tuint],{ aRexprDesc = AEbuiltin ("blockhash", []); aRexprType = builtin_type_a_type Tuint });
          
                  ] in
  
  let get_type i loc =
    try Hashtbl.find type_environment i
    with Not_found -> begin
      report_error ("Type name `" ^ i ^ "' unrecognized") loc;
      dummy_type i
    end in

  let construct_ATprod t1 t2 =
    let desc = ATprod (t1, t2) in
    let prod_ident = a_type_desc_to_ident desc in
    default_not_found (Hashtbl.find type_environment) prod_ident (fun _ ->
      let provable = t1.aTypeProvable && t2.aTypeProvable in
      let ctype = if provable
        then ACtstruct ("struct_" ^ prod_ident,
               ["fst", t1.aTypeCtype; "snd", t2.aTypeCtype])
        else ACtint in
      let type_pair_ident = if provable
        then "tstruct_" ^ prod_ident (* ^ "_" ^ prod_ident *)
        else "tint_" ^ prod_ident in
      let t = { aTypeDesc = desc; aTypeCtype = ctype;
                aTypePairIdent = type_pair_ident; aTypeProvable = provable }
      in Hashtbl.add type_environment prod_ident t;
         add_declaration prod_ident (ADtype t);
         t
    ) in

  let construct_ATarray n t =
    let desc = ATarray (n, t) in
    let array_ident = a_type_desc_to_ident desc in
    default_not_found (Hashtbl.find type_environment) array_ident (fun _ ->
      let ct = ACtarray (n, t.aTypeCtype) in
      let t = {
          aTypeDesc = desc;
          aTypeCtype = ct;
          aTypePairIdent = "t" ^ type_pair_ident_ctype ct ^ "_" ^ array_ident;
          aTypeProvable = t.aTypeProvable
        }
      in Hashtbl.add type_environment array_ident t;
         add_declaration array_ident (ADtype t);
         t
    ) in

  let construct_ATmapping t1 t2 =
    let desc = ATmapping (t1, t2) in
    let mapping_ident = a_type_desc_to_ident desc in
    default_not_found (Hashtbl.find type_environment) mapping_ident (fun _ ->
      let ct = ACtmapping (t1.aTypeCtype, t2.aTypeCtype) in
      let t = {
          aTypeDesc = desc;
          aTypeCtype = ct;
          aTypePairIdent = "t" ^ type_pair_ident_ctype ct ^ "_" ^ mapping_ident;
          aTypeProvable = t1.aTypeProvable && t2.aTypeProvable
        }
      in Hashtbl.add type_environment mapping_ident t;
         add_declaration mapping_ident (ADtype t);
         t
    ) in
  
  
  let construct_ATlist  t =
    let desc = ATlist t in
    let list_ident = a_type_desc_to_ident desc in
    default_not_found (Hashtbl.find type_environment) list_ident (fun _ ->
      let provable = false in
      let ctype = ACtint in
      let type_pair_ident = "tint_" ^ list_ident in
      let t = { aTypeDesc = desc; aTypeCtype = ctype;
                aTypePairIdent = type_pair_ident; aTypeProvable = provable }
      in Hashtbl.add type_environment list_ident t;
         add_declaration list_ident (ADtype t);
         t
    ) in
  
  let rec translate_type typ = match typ.p_type_FO_desc with 
    | PTname i -> get_type i typ.p_type_FO_loc
    | PTbuiltin t -> builtin_type_a_type t
    | PTprod (t1, t2) -> construct_ATprod (translate_type t1) (translate_type t2)
    | PTarray (n, t) ->
      if n <= 0 then report_error ("Array has nonpositive dimensions") typ.p_type_FO_loc;
      construct_ATarray n (translate_type t)
    | PTmapping (t1,t2) ->
       construct_ATmapping (translate_type t1) (translate_type t2)
    | PTlist t -> construct_ATlist (translate_type t)
    | _ -> report_error ("Internal error: data type in non-definition") typ.p_type_FO_loc; tint_Z32 in

  let translate_constructor i c =
    let arg_type_env = Hashtbl.create (List.length c.pTypeConstrArgs) in
    let counter = ref 0 in
    let args' = List.map (fun (f, t) ->
        let f' = if f = "_" then (incr counter; "fld" ^ string_of_int !counter)
                            else f in
        let t' = translate_type t
        in if t'.aTypeProvable && f <> "_"
             then Hashtbl.add arg_type_env f t'.aTypeCtype;
           f', t')
      c.pTypeConstrArgs in
    let ct = parse_constr_annotations arg_type_env c.pTypeConstrAnnotations
    in { aTypeConstrName = c.pTypeConstrName;
         aTypeConstrArgs = args';
         aTypeConstrImpl = ct } in

  let translate_type_def i typ = match typ.p_type_FO_desc with
    | PTdata (PTsingleton [], a) ->
      report_error ("Structure type " ^ i ^ " has no fields") typ.p_type_FO_loc;
      dummy_type i

    | PTdata (PTsingleton lst, a) ->
      let cname, require_impl =
        match parse_type_annotations ctype_environment a with
        | Some (ACtstruct (cname, _)) -> cname, true
        | Some _ -> report_warning ("Structure type " ^ i ^
                      " can only have structure C type, default to type name") typ.p_type_FO_loc;
                    i, false
        | _ -> i, false in

      let flds, struct_lst = List.fold_right (fun (f, t) (flds, struct_lst) ->
        let t' = translate_type t in
        (f, t') :: flds, match struct_lst, t'.aTypeProvable with
          | `Valid_struct lst, true -> `Valid_struct ((f, t'.aTypeCtype) :: lst)
          | `Valid_struct lst, false -> `Missing_ctype [f, t']
          | `Missing_ctype lst, true -> struct_lst
          | `Missing_ctype lst, false -> `Missing_ctype ((f, t') :: lst)
        ) lst ([], `Valid_struct []) in

      let impl, ct, cname', provable = match struct_lst with
        | `Valid_struct lst ->
          let ct = ACtstruct (cname, lst) in
          Some { aImplDesc = ACstruct (List.map (fun (f, ct) ->
                               f, { aImplDesc = ACvar f; aImplType = ct }) lst);
                 aImplType = ct },
          ct, cname, true
        | `Missing_ctype lst ->
          if require_impl then
             report_error (error_string_struct_unimplementable i cname lst) typ.p_type_FO_loc;
          None, ACtint, "int", false in
      let constr = { aTypeConstrName = "Build_" ^ i;
                     aTypeConstrArgs = flds;
                     aTypeConstrImpl = impl } in

      let t = { aTypeDesc = ATdata (i, ATsingleton constr);
                aTypeCtype = ct;
                aTypePairIdent = "t" ^ cname' ^ "_" ^ i;
                aTypeProvable = provable
              }
      in List.iter (fun (f, _) -> match
                                    Hashtbl.find_opt struct_field_environment f with
           | Some t' -> (report_error ("Structure field " ^ f ^
                " for type " ^ i ^ " is already that of type " ^
                match t'.aTypeDesc with ATdata (i', _) -> i' | _ -> "UNKNOWN") typ.p_type_FO_loc)
           | None -> Hashtbl.add struct_field_environment f t
         ) flds;
         t

    | PTdata (PTbranches [], _) ->
      report_error ("Data type " ^ i ^ " has no constructors") typ.p_type_FO_loc;
      dummy_type i

(*    | PTdata (PTbranches [PTbuiltin t], _a) ->
       translate_type (PTbuiltin t) *)

    | PTdata (PTbranches lst, a) ->
      let lst' = List.map (translate_constructor i) lst in
      let t, lst'' = match parse_type_annotations ctype_environment a with
        | None ->
          { aTypeDesc = ATdata (i, ATbranches lst'); aTypeCtype = ACtint;
            aTypePairIdent = "tint_" ^ i; aTypeProvable = false }, lst'
        | Some ct ->
          let lst'', ct' = List.fold_right (fun c (lst', ct) ->
            match c.aTypeConstrImpl with
            | None ->
              (report_error ("Data type " ^ i ^ " annotated with " ^
                "implementation type but constructor " ^ c.aTypeConstrName ^
                " cannot be implemented") (typ.p_type_FO_loc));
              c :: lst', ct
            | Some impl -> match cimpl_respects_ctype impl ct with
              | None ->
                (report_error ("Data constructor " ^ c.aTypeConstrName ^ " is " ^
                  "annotated with implement of type `" ^
                  string_of_a_ctype impl.aImplType ^ "' but type " ^ i ^
                  " requires `" ^ string_of_a_ctype ct ^ "'") (typ.p_type_FO_loc));
                c :: lst', ct
              | Some impl' ->
                { c with aTypeConstrImpl = Some impl' } :: lst', impl'.aImplType
          ) lst' ([], ct)
          in { aTypeDesc = ATdata (i, ATbranches lst''); aTypeCtype = ct';
               aTypePairIdent = "t" ^ type_pair_ident_ctype ct' ^ "_" ^ i;
               aTypeProvable = true },
             lst''
      in List.iter (fun c -> match 
             Hashtbl.find_opt type_constr_environment c.aTypeConstrName with
           | Some (t', _) -> (report_error ("Constructor " ^ c.aTypeConstrName ^
                " for type " ^ i ^ " is already that of type " ^
                match t'.aTypeDesc with ATdata (i', _) -> i' | _ -> "UNKNOWN") typ.p_type_FO_loc)
           | None -> Hashtbl.add type_constr_environment c.aTypeConstrName (t, c)
         ) lst'';
         t

    | t ->
       translate_type {p_type_FO_desc = t; p_type_FO_loc = typ.p_type_FO_loc} in


  let translate_event_def i args =
    let args' = List.map (fun (f, t, b) ->
                           let t' = translate_type t in
                           (if not t'.aTypeProvable then
                              report_error ("The type of the argument " ^ f ^ " of event " ^ i ^ " can not be represented at runtime.") none);
                           (f, t', b))
                         args in
    { aEventName = i;
      aEventArgs = args' } in

  let unfold_arg_type =
    let rec f acc = function
      | { p_type_FO_desc = PTprod (res, t); p_type_FO_loc = loc }->
        f (translate_type t :: acc) res
      | {p_type_FO_desc= t; p_type_FO_loc= l} -> translate_type {p_type_FO_desc = t; p_type_FO_loc= l  } :: acc
    in f [] in

  let rec translate_signature i s = match s.p_signature_desc with
    | PSconstr lst ->
      if (List.exists (fun (f, arg, ret, kind) -> kind == MKconstructor) lst) then
          { aSignatureName = i;
            aSignatureMethods = List.map (fun (f, arg, ret, kind) -> 
                (if kind == MKconstructor && ret.p_type_FO_desc <> (PTbuiltin Tunit) then (report_error (f ^ "is defined as constructor. " ^ "The return type should be unit") ret.p_type_FO_loc));
                f, { aMethodArgumentTypes = unfold_arg_type arg ;
                     aMethodReturnType = translate_type ret;
                     aMethodKind = kind }
              ) lst;
            aSignatureSuppressError = false
          }
      else
          { aSignatureName = i;          
            aSignatureMethods = (List.append (List.map (fun (f, arg, ret, kind) -> 
                (if kind == MKconstructor && ret.p_type_FO_desc <> (PTbuiltin Tunit) then (report_error (f ^ "is defined as constructor. " ^ "The return type should be unit") ret.p_type_FO_loc));
                f, { aMethodArgumentTypes = unfold_arg_type arg ;
                     aMethodReturnType = translate_type ret;
                     aMethodKind = kind }
              ) lst) default_constructor_signatureMethod);
            aSignatureSuppressError = false
          }
    | PSghostize (sg, idents) ->
      let sg' = translate_signature (i ^ "_ghosting") sg in
      let mask = Hashtbl.create 10 in
      let _ = List.iter (fun i -> Hashtbl.add mask i true) idents in
      let suppress_error = ref sg'.aSignatureSuppressError in
      let methods = List.map (fun (name, m) -> name,
          if Hashtbl.mem mask name then
            match m.aMethodKind with
            | MKnormal -> (report_error (name ^ " is a normal method in " ^
                sg'.aSignatureName ^ " and cannot be changed to ghost") sg.p_signature_loc);
              suppress_error := true;
              m
            | MKlogical -> (report_error (name ^ " is a logical method in " ^
                sg'.aSignatureName ^ " and cannot be changed to ghost") sg.p_signature_loc);
              suppress_error := true;
              m
            | _ -> let _ = Hashtbl.remove mask name in
                   if m.aMethodKind = MKconst
                     then { m with aMethodKind = MKconstghost }
                     else m
          else m
        ) sg'.aSignatureMethods
      in Hashtbl.iter (fun m _ ->
             (report_warning ("Attempt to ghostize " ^ m ^ " not in " ^
               sg'.aSignatureName) s.p_signature_loc)
           ) mask;
         { aSignatureName = i;
           aSignatureMethods = methods;
           aSignatureSuppressError = !suppress_error }
    | PSlogicize (sg, idents) ->
      let sg' = translate_signature (i ^ "_logicizing") sg in
      let mask = Hashtbl.create 10 in
      let _ = List.iter (fun i -> Hashtbl.add mask i true) idents in
      let methods = List.map (fun (name, m) -> name,
          if Hashtbl.mem mask name
            then let _ = Hashtbl.remove mask name
                 in { m with aMethodKind = MKlogical }
            else m
        ) sg'.aSignatureMethods
      in Hashtbl.iter (fun m _ ->
             (report_warning ("Attempt to logicize " ^ m ^ " not in " ^
               sg'.aSignatureName) s.p_signature_loc)
           ) mask;
         { aSignatureName = i;
           aSignatureMethods = methods;
           aSignatureSuppressError = sg'.aSignatureSuppressError }
    | PSminus (sg, idents) ->
      let sg' = translate_signature (i ^ "_minusing") sg in
      let mask = Hashtbl.create 10 in
      let _ = List.iter (fun i -> Hashtbl.add mask i true) idents in
      let methods = List.concat @@ List.map (fun (name, m) ->
          if Hashtbl.mem mask name
            then let _ = Hashtbl.remove mask name
                 in []
            else [name, m]
        ) sg'.aSignatureMethods
      in Hashtbl.iter (fun m _ ->
             (report_warning ("Attempt to remove " ^ m ^ " not in " ^
               sg'.aSignatureName) s.p_signature_loc)
           ) mask;
         { aSignatureName = i;
           aSignatureMethods = methods;
           aSignatureSuppressError = sg'.aSignatureSuppressError }
    | PSname i' ->
      try Hashtbl.find signature_environment i'
      with Not_found ->
        report_error ("Signature type " ^ i' ^ " not defined") s.p_signature_loc;
        dummy_signature i in

  let translate_layer_signature i plsig = match plsig.p_layer_signature_desc with
    | PLSconstr lst -> {
        aLayerSignatureName = i;
        aLayerSignatureSlots =
          List.map (fun (s, sg) -> s, translate_signature (i ^ "_" ^ s) sg) lst;
        aLayerSignatureSuppressError = false
      }
    | PLSname i' ->
      try Hashtbl.find layer_signature_environment i'
      with Not_found ->
        (report_error ("Layer signature " ^ i' ^ " not defined") plsig.p_layer_signature_loc);
        dummy_layer_signature i in

  let translate_object_type i t =
    { aObjectBase = translate_layer_signature (i ^ "_base") t.pObjectBase;
      aObjectSignature = translate_signature (i ^ "_sig") t.pObjectSignature } in

  let translate_layer_type i t =
    { aLayerBase = translate_layer_signature (i ^ "_base") t.pLayerBase;
      aLayerSignature = translate_layer_signature (i ^ "_ifc") t.pLayerSignature } in

  let rec translate_expression var_env tmp_env exp = match exp.p_expression_desc with
    | PEglob "keccak256" ->
         (report_error ("keccak256 requires 1 or 2 arguments, but were given 0.") exp.p_expression_loc);
         `RExpr dummy_rexpr
    | PEglob i -> begin
      try let tmp_id, t = List.assoc i tmp_env
          in `RExpr { aRexprDesc = AEtemp (tmp_id, i); aRexprType = t }
      with Not_found ->
      try `LExpr (Hashtbl.find var_env i) with Not_found ->
      try `BigExpr (Hashtbl.find external_const_environment i) with Not_found ->
      match Hashtbl.find_opt ethbuiltins_environment i with
      | Some ([], v) -> `RExpr v
      | Some (argtypes, v) ->
         (report_error ("Ethereum builtin " ^ i ^ " requires " ^ string_of_int (List.length argtypes) ^ "arguments, but were given 0.") exp.p_expression_loc);
         `RExpr dummy_rexpr
      | None ->
         match catch_not_found (Hashtbl.find type_constr_environment) i with
         | None -> (print_endline (string_of_p_expression_desc exp.p_expression_desc)); report_error ("Unknown identifier `" ^ i ^ "'") exp.p_expression_loc;
                   `RExpr dummy_rexpr
         | Some (t, c) ->
            if c.aTypeConstrArgs <> [] then
              (report_error ("Constructor " ^ c.aTypeConstrName ^ " expects " ^
                              string_of_int (List.length c.aTypeConstrArgs) ^
                                " arguments but none supplied" ) exp.p_expression_loc);
            if t.aTypeProvable && not (ctype_is_small t.aTypeCtype)
            then `BigExpr { aBigExprDesc = AEconstr (c, []); aBigExprType = t }
            else `RExpr { aRexprDesc = AEconstr_val (c, []); aRexprType = t }
      end

    | PEconstant c ->
      begin 
      match c with
      | CONaddress addr -> 
          let c' = 
          if String.length addr > 2 then
          begin
          let num_prefix =  String.sub addr 0 2 in
            match num_prefix with 
            | "0x" -> if String.length addr > address_length * 2 + 2 then None else Some c
            | "0o" -> if String.length addr > address_length * 4 + 2 then None else Some c
            | "0b" -> if String.length addr > address_length * 8 + 2 then None else Some c
            (* decimal integer overflow will not pass to this step *)
            | _ -> 
            let binary_string = Backend.BinNumsExt.decimalstring2binarystring addr in
              if String.length binary_string > address_length * 8 + 2 then None else Some c 
          end
          else Some c
          in
          begin
          match c' with
          | Some c -> `RExpr { aRexprDesc = AEconst c; aRexprType = constant_type c }
          | None -> report_error ("Identity constant overflow: address(" ^ addr ^ ")" ) exp.p_expression_loc;
            `RExpr { aRexprDesc = AEconst c; aRexprType = constant_type c }
          end
      | _ ->
      `RExpr { aRexprDesc = AEconst c; aRexprType = constant_type c }
      end
    | PEun (op, e) ->
      let e' = translate_rexpr var_env tmp_env e in
      begin match unop_type op e'.aRexprType with
      | Some t -> `RExpr { aRexprDesc = AEunop (op, e'); aRexprType = t }
      | None ->
        report_error ("Unary operator " ^ string_of_unop op ^
          " not defined on type " ^ (string_of_a_type false e'.aRexprType)) exp.p_expression_loc;
        `RExpr { aRexprDesc = AEunop (op, e'); aRexprType = dummy_type "*UNOP*" }
      end

    | PEbin (op, e1, e2) ->
      let e1' = translate_rexpr var_env tmp_env e1 in
      let e2' = translate_rexpr var_env tmp_env e2 in
      begin match binop_type op e1'.aRexprType e2'.aRexprType with
      | Some t -> `RExpr { aRexprDesc = AEbinop (op, e1', e2'); aRexprType = t }
      | None ->
        report_error ("Binary operator " ^ string_of_binop op ^
          " not defined on type " ^ string_of_a_type false e1'.aRexprType ^
          " and "^ (string_of_a_type false e2'.aRexprType)) exp.p_expression_loc ;
        `RExpr { aRexprDesc = AEbinop (op, e1', e2');
                 aRexprType = dummy_type "*BINOP*" }
      end

    | PEpair (e1, e2) ->
      let e1' = translate_big_expr var_env tmp_env e1 in
      let e2' = translate_big_expr var_env tmp_env e2 in
      let t1 = e1'.aBigExprType in
      let t2 = e2'.aBigExprType in
      let t = construct_ATprod t1 t2 in
      let constr = {
        aTypeConstrName = "pair";
        aTypeConstrArgs = ["fst", t1; "snd", t2];
        aTypeConstrImpl = if t.aTypeProvable then
          Some {
            aImplDesc = ACstruct
              ["fst", { aImplDesc = ACvar "fst"; aImplType = t1.aTypeCtype };
               "snd", { aImplDesc = ACvar "snd"; aImplType = t2.aTypeCtype }];
            aImplType = t.aTypeCtype }
        else
          None
      }
      in `BigExpr {
        aBigExprDesc = AEconstr (constr, ["fst", e1'; "snd", e2']);
        aBigExprType = t
      }

    | PEapp ({p_expression_desc = (PEglob "keccak256"); p_expression_loc = _ }, [{p_expression_desc = PEpair (e1, e2); p_expression_loc = _ }]) ->
      let e1' = translate_rexpr var_env tmp_env e1 in
      let e2' = translate_rexpr var_env tmp_env e2 in
      begin match binop_type OPsha_2 e1'.aRexprType e2'.aRexprType with
      | Some t -> `RExpr { aRexprDesc = AEbinop (OPsha_2, e1', e2'); aRexprType = t }
      | None ->
         report_error ("Unary operator keccak256 not defined on types "
                       ^ string_of_a_type false e1'.aRexprType ^ " and " ^ string_of_a_type false e2'.aRexprType) exp.p_expression_loc; 
        `RExpr { aRexprDesc = AEbinop (OPsha_2, e1', e2'); aRexprType = dummy_type "*BINOP*" }
      end

    | PEapp ({p_expression_desc = (PEglob "keccak256"); p_expression_loc = _}, [e]) ->
      let e' = translate_rexpr var_env tmp_env e in
      begin match unop_type OPsha_1 e'.aRexprType with
      | Some t -> `RExpr { aRexprDesc = AEunop (OPsha_1, e'); aRexprType = t }
      | None ->
         report_error ("Unary operator keccak256 not defined on type "
                       ^ string_of_a_type false e'.aRexprType) exp.p_expression_loc;
        `RExpr { aRexprDesc = AEunop (OPsha_1, e'); aRexprType = dummy_type "*UNOP*" }
      end

    | PEapp ({p_expression_desc = (PEglob i); p_expression_loc = _}, es) when i = "fst" || i = "snd" -> begin
      match es with
      | [] | _ :: _ :: _ ->
        report_error (i ^ " only takes one argument; " ^
                      string_of_int (List.length es) ^ " given") exp.p_expression_loc;
        `LExpr dummy_lexpr
      | [e] ->
        let e' = translate_lexpr var_env tmp_env e in
        match e'.aLexprType.aTypeDesc with
        | ATprod (tfst, tsnd) ->
          `LExpr { aLexprDesc = AEfield (e', i);
                   aLexprType = if i = "fst" then tfst else tsnd;
                   aLexprIsGhost = e'.aLexprIsGhost }
        | _ ->
          report_error (i ^ " can only apply to a product type; " ^
                        string_of_a_type false e'.aLexprType ^ " given")  exp.p_expression_loc;
          `LExpr dummy_lexpr
      end

    (* TODO: *)
    | PEapp ({p_expression_desc = (PEglob i); p_expression_loc = _}, es) when i = "array_sets" -> begin
        let ct = begin match es with
        | e::_ -> 
          let e' = translate_big_expr var_env tmp_env e in
          e'.aBigExprType
        | [] -> {
            aTypeDesc = ATarray(-1, tvoid_unit);
            aTypeCtype = ACtarray(-1, ACtvoid);
            aTypePairIdent = "";
            aTypeProvable = true }
        end in
        let c = {
            aTypeConstrName = "array_sets";
            aTypeConstrArgs = ["_", ct];
            aTypeConstrImpl = None } in
        let t = {
            aTypeDesc = ATarray(-1, tvoid_unit);
            aTypeCtype = ACtarray(-1, ACtvoid);
            aTypePairIdent = "";
            aTypeProvable = true } in
        let params = List.map (fun e ->
          let e' = translate_big_expr var_env tmp_env e in
          "", e') es
        in `BigExpr { aBigExprDesc = AEconstr (c, params); aBigExprType = t }
    end

    | PEapp ({p_expression_desc = (PEglob i); p_expression_loc = _}, es) ->
       begin
       match Hashtbl.find_opt ethbuiltins_environment i with
       | Some (argtypes, {aRexprDesc=AEbuiltin (bi,_); aRexprType=rt})  ->
          let params = try List.map2 (fun et e ->
                               let e' = translate_rexpr var_env tmp_env e in
                               match rexpr_respects_type e' et with
                               | None ->
                                  report_error ("Ethereum builtin " ^ i ^ " requires an argument of type " ^ string_of_a_type false et  ^ " but supplied " ^
              "expression of type " ^ string_of_a_type false e'.aRexprType) exp.p_expression_loc;
                                  dummy_rexpr
                               | Some e'' -> e'') argtypes es
                     with Invalid_argument _ (*"List.map2"*) ->
            report_error ("Ethereum builint " ^ i ^ " expects " ^
              string_of_int (List.length argtypes) ^
              " arguments but " ^ string_of_int (List.length es) ^ " supplied") exp.p_expression_loc;
            []
          in
           `RExpr {aRexprDesc=AEbuiltin (bi,params); aRexprType=rt}          
       | Some (_, _) ->
          report_error ("internal error, not a AEbuiltin") exp.p_expression_loc;
         `RExpr dummy_rexpr

       | None ->
       begin match Hashtbl.find_opt type_constr_environment i with
        (* We can make this a store even for non-provable big types in order
           to compile ghost expressions.

           TODO: It would make sense to actually check if we are in a ghost context.
           One way to do so would be to use the side_effect system, and classify
           constructing constructors of non-provable datatypes as a dependsOnAbstraction effect. *)
      | Some (t, c) when t.aTypeProvable && not (ctype_is_small t.aTypeCtype) ->
        let params = try List.map2 (fun (f, ft) e ->
          let e' = translate_big_expr var_env tmp_env e in
          match big_expr_respects_type e' ft with
          | None ->
            report_error ("Field " ^ f ^ " of constructor " ^ c.aTypeConstrName ^
              " has type " ^ string_of_a_type false ft ^ " but supplied " ^
              "expression of type " ^ string_of_a_type false e'.aBigExprType)  exp.p_expression_loc;
            f, e'
          | Some e'' -> f, e'') c.aTypeConstrArgs es
                     with Invalid_argument _ (*"List.map2"*) ->
            report_error ("Constructor " ^ c.aTypeConstrName ^ " expects " ^
              string_of_int (List.length c.aTypeConstrArgs) ^
              " arguments but " ^ string_of_int (List.length es) ^ " supplied") exp.p_expression_loc;
            []
        in `BigExpr { aBigExprDesc = AEconstr (c, params); aBigExprType = t }
      | Some (t, c) ->
        let params = try List.map2 (fun (f, ft) e ->
          let e' = translate_rexpr var_env tmp_env e in
          match rexpr_respects_type e' ft with
          | None ->
            report_error ("Field " ^ f ^ " of constructor " ^ c.aTypeConstrName ^
              " has type " ^ string_of_a_type false ft ^ " but supplied " ^
              "expression of type " ^ string_of_a_type false e'.aRexprType) exp.p_expression_loc;
            f, e'
          | Some e'' -> f, e'') c.aTypeConstrArgs es
          with Invalid_argument _ (*"List.map2"*) ->
            report_error ("Constructor " ^ c.aTypeConstrName ^ " expects " ^
              string_of_int (List.length c.aTypeConstrArgs) ^
              " arguments but " ^ string_of_int (List.length es) ^ " supplied") exp.p_expression_loc;
            []
        in `RExpr { aRexprDesc = AEconstr_val (c, params); aRexprType = t }

      (*
      | None -> match catch_not_found (Hashtbl.find external_function_environment) i with
      | Some (s, m) ->
        let rec unfold_arg e = function
          | [] -> report_error "Internal error: argument list empty"; []
          | [t] -> [translate_big_expr_typed var_env tmp_env e t ("External call" ^ i)]
          | t :: tres -> match e with
            | PEpair (e', eres) ->
              translate_big_expr_typed var_env tmp_env e' t ("External call" ^ i)
                :: unfold_arg eres tres
            | _ -> report_error "Insuffecient number of argument supplied"; []
        in begin match es with
        | [] -> report_error "Internal error: no argument supplied";
                `BigExpr dummy_big_expr
        | [e] ->
          `BigExpr { aBigExprDesc = AEexternal_call (s, i,
                                      unfold_arg e m.aMethodArgumentTypes);
                     aBigExprType = m.aMethodReturnType }
        | _ -> report_warning ("More than one (currying) argument for " ^
                 "external function call " ^ i ^ "(" ^ s ^"): ignored");
               `BigExpr dummy_big_expr
        end
      *)

      | None -> report_error ("Unknown identifier `" ^ i ^ "'")  exp.p_expression_loc;
                `RExpr dummy_rexpr
       end
       end

    | PEapp _ -> report_error ("Application of complex head illegally appears in expressions")  exp.p_expression_loc;
                 `RExpr dummy_rexpr

    | PEstruct [] -> report_error ("Empty structure construction is not allowed")  exp.p_expression_loc;
                     `BigExpr dummy_big_expr
    | PEstruct (((f1, _) :: _) as lst) -> begin match 
        Hashtbl.find_opt struct_field_environment f1 with
      | Some ({ aTypeDesc = ATdata (i, ATsingleton c) } as t) ->
        List.iter (fun (i', _) ->
          if not (List.mem_assoc i' c.aTypeConstrArgs) then
            (report_error ("Type " ^ i ^ " does not contain field " ^ i') exp.p_expression_loc)
        ) lst;
        let lst' = List.map (fun (f, ft) ->
          match catch_not_found (List.assoc f) lst with
          | None -> report_error ("Field " ^ f ^ " not initialized when " ^
                      "constructing structure type " ^ i) exp.p_expression_loc;
                    f, dummy_big_expr
          | Some e' ->
            let e'' = translate_big_expr var_env tmp_env e' in
            match big_expr_respects_type e'' ft with
            | None ->
              report_error ("Field " ^ f ^ " of structure type " ^ i ^ " has type " ^
                string_of_a_type false ft ^ " but supplied expression of type " ^
                string_of_a_type false e''.aBigExprType) exp.p_expression_loc;
              f, e''
            | Some e''' -> f, e'''
          ) c.aTypeConstrArgs
        in `BigExpr { aBigExprDesc = AEstruct (c, lst'); aBigExprType = t }
      | Some t -> report_error ("Internal error: constructor " ^ f1 ^
                    " mapped to non-structure type " ^ string_of_a_type false t) exp.p_expression_loc;
                  `BigExpr dummy_big_expr
      | None -> report_error ("Unknown structure field name " ^ f1) exp.p_expression_loc;
                `BigExpr dummy_big_expr
      end

    | PEfield (e, f) ->
      let e' = translate_lexpr var_env tmp_env e in
      begin match e'.aLexprType.aTypeDesc with
      | ATdata (i, ATsingleton c) ->
        begin match catch_not_found (List.assoc f) c.aTypeConstrArgs with
        | None ->
          report_error ("Data type " ^ i ^ " does not have the field " ^ f) exp.p_expression_loc;
          `LExpr dummy_lexpr
        | Some t ->
          `LExpr { aLexprDesc = AEfield (e', f);
                   aLexprType = t;
                   aLexprIsGhost = e'.aLexprIsGhost }
        end
      | _ -> report_error (string_of_a_type false e'.aLexprType ^ " is not a" ^
               " structure type; only structure types can be accessed through fields") exp.p_expression_loc;
             `LExpr dummy_lexpr
      end

    | PEindex (e, idx) ->
      let e' = translate_lexpr var_env tmp_env e in
      let idx' = translate_rexpr var_env tmp_env idx in
      begin match e'.aLexprType.aTypeDesc, idx'.aRexprType.aTypeDesc with
      | ATarray (n, t), ATbuiltin Tint ->
        `LExpr { aLexprDesc = AEindex (e', idx');
                 aLexprType = t;
                 aLexprIsGhost = e'.aLexprIsGhost }
      | ATarray _, _ ->
        report_error (string_of_a_type false idx'.aRexprType ^ " is not integer;" ^
          " arrays can only be indexed by integers") exp.p_expression_loc;
        `LExpr dummy_lexpr
      | ATmapping (tkey, tval), t ->
	 if tkey.aTypeDesc <> t then 
	   begin report_error ("The index expression has type " ^ (string_of_a_type false idx'.aRexprType)
			  ^ " but should have type " ^ (string_of_a_type false tkey) ^ ".");
		 `LExpr dummy_lexpr end
	 else
	   `LExpr { aLexprDesc = AEindex (e', idx');
		    aLexprType = tval;
        aLexprIsGhost = e'.aLexprIsGhost }                   
      | _ -> report_error (string_of_a_type false e'.aLexprType ^ " is not an" ^
               " array type; only array types can be accessed through indexing") exp.p_expression_loc;
             `LExpr dummy_lexpr
      end

  and translate_rexpr var_env tmp_env e =
    match translate_expression var_env tmp_env e with
    | `RExpr e' -> e'
    | `LExpr e' ->
      report_error ("Normal expression expected but found an L-value") e.p_expression_loc ;
      { dummy_rexpr with aRexprType = e'.aLexprType }
    | `BigExpr e' ->
       report_error ("Normal expression expected but found a data constructor") e.p_expression_loc;
      { dummy_rexpr with aRexprType = e'.aBigExprType }
  and translate_lexpr var_env tmp_env e =
    match translate_expression var_env tmp_env e with
    | `RExpr e' ->
      report_error ("L-value expression expected but found an R-value") e.p_expression_loc;
      { dummy_lexpr with aLexprType = e'.aRexprType }
    | `LExpr e' -> e'
    | `BigExpr e' ->
      report_error ("L-value expression expected but found a data constructor")  e.p_expression_loc;
      { dummy_lexpr with aLexprType = e'.aBigExprType }
  and translate_big_expr var_env tmp_env e =
    match translate_expression var_env tmp_env e with
    | `RExpr e' -> { aBigExprDesc = AErexpr e'; aBigExprType = e'.aRexprType }
    | `LExpr e' ->
      report_error ("Data constructor expected but found an L-value")  e.p_expression_loc;
      { dummy_big_expr with aBigExprType = e'.aLexprType }
    | `BigExpr e' -> e' in

  let translate_object_field i logical_object (f, t, e, is_ghost) =
    let t' = translate_type t in
    let e' = translate_big_expr (empty_var_env ()) (empty_tmp_env ()) e in
    let e'' =
        let is_array_sets e =
            match e.aBigExprDesc with
            | AEconstr (ty, el) -> 0 = String.compare ty.aTypeConstrName "array_sets"
            | _ -> false
        in
        if is_array_sets e' then e'
        else
        match big_expr_respects_type e' t' with
      | None ->
        report_error ("Object field " ^ i ^ "." ^ f ^ " has type " ^
          string_of_a_type false t' ^ " but initialized to a value of type " ^
          string_of_a_type false e'.aBigExprType)  e.p_expression_loc;
        e'
      | Some e'' -> e'' in
    match expression_to_compile_time_constant e'' with
    | Some e3 -> { aObjectFieldName = f; aObjectFieldType = t';
                   aObjectFieldInitial = e3;
                   aObjectFieldIsLogical = logical_object || is_ghost }
    | None -> report_error ("Object field " ^ i ^ "." ^ f ^
                            " initialized with non-constant")  e.p_expression_loc;
      { aObjectFieldName = f; aObjectFieldType = t';
        aObjectFieldInitial = ACONconst CONunit;
        aObjectFieldIsLogical = logical_object || is_ghost } in

  let translate_object_field_to_Constructor i logical_object (ident, t, e, is_ghost) =
    let lexpr_list = match e.p_expression_desc with
                        | PEstruct lst ->
                            List.map (fun (id, pe) ->
                                (let le = {
                                          aLexprDesc = AEglob(ident);
                                          aLexprType = translate_type t;
                                          aLexprIsGhost = is_ghost
                                        } in 
                                  {aLexprDesc = AEfield(le, id); aLexprType = translate_type t; aLexprIsGhost = is_ghost; })) lst
                        | PEglob("array_init") -> []
                        | PEconstant(CONhashvalue) -> []
                        | PEglob("mapping_init") -> []
                        | _ -> [{aLexprDesc = AEglob(ident); aLexprType = translate_type t; aLexprIsGhost = is_ghost; }]
                      in
    let rexpr_list = match e.p_expression_desc with
                        | PEstruct lst -> 
                            List.map (fun (id, pe) -> (translate_rexpr (empty_var_env ()) (empty_tmp_env ()) pe)) lst
                        | PEglob("array_init") -> []
                        | PEconstant(CONhashvalue) -> []
                        | PEglob("mapping_init") -> []  
                        | _ ->
                            let e_toConstructor = translate_rexpr (empty_var_env ()) (empty_tmp_env ()) e in
                                        [e_toConstructor] 
              in
        let fields_to_commands = 
            if (List.length(lexpr_list) > 1) then
                (List.map2 (fun lexpr rexpr -> 
                            {
                              aCmdDesc = ACstore(lexpr, rexpr);
                              aCmdType = translate_type t;
                              aCmdEnv = [] ; 
                              aCmdEffect = side_effect_pure;
                            } 
                   ) lexpr_list rexpr_list)
            else if (List.length(lexpr_list) == 0) then begin
                [{
                  aCmdDesc = ACskip;
                  aCmdType = translate_type t;
                  aCmdEnv = [] ; 
                  aCmdEffect = side_effect_pure;
                }]
              end
            else
                [{
                  aCmdDesc = ACstore((List.hd lexpr_list), (List.hd rexpr_list));
                  aCmdType = translate_type t;
                  aCmdEnv = [] ; 
                  aCmdEffect = side_effect_pure;
                }]
              in
        if List.length(fields_to_commands) = 1 then
            (List.hd fields_to_commands)
        else
            (commandList_helper fields_to_commands (List.hd fields_to_commands).aCmdType [] side_effect_pure)
      in

  let translate_rexpr_typed var_env tmp_env e t msg =
    let e' = translate_rexpr var_env tmp_env e in
    match rexpr_respects_type e' t with
    | None ->
      report_error (msg ^ ": expression of type " ^ string_of_a_type false t ^
        " required; " ^ string_of_a_type false e'.aRexprType ^ " given")  e.p_expression_loc;
      e'
    | Some e'' -> e'' in

  let rec unfold_call_arg var_env tmp_env e = function
    | [] -> report_error ("Internal error: argument list empty")  e.p_expression_loc; []
    | [t] -> [translate_rexpr_typed var_env tmp_env e t "Method call"]
    | t :: tres -> match e with
      | {p_expression_desc = PEpair (e', eres); p_expression_loc = _} ->
        translate_rexpr_typed var_env tmp_env e' t "Method call"
          :: unfold_call_arg var_env tmp_env eres tres
      | _ -> report_error ("Insufficient number of argument supplied") e.p_expression_loc; [] in

  let rec unfold_event_arg var_env tmp_env e = function
    | [] -> []
    | [_,t,_] -> [translate_rexpr_typed var_env tmp_env e t "Event call"]
    | (_,t,_) :: tres -> match e with
      | {p_expression_desc = PEpair (e', eres); p_expression_loc = _} ->
        (translate_rexpr_typed var_env tmp_env e' t "Event call")
          :: unfold_event_arg var_env tmp_env eres tres
      | _ -> report_error ("Insufficient number of argument supplied") e.p_expression_loc; [] in


  let unfold_extcall_arg var_env tmp_env =
    let trans_expr e t = match translate_expression var_env tmp_env e with
      | `RExpr e' -> AEXrexer (match rexpr_respects_type e' t with
        | None ->
          report_error ("External call: expression of type " ^ string_of_a_type false t ^
            " required; " ^ string_of_a_type false e'.aRexprType ^ " given") e.p_expression_loc;
          e'
        | Some e'' -> e'')
      | `LExpr e' ->
        if e'.aLexprType <> t then
          report_error ("External call: expression of type " ^ string_of_a_type false t ^
            " required; " ^ string_of_a_type false e'.aLexprType ^ " given") e.p_expression_loc;
        AEXlexpr e'
      | `BigExpr _ ->
        report_error ("External function call cannot take data constructors") e.p_expression_loc;
        AEXrexer { dummy_rexpr with aRexprType = t } in
    let rec f e = function
      | [] -> report_error ("Internal error: argument list empty") e.p_expression_loc; []
      | [t] -> [trans_expr e t]
      | t :: tres -> match e with
        | {p_expression_desc = PEpair (e', eres); p_expression_loc = _} ->
          trans_expr e' t :: f eres tres
        | _ -> report_error ("Insuffecient number of argument supplied")  e.p_expression_loc; []
    in f in

  let parse_captured_command_annotations tmp_env = function
    | PAclause (name, tmps) :: _ ->
      let captured_tmps = List.rev @@ List.fold_left (fun res a -> match a with
          | PAclause (tmp, _) ->
            begin match catch_not_found (List.assoc tmp) tmp_env with
            | Some info -> (tmp, info) :: res
            | None ->
              report_warning ("Capturing unknown variable `" ^ tmp ^ "' is ignored" ) none;
              res
            end
          | _ -> res
        ) [] tmps
      in Some { aCapturedName = name; aCapturedTemp = captured_tmps }
    | _ -> None in

  let rec translate_command mk env expect_bool next_id tmp_env cmd  =
    let return next_id' t eff d =
      { aCmdDesc = d; aCmdType = t; aCmdEnv = tmp_env; aCmdEffect = eff },
      next_id'
    in match cmd.p_command_desc with
      (* | PCskip -> ACskip, tvoid_unit, next_id *)
      | PCyield ({p_expression_desc = PEapp ({p_expression_desc = (PEfield ({p_expression_desc = (PEglob s); p_expression_loc = _}, m)); p_expression_loc = _ }, arg :: rest); p_expression_loc = _ }) ->
        if rest <> [] then
          report_warning ("More than one (currying) argument for primitive call " ^
            s ^ "." ^ m ^ ": ignored") cmd.p_command_loc;
        begin match find_method env s m with  
        | `Not_found msg ->
          report_error msg cmd.p_command_loc; return next_id tvoid_unit side_effect_pure ACskip
        | `Found mt ->
          if mk = MKconstructor then
              begin
              report_error ("Method call in constructor is not working in current version of compiler") cmd.p_command_loc;
              end;
          return next_id mt.aMethodReturnType (method_side_effect mt) @@
            ACcall (s, m, unfold_call_arg env.variable_env tmp_env
                                               arg mt.aMethodArgumentTypes)
        end

      | PCyield ({p_expression_desc = (PEglob v); p_expression_loc = _})
      | PCstore (_, ({p_expression_desc = (PEglob v); p_expression_loc = _}))
          when Hashtbl.mem external_const_environment v ->
        let s, i, t = match Hashtbl.find external_const_environment v with
          | { aBigExprDesc = AEexternal_const (s, i); aBigExprType = t } ->
            s, i, t
          | _ -> assert false in
        let dest, res_t, eff = match cmd.p_command_desc with
          | PCstore (el, _) ->
            let el' = translate_lexpr env.variable_env tmp_env el
            (*XXX: type check*)
            in Some el', tvoid_unit, side_effect_store el'.aLexprIsGhost
          | _ -> None, t, side_effect_pure in
        return next_id res_t { eff with invokesLogical = true } @@
          ACexternal (dest, s, i, [])

      | PCyield ({p_expression_desc = PEapp ({p_expression_desc =  (PEglob f); p_expression_loc = _ }, arg :: rest); p_expression_loc = _})
      | PCstore (_, {p_expression_desc = PEapp ({p_expression_desc = (PEglob f); p_expression_loc = _ }, arg :: rest); p_expression_loc = _ })
          when Hashtbl.mem external_function_environment f ->
        let (s, mt) = Hashtbl.find external_function_environment f in
        let dest, res_t, eff = match cmd.p_command_desc with
          | PCstore (el, _) ->
            let el' = translate_lexpr env.variable_env tmp_env el
            (*XXX: type check*)
            in Some el', tvoid_unit, side_effect_store el'.aLexprIsGhost
          | _ -> None, mt.aMethodReturnType, side_effect_pure in
        if rest <> [] then
          report_warning ("More than one (currying) argument for external function call " ^
            f ^ ": ignored") cmd.p_command_loc;
        return next_id res_t { eff with invokesLogical = true } @@
          ACexternal (dest, s, f, unfold_extcall_arg env.variable_env tmp_env
                                                     arg mt.aMethodArgumentTypes)

      | PCyield e ->
        begin match e.p_expression_desc with
        | PEbin (OPeq, _, _) when not expect_bool ->
          report_warning ("Found an equality test as return value--did you mean to do an assignment using `:='?") cmd.p_command_loc
        | _ -> ()
        end;
        begin match translate_expression env.variable_env tmp_env e with
        | `RExpr e' -> return next_id e'.aRexprType side_effect_pure @@ ACyield e'
        | `LExpr e' -> return next_id e'.aLexprType
            { side_effect_pure with dependsOnAbstraction = e'.aLexprIsGhost } @@
            ACload e'
        | `BigExpr _ -> report_error ("Not allowed to return data type constructs") cmd.p_command_loc; 
                        return next_id tvoid_unit side_effect_pure ACskip
        end

      | PClet (i, c1, c2) ->
         let c1', id1 = translate_command mk env false (next_id + 1) tmp_env c1 in
         let c2', id2 = translate_command mk env expect_bool id1
                          ((i, (next_id, c1'.aCmdType)) :: tmp_env) c2
         in return id2 c2'.aCmdType
              (side_effect_join c1'.aCmdEffect c2'.aCmdEffect) @@
              AClet (next_id, i, c1', c2')

      | PCsequence (c1, c2) ->
        let c1', id1 = translate_command mk env false next_id tmp_env c1 in
        let c2', id2 = translate_command mk env expect_bool id1 tmp_env c2 in
        let c1'' = match command_respects_type c1' tvoid_unit with
          | None -> report_error ("Command on the left of a sequence must not return values") c1.p_command_loc; 
                    c1'
          | Some c1'' -> c1''
        in return id2 c2'.aCmdType
             (side_effect_join c1''.aCmdEffect c2'.aCmdEffect) @@
             ACsequence (c1'', c2')

      (*
      | PCcall (s, f, arg) -> begin
        match find_method env s f with
        | `Not_found msg -> report_error msg; ACskip, tvoid_unit, next_id
        | `Found (targ, tret, kind) ->
          (* XXX: check kind *)
          ACcall (s, f, unfold_argument_expr env.variable_env tmp_env arg targ),
          tret, next_id
        end
      *)

      | PCcond (e, c, None) ->
        let e' = translate_rexpr_typed env.variable_env tmp_env e tint_bool
                   "Condition" in
        let c', next_id' = translate_command mk env false next_id tmp_env c in
        let c'' = match command_respects_type c' tvoid_unit with
          | None -> report_error ("The true branch of an if statement without a falsebranch must not return values") cmd.p_command_loc; 
                    c'
          | Some c'' -> c'' in
        let cskip = { aCmdDesc = ACskip; aCmdType = tvoid_unit;
                      aCmdEnv = tmp_env; aCmdEffect = side_effect_pure }
        in return next_id' tvoid_unit c'.aCmdEffect @@ ACcond (e', c'', cskip)
      | PCcond (e, c1, Some c2) ->
        let e' = translate_rexpr_typed env.variable_env tmp_env e tint_bool
                   "Condition" in
        let c1', id1 = translate_command mk env expect_bool next_id tmp_env c1 in
        let c2', id2 = translate_command mk env expect_bool id1 tmp_env c2 in
        let c1'', c2'', t = match unify_commands [c1'; c2'] with
          | Some ([c1''; c2''], t) -> c1'', c2'', t
          | _ ->
            report_error ("Two branches of a condition have to have the same" ^
              " type but the then branch has type " ^
              string_of_a_type false c1'.aCmdType ^ " and the else branch has " ^
              string_of_a_type false c2'.aCmdType) cmd.p_command_loc; 
            c1', c2', c1'.aCmdType
        (*
        let c1'', c2'' = match command_respects_type c1' c2'.aCmdType with
          | Some c1'' -> c1'', c2'
          | None -> match command_respects_type c2' c1'.aCmdType with
          | Some c2'' -> c1', c2''
          | None ->
            report_error ("Two branches of a condition have to have the same" ^
              " type but the then branch has type " ^
              string_of_a_type false c1'.aCmdType ^ " and the else branch has " ^
              string_of_a_type false c2'.aCmdType);
            c1', c2'
        *)
        in return id2 t (side_effect_join c1''.aCmdEffect c2''.aCmdEffect) @@
             ACcond (e', c1'', c2'')

      | PCfor (i, e1, e2, c, a) ->
        let e1' = translate_rexpr_typed env.variable_env tmp_env e1 tint_Z32
                    "Starting value of a for loop" in
        let e2' = translate_rexpr_typed env.variable_env tmp_env e2 tint_Z32
                    "Ending value of a for loop" in
        let n_iter, n_end = next_id, next_id + 1 in
        let c', next_id' = translate_command mk env false (n_end + 1)
          ((i, (n_iter, tint_Z32)) :: (("_e_" ^ string_of_int n_end, (n_end, tint_Z32)) :: tmp_env)) c in
        let c'' = match command_respects_type c' tvoid_unit with
          | None -> report_error ("For loop body cannot return a value") cmd.p_command_loc;
                    c'
          | Some c'' -> c''
        in return next_id' tvoid_unit c''.aCmdEffect @@
             ACfor (n_iter, i, e1', n_end, e2', c'',
                    parse_captured_command_annotations tmp_env a)

      | PCmatch (e, ls) ->
        let e', is_ghost = match translate_expression env.variable_env tmp_env e with
          | `RExpr { aRexprDesc = AEtemp (n, i); aRexprType = t } ->
            { aMatchableDesc = AMtemp (n, i); aMatchableType = t }, false
          | `RExpr e' ->
            (*XXX*)
            (* We could do two things here: either relax the syntax of a_matchable to allow
               general r-expressions, or make up a new temporary variable, and elaborate
               the match into a let-expression binding the tempvar, followed by a match on the tempvar. *)
            report_error ("Pattern matching on arbitrary expression not implemented yet") cmd.p_command_loc; 
            { aMatchableDesc = AMtemp (-1, "*DUMMY*");
              aMatchableType = e'.aRexprType }, false
          | `LExpr e' ->
            { aMatchableDesc = AMlexpr e'; aMatchableType = e'.aLexprType },
            e'.aLexprIsGhost
          | `BigExpr e' ->
            report_error ("Data constructor not allowed for pattern matching") cmd.p_command_loc;
            { aMatchableDesc = AMtemp (-1, "*DUMMY*");
              aMatchableType = e'.aBigExprType }, false in
        let cvs, cs, next_id' = List.fold_right (fun cl (cvs, cs, next_id) ->
          match translate_clause mk env expect_bool next_id tmp_env e' cl with
          | None -> cvs, cs, next_id
          | Some ((con, v, c), next_id') ->
            if con = None && cvs <> [] then
              report_error ("Only the last clause of a match can be a wildcard") cmd.p_command_loc; 
            (con, v) :: cvs, c :: cs, next_id'
          ) ls ([], [], next_id) in
        let ls', t = match unify_commands cs with
          | Some (cs', t) -> List.map2 (fun (con, v) c -> con, v, c) cvs cs', t
          | None -> match cs with
            | [] -> report_error ("Pattern matching with no clauses") cmd.p_command_loc;
                    [], tvoid_unit
            | c :: _ ->
              report_error ("Pattern matching clauses have to return the same type") cmd.p_command_loc; 
              List.map2 (fun (con, v) c -> con, v, c) cvs cs, c.aCmdType in
        let eff = List.fold_left (fun eff (_, _, c) ->
                    side_effect_join eff c.aCmdEffect
                  ) side_effect_pure ls'
        in return next_id' t { eff with dependsOnAbstraction = is_ghost } @@
             ACmatch (e', ls')

      | PCstore (el, er) ->
        let el' = translate_lexpr env.variable_env tmp_env el in
        let eff = side_effect_store el'.aLexprIsGhost in
        begin match translate_expression env.variable_env tmp_env er with
        | `RExpr er' ->
          let er'' = match rexpr_respects_type er' el'.aLexprType with
            | None ->
              report_error ("RHS of an assignment has type " ^
                string_of_a_type false er'.aRexprType ^ " but LHS requires " ^
                string_of_a_type false el'.aLexprType) cmd.p_command_loc;
              er'
            | Some er'' -> er''
          in return next_id tvoid_unit eff @@ ACstore (el', er'')
        | `LExpr _ -> report_error ("RHS of an assignment cannot be an L-value") cmd.p_command_loc;
                      return next_id tvoid_unit eff @@ ACstore (el', dummy_rexpr)
        | `BigExpr er' ->
          let er'' = match big_expr_respects_type er' el'.aLexprType with
            | None ->
              report_error ("RHS of a construction assignment has type " ^
                string_of_a_type false er'.aBigExprType ^ " but LHS requires " ^
                string_of_a_type false el'.aLexprType) cmd.p_command_loc;
              er'
            | Some er'' -> begin match er''.aBigExprDesc with
              | AEstruct (_, ls) | AEconstr (_, ls) ->
                List.iter (fun (i, e) -> match e.aBigExprDesc with
                  | AErexpr _ | AEconstr ({ aTypeConstrName = "array_init" }, _)
                  | AEexternal_const _ -> ()
                  | _ ->
                    (* NOTE: Coqgen relies on this restriction *)
                    (report_error ("Nested data type construction on field " ^
                      i ^ " not supported yet")  cmd.p_command_loc)
                ) ls
              | _ -> ()
              end;
              er''
          in return next_id tvoid_unit eff @@ ACconstr (el', er'')
        end

      | PCemit ({p_expression_desc = PEapp ({p_expression_desc = (PEglob f); p_expression_loc = _ }, args :: rest); p_expression_loc = _}) -> begin
        if rest <> [] then
          report_warning ("More than one (currying) argument for event " ^
            f ^": ignored") cmd.p_command_loc;

         match Hashtbl.find_opt event_environment f with
         | None -> report_error ("Unknown event name "^ f ^ ".") cmd.p_command_loc;
            return next_id tvoid_unit side_effect_pure @@ ACskip
         | Some et ->
            return next_id tvoid_unit side_effect_pure @@
              ACemit (et, unfold_event_arg env.variable_env tmp_env args et.aEventArgs)
        end

      | PCemit _ ->
         report_error ("The argument of an emit-statement must be an explicit application of the form `EventName(arg1, ... argn)`.") cmd.p_command_loc;
            return next_id tvoid_unit side_effect_pure @@ ACskip


      | PCfail -> return next_id tvoid_unit side_effect_pure ACfail
      | PCassert c -> translate_assertion mk env next_id tmp_env true c
      | PCdeny c -> translate_assertion mk env next_id tmp_env false c

      | PCghost c ->
        let c', next_id' = translate_command mk env false next_id tmp_env c in
        let c'' = match command_respects_type c' tvoid_unit with
          (* The runtime in the backend relies on these being void, but we could
             make the typechecker insert extra code to return a unit value here.*)
          | None -> report_error ("Ghost commands must not return values") cmd.p_command_loc;
                    c'
          | Some c'' -> c''
        in return next_id' tvoid_unit side_effect_pure @@ ACghost c''

      | PCfirst (i, e1, e2, c1, c2o, c3o, a) ->
        let e1' = translate_rexpr_typed env.variable_env tmp_env e1 tint_Z32
                    "Starting value of a first loop" in
        let e2' = translate_rexpr_typed env.variable_env tmp_env e2 tint_Z32
                    "Ending value of a first loop" in
        let n_iter, n_end, n_c = next_id, next_id + 1, next_id + 2 in
        let new_tmp_env = (i, (n_iter, tint_Z32)) :: ("_e_" ^ string_of_int n_end, (n_end, tint_Z32)) :: tmp_env in
        let c1', id1 = translate_command mk env true (n_c + 1) new_tmp_env c1 in
        let c1'' = match command_respects_type c1' tint_bool with
          | None -> report_error ("First loop body must return a boolean") cmd.p_command_loc;
                    c1'
          | Some c1'' -> c1'' in
        let _ = if c1''.aCmdEffect.affectsAbstraction ||
                   c1''.aCmdEffect.affectsImplementation ||
                   c1''.aCmdEffect.invokesLogical then
                  (report_error ("First loop body cannot have side-effects") cmd.p_command_loc) in
        let c2', id2 = match c2o with
          | Some c2 -> translate_command mk env expect_bool id1 new_tmp_env c2
          | None -> { aCmdDesc = ACyield { aRexprDesc = AEtemp (n_iter, i);
                                           aRexprType = tint_Z32 };
                      aCmdType = tint_Z32;
                      aCmdEnv = new_tmp_env;
                      aCmdEffect = side_effect_pure },
                    id1 in
        let c3', id3 = match c3o with
          | Some c3 -> translate_command mk env expect_bool id2 new_tmp_env c3
          | None ->
            if c2'.aCmdType = tint_Z32 then
              { aCmdDesc = ACyield { aRexprDesc = AEtemp (n_iter, i);
                                     aRexprType = tint_Z32 };
                aCmdType = tint_Z32;
                aCmdEnv = new_tmp_env;
                aCmdEffect = side_effect_pure },
              id2
            else begin
              if c2'.aCmdType <> tvoid_unit then
                report_error ("First loop's found branch must either return " ^
                  "an integer or not return anything when there is no else branch") cmd.p_command_loc;
              { aCmdDesc = ACskip; aCmdType = c2'.aCmdType;
                aCmdEnv = tmp_env; aCmdEffect = side_effect_pure },
              id2
            end in
        let c2'', c3'', t = match unify_commands [c2'; c3'] with
          | Some ([c2''; c3''], t) -> c2'', c3'', t
          | _ ->
            report_error ("Two branches of a first loop have to have the same" ^
              " type but the found branch has type " ^
              string_of_a_type false c2'.aCmdType ^ " and the else branch has " ^
              string_of_a_type false c3'.aCmdType) cmd.p_command_loc;
            c2', c3', c2'.aCmdType
        in return id3 t
             (side_effect_join c1''.aCmdEffect
               (side_effect_join c2''.aCmdEffect c3''.aCmdEffect)) @@
             ACfirst (n_iter, i, e1', n_end, e2', n_c, c1'', c2'', c3'',
                      parse_captured_command_annotations tmp_env a)

      | PCfold (i_iter, e1, e2, i_acc, e3, c, a) ->
        let e1' = translate_rexpr_typed env.variable_env tmp_env e1 tint_Z32
                    "Starting value of a fold" in
        let e2' = translate_rexpr_typed env.variable_env tmp_env e2 tint_Z32
                    "Ending value of a fold" in
        let e3' = translate_rexpr env.variable_env tmp_env e3 in
        let n_iter, n_end, n_acc, n_c = next_id, next_id + 1, next_id + 2, next_id + 3 in
        let t = e3'.aRexprType in
        let new_tmp_env =
          (i_acc, (n_acc, t)) :: (i_iter, (n_iter, tint_Z32)) ::
            ("_e_" ^ string_of_int n_end, (n_end, tint_Z32)) :: tmp_env in
        let c', next_id' = translate_command mk env expect_bool (n_c + 1) new_tmp_env c in
        let c'' = match command_respects_type c' t with
          | None ->
            report_error ("Body of a fold must have the same type as the " ^
              " initial value: " ^ string_of_a_type false t) cmd.p_command_loc;
            c'
          | Some c'' -> c''
        in if c''.aCmdEffect.affectsAbstraction ||
              c''.aCmdEffect.affectsImplementation ||
              c''.aCmdEffect.invokesLogical then
             report_error ("Body of a fold cannot have side-effects") cmd.p_command_loc;
           return next_id' t c''.aCmdEffect @@
             ACfold (n_iter, i_iter, e1', n_end, e2', n_acc, i_acc, e3', n_c, c'',
                     parse_captured_command_annotations tmp_env a)

  and translate_assertion mk env next_id tmp_env is_assert c =
    let c', next_id' = translate_command mk env true (next_id + 1) tmp_env c in
    let _ = if c'.aCmdEffect.affectsAbstraction ||
               c'.aCmdEffect.affectsImplementation ||
               c'.aCmdEffect.invokesLogical then
                report_error ("Assertions cannot have side-effects") c.p_command_loc in
    let c'' = match command_respects_type c' tint_bool with
      | None -> report_error ((if is_assert then "Assertion" else "Denial") ^
                              " has to be of boolean type") c.p_command_loc;
                c'
      | Some c'' -> c'' in
    let rev, c_assertion = command_remove_negation c'' in
    let assert_possitive = is_assert <> rev (* is_assert xor rev *)
    in { aCmdDesc = if assert_possitive then ACassert c_assertion
                                        else ACdeny c_assertion;
         aCmdType = tvoid_unit;
         aCmdEnv = tmp_env;
         aCmdEffect = side_effect_pure },
       next_id'

  and translate_clause mk env expect_bool next_id tmp_env e (cid, vs, c) =
    if cid = "_" then
      let c', next_id' = translate_command mk env expect_bool next_id tmp_env c
      in Some ((None, [], c'), next_id')
    else if cid = "CONS" then begin  (* special cased because we want lists to be a little bit polymorphic. *)
      match e.aMatchableType.aTypeDesc with
      | ATlist t -> begin
          match vs with
                | [v1;v0] ->
                     let variable_env' = Hashtbl.copy env.variable_env in
                     let env' = { env with variable_env = variable_env' } in
                     let v_env = (v1, (next_id+1,e.aMatchableType))::(v0, (next_id, t))::[] in
                     let tmp_env'= (v1, (next_id+1,e.aMatchableType))::(v0, (next_id,t))::tmp_env in
                     let next_id' = next_id+2 in
                     let con = {aTypeConstrName = "CONS"; aTypeConstrArgs = [(v0, t); (v1, e.aMatchableType)]; aTypeConstrImpl = None} in
                     let c', next_id'' = translate_command mk env' expect_bool next_id' tmp_env' c in
                        Some ((Some con, v_env, c'), next_id'')
                | _ -> report_error ("incorrect number of arguments for CONS constructor") c.p_command_loc; None
                end
      | _ -> begin
             report_error ("The matched expression is of type " ^ string_of_a_type false e.aMatchableType
                           ^ " but pattern requires a list type ") c.p_command_loc;
             None end
      end
    else if cid = "NIL" then begin  (* special cased because we want lists to be a little bit polymorphic. *)
      match e.aMatchableType.aTypeDesc with
      | ATlist t -> begin
                    match vs with
                    | [] ->
                         let variable_env' = Hashtbl.copy env.variable_env in
                         let env' = { env with variable_env = variable_env' } in
                         let v_env = [] in
                         let con = {aTypeConstrName = "NIL"; aTypeConstrArgs = []; aTypeConstrImpl = None} in
                         let c', next_id'' = translate_command mk env' expect_bool next_id tmp_env c in
                          Some ((Some con, v_env, c'), next_id'')
                  | _ -> report_error ("incorrect number of arguments for NIL constructor.") c.p_command_loc; None
                  end
      | _ -> begin
             report_error ("The matched expression is of type " ^ string_of_a_type false e.aMatchableType
                           ^ " but pattern requires a list type ") c.p_command_loc;
             None end
      end
    else match catch_not_found (Hashtbl.find type_constr_environment) cid with
    | None ->
      report_error ("Constructor " ^ cid ^ " not defined (constructor for type " ^
        string_of_a_type false e.aMatchableType ^ " required)") c.p_command_loc;
      None
    | Some (t', con) -> if t' <> e.aMatchableType
      then begin
        report_error ("Constructor " ^ cid ^ " is for type " ^
          string_of_a_type false t' ^ " but pattern requires type " ^
          string_of_a_type false e.aMatchableType) c.p_command_loc;
        None end
      else try
        let variable_env' = Hashtbl.copy env.variable_env in
        let env' = { env with variable_env = variable_env' } in
        let next_id', v_env, tmp_env' =
          List.fold_left2 (fun (next_id, v_env, tmp_env) i (_, t) ->
            if i = "_" then
              next_id, ("_", (-1, t)) ::v_env, tmp_env (* Note that the this returns a "junk" environment, use discerningly. *)
            else next_id + 1, (i, (next_id, t)) :: v_env,
                              (i, (next_id, t)) :: tmp_env
          ) (next_id, [], tmp_env) vs con.aTypeConstrArgs in
        let c', next_id'' = translate_command mk env' expect_bool next_id' tmp_env' c in
        let _ = match con.aTypeConstrImpl with
          | Some { aImplDesc = ACdefault } ->
            if c'.aCmdDesc <> ACfail then
              report_error ("Pattern matching branch for default constructor can only be ``fail''") c.p_command_loc;
          | _ -> ()
        in Some ((Some con, v_env, c'), next_id'')
      with Invalid_argument _ (* "List.fold_left2" *) ->
        report_error ("Constructor " ^ cid ^ " has " ^
          string_of_int (List.length con.aTypeConstrArgs) ^ " arguments, but " ^
          string_of_int (List.length vs) ^ " given")  c.p_command_loc;
        None
  in

  let rec fields_helper objfields = match objfields with
    | [] -> {aCmdDesc = ACskip; aCmdType = builtin_type_a_type Tunit; aCmdEnv = []; aCmdEffect = side_effect_pure}
    | hd :: tl -> { aCmdDesc = ACsequence(hd, (fields_helper tl)); aCmdType = hd.aCmdType; aCmdEnv = hd.aCmdEnv; aCmdEffect = hd.aCmdEffect}
    in

  let generate_default_constructor objfields =
    let method_cmd = {
                      aCmdDesc = ACskip;
                      aCmdType = tvoid_unit;
                      aCmdEnv = [];
                      aCmdEffect = side_effect_pure
                    } in
    let fields = fields_helper objfields in 
    let method_body = { aCmdDesc = ACsequence(fields, method_cmd); aCmdType =  builtin_type_a_type Tunit; aCmdEnv = []; aCmdEffect = side_effect_pure} in
            [{ aMethodName = "constructor";
              aMethodType = { aMethodArgumentTypes = [];
                              aMethodReturnType = tvoid_unit;
                              aMethodKind = MKconstructor };
              aMethodArguments = [];
              aMethodSemantics = ASdefault;
              aMethodBody = method_body;
              aMethodParamEnv = []
            }] in

  let translate_method_body env name semantics body params kind ret_opt logical_object meloc objfields =
    let next_id, param_tmp_env = 
      if kind == MKconstructor then
            constructor_temp_identifier_start, 
            List.fold_left (fun (env : tmp_env_t) (a, t) ->
                (incr constructor_current_id_num);
                if a<>"_" && a<>"()" then (a, (constructor_current_id_num.contents, t)) :: env else env) 
              (empty_tmp_env ()) params
        else
            List.fold_left (fun (next_id, env) (a, t) ->
              next_id + 1, if a<>"_" && a<>"()" then (a, (next_id, t)) :: env else env)
            (function_start_id_num + 1, empty_tmp_env ()) params in
    let c, _ = 
              if kind == MKconstructor 
                then let method_cmd, temp_id = translate_command MKconstructor env false next_id param_tmp_env body in
                    let fields = fields_helper objfields in 
                    { aCmdDesc = ACsequence(fields, method_cmd); aCmdType =  builtin_type_a_type Tunit; aCmdEnv = []; aCmdEffect = side_effect_pure}, temp_id
                else translate_command kind env false next_id param_tmp_env body in
    let c' = match ret_opt with
      | None -> c
      | Some t -> match command_respects_type c t with
      | None ->
        report_error ("Body of method " ^ name ^ " has type " ^
          string_of_a_type false c.aCmdType ^ " but " ^
          string_of_a_type false t ^ " required") meloc;
        c
      | Some c' -> c' in
    if c'.aCmdEffect.invokesLogical && not logical_object && kind <> MKlogical then
      report_error ("Method " ^ name ^
        " is not declared logical but calls logical primitives") meloc;
    if c'.aCmdEffect.dependsOnAbstraction && not logical_object && (kind = MKconst || kind = MKnormal) then
      report_error ("Method " ^ name ^
        " is not declared a ghost but reads ghost fields or return values from ghost primitive") meloc;
    if c'.aCmdEffect.affectsImplementation && not logical_object && kind <> MKnormal && kind <> MKlogical && kind <> MKconstructor && kind <> MKrefined then
      report_error ("Method " ^ name ^ " is declared to be " ^
        string_of_method_kind kind ^ "but modifies the program state") meloc;
    if c'.aCmdEffect.affectsAbstraction && (kind = MKconst || kind = MKconstghost) then
      report_error ("Method " ^ name ^ " is declared to be " ^
        string_of_method_kind kind ^ "but modifies the abstract state") meloc;
    let param_names, param_types = List.split params in
    { aMethodName = name;
      aMethodType = { aMethodArgumentTypes = param_types;
                      aMethodReturnType = c'.aCmdType;
                      aMethodKind = kind };
      aMethodArguments = param_names;
      aMethodSemantics = semantics;
      aMethodBody = c';
      aMethodParamEnv = param_tmp_env
    } in

  let translate_method objname var_env base func_env proc_env name m mt logical_object mloc objfields =
    let sem = try
      match List.find (function PAclause ("semantics", _) -> true
                              | _ -> false) m.pMethodAnnotations with
      | PAclause (_, [PAclause ("setCR3", _)]) -> ASsetCR3
      | PAclause (_, [PAclause ("assembly", _)]) -> ASassembly
      | PAclause (_, [PAclause ("assembly'", _)]) -> ASassembly'
      | PAclause (_, [PAclause ("trap_info_get", _)]) -> AStrap_info_get
      | PAclause (_, [PAclause ("trap_info_ret", _)]) -> AStrap_info_ret
      | PAclause (_, []) ->
        report_warning ("Incomplete semantics directive given; " ^
          "using default wrapper") mloc;
        raise Not_found
      | PAclause (_, [PAclause (name, _)]) ->
        report_warning ("Unknown semantic wrapper: " ^ name ^
          "; using default wrapper instead") mloc;
        raise Not_found
      | _ -> assert false
      with Not_found -> ASdefault in

    let _ = match m.pMethodReturnType with
      | Some ret' when translate_type ret' <> mt.aMethodReturnType ->
        report_error ("Method " ^ name ^ " is annotated with a type " ^
                        "incompatible to the record; annotation ignored") mloc
      | _ -> () in
    let params = try List.map2 (fun (a, t_opt) t -> begin match t_opt with
      | Some t' when translate_type t' <> t ->
        report_error ("Argument " ^ a ^ " of method " ^ name ^
          " is annotated as " ^ string_of_a_type false (translate_type t') ^
          " but is " ^ string_of_a_type false t ^
          " in the record; annotation ignored") mloc
      | _ -> ()
      end;
      a, t
      ) m.pMethodArguments mt.aMethodArgumentTypes
      with Invalid_argument _ (* "List.map2" *) ->
        report_error ("Method " ^ name ^ " expects " ^
          string_of_int (List.length mt.aMethodArgumentTypes) ^
          " arguments in the record type but " ^
          string_of_int (List.length m.pMethodArguments) ^ " assumed") mloc;
        [] in
    let res = translate_method_body
         (mk_cmd_static_env var_env base func_env proc_env)
         name sem m.pMethodBody
         params mt.aMethodKind (Some mt.aMethodReturnType) logical_object m.pMethodLoc objfields

    (* TODO: type check for other semantic wrappers *)
    (* (We check for cases where the proof obligations would not be satisfiable,
        e.g. if the user tries to use a "raw type" (one which has a non-trivial injection)
        as a method return type. But the check is not complete, e.g. if you try to
        annotate a method with set_cr3, then the type of the method must be (globalpointer -> unit)
        or the generated Coq layer file will not typecheck. *)
    in if sem = ASdefault && method_kind_realizing mt.aMethodKind &&
            (a_type_contains_raw_type res.aMethodType.aMethodReturnType ||
             List.exists a_type_contains_raw_type
                         res.aMethodType.aMethodArgumentTypes) then
         report_error ("Method " ^ name ^ " in object " ^ objname ^
           " has a signature containing raw types: " ^
           "requires non-default semantic wrapper") mloc;
       res in
  (*
  let translate_procedure var_env interface func_env proc_env name m =
    let params = List.map (fun (i, t_opt) -> match t_opt with
      | None when i = "_" -> i, tvoid_unit
      | None (* when i <> "_" *) ->
        report_error (name ^ " is a procedure or function whose argument " ^
                      i ^ " must be annotated with types");
        i, dummy_type (name ^ "_" ^ i)
      | Some t -> i, translate_type t
      ) m.pMethodArguments in
    let ret = match m.pMethodReturnType with
      | None -> None
      | Some t -> Some (translate_type t)
    in translate_method_body
         (mk_cmd_static_env var_env interface func_env proc_env)
         name ASdefault m.pMethodBody params ret in
  let translate_function func_env name m =
    let m' = translate_procedure (empty_var_env ()) [] func_env [] name m
    in if not (command_is_pure m'.aMethodBody) then
         report_error ("Function " ^ name ^ " has non-pure body");
       m' in
  *)

  (*
  let object_next_serial = ref 0 in
  let get_object_serial _ =
    let n = !object_next_serial in object_next_serial := n + 1; n in
  *)

  let translate_object_constr i c =
    let fields = List.map (translate_object_field i (c.pObjKind <> POnormal))
                          c.pObjFields in
    let fields_to_constructorMethod =
      if c.pObjKind = POnormal then
        List.map (translate_object_field_to_Constructor i (c.pObjKind <> POnormal))
          c.pObjFields
      else
        []  (* Don't need constructor code for logical object fields *)
    in
    let var_env = empty_var_env () in
    let _ = List.iter (fun f -> Hashtbl.add var_env f.aObjectFieldName
        { aLexprDesc = AEglob f.aObjectFieldName;
          aLexprType = f.aObjectFieldType;
          aLexprIsGhost = f.aObjectFieldIsLogical }
      ) fields in

    let base = translate_layer_signature (i ^ "_base") c.pObjType.pObjectBase in
    let signature = translate_signature (i ^ "_signature") c.pObjType.pObjectSignature in

    (*
    let method_def_type_translate =
      if c.pObjKind = POnormal then
        fun mt -> mt
      else
        fun mt -> if method_kind_realizing mt.aMethodKind
          then { mt with aMethodKind = MKlogical }
          else mt in
    *)

    let methods, procedures, functions =
      List.fold_left (fun (m_res, p_res, f_res) (name, m) ->
        (*
        if List.mem (PAclause ("function", [])) m.pMethodAnnotations then
          m_res, p_res, (translate_function f_res name m) :: f_res
        else if List.mem (PAclause ("procedure", [])) m.pMethodAnnotations then
          m_res,
          (translate_procedure var_env base.aLayerSignatureSlots f_res p_res
                               name m) :: p_res,
          f_res
        else *) match catch_not_found (List.find (fun (i, _) -> i = name))
                     signature.aSignatureMethods with
          | None -> report_warning ("Unknown method " ^ name ^ " for record type " ^
                      signature.aSignatureName ^ " in object " ^ i ^ " ignored") c.pObjLoc;
                    m_res, p_res, f_res
          | Some (_, mt) ->
            (translate_method i var_env base.aLayerSignatureSlots f_res p_res
                    name m mt (c.pObjKind <> POnormal) c.pObjLoc fields_to_constructorMethod) :: m_res,
            p_res, f_res
      ) ([], [], []) c.pObjMethods in

    let methods = if (List.exists (fun (m) -> m.aMethodType.aMethodKind == MKconstructor) methods) then 
                      methods
                  else 
                      methods @ (generate_default_constructor fields_to_constructorMethod)
                    in
    if (List.length (List.find_all (fun m -> m.aMethodType.aMethodKind == MKconstructor) methods)) > 1 then
        report_error ("Only one constructor in a layer is allowed") c.pObjLoc;

    let method_sig_type_translate =
      if c.pObjKind <> POlogical then
        fun mt -> mt
      else
        fun mt -> match mt.aMethodKind with
          | MKconst -> { mt with aMethodKind = MKconstghost }
          | MKnormal -> { mt with aMethodKind = MKlogical }
          | _ -> mt in
    let method_types = List.map (fun (name, mt) ->
              if List.for_all (fun m -> m.aMethodName <> name) methods then
                report_error ("Method " ^ name ^ " for record type " ^
                  signature.aSignatureName ^ " undefined in object " ^ i) c.pObjLoc;
              (name, method_sig_type_translate mt)
            ) signature.aSignatureMethods in
    let signature' = { signature with aSignatureMethods = method_types } in
    let t = { aObjectBase = base; aObjectSignature = signature' } in
    { aObjectName = i;
      aObjectAddress = None;
      (*aObjectSerial = get_object_serial ();*)
      aObjectCoreType = t;
      aObjectType = t;
      aObjectRequireImpl = c.pObjKind = POnormal;
      aObjectIsTrusted = c.pObjKind = POtrusted;
      aObjectFields = fields;
      aObjectMethods = List.rev methods;
      aObjectProcedures = List.rev procedures;
      aObjectFunctions = List.rev functions } in

  let rec translate_object i obj = match obj.p_object_desc with
      | POconstr c -> translate_object_constr i c
      | POrelax (o0, layer_sig) ->
        let o0' = translate_object (i ^ "_src") o0 in
        let layer_sig' = translate_layer_signature (i ^ "_relax") layer_sig in
        check (layer_signature_subseteq o0'.aObjectType.aObjectBase layer_sig')
              ("In definition of object " ^ i ^ " attempt to relax " ^
                string_of_a_layer_signature o0'.aObjectType.aObjectBase ^ " to " ^
                  string_of_a_layer_signature layer_sig' ^ ", which is not a superset") obj.p_object_loc;
        { o0' with aObjectType = { o0'.aObjectType with aObjectBase = layer_sig' } }
      | POclone i' -> 
        (try 
          (* find the original object *)
          let origo = Hashtbl.find object_environment i' in
          (* (if origo.aObjectAddress = None then
            report_error ("Object " ^ i' ^ " cloned with no address attached") obj.p_object_loc
          else ()); *)
          (* create aliased new object *)
          let i'' = fresh i' in
          let no = { origo with aObjectName = i'' } in 
          (* FIXME: we should force every cloned object to have an address instantiation *)
          (* store new object in hashtable *)
          Hashtbl.add object_environment i'' no;
          (* return new object *)
          try Hashtbl.find object_environment i''
          with Not_found ->
            report_error ("Object " ^ i'' ^ " not defined") obj.p_object_loc;
            dummy_object i
        with Not_found ->
          report_error ("Object " ^ i' ^ " not defined") obj.p_object_loc;
          dummy_object i )
      | POname i' ->
        try Hashtbl.find object_environment i'
        with Not_found ->
          report_error ("Object " ^ i' ^ " not defined") obj.p_object_loc;
          dummy_object i in

  let translate_object_definition i o =
    let o' = translate_object i o.pObjectDesc in
    begin match o.pObjectType with
    | Some t ->
      let t' = translate_object_type i t in
      if not (object_type_equal t' o'.aObjectType) then
        report_error ("Object " ^ i ^ " annotated with type " ^
          string_of_a_object_type t' ^ " but is bound to that of " ^
          "type " ^ string_of_a_object_type o'.aObjectType) o.pObjectLoc
    | None -> ()
    end;
    o' in

  (*
  let layer_next_serial = ref 0 in
  let get_layer_serial _ =
    let n = !layer_next_serial in layer_next_serial := n + 1; n in
  *)
  let translate_proposition pprop = match pprop.p_proposition_desc with
    | PPexternal s -> s
    | PPident i ->
      match catch_not_found (Hashtbl.find external_prop_environment) i with
      | None ->
        report_error ("Unrecognized proposition name: " ^ i) pprop.p_proposition_loc;
        "***UNRECOGNIZED PROP***"
      | Some s -> s in

  (* Layers are handled in two steps.
     First we typecheck the definitions, creating an a_checked_layer,
     translate_layer_constr and translate_layer_definition. *)

  let translate_layer_constr i l de_loc = match l with 
    | [] -> report_error ("Layer " ^ i ^ " has no members") de_loc;
      let dummy_type = dummy_layer_type i in
      { aCheckedLayerName = i;
        (*aCheckedLayerSerial = -1;*)
        (*aCheckedLayerCoreType = dummy_type;*)
        aCheckedLayerType = dummy_type;
        aCheckedLayerAccessor = AAnone;
        aCheckedLayerInvariant = None;
        aCheckedLayerPassthroughs = dummy_layer_signature i;
        aCheckedLayerTipObjectSet = IdentSet.empty;
        aCheckedLayerKeelObjectSet = IdentSet.empty;
        aCheckedLayerDesc = ALconstr []
      }
    | (s0, o0) :: lst ->
      let dsgo0 = match o0.p_object_inst_desc with 
        | POinternal o0_in -> o0_in
        | POexternal (_, o0_ext) -> o0_ext
      in
      let o0' = translate_object (i ^ "_" ^ s0) dsgo0 in
      (* need to update o0' information with the new address *)
      let o0_addr = match o0.p_object_inst_desc with 
        | POinternal o0_in -> 
          (* report_warning ("Object " ^ o0'.aObjectName ^ " is internal. ") de_loc; *)
          None
        | POexternal ((CONaddress addr), o0_ext) -> 
          (* report_warning ("Object " ^ o0'.aObjectName ^ " is external. ") de_loc; *)
          Some addr
        | _ -> 
          (* report_error ("Object " ^ o0'.aObjectName ^ " have bad address linking ") de_loc; *)
          None
      in
      (if Hashtbl.mem object_environment o0'.aObjectName then () else 
      report_error ("Object " ^ o0'.aObjectName ^ " does not exist. ") de_loc;
      ());
      let o0' = { o0' with aObjectAddress = o0_addr } in 
      Hashtbl.replace object_environment o0'.aObjectName o0';
      
      (* (try 
          let dummy = Hashtbl.find object_environment o0'.aObjectName in
          (* store new object in hashtable *)
          
          ()
        with Not_found ->
          report_error ("Object " ^ i ^ " linked with an address but is uninstantiated. ") de_loc;
          ()); *)
      
      let base = o0'.aObjectType.aObjectBase in
      let lst' = (s0, o0') :: List.map (fun (s, o) ->
        let dsgo' = match o.p_object_inst_desc with 
          | POinternal o_in -> o_in
          | POexternal (_, o_ext) -> o_ext
        in
        let o' = translate_object (i ^ "_" ^ s) dsgo' in
        let o_addr = match o.p_object_inst_desc with 
          | POinternal o_in -> 
            (* report_warning ("Object " ^ o'.aObjectName ^ " is internal. ") de_loc; *)
            None
          | POexternal ((CONaddress addr), o0_ext) -> 
            (* report_warning ("Object " ^ o'.aObjectName ^ " is external. ") de_loc; *)
            Some addr
          | _ -> 
            report_error ("Object " ^ o0'.aObjectName ^ " have bad address linking ") de_loc;
            None
        in

        let o' = { o' with aObjectAddress = o_addr } in
        Hashtbl.replace object_environment o'.aObjectName  o';
        (* let oid_opt = match dsgo'.p_object_desc with
          | POname i' -> Some i'
          (* | POclone i' -> Some i' *)
          | _ -> None in
        (match oid_opt with
        | Some i -> 
          (try 
            let dummy = Hashtbl.find object_environment i in
            (* store new object in hashtable *)
            
            ()
          with Not_found ->
            report_error ("Object " ^ i ^ " linked with an address but is uninstantiated. ") de_loc;
            ())
        | None -> ()); *)

        if not (object_type_equal base o'.aObjectType.aObjectBase) then
          report_error ("In layer " ^ i ^ ", slot " ^ s ^ " (object " ^
            o'.aObjectName ^ ") has different base type to slot " ^ s0 ^
            "(object " ^ o'.aObjectName ^ "); " ^
            string_of_a_layer_signature o'.aObjectType.aObjectBase ^ " vs " ^
            string_of_a_layer_signature base) dsgo0.p_object_loc;
        s, o') lst in
      let object_set = IdentSet.of_list (List.map
             (fun (_, { aObjectName = name }) -> name) lst') in
      let layer_type = {
            aLayerBase = base;
            aLayerSignature = {
              aLayerSignatureName = i ^ "_layer_sig";
              aLayerSignatureSlots =
                List.map (fun (s, o) -> s, o.aObjectType.aObjectSignature) lst';
              aLayerSignatureSuppressError = false
            }
          }
      in { aCheckedLayerName = i;
           (*aCheckedLayerSerial = get_layer_serial ();*)
           (*aCheckedLayerCoreType = layer_type;*)
           aCheckedLayerType = layer_type;
           aCheckedLayerAccessor = AAnone;
           aCheckedLayerInvariant = None;
           aCheckedLayerPassthroughs = empty_layer_signature;
           aCheckedLayerTipObjectSet = object_set;
           aCheckedLayerKeelObjectSet = object_set;
           aCheckedLayerDesc = ALconstr lst'
         } in

  let translate_layer_definition i l =
    let default_loc = l.pLayerLoc in 
    let rec checklayer i layer = match layer.p_layer_desc with
      | PLconstr l -> translate_layer_constr i l default_loc
      | PLrelax (l0, t) ->
        let l0' = checklayer (i (* ^ "_src" *)) l0 in
        let t' = translate_layer_type (i ^ "_relax") t in
        (* rename to match the typing rule on paper *)
        let i1, i2 = l0'.aCheckedLayerType.aLayerBase,
                     l0'.aCheckedLayerType.aLayerSignature in
        let i1', i2' = t'.aLayerBase, t'.aLayerSignature in

        let msg = "Relaxing layer " ^ i ^ ": " in begin
        check (layer_signature_subseteq i1 i1')
              (msg ^ "base layer signature not a subset.") l0.p_layer_loc;
        check (layer_signature_subseteq i2' (layer_signature_union i2 i1'))
              (msg ^ "exporting layer signature is bigger than provided.") l0.p_layer_loc;
        (* New layer refinement mechanism requires this
        else if not (layer_signature_disjoint i2 (layer_signature_minus i1' i1)) then
          report_error (msg ^ "ambiguous source of exporting slot");
        *)
        { l0' with
          aCheckedLayerName = i;
          aCheckedLayerType = t';
          aCheckedLayerPassthroughs = layer_signature_minus i2' i2;
          aCheckedLayerDesc = ALrelax l0' }
        end
      | PLinst (l1, l2) ->
        let l1' = checklayer (i ^ "_top") l1 in
        let l2' = checklayer (i ^ "_bot") l2 in
        if not (layer_signature_equal l1'.aCheckedLayerType.aLayerBase
                                 l2'.aCheckedLayerType.aLayerSignature) then
          report_error ("Instantiating layer " ^ i ^ " but signatures mismatch: " ^
            "top layer " ^ l1'.aCheckedLayerName ^ " requires " ^
            string_of_a_layer_signature l1'.aCheckedLayerType.aLayerBase ^ " but " ^
            "bottom layer " ^ l2'.aCheckedLayerName ^ " provides " ^
            string_of_a_layer_signature l2'.aCheckedLayerType.aLayerSignature) none;
        let tip_intersection =
              IdentSet.inter l1'.aCheckedLayerTipObjectSet
                             l2'.aCheckedLayerTipObjectSet in
        let keel_intersection =
              IdentSet.inter l1'.aCheckedLayerKeelObjectSet
                             l2'.aCheckedLayerKeelObjectSet in
        let layer_type =
          { aLayerBase = l2'.aCheckedLayerType.aLayerBase;
            aLayerSignature = l1'.aCheckedLayerType.aLayerSignature } in
        if not (IdentSet.is_empty tip_intersection) then
          report_error ("Instantiating layer " ^ i ^ " with overlapping tip objects: " ^
            String.concat ", " (IdentSet.elements tip_intersection)) layer.p_layer_loc;
        if not (IdentSet.is_empty keel_intersection) then
          report_error ("Instantiating layer " ^ i ^ " with overlapping keel objects: " ^
            String.concat ", " (IdentSet.elements keel_intersection)) layer.p_layer_loc;
        { aCheckedLayerName = i;
          (*aCheckedLayerSerial = get_layer_serial ();*)
          (*aCheckedLayerCoreType = layer_type;*)
          aCheckedLayerType = layer_type;
          aCheckedLayerAccessor = l1'.aCheckedLayerAccessor;
          aCheckedLayerInvariant = l1'.aCheckedLayerInvariant;
          aCheckedLayerPassthroughs = empty_layer_signature;
          (*aCheckedLayerObjects = l1'.aCheckedLayerObjects @ l2'.aCheckedLayerObjects;*)
          aCheckedLayerTipObjectSet =
            IdentSet.union l1'.aCheckedLayerTipObjectSet l2'.aCheckedLayerTipObjectSet;
          aCheckedLayerKeelObjectSet =
            IdentSet.union l1'.aCheckedLayerKeelObjectSet l2'.aCheckedLayerKeelObjectSet;
          aCheckedLayerDesc = ALinst (l1', l2')
        }
      | PLrefine (l1, l2, p) ->
        let l1' = checklayer (i ^ "_logic") l1 in
        let l2' = checklayer (i ^ "_impl") l2 in
        if not (layer_signature_equal l1'.aCheckedLayerType.aLayerBase
                                      l2'.aCheckedLayerType.aLayerSignature) then
          report_error ("Layer refinement for " ^ i ^ " but signatures mismatch: " ^
            "logical layer " ^ l1'.aCheckedLayerName ^ " requires " ^
            string_of_a_layer_signature l1'.aCheckedLayerType.aLayerBase ^ " but " ^
            "implementation layer " ^ l2'.aCheckedLayerName ^ " provides " ^
            string_of_a_layer_signature l2'.aCheckedLayerType.aLayerSignature) layer.p_layer_loc;
        check (layer_signature_subseteq l1'.aCheckedLayerType.aLayerSignature
                                         l1'.aCheckedLayerType.aLayerBase)
              ("Layer refinement for " ^ i ^ " failed: logical layer " ^
                 l1'.aCheckedLayerName ^ " exports not a subset of what it requires " ^
                   " (" ^ string_of_a_layer_signature l1'.aCheckedLayerType.aLayerSignature ^ " vs " ^
                     string_of_a_layer_signature l1'.aCheckedLayerType.aLayerBase ^ ")") layer.p_layer_loc;
        (*
        if not (layer_type_equal l1'.aCheckedLayerType
                                 l2'.aCheckedLayerType) then
          report_error ("Layer refinement for " ^ i ^ " failed: logical layer " ^
            l1'.aCheckedLayerName ^ " does not have the same type as " ^
            l2'.aCheckedLayerName ^
            " (" ^ string_of_a_layer_type l1'.aCheckedLayerType ^ " vs " ^
            string_of_a_layer_type l2'.aCheckedLayerType ^ ")");
        *)
        (* XXX: what are the typecheck criteria now? *)
        (*
        if l1'.aCheckedLayerType.aLayerBase.aLayerSignatureSlots <> [] then
          report_error ("Layer refinement for " ^ i ^ " failed: " ^
            "currently only support grounded layer refinements");
        *)
        { aCheckedLayerName = i;
          (*aCheckedLayerSerial = get_layer_serial ();*)
          (*aCheckedLayerCoreType = l1'.aCheckedLayerType;*)
          aCheckedLayerType = l2'.aCheckedLayerType;
          aCheckedLayerAccessor = l1'.aCheckedLayerAccessor;
          aCheckedLayerInvariant = l1'.aCheckedLayerInvariant;
          aCheckedLayerPassthroughs = empty_layer_signature;
          (*aCheckedLayerObjects = l1'.aCheckedLayerObjects @ l2'.aCheckedLayerObjects;*)
          aCheckedLayerTipObjectSet = l1'.aCheckedLayerTipObjectSet;
          aCheckedLayerKeelObjectSet = l2'.aCheckedLayerKeelObjectSet;
          aCheckedLayerDesc = ALrefine (l1', l2', translate_proposition p)
        }
      | PLname i' ->
        try Hashtbl.find layer_environment i'
        with Not_found ->
          report_error ("Layer " ^ i' ^ " not defined") layer.p_layer_loc;
          let dummy_type = dummy_layer_type i in
          { aCheckedLayerName = i';
            (*aCheckedLayerSerial = -1;*)
            (*aCheckedLayerCoreType = dummy_type;*)
            aCheckedLayerType = dummy_type;
            aCheckedLayerAccessor = AAnone;
            aCheckedLayerInvariant = None;
            aCheckedLayerPassthroughs = dummy_layer_signature i;
            aCheckedLayerTipObjectSet = IdentSet.empty;
            aCheckedLayerKeelObjectSet = IdentSet.empty;
            aCheckedLayerDesc = ALconstr []
          } in


    (* In the second step, we "normalize" the layer by re-associating all layer instantiations to the right,
       and by pushing down layer relaxations to create passthrough.

       Finally we call generate_declarations on the normalized layer.
      *)

    let rec push_relax l passthroughs =
      if not (layer_signature_disjoint l.aCheckedLayerType.aLayerSignature
                                       passthroughs) then
        report_error ("Layer " ^ l.aCheckedLayerName ^ " is pushed with passthroughs " ^
          "overlapping with exported layer signature.\nLayer signature: " ^
          string_of_a_layer_signature l.aCheckedLayerType.aLayerSignature ^
          "\nPassthroughs: " ^ string_of_a_layer_signature passthroughs) none;
      let layer_type = {
        aLayerBase =
          layer_signature_union l.aCheckedLayerType.aLayerBase passthroughs;
        aLayerSignature =
          layer_signature_union l.aCheckedLayerType.aLayerSignature passthroughs } in
      debug_endline ("push_relax [" ^ l.aCheckedLayerName ^ "] w/ type " ^
        string_of_a_layer_type l.aCheckedLayerType);
      debug_endline ("push_relax [" ^ l.aCheckedLayerName ^ "] w/ ps " ^
        string_of_a_layer_signature passthroughs);
      debug_endline ("push_relax [" ^ l.aCheckedLayerName ^ "] ret type " ^
        string_of_a_layer_type layer_type);
      match l.aCheckedLayerDesc with
      | ALconstr _ ->
        { l with
          aCheckedLayerPassthroughs =
            layer_signature_union l.aCheckedLayerPassthroughs passthroughs;
          aCheckedLayerType = layer_type
        }
      | ALrelax l' ->
        let l'' = push_relax l' @@ layer_signature_union l.aCheckedLayerPassthroughs passthroughs (*base @@ Some (infered_interface ())*)
        in { l'' with aCheckedLayerType = layer_type }
      | ALinst (l1, l2) ->
        let l1' = push_relax l1 passthroughs in
        let l2' = push_relax l2 passthroughs
        in { l with
             aCheckedLayerType = layer_type;
             aCheckedLayerDesc = ALinst (l1', l2') }
      | ALrefine (l1, l2, p) ->
        let l1' = push_relax l1 passthroughs in
        let l2' = push_relax l2 passthroughs
        in { l with
             aCheckedLayerType = layer_type;
             aCheckedLayerDesc = ALrefine (l1', l2', p) } in

    let remove_relax l =
      push_relax l { aLayerSignatureName = "*empty passthrough*";
                     aLayerSignatureSlots = [];
                     aLayerSignatureSuppressError = false } in
    (*
    let rec remove_relax l = match l.aCheckedLayerDesc with
      | ALconstr _ -> l
      | ALrelax l' -> push_relax l' l.aCheckedLayerPassthroughs
      | ALinst (l1, l2) ->
        { l with aCheckedLayerDesc = ALinst (remove_relax l1, remove_relax l2) }
      | ALrefine (l1, l2) ->
        { l with aCheckedLayerDesc = ALrefine (remove_relax l1, remove_relax l2) } in
    *)

    let l' = checklayer i l.pLayerDesc in

    let _ =
      debug_endline ("Pre remove " ^ l'.aCheckedLayerName ^ ": " ^
        string_of_a_checked_layer l') in

    let l' =
      let l' = remove_relax l' in
      match catch_not_found (List.find
          (function (PAclause ("accessor", [_])) -> true | _ -> false))
          l.pLayerAnnotations with
      | None -> l'
      | Some (PAclause (_, PAclause ("LoadStoreSem1", _) :: _)) ->
        { l' with aCheckedLayerAccessor = AALoadStoreSem1 }
      | Some (PAclause (_, PAclause ("LoadStoreSem2", _) :: _)) ->
        { l' with aCheckedLayerAccessor = AALoadStoreSem2 }
      | Some (PAclause (_, PAclause ("LoadStoreSem3", _) :: _)) ->
        { l' with aCheckedLayerAccessor = AALoadStoreSem3 }
      | Some (PAclause (_, name)) ->
        report_warning ("Unrecognized accessor \"" ^
                          string_of_p_annotations name ^ "\" ignored") none;
        l'
      | _ -> assert false in

    let l' = match l.pLayerInvariant with
      | None -> l'
      | Some p -> { l' with
                    aCheckedLayerInvariant = Some (translate_proposition p) }

    in let _ =
      debug_endline ("Post remove " ^ l'.aCheckedLayerName ^ ": " ^
        string_of_a_checked_layer l')

    in l' in

  let generate_layer l =
    let rec normalize_checked_layer l base_opt refined_opt =
      match l.aCheckedLayerDesc with
      | ALrelax _ -> assert false

      | ALinst (l1, l2) ->
        let l1' = { l1 with
                    aCheckedLayerTipObjectSet = l.aCheckedLayerTipObjectSet;
                    aCheckedLayerKeelObjectSet = l.aCheckedLayerKeelObjectSet } in
        let l1' = if l.aCheckedLayerAccessor = AAnone
                    then l1'
                    else { l1' with aCheckedLayerAccessor = l.aCheckedLayerAccessor } in
        let l1' = if l.aCheckedLayerInvariant = None
                    then l1'
                    else { l1' with aCheckedLayerInvariant = l.aCheckedLayerInvariant }
        in normalize_checked_layer l1'
             (Some (normalize_checked_layer l2 base_opt None))
             refined_opt
      | ALrefine (l1, l2, p) ->
        let l1' = { l1 with
                    aCheckedLayerTipObjectSet = l.aCheckedLayerTipObjectSet;
                    aCheckedLayerKeelObjectSet = l.aCheckedLayerKeelObjectSet } in
        let l1' = if l.aCheckedLayerAccessor = AAnone
                    then l1'
                    else { l1' with aCheckedLayerAccessor = l.aCheckedLayerAccessor } in
        let l1' = if l.aCheckedLayerInvariant = None
                    then l1'
                    else { l1' with aCheckedLayerInvariant = l.aCheckedLayerInvariant }
        in normalize_checked_layer l1' None
             (Some (normalize_checked_layer l2 base_opt refined_opt, p))

      | ALconstr lst -> match base_opt, refined_opt with
        | Some _, Some _ ->
          report_error ("Refining with composed layers not supported") none;
          {
            aLayerName = l.aCheckedLayerName;
            (*aLayerSerial = get_layer_serial ();*)
            aLayerType = l.aCheckedLayerType;
            aLayerAccessor = l.aCheckedLayerAccessor;
            aLayerInvariant = l.aCheckedLayerInvariant;

            aLayerFreshObjects = lst;
            aLayerPassthroughObjects = [];
            aLayerAllObjects = lst;

            aLayerTipObjectSet = l.aCheckedLayerTipObjectSet;
            aLayerKeelObjectSet = l.aCheckedLayerKeelObjectSet;

            aLayerDesc = ALbottom
          }
        | None, None -> assert (l.aCheckedLayerPassthroughs.aLayerSignatureSlots = []);
          {
            aLayerName = l.aCheckedLayerName;
            (*aLayerSerial = get_layer_serial ();*)
            aLayerType = l.aCheckedLayerType;
            aLayerAccessor = l.aCheckedLayerAccessor;
            aLayerInvariant = l.aCheckedLayerInvariant;

            aLayerFreshObjects = lst;
            aLayerPassthroughObjects = [];
            aLayerAllObjects = lst;

            aLayerTipObjectSet = l.aCheckedLayerTipObjectSet;
            aLayerKeelObjectSet = l.aCheckedLayerKeelObjectSet;

            aLayerDesc = ALbottom
          }
        | Some base, None | None, Some (base, _) ->
          let base_objects = base.aLayerFreshObjects @ base.aLayerPassthroughObjects in
          let passthroughs = List.map (fun (s, sg) ->
            match catch_not_found (List.assoc s) base_objects with
            | None ->
              report_error ("Layer " ^ base.aLayerName ^
                ": does not contain or depend on slot " ^ s ^ "\nslots = [" ^
                String.concat "; " (List.map fst base_objects) ^ "]") none;
              s, dummy_object s
            | Some obj ->
              (* (* broke our totally fine MContainerImpl, likely would have
                    caught any problem in translate_layer_definition any way *)
              if not (signature_subseteq sg obj.aObjectType.aObjectSignature) then
                report_error ("Layer " ^ l.aCheckedLayerName ^
                  ": object under slot " ^ s ^ " has signature " ^
                  string_of_a_signature obj.aObjectType.aObjectSignature ^
                  " which is incompatible with passthrough requirement " ^
                  string_of_a_signature sg);
              *)
              s, obj
            ) l.aCheckedLayerPassthroughs.aLayerSignatureSlots in
          {
            aLayerName = l.aCheckedLayerName;
            (*aLayerSerial = get_layer_serial ();*)
            aLayerType = l.aCheckedLayerType;
            aLayerAccessor = if l.aCheckedLayerAccessor = AAnone
                               then base.aLayerAccessor
                               else l.aCheckedLayerAccessor;
            aLayerInvariant = l.aCheckedLayerInvariant;

            aLayerFreshObjects = lst;
            aLayerPassthroughObjects = passthroughs;
            aLayerAllObjects = lst @ base.aLayerAllObjects;

            aLayerTipObjectSet = l.aCheckedLayerTipObjectSet;
            aLayerKeelObjectSet = l.aCheckedLayerKeelObjectSet;

            aLayerDesc = match refined_opt with
              | None -> ALontop base
              | Some (refined, p) -> ALrefine_bottom (refined, p)
          } in

    (* Spit out all grounded layers that need C verification. *)
    let rec generate_declarations l =
      if not (Hashtbl.mem layer_declarations l.aLayerName) then
        let _ = Hashtbl.add layer_declarations l.aLayerName true in
        let _ = match l.aLayerDesc with
          | ALbottom -> ()
          | ALontop base -> generate_declarations base
          | ALrefine_bottom (impl, _) -> generate_declarations impl
          (*
          | ALrefine_ontop (_, impl) -> generate_declarations impl
          *)
        in add_declaration l.aLayerName (ADlayer l)
    in assert (l.aCheckedLayerType.aLayerBase.aLayerSignatureSlots = []);
       generate_declarations (normalize_checked_layer l None None) in

  let layer_generated = ref false in
  let last_layer = ref None in
  let _ = List.iter (function
    | i, {p_declaration_desc = (PDtype t); p_declaration_loc = _ } ->
      let t' = translate_type_def i t
      in if Hashtbl.mem type_environment i
           then report_error ("Type " ^ i ^ " already defined") t.p_type_FO_loc;
         Hashtbl.add type_environment i t';
         if t'.aTypeProvable
           then Hashtbl.add ctype_environment i t'.aTypeCtype;
         add_declaration i (ADtype t')
    | i, {p_declaration_desc = (PDevent e); p_declaration_loc = _ } ->
       let e' = translate_event_def i e in
       (if Hashtbl.mem event_environment i
        then (report_error ("Event " ^ i ^ " already defined") none)
        else Hashtbl.add event_environment i e');
       add_declaration i (ADevent e')
    | i, {p_declaration_desc =  (PDsignature s); p_declaration_loc = _ } ->
      if Hashtbl.mem signature_environment i
        then report_error ("Signature " ^ i ^ " already defined") s.p_signature_loc;
      Hashtbl.add signature_environment i (translate_signature i s)
    | i, {p_declaration_desc =  (PDlayer_sig s); p_declaration_loc = _ } ->
      if Hashtbl.mem layer_signature_environment i
        then report_error ("Layer signature " ^ i ^ " already defined") s.p_layer_signature_loc;
      Hashtbl.add layer_signature_environment i (translate_layer_signature i s)
    | i,{p_declaration_desc =  (PDobject o); p_declaration_loc = _ } ->
      let o' = translate_object_definition i o (* in
      let _ = match o.pObjectDesc with
        | POconstr _ -> add_declaration i (ADobject o')
        | _ -> ()
      *)
      in if Hashtbl.mem object_environment i
           then report_error ("Object " ^ i ^ " already defined") o.pObjectLoc;
         Hashtbl.add object_environment i o'
    | i, {p_declaration_desc =  (PDlayer l); p_declaration_loc = _ } -> begin
      if Hashtbl.mem layer_environment i
        then report_error ("Layer " ^ i ^ " already defined") l.pLayerLoc;
      let l' = translate_layer_definition i l
      in Hashtbl.add layer_environment i l';
         match List.mem (PAclause ("codegen", [])) l.pLayerAnnotations,
               l'.aCheckedLayerType.aLayerBase.aLayerSignatureSlots = [] with
         | true, true ->
           generate_layer l';
           layer_generated := true;
           last_layer := None
         | true, false ->
           report_warning ("Layer " ^ i ^
             " is not ground hence cannot do codegen yet marked so") none
         | false, true ->
           last_layer := Some l'
         | _, _ -> ()
      end
    | _, {p_declaration_desc =  PDexternal_with (s, ann) ; p_declaration_loc = _} ->
      let rec scrape_clauses acc = function
        | [] -> acc
        | PAclause (c, ls) :: res -> scrape_clauses (c :: acc) (ls @ res)
        | _ :: res -> scrape_clauses acc res in
      let rec find_exceptions = function
        | [] -> []
        | PAclause ("except", ls) :: res -> scrape_clauses [] (ls @ res)
        | _ :: res -> find_exceptions res
      in add_external_verabtim s (find_exceptions ann)
    | i, {p_declaration_desc =  PDexternal_type (s, ill_opt, ann) ; p_declaration_loc = loc }->
      if Hashtbl.mem type_environment i
        then report_error ("Type " ^ i ^ " already defined") loc;
      if List.mem (PAclause ("as", [PAclause ("global_abstract_data_type", [])]))
                  ann
        then global_abstract_data_type_store := Some i;
      Hashtbl.add type_environment i
        { aTypeDesc = ATexternal {
            aEXTypeName = i;
            aEXTypeString = s;
            aEXTypeLowLevelInv = ill_opt;
            aEXTypeDefault = None };
          aTypeCtype = ACtint;
          aTypePairIdent = "t" ^ i;
          aTypeProvable = false
        }
    | i, {p_declaration_desc =  PDexternal_const (s, t, ann) ; p_declaration_loc = loc } ->
      if Hashtbl.mem external_const_environment i
        then report_error ("External constant " ^ i ^ " already defined") loc;
      let t' = translate_type t in
      (* Check if the type [t'] is an external type without a default value yet *)
      let _ = match t'.aTypeDesc with
        | ATexternal ({ aEXTypeDefault = None } as extype) ->
          (* None found--set ourselves as one *)
          extype.aEXTypeDefault <- Some (s, i);
          (* global_abstract_data_type is the universe and can't contain itself
             --don't generate declaration like other small types *)
          begin match !global_abstract_data_type_store with
          | Some glb_i when glb_i = extype.aEXTypeName -> ()
          | _ -> add_declaration extype.aEXTypeName (ADtype t')
          end
        | _ -> ()
      in Hashtbl.add external_const_environment i
           { aBigExprDesc = AEexternal_const (s, i);
             aBigExprType = t'
           };
         add_folded_unfolding_symbol ann s loc
    | i, {p_declaration_desc =  PDexternal_function (s, arg, ret, ann); p_declaration_loc = loc } ->
      if Hashtbl.mem external_function_environment i
        then report_error ("External function " ^ i ^ " already defined") loc;
      Hashtbl.add external_function_environment i
        (s, { aMethodArgumentTypes = unfold_arg_type arg;
              aMethodReturnType = translate_type ret;
              aMethodKind = MKconst
            });
      add_folded_unfolding_symbol ann s loc
    | i, {p_declaration_desc =  PDexternal_prop (s, _) ; p_declaration_loc = _ } ->  (* ignoring the type given *)
      Hashtbl.add external_prop_environment i s
  ) parsed in

  let _ = match !last_layer with
    | Some l -> generate_layer l
    | None ->
      if not !layer_generated then
        (report_warning "No layer eligible for codegen" none) in
  let _ = Hashtbl.iter (fun i t -> match t.aTypeDesc with
    | ATexternal ({ aEXTypeDefault = None } as extype) ->
      (report_error ("No value of external type " ^ extype.aEXTypeName ^ " (" ^
        extype.aEXTypeString ^ ") supplied, cannot pick a default value") none)
    | _ -> ()) type_environment in
  let global_abstract_data_type = match !global_abstract_data_type_store with
    | None -> None
    | Some i -> match catch_not_found (Hashtbl.find type_environment) i with
      | Some { aTypeDesc = ATexternal ({ aEXTypeDefault = Some _ } as extype) }
        -> Some extype
      | Some { aTypeDesc = ATexternal _ } ->
        None  (* Already reported error for external type w/o default value *)
      | _ ->
        report_error "Internal error: global_abstract_data_type not external/not found" none;
        None

  in !has_failure,
     { aFileDeclarations = get_declarations ();
       aFileGlobalAbstractDataType = global_abstract_data_type;
       aFileExternalVerbatim = get_external_verbatim ();
       aFileExternalSymbols = get_external_symbols ();
      }
