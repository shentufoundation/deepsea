(* this file should generate UCLID5 code for the defi-verification project *)
(* print from Ast*)

(* type utype = 
  | UTinteger
  | UTboolean
  | UTrecord of ident * (ident * utype) list (* type result_t = record {valid : boolean, value : integer} *)
  | UTmapping of utype * utype

type uval = 
  | UVinteger of int
  | UVboolean of bool
  | UVident of string
  | UVfield of string * string *)

(* write string printing first, then write ast *)

open Ast
open Astcommon
open Abi

type uclidgen_env = {
  project_name : string;
}

let module_name_regexp = Str.regexp "[a-zA-Z_][a-zA-Z0-9_']*"
let default_module_name = "EdsgerGen"

let new_uclidgen_env filename  ast =
  let project_name =
    let basename = Filename.basename filename in
    if Str.string_match module_name_regexp basename 0 then
      Str.matched_string basename
    else begin
      print_endline ("Cannot use prefix of file name '" ^ basename ^
        "' as Coq module name: default to '" ^ default_module_name ^ "'");
      default_module_name
    end in
  let _ = try Unix.mkdir project_name 0o777
          with Unix.Unix_error (Unix.EEXIST, _, _) -> () in
  (*make the extract dir*)
#ifndef REDACTED
  (* let __ = try Unix.mkdir (project_name ^ "/" ^ "extraction") 0o777
          with Unix.Unix_error (Unix.EEXIST, _, _) -> () in *)
#endif
  {
    project_name = project_name;
  }

let uclidgen_fatal_error loc func msg =
  print_endline ("CoqGen error at " ^ loc ^ " (" ^ func ^ "): " ^ msg);
  assert false

 let builtin_base_layer_name = "BuiltinBase"

 let builtin_base_layer =
  { aLayerName = builtin_base_layer_name;
    aLayerType = {
      aLayerBase = {
        aLayerSignatureName = builtin_base_layer_name ^ "_base";
        aLayerSignatureSlots = [];
        aLayerSignatureSuppressError = false
      };
      aLayerSignature = {
        aLayerSignatureName = builtin_base_layer_name ^ "_inter";
        aLayerSignatureSlots = [];
        aLayerSignatureSuppressError = false
      }
    };

    aLayerAccessor = AAnone;
    aLayerInvariant = None;

    aLayerFreshObjects = [];
    aLayerPassthroughObjects = [];
    aLayerAllObjects = [];

    aLayerTipObjectSet = IdentSet.empty;
    aLayerKeelObjectSet = IdentSet.empty;

    aLayerDesc = ALbottom
  }


let unmingledFieldName l o f = 
  (* l.aLayerName ^ "_" ^   *)
  o.aObjectName ^ "_" ^ f.aObjectFieldName

let is_map a =
  match a with
  | ATmapping (t1, t2) -> true
  | _ -> false

module SS = Set.Make(String)

let str_binop_uclid = function
  | OPplus -> "+"
  | OPminus -> "-"
  | OPtimes -> "*"
  | OPdivide -> "/"
  | OPremainder -> "%"
  | OPand -> "&&"
  | OPor -> "||"
  | OPeq -> "="
  | OPne -> "!="
  | OPlt -> "<"
  | OPle -> "<="
  | OPgt -> ">"
  | OPge -> ">="
  | OPshl -> "<<"
  | OPshr -> ">>"
  | OPxor -> "^"
  | OPbitand -> "&"
  | OPbitor -> "|"
  | OPsha_2 -> "keccak256"

let rec str_atype_uclid a =
  match a with
  | ATbuiltin Tint -> "integer"
  | ATbuiltin Tuint -> "integer" (* TODO: use clight builtin type *)
  | ATbuiltin Tbool -> "boolean"
  | ATbuiltin Tunit -> "()"
  | ATbuiltin Taddress -> "address"
  | ATbuiltin Thashvalue -> "hashvalue"
  | ATbuiltin Tglobalpointer -> "globalpointer"
  (*
  | ATbuiltin Tval -> "val"
  | ATbuiltin Tflatmem -> "flatmem"
  *)
  | ATdata (i, _) -> i
  | ATprod (t1, t2) -> "(" ^ str_atype_uclid t1.aTypeDesc ^ ", "
                              ^ str_atype_uclid t2.aTypeDesc ^ ")"
  (* | ATarray (n, t) -> "AR" ^ string_of_int n ^ "_" ^ a_type_desc_to_ident t.aTypeDesc *)
  | ATmapping (t1, t2) -> "[" ^ str_atype_uclid t1.aTypeDesc ^ "] " ^ str_atype_uclid t2.aTypeDesc
  | ATlist t -> "list_" ^ str_atype_uclid t.aTypeDesc
  | ATexternal extype -> extype.aEXTypeName

let get_map_idx_type a =
  match a with
  | ATmapping (t1, t2) -> str_atype_uclid t1.aTypeDesc
  | _ -> uclidgen_fatal_error __LOC__ "get_map_idx_type"
  "getting index of non-mapping"

let str_constant c =
  match c with
  | CONint n -> string_of_int n
  | CONuint n -> string_of_int n
  | CONbool b -> if b then "true" else "false"
  | CONunit -> "()"
  (* initial constant is the same as uint 0 *)
  | CONaddress addr -> Backend.BinNumsExt.numstring2decimalstring addr
  | _ -> uclidgen_fatal_error __LOC__ "str_constant"
  "constant type not supported"

let rec str_a_compile_time_constant = function
  | ACONconst c -> str_constant c
  | _ -> uclidgen_fatal_error __LOC__ "str_a_compile_time_constant"
  "does not support complex compile time constants"

let rec str_rexpr_uclid e o = 
  match e.aRexprDesc with
  | AEconst c -> str_constant c
  | AEconstr_val (c, ls) -> uclidgen_fatal_error __LOC__ "str_rexpr_uclid"
    "AEconstr_val  not supported"
  | AEtemp (n, i) -> 
    if String.equal i "resultU" then
      "    r = resultU;"
    else
      i
  | AEunop (op, e) -> 
    string_of_unop op ^ str_rexpr_uclid e o
  | AEbinop (op, e1, e2) ->
      ("(" ^ str_rexpr_uclid e1 o ^ " " ^ str_binop_uclid op ^ " " ^
      str_rexpr_uclid e2 o ^ ")")
  | AEbuiltin ("address",[]) ->
    (* need walk ast to insert actual addresses*)
    (match o.aObjectAddress with
    | Some addr -> 
      o.aObjectName ^ "_address"
      (* Backend.BinNumsExt.numstring2decimalstring addr *)
    | None -> 
      uclidgen_fatal_error __LOC__ "str_rexpr_uclid"
      ("does not support unlinked (address) object"))
  | AEbuiltin ("caller",[]) ->
     "msg_caller"
  | AEbuiltin (other,args) -> uclidgen_fatal_error __LOC__ "output_rexpr"
               ("Internal error, encountered unknown builtin \""^other^"\".")

exception PrimitiveNotFound of string

let rec str_lexpr_uclid e o = 
  match e.aLexprDesc with
  | AEglob i -> o.aObjectName ^ "_" ^ i
  | AEfield (e, i) -> str_lexpr_uclid e o ^ "." ^ i
  | AEindex (e1, e2) -> str_lexpr_uclid e1 o ^ "[" ^ str_rexpr_uclid e2 o ^ "]"

let rec str_lexpr_modified_uclid e o modified = 
  match e.aLexprDesc with
  | AEglob i -> SS.add (o.aObjectName ^ "_" ^ i) modified
  | AEfield (e, i) -> str_lexpr_modified_uclid e o modified
  | AEindex (e1, e2) -> 
    (str_lexpr_modified_uclid e1 o modified)

let rec str_cmd_uclid acmd base_layer o idnt = 
  let prefix = o.aObjectName ^ "_" in 
  match acmd.aCmdDesc with
  | ACskip -> 
    ""
    (* idnt ^ "skip;\n" *)
  | ACyield e -> 
    (* UCLID does not have the distinction *)
    str_rexpr_uclid e o
  | AClet (n, x, c1, c2) ->
    (* match c1 with ACcond here to hack if-then-else *)
    (* match c1 with ACcall here to hack function calling *)
    (* let declp = (if String.equal (str_atype_uclid c1.aCmdType.aTypeDesc) "()" then
      ""
    else 
      idnt ^ "var " ^ x ^ " : " ^ str_atype_uclid c1.aCmdType.aTypeDesc ^ ";\n")
    in *)
    let declp = "" in
    (match c1.aCmdDesc with
    | ACcond (e, c1', c2') -> 
      declp ^
      idnt ^ "if (" ^ str_rexpr_uclid e o ^ ")\n" ^ 
      idnt ^ "{\n" ^ 
      idnt ^ str_cmd_uclid c1' base_layer o (idnt ^ "  ") ^ "\n" ^ 
      idnt ^ "  " ^ x ^ " = ;\n" ^ 
      idnt ^ "}\n" ^ 
      idnt ^ "else\n" ^ 
      idnt ^ "{\n" ^ 
      idnt ^ str_cmd_uclid c2' base_layer o (idnt ^ "  ") ^ "\n" ^
      idnt ^ "  " ^ x ^ " = ;\n" ^ 
      idnt ^ "}\n"
    | ACcall (s, f, es, _, _) -> 
      (match o.aObjectAddress with
      | Some addr -> 
        let o' = try (List.assoc s base_layer.aLayerAllObjects)
                with Not_found -> raise (PrimitiveNotFound s) in
        
        
        let x' = (if String.equal (str_atype_uclid (List.find (fun m -> m.aMethodName = f) o'.aObjectMethods).aMethodType.aMethodReturnType.aTypeDesc) "()" then
          idnt ^ "call () = " ^ (o'.aObjectName ^ "_" ^ f ^ " (")
        else 
          idnt ^ "call " ^ "(" ^ x ^ ")") ^ " = " ^ (o'.aObjectName ^ "_" ^ f ^ " (")
        in

        declp ^ x' ^ 
        String.concat ", " (List.map (fun e -> let p = (str_rexpr_uclid e o) in if String.equal p "()" then "" else p) es) ^ ", " ^ 
        (* (Backend.BinNumsExt.numstring2decimalstring addr) *)
        prefix ^ "address"
        ^ ");\n"
      | None -> 
        uclidgen_fatal_error __LOC__ "str_cmd_uclid"
        ("does not support unlinked (address) object"))
    | _ -> 
      declp ^
      idnt ^ x ^ " = " ^ str_cmd_uclid c1 base_layer o idnt ^ ";\n") ^ 

    str_cmd_uclid c2 base_layer o idnt
  | ACsequence (c1, c2) -> 
    str_cmd_uclid c1 base_layer o idnt ^
    str_cmd_uclid c2 base_layer o idnt
    | ACcall (s, f, es, _, _) -> 
    (match o.aObjectAddress with
    | Some addr -> 
      let o' = try (List.assoc s base_layer.aLayerAllObjects)
              with Not_found -> raise (PrimitiveNotFound s) in
      let calledt = (if String.equal (str_atype_uclid acmd.aCmdType.aTypeDesc) "()" then
          "()"
        else 
          "(XXXXX)")
      in
      idnt ^ "call " ^ calledt ^ " = " ^ (o'.aObjectName ^ "_" ^ f ^ " (") ^ 
      String.concat ", " (List.map (fun e -> 
        let p = (str_rexpr_uclid e o) in if String.equal p "()" then "" else p) es) ^ ", " ^ 
      (* (Backend.BinNumsExt.numstring2decimalstring addr) *)
      prefix ^ "address"
       ^ ");\n"
    | None -> 
      uclidgen_fatal_error __LOC__ "str_cmd_uclid"
      ("does not support unlinked (address) object"))
      (* "call " ^ s ^ "." ^ f ^ " (" ^
      String.concat ", " (List.map string_of_a_rexpr es) ^ ")" *)
  | ACcond (e, c1, c2) -> 
    idnt ^ "if (" ^ str_rexpr_uclid e o ^ ")\n" ^ 
    idnt ^ "{\n" ^ 
    idnt ^ str_cmd_uclid c1 base_layer o (idnt ^ "  ") ^ "\n" ^ 
    idnt ^ "}\n" ^ 
    idnt ^ "else\n" ^ 
    idnt ^ "{\n" ^ 
    idnt ^ str_cmd_uclid c2 base_layer o (idnt ^ "  ") ^ "\n" ^
    idnt ^ "}\n"
  | ACfor (ni, i, l, nh, h, c, occ) ->
    idnt ^ "var " ^ i ^ " : integer;\n" ^ 
    idnt ^ i ^ " = " ^ str_rexpr_uclid l o ^ ";\n" ^ 
    idnt ^ "while (" ^ i ^ " < " ^ str_rexpr_uclid h o ^ ") { \n" ^ 
    str_cmd_uclid c base_layer o (idnt ^ "  ") ^ 
    idnt ^ "}\n"
  | ACmatch (e, cls) ->
    uclidgen_fatal_error __LOC__ "str_cmd_uclid"
    ("does not support ACmatch")
  | ACemit (et, es) -> 
    uclidgen_fatal_error __LOC__ "str_cmd_uclid"
    ("does not support ACemit")
  | ACload e -> 
    str_lexpr_uclid e o
  | ACstore (v, e) -> 
    (* should be same as normal assign *)
    idnt ^ str_lexpr_uclid v o ^ " = " ^ str_rexpr_uclid e o ^ ";\n"
  | ACconstr (v, e) ->
    uclidgen_fatal_error __LOC__ "str_cmd_uclid"
    ("does not support ACconstr")
  | ACfail ->
    uclidgen_fatal_error __LOC__ "str_cmd_uclid"
    ("does not support ACfail")
  | ACassert c -> 
    idnt ^ "assume (" ^ str_cmd_uclid c base_layer o idnt ^ ");\n"
  | ACdeny c ->
    uclidgen_fatal_error __LOC__ "str_cmd_uclid"
    ("does not support ACdeny")
  | ACghost c ->
    uclidgen_fatal_error __LOC__ "str_cmd_uclid"
    ("does not support ACghost")
  | ACfirst (ni, i, l, nh, h, nc, c1, c2, c3, occ) ->
    uclidgen_fatal_error __LOC__ "str_cmd_uclid"
    ("does not support ACfirst")
  | ACfold (ni, i, l, nh, h, na, a, s, nc, c, occ) ->
    uclidgen_fatal_error __LOC__ "str_cmd_uclid"
    ("does not support ACfold")
  | ACexternal (dest, s, i, args) ->
    uclidgen_fatal_error __LOC__ "str_cmd_uclid"
    ("does not support ACexternal")

let rec get_modified_uclid acmd base_layer o modified =
  let prefix = o.aObjectName ^ "_" in 
  match acmd.aCmdDesc with
  | ACskip -> 
    modified
  | ACyield e -> 
    (* UCLID does not have the distinction *)
    modified
  | AClet (n, x, c1, c2) ->
    SS.union (get_modified_uclid c1 base_layer o modified) (get_modified_uclid c2 base_layer o modified)
  | ACsequence (c1, c2) -> 
    SS.union (get_modified_uclid c1 base_layer o modified) (get_modified_uclid c2 base_layer o modified)
  | ACcall (s, f, es, _, _) -> 
    let o' = try (List.assoc s base_layer.aLayerAllObjects)
            with Not_found -> raise (PrimitiveNotFound s) in
    get_modified_uclid (List.find (fun m -> m.aMethodName = f) o'.aObjectMethods).aMethodBody base_layer o' modified
  | ACcond (e, c1, c2) -> 
    SS.union (get_modified_uclid c1 base_layer o modified) (get_modified_uclid c2 base_layer o modified)
  | ACfor (ni, i, l, nh, h, c, occ) ->
    (get_modified_uclid c base_layer o modified)
  | ACmatch (e, cls) ->
    uclidgen_fatal_error __LOC__ "str_cmd_uclid"
    ("does not support ACmatch")
  | ACemit (et, es) -> 
    uclidgen_fatal_error __LOC__ "str_cmd_uclid"
    ("does not support ACemit")
  | ACload e -> 
    modified
  | ACstore (v, e) -> 
    (* should be same as normal assign *)
    str_lexpr_modified_uclid v o modified
  | ACconstr (v, e) ->
    uclidgen_fatal_error __LOC__ "str_cmd_uclid"
    ("does not support ACconstr")
  | ACfail ->
    uclidgen_fatal_error __LOC__ "str_cmd_uclid"
    ("does not support ACfail")
  | ACassert c -> 
    (get_modified_uclid c base_layer o modified)
  | ACdeny c ->
    uclidgen_fatal_error __LOC__ "str_cmd_uclid"
    ("does not support ACdeny")
  | ACghost c ->
    uclidgen_fatal_error __LOC__ "str_cmd_uclid"
    ("does not support ACghost")
  | ACfirst (ni, i, l, nh, h, nc, c1, c2, c3, occ) ->
    uclidgen_fatal_error __LOC__ "str_cmd_uclid"
    ("does not support ACfirst")
  | ACfold (ni, i, l, nh, h, na, a, s, nc, c, occ) ->
    uclidgen_fatal_error __LOC__ "str_cmd_uclid"
    ("does not support ACfold")
  | ACexternal (dest, s, i, args) ->
    uclidgen_fatal_error __LOC__ "str_cmd_uclid"
    ("does not support ACexternal")

let rec get_vardecl_uclid acmd base_layer o modified = 
  let prefix = o.aObjectName ^ "_" in 
  match acmd.aCmdDesc with
  | ACskip -> 
    modified
  | ACyield e -> 
    (* UCLID does not have the distinction *)
    modified
  | AClet (n, x, c1, c2) ->
    let mod' = if String.equal x "_" then modified else
      SS.add ("    var " ^ x ^ " : " ^ str_atype_uclid c1.aCmdType.aTypeDesc ^ ";\n") modified 
    in
    SS.union (get_vardecl_uclid c1 base_layer o mod') (get_vardecl_uclid c2 base_layer o mod')
  | ACsequence (c1, c2) -> 
    SS.union (get_vardecl_uclid c1 base_layer o modified) (get_vardecl_uclid c2 base_layer o modified)
  | ACcall (s, f, es, _, _) -> 
    modified
  | ACcond (e, c1, c2) -> 
    SS.union (get_vardecl_uclid c1 base_layer o modified) (get_vardecl_uclid c2 base_layer o modified)
  | ACfor (ni, i, l, nh, h, c, occ) ->
    (get_vardecl_uclid c base_layer o modified)
  | ACmatch (e, cls) ->
    uclidgen_fatal_error __LOC__ "str_cmd_uclid"
    ("does not support ACmatch")
  | ACemit (et, es) -> 
    uclidgen_fatal_error __LOC__ "str_cmd_uclid"
    ("does not support ACemit")
  | ACload e -> 
    modified
  | ACstore (v, e) -> 
    (* should be same as normal assign *)
    modified
  | ACconstr (v, e) ->
    uclidgen_fatal_error __LOC__ "str_cmd_uclid"
    ("does not support ACconstr")
  | ACfail ->
    uclidgen_fatal_error __LOC__ "str_cmd_uclid"
    ("does not support ACfail")
  | ACassert c -> 
    (get_vardecl_uclid c base_layer o modified)
  | ACdeny c ->
    uclidgen_fatal_error __LOC__ "str_cmd_uclid"
    ("does not support ACdeny")
  | ACghost c ->
    uclidgen_fatal_error __LOC__ "str_cmd_uclid"
    ("does not support ACghost")
  | ACfirst (ni, i, l, nh, h, nc, c1, c2, c3, occ) ->
    uclidgen_fatal_error __LOC__ "str_cmd_uclid"
    ("does not support ACfirst")
  | ACfold (ni, i, l, nh, h, na, a, s, nc, c, occ) ->
    uclidgen_fatal_error __LOC__ "str_cmd_uclid"
    ("does not support ACfold")
  | ACexternal (dest, s, i, args) ->
    uclidgen_fatal_error __LOC__ "str_cmd_uclid"
    ("does not support ACexternal")

let gen_uclid env final_layer fileDeclarations = 
  let out = open_out (env.project_name ^ "/dsmodel.ucl") in

  let rec indent lvl = 
    if lvl = 0 then "" else "  "  ^ indent (lvl - 1)
  in
  let iter_fields action =
    List.iter (fun (_, o) ->
      List.iter (fun f -> action o f
    ) o.aObjectFields
   ) final_layer.aLayerAllObjects
 in
 let has_fields =
   List.exists (fun (_,o) ->
       List.exists (fun f -> true
         ) o.aObjectFields
     ) final_layer.aLayerAllObjects
 in
 let unmingledFieldName _o f = 
   unmingledFieldName final_layer _o f
 in

 let record_field_leftover = ref "\n  " in

 output_string out
"
/* file automatically generated by DeepSEA */

module dsmodel {

  type address = integer;

  input me_caller : address;
  input hackaux : address;
  input victim : address;

";

 (* print the this_address declarations *)
 List.iter (function
 | i, ADlayer l ->
   List.iter (fun (_, o) ->
     match o.aObjectAddress with
     | Some ad -> 
     output_string out ("  const " ^ o.aObjectName ^ 
       "_address : address = " ^ (Backend.BinNumsExt.numstring2decimalstring ad) ^ ";\n")
     | None -> 
       ()
   ) l.aLayerFreshObjects
 | _, _ -> ()
 ) fileDeclarations;

 (* print the state variable declarations, need to also generate axioms *)
 (* TODO: add axioms generation *)
 iter_fields (fun _o f ->
     output_string out ("  var " ^ unmingledFieldName _o f ^ " : ");
     output_string out (str_atype_uclid f.aObjectFieldType.aTypeDesc);
     output_string out ";\n"
     (* output_type_expr out "    " f.aObjectFieldType; *)
 );
 output_string out "  var dummy : integer;\n";

 (* print the actual procedures *)
 List.iter (function
 | i, ADlayer l ->
  let has_base_layer, base_layer =
    match l.aLayerDesc with
    | ALbottom ->                false, builtin_base_layer
    | ALontop l' ->              true,  l'
    | ALrefine_bottom (l', p) -> true,  l' in

   List.iter (fun (_, o) ->
     List.iter (fun m ->
       let method_full_name = o.aObjectName ^ "_" ^ m.aMethodName in
       let mt = m.aMethodType in
       (* prepare_command env out base_layer o.aObjectName method_full_name
                       false [] Typecheck.function_start_id_num m.aMethodBody; *)
       output_string out ("\n  procedure " ^ method_full_name ^ " (");
       (if (List.length m.aMethodArguments = 1) && (String.equal "()" (List.hd m.aMethodArguments))
       then 
        ()
       else
          List.iteri (
            fun idx arg -> 
              output_string out arg;
              output_string out (" : ");
              output_string out (str_atype_uclid (List.nth mt.aMethodArgumentTypes idx).aTypeDesc);
              (* (if idx + 1 != (List.length m.aMethodArguments) then  *)
                output_string out ", "
              (* else 
                ()) *)
          ) m.aMethodArguments;
          output_string out "msg_caller : address"
          );

       (* ouput_params out m.aMethodArguments mt.aMethodArgumentTypes; *)
       
       output_string out (if String.equal (str_atype_uclid mt.aMethodReturnType.aTypeDesc) "()" then
       ")\n"
      else 
       ")\n    returns (r : " ^ str_atype_uclid mt.aMethodReturnType.aTypeDesc ^ ")\n");
       let modd = (get_modified_uclid m.aMethodBody base_layer o SS.empty) in
       (if SS.cardinal modd = 0 then
        output_string out ""
       else
        (output_string out "    modifies ";
        SS.iter (fun e -> output_string out (e ^ ", ")) modd;
        output_string out "dummy";
        output_string out ";\n"));
       output_string out "  {\n";

       let vardecls = (get_vardecl_uclid m.aMethodBody base_layer o SS.empty) in
       SS.iter (fun e -> output_string out (e)) vardecls;

       output_string out (str_cmd_uclid m.aMethodBody base_layer o (indent 2));
       (* output_command env out base_layer i method_full_name "      " false [] m.aMethodBody; *)
       output_string out "\n  }\n"   
       (* output_string out ("unfold " ^ method_full_name ^ "_opt in *;\n") *)
     ) o.aObjectMethods
   ) l.aLayerFreshObjects
 | _, _ -> ()
) fileDeclarations;

 (* print the init section *)
 output_string out
"\n  init {
";

 (* print the state variable definitions in init {} *)
 if has_fields then begin
  iter_fields (fun _o f ->
      (if is_map f.aObjectFieldType.aTypeDesc then
        (* assume (forall (i : integer) :: balances[i] == 0); *)
        (output_string out "    assume (forall (i : ";
        output_string out (get_map_idx_type f.aObjectFieldType.aTypeDesc);
        output_string out ") :: ";
        output_string out (unmingledFieldName _o f ^ "[i] == 0);\n");
        ())
      else
        (output_string out ("    " ^ unmingledFieldName _o f ^ " = ");
        (* output_compile_time_constant out "      " f.aObjectFieldInitial; *)
        output_string out (str_a_compile_time_constant f.aObjectFieldInitial);
        output_string out ";\n";
        ()))
  )
  end;
  output_string out
"  }\n";

  (* print the transitions, need function signatures *)
  let transfl = List.fold_left (fun acc ee ->
    match ee with
    | i, ADlayer l ->
    let has_base_layer, base_layer =
      match l.aLayerDesc with
      | ALbottom ->                false, builtin_base_layer
      | ALontop l' ->              true,  l'
      | ALrefine_bottom (l', p) -> true,  l' in
      List.fold_left (fun acc' (_, o) ->
        List.fold_left (fun acc'' m ->
          let method_full_name = o.aObjectName ^ "_" ^ m.aMethodName in
          (* (m, string_of_method_kind m.aMethodType.aMethodKind) :: acc'' *)
          match m.aMethodType.aMethodKind with
          (* find-grained search that only considers functions annotated with logical *)
          | MKrefined -> (m, method_full_name) :: acc''
          | _ -> acc''
        ) acc' o.aObjectMethods
      ) acc l.aLayerFreshObjects
    | _, _ -> acc
  ) [] fileDeclarations 
  in

  (* print the this_address declarations *)
  let possible_addrs = List.fold_left (fun acc ee ->
    match ee with
    | i, ADlayer l ->
      List.fold_left (fun acc' (_, o) ->
        match o.aObjectAddress with
        | Some ad -> 
          (o.aObjectName ^ "_address") :: acc'
        | None -> 
          acc'
      ) acc l.aLayerFreshObjects
    | _, _ -> acc
    ) [] fileDeclarations
  in

  output_string out 
("
  next {
    var i : integer;
    assume (i >= 1 && i <= " ^ string_of_int (List.length transfl) ^ ");

    case 
");

  let idnt = indent 3 in

  List.iteri (fun idx e ->
    match e with
    | (ee, eename) ->
      output_string out ("    (i == " ^ string_of_int (idx+1) ^ ") : {\n");
      (* generate dummy return values *)
      output_string out (if String.equal (str_atype_uclid ee.aMethodType.aMethodReturnType.aTypeDesc) "()" then
        ""
        else 
        (idnt ^ "var r : " ^ str_atype_uclid ee.aMethodType.aMethodReturnType.aTypeDesc ^ ";\n"));
      (* traverse the parameter declarations *)
      (if (List.length ee.aMethodArguments = 1) && (String.equal "()" (List.hd ee.aMethodArguments))
        then 
        () (* if thre is no parameter needed *)
        else
        List.iteri (
          fun idx arg -> 
            output_string out (idnt ^ "var ");
            output_string out arg;
            output_string out (" : ");
            output_string out (str_atype_uclid (List.nth ee.aMethodType.aMethodArgumentTypes idx).aTypeDesc);
            (* (if idx + 1 != (List.length m.aMethodArguments) then  *)
              output_string out ";\n"
            (* else 
              ()) *)
        ) ee.aMethodArguments;
      );
      (* traverse the parameter assumptions *)
      (if (List.length ee.aMethodArguments = 1) && (String.equal "()" (List.hd ee.aMethodArguments))
        then 
        () (* if thre is no parameter needed *)
        else
        List.iteri (
          fun idx arg -> 
            let type_str = (str_atype_uclid (List.nth ee.aMethodType.aMethodArgumentTypes idx).aTypeDesc) in
            (if String.equal type_str "integer"
            then 
              output_string out (idnt ^ "assume (" ^ arg ^ " >= 0 && " ^ arg ^ " <= 100000000);\n")
            else 
              (if String.equal type_str "address"
              then 
                (* (output_string out (idnt ^ "// MANUAL : need to iterate through all possible addresses (oracle)\n"); *)
                (output_string out (idnt ^ "assume (");
                List.iter (
                  fun eee -> output_string out ("(" ^ arg ^ " == " ^ eee ^ ") || ")
                ) possible_addrs;
                output_string out ("(" ^ arg ^ " == hackaux) || " ^ "(" ^ arg ^ " == victim)" ^ ");\n"))
              else
                uclidgen_fatal_error __LOC__ "uclidgen"
                ("only address and integer params are permitted for now")))
        ) ee.aMethodArguments;
      );
      (* make the actual call *)
      output_string out (idnt ^ "call " ^ 
        (if String.equal (str_atype_uclid ee.aMethodType.aMethodReturnType.aTypeDesc) "()" then
          "()"
        else 
          "(r)") ^ " = " ^ eename);
      (if (List.length ee.aMethodArguments = 1) && (String.equal "()" (List.hd ee.aMethodArguments))
        then 
          output_string out "(me_caller);\n" (* if thre is no parameter needed *)
        else
          (output_string out " (";
          List.iteri (
            fun idx arg -> 
              output_string out arg;
                output_string out ", "
          ) ee.aMethodArguments;
          output_string out "me_caller);\n")
      );
      output_string out "    }\n"
    | _ -> uclidgen_fatal_error __LOC__ "uclidgen"
      ("illegal pattern when emitting transition calls in next{} ")
  ) transfl;

  output_string out "    esac
  }
";
  output_string out "\n}\n\n";

  (* print the main module which does the search *)
  output_string out
"module main {
 
  type address = integer;

  // MANUAL : set the actual hacker address
  const me_caller : address = XXXXX;
  const hackaux : address = XXXXX;
  const victim : address = XXXXX;

  instance m : dsmodel (me_caller : (me_caller), hackaux : (hackaux), victim : (victim));

  init {
    // give initial values
    // init (m);

    // MANUAL :  set initial capital, need manual input
    assume (m.token_balances[me_caller] == XXXXX);
    assume (m.token1_balances[me_caller] == XXXXX);
  }

  next {
    next (m);
  }

  // does not exist a state where after that state the hacker's money is more than before
  // this models a flashloan attack, where everything happens in one single moment
  // we should also add an axiom saying that no simple arbitrage exists, that CEX price is same as DEX
  // CANNOT 10x in 5 steps
  invariant no_arb : m.token_balances[me_caller] < (XXXXX * 10);

  // if there is no_arbitrage condition in state s, (meaning symbolically every asset is balanced with each other)
  //  then there is no_arbitrage in any future state unless somebody does a stupic thing, but there is no one stupid
  //  since we are only targeting flash-loan attacks here
  property[LTL] no_arbitrage : !F(m.token_balances[me_caller] > (XXXXX * 10));

  control {
    v = bmc(6); // what is difference between bmc and unroll?
    unroll (5);
    check;
    print_results;
  }

}\n"

let uclidgen filename ast =
  let final_layer = ref None in
  let env = new_uclidgen_env filename ast
  in 
     List.iter (function
     | i, ADtype t -> ()
     | i, ADevent _ -> ()
     | i, ADlayer l ->
       final_layer := Some l
     ) ast.aFileDeclarations;
     begin match !final_layer with
     | None -> ()
     | Some l -> gen_uclid env l ast.aFileDeclarations;
      (* gen_global_abstract_data_type env l ast.aFileGlobalAbstractDataType ast.aFileDeclarations *)
     end
     (* delete_uclidgen_env env *)
