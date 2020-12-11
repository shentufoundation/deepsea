open Ast
open Astcommon
open Abi

open Backend
  open StmtMiniC
  open ExpMiniC
  module D = Datatypes
  open DatatypesExt
       
type coqgen_env = {
  filename : string;
  project_name : string;

  external_verbatim : (string * int) list;
  folded_external_symbol_string : string;
  partial_external_symbols : (ident, unit) Hashtbl.t;

  folded_symbols : string ref
}

let catch_not_found f v = try Some (f v) with Not_found -> None
let string_of_ident n = string_of_int n ^ "%positive"

(* This regular expression defines the names that can be used as Coq module names.
   If the user specifies something else, we take the longest prefix that satsifies the regexp. *)
let module_name_regexp = Str.regexp "[a-zA-Z_][a-zA-Z0-9_']*"
let default_module_name = "EdsgerGen"
let builtin_base_layer_name = "BuiltinBase"

let new_coqgen_env filename ast =
  let project_name =
    let basename = Filename.basename filename in
    if Str.string_match module_name_regexp basename 0 then
      Str.matched_string basename
    else begin
      print_endline ("Cannot use prefix of file name '" ^ basename ^
        "' as Coq module name: default to '" ^ default_module_name ^ "'");
      default_module_name
    end in

  let env0 = {
    filename = filename;
    project_name = project_name;

    external_verbatim = List.map (fun (s, excepts) ->
        s, 0
      ) ast.aFileExternalVerbatim;
    folded_external_symbol_string =
      String.concat " " (List.map (fun e -> e.aExtConstName)
				  (List.filter (fun e -> not e.aExtConstUnfolding)
					       ast.aFileExternalSymbols));
    partial_external_symbols =
      (let tbl = Hashtbl.create 0 in
       let _ = List.map (fun e ->if e.aExtConstPartial
				then Hashtbl.add tbl e.aExtConstName ())
		       ast.aFileExternalSymbols in
       tbl);
    
    folded_symbols = ref "" } in  
    env0


  (* TODO: we may need to do something like this?*)
  (*
  let output_hashtbl out tbl start_num =
    let arr = Array.make (Hashtbl.length tbl) "" in
    let i = ref 0 in
    let _ = Hashtbl.iter (fun x _ -> arr.(!i) <- x; incr i) tbl in
    let _ = Array.sort compare arr in
    if !i > 0 then
      let _ = output_char out '\n' in
      let num = ref start_num in
      let _ = Array.iter (fun x ->
          output_string out ("Definition " ^ x ^ " : ident  := " ^ string_of_ident !num ^ ".\n");
          incr num
        ) arr
      in !num
    else
      start_num in

  let file_EdsgerIdents = new_file env "EdsgerIdents"
        "Require Import BinPos.  (* positive_scope *)\nRequire Import backend.AST. (* for ident *)\n" 
        file_class_EdsgerIdents in
  let n = output_hashtbl file_EdsgerIdents env.cstruct_idents 550 in
  let _ = output_hashtbl file_EdsgerIdents env.global_idents n in
                         (* ((n / 500 + 1) * 500) in *)
  close_out file_EdsgerIdents;
   *)

let add_folded_symbols env syms =
  env.folded_symbols :=
    String.concat " " syms ^
    if !(env.folded_symbols) = "" then
      ""
    else
      " " ^ !(env.folded_symbols)

let coqgen_warning  msg =
  print_endline ("minicgen warning: " ^ msg)

let coqgen_fatal_error loc func msg =
  print_endline ("minicgen error at " ^ loc ^ " (" ^ func ^ "): " ^ msg);
  assert false

(* Todo: figure out if we need to do anything for type declarations. *)
(*									    
let rec output_prepare_ctype env out = function
  | ACtpointer ct | ACtarray (_, ct) -> output_prepare_ctype env out ct
  | ACtstruct (name, lst) ->
    output_cstruct_ident env (struct_name_to_ident name);
    List.iter (fun (fld, ct) ->
      output_cstruct_ident env (struct_field_name_to_ident name fld);
      output_prepare_ctype env out ct;
    ) lst;
  | _ -> ()
 *)

(* TODO: move these functions to a common file. *)
let rec positive_of_int n =
    let open Backend.BinNums in
    if n = 1 then
      Coq_xH
    else if (n land 1) = 1 then
      Coq_xI (positive_of_int (n asr 1))
    else
      Coq_xO (positive_of_int (n asr 1))
    
let coq_Z_of_int n =
    let open Backend.BinNums in
    if n = 0 then Z0
    else if n > 0 then Zpos (positive_of_int n)
    else Zneg (positive_of_int (-n))

let rec coqlist_of_list = function
  | [] -> Backend.Datatypes.Coq_nil
  | x::xs -> Backend.Datatypes.(Coq_cons (x, coqlist_of_list xs))


let rec int_of_positive p =
  let open Backend.BinNums in
  match p with
  | Coq_xI rest -> 2*(int_of_positive rest) + 1
  | Coq_xO rest -> 2*(int_of_positive rest)
  | Coq_xH -> 1


let int_of_z =
  let open Backend.BinNums in
  function
  | Z0 -> 0
  | Zpos rest -> int_of_positive rest
  | Zneg rest -> -(int_of_positive rest)
				 
(* end of functions to move. *)	   

let ident_table : (ident, int) Hashtbl.t = Hashtbl.create 1000
let ident_counter : int ref = ref 550
				 
let ident_generator = fun prefix midfix postfix ->
    let id = (prefix ^ midfix ^ "_"^ postfix) in
    try positive_of_int (Hashtbl.find ident_table id)
    with Not_found -> begin
       let n = !ident_counter in
       ident_counter := !ident_counter + 1;
       Hashtbl.add ident_table id n;
       positive_of_int n
      end

(* Todo: look more carefully at this and generate correct numbers. *)
let struct_name_to_ident2 = ident_generator "" "struct"
let struct_field_name_to_ident2 = ident_generator ""
let backend_ident_of_globvar  = ident_generator "var_"
let backend_ident_of_funcname = ident_generator "ident_"
let backend_ident_of_tempvar i = positive_of_int i
				    
let methods_tbl = Hashtbl.create 0

let rec gen_ctype =
  let open Backend.Ctypes in 
  function    
  | ACtint  -> Tint (I256, Unsigned)
  | ACtchar -> Tint (I8, Unsigned)
  | ACtvoid -> Tvoid
  | ACtpointer ct -> coqgen_fatal_error __LOC__ "gen_ctype" "ACtpointer not supported"
  | ACtarray (n, ct) -> Tarray (gen_ctype ct, coq_Z_of_int n)
  | ACtmapping (t1, t2) -> Thashmap (gen_ctype t1, gen_ctype t2)
  | ACtstruct (name, lst) -> Tstruct (struct_name_to_ident2 name,
				      gen_ctype_fields name lst)

  and gen_ctype_fields sname  =
  let open Backend.Ctypes in 
  function
  | [] -> Fnil
  | (fld,ct)::flds -> Fcons (struct_field_name_to_ident2 sname fld,
			     gen_ctype ct,
			     gen_ctype_fields sname flds)
	   
(*
let cimpl_struct_name impl =
 match impl.aImplType with
   | ACtstruct (i, _) -> i
   | _ -> coqgen_fatal_error __LOC__ "output_cval_cimpl"
                 "Structure a_cimpl with non-structural a_ctype"

(* Takes a_cimpl, and outputs a Coq expression which builds an extended C value. *)
let rec output_cval_cimpl out ind out_var get_var =
  let rec output impl = match impl.aImplDesc with
    | ACdefault -> output_string out "CVany"
    | ACint n ->
      output_string out ("(CVval (Values.Vint (Int256.repr " ^ string_of_int n ^ ")))")
    | ACvar v -> out_var out ind v
    | ACplus _ ->
      output_string out ("(CVval (Values.Vint (Int256.repr " ^
        get_coq_val_cimpl ind get_var impl ^ ")))")
    | ACtimes _ ->
      output_string out ("(CVval (Values.Vint (Int256.repr " ^
        get_coq_val_cimpl ind get_var impl ^ ")))")
    | ACcond (b, i1, i2) ->
      output_string out ("(if " ^ get_coq_val_cimpl ind get_var b ^ " then ");
      output i1;
      output_string out " else ";
      output i2;
      output_char out ')'
    | ACarray _ -> output_string out "CVany (* not yet implemented *)" (* ???: how? *)
    | ACstruct ls ->
        match impl.aImplType with
	| ACtstruct (ci, cls) ->
	  output_string out ("\n" ^ ind ^ "(CVstruct (CSmap");
	  List.iter2 (fun (f, i) (cf, _cfTy) ->
	    output_string out ("\n  " ^ ind ^ "(PTree.set  " ^
	      struct_field_name_to_ident ci cf ^ " ");
	    output_char out ' ';
	    output i;
	  ) ls cls;
	  output_string out ("\n  " ^ ind ^ "(@PTree.empty cval)))" ^ String.make (List.length ls) ')')
	| _ -> coqgen_fatal_error __LOC__ "output_cval_cimpl"
                 "Structure a_cimpl with non-structural a_ctype"
  in output
 *)


(* Take an environment env, a name of a type i, and the description of the type t, output all necessary definitions for the type.
   (Naming convention: This function opens the output channels itself, so it's called gen_ instead of output_.)
 *)
(*
let gen_type env i t =
  let type_out = env.coq_DataTypes in
  let ops_out = env.coq_DataTypeOps in
  match t.aTypeDesc with
  | ATdata (i', _) when i <> i' ->
    output_string type_out ("\nDefinition " ^ i ^ " := " ^ i' ^ ".\n")
  | ATprod _ | ATarray _ as d when i <> a_type_desc_to_ident d ->
    output_string type_out ("\nDefinition " ^ i ^ " := ");
    output_type_expr type_out "  " t;
    output_string type_out (".\n")
  | _ ->
    let _ = (* (Coq) data type definition *)
      output_char type_out '\n';
      match t.aTypeDesc with
      | ATdata (_, ATsingleton c) -> gen_struct_type env i c
      | ATdata (_, ATbranches cs) -> gen_data_type env i cs
      | _ ->
        output_string type_out ("Definition " ^ i ^ " := ");
        output_type_expr type_out "  " t;
        output_string type_out (".\n") in
    let _ = (* Type pair notation *)
      output_prepare_ctype env ops_out t.aTypeCtype;
      output_string ops_out ("Definition " ^ t.aTypePairIdent ^
        " := (Tpair " ^ i ^ " ");
      output_ctype ops_out "  " t.aTypeCtype;
      output_string ops_out ").\n" in
    let _ = (* [HyperTypeImpl] and [HyperBinaryImpl Oeq] *)
      output_hyper_type_impl ops_out t;
      output_hyper_type ops_out t;
      output_hyper_binary_impl_eq ops_out t in
    (* let _ = (* naturally_aligned *)
      output_type_naturally_aligned ops_out t in *)
    let _ = (* [HyperFieldImpl] and [HyperIndexImpl] *)
      match t.aTypeDesc with
      | ATprod (t1, t2) as d ->
        output_cstruct_ident env
          (struct_field_name_to_ident ("struct_" ^ a_type_desc_to_ident d) "fst");
        output_record_hyper_field_impl
          ops_out t.aTypePairIdent
          ("struct_" ^ a_type_desc_to_ident d) "fst" t1 0
          (fun out s -> output_string out ("fst " ^ s))
          (fun out v s -> output_string out ("(" ^ v ^ ", snd " ^ s ^ ")"));
        output_record_hyper_field
          ops_out t ("struct_" ^ a_type_desc_to_ident d) "fst" t1;
(*        output_record_hyper_field_passthrough
          ops_out t
          ("struct_" ^ a_type_desc_to_ident d) "fst" t1; *)
        output_cstruct_ident env
          (struct_field_name_to_ident ("struct_" ^ a_type_desc_to_ident d) "snd");
        output_record_hyper_field_impl
          ops_out t.aTypePairIdent
          ("struct_" ^ a_type_desc_to_ident d) "snd" t2
          (calign (csizeof t1.aTypeCtype) (calignof t2.aTypeCtype))
          (fun out s -> output_string out ("snd " ^ s))
          (fun out v s -> output_string out ("(fst " ^ s ^ ", " ^ v ^ ")"));
        output_record_hyper_field
          ops_out t ("struct_" ^ a_type_desc_to_ident d) "snd" t2;
(*        output_record_hyper_field_passthrough
          ops_out t
          ("struct_" ^ a_type_desc_to_ident d) "snd" t2 *)
      | ATdata (i, ATsingleton c) ->
        let pos = ref 0 in
        List.iter (fun (f, ft) ->
          let offset = calign !pos (calignof ft.aTypeCtype) in
          output_cstruct_ident env (struct_field_name_to_ident i f);
          output_record_hyper_field_impl ops_out t.aTypePairIdent i f ft offset
            (fun out s -> output_string out (f ^ " " ^ s))
            (fun out v s ->
              output_string out c.aTypeConstrName;
              List.iter (fun (f', _) ->
                if f' = f
                  then output_string out (" " ^ v)
                  else output_string out (" (" ^ f' ^ " " ^ s ^ ")")
              ) c.aTypeConstrArgs);
          output_record_hyper_field ops_out t i f ft;
          (* output_record_hyper_field_passthrough ops_out t i f ft; *)
          pos := offset + csizeof ft.aTypeCtype
        ) c.aTypeConstrArgs
      | ATdata (i, ATbranches [{ aTypeConstrImpl = Some {aImplDesc= ACdefault}} as cd;
			       { aTypeConstrImpl = Some ({aImplDesc=ACstruct cs} as impl)} as c])
      | ATdata (i, ATbranches [{ aTypeConstrImpl = Some ({aImplDesc=ACstruct cs}  as impl)} as c ;
			       { aTypeConstrImpl = Some {aImplDesc = ACdefault}} as cd]) ->
        let pos = ref 0 in
        List.iter2 (fun (f, ft) (cf, _cfimpl) ->
	  let offset = calign !pos (calignof ft.aTypeCtype) in
	  let ci = cimpl_struct_name impl in
          output_cstruct_ident env (struct_field_name_to_ident ci cf);
          output_twobranch_hyper_field_impl ops_out t i ci f ft cf offset c cd;
          output_twobranch_hyper_field ops_out t i ci f cf ft;
          (* output_twobranch_hyper_field_passthrough ops_out t i ci f cf ft; *)
          pos := offset + csizeof ft.aTypeCtype
        ) c.aTypeConstrArgs cs
      | ATarray (n, t') ->
        output_hyper_index_impl ops_out t.aTypePairIdent n t';
        output_hyper_index ops_out t n t';
      | ATmapping (t1, t2) ->
         output_hyper_hash_impl ops_out t.aTypePairIdent t1 t2;
         output_hyper_hash ops_out t.aTypePairIdent t1 t2;
      | _ -> ()
    in ()

 *)

let gen_unop = 
  let open Backend.Cop in
  function
  | OPneg -> Oneg
  | OPnot -> Onotbool
  | OPbitnot -> Onotint
  | OPbitneg -> Onotint
  | OPsha_1 -> Osha_1
             
let gen_binop =
  let open Backend.Cop in
  function
  | OPplus -> Oadd
  | OPminus -> Osub
  | OPtimes -> Omul
  | OPdivide -> Odiv
  | OPremainder -> Omod
  | OPand -> Oand
  | OPor -> Oor
  | OPeq -> Oeq
  | OPne -> One
  | OPlt -> Olt
  | OPle -> Ole
  | OPgt -> Ogt
  | OPge -> Oge
  | OPshl -> Oshl
  | OPshr -> Oshr
  | OPxor -> Oxor
  | OPbitand -> Oand
  | OPbitor -> Oor
  | OPsha_2 -> Osha_2

let rec gen_rexpr e =
  let open Backend.Ctypes in
  let open Backend.Integers in
  let open Backend.Language in
  let open Backend.BinNumsExt in
  match e.aRexprDesc with	     
  | AEconst (CONint n) ->
     Econst_int256 (Int256.repr (coq_Z_of_int n), gen_ctype e.aRexprType.aTypeCtype)
  | AEconst (CONuint n) ->
     Econst_int256 (Int256.repr (coq_Z_of_int n), gen_ctype e.aRexprType.aTypeCtype)
  | AEconst (CONbool true) ->
    Econst_int256 (Int256.one, Tint (I256, Unsigned))
  | AEconst (CONbool false) ->
    Econst_int256 (Int256.zero, Tint (I256, Unsigned))
  | AEconst CONhashvalue ->
     coqgen_fatal_error __LOC__ "output_rexpr" "Assigning hashvalue default value is not supported yet."    
  | AEconst (CONaddress addr) -> 
    Econst_int256 (Int256.repr (z_of_numstring addr), Tint (I256, Unsigned))
  | AEconst CONunit ->
    Econst_int256 (Int256.zero, Tvoid)
  | AEconst CONglobalpointer_undef ->
     coqgen_fatal_error __LOC__ "output_rexpr" "Internal error."
  | AEconstr_val (c, []) -> 
    Econst_int256 (begin match c.aTypeConstrImpl with
      | None | Some { aImplDesc = ACdefault } -> Int256.zero
      | Some { aImplDesc = ACint n }          -> (Int256.repr (coq_Z_of_int n))
      | _ -> coqgen_fatal_error __LOC__ "output_rexpr" "Internal error."
		    end,
		   gen_ctype e.aRexprType.aTypeCtype)
  | AEconstr_val (c, _) -> coqgen_fatal_error __LOC__ "output_rexpr" "Internal error: Nonempty AEconstr is not supported."
  | AEtemp (n, _i) ->
    Etempvar (backend_ident_of_tempvar n, gen_ctype e.aRexprType.aTypeCtype)
  | AEunop (op, e') ->
     Eunop (gen_unop op,
	    gen_rexpr e',
	    gen_ctype e.aRexprType.aTypeCtype)
  | AEbinop (op, e1, e2) ->
     Ebinop (gen_binop op,
	     gen_rexpr e1,
	     gen_rexpr e2,
	     gen_ctype e.aRexprType.aTypeCtype)	     
  | AEbuiltin ("address",[]) ->
     Ecall0 (Backend.MachineModel.Baddress,
	     gen_ctype e.aRexprType.aTypeCtype)
  | AEbuiltin ("origin",[]) ->
     Ecall0 (Backend.MachineModel.Borigin,
	     gen_ctype e.aRexprType.aTypeCtype)
  | AEbuiltin ("caller",[]) ->
     Ecall0 (Backend.MachineModel.Bcaller,
	     gen_ctype e.aRexprType.aTypeCtype)
  | AEbuiltin ("callvalue",[]) ->
     Ecall0 (Backend.MachineModel.Bcallvalue,
	     gen_ctype e.aRexprType.aTypeCtype)
  | AEbuiltin ("coinbase",[]) ->
     Ecall0 (Backend.MachineModel.Bcoinbase,
	     gen_ctype e.aRexprType.aTypeCtype)
  | AEbuiltin ("timestamp",[]) ->
     Ecall0 (Backend.MachineModel.Btimestamp,
	     gen_ctype e.aRexprType.aTypeCtype)
  | AEbuiltin ("number",[]) ->
     Ecall0 (Backend.MachineModel.Bnumber,
	     gen_ctype e.aRexprType.aTypeCtype)
  | AEbuiltin ("chainid",[]) ->
     Ecall0 (Backend.MachineModel.Bchainid,
	     gen_ctype e.aRexprType.aTypeCtype)
  | AEbuiltin ("selfbalance",[]) ->
     Ecall0 (Backend.MachineModel.Bselfbalance,
	     gen_ctype e.aRexprType.aTypeCtype)
  | AEbuiltin ("balance",[e1]) ->
     Ecall1 (Backend.MachineModel.Bbalance, (gen_rexpr e1),
	     gen_ctype e.aRexprType.aTypeCtype)
  | AEbuiltin ("blockhash",[e1]) ->
     Ecall1 (Backend.MachineModel.Bblockhash, (gen_rexpr e1),
	     gen_ctype e.aRexprType.aTypeCtype)
	    
  | AEbuiltin (other,_) -> coqgen_fatal_error __LOC__ "output_rexpr"
               ("Internal error, encountered unknown builtin \""^other^"\".")

let rec gen_lexpr obj e =
  let open Backend.Integers in
  let open Backend.Language in
  match e.aLexprDesc with
  | AEglob i ->
     Eglob (backend_ident_of_globvar obj i,
	   gen_ctype e.aLexprType.aTypeCtype)
  | AEfield (e', f) ->
     Efield (gen_lexpr obj e',
	           begin match e'.aLexprType.aTypeDesc with
			 | ATdata (i, _) -> struct_field_name_to_ident2 i f
			 | ATprod _ as d -> struct_field_name_to_ident2 ("struct_" ^ a_type_desc_to_ident d) f
			 | _ -> coqgen_fatal_error __LOC__ "output_lexpr"
						   "Only ATdata and ATprod can be accessed through structure field selector"
		   end,
		   gen_ctype e.aLexprType.aTypeCtype)
  | AEindex (e', idx) ->
    Eindex (gen_lexpr obj e', gen_rexpr idx, gen_ctype e.aLexprType.aTypeCtype)



let rec gen_constr_assignments target cname flds es =
  match flds, es with
  | (fld_id,ty)::flds', e::es' ->
     Ssequence (Sassign (Efield (target,
				 struct_field_name_to_ident2 cname fld_id,
				 gen_ctype ty),
			 e),
		gen_constr_assignments target cname flds' es')
  | _,_ -> Sskip
  
		
let gen_constr_command obj el er =
    let c, ls, cfields, i =
      match er.aBigExprDesc, er.aBigExprType.aTypeDesc with
      | AEstruct (c, ls), ATdata (i, _) 
      | AEconstr (c, ls), ATdata (i, _) ->
        begin match er.aBigExprType.aTypeCtype with
        | ACtstruct (cname,cfields) -> c, ls, cfields, cname
        | _                         -> c, ls, List.map (fun (i,e) -> (i, e.aBigExprType.aTypeCtype)) ls,      i
        end
      | _, (ATprod _ ) -> 
        coqgen_fatal_error __LOC__ "output_constr_command"
          "???: can we have pairs for data construction?"
      | _ -> coqgen_fatal_error __LOC__ "output_constr_command"
               "Only true big expression can be on the RHS of a data construction"
    in
    let es = List.map (fun (f, e) -> match e.aBigExprDesc with
           | AErexpr re -> gen_rexpr re
           | _ -> coqgen_fatal_error __LOC__ "output_constr_command"
                    "Currently only small expressions are supported as contructor arguments")
          ls in		 
    gen_constr_assignments (gen_lexpr obj el) i cfields es


  let gen_matchable_as_lexpr obj m = match m.aMatchableDesc with
    | AMtemp (id,ident) ->
       coqgen_fatal_error __LOC__ "gen_matchable_as_lexp" "Pattern matches on tempvars not yet implemented."
    | AMlexpr lexpr -> gen_lexpr obj lexpr

  let gen_matchable_as_rexpr m = match m.aMatchableDesc with
    | AMtemp (id,ident) ->  Etempvar (backend_ident_of_tempvar id,
				      gen_ctype m.aMatchableType.aTypeCtype)
    | AMlexpr lexpr -> 
       coqgen_fatal_error __LOC__ "gen_matchable_as_lexp" "Pattern matches on lexpr not yet implemented."

  (* When calling a primitive from a lower layer, we need to look up
     which object it is defined in. *)			  
  let backend_ident_of_primitive underlay slotname funcname =
    match underlay with
    | ALontop l ->
       let o = List.assoc slotname l.aLayerAllObjects in      			  
       backend_ident_of_funcname o.aObjectName funcname
    | _ -> raise Not_found

			  
  (* There are a few pattern-matching idioms that can be rendered into C code, so we recognize those,
     and for the other ones we just give up.

     First, if there is just a single constructor of a big type, and then a default type (which fails), then
     we can translate the case-expression as a projection function. *)
let rec gen_projection_style_match scrutinee struct_name tenv fields body =
  match tenv, fields with
  | (ident,(tmp_id, tmp_typ))::tenv', (fieldname, _)::fields' ->
     let rest = gen_projection_style_match scrutinee struct_name tenv' fields' body in
     if (ident = "_") then
       rest
     else
       Ssequence (Sassign (Etempvar (backend_ident_of_tempvar tmp_id,
				      gen_ctype tmp_typ.aTypeCtype),
			    Efield (scrutinee,
				    struct_field_name_to_ident2 struct_name fieldname,
				    gen_ctype tmp_typ.aTypeCtype)),
		  rest)
		   
		    
       
  | _,_ -> body

let rec gen_case_style_match underlay obj pure scrutinee dest clss =
  let open Backend.Cop in
  let open Backend.Ctypes in
  match clss with
  | []  -> coqgen_fatal_error __LOC__ "gen_case_style_match" "empty match (this should be impossible)"
  | (_,_,branch)::[] ->
      gen_cmd underlay obj pure branch dest  (* last, maybe catch-all, branch *)
  | (None,_,_)::_ -> coqgen_fatal_error __LOC__ "gen_case_style_match" "only the last branch can be a wildcard"
  | (Some c,_,branch)::rest ->     
     Sifthenelse (Ebinop (Oeq,
			  gen_matchable_as_rexpr scrutinee,
			  gen_rexpr { aRexprDesc = AEconstr_val(c, []); aRexprType = scrutinee.aMatchableType},
			  Tint (IBool, Unsigned)),
		  gen_cmd underlay obj pure branch dest,
		  gen_case_style_match underlay obj pure scrutinee dest rest)

     
(*		      
  (* Second, if all the matched constructors are realized as distinct constant integers, and none of them bind any varaibles,
     then we can translate the match into a series of if-statements. *)
  and output_case_style_match scrutinee clss bodyname_stem path ind =
    let lastclause = List.length clss - 1 in
    List.iteri (fun i cls ->
		match cls with
		| (Some c,_,_) when (i<>lastclause) ->
		   output_string out (ind ^ "(CCifthenelse\n" ^
			              ind ^ "  (ECbinop tint_bool Oeq\n" ^ ind ^ "     ");
		   output_matchable_as_rexpr out obj scrutinee;
                   output_string out ("\n"^ ind ^ "     ");
		   output_rexpr out ("\n"^  ind ^ "     ") { aRexprDesc = AEconstr_val(c, []); aRexprType = scrutinee.aMatchableType};
                   output_string out (")\n" ^ ind ^ "       "
				      ^ bodyname_stem ^ (string_of_path (i::path)) ^ "\n")
		| _ -> output_string out (ind ^ "       " ^  bodyname_stem ^ (string_of_path (i::path))
					  ^ String.make (List.length clss - 1) ')' ^"\n")
	       )
               clss

 *)
			   
(* This function is the composition of "output" in coqgen.ml and synth_stmt_stmt in SYnthesisStmt.v. *)	       
and gen_cmd underlay obj pure c dest =
  let open Backend.Cop in
  let open Backend.Language in
  match c.aCmdDesc with
  | ACskip -> Sskip
  | ACyield e -> Sset (positive_of_int dest, gen_rexpr e)
  | AClet (n, _i, c1, c2) ->
     Ssequence (gen_cmd underlay obj pure c1 n,
		gen_cmd underlay obj pure c2 dest)
  | ACsequence (c1, c2) ->
     Ssequence (gen_cmd underlay obj pure c1 dest,
		gen_cmd underlay obj pure c2 dest)
  | ACcall (s, f, es) ->
     let retval_dest = begin match c.aCmdType.aTypeCtype with
			     | ACtvoid -> Backend.Datatypes.None
			     | _ -> Backend.Datatypes.Some (positive_of_int dest)
		       end in
     let args = begin match es with
		      | [e] when e.aRexprType.aTypeDesc = ATbuiltin Tunit -> []
		      | _ -> List.map gen_rexpr es
		end in
     Scall (retval_dest,
	    backend_ident_of_primitive underlay s f,
	    coqlist_of_list args)     
  | ACcond (e, c_then, c_else) ->
     Sifthenelse (gen_rexpr e,
		  gen_cmd underlay obj pure c_then dest,
		  gen_cmd underlay obj pure c_else dest)
  | ACfor (n_iter, i, e1, n_end, e2, c, None) ->
     let id_it = backend_ident_of_tempvar n_iter in
     let id_end = backend_ident_of_tempvar n_end in
     let tint = Backend.Ctypes.(Tint (I256, Unsigned)) in
     Ssequence
       (Sset (id_end, gen_rexpr e2),
        coq_Sfor (Sset (id_it, gen_rexpr e1))
                 (Ebinop (Olt, Etempvar (id_it, tint), Etempvar (id_end, tint), tint))
		 (gen_cmd underlay obj pure c dest)
                 (Sset (id_it, (Ebinop (Oadd,
				    Etempvar (id_it, tint),
                                    Econst_int256 (Backend.Integers.Int256.one, tint),
				    tint)))))
    | ACfor (n_iter, i, e_start, n_end, e_end, c0, Some cc) ->
       coqgen_fatal_error __LOC__ "gen_cmd" "For-loop slices are not supported."
    | ACmatch (e, clss) ->
      let is_simple_int c =  match c with
	| (Some { aTypeConstrImpl = Some {aImplDesc = ACint _}}, [], _) -> true
	| (None, _, _) -> true
	| _ -> false
      in
      begin match clss with
	    | [(Some { aTypeConstrImpl = Some {aImplDesc = ACstruct fields; aImplType = ACtstruct (struct_name,_)} }, tenv, cmd); (None,_,_)]
	    | [(Some { aTypeConstrImpl = Some {aImplDesc = ACstruct fields; aImplType = ACtstruct (struct_name,_)} }, tenv, cmd)]
               -> 
               gen_projection_style_match (gen_matchable_as_lexpr obj e)  struct_name tenv fields (gen_cmd underlay obj pure cmd dest) 
	    | _ when List.for_all is_simple_int clss ->
	       gen_case_style_match underlay obj pure e dest clss
	    | _ ->
       coqgen_fatal_error __LOC__ "gen_cmd" ("Don't know how to synthesise code for the pattern match on " ^ string_of_a_matchable e)
      end
    | ACemit (et, es) ->
       let open Backend.Integers in
       let open Backend.BinNums in
       let topics, args = List.partition (fun ((_,_,idxbl),_) -> idxbl)
                            (List.combine et.aEventArgs es) in
       let hash = Abi.event_topic_of_event_type et in
       Slog (coqlist_of_list
               (Econst_int256 (Int256.repr hash, Tint (I256, Unsigned))
                          :: (List.map (fun (_,e) -> gen_rexpr e) topics)),
             coqlist_of_list (List.map (fun (_,e) -> gen_rexpr e) args))
    | ACload e ->
       Sset (positive_of_int dest, gen_lexpr obj e)
    | ACstore (el, er) ->
       Sassign (gen_lexpr obj el, gen_rexpr er)
    | ACconstr (el, er) ->
       (gen_constr_command obj el er)

    | ACfail -> Srevert
    | ACassert c -> Ssequence(gen_cmd underlay obj pure c dest,
			      Sifthenelse(Etempvar(backend_ident_of_tempvar dest,
						   Tint (I256, Unsigned)),
					  Sskip,
					  Srevert))
    | ACdeny c   -> Ssequence(gen_cmd underlay obj pure c dest,
			      Sifthenelse(Etempvar(backend_ident_of_tempvar dest,
						   Tint (I256, Unsigned)),
					  Srevert,
					  Sskip))

    | ACghost c -> Sskip

(*		     
    | ACfirst (n_iter, i, e_start, n_end, e_end, n_c, c, c_then, c_else, None) ->
      output_string out ("(CCfirst (* " ^ i ^ " := *) " ^ string_of_ident n_iter ^
        " " ^ string_of_ident n_end ^ " " ^ string_of_ident n_c ^ " ");
      output_rexpr out ("  " ^ ind) e_start;
      output_char out ' ';
      output_rexpr out ("  " ^ ind) e_end;
      output_string out ("\n" ^ ind);
      output ("  " ^ ind) true (1::path) c;
      output_string out ("\n" ^ ind);
      output ("  " ^ ind) pure (2::path) c_then;
      output_string out ("\n" ^ ind);
      output ("  " ^ ind) pure (3::path) c_else;
      output_char out ')'
    | ACfirst (n_iter, i, e_start, n_end, e_end, n_c, _, _, _, Some cc) ->
      let slice_name = method_full_name ^ "_slice_" ^ cc.aCapturedName in
      let args = String.concat "" (List.map (fun (_, (tmp_id, _)) ->
                   " (SpecTree.get " ^ string_of_ident tmp_id ^ " se)"
                 ) cc.aCapturedTemp) in
      let spec_post_matters =
        if pure then ""
                else " _ (fun r => k (ss_mem r, ss_return r))" in
      if pure then
        output_string out ("(CCrespec\n" ^ ind)
      else
        output_string out ("(CCrespec_opt\n" ^ ind);
      output_tmp_env out ind 0 c.aCmdEnv;
      output_string out ("\n" ^ ind ^
        "(CCfirst (* " ^ i ^ " := *) " ^ string_of_ident n_iter ^
        " " ^ string_of_ident n_end ^ " " ^ string_of_ident n_c ^ " ");
      output_rexpr out ("  " ^ ind) e_start;
      output_char out ' ';
      output_rexpr out ("  " ^ ind) e_end;
      output_string out ("\n  " ^ ind ^
        slice_name ^ " " ^ slice_name ^ "_found " ^ slice_name ^ "_notfound)\n" ^ ind ^
        (if pure then
           "(fun se d =>\n"
         else
           "(fun se d r k =>\n") ^ ind ^
        " match " ^ cc.aCapturedName ^ args ^ " ");
      (* output_rexpr_gallina out "se" ("    " ^ ind) e_start; *)
      output_char out ' ';
      (* output_rexpr_gallina out "se" ("    " ^ ind) e_end; *)
      output_string out (" d with\n" ^ ind ^
        " | inleft (exist n _) => " ^ slice_name ^ "_found_spec" ^ args ^ "\n" ^ ind ^
        "     n d" ^ spec_post_matters ^ "\n" ^ ind ^
        " | inright _ => " ^ slice_name ^ "_notfound_spec" ^ args ^ "\n     " ^ ind);
      (* output_rexpr_gallina out "se" ("       " ^ ind) e_end; *)
      output_string out (" d" ^ spec_post_matters ^ "\n" ^ ind ^
        " end))")

    | ACfold (n_iter, i_iter, e_start, n_end, e_end, n_acc, i_acc, e_init, n_c, c, None) ->
      output_string out ("(CCfold (* " ^ i_iter ^ " := *) " ^ string_of_ident n_iter ^
        " " ^ string_of_ident n_end ^
        " (* " ^ i_acc ^ " := *) " ^ string_of_ident n_acc ^
        " " ^ string_of_ident n_c ^ " ");
      output_rexpr out ("  " ^ ind) e_start;
      output_char out ' ';
      output_rexpr out ("  " ^ ind) e_end;
      output_char out ' ';
      output_rexpr out ("  " ^ ind) e_init;
      output_string out ("\n" ^ ind);
      output ("  " ^ ind) true path c;
      output_char out ')'
    | ACfold (n_iter, i_iter, e_start, n_end, e_end, n_acc, i_acc, e_init, n_c, _, Some cc) ->
      output_string out ("(CCrespec\n" ^ ind);
      output_tmp_env out ind 0 c.aCmdEnv;
      output_string out ("\n" ^ ind ^
        method_full_name ^ "_slice_" ^ cc.aCapturedName ^ "\n" ^ ind ^
        "(fun se => " ^ cc.aCapturedName);
      List.iter (fun (_, (tmp_id, _)) ->
        output_string out (" (SpecTree.get " ^ string_of_ident tmp_id ^ " se)")
      ) cc.aCapturedTemp;
      output_string out "))"

    | ACexternal (dest, s, i, []) ->
      output_string out ("(CCrespec_opt\n" ^ ind);
      output_tmp_env out ind 0 c.aCmdEnv;
      output_string out ("\n" ^ ind);
      output_string out ("(CCpanic " ^ c.aCmdType.aTypePairIdent ^ ")");
      output_string out ("\n" ^ ind);
      output_string out ("(fun se => ");
      begin match dest with
      | None -> output_string out ("ret (" ^ s ^ ")")
      | Some el -> output_string out "(@bind _ (@Monad_DS GlobalLayerSpec) _ _ get (fun d =>  put (";
                   (*output_lexpr_set_gallina out obj "se" s "d" ("    " ^ ind) el; *)
                   output_string out ")))"
      end;
      output_string out "))"

    | ACexternal (dest, s, i, args) ->
      let is_partial = Hashtbl.mem env.partial_external_symbols s in
      output_string out ("(CCrespec_opt\n" ^ ind);
      output_tmp_env out ind 0 c.aCmdEnv;
      output_string out ("\n" ^ ind);
      output_string out ("(CCpanic " ^ c.aCmdType.aTypePairIdent ^ ")");
      output_string out ("\n" ^ ind);
      output_string out ("(fun se => @bind _ (@Monad_DS GlobalLayerSpec) _ _ get (fun d =>");
      output_string out ("\n" ^ ind);
      (if is_partial
       then output_string out ("match " ^ s)
       else output_string out ("let a := " ^ s));
      List.iter (function
        | AEXrexer e ->
          output_string out ("\n     " ^ ind)
        (* output_rexpr_gallina out "se" ("    " ^ ind) e*)
        | AEXlexpr e ->
          output_string out ("\n     " ^ ind)
		(* output_lexpr_get_gallina out obj "se" "d" ("    " ^ ind) e  *)
      ) args;
      (if is_partial
       then output_string out (" with\n" ^
	 ind ^ " | None => mzero\n" ^ ind ^ " | Some a => ")
       else output_string out " in\n");
      begin match dest with
      | None -> output_string out "@ret _ (@Monad_DS GlobalLayerSpec) _ a"
      | Some el -> output_string out "put (";
                   (* output_lexpr_set_gallina out obj "se" "a" "d" ("    " ^ ind) el; *)
                   output_string out ")"
      end;
      if is_partial
      then output_string out ("\n" ^ ind ^ " end)))")
      else output_string out ("\n" ^ ind ^ " )))")
 *)
  | _ -> coqgen_fatal_error __LOC__ "gen_cmd" "todo"


(* Print MiniC expressions/statements, for debugging purposes. *)


let rec string_of_ctype = 
  let open Backend.Ctypes in
  function
  | Tvoid -> "Tvoid"
  | Tint (_,_) -> "TInt (SIZE,SIGNEDNESS)"
  | Tpointer _ -> "Tpointer"
  | Tarray (t,z) -> ("Tarray ("^string_of_ctype t^","^string_of_int(int_of_z z)^")")
  | Thashmap (t1,t2) -> ("Thashmap("^string_of_ctype t1^","^string_of_ctype t2^")")
  | Tfunction (ts,t) -> "Tfunction (TYPES,TYPE)"
  | Tstruct (id,flds) -> "Tstring (IDENT, FIELDS)"
  | Tunion  (id,flds) -> "Tunion (IDENT, FIELDS)"
  | Tcomp_ptr id -> "Tcomp_ptr ID"
		    
let rec string_of_expr = function
  | Econst_int (z, t) -> ("Econst_int (" ^ string_of_int (int_of_z z) ^ ","^string_of_ctype t^")")
  | Econst_int256 (z, t) -> ("Econst_int (" ^ string_of_int (int_of_z z) ^ ","^string_of_ctype t^")")
  | Evar (id,t) -> ("Evar("^string_of_int (int_of_positive id)^","^string_of_ctype t^")")
  | Eglob (id,t) -> ("Eglob("^string_of_int (int_of_positive id)^","^string_of_ctype t^")")
  | Etempvar (id,t) -> ("Etempvar("^string_of_int (int_of_positive id)^","^string_of_ctype t^")")
  | Ederef (e,t) -> ("Ederef(" ^ string_of_expr e ^","^ string_of_ctype t ^")")
  | Eaddr (e,t) -> ("Eaddr(" ^ string_of_expr e ^","^ string_of_ctype t ^")")
  | Eunop (op,e,t) -> ("Eunop(OP,"^string_of_expr e^","^string_of_ctype t ^")")
  | Ebinop (op,e1,e2,t) -> ("Ebinop(OP,"^string_of_expr e1^","^string_of_expr e2^","^string_of_ctype t ^")")
  | Efield (e, ident, t) ->("Efield("^string_of_expr e^","^string_of_int (int_of_positive ident)^","^string_of_ctype t^")")
  | Eindex (e1,e2,t) -> ("Eindex("^string_of_expr e1 ^","^string_of_expr e2^","^string_of_ctype t^")")
  | Ecall0 (bt,t) -> "Ecall0(BUILTIN,TYPE)"
  | Ecall1 (bt,e,t) -> "Ecall0(BUILTIN,EXPR,TYPE)"

let rec string_of_statement = function
  | Sskip -> "Sskip"
  | Sassign (e1,e2) -> ("Sassign("^string_of_expr e1 ^","^ string_of_expr e2 ^")")
  | Sset (id,e) -> ("Sset("^string_of_int(int_of_positive id)^","^string_of_expr e^")")
  | Scall (None, lab, exprs) -> "Scall(None, LABEL, ARGS)"
  | Scall (Some id, lab, expr) -> "Scall(Some ID, LABEL, ARGS)"
  | Ssequence (s1,s2) -> ("Ssequence("^string_of_statement s1 ^","^ string_of_statement s2^")")
  | Sifthenelse (e,s1,s2) -> ("Sifthenelse("^string_of_expr e^","^string_of_statement s1^","^string_of_statement s2 ^")")
  | Sloop s -> "(Sloop "^string_of_statement s^")"
  | Sbreak -> "Sbreak"
  | Sreturn None -> "Sreturn None"
  | Sreturn (Some id) -> ("Sreturn Some("^string_of_int (int_of_positive id)^")")		      
  | Stransfer (e1,e2) -> "Stransfer ("^string_of_expr e1 ^","^ string_of_expr e2 ^")"
  | Scallmethod (e1,ids,z,e,es) -> "Scallmethod TODO"
  | Slog _ -> "Slog TODO" 
  | Srevert -> "Srevert"

let rec string_of_params = 
  let open Backend.Datatypes in
  function
  | Coq_cons (Coq_pair(id, t) , params) -> "("^string_of_int (int_of_positive id) ^","^string_of_ctype t ^")::"^ string_of_params params
  | Coq_nil -> "nil"
		 
let string_of_methoddef md =
   "{ fn_return = " ^ string_of_ctype md.fn_return ^ ";\n"
  ^"  fn_params = " ^ string_of_params md.fn_params ^";\n"
  ^"  fn_temps =  " ^ string_of_params md.fn_temps ^";\n"
  ^"  fn_locals = " ^ string_of_params md.fn_locals ^";\n"
  ^"  fn_body = " ^ string_of_statement md.fn_body ^"\n"
  ^"}"
     
let method_classify mt =
  (* is pure *) mt.aMethodKind = MKconst || mt.aMethodKind = MKconstghost,
  (* has return *) mt.aMethodReturnType.aTypeDesc <> ATbuiltin Tunit			    

let rec filter_map f ls =
  match ls with
  | [] -> []
  | x::xs -> match f x with
	     | Some y -> y :: filter_map f xs
	     | None -> filter_map f xs

let gen_object_fields o =
  let open Backend.Datatypes in
  coqlist_of_list
    (filter_map (fun f ->
		 if not f.aObjectFieldIsLogical then
		   Some (Coq_pair (backend_ident_of_globvar o.aObjectName f.aObjectFieldName,
				   gen_ctype f.aObjectFieldType.aTypeCtype))
		 else
		   None			    
		) o.aObjectFields)


let gen_params (env : tmp_env_t) =
  List.map (fun (_, (x, t)) ->
      Backend.Datatypes.Coq_pair (backend_ident_of_tempvar x,
                                  gen_ctype t.aTypeCtype))
    (List.rev env)

let rec gen_tempenv = function
  | [] -> []
  | (id, typ) :: ts ->
     Backend.Datatypes.Coq_pair (backend_ident_of_tempvar id, gen_ctype typ) :: gen_tempenv ts

let builtinBase_local_ident_start_constructor_temp = 111

(* This function is the composition of "output" in coqgen.ml and synth_stmt_locals in SynthesisStmt.v. *)
let rec gen_cmd_locals c dest =
  match c.aCmdDesc with
  | ACskip -> []
  | ACyield e -> []
  | AClet (n, _i, c1, c2) ->
     (n, c1.aCmdType.aTypeCtype) :: gen_cmd_locals c1 n @ gen_cmd_locals c2 dest
  | ACsequence (c1, c2) ->
     gen_cmd_locals c1 dest @ gen_cmd_locals c2 dest
  | ACcall (s, f, es) -> []
  | ACcond (e, c1, c2) ->
     gen_cmd_locals c1 dest @ gen_cmd_locals c2 dest
  | ACfor (n_iter, i, e1, n_end, e2, c, None) ->
     (n_iter, ACtint)::(n_end,ACtint)::gen_cmd_locals c dest
    | ACfor (n_iter, i, e_start, n_end, e_end, c0, Some cc) ->
       coqgen_fatal_error __LOC__ "gen_cmd_locals" "For-loop slices are not supported."
    | ACmatch (e, clss) ->
       List.concat
	 (List.map (fun ((c, tmpenv, branch) : a_clause) ->
		    List.map (fun (_, (tmpid , tmp_ty)) -> (tmpid, tmp_ty.aTypeCtype))
			     tmpenv
		    @ gen_cmd_locals branch dest)
	    clss)

    | ACemit _ -> []    
    | ACload e -> []
    | ACstore (el, er) -> []
    | ACconstr (el, er) -> []
    | ACfail -> []
    | ACassert c -> []
    | ACdeny c -> []
    | ACghost c -> []
    | ACfirst (n_iter, i, e_start, n_end, e_end, n_c, c, c_then, c_else, None) ->
       (n_iter, ACtint)::(n_end, ACtint)::(n_c, ACtint)::
	 gen_cmd_locals c dest @ gen_cmd_locals c_then dest @ gen_cmd_locals c_else dest
    | ACfirst (n_iter, i, e_start, n_end, e_end, n_c, _, _, _, Some cc) ->
       coqgen_fatal_error __LOC__ "gen_cmd_locals" "todo: implement first"
    | ACfold (n_iter, i_iter, e_start, n_end, e_end, n_acc, i_acc, e_init, n_c, c, None) ->
       coqgen_fatal_error __LOC__ "gen_cmd_locals" "todo: implement fold"
    | ACfold (n_iter, i_iter, e_start, n_end, e_end, n_acc, i_acc, e_init, n_c, _, Some cc) ->
       coqgen_fatal_error __LOC__ "gen_cmd_locals" "todo: implement fold"
    | ACexternal (dest, s, i, []) -> []
    | ACexternal (dest, s, i, args) -> []

let builtinBase_local_ident_start = 10
				  
let gen_methoddef underlay objname m =
  let open Backend.Datatypes in
  let mt = m.aMethodType in
  let dest = builtinBase_local_ident_start in  
  let is_pure, has_return = method_classify mt in
  let body = gen_cmd underlay objname is_pure m.aMethodBody dest in
  let ret_type = gen_ctype mt.aMethodReturnType.aTypeCtype in
  { fn_return = ret_type ;
    fn_params = coqlist_of_list (gen_params m.aMethodParamEnv);
    fn_temps  = coqlist_of_list (gen_tempenv ((dest,mt.aMethodReturnType.aTypeCtype)
					      :: gen_cmd_locals m.aMethodBody dest));
    fn_locals = coqlist_of_list [];
    fn_body =  (if has_return then
                  Ssequence (body,
			                      (Sreturn (Some (positive_of_int dest))))
		else
		  Ssequence (body, Sreturn None))
  }

let gen_method_stub underlay objname m =
  let open Backend.Datatypes in
  let mt = m.aMethodType in
  let dest = builtinBase_local_ident_start in  
  let ret_type = gen_ctype mt.aMethodReturnType.aTypeCtype in
  let params =  gen_params m.aMethodParamEnv in
  { fn_return = ret_type ;
    fn_params = coqlist_of_list params ;
    fn_temps  = coqlist_of_list [Coq_pair (positive_of_int dest, ret_type)] ;
    fn_locals = coqlist_of_list [];
    fn_body =  Ssequence (Scall (Some (positive_of_int dest),
				 backend_ident_of_funcname objname m.aMethodName,
				 (coqlist_of_list (List.map (fun (Coq_pair (x,t)) -> (Etempvar (x, t))) params))),
			    Sreturn (Some (positive_of_int dest)))
  }

(* An issue might arise if constructors have temps with the same idents but
* different types. *)
let remove_duplicates tbl temps =
  coqlist_of_list (List.filter
    (fun pair ->
      let ident = D.fst pair in
      let found = Hashtbl.mem tbl ident in
      Hashtbl.add tbl ident true;
      not found
    )
    (caml_list temps)
  )

let concat_constructor f g =
  let tmp_tbl = Hashtbl.create 0 in
    D.Some
    { fn_return = f.fn_return;
      fn_params = remove_duplicates tmp_tbl (D.app f.fn_params g.fn_params);
      fn_temps = remove_duplicates tmp_tbl (D.app f.fn_temps g.fn_temps);
      fn_locals = coqlist_of_list [];
      fn_body = Ssequence (f.fn_body, g.fn_body);
    }

(* When there are multiple objects or multiple layers, we just concatenate together the code for all of them. The constructors from different layers also concatenate together and parameters in constructor is not work for now *)    
let concat_genv genv1 genv2 =
  let open Backend.Datatypes in
  let open Backend.Maps0 in
  let open Backend.Globalenvs in
  let open Backend.Globalenvs.Genv in
  match genv1.genv_constructor, genv2.genv_constructor with 
  | None , Some(f) ->
     { genv_vars = app genv1.genv_vars genv2.genv_vars;
       genv_funcs = app genv1.genv_funcs genv2.genv_funcs;
       genv_methods = app genv1.genv_methods genv2.genv_methods;
       genv_defs = PTree.union genv1.genv_defs genv2.genv_defs;
       genv_fundefs = PTree.union genv1.genv_fundefs genv2.genv_fundefs;
       genv_methoddefs = IntMap.union genv1.genv_methoddefs genv2.genv_methoddefs;
       genv_constructor = Some f }
  | None , None ->
     { genv_vars = app genv1.genv_vars genv2.genv_vars;
       genv_funcs = app genv1.genv_funcs genv2.genv_funcs;
       genv_methods = app genv1.genv_methods genv2.genv_methods;
       genv_defs = PTree.union genv1.genv_defs genv2.genv_defs;
       genv_fundefs = PTree.union genv1.genv_fundefs genv2.genv_fundefs;
       genv_methoddefs = IntMap.union genv1.genv_methoddefs genv2.genv_methoddefs;
       genv_constructor = None }
  | Some(f), Some(g) -> 
     { genv_vars = app genv1.genv_vars genv2.genv_vars;
       genv_funcs = app genv1.genv_funcs genv2.genv_funcs;
       genv_methods = app genv1.genv_methods genv2.genv_methods;
       genv_defs = PTree.union genv1.genv_defs genv2.genv_defs;
       genv_fundefs = PTree.union genv1.genv_fundefs genv2.genv_fundefs;
       genv_methoddefs = IntMap.union genv1.genv_methoddefs genv2.genv_methoddefs;
       genv_constructor = concat_constructor f g }
  | _,_ -> coqgen_fatal_error __LOC__ "concat_genv" "internal error, nonempty constructor"

let gen_object_methods gen_methodname gen_method underlay o =
  let open Backend.Datatypes in
  coqlist_of_list
    (filter_map
       (fun m ->
	if m.aMethodType.aMethodKind <> MKghost &&
	   m.aMethodType.aMethodKind <> MKconstghost &&
     m.aMethodType.aMethodKind <> MKconstructor
	then
	  begin
	  Some (Coq_pair (gen_methodname m,
			  gen_method underlay o.aObjectName m))
	  end
	else
	  None)
       o.aObjectMethods) 

let last_and_rest xs =
  match List.rev xs with
  | [] -> None
  | y::ys -> Some (y, List.rev ys)

let gen_constructor gen_methoddef underlay o last_layer =
  let open Backend.Datatypes in
  let dest = builtinBase_local_ident_start_constructor_temp in
  let constructor_list = (List.filter (fun x -> x.aMethodType.aMethodKind == MKconstructor ) o.aObjectMethods) in
      let constructor_method = List.hd constructor_list in
      let mt = constructor_method.aMethodType in
      let is_pure, has_return = method_classify mt in
      let body = gen_cmd underlay o.aObjectName is_pure constructor_method.aMethodBody dest in
      let ret_type = gen_ctype mt.aMethodReturnType.aTypeCtype in
      let params = gen_params constructor_method.aMethodParamEnv in
      Some { fn_return = ret_type;
             fn_params = coqlist_of_list params;
             fn_temps  = coqlist_of_list (gen_tempenv ((dest,mt.aMethodReturnType.aTypeCtype)
                          :: gen_cmd_locals constructor_method.aMethodBody dest));
             fn_locals = coqlist_of_list [];
             fn_body = if last_layer
                       then Ssequence (body, (Sreturn None))
                       else body
        }

let make_methname m =
  let i = coq_Z_of_int (function_selector_intval_of_method m) in
  Hashtbl.add methods_tbl i m.aMethodName;
  i

(* For the last layer we have to make "methods" instead of functions,
   and additionally we need to make "shim methods" for any functions
   exported from lower layers, to let them be called externally. *)
let gen_object underlay last_layer o =
  let open Backend.Datatypes in
  let open Backend.Globalenvs.Genv in
  let make_funcname m = backend_ident_of_funcname o.aObjectName m.aMethodName in
  new_genv (gen_object_fields o)
	   (if not last_layer
	    then gen_object_methods make_funcname gen_methoddef underlay o
	    else Coq_nil)
	   (if last_layer
	    then gen_object_methods make_methname gen_methoddef underlay o
	    else Coq_nil)
     (gen_constructor gen_methoddef underlay o last_layer)

let gen_object_stubs underlay o =
  let open Backend.Datatypes in
  let open Backend.Globalenvs.Genv in
  new_genv Coq_nil
	   Coq_nil
	   (gen_object_methods make_methname gen_method_stub underlay o)
	    None
	   
let gen_layer last_layer l =
  let underlay = l.aLayerDesc in
  List.fold_left concat_genv
		 Backend.Globalenvs.Genv.empty_genv
		 ( (List.map (fun (_,o) -> gen_object underlay last_layer o)
			     l.aLayerFreshObjects)
		  @(List.map (fun (_,o) -> gen_object_stubs underlay o)
			     l.aLayerPassthroughObjects))		  

let add_by_prefix tbl prefix of_int id n =
  let len = String.length prefix in
  let real_prefix = String.sub id 0 len in
  if real_prefix = prefix then
    Hashtbl.add tbl (of_int n) (String.sub id len ((String.length id)-len))

let gen_name_tables () =
  let nt = Backend.NameTablesExt.empty_name_tables in
  Hashtbl.iter (add_by_prefix nt.vars_tbl "var_" positive_of_int) ident_table;
  Hashtbl.iter (add_by_prefix nt.funcs_tbl "ident_" positive_of_int) ident_table;
  nt.methods_tbl <- methods_tbl;
  nt

let minicgen ast =
  match last_and_rest ast.aFileDeclarations with
  | Some ((_, ADlayer last), rest) ->
     let rest_compiled =
       List.fold_left
	 concat_genv  Backend.Globalenvs.Genv.empty_genv
	 (filter_map
	    (function
	     (* todo, do we need to do anything for type declarations? *)
	     | i, ADtype t -> None
             | i, ADevent _ -> None
	     | i, ADlayer l -> Some (gen_layer false l))
	    rest) in
     let last_compiled = gen_layer true last in
     gen_name_tables (), concat_genv rest_compiled last_compiled
  | _ -> coqgen_warning "Last declaration is not a layer, no code will be generated";
	 Backend.NameTablesExt.empty_name_tables, Backend.Globalenvs.Genv.empty_genv

let concat_constructor md1 md2 =
  let args, argtypes = (match md1.aMethodType.aMethodArgumentTypes, md2.aMethodType.aMethodArgumentTypes with
          | [t], ts when t.aTypeDesc = ATbuiltin Tunit -> md2.aMethodArguments , ts
          | ts, [t] when t.aTypeDesc = ATbuiltin Tunit -> md1.aMethodArguments , ts
          | ts1, ts2 -> List.append md1.aMethodArguments md2.aMethodArguments,
                        List.append ts1 ts2)
  in
  {
    aMethodName = "constructor";
    aMethodArguments = args;
    aMethodType = {
        aMethodArgumentTypes = argtypes;
        aMethodReturnType = md2.aMethodType.aMethodReturnType;
        aMethodKind = MKconstructor;
                  };
    aMethodSemantics = md2.aMethodSemantics;
    aMethodBody = md2.aMethodBody;
    aMethodParamEnv = md2.aMethodParamEnv;
  }

let abigen ast =
  let ets = List.concat (List.map (function
                             | (_, ADevent et) -> [et]
                             | _ -> [])
                           ast.aFileDeclarations) in
  let cts = List.concat (List.map (function 
                            | (_, ADlayer l) -> (List.concat((List.map (fun (_, obj) -> (List.filter (fun (md) -> md.aMethodType.aMethodKind == MKconstructor) obj.aObjectMethods)) l.aLayerFreshObjects)))
                            | _ -> [])
                          ast.aFileDeclarations) in
    let constructor = List.fold_left (concat_constructor) (List.hd cts) (List.tl cts) in
      match last_and_rest ast.aFileDeclarations with
    | Some ((_, ADlayer last), rest) ->
       Abi.json_of_layer last ets constructor
    | _ -> "[]"
