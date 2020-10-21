open Ast 
open Sast
open List

module StringMap = Map.Make(String)


(* 
let strore_ids ta = function *)


(* need to implement *)
let check (signature, implementation) =

  (* Add variable id in interface to symbol table *)
  let add_var map var =
    let dup_err v = "duplicate variable " ^ (string_of_expr v) ^ " in interface"
    and make_err er = raise (Failure er)
    in match var with (* No duplicate variables or redefinitions of built-ins *)
      Var(x, t) when StringMap.mem (string_of_expr x) map -> make_err (dup_err x)
    | Var(x, t) ->  StringMap.add (string_of_expr x) var map
    | TypeAssigndecl(x, t) when StringMap.mem (string_of_expr x) map -> make_err (dup_err x)
    | TypeAssigndecl(x, t) ->  StringMap.add (string_of_expr x) var map
    | MapAssigndecl(x, t) when StringMap.mem (string_of_expr x) map -> make_err (dup_err x)
    | MapAssigndecl(x, t) ->  StringMap.add (string_of_expr x) var map
    | Eventdecl(x, t) when StringMap.mem (string_of_expr x) map -> make_err (dup_err x)
    | Eventdecl(x, t) ->  StringMap.add (string_of_expr x) var map
    | _ -> map
  in

  (* Collect all variable names into one symbol table *)
  let var_decls = List.fold_left add_var StringMap.empty signature.interfacebody in

  (* Add method name in interface to symbol table *)
  let add_func map func =
    let dup_err v = "duplicate method " ^ (string_of_expr v) ^ " in interface"
    and make_err er = raise (Failure er)
    in match func with (* No duplicate variables or redefinitions of built-ins *)
      Constructordecl(l, t1, t2) when StringMap.mem (string_of_expr l) map -> make_err (dup_err l)
    | Constructordecl(l, t1, t2) ->  StringMap.add (string_of_expr l) func map
    | Methodecls (l, t1, t2) when StringMap.mem (string_of_expr l) map -> make_err (dup_err l)
    | Methodecls (l, t1, t2) ->  StringMap.add (string_of_expr l) func map
    | _ -> map
  in

  (* Collect all function names into one symbol table *)
  let func_decls = List.fold_left add_func StringMap.empty signature.interfacebody in

  (* Return a function from our symbol table *)
  let find_func s =
    try StringMap.find s func_decls
    with Not_found -> raise (Failure ("unrecognized method " ^ s))
  in

  let count_constructor num func =
    match func with
      Constructordecl(l, t1, t2) -> num +1
      | _ -> num
  in

  (* check constructor only announce once in interface *)
  let _  = 
    let constructor_num = List.fold_left count_constructor 0 signature.interfacebody in
    match constructor_num with
    0 -> raise (Failure "No constructor in interface")
    | 1 -> constructor_num
    | _ -> raise (Failure "Multiple constructors in interface")
  in
  
  (* Check all methods are implemented only once *)

  let add_implement map impl = 
    let dup_err v = "duplicate method " ^ (string_of_expr v) ^ " in implementation"
    and make_err er = raise (Failure er)
    in match impl with
      impl when StringMap.mem (string_of_expr impl.methodname) map -> make_err (dup_err impl.methodname)
      | impl ->  StringMap.add (string_of_expr impl.methodname) impl map
  in

  let _ = List.fold_left add_implement StringMap.empty  implementation.methods in

  let rec check_expr = function
    | NumLit l -> (Int, SNumLit l)
    | BoolLit l -> (Bool, SBoolLit l)
    | StrLit l -> (Void("void"), SStrLit l)
    (* check Id retrun with the correct type, keep Int for now *)
    | Id x -> (Int, SId(Sglobal, x))
    | EnvLit(x, y) -> (Void("Env"), SEnvLit(x,y))
    | Mapexpr(e1, e2) -> (Int, SMapexpr(check_expr e1, List.map check_expr e2))
    | Binop(e1, op, e2) -> (Int, SBinop(check_expr e1, op, check_expr e2))
    | Logexpr(e1, e2) -> (Void("void"), SLogexpr(check_expr e1, List.map check_expr e2))
    | Storageassign (e1, e2) -> (Int, SStorageassign(check_expr e1, check_expr e2))
    | Comparsion (e1, op, e2) -> (Int, SComparsion(check_expr e1, op, check_expr e2))
    | Voidlit(s) -> (Void("void"), SVoidlit(s) )
  in

  let check_func func = 

    let check_args_type var1 t2 = 
      let check_type x1 t1 t2 =  let tag = (t1 = t2)
      and unmatch_err = "function argument " ^ string_of_expr x1 ^ " has type " 
      ^ string_of_typ t1 ^ " ,which is unmatch with declaration type " ^ string_of_typ t2 in
        match tag with 
        true -> t1
        | false -> raise (Failure unmatch_err)
      in
      match var1, t2 with
      Var(x1, t1), t2 -> check_type x1 t1 t2
      | _, _ -> raise (Failure "Not a legal variables in arguments")

    in

    let sfunc = function
       | Id l -> (Void("void"), SStrLit l)
       | e -> raise (Failure ("Not a function name " ^ string_of_expr e))
    in

    let func_decl = find_func (string_of_expr func.methodname) in

    let params_types, return_type = match func_decl with 
      Methodecls(expr, typli, typ) -> (typli, typ)
      | _ -> raise (Failure "Not legal method")
    in
    
    (* If the only arg is void and no arguments in method, then skip check args *)
    let skip_check_args =  
      if List.length params_types = 1 then 
        let first_arg = List.hd params_types in 
        match first_arg with 
        Void("void") -> if List.length func.params = 0 then true else false
        | _ -> false
      else false


    in

    let _ = if skip_check_args then true else

      (* Check argument types length matches with declaration  *)
      let _ = let typ_len_func = List.length func.params
        in let typ_len_decl = List.length params_types in
        match typ_len_func, typ_len_decl with
        typ_len_func, typ_len_decl when (typ_len_func > typ_len_decl)
        -> raise (Failure ("Redundant arguments in method " ^ string_of_expr func.methodname))
        | typ_len_func, typ_len_decl when (typ_len_func < typ_len_decl)
        -> raise (Failure ("Missing arguments in method " ^ string_of_expr func.methodname))
        | _, _ -> typ_len_func
      in

      (* Check whether variable argument type matches with declaration *)
      let _ = (List.map2 check_args_type func.params params_types) in false

    in
    let add_var_args map var =
      let dup_err v = "duplicate variable " ^ (string_of_expr v) ^ " in method arguments"
      and make_err er = raise (Failure er)
      in match var with (* No duplicate variables or redefinitions of built-ins *)
        Var(x, t) when StringMap.mem (string_of_expr x) map -> make_err (dup_err x)
      | Var(x, t) ->  StringMap.add (string_of_expr x) var map
      | _ -> raise (Failure "Only variable allows in method arguments")
    in

    let var_sym = List.fold_left add_var_args var_decls func.params in

    (* Return a variable from our symbol table *)
    let find_var s = let s_type = 
      try StringMap.find s var_sym
      with Not_found -> raise (Failure ("unrecognized variable " ^ s))
      in
      match s_type with
      Var(x, t) -> t
      | TypeAssigndecl(x, t) -> t
      | MapAssigndecl(x, t) -> t
      | Eventdecl(x, t) -> Void("void")
      | _ -> raise (Failure ("unrecognized variable " ^ string_of_decl s_type ))
    in

    let rec check_expr = function
      | NumLit l -> (Int, SNumLit l)
      | BoolLit l -> (Bool, SBoolLit l)
      | StrLit l -> (Void("void"), SStrLit l)
      (* check Id retrun with the correct type, keep Int for now *)
      | Id x -> 
        let t = find_var x in
        if StringMap.mem x var_decls then
          (find_var x, SId(Sglobal, x))
        else (find_var x, SId(Slocal, x))
      | EnvLit(x, y) -> (Void("Env"), SEnvLit(x,y))
      | Mapexpr(e1, e2) as e -> 
        let id_err = string_of_expr e1 ^ " is not a id in " ^ string_of_expr e in
        let (t1, e1') = match e1 with 
          Id(id) -> check_expr e1
          | _ -> raise (Failure id_err)
        in
        let type_err = "Id " ^ string_of_expr e1 ^ " " ^
                      " is " ^ string_of_typ t1 ^ 
                      " type, not a map struct in " ^ string_of_expr e in
        let e2' = List.map check_expr e2 in
        let check_map_key_type key_type sexpr2 = 
          match sexpr2 with
            (type2, sx2) -> 
            let key_err = "Expresion "  ^ (string_of_sexpr sexpr2) 
                      ^ " has type " ^ (string_of_typ type2) ^ ", but type "
                      ^ (string_of_typ key_type) ^ " is required in "
                      ^ string_of_expr e
            in 
            
            if type2 = Void("Env") then sexpr2
            else if key_type = type2 then 
            match key_type with
             Int | Uint("uint") | Address("ADDRESS") -> sexpr2
            | _ -> raise (Failure ("Type " ^ string_of_typ key_type ^
            " is not allowed as key type in map " ^ string_of_expr e ))
            else
              raise (Failure key_err)
        in
        let value_type = match t1 with
          Mapstruct(key_typli, value_type) -> 
          (* Check map query types length matches with map declaration  *)
          let key_type_ls_len = List.length key_typli
          and query_type_ls_len = List.length e2' in
          let _ =
          match key_type_ls_len, query_type_ls_len with
          key_type_ls_len, query_type_ls_len when 
          (key_type_ls_len > query_type_ls_len) ->
          raise (Failure ("Missing query value in map " ^ string_of_expr e))
          | key_type_ls_len, query_type_ls_len when 
          (key_type_ls_len < query_type_ls_len) ->
          raise (Failure ("Redundant query value in map " ^ string_of_expr e))
          | _ -> key_type_ls_len
          in
          let _ = List.map2 check_map_key_type key_typli e2' in
          value_type
          | _ -> raise (Failure type_err)
        in 
        (value_type, SMapexpr((t1, e1'), e2'))
      (* Binop : Add | Sub | Times | Divide | And | Or *)
      | Binop(e1, op, e2) as e -> 
        let (t1, e1') = check_expr e1
        and (t2, e2') = check_expr e2 in
        let err = "Illegal binary operator " ^
                  string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                  string_of_typ t2 ^ " in " ^ string_of_expr e
        in
        (* All binary operators require operands of the same type*)
        
        if t1 = Void("Env") then
          (t2, SBinop((t1, e1'), op, (t2, e2')))
        else if t2 = Void("Env") then
          (t1, SBinop((t1, e1'), op, (t2, e2')))
        else if t1 = t2 then
          (* Determine expression type based on operator and operand types *)
          let t = match op with
            Add | Sub | Times | Divide when t1 = Uint("uint") -> Uint("uint")
            | Add | Sub | Times | Divide when t1 = Int -> Int
            | And | Or when t1 = Bool -> Bool
            | _ -> raise (Failure err)
          in
          (t, SBinop((t1, e1'), op, (t2, e2')))
        else if (t1 = Uint("uint") && t2 = Int) || (t1 = Int && t2 = Uint("uint")) then
          let t = match op with
          Add | Sub | Times | Divide -> Int
          | _ -> raise (Failure err)
          in
          (t, SBinop((t1, e1'), op, (t2, e2')))
        else raise (Failure err)

      | Logexpr(e1, e2) -> (Void("Log"), SLogexpr(check_expr e1, List.map check_expr e2))
      | Storageassign (e1, e2) as e -> 
        let (t1, e1') = check_expr e1
        and (t2, e2') = check_expr e2 in
        let err = "Illegal storage assign: " ^
                  string_of_typ t1 ^ " <- " ^
                  string_of_typ t2 ^ " in " ^ string_of_expr e
        in
        (* All binary operators require operands of the same type*)
        if t2 = Void("Env") && t1 != Void("Env") then
          (t1, SStorageassign((t1, e1'), (t2, e2')))
        else if (t1 = Uint("uint") && t2 = Int) || (t1 = Int && t2 = Uint("uint")) then
          (Void("void"), SStorageassign((t1, e1'), (t2, e2')))
        else if t1 = t2 then
          (Void("void"), SStorageassign((t1, e1'), (t2, e2')))
        else raise (Failure err)

      (* Comparsion : Equal | Neq | LGT | RGT | LGTEQ | RGTEQ *)
      | Comparsion (e1, op, e2) as e -> 
        let (t1, e1') = check_expr e1
        and (t2, e2') = check_expr e2 in
        let err = "Illegal binary operator " ^
                  string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                  string_of_typ t2 ^ " in " ^ string_of_expr e
        in
        (* All binary operators require operands of the same type*)
        if t1 = Void("Env") then
          (t2, SComparsion((t1, e1'), op, (t2, e2')))
        else if t2 = Void("Env") then
          (t1, SComparsion((t1, e1'), op, (t2, e2')))
        else if t1 = t2 then
          (* Determine expression type based on operator and operand types *)
          let t = match op with
            | Equal | Neq | LGT | RGT | LGTEQ | RGTEQ when t1 = Uint("uint") || t1 = Int -> Bool
            | _ -> raise (Failure err)
          in
          (t, SComparsion((t1, e1'), op, (t2, e2')))
        else if (t1 = Uint("uint") && t2 = Int) || (t1 = Int && t2 = Uint("uint")) then
          let t = match op with
          | Equal | Neq | LGT | RGT | LGTEQ | RGTEQ -> Bool
          | _ -> raise (Failure err)
          in
          (t, SComparsion((t1, e1'), op, (t2, e2')))
        else raise (Failure err)
      | Voidlit(s) -> (Void("void"), SVoidlit(s) )
    in

    let sreturns = 
      let (t, sx) = check_expr func.returns in
      match t with 
      Void("Env") -> (return_type, sx)
      | _ -> if t = return_type then (t, sx) 
      else 
        let return_type_unmatch_err = "Return type of method " ^ (string_of_expr func.methodname)
        ^ " is: " ^ (string_of_typ return_type) ^ ",\n with is unmatch with "
        ^ " expression: " ^ (string_of_sexpr (t, sx)) in 
        if t = return_type then (t, sx)
        else raise (Failure return_type_unmatch_err)

    in
    { 
      smethodname = sfunc func.methodname;
      sparams = func.params;
      sguard_body = List.map check_expr func.guard_body;
      sstorage_body = List.map check_expr func.storage_body;
      seffects_body = List.map check_expr func.effects_body;
      sreturns = sreturns;
    }
  in

  let sinterface_def =
      {
        ssignaturename = check_expr signature.signaturename;
        sinterfacebody = signature.interfacebody;
      }
  in 

  let simplementation_def = 
    {
      sconsturctor = {
        sname = check_expr implementation.consturctor.name;
        sparams = implementation.consturctor.params;
        sconsturctor_body = List.map check_expr implementation.consturctor.consturctor_body;
        sreturn_type = implementation.consturctor.return_type;
      };

      smethods = List.map check_func implementation.methods;
    }
  in 

  let sprogram = (sinterface_def, simplementation_def)
  in
  sprogram
