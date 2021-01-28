(* TODO: Change var store to glob store *)

open Ast
open AstExt
open Printf

open Backend
  open Ctypes
  open CtypesExt
  open BinNums
  open BinNumsExt
  module D = Datatypes
  open DatatypesExt
  open Globalenvs
  open Language
  open LanguageExt
  open ExpMiniC
  open StmtMiniC

let raise_msg s = Core.raise_s (Core.Sexp.Atom s)

let match_intsize = function
  | "8" -> I8
  | "16" -> I16
  | "32" -> I32
  | "256" | "" -> I256
  | _ as s -> raise_msg (sprintf "invalid intsize: %s" s)

type ('a, 'b) ident_store = {
  tbl: ('a, 'b) Hashtbl.t;
  mutable lst: 'a List.t
}

let glob_store = { tbl = Hashtbl.create 0; lst = [] }
let func_store = { tbl = Hashtbl.create 0; lst = [] }
let method_store = { tbl = Hashtbl.create 0; lst = [] }

let constructor = ref None

let struct_field_tbl = Hashtbl.create 0
let struct_type_tbl = Hashtbl.create 0
let struct_name_tbl = Hashtbl.create 0

let name_tbls = NameTablesExt.empty_name_tables

let flip_tbl tbl =
  let new_tbl = Hashtbl.create (Hashtbl.length tbl) in
  Hashtbl.iter (fun name (ident, _) -> Hashtbl.add new_tbl ident name) tbl;
  new_tbl

let strip_prefix prefix name =
  let prefix_len = String.length prefix in
  let name_len = String.length name in
  if name_len > prefix_len &&
    String.sub name 0 prefix_len = prefix
  then
    Some (int_of_string (String.sub name prefix_len (name_len - prefix_len)))
  else
    None

let rec match_type = function
    | Pvoid -> Tvoid
    | Pint i -> Tint (match_intsize i, Unsigned)
    | Pbool -> Tint (IBool, Unsigned)
    | Ppointer t -> Tpointer (Coq_mem, (match_type t)) (* Coq_mem is a dummy val *)
    | Parray (t, n) -> Tarray (match_type t, Zpos (positive_of_int n))
    | Pmapping (t1, t2) -> Thashmap (match_type t1, match_type t2)
    | Pstruct (i, v) -> let (i', f) = add_struct i v in
      let t = Tstruct (i', f) in Hashtbl.add struct_type_tbl i t; t
    | Punion (i, v) -> let (i', f) = add_struct i v in
      let t = Tunion (i', f) in Hashtbl.add struct_type_tbl i t; t
    | Puserstruct s -> Hashtbl.find struct_type_tbl s
(* add a struct or union *)
and add_struct i v =
  (* let _, t, i = v in *)
  (* can try: store by ident, and have an ident->name table for checking duplicates *)
  (* if Hashtbl.mem struct_field_tbl i then *)
  (*   raise_msg (sprintf "redeclaration of struct type %s" i); *)
  let field_tbl = Hashtbl.create 0 in
  List.iter (fun (_, t, i') ->
    let field_num = (match strip_prefix "field_" i' with 
    | Some n -> positive_of_int n
    | None -> positive_of_int ((Hashtbl.length field_tbl) + 1)
    ) in
    Hashtbl.add field_tbl i' (field_num, match_type t)) v;
  let struct_num = positive_of_int ((Hashtbl.length struct_name_tbl)+1) in
  Hashtbl.add struct_field_tbl i field_tbl;
  Hashtbl.add struct_name_tbl struct_num i;
  let field_list = Hashtbl.fold (fun _ (n, t) prev -> Fcons (n, t, prev)) field_tbl Fnil in
  struct_num, field_list

let process_var store prefix var =
  let m, t, i = var in
  if Hashtbl.mem store.tbl i then
    raise_msg (sprintf "redeclaration of variable %s" i);
  let i' = match strip_prefix prefix i with
    | Some i -> positive_of_int i
    | None -> positive_of_int ((Hashtbl.length store.tbl) + 1) in
  let t' = match_type t in
  Hashtbl.add store.tbl i (i', (t', m));
  store.lst <- store.lst @ [i]

let process_glob store var =
  let m, t, i = var in
  if Hashtbl.mem store.tbl i then
    raise_msg (sprintf "redeclaration of variable %s" i);
  let i' = match strip_prefix "global_" i with
    | Some i -> positive_of_int i
    | None -> positive_of_int ((Hashtbl.length store.tbl) + 1) in
  let t' = match_type t in
  Hashtbl.add store.tbl i (i', t');
  store.lst <- store.lst @ [i]

let process_param store prefix var =
  let () = process_var store prefix var in
  let _, _, i = var in let i', (t', m) = Hashtbl.find store.tbl i in
  D.Coq_pair (i', t')

let process_temp store prefix var =
  let () = process_var store prefix var in
  let _, _, i = var in let i', (t', m) = Hashtbl.find store.tbl i in
  m, D.Coq_pair (i', t')

let func_defined name =
  if (Hashtbl.mem func_store.tbl name)
    || (Hashtbl.mem method_store.tbl name)
    || (name = "constructor" && (Option.is_some !constructor)) then
    raise_msg (sprintf "redefinition of function %s" name)

let add_func name =
  let () = func_defined name in
  let i = match strip_prefix "func_" name with
    | Some n -> positive_of_int n
    | None -> positive_of_int ((Hashtbl.length func_store.tbl) + 1) in
  Hashtbl.add func_store.tbl name (i, empty_coq_function);
  func_store.lst <- func_store.lst @ [name]

let replace_func name f =
  let (i, _) = Hashtbl.find func_store.tbl name in
  Hashtbl.replace func_store.tbl name (i, f);
  i

let add_method name =
  let () = func_defined name in
  let i = match strip_prefix "method_" name with
    | Some n -> coq_Z_of_int n
    | None -> coq_Z_of_int ((Hashtbl.length method_store.tbl) + 1) in
  Hashtbl.add method_store.tbl name (i, empty_coq_function);
  method_store.lst <- method_store.lst @ [name]

let replace_method name f =
  let (i, _) = Hashtbl.find method_store.tbl name in
  Hashtbl.add method_store.tbl name (i, f);
  i

let rec match_expression tmp_tbl e =
  let match_ex = match_expression tmp_tbl in
  match e with
  (* currently setting to the default int: unsigned 256 *)
  | Pconst s ->
      Econst_int256 (coq_Z_of_Z (Z.of_string s), match_type (Pint ""))
  | Pvar s ->
    if Hashtbl.mem tmp_tbl s then
      let i, (t, m) = Hashtbl.find tmp_tbl s in
      (match m with
      | true -> Evar (i, t)
      | false -> Etempvar (i, t))
    else if Hashtbl.mem glob_store.tbl s then
      let i, t = Hashtbl.find glob_store.tbl s in
        Eglob (i, t)
    else
      raise_msg (sprintf "undefined variable %s" s)
  | Pderef e -> (match e with
    | Pvar _ -> let e' = match_ex e in Ederef (e', typeof e')
    | _ -> raise_msg (sprintf "cannot deref non-variable %s" (show_p_expr e)) )
  | Punop (u, e) -> let e' = match_ex e in Eunop (u, e', typeof e')
  | Pbinop (e1, b, e2) ->
    let e1' = match_ex e1 in
    let e2' = match_ex e2 in
    if ((typeof e1') <> (typeof e2')) then
      raise_msg (sprintf "types of %s and %s incompatible: %s and %s"
        (show_p_expr e1) (show_p_expr e2)
        (show_type (typeof e1')) (show_type (typeof e2')));
    Ebinop (b, e1', e2', typeof e1')
  | Pfield (e, i) ->
    let e' = match_ex e in
    let n = match typeof e' with
      | Tstruct (i, _) -> i
      | Tunion (i, _) -> i
      | _ -> raise_msg (sprintf "not a struct: %s" (show_p_expr e)) in
    (* Hashtbl.iter (fun a b -> print_endline ((show_pos a) ^ b)) struct_name_tbl; *)
    let s = Hashtbl.find struct_name_tbl n in
    let n', t = Hashtbl.find (Hashtbl.find struct_field_tbl s) i in
    Efield (e', n', t)
  | Parrayderef (e1, e2) -> let e1' = match_ex e1 in (
    match typeof e1' with
    | Tarray (t, _) -> Eindex (e1', match_ex e2, t)
    | Thashmap (_, t) -> Eindex (e1', match_ex e2, t)
    | _ -> raise_msg (sprintf "%s: not an array or hashmap" (show_p_expr e1)))
  | Pcall0 b -> Ecall0 (b, match_type (Pint "256"))
  | Pcall1 (b, e) -> Ecall1 (b, match_ex e, match_type (Pint "256"))


let matach_ex_opt tmp_tbl = function
  | None -> D.None
  | Some e -> D.Some (match_expression tmp_tbl e)

let rec flatten_statements match_func = function
  | [] -> Sskip
  | hd::tl -> Ssequence (match_func hd, flatten_statements match_func tl)

let rec match_statement tmp_tbl in_loop stmt =
  let match_ex = match_expression tmp_tbl in
  let match_ex_list e = coqlist_of_list (List.map match_ex e) in
  let loop = match stmt with Ploop _ -> true | _ -> false in
  let in_loop' = loop || in_loop in
  let flat_stmts = flatten_statements (match_statement tmp_tbl in_loop') in
  match stmt with
  | Passign (e1, e2) -> (
      let e1' = match_ex e1 in
      let e2' = match_ex e2 in
      match e1' with
      | Etempvar (i, _) -> Sset (i, e2')
      | Evar _ | Eglob _ | Eindex _ | Efield _ -> Sassign (e1', e2')
      | _ -> raise_msg (sprintf "cannot assign to nonvariable %s" (show_p_expr e1)
      )
    )
  | Pifthenelse (e, s1, s2) ->
    let s2' = match s2 with
      | None -> Sskip
      | Some s -> flat_stmts s in
    Sifthenelse (match_ex e, flat_stmts s1, s2')
  | Ploop s -> Sloop (flat_stmts s)
  | Pbreak -> if not in_loop then raise_msg ("break statment must be in a loop");
    Sbreak
  | Preturn e -> (match e with
    | None -> Sreturn None
    | Some e' -> (match match_ex e' with
      | Etempvar (i, _) -> Sreturn (Some i)
      | _ -> raise_msg (sprintf "not a tempvar: %s" (show_p_expr e'))
      )
    )
  | Ptransfer (e1, e2) -> Stransfer (match_ex e1, match_ex e2)
  | Prevert -> Srevert
  | Pcall (i, f, e) ->
    let i' = match i with
    | None -> D.None
    | Some s -> D.Some (fst (Hashtbl.find tmp_tbl s)) in
      let f' = fst (Hashtbl.find func_store.tbl f) in
      let e' = coqlist_of_list (List.map match_ex e) in
      Scall(i', f', e')
  | Plog (e1, e2) ->  Slog (match_ex_list e1, match_ex_list e2)
  | Pcallmethod (addr, ret, signature, value, gas, args) ->
    let ret' = coqlist_of_list (List.map (fun e -> match match_ex e with
        | Etempvar (i, _) -> i
        | _ -> raise_msg (sprintf "Not a temp variable: %s" (show_p_expr e))
      )
      ret) in
    let signature' = match match_ex signature with
      | Econst_int256 (i, _) -> i
      | _ -> raise_msg (sprintf "Not an int: %s" (show_p_expr signature))
    in
    Scallmethod (match_ex addr, ret', signature', match_ex value,
      matach_ex_opt tmp_tbl gas, match_ex_list args)

let preprocess_func f =
  match f.p_fn_name with
  | "constructor" -> ()
  | _ -> match f.p_fn_visibility with
    | None -> add_method f.p_fn_name
    (* default to public *)
    | Some v -> match v with
      | Private -> add_func f.p_fn_name
      | Public -> add_method f.p_fn_name

let append_return body =
  let rev = List.rev body in
  let appended = match rev with
  | [] -> [ Sreturn None ]
  | hd::_ -> match hd with
    | Sreturn _ -> rev
    | _ -> (Sreturn None) :: rev
  in
  List.rev appended

let process_func f =
  let tmp_store = { tbl = Hashtbl.create 0; lst = [] } in
  let params = List.map (process_param tmp_store "tmp_") f.p_fn_params in
  let vars = List.map (process_temp tmp_store "tmp_") f.p_fn_temps in
  let filter flip (m, it) = match flip m with
    | true -> Some it
    | false -> None in
  let temps = List.filter_map (filter (fun b -> not b)) vars in
  let locals = List.filter_map (filter (fun b -> b)) vars in
  let body = List.map (match_statement tmp_store.tbl false) f.p_fn_body in
  let body' = append_return body in
  let f' = { fn_return = (match_type f.p_fn_return);
    fn_params = coqlist_of_list params;
    fn_temps = coqlist_of_list temps;
    fn_locals = coqlist_of_list locals;
    fn_body = flatten_statements (fun x -> x) body';
  } in
  match f.p_fn_name with
  | "constructor" -> func_defined f.p_fn_name;
    constructor := Some f';
    name_tbls.constructor_tmps_tbl <- flip_tbl tmp_store.tbl
  | _ ->
    let process_method () = let i = replace_method f.p_fn_name f' in
      Hashtbl.add name_tbls.methods_tmps_tbl i (flip_tbl tmp_store.tbl) in
    match f.p_fn_visibility with
    | None -> process_method ()
    (* default to public *)
    | Some v -> match v with
      | Private -> let i = replace_func f.p_fn_name f' in
       Hashtbl.add name_tbls.funcs_tmps_tbl i (flip_tbl tmp_store.tbl)
      | Public -> process_method ()

(* We're not really using these, but leaving them here in case we ever want to
 * inculde struct typedefs as a language feature of minic *)
let process_type t = let _ = match t with
  | Pstruct _ | Punion _ -> match_type t
  | _ -> raise_msg "invalid type declaration" in ()

let add_name_tables () =
  name_tbls.funcs_tbl <- flip_tbl func_store.tbl;
  name_tbls.methods_tbl <- flip_tbl method_store.tbl;
  name_tbls.vars_tbl <- flip_tbl glob_store.tbl

let map_store store =
  let f tbl name =
    let (i, t) = Hashtbl.find tbl name in
    D.Coq_pair (i, t) in
  coqlist_of_list (List.map (f store.tbl) store.lst)

let typecheck parsed =
  let preprocess_declaration = function
    | Ptype_decl t -> ()
    | Pvar_decl v -> ()
    | Pfunc f -> preprocess_func f in
  let process_declaration = function
    | Ptype_decl t -> process_type t
    | Pvar_decl v -> process_glob glob_store v
    | Pfunc f -> process_func f in
  let () = List.iter preprocess_declaration parsed in
  let () = List.iter process_declaration parsed in
  add_name_tables ();
  let vars = map_store glob_store in
  let funcs = map_store func_store in
  let methods = map_store method_store in
  let constructor = coq_option !constructor in
  name_tbls, Genv.new_genv vars funcs methods constructor
