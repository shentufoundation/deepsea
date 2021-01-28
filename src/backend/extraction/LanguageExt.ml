open ExpMiniC
open StmtMiniC
module L = Language
module D = Datatypes
module DE = DatatypesExt
module G = Globalenvs
module P = Printing
open BinNumsExt
open NameTablesExt
open Maps0

(* let raise_msg s = Core.raise_s (Core.Sexp.Atom s) *)
let _ = Core.raise_s

let sprintf = Printf.sprintf
let show_type = CtypesExt.show_type

let empty_coq_function = {
  fn_return = Tvoid;
  fn_params = D.Coq_nil;
  fn_temps = D.Coq_nil;
  fn_locals = D.Coq_nil;
  fn_body = Sskip
}

let verbose = ref false
let name_tbls = empty_name_tables

let show_ident prefix show_f tbl ident =
  if !verbose then
    try
      sprintf "%s_%s /* %s */" prefix (show_f ident) (Hashtbl.find tbl ident)
    with Not_found -> (* remove this when edsger name tables are finished *)
      sprintf "%s_%s" prefix (show_f ident)
  else
    sprintf "%s_%s" prefix (show_f ident)

let show_global () = show_ident "global" show_pos name_tbls.vars_tbl
let show_func () = show_ident "func" show_pos name_tbls.funcs_tbl
let show_method () = show_ident "method" show_coq_int name_tbls.methods_tbl
let show_func_temp func_ident =
  let tbl = try Hashtbl.find name_tbls.funcs_tmps_tbl func_ident
    with Not_found -> Hashtbl.create 0 in
  show_ident "tmp" show_pos tbl
let show_method_temp method_ident =
  let tbl = try Hashtbl.find name_tbls.methods_tmps_tbl method_ident
    with Not_found -> Hashtbl.create 0 in
  show_ident "tmp" show_pos tbl

let caml_prod_list l =
  List.map DE.caml_prod (DE.caml_list l)

let structure xs =
  ("├── " ^ (List.hd xs)) :: List.map (fun x -> "│   " ^ x) (List.tl xs)

let indent prefix xs =
  List.map (fun x -> prefix ^ x) xs

(* put a newline before and after statement effectively *)
let pad s =
  List.rev ("" :: List.rev ("" :: s))

(* val show_expr : expr -> string *)
let rec show_expr show_tmp expr =
  let show_expr = show_expr show_tmp in
  let expr' = match expr with
  | Econst_int (v, t) ->
    show_coq_int v
  | Econst_int256 (v, t) ->
    show_coq_int v
  | Etempvar (ident, t) ->
    show_tmp ident
  | Evar (ident, t) ->
    show_tmp ident
  | Eglob (ident, t) -> 
    show_global () ident
  | Ederef (e, t) ->
    "*" ^ show_expr e
  | Eunop (op, e, t) ->
    let s = show_expr e in
    let symb = CopExt.show_unop_symb op in
      sprintf "%s%s" symb s
  | Ebinop (op, e1, e2, t) ->
    let s1 = show_expr e1 in
    let s2 = show_expr e2 in
    let symb = (CopExt.show_binop_symb op) in
      sprintf "(%s %s %s)" s1 symb s2
  | Efield (e, ident, t) ->
    sprintf "%s.%s" (show_expr e) (P.show_field ident)
  | Eindex (e1, e2, t) ->
    sprintf "%s[%s]" (show_expr e1) (show_expr e2)
  | Ecall0 (op, t) ->
    sprintf "%s" (BuiltinSemanticsExt.show0 op)
  | Ecall1 (op, e, t) ->
    sprintf "%s(%s)"
      (BuiltinSemanticsExt.show1 op)
      (show_expr e)
  | Eaddr (_, _) -> ""
  in
  if !verbose then
    sprintf "%s /* %s */" expr' (show_type (typeof expr))
  else
    expr'

let show_expr_opt show_tmp = function
  | D.None -> ""
  | D.Some e -> show_expr show_tmp e

(* val show_stmt : stmt -> string list *)
let rec show_stmt show_tmp indent stmt =
  let show_stmt = show_stmt show_tmp in
  let show_expr = show_expr show_tmp in
  let s = match stmt with
  | Sskip ->
    [  ]
  | Sassign (e1, e2) ->
    [ sprintf "%s = %s;" (show_expr e1) (show_expr e2) ]
  | Sset (ident, e) ->
    [ sprintf "%s = %s;" (show_tmp ident) (show_expr e) ]
  | Scall (ident_option, label, es) -> (match ident_option with
    | None -> [ sprintf "%s(%s);" (show_func () label)
      (String.concat ", " (List.map show_expr (DE.caml_list es))) ]
    | Some i -> [ sprintf "%s <- %s(%s);" (show_tmp i) (show_func () label)
      (String.concat ", " (List.map show_expr (DE.caml_list es))) ])
  | Ssequence (s1, s2) ->
  List.append (show_stmt false s1) (show_stmt false s2)
  | Sifthenelse (e, s1, s2) ->
    pad (List.flatten [
      [ sprintf "if (%s) {" (show_expr e) ];
      show_stmt true s1;
      [ "} else {" ];
      show_stmt true s2;
      [ "}" ];
    ])
  | Sloop s ->
    pad (List.flatten [
    (* TODO: add "true" and "false" keywords *)
      [ "while {" ] ;
      show_stmt true s;
      [ "}" ];
    ])
  | Sbreak ->
    [ "break;" ]
  | Sreturn ident_option -> (
    match ident_option with
      | D.None -> [ ]
      | D.Some i ->
        [ sprintf "return %s;" (show_tmp i) ])
  | Stransfer (e1, e2) ->
    [ sprintf "transfer(%s, %s);" (show_expr e1) (show_expr e2) ]
  | Scallmethod (e1, idents, e2, v, g, es) ->
      [ sprintf "callmethod(%s);" (String.concat "; " [
          (show_expr e1);
          (String.concat ", " (List.map show_tmp (DE.caml_list idents)));
          (show_coq_int e2);
          (show_expr v);
          (show_expr_opt show_tmp g);
          (String.concat ", " (List.map show_expr (DE.caml_list es)))
        ])
      ]
  | Slog (topics, args) -> let f l = String.concat ", " (List.map show_expr l) in
    [ sprintf "emit(%s; %s);" (f (DE.caml_list topics)) (f (DE.caml_list args)) ]
  | Srevert ->
    [ "revert;" ]
  in
  if indent then
    (List.map (( ^ ) P.tab) s)
  else s

let show_body show_tmp stmt =
  String.concat "\n" (show_stmt show_tmp true stmt)

let show_params show_tmp params =
  let mapper (ident, t) =
    sprintf "%s %s" (show_type t) (show_tmp ident) in
  String.concat ", " (List.map mapper params)

let sort_id l =
  (* List.sort (fun (id1, _) (id2, _) -> compare (int_of_pos id1) (int_of_pos id2)) l *)
  l

let show_temps show_tmp params =
  let mapper (ident, t) = sprintf "%s%s %s;" P.tab (show_type t) (show_tmp ident) in
  let l = List.map mapper params in
  String.concat "\n" l

let show_locals show_tmp params =
  let mapper (ident, t) = sprintf "%smemory %s %s;" P.tab (show_type t) (show_tmp ident) in
  let l = List.map mapper params in
  String.concat "\n" l

let show_coq_function show_tmp f =
  P.concat_nonempty
    [show_temps show_tmp (caml_prod_list f.fn_temps);
     show_locals show_tmp (caml_prod_list f.fn_locals);
     show_body show_tmp (f.fn_body)]

let show_methoddef m id =
  let ret = (show_type (fn_return m)) in
  let show_tmp = show_method_temp id in
  let params = (show_params show_tmp (caml_prod_list m.fn_params)) in
  sprintf "%s %s(%s) {\n%s\n}"
    ret (show_method () id) params (show_coq_function show_tmp m)

let show_fundef (id, f) =
  let ret = (show_type (fn_return f)) in
  let show_tmp = show_func_temp id in
  let params = (show_params show_tmp (caml_prod_list f.fn_params)) in
  sprintf "private %s %s(%s) {\n%s\n}"
    ret (show_func () id) params (show_coq_function show_tmp f)

let show_fundefs funcs fundefs =
  let get_fundef i = match PTree.get i fundefs with
    | D.None -> raise (Failure "None function")
    | D.Some func -> (i, func) in
  let l = List.map get_fundef (DE.caml_list funcs) in
  let l' = List.map show_fundef l in
  String.concat "\n\n" l'

let show_defs vars defs =
  let get_def i = match PTree.get i defs with
    | D.None -> raise (Failure "None definition")
    | D.Some def -> (i, def) in
  let l = List.map get_def (DE.caml_list vars) in
  let f (ident, t) = sprintf "%s %s;" (show_type t) (show_global () ident) in
  let l' = List.map f l in
  String.concat "\n" l'

let show_funcs is =
  let is = DE.caml_list is in
  let l = List.map (show_func ()) is in
  String.concat "\n" l

let show_constructor c =
  match c with
  | D.None    -> ""
  | D.Some c' ->
    let show_tmp = show_ident "tmp" show_pos name_tbls.constructor_tmps_tbl in
    let params = (show_params show_tmp (caml_prod_list c'.fn_params)) in
    sprintf "void constructor(%s) {\n%s\n}" params (show_coq_function show_tmp c')

let show_genv v nt {
  G.Genv.genv_vars = vars;
  G.Genv.genv_funcs = funcs;
  G.Genv.genv_methods = methods;
  G.Genv.genv_defs = defs;
  G.Genv.genv_fundefs = fundefs;
  G.Genv.genv_methoddefs = methoddefs;
  G.Genv.genv_constructor = constructor;
} =
  if v then verbose := true;
  copy_name_tables name_tbls nt;
  let mapper id =
    match (G.IntMap.get id methoddefs) with
    | D.None   -> raise (Failure "method id mapped to None")
    | D.Some f -> show_methoddef f id in
  let method_ids = DE.caml_list methods in
  let methods_repr = String.concat "\n\n" (List.map mapper method_ids) in
  let constructor_repr = show_constructor constructor in
  let defs_repr = show_defs vars defs in
  let fundefs_repr = show_fundefs funcs fundefs in
  P.concat_nonempty
    [
      defs_repr;
      fundefs_repr;
      constructor_repr;
      methods_repr;
    ]
