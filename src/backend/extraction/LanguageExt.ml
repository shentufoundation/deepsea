module L = Language
module D = Datatypes
module DE = DatatypesExt
module G = Globalenvs

let sprintf = Printf.sprintf
let show_type = CtypesExt.show_type
let show_pos = BinNumsExt.show_pos
let show_int = BinNumsExt.show_int
let style = "* "
let delimiter = String.make 16 '-'

let join_lines a b = a ^ b ^ "\n"

let fn_params' x =
  List.map DE.caml_prod (DE.caml_list (L.fn_params x))

let fn_temps' x =
  List.map DE.caml_prod (DE.caml_list (L.fn_temps x))

let structure xs =
  ("├── " ^ (List.hd xs)) :: List.map (fun x -> "│   " ^ x) (List.tl xs)

let indent prefix xs =
  List.map (fun x -> prefix ^ x) xs

(* for single line representations *)
let mono op xs =
  sprintf "%s %s" op (List.fold_left (fun acc x -> acc ^ x ^ " ") "" xs)

(* for multiline representations *)
let poly op args =
  op :: (List.flatten (List.map structure args))

(* val show_expr : expr -> string list *)
let rec show_expr expr =
  match expr with
  | L.Econst_int (v, t) ->
     [ mono "CONST_INT" [ show_int v ; show_type t ] ]
  | L.Econst_int256 (v, t) ->
     [ mono "CONST_INT256" [ show_int v ; show_type t ] ]
  | L.Evar (ident, t) ->
     [ mono "VAR" [ show_pos ident ; show_type t ] ]
  | L.Etempvar (ident, t) ->
     [ mono "TEMPVAR" [ show_pos ident ; show_type t ] ]
  | L.Ederef (e, t) ->
     poly "DEREF" [ show_expr e ; [ show_type t ] ]
  | L.Eunop (op, e, t) ->
     poly "UNOP" [ [ CopExt.show_unop op ] ; show_expr e ; [ show_type t ] ]
  | L.Ebinop (op, e1, e2, t) ->
     poly "BINOP" [
         [ CopExt.show_binop op ] ;
         show_expr e1 ;
         show_expr e2 ;
         [ show_type t ] ;
     ]
  | L.Efield (e, ident, t) ->
     poly "FIELD" [ show_expr e ; [ show_pos ident ] ; [ show_type t ] ]
  | L.Earrayderef (e1, e2, t) ->
     poly "ARRAYDEREF" [ show_expr e1 ; show_expr e2 ; [ show_type t ] ]
  | L.Ehashderef (e1, e2, t) ->
     poly "HASHDEREF" [ show_expr e1 ; show_expr e2 ; [ show_type t ] ]
  | L.Ecall0 (op, t) ->
     [ mono "CALL0" [ BuiltinSemanticsExt.show0 op ; show_type t ] ]
  | L.Ecall1 (op, e, t) ->
     poly "CALL1" [
         [ BuiltinSemanticsExt.show1 op ] ;
         show_expr e ;
         [ show_type t ] ;
     ]

(* val show_stmt : stmt -> string list *)
let rec show_stmt stmt =
  match stmt with
  | L.Sskip ->
     [ mono "SKIP" [] ]
  | L.Sassign (e1, e2) ->
     poly "ASSIGN" [ show_expr e1 ; show_expr e2 ]
  | L.Sset (ident, e) ->
     poly "SET" [ [ show_pos ident ] ; show_expr e ]
  | L.Scall (ident_option, label, es) ->
     [ mono "CALL UNIMPLEMENTED" [] ]
  | L.Ssequence (s1, s2) -> List.append (show_stmt s1) (show_stmt s2)
  | L.Sifthenelse (e, s1, s2) ->
     poly "IFTHENELSE" [ show_expr e ; show_stmt s1 ; show_stmt s2 ]
  | L.Sloop s ->
     poly "LOOP" [ show_stmt s ]
  | L.Sbreak ->
     [ mono "BREAK" [] ]
  | L.Sreturn expr_option ->
     let repr = match expr_option with
       | D.None -> [ "NONE" ]
       | D.Some e -> show_expr e in
     poly "RETURN" [ repr ]
  | L.Stransfer (e1, e2) ->
     poly "TRANSFER" [ show_expr e1 ; show_expr e2 ]
  | L.Scallmethod (e1, idents, v, e2, es) ->
     [ mono "CALLMETHOD UNIMPLEMENTED" [] ]
  | L.Slog (topics, args) ->
     [ mono "LOG UNIMPLEMENTED" [] ]
  | L.Srevert ->
     [ mono "REVERT" [] ]

let show_body stmt =
  let indented = indent style (show_stmt stmt) in
  List.fold_left join_lines "" indented

let show_params params =
  let mapper (ident, t) = sprintf "%s : %s" (show_pos ident) (show_type t) in
  let indented = indent style (List.map mapper params) in
  List.fold_left join_lines "" indented

let show_coq_function f =
  sprintf "return type: %s\n\nparams:\n%s\ntemps:\n%s\nbody:\n%s"
    (show_type (L.fn_return f))
    (show_params (fn_params' f))
    (show_params (fn_temps' f))
    (show_body (L.fn_body f))

let show_methoddef m id =
  sprintf "%s METHOD %s %s\n\n%s"
    delimiter (show_int id) delimiter (show_coq_function m)

let show_fundef (id, f) =
  sprintf "%sFUNCTION %s %s\n\n%s"
    delimiter (show_pos id) delimiter (show_coq_function f)

let show_fundefs fundefs =
  let sanitized = List.map DE.caml_prod (DE.caml_list (Maps0.PTree.elements fundefs)) in
  List.fold_left join_lines "" (List.map show_fundef sanitized)

let show_ident_list is =
  let is = DE.caml_list is in
  let var_strings = List.map show_pos is in
  let indented = indent style var_strings in
  List.fold_left join_lines "" indented

let show_vars vs =
  sprintf "%s VARS %s\n\n%s" delimiter delimiter (show_ident_list vs)

let show_funcs fs =
  sprintf "%s FUNCS %s\n\n%s" delimiter delimiter (show_ident_list fs)

let show_constructor c =
  match c with
  | D.None    -> sprintf "%s CONSTRUCTOR %s\n\nempty" delimiter delimiter
  | D.Some c' ->
     sprintf "%s CONSTRUCTOR %s\n%s"
       delimiter delimiter (show_coq_function c')

let show_genv {
        G.Genv.genv_vars = vars;
        G.Genv.genv_funcs = funcs;
        G.Genv.genv_methods = methods;
        G.Genv.genv_defs = defs;
        G.Genv.genv_fundefs = fundefs;
        G.Genv.genv_methoddefs = methoddefs;
        G.Genv.genv_constructor = constructor;
} =
  let mapper id =
    match (G.IntMap.get id methoddefs) with
    | D.None   -> "NONE"
    | D.Some f -> show_methoddef f id in
  let method_ids = DE.caml_list methods in
  let methods_repr = List.fold_left join_lines "" (List.map mapper method_ids) in
  let constructor_repr = show_constructor constructor in
  let vars_repr = show_vars vars in
  let funcs_repr = show_funcs funcs in
  let fundefs_repr = show_fundefs fundefs in
  sprintf "%s\n%s\n%s\n%s\n\n%s"
    vars_repr
    funcs_repr
    fundefs_repr
    constructor_repr
    methods_repr
                    
