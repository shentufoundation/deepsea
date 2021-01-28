open Ast
open Backend
  open CopExt
  open Printf
  open MachineModelExt

let rec show_p_expr = function
  | Pconst s -> s
  | Pvar s -> s
  | Pderef e -> "*" ^ (show_p_expr e)
  | Punop (unop, e) -> (show_unop_symb unop) ^ (show_p_expr e)
  | Pbinop (e1, binop, e2) -> sprintf "(%s %s %s)" (show_p_expr e1) (show_binop_symb binop) (show_p_expr e2)
  | Pfield (e, s) -> sprintf "%s.%s" (show_p_expr e) s
  | Parrayderef (e1, e2) -> sprintf "%s[%s]" (show_p_expr e1) (show_p_expr e2)
  | Pcall0 b -> show_builtin0 b
  | Pcall1 (b, e) -> sprintf "%s(%s)" (show_builtin1 b) (show_p_expr e)
