open Backend

type p_type =
  | Pvoid
  | Pint of string
  | Pbool
  | Ppointer of p_type
  | Parray of p_type * int
  | Pmapping of p_type * p_type
  | Pstruct of string * p_variable_declaration list
  | Punion of string * p_variable_declaration list
  | Puserstruct of string
and p_variable_declaration = bool * p_type * string

type p_expr =
  (* Econst_int or Econst_int256 *)
  | Pconst of string
  (* Eglob, Evar and Etempvar *)
  | Pvar of string
  | Pderef of p_expr
  | Punop of Cop.unary_operation * p_expr
  | Pbinop of p_expr * Cop.binary_operation * p_expr
  | Pfield of p_expr * string
  (* Earrayderef and Ehashderef *)
  | Parrayderef of p_expr * p_expr
  | Pcall0 of MachineModel.builtin0
  | Pcall1 of MachineModel.builtin1 * p_expr

type p_statement =
  (* Sassign, Sset, Stransfer *)
  | Passign of p_expr * p_expr
  | Pifthenelse of p_expr * p_statements * p_statements option
  | Ploop of p_statements
  | Pbreak
  | Preturn of p_expr option
  | Ptransfer of p_expr * p_expr
  | Prevert
  | Pcall of string option * string * p_expr list
  | Plog of p_expr list * p_expr list
  | Pcallmethod of p_expr * p_expr list * p_expr * p_expr * p_expr option * p_expr list
and p_statements = p_statement list


type visibility = Public | Private

type p_function = { p_fn_visibility: visibility option;
                    p_fn_name: string;
                    p_fn_return : p_type;
                    p_fn_params : p_variable_declaration list;
                    p_fn_temps : p_variable_declaration list;
                    p_fn_body : p_statement list }

type p_declaration =
  | Ptype_decl of p_type
  | Pvar_decl of p_variable_declaration
  | Pfunc of p_function

type p_file = p_declaration list
