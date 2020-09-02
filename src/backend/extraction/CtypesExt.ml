module C = Ctypes

let sprintf = Printf.sprintf
let show_pos = BinNumsExt.show_pos
let show_int = BinNumsExt.show_int

let rec show_type = function
  | C.Tvoid -> "void"
  | C.Tint (intsize, signedness) ->
     let signed = match signedness with
       | C.Signed -> "signed"
       | C.Unsigned -> "unsigned"
     and size = match intsize with
       | C.I8    -> "8"
       | C.I16   -> "16"
       | C.I32   -> "32"
       | C.I256  -> "256"
       | C.IBool -> "bool" in
     signed ^ size
  | C.Tpointer t -> sprintf "(%s pointer" (show_type t)
  | C.Tarray (t, z) -> sprintf "(%s %s array)" (show_type t) (show_int z)
  | C.Thashmap (t1, t2) -> sprintf "(%s %s hashmap)" (show_type t1) (show_type t2)
  | C.Tfunction (ts, t) -> sprintf "(%s function)" (show_typelist ts)
  | C.Tstruct (ident, fs) ->
     sprintf "(%s %s struct)" (show_pos ident) (show_fieldlist fs)
  | C.Tunion (ident, fs) ->
     sprintf "(%s %s union)" (show_pos ident) (show_fieldlist fs)
  | C.Tcomp_ptr ident -> sprintf "(%s comp_ptr)" (show_pos ident)
and show_typelist types =
  let rec show_typelist' = function
    | C.Tnil -> ""
    | C.Tcons (t, ts) -> (show_type t) ^ ", " ^ (show_typelist' ts) in
  sprintf "[ %s]" (show_typelist' types)
and show_fieldlist fields =
  let rec show_fieldlist' = function
    | C.Fnil -> ""
    | C.Fcons (ident, t, fs) ->
       sprintf "%s : %s , %s" (show_pos ident) (show_type t) (show_fieldlist' fs) in
  sprintf "[ %s]" (show_fieldlist' fields)
