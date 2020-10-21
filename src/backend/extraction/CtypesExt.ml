module C = Ctypes
module P = Printing

let sprintf = Printf.sprintf
let show_pos = BinNumsExt.show_pos
let show_int = BinNumsExt.show_coq_int
let int_of_pos = BinNumsExt.int_of_pos

let rec flatten_fieldlist = function
  | C.Fnil -> []
  | C.Fcons (i, t, fl) -> (i, t) :: (flatten_fieldlist fl)

let show_kind = function
  | C.Coq_mem -> "mem"
  | C.Coq_stor -> "stor"
  | C.Coq_call -> "call"

let rec show_type = function
  | C.Tvoid -> "void"
  | C.Tint (intsize, signedness) -> (
    match intsize with
    | C.IBool -> "bool"
    | _ ->
     let signed = match signedness with
       | C.Signed -> raise (Failure "signed ints not supported")
       | C.Unsigned -> "int"
     and size = match intsize with
       | C.I8    -> "8"
       | C.I16   -> "16"
       | C.I32   -> "32"
       | C.I256  -> "256"
       | C.IBool -> raise (Failure "unreachable") in
     signed ^ size )
  | C.Tpointer (k, t) -> sprintf "%s* // Kind: %s" (show_type t) (show_kind k)
  | C.Tarray (t, z) -> sprintf "%s[%s]" (show_type t) (show_int z)
  | C.Thashmap (t1, t2) -> sprintf "%s#[%s]" (show_type t1) (show_type t2)
  | C.Tfunction (ts, t) -> sprintf "(%s function)" (show_typelist ts)
  | C.Tstruct (ident, fs) ->
    sprintf "struct %s %s" (P.show_struct ident) (show_fieldlist fs)
  | C.Tunion (ident, fs) ->
    sprintf "union %s %s" (P.show_struct ident) (show_fieldlist fs)
  | C.Tcomp_ptr ident -> sprintf "(%s comp_ptr)" (show_pos ident)
and show_typelist types =
  let rec show_typelist' = function
    | C.Tnil -> ""
    | C.Tcons (t, ts) -> (show_type t) ^ ", " ^ (show_typelist' ts) in
  sprintf "[ %s]" (show_typelist' types)
and show_fieldlist fields =
  let caml_fields = flatten_fieldlist fields in
  let sorted = List.sort
      (fun f1 f2 -> compare (int_of_pos (fst f1))(int_of_pos (fst f2))) caml_fields in
  let mapped = List.map (fun (ident, t) ->
      sprintf "%s%s %s;" (P.tab) (show_type t) (P.show_field ident)) sorted in
  String.concat "\n" (["{"] @ mapped @ ["}"])
