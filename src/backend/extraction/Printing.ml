(* using four sapces instead of the tab character *)
let tab = String.make 4 ' '

let show_field ident =
  Printf.sprintf "field_%s" (BinNumsExt.show_pos ident)

let show_struct ident =
  Printf.sprintf "struct_%s" (BinNumsExt.show_pos ident)

let concat_nonempty l =
  let f s = s <> "" in
  String.concat "\n\n" (List.filter f l)
