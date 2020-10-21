module D = Datatypes

let rec eval_nat_tail d n = 
  match d with
  | D.O -> n
  | D.S rest -> (eval_nat_tail rest (n+1))

let rec eval_nat d = eval_nat_tail d 0

let rec caml_list = function
  | D.Coq_nil -> []
  | D.Coq_cons (a, l) -> a :: caml_list l

let caml_prod = function
  | D.Coq_pair (a, b) -> (a, b)

let caml_bool = function
  | D.Coq_true -> true
  | D.Coq_false -> false

let caml_option = function
  | D.Some a -> Some a
  | D.None -> None

let coq_option = function
  | Some a -> D.Some a
  | None -> D.None

let bit_of_bool = function
  | D.Coq_true -> 1
  | D.Coq_false -> 0

let char_of_ascii = function
  | Ascii.Ascii(b0, b1, b2, b3, b4, b5, b6, b7) ->
     Char.chr (bit_of_bool b0
               +   2 * bit_of_bool b1
               +   4 * bit_of_bool b2
               +   8 * bit_of_bool b3
               +  16 * bit_of_bool b4
               +  32 * bit_of_bool b5
               +  64 * bit_of_bool b6
               + 128 * bit_of_bool b7)

let rec caml_string = function
  | String0.EmptyString -> ""
  | String0.String(a, s) -> String.make 1 (char_of_ascii a) ^ caml_string s

let rec coqlist_of_list = function
  | [] -> D.Coq_nil
  | x::xs -> D.(Coq_cons (x, coqlist_of_list xs))
