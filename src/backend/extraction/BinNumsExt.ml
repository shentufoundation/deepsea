open BinNums

let rec show_pos_bin = function
  | Coq_xI v -> (show_pos_bin v) ^ "1"
  | Coq_xO v -> (show_pos_bin v) ^ "0"
  | Coq_xH   -> "1"

let rec int_of_pos p =
    let bin = show_pos_bin p in
    Z.of_string ("0b" ^ bin)

let rec show_pos p =
    let dec = int_of_pos p in
    Z.to_string dec

let rec show_int_bin = function
  | Z0 -> "Z0"
  | Zpos x -> Printf.sprintf "ZPOS%s" (show_pos x)
  | Zneg x -> Printf.sprintf "ZNEG-%s" (show_pos x)

let rec int_of_coq_int = function
    | Z0 -> Z.zero
    | Zpos x -> int_of_pos x
    | Zneg x -> Z.neg (int_of_pos x)

let show_coq_int i = Z.to_string (int_of_coq_int i)

let rec show_nat = function
  | N0     -> "N0"
  | Npos x -> "N" ^ show_pos x

let (--) i j =
    let rec aux n acc =
      if n < i then acc else aux (n-1) (n :: acc)
    in aux j []

let cons_bit = function
  | true, None -> Some Coq_xH
  | false, None -> None
  | true , Some v -> Some (Coq_xI v)
  | false, Some v -> Some (Coq_xO v)

let positive_of_byte b rest =
  List.fold_right
    (fun i v -> cons_bit ((b land (1 lsl i) <> 0), v))
    (0--7)
    rest

let positive_of_bytestring_aux bytes rest =
  List.fold_right
    (fun i v -> positive_of_byte (int_of_char (String.get bytes i)) v)
    (List.rev (0--(String.length bytes -1)))
    rest

let z_of_bytestring bytes =
  match positive_of_bytestring_aux bytes None with
  | None -> Z0
  | Some v -> Zpos v

let positive_of_num b rest base =
  List.fold_right
    (fun i v -> cons_bit ((b land (1 lsl i) <> 0), v))
    (0--base)
    rest

let rec positive_of_int n =
    if n = 1 then
      Coq_xH
    else if (n land 1) = 1 then
      Coq_xI (positive_of_int (n asr 1))
    else
      Coq_xO (positive_of_int (n asr 1))

let rec positive_of_Z n =
    if n = Z.one then
      Coq_xH
    else if (Z.logand n Z.one) = Z.one then
      Coq_xI (positive_of_Z (Z.shift_right n 1))
    else
      Coq_xO (positive_of_Z (Z.shift_right n 1))

let coq_Z_of_int n =
    if n = 0 then Z0
    else if n > 0 then Zpos (positive_of_int n)
    else Zneg (positive_of_int (-n))

let coq_Z_of_Z n =
    if n = Z.zero then Z0
    else if n > Z.zero then Zpos (positive_of_Z n)
    else Zneg (positive_of_Z (Z.neg n))

let positive_of_numstring_base bytes rest base =
  let base_len =
    match base with
    "0x" -> 3
    | "0o" -> 2
    | "0b" -> 0
    | _ -> raise (Failure "not supported number prefix")
  in

  List.fold_right
    (fun i v -> positive_of_num (int_of_string (base ^ (String.sub bytes i 1))) v base_len)
    (List.rev (0--(String.length bytes -1)))
    rest

let decimalstring2binarystring bytes =
  let odds2one c =
    match c with
    '1' | '3' | '5' | '7' | '9' -> 1
    | _ -> 0
  in
  let dividebytwo bytes =
    let rec dividebytwo_wise bytes new_s add =

      let c = String.get bytes 0 in
      let bytes = String.sub bytes 1 (String.length bytes -1) in
      let new_dgt = ((int_of_char c) - (int_of_char '0')) / 2 + add  in
      let new_s = Printf.sprintf  ("%s%d") new_s new_dgt in
      let add = odds2one(c) * 5 in
      if String.length bytes = 0 then new_s
      else
      dividebytwo_wise bytes new_s add
    in
    dividebytwo_wise bytes "" 0
  in
  let rec decimal2bin bytes stack =
    let rec zerotrim bytes =
      begin
      if String.length bytes > 1 && String.get bytes 0 = '0' then
        zerotrim (String.sub bytes 1 (String.length bytes - 1))
      else
        bytes
      end
    in
    let bytes = if String.length bytes > 0 then zerotrim bytes else bytes in
    if bytes = "0" then
      begin
      match stack with
      "" -> "0"
      | _ -> stack
      end
    else
      let new_bytes = dividebytwo bytes in
      let new_dgt = odds2one (String.get bytes (String.length bytes -1)) in
      let stack = Printf.sprintf ("%d%s") new_dgt stack in
      decimal2bin new_bytes stack
  in
  "0b" ^ (decimal2bin bytes "")

let stringaddstring a b =
  let rec straddstr a b add stack =

    match a, b, add with
    | "", "", true -> Printf.sprintf ("%d%s") 1 stack
    | "", _, false -> Printf.sprintf ("%s%s") b stack
    | _, "", false -> Printf.sprintf ("%s%s") a stack
    | "", _, true ->
      let b_end = String.get b (String.length b -1) in
      let b' = String.sub b 0 (String.length b -1) in
      begin
      match b_end with
      | '9' ->
        begin
        let stack = Printf.sprintf ("%c%s") '0' stack in
        straddstr "" b' true stack
        end
      | _ ->
        let c = char_of_int(int_of_char b_end + 1) in
        let stack = Printf.sprintf ("%c%s") c stack in
        straddstr "" b' false stack
        end
    | _, "", true ->
      let a_end = String.get a (String.length a -1) in
      let a' = String.sub a 0 (String.length a -1) in
      begin
      match a_end with
      | '9' ->
        begin
        let stack = Printf.sprintf ("%c%s") '0' stack in
        straddstr  a' "" true stack
        end
      | _ ->
        let c = char_of_int (int_of_char a_end + 1) in
        let stack = Printf.sprintf ("%c%s") c stack in
        straddstr "" a' false stack
        end
    | _, _, _ ->
      let a_end = String.get a (String.length a -1) in
      let b_end = String.get b (String.length b -1) in
      let a' = String.sub a 0 (String.length a -1) in
      let b' = String.sub b 0 (String.length b -1) in
      let c = if add
        then int_of_char a_end + int_of_char b_end +1 - (int_of_char '0') * 2
        else int_of_char a_end + int_of_char b_end - (int_of_char '0') * 2
      in
      let add' = (c / 10 = 1) in
      let c' = String.get (Printf.sprintf ("%d") c)
        (String.length (Printf.sprintf ("%d") c) -1)  in
      let stack' = Printf.sprintf ("%c%s") c' stack in
      straddstr a' b' add' stack'
  in
  straddstr a b false ""

let rec strtimenum bytes n =
  match n with
  | 0 -> "0"
  | 1 -> bytes
  | n  -> stringaddstring bytes (strtimenum bytes (n-1))

let numstring2decimalstring bytes =
  if String.length bytes > 2 then
  let bytes_left = String.sub bytes 2 (String.length bytes -2) in
  begin
  let num_prefix =  String.sub bytes 0 2 in
  let base =
  match num_prefix with
  "0x" -> 16
  | "0o" -> 8
  | "0b" -> 2
  | _ -> 10
  in
  if base = 10 then bytes
  else
  begin
  let rec num2decimal bytes stack base_t =
    if String.length bytes = 0 then stack
    else
    let bytes_end = String.sub bytes (String.length bytes -1) 1 in
    let bytes_end_num = int_of_string (num_prefix ^ bytes_end) in
    let bytes_left = String.sub bytes 0 (String.length bytes -1) in
    let stack' = stringaddstring stack (strtimenum base_t bytes_end_num )
    in
    num2decimal bytes_left stack' (strtimenum base_t base)
  in
  num2decimal bytes_left "0" "1"
  end
  end
  else
  bytes


(* TODO: support large decimal string to Coq z int *)
let z_of_numstring bytes =
  if String.length bytes > 2 then
    let num_prefix =  String.sub bytes 0 2 in
    let num_literal = String.sub bytes 2 (String.length bytes -2) in
    let z =
      match num_prefix with
      | "0x" | "0o" | "0b" ->
      begin
      match (positive_of_numstring_base num_literal None num_prefix) with
      | None -> Z0
      | Some v -> Zpos v
      end
      | _ -> coq_Z_of_int (int_of_string bytes)
    in
    z
  else
    coq_Z_of_int (int_of_string bytes)

