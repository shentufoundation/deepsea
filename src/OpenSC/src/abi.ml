(* Methods related to the Solidity ABI. Right now, the only thing here is the code to compute the method name hashes. 

See https://solidity.readthedocs.io/en/v0.4.24/abi-spec.html for the spec. *)


open Ast
open Sast

(* String.sub s start len  *)

(* Todo: Solidity uses a primitive type "address", which is an
   unsigned 160-bit integer. So in order to be ABI-compatible with
   token contracts etc, we will need to support that. *)
		    
let eth_string_of_builtin_type = function
  | Int  -> "uint256"
  | Uint(_) -> "uint256"
  | Bool -> "bool"
  | Address x -> "uint256"
  | Void x -> "UNIT"  (* The capitalized types will not be ABI-compatible with Solidity. *)
  | _ -> "UNSUPPORTED"
		      
let eth_string_of_method_type pt =
  if List.length pt = 0
  then ""
  else String.concat "," (List.map eth_string_of_builtin_type pt)
		      
let eth_string_of_method_definition m = 
  let name_of_method = function
  (_, (SStrLit s)) -> s in
  let params_types_of_method declist = 
    List.map (fun (Var(e,t)) -> t) declist in
  (name_of_method m.smethodname) ^ "(" ^ eth_string_of_method_type (params_types_of_method m.sparams) ^ ")"

(* let json_string_of_arg i t =
    Printf.sprintf "{\"name\":\"%s\", \"type\":\"%s\"}" i (eth_string_of_a_type t)
								      
let json_inputs_of_method idents argtypes =
    if List.length argtypes = 1 && (List.nth argtypes 0).aTypeDesc = ATbuiltin Tunit
    then "[]"									      
    else "["
	 ^ (String.concat "," (List.map2 json_string_of_arg idents argtypes))
	 ^ "]"

let json_stateMutability_of_method_kind mk =
  match mk with
  | MKconst    
  | MKconstghost -> "view"
  | _ -> "payable"

let json_constantness_of_method_kind mk = 
  match mk with
  | MKconst    
  | MKconstghost -> "true"
  | _ -> "false"

let json_outputs_of_method_type mt =
  if (mt.aMethodReturnType.aTypeDesc = ATbuiltin Tunit)
  then "[]"
  else Printf.sprintf "[{\"name\":\"\", \"type\":\"%s\"}]" (eth_string_of_a_type mt.aMethodReturnType)
	   
let json_of_method_definition md =
  Printf.sprintf
" {\"type\":\"function\",
   \"name\":\"%s\",
   \"inputs\":%s,
   \"outputs\":%s,
   \"payable\":true,
   \"constant\":%s,
   \"stateMutability\":\"%s\"}"
  md.aMethodName
  (json_inputs_of_method md.aMethodArguments md.aMethodType.aMethodArgumentTypes)
  (json_outputs_of_method_type md.aMethodType)
  (json_constantness_of_method_kind md.aMethodType.aMethodKind)			       
  (json_stateMutability_of_method_kind md.aMethodType.aMethodKind)

let json_of_layer l =
  "[" ^
  (String.concat ",\n"
    (List.concat ((List.map (fun (_,o) -> List.map json_of_method_definition o.aObjectMethods)
			     l.aLayerFreshObjects)
		 @(List.map (fun (_,o) -> List.map json_of_method_definition o.aObjectMethods)
			     l.aLayerPassthroughObjects))))
    ^ "]" *)
  
open Cryptokit

let keccak_string (str: string) : string =
  let hashval = hash_string (Hash.keccak 256) str in
  Printf.sprintf "0x%02x%02x%02x%02x" (Char.code (String.get hashval 0)) (Char.code (String.get hashval 1)) (Char.code (String.get hashval 2))(Char.code (String.get hashval 3))
(*    (0x01000000) * Char.code (String.get hashval 0)
  + (0x00010000) * Char.code (String.get hashval 1)
  + (0x00001000) * Char.code (String.get hashval 2)
  +                Char.code (String.get hashval 3) *)


let keccak_intval (str: string) : int =
  let hashval = hash_string (Hash.keccak 256) str in
    (0x01000000) * Char.code (String.get hashval 0)
  + (0x00010000) * Char.code (String.get hashval 1)
  + (0x00000100) * Char.code (String.get hashval 2)
  +                Char.code (String.get hashval 3) 

		 
let function_selector_of_method md : string =
  let str = eth_string_of_method_definition md in
  keccak_string str ^ " = \"" ^ str ^"\"" 

let function_selector_intval_of_method md : int =
  keccak_intval (eth_string_of_method_definition md)
