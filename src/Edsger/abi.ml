#include "config.h"

(* Methods related to the Solidity ABI. Right now, the only thing here is the code to compute the method name hashes. 

See https://solidity.readthedocs.io/en/v0.4.24/abi-spec.html for the spec. *)

open Astcommon
open Ast


(* Todo: Solidity uses a primitive type "address", which is an
   unsigned 160-bit integer. So in order to be ABI-compatible with
   token contracts etc, we will need to support that. *)
		    
let eth_string_of_builtin_type = function
  | Tint  -> "uint256"
  | Tuint -> "uint256"
  | Tbool -> "bool"
#ifdef ANT
  | Taddress -> "identity"
#else
  | Taddress -> "address"
#endif
  | Thashvalue -> "uint256"
  | Tunit -> "UNIT"  (* The capitalized types will not be ABI-compatible with Solidity. *)
  | Tglobalpointer -> "UNSUPPORTED"
    
let rec eth_string_of_a_type_desc = function
  | ATbuiltin t  -> eth_string_of_builtin_type t
  | ATdata (id, d) ->  "UNSUPPORTED"    (* Todo: Solidity encodes this as a tuple---but we don't currently support struct arguments anyway. *)
  | ATprod (t1,t2) ->   "(" ^ eth_string_of_a_type_desc t1.aTypeDesc
		      ^ "," ^ eth_string_of_a_type_desc t2.aTypeDesc ^ ")"
  | ATarray(n,t) -> eth_string_of_a_type_desc t.aTypeDesc ^ "[" ^ string_of_int n ^ "]"
  | ATmapping(_,_) -> "UNSUPPOERTED" (*todo*)										      
  | ATlist _ -> "UNSUPPORTED"
  | ATexternal _ -> "UNSUPPORTED"

let rec eth_string_of_a_type t =
  eth_string_of_a_type_desc t.aTypeDesc
		      
let eth_string_of_method_type mt =
  if List.length mt.aMethodArgumentTypes = 1 && (List.nth mt.aMethodArgumentTypes 0).aTypeDesc = ATbuiltin Tunit
  then ""											    
  else String.concat "," (List.map eth_string_of_a_type mt.aMethodArgumentTypes)
		      
let eth_string_of_method_definition md =
  md.aMethodName ^ "(" ^ eth_string_of_method_type md.aMethodType ^ ")"

let eth_string_of_event_type et =
  et.aEventName
  ^ "("
  ^ String.concat "," (List.map (fun (_,t,_) -> eth_string_of_a_type t)
                         et.aEventArgs)
  ^ ")"
  
let json_string_of_arg i t =
    Printf.sprintf "{\"name\":\"%s\", \"type\":\"%s\"}" i (eth_string_of_a_type t)

let json_type_of_function mk =
    if mk == MKconstructor 
    then "constructor"
    else "function" 
								      
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
  | MKconstructor -> "nonpayable"
  | _ -> "payable"

let json_constantness_of_method_kind mk = 
  match mk with
  | MKconst
  | MKconstghost -> "true"
  | _ -> "false"

let json_payable_of_method_kind mk =
  match mk with
  | MKconstructor     
  | MKconst
  | MKconstghost -> "false"
  | _ -> "true"

let json_outputs_of_method_type mt =
  if (mt.aMethodReturnType.aTypeDesc = ATbuiltin Tunit)
  then "[]"
  else Printf.sprintf "[{\"name\":\"\", \"type\":\"%s\"}]" (eth_string_of_a_type mt.aMethodReturnType)
	   
let json_of_method_definition md =
  Printf.sprintf
" {\"type\":\"%s\",
   \"name\":\"%s\",
   \"inputs\":%s,
   \"outputs\":%s,
   \"payable\":%s,
   \"constant\":%s,
   \"stateMutability\":\"%s\"}"
  (json_type_of_function md.aMethodType.aMethodKind)
  md.aMethodName
  (json_inputs_of_method md.aMethodArguments md.aMethodType.aMethodArgumentTypes)
  (json_outputs_of_method_type md.aMethodType)
  (json_payable_of_method_kind md.aMethodType.aMethodKind)	          
  (json_constantness_of_method_kind md.aMethodType.aMethodKind)			       
  (json_stateMutability_of_method_kind md.aMethodType.aMethodKind)

let json_string_of_event_arg = function
  | (i, t, idx) ->
     let t_str = eth_string_of_a_type t in
     Printf.sprintf "{\"name\":\"%s\", \"type\":\"%s\", \"internalType\": \"%s\", \"indexed\": %s}"
       i t_str t_str (string_of_bool idx)
  
let json_inputs_of_event_args args =
  "["
  ^ (String.concat ", " (List.map json_string_of_event_arg args))
  ^ "]"
  
let json_of_event_type et =
  Printf.sprintf
" {\"type\":\"event\",
   \"name\":\"%s\",
   \"inputs\":%s}"
  et.aEventName
  (json_inputs_of_event_args et.aEventArgs)
  
let json_of_layer l ets cts =
  "[" ^
  (json_of_method_definition cts) ^",\n" ^
  (String.concat ",\n"
    (List.concat ((List.map (fun (_,o) -> List.map json_of_method_definition (List.filter (fun (md) -> md.aMethodType.aMethodKind <> MKconstructor) o.aObjectMethods))
			     l.aLayerFreshObjects)
		 @(List.map (fun (_,o) -> List.map json_of_method_definition o.aObjectMethods)
			     l.aLayerPassthroughObjects)
                 @[List.map json_of_event_type ets])))
  ^ "]"
  
open Cryptokit

let keccak_string (str: string) : string =
  let hashval = hash_string (Hash.keccak 256) str in
  Printf.sprintf "0x%02x%02x%02x%02x" (Char.code (String.get hashval 0)) (Char.code (String.get hashval 1)) (Char.code (String.get hashval 2))(Char.code (String.get hashval 3))

let keccak_intval (str: string) : int =
  let hashval = hash_string (Hash.keccak 256) str in
    (0x01000000) * Char.code (String.get hashval 0)
  + (0x00010000) * Char.code (String.get hashval 1)
  + (0x00000100) * Char.code (String.get hashval 2)
  +                Char.code (String.get hashval 3) 

		 
let function_selector_of_method md : string =
  let str = eth_string_of_method_definition md in
  keccak_string str ^ " = \"" ^ str ^ "\"" 


let function_selector_intval_of_method md : int =
  keccak_intval (eth_string_of_method_definition md)

open Backend.BinNums 
  
let event_topic_of_event_type et =
  let str = eth_string_of_event_type et in
  Backend.BinNumsExt.z_of_bytestring (hash_string (Hash.keccak 256) str)
       
