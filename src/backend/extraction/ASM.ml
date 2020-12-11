open Integers
open StmtCGraph
open ExpCintptr

module Links = Map.Make(struct type t = AST.label let compare = compare end)

type asm =
| EVM_STOP
| EVM_ADD
| EVM_MUL
| EVM_SUB
| EVM_DIV
| EVM_SDIV
| EVM_MOD
| EVM_SMOD
| EVM_ADDMOD
| EVM_MULMOD
| EVM_EXP
| EVM_SIGNEXTEND
| EVM_LT
| EVM_GT
| EVM_SLT
| EVM_SGT
| EVM_EQ
| EVM_ISZERO
| EVM_AND
| EVM_OR
| EVM_XOR
| EVM_NOT
| EVM_BYTE
| EVM_SHA3
| EVM_ADDRESS
| EVM_BALANCE
| EVM_ORIGIN
| EVM_CALLER
| EVM_CALLVALUE
| EVM_CALLDATALOAD
| EVM_CALLDATASIZE
| EVM_CODESIZE
| EVM_GASPRICE
| EVM_EXTCODESIZE
| EVM_BLOCKHASH
| EVM_COINBASE
| EVM_TIMESTAMP
| EVM_NUMBER
| EVM_DIFFICULTY
| EVM_GASLIMIT
| EVM_CHAINID
| EVM_SELFBALANCE
| EVM_GAS
| EVM_CODECOPY
| EVM_POP
| EVM_MLOAD
| EVM_MSTORE
| EVM_MSTORE8
| EVM_SLOAD
| EVM_SSTORE
| EVM_JUMP
| EVM_JUMPI
| EVM_JUMPDEST of string
| EVM_PUSH of int * string
| EVM_DUP of int
| EVM_SWAP of int
| EVM_LOG of int
| EVM_CALL
| EVM_REVERT
| EVM_RETURN
| EVM_TOTAL_LENGTH of int

type asm_program  = asm list
type evm_program  = EVM.evm list

type intermediate = {
    constructor : evm_program ;
    body        : evm_program
  }

(* we assume jump destinations can fit in 4 bytes *)
let address_bytes = 4


let pad len s =
  String.make (len - (String.length s)) '0' ^ s

(* compute number of bytes needed to represent a value *)
let allocate =
  let allocate' p =
    let rec count_digits = function
      | BinNums.Coq_xI v -> 1 + count_digits v
      | BinNums.Coq_xO v -> 1 + count_digits v
      | BinNums.Coq_xH   -> 1
    in (count_digits p + 7) / 8
  in function
  | BinNums.Z0 -> 1
  | BinNums.Zpos x -> allocate' x
  | BinNums.Zneg x -> raise (Failure "allocate is undefined on negative numbers")

(* construct a mapping from label -> PC *)
let map_labels program = 
  let rec map_labels' program counter =
    match program with
    | [] -> Links.empty
    | (x :: xs) ->
       match x with
       | EVM.Coq_evm_label l -> Links.add l counter (map_labels' xs (counter + 1))
       | EVM.Coq_evm_totallength l -> map_labels' xs (counter + 3)
       | EVM.Coq_evm_push_label l -> map_labels' xs (counter + 5)
       | EVM.Coq_evm_push v -> map_labels' xs (counter + 1 + allocate v)
       | _ -> map_labels' xs (counter + 1)
  in map_labels' program 0

let hex x = Printf.sprintf "%x" x

(* compute the bytecode for some push data *) 
let assemble_pushdata n data =
  let hex_of_coq_int =
    let rec hex_of_coq_pos partial count =
      let rec pow2 = function
        | 0 -> 1
        | n -> 2 * (pow2 (n-1)) in
      let lastchar = if count == 4 then Printf.sprintf "%x" partial else ""
      and partial = if count == 4 then 0 else partial
      and count = count mod 4
      in function
      | BinNums.Coq_xI rest -> (hex_of_coq_pos (partial + (pow2 count)) (count + 1) rest) ^ lastchar
      | BinNums.Coq_xO rest -> (hex_of_coq_pos partial (count + 1) rest) ^ lastchar
      | BinNums.Coq_xH -> (hex (partial + (pow2 count))) ^ lastchar
    in function
    | BinNums.Z0 -> "0"
    | BinNums.Zpos v -> hex_of_coq_pos 0 0 v
    | BinNums.Zneg v -> raise (Failure "undefined")
  in
  pad (n * 2) (hex_of_coq_int data)

let rec hex_of_coq_pos partial count =
  let rec pow2 = function
    | 0 -> 1
    | n -> 2 * (pow2 (n-1)) in
  let lastchar = if count == 4 then Printf.sprintf "%x" partial else ""
  and partial = if count == 4 then 0 else partial
  and count = count mod 4
  in function
  | BinNums.Coq_xI rest -> (hex_of_coq_pos (partial + (pow2 count)) (count + 1) rest) ^ lastchar
  | BinNums.Coq_xO rest -> (hex_of_coq_pos partial (count + 1) rest) ^ lastchar
  | BinNums.Coq_xH -> (hex (partial + (pow2 count))) ^ lastchar

(* hex_of_coq_pos 0 0 v *)

let rec scgraph_expr e = 
  let spos = hex_of_coq_pos 0 0 in
  match e with
  | Econst_int (i, t) -> "int"
  | Econst_int256 (v, t) -> "0x" ^ assemble_pushdata (allocate v) v
  | Etempvar (i, t) -> "tempvar_" ^ spos i
  | Esload (e, t) -> "(sload " ^ scgraph_expr e ^ ")"
  | Emload (e, t) -> "(mload " ^ scgraph_expr e ^ ")"
  | Eaddr (e, t) -> "(addr " ^ scgraph_expr e ^ ")"
  | Eunop (uo, e, t) -> "(unop @ " ^ scgraph_expr e ^ ")"
  | Ebinop (bo, e1, e2, t) -> "(binop @ " ^ scgraph_expr e1 ^ " " ^ scgraph_expr e2 ^ ")"
  | Ecall0 (b0, t) -> "call0"
  | Ecall1 (b1, e, t) -> "(call1 " ^ scgraph_expr e ^ ")"

let getlabel id s = 
  let spos = hex_of_coq_pos 0 0 in
  match s with 
  | StmtCGraph.Sskip n -> 
    "\"" ^ (spos id) ^ " : skip\""
  | Ssassign (e1, e2, n) -> 
    "\"" ^ (spos id) ^ " : " ^ scgraph_expr e1 ^ " :s= " ^ scgraph_expr e2 ^ "\""
  | Smassign (e1, e2, n) -> 
    "\"" ^ (spos id) ^ " : " ^ scgraph_expr e1 ^ " :m= " ^ scgraph_expr e2 ^ "\""
  | Sset (i, e, n) -> 
    "\"" ^ (spos id) ^ " : " ^ "tempvar_" ^ spos i ^ " = " ^ scgraph_expr e ^ "\""
  | Scall (retval, l, el, n) -> 
    "\"" ^ (spos id) ^ " : call\""
  | Scond (e, n, n') -> 
    "\"" ^ (spos id) ^ " : if " ^ scgraph_expr e ^ "\""
  | Shash (e1, e2, eo, n') -> 
    "\"" ^ (spos id) ^ " : shash  \""
  | Sreturn (eo, n) -> 
    "\"" ^ (spos id) ^ " : return \""
  | Sdone -> 
    "\"" ^ (spos id) ^ " : done \""
  | Stransfer (e1, e2, n, n') -> 
    "\"" ^ (spos id) ^ " : transfer \""
  | StmtCGraph.Scallmethod (eil, il, i, e, el, n, n') ->
    "\"" ^ (spos id) ^ " : callmethod \""
  | Slog (el1, el2, n) -> 
    "\"" ^ (spos id) ^ " : log \""
  | Srevert ->
    "\"" ^ (spos id) ^ " : revert \""

let mnemonics_cgraph cd en = 
  let spos = hex_of_coq_pos 0 0 in
  "digraph mygraph {
  node [shape=box];

  \"entry\"
  \"entry\" -> \"" ^ spos en ^ "\"\n" ^  
  String.concat "\n"
  (List.map (fun x -> 
    match x with
    | (id, s) ->
      let label = getlabel id s in
      let transitionsn n = 
        String.concat "\n"
        (List.map
        (fun x -> match x with | (id, s') -> label ^ " -> " ^ getlabel id s')
        (List.filter (fun x -> match x with | (id, s) -> if id = n then true else false) cd))
      in
      match s with
      | Sskip n -> 
        label ^ "\n" ^ transitionsn n
      | Smassign (e1, e2, n) -> 
        label ^ "\n" ^ transitionsn n
      | Ssassign (e1, e2, n) -> 
        label ^ "\n" ^ transitionsn n
      | Sset (i, e, n) -> 
        label ^ "\n" ^ transitionsn n
      | Scall (io, l, el, n) -> 
        label ^ "\n" ^ transitionsn n
      | Scond (e, n1, n2) -> 
        label ^ "\n" ^ transitionsn n1 ^ "\n" ^ transitionsn n2
      | Sreturn (eo, n) -> 
        label ^ "\n" ^ transitionsn n
      | Shash (e1, e2, eo, n) -> 
        label ^ "\n" ^ transitionsn n
      | Sdone -> 
        label ^ "\n"
      | Stransfer (e1, e2, n1, n2) -> 
        label ^ "\n" ^ transitionsn n1 ^ "\n" ^ transitionsn n2
      | Scallmethod (eil, il, i, e, el, n1, n2) ->
        label ^ "\n" ^ transitionsn n1 ^ "\n" ^ transitionsn n2
      | Slog (el1, el2, n) -> 
        label ^ "\n" ^ transitionsn n
      | Srevert -> 
        label ^ "\n"
    ) cd)
  ^ "\n}"

let mnemonics_clash cg = 
  let spos = hex_of_coq_pos 0 0 in
  "digraph mygraph {
  node [shape=box];\n" ^
  String.concat "\n"
  (List.map (fun x -> 
    let label a = "\" tempvar_" ^ (spos a) ^ " \"" in
    match x with
    | (id, l) ->
      label id ^ "\n" ^ 
      (String.concat "\n"
      (List.map (fun xx -> 
        label id ^ " -> " ^ label xx
      ) l))
    ) cg)
  ^ "\n}"

let constructor_counter = ref (-1)

(* compute the bytecode for an opcode *)
let assemble_op x = pad 2 (hex x)

(* evm -> asm *)
let transform_inst links = function
  | EVM.Coq_evm_stop               -> EVM_STOP
  | EVM.Coq_evm_add                -> EVM_ADD
  | EVM.Coq_evm_mul                -> EVM_MUL
  | EVM.Coq_evm_sub                -> EVM_SUB
  | EVM.Coq_evm_div                -> EVM_DIV
  | EVM.Coq_evm_sdiv               -> EVM_SDIV
  | EVM.Coq_evm_mod                -> EVM_MOD
  | EVM.Coq_evm_smod               -> EVM_SMOD
  | EVM.Coq_evm_addmod             -> EVM_ADDMOD
  | EVM.Coq_evm_mulmod             -> EVM_MULMOD
  | EVM.Coq_evm_exp                -> EVM_EXP
  | EVM.Coq_evm_signextend         -> EVM_SIGNEXTEND
  | EVM.Coq_evm_lt                 -> EVM_LT
  | EVM.Coq_evm_gt                 -> EVM_GT
  | EVM.Coq_evm_slt                -> EVM_SLT
  | EVM.Coq_evm_sgt                -> EVM_SGT
  | EVM.Coq_evm_eq                 -> EVM_EQ
  | EVM.Coq_evm_iszero             -> EVM_ISZERO
  | EVM.Coq_evm_and                -> EVM_AND
  | EVM.Coq_evm_or                 -> EVM_OR
  | EVM.Coq_evm_xor                -> EVM_XOR
  | EVM.Coq_evm_not                -> EVM_NOT
  | EVM.Coq_evm_byte               -> EVM_BYTE
  | EVM.Coq_evm_sha3               -> EVM_SHA3
  | EVM.Coq_evm_address            -> EVM_ADDRESS
  | EVM.Coq_evm_balance            -> EVM_BALANCE
  | EVM.Coq_evm_origin             -> EVM_ORIGIN
  | EVM.Coq_evm_caller             -> EVM_CALLER
  | EVM.Coq_evm_callvalue          -> EVM_CALLVALUE
  | EVM.Coq_evm_calldataload       -> EVM_CALLDATALOAD
  | EVM.Coq_evm_calldatasize       -> EVM_CALLDATASIZE
  | EVM.Coq_evm_codesize           -> EVM_CODESIZE
  | EVM.Coq_evm_gasprice           -> EVM_GASPRICE
  | EVM.Coq_evm_extcodesize        -> EVM_EXTCODESIZE
  | EVM.Coq_evm_blockhash          -> EVM_BLOCKHASH
  | EVM.Coq_evm_coinbase           -> EVM_COINBASE
  | EVM.Coq_evm_timestamp          -> EVM_TIMESTAMP
  | EVM.Coq_evm_number             -> EVM_NUMBER
  | EVM.Coq_evm_chainid            -> EVM_CHAINID
  | EVM.Coq_evm_selfbalance        -> EVM_SELFBALANCE
  | EVM.Coq_evm_difficulty         -> EVM_DIFFICULTY
  | EVM.Coq_evm_gaslimit           -> EVM_GASLIMIT
  | EVM.Coq_evm_gas                -> EVM_GAS
  | EVM.Coq_evm_codecopy           -> EVM_CODECOPY
  | EVM.Coq_evm_pop                -> EVM_POP
  | EVM.Coq_evm_mload              -> EVM_MLOAD
  | EVM.Coq_evm_mstore             -> EVM_MSTORE
  | EVM.Coq_evm_mstore8            -> EVM_MSTORE8
  | EVM.Coq_evm_sload              -> EVM_SLOAD
  | EVM.Coq_evm_sstore             -> EVM_SSTORE
  | EVM.Coq_evm_jump               -> EVM_JUMP
  | EVM.Coq_evm_jumpi              -> EVM_JUMPI
  | EVM.Coq_evm_label l            -> EVM_JUMPDEST (Printf.sprintf "%08x" (Links.find l links))
  | EVM.Coq_evm_push x ->
     EVM_PUSH (allocate x, assemble_pushdata (allocate x) x)
  | EVM.Coq_evm_push_label l ->
     EVM_PUSH (address_bytes, Printf.sprintf "%08x" (Links.find l links))
  | EVM.Coq_evm_dup n ->
     EVM_DUP (DatatypesExt.eval_nat n)
  | EVM.Coq_evm_swap n ->
     EVM_SWAP (DatatypesExt.eval_nat n)
  | EVM.Coq_evm_log n              -> EVM_LOG (DatatypesExt.eval_nat n)
  | EVM.Coq_evm_call               -> EVM_CALL
  | EVM.Coq_evm_revert             -> EVM_REVERT
  | EVM.Coq_evm_return             -> EVM_RETURN
  | EVM.Coq_evm_totallength n      -> EVM_TOTAL_LENGTH (DatatypesExt.eval_nat n)

(* asm -> bytecode *)
let assemble_inst programsize = function
  | EVM_STOP -> "00"
  | EVM_ADD -> "01"
  | EVM_MUL -> "02"
  | EVM_SUB -> "03"
  | EVM_DIV -> "04"
  | EVM_SDIV -> "05"
  | EVM_MOD -> "06"
  | EVM_SMOD -> "07"
  | EVM_ADDMOD -> "08"
  | EVM_MULMOD -> "09"
  | EVM_EXP -> "0a"
  | EVM_SIGNEXTEND -> "0b"
  | EVM_LT -> "10"
  | EVM_GT -> "11"
  | EVM_SLT -> "12"
  | EVM_SGT -> "13"
  | EVM_EQ -> "14"
  | EVM_ISZERO -> "15"
  | EVM_AND -> "16"
  | EVM_OR -> "17"
  | EVM_XOR -> "18"
  | EVM_NOT -> "19"
  | EVM_BYTE -> "1a"
  | EVM_SHA3 -> "20"
  | EVM_ADDRESS -> "30"
  | EVM_BALANCE -> "31"
  | EVM_ORIGIN -> "32"
  | EVM_CALLER -> "33"
  | EVM_CALLVALUE -> "34"
  | EVM_CALLDATALOAD -> "35"
  | EVM_CALLDATASIZE -> "36"
  | EVM_CODESIZE -> "38"
  | EVM_GASPRICE -> "3a"
  | EVM_EXTCODESIZE -> "3b"
  | EVM_BLOCKHASH -> "40"
  | EVM_COINBASE -> "41"
  | EVM_TIMESTAMP -> "42"
  | EVM_NUMBER -> "43"
  | EVM_DIFFICULTY -> "44"
  | EVM_GASLIMIT -> "45"
  | EVM_CHAINID -> "46"
  | EVM_SELFBALANCE -> "47"
  | EVM_GAS -> "5a"
  | EVM_CODECOPY -> "39"
  | EVM_POP -> "50"
  | EVM_MLOAD -> "51"
  | EVM_MSTORE -> "52"
  | EVM_MSTORE8 -> "53"
  | EVM_SLOAD -> "54"
  | EVM_SSTORE -> "55"
  | EVM_JUMP -> "56"
  | EVM_JUMPI -> "57"
  | EVM_JUMPDEST l -> "5b"
  | EVM_PUSH (n, data) ->
     (assemble_op (95 + n)) ^ data
  | EVM_DUP n -> assemble_op (127 + n)
  | EVM_SWAP n -> assemble_op (143 + n)
  | EVM_LOG n -> assemble_op (160 + n) 
  | EVM_CALL -> "f1"
  | EVM_REVERT -> "fd"
  | EVM_RETURN -> "f3"
  | EVM_TOTAL_LENGTH n -> let total_length =  hex(programsize + ((n) * 32)) in
                         "61" ^ (pad 4 total_length)

let show_asm_inst = function
  | EVM_STOP -> "STOP"
  | EVM_ADD -> "ADD"
  | EVM_MUL -> "MUL"
  | EVM_SUB -> "SUB"
  | EVM_DIV -> "DIV"
  | EVM_SDIV -> "SDIV"
  | EVM_MOD -> "MOD"
  | EVM_SMOD -> "SMOD"
  | EVM_ADDMOD -> "ADDMOD"
  | EVM_MULMOD -> "MULMOD"
  | EVM_EXP -> "EXP"
  | EVM_SIGNEXTEND -> "SIGNEXTEND"
  | EVM_LT -> "LT"
  | EVM_GT -> "GT"
  | EVM_SLT -> "SLT"
  | EVM_SGT -> "SGT"
  | EVM_EQ -> "EQ"
  | EVM_ISZERO -> "ISZERO"
  | EVM_AND -> "AND"
  | EVM_OR -> "OR"
  | EVM_XOR -> "XOR"
  | EVM_NOT -> "NOT"
  | EVM_BYTE -> "BYTE"
  | EVM_SHA3 -> "SHA3"
  | EVM_ADDRESS -> "ADDRESS"
  | EVM_BALANCE -> "BALANCE"
  | EVM_ORIGIN -> "ORIGIN"
  | EVM_CALLER -> "CALLER"
  | EVM_CALLVALUE -> "CALLVALUE"
  | EVM_CALLDATALOAD -> "CALLDATALOAD"
  | EVM_CALLDATASIZE -> "CALLDATASIZE"
  | EVM_CODESIZE -> "CODESIZE"
  | EVM_GASPRICE -> "GASPRICE"
  | EVM_EXTCODESIZE -> "EXTCODESIZE"
  | EVM_BLOCKHASH -> "BLOCKHASH"
  | EVM_COINBASE -> "COINBASE"
  | EVM_TIMESTAMP -> "TIMESTAMP"
  | EVM_NUMBER -> "NUMBER"
  | EVM_DIFFICULTY -> "DIFFICULTY"
  | EVM_GASLIMIT -> "GASLIMIT"
  | EVM_CHAINID -> "CHAINID"
  | EVM_SELFBALANCE -> "SELFBALANCE"
  | EVM_GAS -> "GAS"
  | EVM_CODECOPY -> "CODECOPY"
  | EVM_POP -> "POP"
  | EVM_MLOAD -> "MLOAD"
  | EVM_MSTORE -> "MSTORE"
  | EVM_MSTORE8 -> "MSTORE8"
  | EVM_SLOAD -> "SLOAD"
  | EVM_SSTORE -> "SSTORE"
  | EVM_JUMP -> "JUMP"
  | EVM_JUMPI -> "JUMPI"
  | EVM_JUMPDEST l -> ("L"^ l ^ ": JUMPDEST")
  | EVM_PUSH (n, data) ->
     Printf.sprintf "%-16s %s" (Printf.sprintf "PUSH%d" n) data
  | EVM_DUP n -> Printf.sprintf "DUP%d" n
  | EVM_SWAP n -> Printf.sprintf "SWAP%d" n
  | EVM_CALL -> "CALL"
  | EVM_REVERT -> "REVERT"
  | EVM_RETURN -> "RETURN"
  | EVM_LOG n -> Printf.sprintf "LOG%d" n
  | EVM_TOTAL_LENGTH n ->  Printf.sprintf "TOTAL_LENGTH%d" n (* this is a dummy total length for futher calculation when push the constructor arguments *)

let show_evm program =
  List.fold_left (fun acc x -> acc ^ "\n" ^ (EVMExt.show x)) "" program

let split program label =
  let rec constructor_helper target acc = function
    | x :: xs ->
       if (x = target) then
         acc
       else
         constructor_helper target (acc @ [x]) xs
    | [] -> []
  and body_helper target = function
    | x :: xs ->
       if (x = target) then
         x :: xs
       else
         body_helper target xs
    | [] -> []
  in
  {
    constructor = constructor_helper (EVM.Coq_evm_label label) [] program ;
    body = body_helper (EVM.Coq_evm_label label) program
  }

let size_of_inst = function
  | EVM_PUSH (n, data) -> 1 + n
  | EVM_TOTAL_LENGTH n -> 3
  | _ -> 1

let size_of_program asm =
  List.fold_left (fun acc x -> acc + (size_of_inst x)) 0 asm 

let assemble program =
  let programsize = size_of_program program in
  let bytecode_list = List.map (assemble_inst programsize) program in
  List.fold_left (fun acc x -> acc ^ x) "" bytecode_list

let transform program entrypoint =
  let transform_intermediate program =
    let transform' links p =
      List.map (transform_inst links) p in
    let chop_links n =
      Links.map (fun x -> x - n) in
    let links = map_labels (program.constructor @ program.body) in
    let constructor_asm = transform' links program.constructor in
    let constructor_size = size_of_program constructor_asm in
    let links' = chop_links constructor_size links in
    let body_asm = transform' links' program.body in
    (constructor_asm @ body_asm), body_asm
  in
  transform_intermediate (split program entrypoint)

let mnemonics program =
  List.fold_left (fun acc x -> acc ^ "\n" ^ (show_asm_inst x)) "" program


(* Frontend for WebAssembly *)

(* coq_module = { coq_M_types : functype list; coq_M_funcs : func list;
                    coq_M_tables : table list; coq_M_mems : mem list;
                    coq_M_globals : global list; coq_M_elem : elem list;
                    coq_M_data : data list; coq_M_start : start option;
                    coq_M_imports : import list; coq_M_exports : export list } *)

(* type func = { coq_F_type : nat; coq_F_locals : valtype list; coq_F_body : expr } *)

let rec indent lvl = 
  if lvl = 0 then "" else "  "  ^ indent (lvl - 1)

let matchtypes tx = 
  match tx with
  | Types.T_i32 -> "i32"
  | Types.T_i64 -> "i64"
  | Types.T_f32 -> "f32"
  | Types.T_f64 -> "f64"

(* address, data, value, result, path, topic1, topic2, topic3, topic4 *)
(* corresponds to 0, 256, 256 * 2, 256 * 3, ... *)
let wasm_frontend_data = 
  (* wasm_f131_data ^ "\n" ^ *)
  (String.concat "\n"
  [
    (*
    (indent 1) ^ "(data (i32.const 1179648) \"0\")"; (* address *)
    (indent 1) ^ "(data (i32.const 1179904) \"0\")"; (* data *)
    (indent 1) ^ "(data (i32.const 1180160) \"0\")"; (* value *)
    (indent 1) ^ "(data (i32.const 1180416) \"0\")"; (* result *)
    (indent 1) ^ "(data (i32.const 1180672) \"0\")"; (* path *)
    (indent 1) ^ "(data (i32.const 1180928) \"0\")"; (* topic1 *)
    (indent 1) ^ "(data (i32.const 1181184) \"0\")"; (* topic2 *)
    (indent 1) ^ "(data (i32.const 1181440) \"0\")"; (* topic3 *)
    (indent 1) ^ "(data (i32.const 1181696) \"0\")"; (* topic4 *)
    *)

    (indent 1) ^ "(data (i32.const 0) \"0\")"; (* address *)
    (indent 1) ^ "(data (i32.const 256) \"0\")"; (* data *)
    (indent 1) ^ "(data (i32.const 512) \"0\")"; (* value *)
    (indent 1) ^ "(data (i32.const 768) \"0\")"; (* result *)
    (indent 1) ^ "(data (i32.const 1024) \"0\")"; (* path *)
    (indent 1) ^ "(data (i32.const 1280) \"0\")"; (* topic1 *)
    (indent 1) ^ "(data (i32.const 1536) \"0\")"; (* topic2 *)
    (indent 1) ^ "(data (i32.const 1792) \"0\")"; (* topic3 *)
    (indent 1) ^ "(data (i32.const 2048) \"0\")"; (* topic4 *)
  ])

(* three globals reserved for scratch space *)
let wasm_frontend_global =
  indent 1 ^ "(global (mut i32) (i32.const 0))" ^ 
  indent 1 ^ "(global (mut i32) (i32.const 0))" ^ 
  indent 1 ^ "(global (mut i32) (i32.const 0))"

let wasm_frontend_mems = 
  (indent 1) ^ "(memory (;0;) 20)"

(* aux function definitions *)
let wasm_frontend_aux_fucs : string =
  (* pow *)
  indent 1 ^ "(func $f128 (param $0 i32) (param $1 i32) (result i32)
    (local $2 i32)
    (local $3 i32)
    (local.set $3
    (select
      (local.get $0)
      (i32.const 1)
      (i32.and
      (local.get $1)
      (i32.const 1)
      )
    )
    )
    (block $label$0
    (br_if $label$0
      (i32.eqz
      (local.tee $1
        (i32.shr_s
        (local.get $1)
        (i32.const 1)
        )
      )
      )
    )
    (loop $label$1
      (local.set $3
      (i32.mul
        (select
        (local.tee $0
          (i32.mul
          (local.get $0)
          (local.get $0)
          )
        )
        (i32.const 1)
        (i32.and
          (local.get $1)
          (i32.const 1)
        )
        )
        (local.get $3)
      )
      )
      (local.set $1
      (local.tee $2
        (i32.shr_s
        (local.get $1)
        (i32.const 1)
        )
      )
      )
      (br_if $label$1
      (local.get $2)
      )
    )
    )
    (local.get $3))" ^ "\n" ^ 
  (* sha1, $f129, not used *)
  indent 1 ^ "(func $f129 (param $p0 i32) (param $p1 i32) (result i32) 
      local.get 0
    )" ^ "\n" ^
  (* sha2, $f130, not used *)
  indent 1 ^ "(func $f130 (param $p0 i32) (param $p1 i32) (result i32) 
      local.get 0
    )" ^ "\n" ^
  (* sha3, $f131, seperate definition *)
  (* wasm_f131_func ^ "\n" ^ *)
  (* indent 1 ^ "(func $f131 (param $p0 i32) (param $p1 i32) (result i32) 
      local.get 0
    )" ^ "\n" ^ *)
  (* notint, $f132 *)
  indent 1 ^ "(func $f132 (param $p0 i32) (result i32)
    local.get $p0
    i32.const -1
    i32.xor)" ^ "\n" ^ 
  (* setup return state, used for debugging & revert messages *)
  indent 1 ^ "(func $set_returndata (param $value i32) (result )
    i32.const 256 ;; offset to store
    local.get $value ;; valut to store
    i32.store
  )" ^ "\n" ^ 
  (* fallback, reverts state *)
  indent 1 ^ "(func $fallback (param ) (result) 
    i32.const 256
    i32.const 4
    call $f28
    )" ^ "\n" ^ 
  indent 1 ^ "(func $chendian32 (param $p0 i32) (result i32)
    local.get $p0
    i32.const 24
    i32.shl
    local.get $p0
    i32.const 8
    i32.shl
    i32.const 16711680
    i32.and
    i32.or
    local.get $p0
    i32.const 8
    i32.shr_u
    i32.const 65280
    i32.and
    local.get $p0
    i32.const 24
    i32.shr_u
    i32.or
    i32.or)"

let wasm_frontend_imports is_runtime = 
  (indent 1) ^ "(import  \"ethereum\" \"getAddress\"  (func $f0 (param i32) (result )))" ^ "\n" ^
  (indent 1) ^ "(import  \"ethereum\" \"getExternalBalance\"  (func $f1 (param i32 i32) (result )))" ^ "\n" ^
  (indent 1) ^ "(import  \"ethereum\" \"getBlockHash\"  (func $f2 (param i64 i32) (result i32)))" ^ "\n" ^
  (indent 1) ^ "(import  \"ethereum\" \"callDataCopy\"  (func $f4 (param i32 i32 i32) (result )))" ^ "\n" ^
  (indent 1) ^ "(import  \"ethereum\" \"getCallDataSize\"  (func $f5 (param ) (result i32)))" ^ "\n" ^
  (indent 1) ^ "(import  \"ethereum\" \"callDelegate\"  (func $f7 (param i64 i32 i32 i32) (result i32)))" ^ "\n" ^
  (indent 1) ^ "(import  \"ethereum\" \"storageStore\"  (func $f8 (param i32 i32) (result )))" ^ "\n" ^
  (indent 1) ^ "(import  \"ethereum\" \"storageLoad\"  (func $f9 (param i32 i32) (result )))" ^ "\n" ^
  (indent 1) ^ "(import  \"ethereum\" \"getCaller\"  (func $f10 (param i32) (result )))" ^ "\n" ^
  (indent 1) ^ "(import  \"ethereum\" \"getCallValue\"  (func $f11 (param i32) (result )))" ^ "\n" ^
  (indent 1) ^ "(import  \"ethereum\" \"getBlockCoinbase\"  (func $f14 (param i32) (result i32)))" ^ "\n" ^
  (indent 1) ^ "(import  \"ethereum\" \"getBlockDifficulty\"  (func $f16 (param i32) (result )))" ^ "\n" ^
  (indent 1) ^ "(import  \"ethereum\" \"getGasLeft\"  (func $f19 (param ) (result i64)))" ^ "\n" ^
  (indent 1) ^ "(import  \"ethereum\" \"getBlockGasLimit\"  (func $f20 (param ) (result i64)))" ^ "\n" ^
  (indent 1) ^ "(import  \"ethereum\" \"getTxGasPrice\"  (func $f21 (param i32) (result )))" ^ "\n" ^
  (indent 1) ^ "(import  \"ethereum\" \"log\"  (func $f22 (param i32 i32 i32 i32 i32 i32 i32) (result )))" ^ "\n" ^
  (indent 1) ^ "(import  \"ethereum\" \"getBlockNumber\"  (func $f23 (param ) (result i64)))" ^ "\n" ^
  (indent 1) ^ "(import  \"ethereum\" \"getTxOrigin\"  (func $f24 (param i32) (result )))" ^ "\n" ^
  (indent 1) ^ "(import  \"ethereum\" \"useGas\"  (func $f25 (param i64) (result )))" ^ "\n" ^
  (indent 1) ^ "(import  \"ethereum\" \"getBlockTimestamp\"  (func $f27 (param ) (result i64)))" ^ "\n" ^
  (indent 1) ^ "(import  \"ethereum\" \"revert\"  (func $f28 (param i32 i32) (result )))" ^ "\n" ^
  (indent 1) ^ "(import  \"ethereum\" \"getReturnDataSize\"  (func $f29 (param ) (result i32)))" ^ "\n" ^ (* special EEI for SOLL *)
  (indent 1) ^ "(import  \"ethereum\" \"returnDataCopy\"  (func $f30 (param i32 i32 i32) (result )))" ^ "\n" ^
  (indent 1) ^ "(import  \"ethereum\" \"call\"  (func $f3 (param i64 i32 i32 i32 i32) (result i32)))" ^ "\n" ^

  (if is_runtime then "" else (
    (* (indent 1) ^ "(import  \"ethereum\" \"selfDestruct\"  (func $f26 (param i32) (result )))" ^ "\n" ^ *)
    (* (indent 1) ^ "(import  \"ethereum\" \"externalCodeCopy\"  (func $f17 (param i32 i32 i32 i32) (result )))" ^ "\n" ^ *)
    (* (indent 1) ^ "(import  \"ethereum\" \"getExternalCodeSize\"  (func $f18 (param i32) (result i32)))" ^ "\n" ^ *)
    (* (indent 1) ^ "(import  \"ethereum\" \"create\"  (func $f15 (param i32 i32 i32 i32) (result i32)))" ^ "\n" ^ *)
    (* (indent 1) ^ "(import  \"ethereum\" \"callCode\"  (func $f6 (param i64 i32 i32 i32 i32) (result i32)))" ^ "\n" ^ *)
    (* (indent 1) ^ "(import  \"ethereum\" \"codeCopy\"  (func $f12 (param i32 i32 i32) (result )))" ^ "\n" ^ *)
    (* (indent 1) ^ "(import  \"ethereum\" \"getCodeSize\"  (func $f13 (param ) (result i32)))" ^ "\n" *)
    "" (* disabled above abi for SOLL *)
  )) ^ 

  (indent 1) ^ "(import  \"ethereum\" \"finish\"  (func $finish (param i32 i32) (result )))"

let wasm_frontend_exports = 
  String.concat "\n"
  [ 
    (indent 1) ^ "(export \"memory\" (memory 0))";
    (indent 1) ^ "(export \"main\" (func $main))";
  ]

(* typeidx still start from 0, since it cannot be indexed by name *)
let wasm_frontend_types (tl: Types.functype list) = 
  String.concat "\n"
  (List.mapi (fun idx tlx -> 
                          let idx = idx in
                          match tlx with
                          | Types.FT (pt, rt) -> 
                            indent 1 ^ 
                            "(type " ^ "(;" ^ string_of_int idx ^ ";) " ^ "(func " ^
                            "(param " ^
                            (String.concat " " (List.map matchtypes (DatatypesExt.caml_list pt))) ^
                            ")" ^ " " ^
                            "(result " ^ 
                            (String.concat " " (List.map matchtypes (DatatypesExt.caml_list rt))) ^
                            ")" ^ ")" ^ ")"
              ) tl)

let matchblocktype (bt: Structure.blocktype) = 
  match bt with
  | BT_typeidx n -> "(type " ^ string_of_int (DatatypesExt.eval_nat n) ^ ")"
  | BT_valtype vo -> 
    (match DatatypesExt.caml_option vo with 
    | None -> ""
    | Some vt -> "(result " ^ matchtypes vt ^ ")")

let rec wasm_frontend_expr (i: Structure.instr) (ind: int): string = 
  match i with
  | Const v -> 
    (match v with
    | Coq_i32 vl -> indent ind ^  "i32.const " ^ string_of_int (DatatypesExt.eval_nat (Int0.I32.to_nat vl))
    | _ -> print_endline "unexpected value other than i32"; exit 1)
  | Const_256 v -> 
    indent ind ^  "i32.const 0x" ^ assemble_pushdata (allocate v) v
  | Const_nat v -> 
    indent ind ^  "i32.const " ^ string_of_int (DatatypesExt.eval_nat v)
  | Unop unop -> 
    (match unop with
    | Values.Coq_i32 cunop -> 
      (match cunop with 
      | Clz -> indent ind ^ "i32.clz"
      | Ctz -> indent ind ^ "i32.ctz"
      | Popcnt -> indent ind ^ "i32.popcnt")
    | _ -> print_endline "unexpected value other than i32"; exit 1)
  | Binop binop ->
    (match binop with
    | Values.Coq_i32 cbinop -> 
      (match cbinop with 
      | Add -> indent ind ^ "i32.add"
      | Sub -> indent ind ^ "i32.sub"
      | Mul -> indent ind ^ "i32.mul"
      | Div sx -> indent ind ^ "i32.div_u"
      | Rem sx -> indent ind ^ "i32.rem_u"
      | And -> indent ind ^ "i32.and"
      | Or -> indent ind ^ "i32.or"
      | Xor -> indent ind ^ "i32.xor"
      | Shl -> indent ind ^ "i32.shl"
      | Shr sx -> indent ind ^ "i32.shr_u"
      | Rotl -> indent ind ^ "i32.rotl"
      | Rotr -> indent ind ^ "i32.rotr")
    | _ -> print_endline "unexpected value other than i32"; exit 1)
  | Testop testop ->
    (match testop with
    | Values.Coq_i32 ctestop -> indent ind ^ "i32.eqz"
    | _ -> print_endline "unexpected value other than i32"; exit 1)
  | Relop relop -> 
    (match relop with
    | Values.Coq_i32 crelop -> 
      (match crelop with 
      | Eq -> indent ind ^ "i32.eq"
      | Ne -> indent ind ^ "i32.ne"
      | Lt sx -> indent ind ^ "i32.lt_u"
      | Gt sx -> indent ind ^ "i32.gt_u"
      | Le sx -> indent ind ^ "i32.le_u"
      | Ge sx -> indent ind ^ "i32.ge_u")
    | _ -> print_endline "unexpected value other than i32"; exit 1)
  | Memop memop -> 
    (match memop with
    | Values.Coq_i32 cmemop -> 
      (match cmemop with 
      | Store -> indent ind ^ "i32.store"
      | Load -> indent ind ^ "i32.load")
    | _ -> print_endline "unexpected value other than i32"; exit 1)
  | Cvtop cvtop -> 
    (match cvtop with
    | Values.Coq_i32 ccvtop -> 
      (match ccvtop with 
      | Wrap_i64 -> indent ind ^ "i32.wrap_i64"
      | Extend_i32 sx -> indent ind ^ "i64.extend_i32_u"
      | Trunc_f32 sx -> indent ind ^ "i32.trunc_f32_u"
      | Trunc_f64 sx -> indent ind ^ "i32.trunc_f64_u"
      | Reinterpret -> indent ind ^ "i32.reinterpret_f32")
    | _ -> print_endline "unexpected value other than i32"; exit 1)
  | Drop -> indent ind ^ "drop"
  | Select -> indent ind ^ "select"
  | Local_get n -> indent ind ^ "local.get " ^ string_of_int (DatatypesExt.eval_nat n)
  | Local_set n -> indent ind ^ "local.set " ^ string_of_int (DatatypesExt.eval_nat n)
  | Local_tee n -> indent ind ^ "indent ind ^ local.tee " ^ string_of_int (DatatypesExt.eval_nat n)
  | Global_get n -> indent ind ^ "global.get " ^ string_of_int (DatatypesExt.eval_nat n)
  | Global_set n -> indent ind ^ "global.set " ^ string_of_int (DatatypesExt.eval_nat n)
  | Nop -> indent ind ^ "nop"
  | Unreachable -> indent ind ^ "unreachable"
  | Block (bt, il) -> 
    indent ind ^ "(block " ^ matchblocktype bt ^ "\n" ^
    String.concat "\n"
    (List.map
      (fun fx -> wasm_frontend_expr fx (ind + 1))
      (DatatypesExt.caml_list il)) ^ "\n" ^
    indent ind ^ ")"
  | Loop (bt, il) -> 
    indent ind ^ "(block loop " ^ matchblocktype bt ^ "\n" ^
    String.concat "\n"
    (List.map
      (fun fx -> wasm_frontend_expr fx (ind + 1))
      (DatatypesExt.caml_list il)) ^ "\n" ^
    indent (ind+1) ^ "br 0" ^ "\n" ^ 
    indent ind ^ "end)"
  | If (bt, ilt, ile) -> 
    (* if *)
    indent ind ^ "if " ^ matchblocktype bt ^ "\n" ^
    (* then *)
    String.concat "\n"
    (List.map
      (fun fx -> wasm_frontend_expr fx (ind+1))
      (DatatypesExt.caml_list ilt)) ^ "\n" ^
    (* else *)
    indent (ind) ^ "else " ^ "\n" ^ 
    String.concat "\n"
    (List.map
      (fun fx -> wasm_frontend_expr fx (ind+1))
      (DatatypesExt.caml_list ile)) ^ "\n" ^
    indent ind ^ "end"
  | Br n -> indent ind ^ "br " ^ string_of_int (DatatypesExt.eval_nat n)
  | Br_if n -> indent ind ^ "br_if " ^ string_of_int (DatatypesExt.eval_nat n)
  | Br_table (nl, n) -> (print_endline "unexpected command br_table"; exit 1) (* not supported *)
  | Return -> indent ind ^ "return"
  | Call n -> indent ind ^ "call $f" ^ string_of_int (DatatypesExt.eval_nat n)
  | Call_indirect n -> indent ind ^ "call_indirect $f" ^ string_of_int (DatatypesExt.eval_nat n)

let rec list_make n = 
  (if n == 0 then [] else List.cons 0 (list_make (n-1)))

let rec change_endian s = 
  let len = String.length s in 
  String.mapi (fun idx c -> (if idx mod 2 = 0 then String.get s (len - idx - 2) else String.get s (len - idx))) s

(* DeepSEA ABI: first 4 bytes for hash, then 4 bytes for each parameter *)
let wasm_frontend_multiplexer func_abi has_constructor tl is_runtime =
  (* call constructor --> disabled for now *)
  (* (if ((not is_runtime) && has_constructor) then indent 2 ^ "call $constructor\n" else "") ^ *)
      indent 2 ^ "call $f5
    global.set 0
    global.get 0
    i32.const 3
    i32.le_u
    if 
      i32.const 21
      call $set_returndata
      call $fallback
    else 
      nop
    end
    global.get 0
    i32.const 4
    i32.sub
    global.set 0
    i32.const 4
    global.set 1\n" ^
    (* (loop
      global.get 1
      global.get 0
      i32.eq
      if (result i32)
        (br 1)
      else 
        i32.const 768
        global.get 1
        i32.const 4
        call $f4
        i32.const 768
        i32.load
        (i32.add (global.get 1) (i32.const 4))
        (global.set 1)
      end
      (br 0)
    ) *)
    indent 2 ^ "i32.const 768
    i32.const 0
    i32.const 4
    call $f4
    i32.const 768
    i32.load
    call $chendian32
    global.set 2\n" ^ 

  String.concat "\n"
  (List.mapi 
    (fun idx v -> 
      let funcoffset = (if has_constructor then 1 else 0) in
      let ptn = (match List.nth tl (idx+funcoffset) with
      | Types.FT (pt, rt) -> 
      (List.length (DatatypesExt.caml_list pt))) in
      let rtn = (match List.nth tl (idx+funcoffset) with
      | Types.FT (pt, rt) -> 
      (List.length (DatatypesExt.caml_list rt))) in
      indent 2 ^ "global.get 2\n" ^ 
      indent 2 ^ "i32.const 0x" ^ 
      assemble_pushdata (allocate v) v ^ 
      (* (let orig_endian = assemble_pushdata (allocate v) v in if is_runtime then change_endian orig_endian else orig_endian) ^  *)
      "\n" ^  (* need to reverse for runtime *)
      indent 2 ^ "i32.eq\n" ^
      indent 2 ^ "if \n" ^ 
      indent 3 ^ "global.get 0
      i32.const " ^ string_of_int (ptn * 32) ^ "\n" ^
      indent 3 ^ "i32.eq
      i32.eqz
      if 
        i32.const 22" ^ string_of_int idx ^ " 
        call $set_returndata
        call $fallback
      else 
        nop
      end
      i32.const 32 ;; 4 + 7 * 4 = 32, setting offset exclude name
      global.set 1\n" ^ 
      (String.concat "\n"
      (List.map 
        (fun _ -> 
        indent 3 ^ "i32.const 768
      global.get 1
      i32.const 4
      call $f4
      i32.const 768
      i32.load ;; load argument, which is i32
      call $chendian32
      (i32.add (global.get 1) (i32.const 32)) ;; + 8 * 4 = + 32
      (global.set 1)")
      (list_make ptn))) ^ "\n" ^
      indent 3 ^ "(call " ^ ("$f" ^ string_of_int (idx+256+funcoffset)) ^ ")\n" ^ 

      (* display problem for burrow *)
      (* indent 3 ^ "(call $chendian32)\n" ^ *)

      (* store the return value *)
      (if rtn = 0 then "" else 
      indent 3 ^ "global.set 1
      i32.const 256
      global.get 1
      i32.store\n") ^ 
      (* Dealing of `finish` *)
      indent 3 ^ "i32.const 256\n" ^
      indent 3 ^ "i32.const " ^  
        string_of_int (rtn * 4)
      ^ "\n" ^
      indent 3 ^ "(call $finish)\n" ^ 
      indent 2 ^ "else"
    )
  func_abi) ^ "\n" ^
  indent 3 ^ "i32.const 22\n" ^ 
  indent 3 ^ "call $set_returndata\n" ^ 
  indent 3 ^ "(call $fallback)" ^ "\n" ^ 
  (String.concat "\n"
    (List.map 
      (fun x -> indent 3 ^ "end")
  func_abi)) ^ "\n"

(* funcidx start from 256, first 255 is reserved for EEI and aux, aux starts at 128 *)
let wasm_frontend_funcs (fl: Structure.func list) (has_constructor: bool) 
(func_abi: Int.int list) (tl: Types.functype list) (is_runtime: bool) =
  let printflx (idx: int) (flx: Structure.func) = 
    let ptn = (match List.nth tl (idx) with
    | Types.FT (pt, rt) -> 
    (List.length (DatatypesExt.caml_list pt))) in
    let rtn = (match List.nth tl (idx) with
    | Types.FT (pt, rt) -> 
    (List.length (DatatypesExt.caml_list rt))) in
    let idx = idx + 256 in
    (* dealing of constructor *)
    let name = if (idx = 256 && has_constructor) then "$constructor" else ("$f" ^ string_of_int idx) in
    (* let thisABIRaw = List.nth func_abi (idx-256) in *)
    (* function signature *)
    (* indent 1 ^ ";; function ABI: " ^ "0x" ^ assemble_pushdata (allocate thisABIRaw) thisABIRaw ^ "\n" ^  *)
    indent 1 ^ 
    "(func " ^ name ^ " " ^
    "(type " ^ (string_of_int ((DatatypesExt.eval_nat flx.coq_F_type))) ^ ")" ^ "\n" ^
    (* function local declaration *)
    (* (if List.length (list_make ptn) = 0 then "" else
    (String.concat "\n"
    (List.mapi 
      (fun idx lx -> indent 2 ^ "(local " ^ "$l" ^ string_of_int idx ^ " i32)")
      (list_make ptn)) ^ "\n")) ^ *)
    (* parameter above *)
    (if List.length (DatatypesExt.caml_list flx.coq_F_locals) = 0 then "" else
    (String.concat "\n"
    (List.mapi 
      (fun idx lx -> indent 2 ^ "(local " ^ "$l" ^ string_of_int (idx+ptn) ^ " " ^ matchtypes lx ^ ")")
      (DatatypesExt.caml_list flx.coq_F_locals)) ^ "\n")) ^
    (* function body *)
    String.concat "\n"
    (List.map
      (fun fx -> wasm_frontend_expr fx 2)
      (DatatypesExt.caml_list flx.coq_F_body)) ^ "\n" ^
    (* function end *)
    indent 1 ^ ")"
  in
  wasm_frontend_aux_fucs ^ "\n" ^
  (* main: multiplexer *)
  indent 1 ^ "(func $main (param ) (result )\n" ^ 
  wasm_frontend_multiplexer func_abi has_constructor tl is_runtime ^ "\n" ^ 
  indent 1 ^ ")" ^ "\n" ^ 

  (if (not has_constructor) then indent 1 ^ "(func $constructor (param ) (result) 
    nop
    )\n" else "") ^ 
  String.concat "\n"
  (List.mapi printflx fl)
  
let wasm_frontend_exports_debug (fl: Structure.func list) = 
  String.concat "\n"
  (List.mapi 
    (fun idx flx -> 
      if idx = 0 then "" else
      (indent 1) ^ "(export \"" ^ ("f" ^ string_of_int (idx+256)) ^ "\" (func " ^ ("$f" ^ string_of_int (idx+256)) ^ "))")
  fl)

(* SPEC: typeidx and funcidx 0-255 is reserved for EEI functions *)
let mnemonics_wasm (program: Structure.coq_module) (has_constructor: bool) (func_abi: Int.int list) (is_runtime: bool) = 
  (* if has_constructor then "has" else "no"  *)
  (* wasm_frontend_types (DatatypesExt.caml_list program.coq_M_types) *)

  "(module" ^ "\n" ^
  (String.concat "\n"
  [ 
    (wasm_frontend_types (DatatypesExt.caml_list program.coq_M_types)) (*^ "\n" ^ wasm_f131_types*) ;
    wasm_frontend_imports is_runtime;
    (* wasm_f131_table; *)
    wasm_frontend_mems;
    (*wasm_f131_data ^ "\n" ^*) wasm_frontend_data;
    (wasm_frontend_global) (*^ "\n" ^ wasm_f131_global*);
    wasm_frontend_exports;
    (* if is_debug then (wasm_frontend_exports_debug (DatatypesExt.caml_list program.coq_M_funcs)) else ""; *)
    wasm_frontend_funcs (DatatypesExt.caml_list program.coq_M_funcs) has_constructor func_abi (DatatypesExt.caml_list program.coq_M_types) is_runtime;
    (* (string_of_int (List.length (DatatypesExt.caml_list program.coq_M_funcs))); *)
  ])
  ^ "\n" ^
  ")"
