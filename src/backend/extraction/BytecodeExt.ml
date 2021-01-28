open ASM
open Datatypes
open DatatypesExt
open Glue
open OptErrMonad

let exit_evm_fail msg =
  print_endline ("Internal error: compilation failed in EVM backend\n"
                 ^ "with error message: "
                 ^ caml_string msg);
  exit 1

let get_bytecode_params genv =
  match full_compile_genv genv with
  | Error msg -> exit_evm_fail msg
  | Success (Coq_pair (program, entrypoint)) ->
    let (asm, asm_runtime) = transform
        (List.rev (caml_list program))
        entrypoint in
    let programsize = size_of_program asm in
    asm, asm_runtime, programsize

let bytecode runtime genv =
  let asm, asm_runtime, programsize = get_bytecode_params genv in
  match runtime with
  | false -> assemble asm
  | true -> assemble asm_runtime

let assembly runtime genv =
  let asm , asm_runtime, _ = get_bytecode_params genv in
  match runtime with
  | false -> mnemonics asm
  | true -> mnemonics asm_runtime

let ewasm runtime genv =
  match Glue.full_compile_genv_wasm genv with
  | Error msg -> exit_evm_fail msg
  | Success md_n_hc ->
    let (mnd, fabi) = caml_prod md_n_hc in
    let (md, hc) = caml_prod mnd in
    mnemonics_wasm md
      (caml_bool hc)
      (caml_list fabi) runtime
