open AST
open Ascii
open BinInt
open BinNums
open Compiled
open Ctypes
open Datatypes
open Globalenvs
open Integers
open Language
open Language0
open List0
open Maps0
open Monad
open OptErrMonad
open Options
open StmCompile
open String0
open Structure
open Types

val allocate_addrs :
  ident list -> coq_Z -> coq_type PTree.t -> coq_Z PTree.t optErr

val allocations : Language.genv -> coq_Z PTree.t optErr

val genv_compiled : Language.genv -> compiled

val get_main_entrypoint : Language.genv -> label optErr

val extract_funcargtype : (ident, coq_type) prod list -> valtype list

val extract_functype : coq_function -> functype

val wasm_opt_funtype : coq_function option -> functype list

val wasm_to_func : coq_function -> nat -> func

type func_list_n_type = { fl : func list; ti : nat; mtypes : functype list }

val fl : func_list_n_type -> func list

val ti : func_list_n_type -> nat

val mtypes : func_list_n_type -> functype list

val wasm_opt_to_func : coq_function option -> nat -> func_list_n_type

val construct_wasm_opt_func_list :
  coq_function option list -> nat -> func_list_n_type

type method_rec = { flnt : func_list_n_type; mabi : Int.int list }

val flnt : method_rec -> func_list_n_type

val mabi : method_rec -> Int.int list

val wasm_multiplexer_body :
  Int.int list -> coq_function option IntMap.t -> nat -> method_rec

val module_funcs :
  genv -> ((func list, Int.int list) prod, functype list) prod

val module_mems : mem list

val module_imports : import list

val module_exports : export list

val genv_compiled_wasm :
  genv -> ((coq_module, bool) prod, Int.int list) prod optErr
