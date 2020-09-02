open AST
open Datatypes
open EVM
open Gen1
open Gen0
open Gen3
open Gen
open Gen2
open Gen5
open Gen6
open Gen4
open Gen7
open GlobalenvCompile
open Integers
open Language
open Monad
open OptErrMonad
open Structure

val full_compile_genv_wasm :
  genv -> ((coq_module, bool) prod, Int.int list) prod optErr

val full_compile_genv : genv -> (evm list, label) prod optErr
