open AST
open BinNums
open Datatypes
open EVM
open Gen2
open Gen1
open Gen5
open Gen4
open Gen
open Gen3
open Gen0
open Gen7
open Gen8
open Gen6
open Gen9
open GlobalenvCompile
open Integers
open Monad
open OptErrMonad
open StmtCGraph
open StmtMiniC
open Structure

val full_compile_genv_wasm :
  genv -> ((coq_module, bool) prod, Int.int list) prod optErr

val optm_genv :
  genv -> ((positive, StmtCGraph.statement) prod list, positive) prod list
  optErr

val optm_clash : genv -> (positive, positive list) prod list list optErr

val full_compile_genv : genv -> (evm list, label) prod optErr
