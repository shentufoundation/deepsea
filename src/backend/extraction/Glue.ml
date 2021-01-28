open AST
open BinNums
open Datatypes
open EVM
open Gen3
open Gen1
open Gen6
open Gen5
open Gen
open Gen4
open Gen0
open Gen2
open Gen8
open Gen9
open Gen7
open Gen10
open GlobalenvCompile
open Integers
open Monad
open OptErrMonad
open StmtCGraph
open StmtMiniC
open Structure

(** val full_compile_genv_wasm :
    genv -> ((coq_module, bool) prod, Int.int list) prod optErr **)

let full_compile_genv_wasm ge =
  bind (Obj.magic coq_Monad_optErr) (Obj.magic clike_genv ge) (fun clike ->
    bind (Obj.magic coq_Monad_optErr) (Obj.magic clocal_genv clike)
      (fun clocal ->
      bind (Obj.magic coq_Monad_optErr) (Obj.magic cintptr_genv clocal)
        (fun cintptr ->
        bind (Obj.magic coq_Monad_optErr) (Obj.magic wasm_genv cintptr)
          (fun wasm_prog ->
          bind (Obj.magic coq_Monad_optErr) (genv_compiled_wasm wasm_prog)
            (fun program_n_c -> ret (Obj.magic coq_Monad_optErr) program_n_c)))))

(** val optm_genv :
    genv -> ((positive, StmtCGraph.statement) prod list, positive) prod list
    optErr **)

let optm_genv ge =
  bind (Obj.magic coq_Monad_optErr) (Obj.magic clike_genv ge) (fun clike ->
    bind (Obj.magic coq_Monad_optErr) (Obj.magic clocal_genv clike)
      (fun clocal ->
      bind (Obj.magic coq_Monad_optErr) (Obj.magic cintptr_genv clocal)
        (fun cintptr ->
        bind (Obj.magic coq_Monad_optErr) (Obj.magic cgraph_genv cintptr)
          cgraph_viz)))

(** val optm_clash :
    genv -> (positive, positive list) prod list list optErr **)

let optm_clash ge =
  bind (Obj.magic coq_Monad_optErr) (Obj.magic clike_genv ge) (fun clike ->
    bind (Obj.magic coq_Monad_optErr) (Obj.magic clocal_genv clike)
      (fun clocal ->
      bind (Obj.magic coq_Monad_optErr) (Obj.magic cintptr_genv clocal)
        (fun cintptr ->
        bind (Obj.magic coq_Monad_optErr) (Obj.magic cgraph_genv cintptr)
          clash_viz)))

(** val full_compile_genv : genv -> (evm list, label) prod optErr **)

let full_compile_genv ge =
  bind (Obj.magic coq_Monad_optErr) (Obj.magic clike_genv ge) (fun clike ->
    bind (Obj.magic coq_Monad_optErr) (Obj.magic clocal_genv clike)
      (fun clocal ->
      bind (Obj.magic coq_Monad_optErr) (Obj.magic cintptr_genv clocal)
        (fun cintptr ->
        bind (Obj.magic coq_Monad_optErr) (Obj.magic cgraph_genv cintptr)
          (fun cgraph ->
          bind (Obj.magic coq_Monad_optErr) (Obj.magic copt_genv cgraph)
            (fun copt ->
            bind (Obj.magic coq_Monad_optErr) (Obj.magic cbasic_genv copt)
              (fun cbasic ->
              bind (Obj.magic coq_Monad_optErr)
                (Obj.magic clinear_genv cbasic) (fun clinear ->
                bind (Obj.magic coq_Monad_optErr)
                  (Obj.magic clabeled_program clinear) (fun clabeled ->
                  bind (Obj.magic coq_Monad_optErr)
                    (Obj.magic stacked_program clabeled) (fun stacked ->
                    bind (Obj.magic coq_Monad_optErr)
                      (Obj.magic expressionless_program stacked)
                      (fun expressionless ->
                      bind (Obj.magic coq_Monad_optErr)
                        (Obj.magic methodical_genv expressionless)
                        (fun methodical ->
                        bind (Obj.magic coq_Monad_optErr)
                          (Obj.magic genv_compiled methodical)
                          (fun program ->
                          bind (Obj.magic coq_Monad_optErr)
                            (Obj.magic get_main_entrypoint methodical)
                            (fun main_entrypoint ->
                            ret (Obj.magic coq_Monad_optErr) (Coq_pair
                              (program, main_entrypoint)))))))))))))))
