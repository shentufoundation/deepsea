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

(** val allocate_addrs :
    ident list -> coq_Z -> coq_type PTree.t -> coq_Z PTree.t optErr **)

let rec allocate_addrs vars next_addr defs =
  match vars with
  | Coq_nil -> ret (Obj.magic coq_Monad_optErr) PTree.empty
  | Coq_cons (id, rest) ->
    bind (Obj.magic coq_Monad_optErr)
      (fromOption (PTree.get id (Obj.magic defs)) (String ((Ascii (Coq_true,
        Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
        Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
        ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
        Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
        Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
        Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_false,
        Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
        Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
        Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
        ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
        Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_false,
        Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true,
        Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
        (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_false, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
        Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_true,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
        Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
        EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))
      (fun ty ->
      let size = sizeof_words ty in
      bind (Obj.magic coq_Monad_optErr)
        (allocate_addrs rest (Z.add next_addr size) defs) (fun allocated ->
        ret (Obj.magic coq_Monad_optErr) (PTree.set id next_addr allocated)))

(** val allocations : Language.genv -> coq_Z PTree.t optErr **)

let allocations ge =
  allocate_addrs ge.genv_vars Z0 ge.genv_defs

(** val genv_compiled : Language.genv -> compiled **)

let genv_compiled ge =
  bind (Obj.magic coq_Monad_optErr) (Obj.magic allocations ge)
    (fun allocated ->
    code_compiled ge.genv_main allocated ge.genv_main_entrypoint)

(** val get_main_entrypoint : Language.genv -> label optErr **)

let get_main_entrypoint ge =
  ret (Obj.magic coq_Monad_optErr) ge.genv_main_entrypoint

(** val extract_funcargtype : (ident, coq_type) prod list -> valtype list **)

let rec extract_funcargtype = function
| Coq_nil -> Coq_nil
| Coq_cons (_, xs) -> app (Coq_cons (T_i32, Coq_nil)) (extract_funcargtype xs)

(** val extract_functype : coq_function -> functype **)

let extract_functype f =
  let frettype =
    match f.fn_return with
    | Tvoid -> Coq_cons (T_i32, Coq_nil)
    | _ -> Coq_cons (T_i32, Coq_nil)
  in
  let fargtype = extract_funcargtype f.fn_params in FT (fargtype, frettype)

(** val wasm_opt_funtype : coq_function option -> functype list **)

let wasm_opt_funtype = function
| Some f -> Coq_cons ((extract_functype f), Coq_nil)
| None -> Coq_nil

(** val wasm_to_func : coq_function -> nat -> func **)

let wasm_to_func f type_ind =
  { coq_F_type = type_ind; coq_F_locals = (extract_funcargtype f.fn_temps);
    coq_F_body = f.fn_body }

type func_list_n_type = { fl : func list; ti : nat; mtypes : functype list }

(** val fl : func_list_n_type -> func list **)

let fl x = x.fl

(** val ti : func_list_n_type -> nat **)

let ti x = x.ti

(** val mtypes : func_list_n_type -> functype list **)

let mtypes x = x.mtypes

(** val wasm_opt_to_func : coq_function option -> nat -> func_list_n_type **)

let wasm_opt_to_func f type_ind =
  match f with
  | Some fx ->
    { fl = (Coq_cons ((wasm_to_func fx type_ind), Coq_nil)); ti = (S
      type_ind); mtypes = (wasm_opt_funtype f) }
  | None -> { fl = Coq_nil; ti = type_ind; mtypes = Coq_nil }

(** val construct_wasm_opt_func_list :
    coq_function option list -> nat -> func_list_n_type **)

let rec construct_wasm_opt_func_list l type_ind =
  match l with
  | Coq_nil -> { fl = Coq_nil; ti = type_ind; mtypes = Coq_nil }
  | Coq_cons (x, xs) ->
    let tfunc = wasm_opt_to_func x type_ind in
    let rst = construct_wasm_opt_func_list xs tfunc.ti in
    { fl = (app tfunc.fl rst.fl); ti = rst.ti; mtypes =
    (app tfunc.mtypes rst.mtypes) }

type method_rec = { flnt : func_list_n_type; mabi : Int.int list }

(** val flnt : method_rec -> func_list_n_type **)

let flnt x = x.flnt

(** val mabi : method_rec -> Int.int list **)

let mabi x = x.mabi

(** val wasm_multiplexer_body :
    Int.int list -> coq_function option IntMap.t -> nat -> method_rec **)

let rec wasm_multiplexer_body methods methoddefs type_ind =
  match methods with
  | Coq_nil ->
    { flnt = { fl = Coq_nil; ti = type_ind; mtypes = Coq_nil }; mabi =
      Coq_nil }
  | Coq_cons (sg, rest) ->
    let m =
      fromOption (IntMap.get sg methoddefs) (String ((Ascii (Coq_false,
        Coq_true, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
        ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
        Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
        ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
        Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
        Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
        (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_false, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
        Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
        (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
        Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
        Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
        (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false,
        Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
        Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
        Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_false)),
        EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
    in
    let nm = match m with
             | Success mm -> Some mm
             | Error _ -> None in
    let tfunc = wasm_opt_to_func nm type_ind in
    let rst = wasm_multiplexer_body rest methoddefs tfunc.ti in
    { flnt = { fl = (app tfunc.fl rst.flnt.fl); ti = rst.flnt.ti; mtypes =
    (app tfunc.mtypes rst.flnt.mtypes) }; mabi =
    (app (Coq_cons (sg, Coq_nil)) rst.mabi) }

(** val module_funcs :
    genv -> ((func list, Int.int list) prod, functype list) prod **)

let module_funcs ge =
  let methods = ge.Genv.genv_methods in
  let fundefs = ge.Genv.genv_fundefs in
  let methoddefs = ge.Genv.genv_methoddefs in
  let constructor = ge.Genv.genv_constructor in
  let constructor_func = wasm_opt_to_func constructor O in
  let constructor_type = wasm_opt_funtype constructor in
  let multiplexer =
    wasm_multiplexer_body methods methoddefs constructor_func.ti
  in
  let func_func =
    construct_wasm_opt_func_list
      (map (fun x -> Some x) (map snd (PTree.elements fundefs)))
      multiplexer.flnt.ti
  in
  let fd = app constructor_func.fl (app multiplexer.flnt.fl func_func.fl) in
  let ft = app constructor_type (app multiplexer.flnt.mtypes func_func.mtypes)
  in
  Coq_pair ((Coq_pair (fd, multiplexer.mabi)), ft)

(** val module_mems : mem list **)

let module_mems =
  Coq_nil

(** val module_imports : import list **)

let module_imports =
  Coq_nil

(** val module_exports : export list **)

let module_exports =
  Coq_nil

(** val genv_compiled_wasm :
    genv -> ((coq_module, bool) prod, Int.int list) prod optErr **)

let genv_compiled_wasm ge =
  let Coq_pair (p, functypes) = module_funcs ge in
  let Coq_pair (funcs, abi) = p in
  let m = { coq_M_types = functypes; coq_M_funcs = funcs; coq_M_tables =
    Coq_nil; coq_M_mems = module_mems; coq_M_globals = Coq_nil; coq_M_elem =
    Coq_nil; coq_M_data = Coq_nil; coq_M_start = None; coq_M_imports =
    module_imports; coq_M_exports = module_exports }
  in
  let has_constructor =
    match ge.Genv.genv_constructor with
    | Some _ -> Coq_true
    | None -> Coq_false
  in
  ret (Obj.magic coq_Monad_optErr) (Coq_pair ((Coq_pair (m,
    has_constructor)), abi))
