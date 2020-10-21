open AST
open Ascii
open BinNums
open Coqlib
open Datatypes
open Globalenvs
open Maps0
open Monad
open OptErrMonad
open Semantics
open Specif
open StmtClinear
open String0
open Trees

(** val enumerate_rest : Semantics.code -> node list **)

let enumerate_rest c =
  PTree.xkeys c Coq_xH

(** val labels_of_bblock : bblock -> label list **)

let rec labels_of_bblock = function
| Coq_nil -> Coq_nil
| Coq_cons (s, b') ->
  (match s with
   | Sjump n -> Coq_cons (n, (labels_of_bblock b'))
   | Sjumpi (_, n) -> Coq_cons (n, (labels_of_bblock b'))
   | _ -> labels_of_bblock b')

(** val enumerate'_func : (node list, Semantics.code) sigT -> node list **)

let rec enumerate'_func x =
  let todo = projT1 x in
  let c = projT2 x in
  let enumerate'0 = fun todo0 c0 -> enumerate'_func (Coq_existT (todo0, c0))
  in
  (match todo with
   | Coq_nil -> enumerate_rest c
   | Coq_cons (n, ns) ->
     let filtered_var = PTree.get n c in
     (match filtered_var with
      | Some b ->
        Coq_cons (n,
          (enumerate'0 (app (labels_of_bblock b) ns) (PTree.remove n c)))
      | None -> enumerate'0 ns c))

(** val enumerate' : node list -> Semantics.code -> node list **)

let enumerate' todo c =
  enumerate'_func (Coq_existT (todo, c))

(** val enumerate : Semantics.coq_function -> node list **)

let enumerate f =
  enumerate' (Coq_cons (f.fn_entrypoint, Coq_nil)) f.Semantics.fn_code

(** val starts_with : label -> code -> bool **)

let starts_with lbl = function
| Coq_nil -> Coq_false
| Coq_cons (s, _) ->
  (match s with
   | Slabel lbl' -> proj_sumbool (peq lbl lbl')
   | _ -> Coq_false)

(** val add_branch : label -> code -> code **)

let add_branch s k =
  match starts_with s k with
  | Coq_true -> k
  | Coq_false -> Coq_cons ((Sjump s), k)

(** val clinear_block : bblock -> code -> code **)

let rec clinear_block b k =
  match b with
  | Coq_nil -> k
  | Coq_cons (i, b') ->
    (match i with
     | Slabel _ -> clinear_block b' k
     | Sjump n -> add_branch n k
     | _ -> Coq_cons (i, (clinear_block b' k)))

(** val clinear_node : Semantics.coq_function -> node -> code -> code **)

let clinear_node f pc k =
  match PTree.get pc f.Semantics.fn_code with
  | Some b -> Coq_cons ((Slabel pc), (clinear_block b k))
  | None -> k

(** val clinear_body : Semantics.coq_function -> node list -> code **)

let clinear_body f enum =
  list_fold_right (clinear_node f) enum Coq_nil

(** val clinear_function : Semantics.coq_function -> coq_function optErr **)

let clinear_function f =
  let enum = enumerate f in
  ret (Obj.magic coq_Monad_optErr) { fn_return = f.Semantics.fn_return;
    fn_params = f.Semantics.fn_params; fn_temps = f.Semantics.fn_temps;
    fn_locals = f.Semantics.fn_locals; fn_code =
    (add_branch f.fn_entrypoint (clinear_body f enum)) }

(** val clinear_fundef : Semantics.coq_function -> coq_function optErr **)

let clinear_fundef =
  clinear_function

(** val clinear_fundefs :
    Semantics.coq_function PTree.t -> coq_function PTree.t optErr **)

let clinear_fundefs t0 =
  transl_tree clinear_fundef t0

(** val clinear_methoddefs :
    Semantics.coq_function option IntMap.t -> coq_function option IntMap.t
    optErr **)

let clinear_methoddefs methods =
  transl_map clinear_fundef methods

(** val clinear_constructor :
    Semantics.coq_function option -> coq_function optErr **)

let clinear_constructor = function
| Some c ->
  bind (Obj.magic coq_Monad_optErr) (clinear_fundef c) (fun f ->
    ret (Obj.magic coq_Monad_optErr) f)
| None ->
  Error (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false,
    Coq_false, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
    Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
    ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
    Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
    Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
    (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false,
    Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
    Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true,
    Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true,
    Coq_true, Coq_true, Coq_false, Coq_false, Coq_false, Coq_true,
    Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
    Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
    Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
    (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
    Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
    Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
    ((Ascii (Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
    Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
    Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
    (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true,
    Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
    Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
    (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true,
    Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
    Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
    ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
    Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
    Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
    (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false,
    Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
    Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
    (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
    Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
    Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
    (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
    Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
    Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
    ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
    Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
    Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
    (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
    Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
    Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
    Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
    Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
    Coq_false)),
    EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

(** val clinear_genv : Semantics.genv -> genv optErr **)

let clinear_genv ge =
  let vars = ge.Genv.genv_vars in
  let funcs = ge.Genv.genv_funcs in
  let methods = ge.Genv.genv_methods in
  let defs = ge.Genv.genv_defs in
  let fundefs = ge.Genv.genv_fundefs in
  let methoddefs = ge.Genv.genv_methoddefs in
  let constructor = ge.Genv.genv_constructor in
  bind (Obj.magic coq_Monad_optErr) (Obj.magic clinear_fundefs fundefs)
    (fun fundefs0 ->
    bind (Obj.magic coq_Monad_optErr)
      (Obj.magic clinear_methoddefs methoddefs) (fun methoddefs0 ->
      bind (Obj.magic coq_Monad_optErr)
        (Obj.magic clinear_constructor constructor) (fun constructor0 ->
        ret (Obj.magic coq_Monad_optErr) { Genv.genv_vars = vars;
          Genv.genv_funcs = funcs; Genv.genv_methods = methods;
          Genv.genv_defs = defs; Genv.genv_fundefs = fundefs0;
          Genv.genv_methoddefs = methoddefs0; Genv.genv_constructor = (Some
          constructor0) })))
