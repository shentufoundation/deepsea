open AST
open Ascii
open Datatypes
open Globalenvs
open Language5
open Language0
open Maps0
open MemoryModel
open Monad
open OptErrMonad
open Semantics0
open String0
open Trees
open Values

(** val expressionless_expr : expr -> Language5.statement **)

let expressionless_expr = function
| Econst_int256 i -> Spush (Coq_inl (Vint i))
| Evar id -> Spush (Coq_inl (Vptr (Iident id)))
| Etempvar n -> Sdup n
| Ederef -> Ssload
| Eunop o -> Sunop o
| Ebinop (o, s) -> Sbinop (o, s)
| Ecall0 b -> Scall0 b
| Ecall1 b -> Scall1 b

(** val pops : nat -> Language5.statement list **)

let rec pops = function
| O -> Coq_nil
| S m -> Coq_cons (Language5.Spop, (pops m))

(** val cleanup : nat -> Language5.statement list **)

let cleanup n = match n with
| O -> Coq_nil
| S m -> Coq_cons ((Sswap m), (pops n))

(** val expressionless_rt : ret_type -> Language5.ret_type optErr **)

let expressionless_rt = function
| Tvoid_fun -> ret (Obj.magic coq_Monad_optErr) Tfun
| Tvoid_method -> ret (Obj.magic coq_Monad_optErr) Language5.Tvoid_method
| Terror ->
  Error (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false,
    Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
    Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false,
    Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
    Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_true,
    Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
    Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false,
    Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
    Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
    Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false,
    Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
    Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
    (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
    Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
    Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
    ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
    Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
    Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
    (Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
    Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
    Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
    (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
    Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
    Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
    Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false,
    Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
    Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
    Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false,
    Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
    Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false,
    Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
    Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false,
    Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
    Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
    (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
    Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
    Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
    ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
    Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
    Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
    (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
    Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
    Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
    Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
    Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
    (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
    Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
    Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
    (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
    Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
    Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
    ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
    Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
    Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
    (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
    Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
    Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
    (Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
    Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
    Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
    Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_false,
    Coq_false, Coq_true, Coq_false, Coq_true, Coq_false, Coq_true,
    Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
    Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
    Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false,
    Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
    Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
    (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
    Coq_true, Coq_true, Coq_false)),
    EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
| Tsome_fun -> ret (Obj.magic coq_Monad_optErr) Tfun
| Tsome_method -> ret (Obj.magic coq_Monad_optErr) Language5.Tsome_method
| _ -> ret (Obj.magic coq_Monad_optErr) Tconstructor

(** val fetch_args : nat -> nat -> Language5.statement list **)

let rec fetch_args count base =
  match count with
  | O -> Coq_nil
  | S n ->
    app (fetch_args n (S base)) (Coq_cons ((Spush (Coq_inl (Vint
      (call_data_arg_location base)))), (Coq_cons (Scalldataload, Coq_nil))))

(** val extract_lbl : typed_label -> label **)

let extract_lbl = function
| Linternal l' -> l'
| Lcall l' -> l'
| Lreturn l' -> l'

(** val fetch_constructor_args : nat -> nat -> Language5.statement list **)

let rec fetch_constructor_args count base =
  match count with
  | O -> Coq_nil
  | S n ->
    app (fetch_constructor_args n (S base)) (Coq_cons ((Spush (Coq_inl (Vint
      bytes_to_fetch))), (Coq_cons (TotalLength, (Coq_cons ((Spush (Coq_inl
      (Vint coq_MemoryLocation))), (Coq_cons (Scodecopy, (Coq_cons ((Spush
      (Coq_inl (Vint coq_MemoryLocation))), (Coq_cons (Smload,
      Coq_nil))))))))))))

(** val expressionless_stm : statement -> Language5.statement list optErr **)

let expressionless_stm = function
| Sskip ->
  ret (Obj.magic coq_Monad_optErr) (Coq_cons (Language5.Sskip, Coq_nil))
| Srvalue e ->
  ret (Obj.magic coq_Monad_optErr) (Coq_cons ((expressionless_expr e),
    Coq_nil))
| Slvalue e ->
  ret (Obj.magic coq_Monad_optErr) (Coq_cons ((expressionless_expr e),
    Coq_nil))
| Spushvoid ->
  ret (Obj.magic coq_Monad_optErr) (Coq_cons ((Spush (Coq_inl Vunit)),
    Coq_nil))
| Spop ->
  ret (Obj.magic coq_Monad_optErr) (Coq_cons (Language5.Spop, Coq_nil))
| Sassign -> ret (Obj.magic coq_Monad_optErr) (Coq_cons (Ssstore, Coq_nil))
| Sset n ->
  ret (Obj.magic coq_Monad_optErr) (Coq_cons ((Sswap n), (Coq_cons
    (Language5.Spop, Coq_nil))))
| Sdone (n, rt) ->
  bind (Obj.magic coq_Monad_optErr) (Obj.magic expressionless_rt rt)
    (fun rt' ->
    ret (Obj.magic coq_Monad_optErr)
      (app (cleanup n) (Coq_cons ((Language5.Sdone rt'), Coq_nil))))
| Spushlabel l ->
  ret (Obj.magic coq_Monad_optErr) (Coq_cons ((Spush (Coq_inr
    (extract_lbl l))), Coq_nil))
| Slabel l ->
  ret (Obj.magic coq_Monad_optErr) (Coq_cons ((Language5.Slabel l), Coq_nil))
| Sjump ->
  ret (Obj.magic coq_Monad_optErr) (Coq_cons (Language5.Sjump, Coq_nil))
| Sjumpi ->
  ret (Obj.magic coq_Monad_optErr) (Coq_cons (Language5.Sjumpi, Coq_nil))
| Stransfer ->
  ret (Obj.magic coq_Monad_optErr) (Coq_cons (Language5.Stransfer, Coq_nil))
| Scallmethod (i, a, r) ->
  ret (Obj.magic coq_Monad_optErr) (Coq_cons ((Language5.Scallmethod (i, a,
    r)), Coq_nil))
| Slog (n, m) ->
  ret (Obj.magic coq_Monad_optErr) (Coq_cons ((Language5.Slog (n, m)),
    Coq_nil))
| Srevert ->
  ret (Obj.magic coq_Monad_optErr) (Coq_cons (Language5.Srevert, Coq_nil))
| Sfetchargs n ->
  ret (Obj.magic coq_Monad_optErr) (Coq_cons (Language5.Sskip,
    (fetch_args n O)))

(** val expressionless_stm_constructor :
    statement -> Language5.statement list optErr **)

let expressionless_stm_constructor = function
| Sskip ->
  ret (Obj.magic coq_Monad_optErr) (Coq_cons (Language5.Sskip, Coq_nil))
| Srvalue e ->
  ret (Obj.magic coq_Monad_optErr) (Coq_cons ((expressionless_expr e),
    Coq_nil))
| Slvalue e ->
  ret (Obj.magic coq_Monad_optErr) (Coq_cons ((expressionless_expr e),
    Coq_nil))
| Spushvoid ->
  ret (Obj.magic coq_Monad_optErr) (Coq_cons ((Spush (Coq_inl Vunit)),
    Coq_nil))
| Spop ->
  ret (Obj.magic coq_Monad_optErr) (Coq_cons (Language5.Spop, Coq_nil))
| Sassign -> ret (Obj.magic coq_Monad_optErr) (Coq_cons (Ssstore, Coq_nil))
| Sset n ->
  ret (Obj.magic coq_Monad_optErr) (Coq_cons ((Sswap n), (Coq_cons
    (Language5.Spop, Coq_nil))))
| Sdone (n, rt) ->
  bind (Obj.magic coq_Monad_optErr) (Obj.magic expressionless_rt rt)
    (fun rt' ->
    ret (Obj.magic coq_Monad_optErr)
      (app (cleanup n) (Coq_cons ((Language5.Sdone rt'), Coq_nil))))
| Spushlabel l ->
  ret (Obj.magic coq_Monad_optErr) (Coq_cons ((Spush (Coq_inr
    (extract_lbl l))), Coq_nil))
| Slabel l ->
  ret (Obj.magic coq_Monad_optErr) (Coq_cons ((Language5.Slabel l), Coq_nil))
| Sjump ->
  ret (Obj.magic coq_Monad_optErr) (Coq_cons (Language5.Sjump, Coq_nil))
| Sjumpi ->
  ret (Obj.magic coq_Monad_optErr) (Coq_cons (Language5.Sjumpi, Coq_nil))
| Stransfer ->
  ret (Obj.magic coq_Monad_optErr) (Coq_cons (Language5.Stransfer, Coq_nil))
| Scallmethod (i, a, r) ->
  ret (Obj.magic coq_Monad_optErr) (Coq_cons ((Language5.Scallmethod (i, a,
    r)), Coq_nil))
| Slog (n, m) ->
  ret (Obj.magic coq_Monad_optErr) (Coq_cons ((Language5.Slog (n, m)),
    Coq_nil))
| Srevert ->
  ret (Obj.magic coq_Monad_optErr) (Coq_cons (Language5.Srevert, Coq_nil))
| Sfetchargs n ->
  ret (Obj.magic coq_Monad_optErr) (Coq_cons (Language5.Sskip,
    (fetch_constructor_args n O)))

(** val expressionless_code : code -> Language5.code optErr **)

let rec expressionless_code = function
| Coq_nil -> ret (Obj.magic coq_Monad_optErr) Coq_nil
| Coq_cons (s, rest) ->
  bind (Obj.magic coq_Monad_optErr) (expressionless_stm s) (fun s' ->
    bind (Obj.magic coq_Monad_optErr) (expressionless_code rest)
      (fun rest' -> ret (Obj.magic coq_Monad_optErr) (app s' rest')))

(** val expressionless_Constructor_code : code -> Language5.code optErr **)

let rec expressionless_Constructor_code = function
| Coq_nil -> ret (Obj.magic coq_Monad_optErr) Coq_nil
| Coq_cons (s, rest) ->
  bind (Obj.magic coq_Monad_optErr) (expressionless_stm_constructor s)
    (fun s' ->
    bind (Obj.magic coq_Monad_optErr) (expressionless_Constructor_code rest)
      (fun rest' -> ret (Obj.magic coq_Monad_optErr) (app s' rest')))

(** val expressionless_constructor_code : code -> Language5.code optErr **)

let rec expressionless_constructor_code = function
| Coq_nil -> ret (Obj.magic coq_Monad_optErr) Coq_nil
| Coq_cons (s, rest) ->
  bind (Obj.magic coq_Monad_optErr) (expressionless_stm_constructor s)
    (fun s' ->
    bind (Obj.magic coq_Monad_optErr) (expressionless_Constructor_code rest)
      (fun rest' -> ret (Obj.magic coq_Monad_optErr) (app s' rest')))

(** val expressionless_function :
    coq_function -> Language5.coq_function optErr **)

let expressionless_function f =
  bind (Obj.magic coq_Monad_optErr)
    (Obj.magic expressionless_code (fn_code f)) (fun c ->
    ret (Obj.magic coq_Monad_optErr) c)

(** val expressionless_constructor_function :
    coq_function -> Language5.coq_function optErr **)

let expressionless_constructor_function f =
  bind (Obj.magic coq_Monad_optErr)
    (Obj.magic expressionless_constructor_code (fn_code f)) (fun c ->
    ret (Obj.magic coq_Monad_optErr) c)

(** val expressionless_fundefs :
    coq_function PTree.t -> Language5.coq_function PTree.t optErr **)

let expressionless_fundefs t0 =
  transl_tree expressionless_function t0

(** val expressionless_methods :
    coq_function option IntMap.t -> Language5.coq_function option IntMap.t
    optErr **)

let expressionless_methods methods =
  transl_map expressionless_function methods

(** val expressionless_constructor :
    coq_function option -> Language5.coq_function optErr **)

let expressionless_constructor = function
| Some c ->
  bind (Obj.magic coq_Monad_optErr) (expressionless_constructor_function c)
    (fun f -> ret (Obj.magic coq_Monad_optErr) f)
| None ->
  Error (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false,
    Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
    Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
    ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
    Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
    Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
    (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false,
    Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
    Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
    Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
    Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true,
    Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
    Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
    Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true,
    Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
    Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
    Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
    Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true,
    Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
    Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
    Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
    Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
    Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
    Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
    Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
    Coq_false)), EmptyString))))))))))))))))))))))))))))))))))))))))))

(** val expressionless_genv : genv -> Language5.genv optErr **)

let expressionless_genv ge =
  let vars = ge.Genv.genv_vars in
  let funcs = ge.Genv.genv_funcs in
  let methods = ge.Genv.genv_methods in
  let defs = ge.Genv.genv_defs in
  let fundefs = ge.Genv.genv_fundefs in
  let methoddefs = ge.Genv.genv_methoddefs in
  let constructor = ge.Genv.genv_constructor in
  bind (Obj.magic coq_Monad_optErr)
    (Obj.magic expressionless_fundefs fundefs) (fun fundefs0 ->
    bind (Obj.magic coq_Monad_optErr)
      (Obj.magic expressionless_methods methoddefs) (fun methoddefs0 ->
      bind (Obj.magic coq_Monad_optErr)
        (Obj.magic expressionless_constructor constructor)
        (fun constructor0 ->
        ret (Obj.magic coq_Monad_optErr) { Genv.genv_vars = vars;
          Genv.genv_funcs = funcs; Genv.genv_methods = methods;
          Genv.genv_defs = defs; Genv.genv_fundefs = fundefs0;
          Genv.genv_methoddefs = methoddefs0; Genv.genv_constructor = (Some
          constructor0) })))

(** val expressionless_program : program -> Language5.program optErr **)

let expressionless_program = function
| Coq_pair (ge, body) ->
  bind (Obj.magic coq_Monad_optErr) (Obj.magic expressionless_genv ge)
    (fun cge ->
    match label_verify cge with
    | Coq_true -> ret (Obj.magic coq_Monad_optErr) (Coq_pair (cge, body))
    | Coq_false ->
      Error (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
        Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
        Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
        Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
        Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
        (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
        Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
        Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false,
        Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
        Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
        Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
        ((Ascii (Coq_false, Coq_true, Coq_true, Coq_false, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
        Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
        Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false,
        Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
        Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
        Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
        Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
        (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
        Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
        Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true,
        Coq_true, Coq_false, Coq_true, Coq_false)), (String ((Ascii
        (Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
        Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
        (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
        Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
        Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
        Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
        Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
        ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
        Coq_true, Coq_true, Coq_false)),
        EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
