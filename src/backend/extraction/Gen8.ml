open AST
open Ascii
open BinNums
open Datatypes
open ExpStacked
open Globalenvs
open Integers
open LowValues
open Maps0
open MemoryModel
open Monad
open OptErrMonad
open Semantics2
open StmtExpressionless
open StmtStacked
open String0
open Trees

(** val expressionless_expr : expr -> StmtExpressionless.statement **)

let expressionless_expr = function
| Econst_int256 i -> Spush (Coq_inl (Vint i))
| Eglob i -> Spush (Coq_inl (Vhash (Vint (Int256.repr (Zpos i)))))
| Etempvar n -> Sdup n
| Emload -> Smload
| Esload -> Ssload
| Eunop o -> Sunop o
| Ebinop (o, s) -> Sbinop (o, s)
| Ecall0 b -> Scall0 b
| Ecall1 b -> Scall1 b

(** val pops : nat -> StmtExpressionless.statement list **)

let rec pops = function
| O -> Coq_nil
| S m -> Coq_cons (StmtExpressionless.Spop, (pops m))

(** val cleanup : nat -> StmtExpressionless.statement list **)

let cleanup n = match n with
| O -> Coq_nil
| S m -> Coq_cons ((Sswap m), (pops n))

(** val expressionless_rt : ExpStacked.ret_type -> ret_type optErr **)

let expressionless_rt = function
| Tvoid_fun -> ret (Obj.magic coq_Monad_optErr) Tfun
| ExpStacked.Tvoid_method -> ret (Obj.magic coq_Monad_optErr) Tvoid_method
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
| ExpStacked.Tsome_method -> ret (Obj.magic coq_Monad_optErr) Tsome_method
| _ -> ret (Obj.magic coq_Monad_optErr) Tconstructor

(** val fetch_args :
    function_kind -> nat -> nat -> StmtExpressionless.statement list **)

let rec fetch_args fk count base =
  match count with
  | O -> Coq_nil
  | S n ->
    app (fetch_args fk n (S base))
      (match fk with
       | Coq_normalFunction ->
         Coq_cons ((Spush (Coq_inl (Vint (call_data_arg_location base)))),
           (Coq_cons (Scalldataload, Coq_nil)))
       | Coq_constructorFunction ->
         Coq_cons ((Sconstructordataload base), Coq_nil))

(** val extract_lbl : typed_label -> label **)

let extract_lbl = function
| Linternal l' -> l'
| Lcall l' -> l'
| Lreturn l' -> l'

(** val expressionless_stm :
    statement -> function_kind -> StmtExpressionless.statement list optErr **)

let expressionless_stm s kind =
  match s with
  | Sskip ->
    ret (Obj.magic coq_Monad_optErr) (Coq_cons (StmtExpressionless.Sskip,
      Coq_nil))
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
    ret (Obj.magic coq_Monad_optErr) (Coq_cons (StmtExpressionless.Spop,
      Coq_nil))
  | Ssassign -> ret (Obj.magic coq_Monad_optErr) (Coq_cons (Ssstore, Coq_nil))
  | Smassign -> ret (Obj.magic coq_Monad_optErr) (Coq_cons (Smstore, Coq_nil))
  | Sset n ->
    ret (Obj.magic coq_Monad_optErr) (Coq_cons ((Sswap n), (Coq_cons
      (StmtExpressionless.Spop, Coq_nil))))
  | Sdone (n, rt) ->
    bind (Obj.magic coq_Monad_optErr) (Obj.magic expressionless_rt rt)
      (fun rt' ->
      ret (Obj.magic coq_Monad_optErr)
        (app (cleanup n) (Coq_cons ((StmtExpressionless.Sdone rt'), Coq_nil))))
  | Spushlabel l ->
    ret (Obj.magic coq_Monad_optErr) (Coq_cons ((Spush (Coq_inr
      (extract_lbl l))), Coq_nil))
  | Slabel l ->
    ret (Obj.magic coq_Monad_optErr) (Coq_cons ((StmtExpressionless.Slabel
      l), Coq_nil))
  | Sjump_call ->
    (match kind with
     | Coq_normalFunction ->
       ret (Obj.magic coq_Monad_optErr) (Coq_cons (Sjump, Coq_nil))
     | Coq_constructorFunction ->
       Error (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii
         (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
         Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
         Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
         (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_false,
         Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_true,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
         Coq_true, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
         Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
         Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
         Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true,
         Coq_true, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
         Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
         Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
         ((Ascii (Coq_true, Coq_false, Coq_true, Coq_true, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
         Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
         Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
         Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
         Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_true,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
         Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
         Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
         Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
         Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_false,
         Coq_false)),
         EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
  | Sjump_internal ->
    ret (Obj.magic coq_Monad_optErr) (Coq_cons (Sjump, Coq_nil))
  | Sjumpi ->
    ret (Obj.magic coq_Monad_optErr) (Coq_cons (StmtExpressionless.Sjumpi,
      Coq_nil))
  | Shash ->
    ret (Obj.magic coq_Monad_optErr) (Coq_cons (StmtExpressionless.Shash,
      Coq_nil))
  | Stransfer ->
    ret (Obj.magic coq_Monad_optErr) (Coq_cons (StmtExpressionless.Stransfer,
      Coq_nil))
  | Scallargs (i, a, r) ->
    ret (Obj.magic coq_Monad_optErr) (Coq_cons ((StmtExpressionless.Scallargs
      (i, a, r)), Coq_nil))
  | Scallmethod b ->
    ret (Obj.magic coq_Monad_optErr) (Coq_cons
      ((StmtExpressionless.Scallmethod b), Coq_nil))
  | Slog (n, m) ->
    ret (Obj.magic coq_Monad_optErr) (Coq_cons ((StmtExpressionless.Slog (n,
      m)), Coq_nil))
  | Srevert ->
    ret (Obj.magic coq_Monad_optErr) (Coq_cons (StmtExpressionless.Srevert,
      Coq_nil))
  | Sfetchargs n ->
    ret (Obj.magic coq_Monad_optErr) (Coq_cons (StmtExpressionless.Sskip,
      (fetch_args kind n O)))

(** val expressionless_code :
    code -> function_kind -> StmtExpressionless.code optErr **)

let rec expressionless_code c kind =
  match c with
  | Coq_nil -> ret (Obj.magic coq_Monad_optErr) Coq_nil
  | Coq_cons (s, rest) ->
    bind (Obj.magic coq_Monad_optErr) (expressionless_stm s kind) (fun s' ->
      bind (Obj.magic coq_Monad_optErr) (expressionless_code rest kind)
        (fun rest' -> ret (Obj.magic coq_Monad_optErr) (app s' rest')))

(** val expressionless_function :
    function_kind -> coq_function -> StmtExpressionless.coq_function optErr **)

let expressionless_function kind f =
  bind (Obj.magic coq_Monad_optErr)
    (Obj.magic expressionless_code (fn_code f) kind) (fun c ->
    ret (Obj.magic coq_Monad_optErr) c)

(** val expressionless_fundefs :
    coq_function PTree.t -> StmtExpressionless.coq_function PTree.t optErr **)

let expressionless_fundefs t0 =
  transl_tree (expressionless_function Coq_normalFunction) t0

(** val expressionless_methods :
    coq_function option IntMap.t -> StmtExpressionless.coq_function option
    IntMap.t optErr **)

let expressionless_methods methods =
  transl_map (expressionless_function Coq_normalFunction) methods

(** val expressionless_constructor :
    coq_function option -> StmtExpressionless.coq_function optErr **)

let expressionless_constructor = function
| Some c ->
  bind (Obj.magic coq_Monad_optErr)
    (expressionless_function Coq_constructorFunction c) (fun f ->
    ret (Obj.magic coq_Monad_optErr) f)
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

(** val expressionless_genv : genv -> StmtExpressionless.genv optErr **)

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

(** val expressionless_program :
    program -> StmtExpressionless.program optErr **)

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
