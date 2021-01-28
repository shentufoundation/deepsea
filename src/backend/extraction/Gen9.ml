open AST
open Ascii
open Cop
open Datatypes
open Globalenvs
open Integers
open Language
open List0
open LowValues
open Maps0
open Monad
open OptErrMonad
open Options
open Semantics3
open StackEnv
open StmtExpressionless
open String0
open Zpower

(** val methodical_fundefs : coq_function PTree.t -> code **)

let methodical_fundefs t0 =
  flat_map fn_code (map snd (PTree.elements t0))

(** val methodical_opt_function : coq_function option -> code **)

let methodical_opt_function = function
| Some f -> fn_code f
| None -> Coq_nil

(** val methodical_methods : coq_function option IntMap.t -> code **)

let methodical_methods methods =
  flat_map methodical_opt_function (map snd (PTree.elements (snd methods)))

(** val label_method_starts_with : coq_function -> label optErr **)

let label_method_starts_with m =
  match fn_code m with
  | Coq_nil ->
    Error (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true,
      Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
      Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
      Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_false,
      Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
      Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_true, Coq_true, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
      Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_true,
      Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
      Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
      Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
      Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
      Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_false, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
      Coq_true, Coq_true, Coq_false, Coq_true, Coq_false)), (String ((Ascii
      (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
      Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_false,
      Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
      Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_true, Coq_true, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
      Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_false,
      Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
      Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
      Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
      Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
      Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
      (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
      Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
      (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
      Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
      ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
      Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
      Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
      Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
      Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
      (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
      Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
      (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
      Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
      ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
      Coq_true, Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
      (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
      Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_false, Coq_true, Coq_false, Coq_false, Coq_false, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
      Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
      Coq_true, Coq_false, Coq_true, Coq_false, Coq_false)),
      EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
  | Coq_cons (s, _) ->
    (match s with
     | Slabel l -> ret (Obj.magic coq_Monad_optErr) l
     | _ ->
       Error (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true,
         Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
         (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
         Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
         Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
         (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
         Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
         (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
         Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
         Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_false)),
         (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_true, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
         Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
         Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
         Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
         Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_true,
         Coq_false, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
         Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
         Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
         Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
         Coq_true, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
         Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
         Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
         Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
         Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true,
         Coq_true, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
         Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
         Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
         Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
         Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
         Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_false,
         Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
         Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
         Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_false,
         Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
         Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_true, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
         Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_false,
         Coq_false)),
         EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

(** val sg_val : Int.int -> coq_val **)

let sg_val sg =
  Vint (Int256.repr (Int.unsigned sg))

(** val methodical_multiplexer_body :
    Int.int list -> coq_function option IntMap.t -> code optErr **)

let rec methodical_multiplexer_body methods methoddefs =
  match methods with
  | Coq_nil -> ret (Obj.magic coq_Monad_optErr) Coq_nil
  | Coq_cons (sg, rest) ->
    bind (Obj.magic coq_Monad_optErr)
      (methodical_multiplexer_body rest methoddefs) (fun rest' ->
      bind (Obj.magic coq_Monad_optErr)
        (fromOption (IntMap.get sg (Obj.magic methoddefs)) (String ((Ascii
          (Coq_false, Coq_true, Coq_true, Coq_false, Coq_false, Coq_true,
          Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
          Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
          (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true,
          Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
          (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
          Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
          Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
          (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false,
          Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
          (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
          Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
          Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
          (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
          Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
          Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
          Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
          Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
          ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
          Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
          Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
          Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
          Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
          ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
          Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true,
          Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
          Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
          Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
          ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
          Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
          Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
          Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
          Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
          ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_false,
          Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
          Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
          Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
          Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
          ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
          Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
          Coq_true, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
          Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
          Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
          ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
          Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
          Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
          Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
          Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
          ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
          Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
          Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
          Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
          Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
          EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
        (fun m ->
        bind (Obj.magic coq_Monad_optErr)
          (Obj.magic label_method_starts_with m) (fun l ->
          ret (Obj.magic coq_Monad_optErr) (Coq_cons ((Sdup O), (Coq_cons
            ((Spush (Coq_inl (sg_val sg))), (Coq_cons ((Sbinop (Oeq,
            Coq_false)), (Coq_cons ((Spush (Coq_inr l)), (Coq_cons (Sjumpi,
            rest')))))))))))))

(** val methodical_main : program -> code optErr **)

let methodical_main p =
  let ge = fst p in
  let body = snd p in
  let methods = ge.Genv.genv_methods in
  let fundefs = ge.Genv.genv_fundefs in
  let methoddefs = ge.Genv.genv_methoddefs in
  let constructor = ge.Genv.genv_constructor in
  bind (Obj.magic coq_Monad_optErr)
    (methodical_multiplexer_body methods methoddefs) (fun multiplexer_body ->
    ret (Obj.magic coq_Monad_optErr)
      (app (methodical_opt_function constructor) (Coq_cons ((Slabel body),
        (Coq_cons ((Spush (Coq_inl (Vint sb))), (Coq_cons ((Spush (Coq_inl
        (Vint sp))), (Coq_cons (Smstore, (Coq_cons ((Spush (Coq_inl (Vint
        (Int256.repr (two_power_nat sg_shift))))), (Coq_cons ((Spush z0),
        (Coq_cons (Scalldataload, (Coq_cons ((Sbinop (Odiv, Coq_false)),
        (app multiplexer_body (Coq_cons (Srevert,
          (app (methodical_methods methoddefs) (methodical_fundefs fundefs)))))))))))))))))))))))

(** val methodical_genv : program -> Language.genv optErr **)

let methodical_genv p =
  bind (Obj.magic coq_Monad_optErr) (Obj.magic methodical_main p)
    (fun main_code ->
    let ge = fst p in
    let body = snd p in
    let vars = ge.Genv.genv_vars in
    let defs = ge.Genv.genv_defs in
    (match label_verify main_code with
     | Coq_true ->
       Success { genv_vars = vars; genv_defs = defs; genv_main = main_code;
         genv_main_entrypoint = body }
     | Coq_false ->
       Error (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false,
         Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
         (Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
         Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
         Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
         (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
         Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
         Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
         Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
         ((Ascii (Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
         Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
         Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
         Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
         Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
         Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
         Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
         Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
         Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
         ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
         ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
         Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
         Coq_true, Coq_true, Coq_false, Coq_true, Coq_false)), (String
         ((Ascii (Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
         Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_false,
         Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_true, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
         Coq_true, Coq_true, Coq_true, Coq_true, Coq_false)),
         EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
