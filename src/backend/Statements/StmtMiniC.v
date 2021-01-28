Require Import cclib.Coqlib.
Require Import cclib.Errors.
Require Import cclib.Maps.
Require Import cclib.Integers.
Require Import backend.AST.
Require Import backend.Expressions.ExpMiniC.
Require Import backend.Environments.Globalenvs.
Require Import backend.Ctypes.
Require Import backend.Cop. 
Require Import backend.MachineModel.

Inductive statement : Type :=
  | Sskip : statement                   (**r do nothing *)
  | Sassign : expr -> expr -> statement (**r assignment to evm storage.  [lvalue = rvalue], so the left side is computed as pointer.  *)
  | Sset : ident -> expr -> statement   (**r assignment to local env, i.e. the EVM stack. [tempvar = rvalue] *)
  (* Clight's Scall does a full internal function call. (as opposed to a method call) 
    Clike splits this into the simpler tasks of pushing arguments and saving the return value *)
  | Scall: option ident (* where the return value goes *) -> label (*name of function to call. *) -> list expr -> statement (**r function call *)
  | Ssequence : statement -> statement -> statement  (**r sequence *)
  | Sifthenelse : expr  -> statement -> statement -> statement (**r conditional *)
  | Sloop: statement -> statement (**r infinite loop *)
  | Sbreak : statement                      (**r [break] statement *)
  | Sreturn : option ident -> statement      (**r [return] statement with var for return *)

  (* custom ethereum statements *)
  | Stransfer : expr -> expr -> statement (** address.transfer(value) *)
  (* call public external function on address, identified by an int, given a value (the amount of money you send), with multiple return values and arguments *)
  | Scallmethod : expr (*address*) -> list ident (* return values *) -> int (* method signature, a hash of function name and arguments*) -> expr (* value*) -> option expr (* gas *) -> list expr (* args*) -> statement
  | Slog : list expr -> list expr -> statement (** LOG0-6. The first argument is a list of "topics" (at most 6), the second is a list of non-indexed arguments. *)
  | Srevert : statement
  .

(** The C loops are derived forms. *)

Definition Swhile (e: expr) (s: statement) :=
  Sloop (Sifthenelse e s Sbreak).

Definition Sdowhile (s: statement) (e: expr) :=
  Sloop (Ssequence s (Sifthenelse e Sskip Sbreak)).

(* for (s1; e; s4) s3; *)
Definition Sfor (s1: statement) (e2: expr) (s3: statement) (s4: statement) :=
  Ssequence s1 (Sloop (Sifthenelse e2 (Ssequence s3 s4) Sbreak)).

(** ** Functions *)

(** A function definition is composed of its return type ([fn_return]),
  the names and types of its parameters ([fn_params]), the names
  and types of its temps ([fn_temps]), and the body of the
  function (a statement, [fn_body]). *)

Record function : Type := mkfunction {
  fn_return: type;
  fn_params: list (ident * type);
  fn_temps: list (ident * type);
  fn_locals: list (ident * type);
  fn_body: statement
}.

Definition var_names (vars: list(ident * type)) : list ident :=
  List.map (@fst ident type) vars.

(** The semantics uses two environments.  The global environment
  maps names of functions and global variables to memory block references,
  and function pointers to their definitions.  (See module [Globalenvs].) *)

Definition genv := Genv.t function type.

(* This function is for convenience when writing tests. *)
Definition new_genv := Genv.new_genv function type.
