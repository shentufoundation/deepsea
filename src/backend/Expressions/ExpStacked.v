
(* Intermediate language after Clabeled.
Merges a stackframe of local environments and return points
into a single stack.
In order to make return point well-defined,
we split function calls into pushing, jumping, and saving return value *)

Require Import cclib.Coqlib.
Require Import backend.AST.
Require Import backend.Environments.Globalenvs.
Require Import cclib.Integers.
Require Import backend.Ctypes.
Require Import backend.Values.LowValues.
Require Import backend.Options.
Require Import backend.Cop.
Require Import backend.MachineModel.
Require Import backend.MachineModelLow.

(* This is the first phase where we don't use MiniC expressions. Now
expressions is not a recursive datatype, and intermediate values are
instead stored on the stack.

Local env references are direct now, and expression values are on the
stack.  Expressions that take arguments will pop them off the stack *)
Inductive expr : Type :=
  | Econst_int256: int256 -> expr (**r 256-bit integer literal *)
  | Eglob: ident -> expr           (**r variable *)
  | Etempvar: nat -> expr       (**r temporary variable *)
  | Emload: expr          (**r memory pointer dereference (unary [*]) *)
  | Esload: expr          (**r storage pointer dereference (unary [*]) *)
  | Eunop: unary_operation -> expr  (**r unary operation *)
  (* all binary operations use the top of the stack as their first argument and
    second on the stack as their second argument.
    So when computing the first argument, the stack will have an
    unnamed value on top. *)
  | Ebinop: binary_operation -> bool -> expr (**r binary operation. bool is a signedness flag *)
  | Ecall0: builtin0  -> expr
  | Ecall1: builtin1 -> expr.

(* Describes return types of a function, because this affects how method/function calls are compiled: *)
Inductive ret_type : Type :=
  | Tvoid_fun: ret_type (* has to push dummy 0 value *)
  | Tvoid_method: ret_type (* actually returns nothing *)
  | Tvoid_constructor: ret_type (* RETURNs code *)
  | Terror: ret_type (* type cannot be returned, like array *)
  | Tsome_fun: ret_type (* returns something by pushing onto the stack *)
  | Tsome_method: ret_type (* returns with a RETURN command *)
  | Tsome_constructor: ret_type. (* pops value, RETURNs code *)

(* In earlier langauges we had instructions for calling functions. Now we want to compile those into jumps, so we have several types of labels (to enforce uniqueness).
   Eventually, these will again be unified into a single lable type. *)
Inductive typed_label : Type :=
  | Linternal : label -> typed_label (* internal label within the current function *)
  | Lcall: label -> typed_label (* function to call *)
  | Lreturn: label -> typed_label. (* label for returning to the inside of another function *)

(* expression arguments are popped off the stack *)
Inductive statement : Type :=
  | Sskip: statement (* no-op *)
  | Srvalue: expr -> statement (* pushes an expression as an rvalue *)
  | Slvalue : expr -> statement (* pushes an expression as an lvalue. compiled the same but semantically different. (TODO: there will need to be a future pass where these are the same, and pointers are turned into actual integers.) *)
  | Spushvoid : statement (* semantically, pushes a void value onto the stack, as a placeholder *)
  | Spop : statement (* remove a value from the stack *)
  | Sassign : statement
  | Sset : nat -> statement (* nat references a stack position, which must reference a temp in this function's local env. moves the thing on the stack to replace this temp. (So this sets a place in the stack, whereas in previous languages it would have been a temp identifier.) *)
    (* cleanup function and return value pushed on stack.
    first argument is amount of junk to pop off stack, second is about the return value. *)
  | Sdone : nat -> ret_type -> statement

  (* control flow *)
  | Spushlabel: typed_label -> statement
  | Slabel: label -> statement
  | Sjump_call : statement    (* unconditional jump to a different function *)
  | Sjump_internal: statement (* unconditional jump inside a function. *)
  | Sjumpi: statement (* cond, label on stack. jump if nonzero *)

  (* custom ethereum statements *)
  | Stransfer : statement (** address.transfer(value), with address & value from the stack.
    actually all operands to CALL are on the stack, but some have dummy values.  (hopefully, the transfer will eventually get compiled to a single call command.)
    pushes success value *)

  (* This no longer takes a list of expressions for arguments, but it still has the number of arguments.
     The way this should get compiled in later phases is to take right number of arguments from the stack, put them in memory, do the call, and then load the results from memory to the stack. *)                  
  | Scallmethod : int -> nat -> nat -> statement (* sig, number of args, number of retvals. from stack get addr, val, args *)
                                         
  | Slog : nat -> nat -> statement (* number of topics, number of args; both are from stack *)
  | Srevert : statement

  (* This fetches arguments from CALLDATA and puts them on the stack. *)                
  | Sfetchargs : nat -> statement (* how many args to push *)
  .

Definition code : Type := list statement.

Record function : Type := mkfunction {
  fn_code: code;
}.

Definition genv := Genv.t function type.
(* pair genv with label for start of transaction body *)
Definition program : Type := genv * label.

