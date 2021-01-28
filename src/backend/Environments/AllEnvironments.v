Require Import cclib.Coqlib.
Require Import backend.Ctypes.
Require Import cclib.Maps.
Require Import cclib.Integers.
Require Import backend.Values.LowValues.
Require Import backend.Values.HighValues.

(** These should be divided up into their own files. *)

(* temp_env represents a local enviroment. It contains arguments and temps for a function. *)

(* temps all have value 0 until initialized, except args *)
Definition temp_env : Type := PTree.t val.

Definition temp_env_low : Type := PTree.t LowValues.val.

(* stores contents of evm stack, which is is reality a list of values
but may conceptually be function args, temps during execution, or return values. 

The operational semantics use this (only a single stack_data, since the indidual step rules only case about the top part of the stack).
The highest language just uses Temps; then further compilation phases introduce Args, Retval etc. 
*)
Inductive stack_data: Type :=
| Temps: temp_env -> stack_data
| Args: list val -> stack_data     (* When you have just called a function but not yet allocated temps. *)

(* A function has n arguments and m temp variables. 
   The calling function pushes arguments. The function prelude pushes m zeros for the temps.
   Then semantically, the rest of the function body is executed in an temp_env which has (n+m) entries.
   There is an extra complication that if there are two function arguments with the same name, the first one will be shadowed and not put in the temp_env.   
 *)
                                                
| MethodArgs: list val -> stack_data
| Retval: val -> stack_data.

Inductive stack_data_low : Type :=
| TempsL: temp_env_low -> stack_data_low
| ArgsL: list LowValues.val -> stack_data_low
| MethodArgsL: list LowValues.val -> stack_data_low
| RetvalL: LowValues.val -> stack_data_low
.
