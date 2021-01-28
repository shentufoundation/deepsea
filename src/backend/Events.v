(** Observable events, execution traces, and semantics of external calls. *)

Require Import cclib.Coqlib.
Require Import backend.AST.
Require Import cclib.Integers.
Require Import backend.Values.HighValues.

(* TODO: make this a list of log entries, method calls *)
Inductive event : Type :=
  | Etransfer: forall (addr value : val), event
  | Ecallmethod: forall (addr : val) (signature: int) (value : val) (args: list val), event
  | Elog: forall (topics : list val) (args : list val), event.

Definition log := list event.

(* default log. *)
Definition L0 : log := nil.
