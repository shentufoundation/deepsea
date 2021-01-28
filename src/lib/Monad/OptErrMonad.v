(* This is basically the same thing as in the file EitherMonad.v, 
   but I wanted to use more explicit constructor names to keep things simple. *)

Require Import DeepSpec.lib.Monad.Monad.
Require Import DeepSpec.lib.Monad.MonadExc.
Require Import DeepSpec.lib.Monad.MonadInv.
Require Import Coq.Strings.String.


Inductive optErr (A:Type) :=
| Success : A -> optErr A
| Error : string -> optErr A.

Arguments Success {A} a.
Arguments Error {A} msg.

Set Implicit Arguments.
Set Strict Implicit.

Import MonadNotation.
Open Scope monad_scope.

Global Instance Monad_optErr : Monad optErr :=
  { ret  := fun _ v => Success v
  ; bind := fun _ _ c1 c2 => match c1 with
                               | Error msg => Error msg
                               | Success v => c2 v
                             end
  }.

Global Instance Exception_optErr : MonadExc string optErr :=
  { raise := fun _ v => Error v
  ; catch := fun _ c h => match c with
                            | Error v => h v
                            | x => x
                          end
  }.

Global Instance MonadInvBind_optErr: MonadInvBind Monad_optErr.
Proof.
  unfold MonadInvBind. intros.
  destruct ma.
  - exists a. reflexivity.
  - discriminate H.
Defined.

Global Instance MonadInvRet_optErr: MonadInvRet Monad_optErr.
Proof.
  unfold MonadInvRet. simpl. intros.
  injection H; auto.
Defined.
