Require Import DeepSpec.lib.Monad.Monad.

Set Implicit Arguments.
Set Maximal Implicit Arguments.

Class MonadState (T : Type) (m : Type -> Type) : Type :=
{ get : m T
; put : T -> m unit
}.

Section monadic.
  Variable m : Type -> Type.
  Context {M : Monad m}.
  Variable T : Type.
  Context {MS : MonadState T m}.

  Definition modify (f : T -> T) : m unit :=
    bind get (fun x => put (f x)).
  Definition gets {U} (f : T -> U) : m U :=
    bind get (fun x => ret (f x)).

End monadic.

Section SubState.
  Variable m : Type -> Type.
  Context {M : Monad m}.
  Variable T S : Type.
  Context {MS : MonadState T m}.

  Definition StateProd (f : T -> S) (g : S -> T -> T) 
    : MonadState S m :=
  {| get := @gets m M T MS S f
   ; put := fun x => bind get (fun s => put (g x s))
   |}.
End SubState.
