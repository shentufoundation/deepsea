Require Import Monad.

Set Implicit Arguments.
Set Maximal Implicit Arguments.

Class MonadExc E (m : Type -> Type) : Type :=
{ raise : forall {T}, E -> m T
; catch : forall {T}, m T -> (E -> m T) -> m T
}.
