Require Import DeepSpec.lib.Monad.Monad.

Set Implicit Arguments.
Set Maximal Implicit Arguments.

Class MonadT (m : Type -> Type) (mt : Type -> Type) : Type :=
{ lift : forall {t}, mt t -> m t }.

