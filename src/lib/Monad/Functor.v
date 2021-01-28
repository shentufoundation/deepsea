Set Implicit Arguments.
Set Strict Implicit.

Section functor.
  Variable F : Type -> Type.

  Class Functor : Type :=
  { fmap : forall A B, (A -> B) -> F A -> F B }.

  Definition ID {T : Type} (f : T -> T) : Prop :=
    forall x, f x = x.
End functor.

Module FunctorNotation.
  Notation "f <$> x" := (@fmap _ _ f x) (at level 51, right associativity).
End FunctorNotation.
