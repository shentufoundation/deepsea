Set Implicit Arguments.
Set Maximal Implicit Insertion.

Class Applicative T :=
{ pure : forall {A}, A -> T A
; ap : forall {A B}, T (A -> B) -> T A -> T B
}.

Module ApplicativeNotation.
  Notation "f <*> x" := (ap f x) (at level 51, right associativity).
End ApplicativeNotation.
Import ApplicativeNotation.

Section applicative.
  Context {T} {AT:Applicative T}.

  Definition liftA {A B} (f:A -> B) (aT:T A) : T B := pure f <*> aT.
  Definition liftA2 {A B C} (f:A -> B -> C) (aT:T A) (bT:T B) : T C := liftA f aT <*> bT.
End applicative.