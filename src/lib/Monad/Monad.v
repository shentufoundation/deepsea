Require Import DeepSpec.lib.Monad.Functor.
Require Import DeepSpec.lib.Monad.Applicative.

Set Implicit Arguments.
Set Strict Implicit.

Class Monad (m : Type -> Type) : Type :=
{ ret : forall {t}, t -> m t
; bind : forall {t u}, m t -> (t -> m u) -> m u
}.

Section monadic.
  Variable m : Type -> Type.
  Context {M : Monad m}.

  Definition liftM T U (f : T -> U) : m T -> m U :=
    fun x => bind x (fun x => ret (f x)).

  Definition liftM2 T U V (f : T -> U -> V) : m T -> m U -> m V :=
    Eval cbv beta iota zeta delta [ liftM ] in
      fun x y => bind x (fun x => liftM (f x) y).

  Definition liftM3 T U V W (f : T -> U -> V -> W) : m T -> m U -> m V -> m W :=
    Eval cbv beta iota zeta delta [ liftM2 ] in
      fun x y z => bind x (fun x => liftM2 (f x) y z).

  Definition apM {A B} (fM:m (A -> B)) (aM:m A) : m B :=
    bind fM (fun f => liftM f aM).

  Definition bind2 {A B C : Type} (ma : m (A * B)) (f : A -> B -> m C) : m C :=
    bind ma (fun a => match a with | (x, y) => f x y end).

End monadic.

Arguments bind2 {m M A B C} ma f.

Module MonadNotation.

  Delimit Scope monad_scope with monad.

  Notation "c >>= f" := (@bind _ _ _ _ c f) (at level 50, left associativity) : monad_scope.
  Notation "f =<< c" := (@bind _ _ _ _ c f) (at level 51, right associativity) : monad_scope.

  Notation "x <- c1 ;; c2" := (@bind _ _ _ _  c1 (fun x => c2))
    (at level 100, c1 at next level, right associativity) : monad_scope.

  Notation "e1 ;; e2" := (_ <- e1%monad ;; e2%monad)%monad
    (at level 100, right associativity) : monad_scope.

  Notation "'unpack' A , B <- y ;; z" := (bind2 y (fun A => (fun B => z)))
    (at level 100, A ident, B ident, y at next level, only parsing, right associativity) : monad_scope.

End MonadNotation.

Instance Functor_Monad {m} {M:Monad m} : Functor m :=
  { fmap := @liftM _ _ }.

Instance Applicative_Monad {m} {M:Monad m} : Applicative m :=
{ pure := @ret _ _
; ap := @apM _ _
}.
