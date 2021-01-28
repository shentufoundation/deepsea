Require Import FunctionalExtensionality.

Require Import DeepSpec.lib.Monad.Monad.
Require Import DeepSpec.lib.Monad.MonadTrans.
Require Import DeepSpec.lib.Monad.MonadState.
Require Import DeepSpec.lib.Monad.MonadReader.
Require Import DeepSpec.lib.Monad.MonadZero.

Set Implicit Arguments.
Set Strict Implicit.

Section MonadLaws.
  Variable m : Type -> Type.
  Variable M : Monad m.

  Class MonadLaws : Type :=
    { bind_of_return : forall A B,
    forall (a:A) (f:A -> m B),
     (bind (ret a) f) = (f a)
  ; return_of_bind : forall A, 
    forall (aM:m A) (f:A -> m A),
    (forall x, (f x) = (ret x)) ->
    (bind aM f) = aM
  ; bind_associativity :
    forall A B C,
      forall (aM:m A) (f:A -> m B) (g:B -> m C),
      (bind (bind aM f) g) = (bind aM (fun a => bind (f a) g))
  }.

  (*
  Add Parametric Morphism T U  (ML : MonadLaws) : (@bind _ _ T U)
    with signature (eq ==> (eq ==> eq) ==> eq)
      as bind_morph.
  Proof. intros.
         red in H.
         firstorder.
    eapply bind_proper; auto. Qed.

  Add Parametric Morphism T U (tT : type T) (tU : type U) (tokT : typeOk tT) (tokU : typeOk tU) (ML : MonadLaws) (c : m T) (Pc : proper c) : (@bind _ _ T U c)
    with signature ((equal ==> equal) ==> equal)
      as bind_1_morph.
  Proof.
    eapply bind_proper; auto. eapply preflexive; [ | eapply Pc ].
    eapply equiv_prefl; auto.
  Qed.

  Add Parametric Morphism T (tT : type T) (tokT : typeOk tT) (ML : MonadLaws) : (@ret _ _ T)
    with signature (equal ==> equal)
      as ret_morph.
  Proof. eapply ret_proper; auto. Qed.

   *)
  
  Class MonadTLaws (n : Type -> Type)  (nM : Monad n) (MT : MonadT m n) : Type :=
  { lift_ret  : forall T,
    forall x : T,
    (lift (ret x)) = (ret x)
  ; lift_bind : forall T U,
    forall (c : n T) (f : T -> n U),
    (lift (bind c f)) = (bind (lift c) (fun x => lift (f x)))
  }.

  
  Section with_state.
    Context {S : Type}.

    Class MonadStateLaws  (MS : MonadState S m) : Type :=
    { get_put : (bind get put) = (ret tt)
    ; put_get : forall x,
      (bind (put x) (fun _ => get)) = (bind (put x) (fun _ => ret x))
    ; put_put : forall A,
      forall (x y : S) (f : unit -> m A), 
      (bind (put x) (fun _ => bind (put y) f)) =  (bind (put y) f)
    ; get_get : forall A,
      forall (f : S -> S -> m A),
      (bind get (fun s => bind get (f s))) = (bind get (fun s => f s s))
    ; get_ignore : forall A,
      forall (aM : m A), 
      (bind get (fun _ => aM)) = aM
    }.
    
    Class MonadReaderLaws (MR : MonadReader S m) : Type :=
    { ask_local : forall f,
      (local f ask) = (bind ask (fun x => ret (f x)))
    ; local_bind : forall A B,
        forall aM f (g : A -> m B), 
          (local f (bind aM g)) = (bind (local f aM) (fun x => local f (g x)))
    ; local_ret : forall A,
        forall (x : A) f,
          (local f (ret x)) = (ret x)
    ; local_local : forall T,
      forall (s s' : S -> S) (c : m T),
        (local s (local s' c)) = (local (fun x => s' (s x)) c)
    }.

  End with_state.

  Class MonadZeroLaws (MZ : MonadZero m) : Type :=
  { bind_zero : forall A B,
    forall (f : A -> m B),
    (bind mzero f) = mzero
  }.

End MonadLaws.



Hint Rewrite bind_of_return using assumption : monads.
Hint Rewrite bind_associativity using assumption : monads.
Hint Rewrite lift_ret using assumption : monads.
Hint Rewrite lift_bind using assumption : monads.
Hint Rewrite get_put using assumption : monads.
Hint Rewrite put_get using assumption : monads.
Hint Rewrite get_get using assumption : monads.
Hint Rewrite bind_zero using assumption : monads.

Ltac monad_simpl := 
repeat (autorewrite with monads;
try match goal with
  | |- context[@bind ?m ?mLaws ?A ?B ?a ?b] =>
    match b with
        | (fun x =>  _) =>
          let E := fresh "E" in
          let Heq := fresh "H" in
          let x := fresh "x" in
          evar(E : A -> m B);
          assert (b = E) by (unfold E; extensionality x; autorewrite with monads; reflexivity);
          unfold E in *;
          rewrite Heq; clear Heq
    end
end).
