(* 
Many things can fail during a program run, so a lot of helper functions used to define the semantics of the various intermediate languages return options.

Also, many things can fail in compilation, due to failed assumptions, so most of the compilation functions return optErr (i.e. an option or an error message).

Both of these are monad, and this file defines suitable monad instances and tactics that work the same way on both.
*)

Require Export DeepSpec.lib.Monad.Monad.
Require Export DeepSpec.lib.Monad.OptionMonad.
Require  DeepSpec.lib.Monad.OptionMonadLaws.
Require Export DeepSpec.lib.Monad.OptErrMonad.
Require  DeepSpec.lib.Monad.OptErrMonadLaws.
Require Export DeepSpec.lib.Monad.MonadInv.

Require Coq.Strings.String.
Export String.StringSyntax.

(* H is where to bind. a is new var and H1 is new hypothesis for value of a *)
Ltac BindSome H a H1 :=
  match type of H with
  | @eq ?T _ _ =>
    match (eval compute in T) with
    | option _ => apply (@bind_some _ _ OptionMonadLaws.MonadLaws_option MonadInvBind_option) in H; destruct H as (a & H1 & H)
    | optErr _ => apply (@bind_some _ _ OptErrMonadLaws.MonadLaws_optErr MonadInvBind_optErr) in H; destruct H as (a & H1 & H)
    end
end.
  
(* call on a hypothesis of the form Some expr = Some id *)
Ltac SomeSome H id := injection H; clear H; intro; subst id.

Definition fromOption {A:Type} (ov : option A) (msg : String.string) :=
  match ov with
  | Some v => Success v
  | None => Error msg
  end.

Lemma fromOption_Success : forall A (ov : option A) msg v,
    fromOption ov msg = Success v ->
    ov = Some v.
Proof.
  intros.
  destruct ov; simpl in *; congruence.
Qed.  

Export MonadNotation.

Definition optional_filter (A: Type) (a: option A) : list A :=
  match a with None => nil | Some a' => a' :: nil end.
  

