Require Import cclib.Coqlib.
Require Import backend.Ctypes.
Require Import cclib.Maps.
Require Import cclib.Integers.
Require Import backend.Values.LowValues.
Require Import backend.AST.

Section StackEnv.

Variable function : Type.
Variable fn_locals : function -> list (ident * type).

(* at a lower level, storage and memory are separate *)
Definition stack_env : Type := Int256Map.t LowValues.val.

(* Memory layout:
   0x00 - 0x1f          Return Values
   0x20 - 0x3f          Stack Pointer
   0x40 -               Stack *)

Definition sp := Int256.repr (2304 + 32).
Definition sb := Int256.repr (2304 + 64).

Notation "stk [ i ]" := (Int256Map.get i stk) (at level 80).
Notation "stk [ i |-> v ]" := (Int256Map.set i v stk) (at level 80).

(* The stack itself begins at 0x40 *)
Definition empty_stack : stack_env :=
  (Int256Map.init Vzero)[sp|->Vint sb].

Definition push_frame (size:int256) (stk:stack_env) : option stack_env :=
  match stk[sp] with
  | Vint p => Some (stk[sp |-> Vint (Int256.add p size)])
  | _ => None
  end.

Definition pop_frame (size:int256) (stk:stack_env) : option stack_env :=
  match stk[sp] with
  | Vint p => Some (stk[sp |-> Vint (Int256.sub p size)])
  | _ => None
  end.

Fixpoint sizeof (ty:type) : int256 :=
  Int256.repr (sizeof_words ty).

Fixpoint offset (fl:fieldlist) (id:ident) : option int256 :=
  match struct_field fl id with
  | Some (off,_) => Some (Int256.mul (Int256.repr 32) (Int256.repr (Z.of_nat off)))
  | None => None
  end.

Fixpoint frame_size (locs:list (ident * type)) : int256 :=
  match locs with
  | nil => Int256.repr 0
  | cons (_,ty) locs' => Int256.add (Int256.mul (Int256.repr 32) (sizeof ty)) (frame_size locs')
  end.

Definition push_func (f:function) (stk:stack_env) : option stack_env :=
  push_frame (frame_size (fn_locals f)) stk.

Definition pop_func (f:function) (stk:stack_env) : option stack_env :=
  pop_frame (frame_size (fn_locals f)) stk.

Definition read (ptr:val) (stk:stack_env) : option val :=
  match ptr,stk[sp] with
  | Vint p, Vint s =>
    if Int256.eq p sp then None
    else if Int256.lt p s then Some (stk[p])
         else None
  | _,_ => None
  end.

Definition write (ptr:val) (v:val) (stk:stack_env) : option stack_env :=
  match ptr,stk[sp] with
  | Vint p, Vint s =>
    if Int256.eq p sp then None
    else if Int256.lt p s then Some (stk[p |-> v])
         else None
  | _,_ => None
  end.


Fixpoint mkfieldlist (itl:list (ident*type)) : fieldlist :=
  match itl with
  | nil => Fnil
  | cons (i,t) itl' => Fcons i t (mkfieldlist itl')
  end.

Definition var (id:ident) (locals:list(ident*type)) (stk:stack_env) : option val :=
  match stk[sp] with
  | Vint s =>
    let base := Int256.sub s (frame_size locals) in
    let fl := mkfieldlist locals in
    option_map (fun off => Vint (Int256.add base off)) (offset fl id)
  | _ => None
  end.

Definition access_field (v:val) (ty:type) (i:ident) : option val :=
  match v,ty with
  | Vint base, Tstruct _ fl => option_map (fun off => (Vint (Int256.add base off))) (offset fl i)
  | _,_ => None
  end.

Definition index_array (v:val) (ty:type) (ind:val) : option val :=
  match v,ind,ty with
  | Vint base, Vint off, Tarray ty' size =>
    if Int256.lt off (Int256.repr size) then
      Some (Vint (Int256.add base (Int256.mul off (sizeof ty'))))
    else None
  | _, _, _ => None
  end.


End StackEnv.
