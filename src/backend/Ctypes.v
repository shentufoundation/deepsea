(* These are the types we used, but the only ones we pay attention to are void, array, and struct. 
   Anything else is all treated the same. 

*)

(* *********************************************************************)
(*                                                                     *)
(*              The Compcert verified compiler                         *)
(*                                                                     *)
(*          Xavier Leroy, INRIA Paris-Rocquencourt                     *)
(*                                                                     *)
(*  Copyright Institut National de Recherche en Informatique et en     *)
(*  Automatique.  All rights reserved.  This file is distributed       *)
(*  under the terms of the GNU General Public License as published by  *)
(*  the Free Software Foundation, either version 2 of the License, or  *)
(*  (at your option) any later version.  This file is also distributed *)
(*  under the terms of the INRIA Non-Commercial License Agreement.     *)
(*                                                                     *)
(* *********************************************************************)

(** Type expressions for the Compcert C and Clight languages *)

Require Import cclib.Coqlib.
Require Import backend.AST.
Require Import cclib.Errors.
Require Import backend.Options.
(* Require Archi. *)

(** * Syntax of types *)

(** Compcert C types are similar to those of C.  They include numeric types,
  pointers, arrays, function types, and composite types (struct and 
  union).  Numeric types (integers and floats) fully specify the
  bit size of the type.  An integer type is a pair of a signed/unsigned
  flag and a bit size: 8, 16, or 32 bits, or the special [IBool] size
  standing for the C99 [_Bool] type.  64-bit integers are treated separately. *)

Inductive signedness : Type :=
  | Signed: signedness
  | Unsigned: signedness.

Inductive intsize : Type :=
  | I8: intsize
  | I16: intsize
  | I32: intsize
  | I256: intsize
  | IBool: intsize.

Inductive ptrkind : Type :=
| mem : ptrkind
| stor : ptrkind
| call : ptrkind.

Inductive type : Type :=
  | Tvoid: type                            (**r the [void] type *)
  | Tint: intsize -> signedness -> type    (**r integer types *)
  | Tpointer: ptrkind -> type -> type      (**r pointer types ([*ty]) *)
  | Tarray: type -> Z -> type              (**r array types ([ty[len]]) *)
  | Thashmap : type -> type -> type        (** key_type, elem_type *)
  | Tfunction: typelist -> type ->type     (**r function types *)
  | Tstruct: ident -> fieldlist -> type    (**r struct types *)
  | Tunion: ident -> fieldlist -> type     (**r union types *)
  | Tcomp_ptr: ident -> type               (**r pointer to named struct or union *)

with typelist : Type :=
  | Tnil: typelist
  | Tcons: type -> typelist -> typelist

with fieldlist : Type :=
  | Fnil: fieldlist
  | Fcons: ident -> type -> fieldlist -> fieldlist.

Lemma type_eq: forall (ty1 ty2: type), {ty1=ty2} + {ty1<>ty2}
with typelist_eq: forall (tyl1 tyl2: typelist), {tyl1=tyl2} + {tyl1<>tyl2}
with fieldlist_eq: forall (fld1 fld2: fieldlist), {fld1=fld2} + {fld1<>fld2}.
Proof.
  assert (forall (x y: intsize), {x=y} + {x<>y}) by decide equality.
  assert (forall (x y: signedness), {x=y} + {x<>y}) by decide equality.
  generalize ident_eq zeq bool_dec. intros E1 E2 E3.
  decide equality.
  decide equality.
  decide equality.
  generalize ident_eq. intros E1. decide equality.
Defined.

Opaque type_eq typelist_eq fieldlist_eq.

Definition type_int32s := Tint I32 Signed.
Definition type_int256u := Tint I256 Unsigned.
Definition type_bool := Tint IBool Signed.

(* Signedness. *)

Definition is_signed (t: type) : bool :=
  match t with
  | Tint _ Signed => true
  | _ => false
  end.

(* Concatenation and length for fieldlsit. *)

Fixpoint fieldlist_app (fld1 fld2: fieldlist) : fieldlist :=
  match fld1 with
  | Fnil => fld2
  | Fcons id ty fld => Fcons id ty (fieldlist_app fld fld2)
  end.

Fixpoint fieldlist_length (fld: fieldlist) : nat :=
  match fld with
  | Fnil => 0
  | Fcons id ty fld => S (fieldlist_length fld)
  end.

Lemma fieldlist_app_assoc fld1 fld2 fld3 :
  fieldlist_app fld1 (fieldlist_app fld2 fld3)
    = fieldlist_app (fieldlist_app fld1 fld2) fld3.
Proof.
  induction fld1; [ reflexivity | simpl ].
  f_equal; apply IHfld1.
Qed.

Lemma fieldlist_length_app fld1 fld2 :
  fieldlist_length (fieldlist_app fld1 fld2) =
       (fieldlist_length fld1 + fieldlist_length fld2)%nat.
Proof.
  induction fld1; auto.
  simpl. rewrite IHfld1. auto.
Qed.

(** Size of a type in words *)
(* This is added to the EVM backend, which counts words sometimes.
Actually this should never matter, because everything is hashed *)

Fixpoint sizeof_words (t: type) : Z :=
  match t with
  | Tarray t' n  => (sizeof_words t') * (Zmax 0 n)
  | Tstruct _ fld => sizeof_struct_words fld 0
  | _ => 1 (* most things are one word *)
  end

with sizeof_struct_words (fld: fieldlist) (pos: Z) {struct fld} : Z :=
  match fld with
  | Fnil => pos
  | Fcons _ t fld' => sizeof_struct_words fld' (pos + (sizeof_words t))
  end.

(* get offset and type *)
Fixpoint struct_field (fld: fieldlist) (id: ident) : option (nat * type) :=
  match fld with
  | Fnil => None
  | Fcons id' t fld' => if ident_eq id' id then Some (O, t) else
    unpack offset, t' <- struct_field fld' id ;;
      Some (S offset, t')
  end.
