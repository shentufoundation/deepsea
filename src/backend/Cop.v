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

(** Arithmetic and logical operators *)


Require Import cclib.Coqlib.
Require Import backend.AST.
Require Import cclib.Integers.
(* Require Import Floats. *)
Require Import backend.Values.LowValues.
Require Import backend.Values.HighValues.
(*Require Import Memory. *)
Require Import backend.Ctypes.
Require Import SymbolicKeccak.

(** * Syntax of operators. *)

Inductive unary_operation : Type :=
  | Onotbool : unary_operation          (**r boolean negation ([!] in C) *)
  | Onotint : unary_operation           (**r integer complement ([~] in C) *)
  | Oneg : unary_operation              (**r opposite (unary [-]) *)
  | Osha_1 : unary_operation.           (**r Keccak-256 hash. *)

Inductive binary_operation : Type :=
  | Oadd : binary_operation             (**r addition (binary [+]) *)
  | Osub : binary_operation             (**r subtraction (binary [-]) *)
  | Omul : binary_operation             (**r multiplication (binary [*]) *)
  | Odiv : binary_operation             (**r division ([/]) *)
  | Omod : binary_operation             (**r remainder ([%]) *)
  | Oexp : binary_operation             (**r exponentiation ([**]) *)

  | Oand : binary_operation             (**r bitwise and ([&]) *)
  | Oor : binary_operation              (**r bitwise or ([|]) *)
  | Oxor : binary_operation             (**r bitwise xor ([^]) *)
  | Oshl : binary_operation             (**r left shift ([<<]) *)
  | Oshr : binary_operation             (**r right shift ([>>]) *)
  | Oeq: binary_operation               (**r comparison ([==]) *)
  | One: binary_operation               (**r comparison ([!=]) *)
  | Olt: binary_operation               (**r comparison ([<]) *)
  | Ogt: binary_operation               (**r comparison ([>]) *)
  | Ole: binary_operation               (**r comparison ([<=]) *)
  | Oge: binary_operation               (**r comparison ([>=]) *)
           
  | Osha_2 : binary_operation.          (**r Keccak-256 hash *)

Inductive incr_or_decr : Type := Incr | Decr.


Definition sem_unary_operation_int256 (op: unary_operation) (n: int256) : option int256 :=
  match op with
  | Onotbool => Some (if (Int256.eq n Int256.zero) then Int256.one else Int256.zero)
  | Onotint => Some (Int256.not n)
  | Oneg => Some (Int256.neg n)
  | Osha_1 => None (* handled elsewhere. *)
  end.

Definition sem_unary_operation (op: unary_operation) (v: val) (_:type) : option val :=
  match op with
  | Osha_1 => Some (sha_1 v)
  | _ => 
    match v with
    | Vint i => match sem_unary_operation_int256 op i with
                | Some i' => Some (Vint i')
                | None => None
                end
    | _ => None
    end
  end.

Definition int256_of_bool (b:bool) :=
  if b then Int256.one else Int256.zero.

Definition Int256_comparison (signed:bool) (op : comparison) (n m : int256) :=
if signed then Some (int256_of_bool (Int256.cmp  op n m))
          else Some (int256_of_bool (Int256.cmpu op n m)).

(* TODO: complete these. Need to have similar for all the other binary operations. *)
Definition sem_binary_operation_int256 (op: binary_operation) (signed:bool) (n m: int256) : option int256 :=
  match op with
  | Oadd  => Some (Int256.add n m)
  | Osub  => Some (Int256.sub n m)
  | Omul  => Some (Int256.mul n m)
  | Odiv  => Some (Int256.divu n m)
  | Omod => if signed then Some (Int256.mods n m)
                      else Some (Int256.modu n m)
  | Oexp  => None (* todo *)
  | Oand  => Some (Int256.and n m)
  | Oor   => Some (Int256.or  n m)
  | Oxor  => Some (Int256.xor n m)
  | Oshl  => Some (Int256.shl n m)
  | Oshr => if signed then Some (Int256.shr n m)
                      else Some (Int256.shru n m)
  | Oeq   => Int256_comparison signed Ceq n m
  | One   => Int256_comparison signed Cne n m              
  | Olt   => Int256_comparison signed Clt n m
  | Ogt   => Int256_comparison signed Cgt n m
  | Oge   => Int256_comparison signed Cge n m
  | Ole   => Int256_comparison signed Cle n m

  | Osha_2 => None (* handled elsewhere. *)                               
  end.

Definition val_eq_dec : forall (v1 v2 :val), {v1=v2}+{v1<>v2}.
  decide equality.
  + apply Int256.eq_dec.
  + decide equality ; try apply Pos.eq_dec; try apply Int256.eq_dec; try apply IdentExtIndexed.eq.
Defined.

Definition sem_binary_operation (op: binary_operation) (v1 :val) (t1:type) (v2 : val) (t2:type) : option val :=
  match op with
  | Osha_2 => Some (sha_2 v1 v2)
  | Oeq => Some (Vint (if val_eq_dec v1 v2 then (Int256.one) else (Int256.zero)))
  | One => Some (Vint (if val_eq_dec v1 v2 then (Int256.zero) else (Int256.one)))
  | _ => 
    match is_signed t1, is_signed t2 with
    | false, false =>
      match v1, v2 with
      | Vint i1, Vint i2 => match sem_binary_operation_int256 op false i1 i2 with
                            | Some i => Some (Vint i)
                            | None => None
                            end
      | _,_ => None                 
      end
    | _,_ => None (* todo: support signed integer types *)
    end
  end.

Definition sem_unop_low (op: unary_operation) (v:LowValues.val) (t:type) : option LowValues.val :=
  match sem_unary_operation op (OfLow v) t with
  | Some v' => ToLowErr v'
  | None => None
  end.

Definition sem_binop_low (op:binary_operation) (v1:LowValues.val) (t1:type) (v2:LowValues.val) (t2:type) :=
  match sem_binary_operation op (OfLow v1) t1 (OfLow v2) t2 with
  | Some v' => ToLowErr v'
  | None => None
  end.
