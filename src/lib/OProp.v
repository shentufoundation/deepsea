(* *********************************************************************)
(*    DeepSpec, the language of certified softwares                    *)
(*                                                                     *)
(*      Shu-Chun Weng, Yale University                                 *)
(*                                                                     *)
(*  Copyright (c) 2013-2015 Shu-Chun Weng <shu-chun.weng@yale.edu>.    *)
(*                                                                     *)
(*  This program is free software; you can redistribute it and/or      *)
(*  modify it under the terms of the GNU General Public License        *)
(*  version 2 as published by the Free Software Foundation.  Note that *)
(*  the only valid version of the GPL for this work is version 2, not  *)
(*  v2.2 or v3.x or whatever.                                          *)
(*                                                                     *)
(*  This program is distributed in the hope that it will be useful,    *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of     *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *)
(*  GNU General Public License for more details.                       *)
(*                                                                     *)
(*  You should have received a copy of the GNU General Public License  *)
(*  along with this program; if not, write to the Free Software        *)
(*  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,         *)
(*  MA 02110-1301 USA.                                                 *)
(* *********************************************************************)

(** * Optional proposition
    Many constructs do not need extra side conditions, but [True] polutes the
    final result regardless.  [OProp] replaces [Prop] and, along with [oand],
    only inject to Coq [Prop] at the end, without redundent [True]s. *)
Definition OProp := option Prop.
Definition otrue : OProp := None.
Definition ofalse : OProp := Some False.
Definition oprop : Prop -> OProp := @Some Prop.

Definition oProp (op : OProp) := match op with
  | None => True
  | Some p => p
  end.

Definition oand (op1 op2 : OProp) := match op1, op2 with
  | None, _ => op2
  | _, None => op1
  | Some p1, Some p2 => Some (p1 /\ p2)
  end.

Lemma oand_distr op1 op2 : oProp (oand op1 op2) <-> oProp op1 /\ oProp op2.
Proof.
  destruct op1 as [ pp1 |], op2 as [ pp2 |]; simpl;
    split; try intros [ p1 p2 ]; auto.
Qed.

Lemma oconj op1 op2 (p1 : oProp op1)(p2 : oProp op2) : oProp (oand op1 op2).
Proof (proj2 (oand_distr _ _) (conj p1 p2)).

Definition OProp1 A := option (A -> Prop).
Definition otrue1 {A} : OProp1 A := None.
Definition ofalse1 {A} : OProp1 A := Some (fun _ => False).
Definition oprop1 {A} : (A -> Prop) -> OProp1 A := @Some (A -> Prop).

Definition oProp1 {A}(op : OProp1 A) := match op with
  | None => fun _ => True
  | Some p => p
  end.

Definition omap1 {A A'} f (op : OProp1 A) : OProp1 A' :=
  match op with
  | None => None
  | Some p => Some (fun a => f p a)
  end.

Definition oand1 {A}(op1 op2 : OProp1 A) := match op1, op2 with
  | None, _ => op2
  | _, None => op1
  | Some p1, Some p2 => Some (fun a => p1 a /\ p2 a)
  end.

Lemma oand1_distr {A}(op1 op2 : OProp1 A) a :
    oProp1 (oand1 op1 op2) a <-> oProp1 op1 a /\ oProp1 op2 a.
Proof.
  destruct op1 as [ pp1 |], op2 as [ pp2 |]; simpl;
    split; try intros [ p1 p2 ]; auto.
Qed.

Lemma oconj1 {A}(op1 op2 : OProp1 A) a (p1 : oProp1 op1 a)(p2 : oProp1 op2 a) :
    oProp1 (oand1 op1 op2) a.
Proof (proj2 (oand1_distr _ _ _) (conj p1 p2)).

Definition OProp2 A B := option (A -> B -> Prop).
Definition otrue2 {A B} : OProp2 A B := None.
Definition ofalse2 {A B} : OProp2 A B := Some (fun _ _ => False).
Definition oprop2 {A B} : (A -> B -> Prop) -> OProp2 A B := @Some (A -> B -> Prop).

Definition oProp2 {A B}(op : OProp2 A B) := match op with
  | None => fun _ _ => True
  | Some p => p
  end.

Definition omap2 {A B A' B'} f (op : OProp2 A B) : OProp2 A' B' :=
  match op with
  | None => None
  | Some p => Some (fun a b => f p a b)
  end.

Definition omap21 {A B C} f (op : OProp2 A B) : OProp1 C :=
  match op with
  | None => None
  | Some p => Some (fun c => f p c)
  end.

Definition oimply2 {A B}(op1 op2 : OProp2 A B) := match op1, op2 with
  | None, _ => op2
  | _, None => None
  | Some p1, Some p2 => Some (fun a b => p1 a b -> p2 a b)
  end.

Lemma oimply2_distr {A B}(op1 op2 : OProp2 A B) a b :
    oProp2 (oimply2 op1 op2) a b <-> (oProp2 op1 a b -> oProp2 op2 a b).
Proof.
  destruct op1 as [ pp1 |], op2 as [ pp2 |]; simpl; split; auto.
Qed.

Definition oimply2b {A B} p (op : OProp2 A B) := match op with
  | None => None
  | Some p' => Some (fun a b => p a b -> p' a b)
  end.

Lemma oimply2b_distr {A B} p (op : OProp2 A B) a b :
    oProp2 (oimply2b p op) a b <-> (p a b -> oProp2 op a b).
Proof.
  destruct op as [ pp |]; simpl; split; auto.
Qed.

Definition oand2 {A B}(op1 op2 : OProp2 A B) := match op1, op2 with
  | None, _ => op2
  | _, None => op1
  | Some p1, Some p2 => Some (fun a b => p1 a b /\ p2 a b)
  end.

Lemma oand2_distr {A B}(op1 op2 : OProp2 A B) a b :
    oProp2 (oand2 op1 op2) a b <-> oProp2 op1 a b /\ oProp2 op2 a b.
Proof.
  destruct op1 as [ pp1 |], op2 as [ pp2 |]; simpl;
    split; try intros [ p1 p2 ]; auto.
Qed.

Definition oabsorption2 {A B}(op1 op2 : OProp2 A B) := match op1, op2 with
  | None, _ => op2
  | _, None => op1
  | Some p1, Some p2 => Some (fun a b => p1 a b /\ (p1 a b -> p2 a b))
  end.

Lemma oabsorption2_distr {A B}(op1 op2 : OProp2 A B) a b :
    oProp2 (oabsorption2 op1 op2) a b <->
      (oProp2 op1 a b /\ (oProp2 op1 a b -> oProp2 op2 a b)).
Proof.
  destruct op1 as [ pp1 |], op2 as [ pp2 |]; simpl;
    split; try intros [ p1 p2 ]; auto.
Qed.

Lemma oconj2 {A B}(op1 op2 : OProp2 A B) a b
    (p1 : oProp2 op1 a b)(p2 : oProp2 op2 a b) : oProp2 (oand2 op1 op2) a b.
Proof (proj2 (oand2_distr _ _ _ _) (conj p1 p2)).

Definition olift12a {A B}(op : OProp1 A) : OProp2 A B :=
  option_map (fun p a b => p a) op.
Definition olift12b {A B}(op : OProp1 B) : OProp2 A B :=
  option_map (fun p a b => p b) op.

Definition ounlift21a {A B} a (op : OProp2 A B) : OProp1 B :=
  option_map (fun p b => p a b) op.
Definition ounlift21b {A B} b (op : OProp2 A B) : OProp1 A :=
  option_map (fun p a => p a b) op.

Definition OProp3 A B C := option (A -> B -> C -> Prop).
Definition otrue3 {A B C} : OProp3 A B C := None.
Definition ofalse3 {A B C} : OProp3 A B C := Some (fun _ _ _ => False).
Definition oprop3 {A B C} : (A -> B -> C -> Prop) -> OProp3 A B C
  := @Some (A -> B -> C -> Prop).

Definition oProp3 {A B C}(op : OProp3 A B C) := match op with
  | None => fun _ _ _ => True
  | Some p => p
  end.

Definition omap3 {A B C A' B' C'} f (op : OProp3 A B C) : OProp3 A' B' C' :=
  match op with
  | None => None
  | Some p => Some (fun a b c => f p a b c)
  end.

Definition oand3 {A B C}(op1 op2 : OProp3 A B C) := match op1, op2 with
  | None, _ => op2
  | _, None => op1
  | Some p1, Some p2 => Some (fun a b c => p1 a b c /\ p2 a b c)
  end.

Lemma oand3_distr {A B C}(op1 op2 : OProp3 A B C) a b c :
    oProp3 (oand3 op1 op2) a b c <-> oProp3 op1 a b c /\ oProp3 op2 a b c.
Proof.
  destruct op1 as [ pp1 |], op2 as [ pp2 |]; simpl;
    split; try intros [ p1 p2 ]; auto.
Qed.

Lemma oconj3 {A B C}(op1 op2 : OProp3 A B C) a b c
      (p1 : oProp3 op1 a b c)(p2 : oProp3 op2 a b c) :
    oProp3 (oand3 op1 op2) a b c.
Proof (proj2 (oand3_distr _ _ _ _ _) (conj p1 p2)).

Definition olift23ab {A B C}(op : OProp2 A B) : OProp3 A B C :=
  option_map (fun p a b c => p a b) op.
Definition olift23ac {A B C}(op : OProp2 A C) : OProp3 A B C :=
  option_map (fun p a b c => p a c) op.
Definition olift23bc {A B C}(op : OProp2 B C) : OProp3 A B C :=
  option_map (fun p a b c => p b c) op.

Definition ounlift32a {A B C} a (op : OProp3 A B C) : OProp2 B C :=
  option_map (fun p b c => p a b c) op.
Definition ounlift32b {A B C} b (op : OProp3 A B C) : OProp2 A C :=
  option_map (fun p a c => p a b c) op.
Definition ounlift32c {A B C} c (op : OProp3 A B C) : OProp2 A B :=
  option_map (fun p a b => p a b c) op.

Lemma OProp1map1 {A A'}(f : _ -> A' -> Prop)(op : OProp1 A) a :
    f (fun _ => True) a ->
    (oProp1 (omap1 f op) a <-> f (oProp1 op) a).
Proof.
  intros fbase; destruct op; simpl; tauto.
Qed.

(*
Lemma OProp1map2 {A A' B'}(f : _ -> A' -> B' -> Prop)(op : OProp1 A) a b :
    f (fun _ => True) a b ->
    (oProp2 (option_map f op) a b <-> f (oProp1 op) a b).
Proof.
  intros fbase; destruct op; simpl; tauto.
Qed.
*)

Lemma OProp2map1 {A B A'}(f : _ -> A' -> Prop)(op : OProp2 A B) a :
    f (fun _ _ => True) a ->
    (oProp1 (omap21 f op) a <-> f (oProp2 op) a).
Proof.
  intros fbase; destruct op; simpl; tauto.
Qed.

Lemma OProp2map2 {A B A' B'}(f : _ -> A' -> B' -> Prop)(op : OProp2 A B) a b :
    f (fun _ _ => True) a b ->
    (oProp2 (omap2 f op) a b <-> f (oProp2 op) a b).
Proof.
  intros fbase; destruct op; simpl; tauto.
Qed.

Lemma OProp2lift12a {A B} op a b :
    @oProp2 A B (olift12a op) a b <-> oProp1 op a.
Proof.
  destruct op; simpl; tauto.
Qed.

Delimit Scope oprop_scope with oprop.
Notation "{{ x  }}" := (oprop x%type)
         (at level 0, x at level 99) : oprop_scope.
Notation "x /\ y" := (oand x%oprop y%oprop)
         (at level 80, right associativity) : oprop_scope.

Delimit Scope oprop1_scope with oprop1.
Notation "{{ x  }}" := (oprop1 x%type)
         (at level 0, x at level 99) : oprop1_scope.
Notation "x /\ y" := (oand1 x%oprop1 y%oprop1)
         (at level 80, right associativity) : oprop1_scope.
Notation "x m{ f  }" := (omap1 (fun p y => p (f y)) x%oprop1)
         (at level 25, left associativity) : oprop1_scope.

Delimit Scope oprop2_scope with oprop2.
Notation "{{ x  }}" := (oprop2 x%type)
         (at level 0, x at level 99) : oprop2_scope.
Notation "x /\ y" := (oand2 x%oprop2 y%oprop2)
         (at level 80, right associativity) : oprop2_scope.
Notation "x m{ f  } m{ g  }" := (omap2 (fun p y z => p (f y) (g z)) x%oprop2)
         (at level 25, left associativity) : oprop2_scope.
Notation "x m{ f  } m{ g  }" := (omap21 (fun p y => p (f y) (g y)) x%oprop2)
         (at level 25, left associativity) : oprop1_scope.

Delimit Scope oprop3_scope with oprop3.
Notation "{{ x  }}" := (oprop3 x%type)
         (at level 0, x at level 99) : oprop3_scope.
Notation "x /\ y" := (oand3 x%oprop3 y%oprop3)
         (at level 80, right associativity) : oprop3_scope.
Notation "x m{ f  } m{ g  } m{ h  }" := (omap3 (fun p y z w => p (f y) (g z) (h w)) x%oprop3)
         (at level 25, left associativity) : oprop3_scope.
