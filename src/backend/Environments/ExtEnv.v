Require Import cclib.Coqlib.
Require Import backend.Ctypes.
Require Import cclib.Maps.
Require Import cclib.Integers.
Require Import backend.Values.HighValues.
Require Import backend.AST.

Section ExtEnv.

Variable function : Type.
Variable fn_locals : function -> list (ident * type).

Inductive contents :=
| EEVal : forall v:val, contents
| EEUndef : contents
| EEDead : contents
.

(* at a lower level, storage and memory are separate *)
Definition ext_env : Type := IdentExtMap.t (contents).

Notation "env [ i ]" := (IdentExtMap.get i env) (at level 80).
Notation "env [ i |-> v ]" := (IdentExtMap.set i v env) (at level 80).

Definition empty_stack : ext_env :=
  Int256Map.init EEDead.

Fixpoint alloc_eid (ty:type) (root:ident_ext) (ee:ext_env) (init:contents) : option ext_env :=
  match ty with
  | Tarray ty' size =>
    let fix alloc (ind:nat) (ee:ext_env) :=
        match ind with
        | O => Some ee
        | S ind' =>
          match alloc ind' ee with
          | Some ee' => alloc_eid ty' (Index root (Int256.repr (Z.of_nat ind))) ee' init
          | None => None
          end       
        end
    in
    match size with
    | Zpos p => alloc (Pos.to_nat p) ee
    | _ => None
    end
  | Tstruct id fl =>
    let fix alloc (fl:fieldlist) (ee:ext_env) :=
        match fl with
        | Fnil => Some ee
        | Fcons id ty' fl' =>
          match alloc fl' ee with
          | Some se' => alloc_eid ty' (Field root id) se' init
          | None => None
          end
        end
    in alloc fl ee
  | _ => Some (ee[root|->init])
  end.

Fixpoint initial_locs (ctx:int256) (locs : list (ident*type)) (ee:ext_env) : option ext_env :=
  match locs with
  | nil => Some ee
  | (id,ty)::locs' =>
    match initial_locs ctx locs' ee with
    | Some se' => alloc_eid ty (Field (Local ctx) id) ee EEUndef
    | None => None
    end
  end.

Fixpoint clear_locs (ctx:int256) (locs : list (ident*type)) (ee:ext_env) : option ext_env :=
  match locs with
  | nil => Some ee
  | (id,ty)::locs' =>
    match initial_locs ctx locs' ee with
    | Some se' => alloc_eid ty (Field (Local ctx) id) ee EEDead
    | None => None
    end
  end.

Fixpoint frame_size (locs:list (ident * type)) : int256 :=
  match locs with
  | nil => Int256.repr 0
  | cons (_,ty) locs' => Int256.add (Int256.repr (sizeof_words ty)) (frame_size locs')
  end.

Definition push_func (ctx:int256) (f:function) (ee:ext_env) : option (ext_env*int256) :=
  match initial_locs ctx (fn_locals f) ee with
  | Some ee' => Some (ee', Int256.add ctx (frame_size (fn_locals f)))
  | None => None
  end.

Definition pop_func (ctx:int256) (f:function) (ee:ext_env) : option (ext_env*int256) :=
  match clear_locs ctx (fn_locals f) ee with
  | Some ee' => Some (ee', Int256.sub ctx (frame_size (fn_locals f)))
  | None => None
  end.

Definition read (eid:ident_ext) (ee:ext_env) : option val :=
  match ee[eid] with
  | EEVal v => Some v
  | _ => None
  end.

Definition write (eid:ident_ext) (v:val) (ee:ext_env) : option ext_env :=
  match ee[eid] with
  | EEDead => None
  | _ => Some (ee[eid|->(EEVal v)])
  end.

Definition var (ctx:int256) (id:ident) (ee:ext_env) : option ident_ext :=
  let eid := Field (Local ctx) id in
  match ee[eid] with
  | EEDead => None
  | _ => Some eid
  end.

Definition glob (id:ident) (ee:ext_env) : option ident_ext :=
  Some (Field Global id).

Definition addr (eid:ident_ext) : val :=
  Vptr (LVeid eid).

Definition deref (v:val) : option ident_ext :=
  match v with
  | Vptr (LVeid eid) => Some eid
  | _ => None
  end.

Definition access_field (base:ident_ext) (ty:type) (fid:ident) : option ident_ext :=
  match ty with
  | Tstruct _ fl =>
    match struct_field fl fid with
    | Some _ => Some (Field base fid)
    | None => None
    end
  | _ => None
  end.

Definition index_array (base:ident_ext) (ty:type) (ind:val) : option ident_ext :=
  match ind,ty with
  | Vint off, Tarray ty' size =>
    if Int256.lt off (Int256.repr size) then
      Some (Index base off)
    else None
  | Vint off, Thashmap _ _  =>
      Some (Index base off)
  | _, _ => None
  end.

End ExtEnv.
