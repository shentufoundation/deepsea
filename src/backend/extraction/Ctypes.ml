open AST
open BinInt
open BinNums
open Datatypes
open Monad
open OptionMonad
open Specif

type signedness =
| Signed
| Unsigned

type intsize =
| I8
| I16
| I32
| I256
| IBool

type ptrkind =
| Coq_mem
| Coq_stor
| Coq_call

type coq_type =
| Tvoid
| Tint of intsize * signedness
| Tpointer of ptrkind * coq_type
| Tarray of coq_type * coq_Z
| Thashmap of coq_type * coq_type
| Tfunction of typelist * coq_type
| Tstruct of ident * fieldlist
| Tunion of ident * fieldlist
| Tcomp_ptr of ident
and typelist =
| Tnil
| Tcons of coq_type * typelist
and fieldlist =
| Fnil
| Fcons of ident * coq_type * fieldlist

(** val sizeof_words : coq_type -> coq_Z **)

let rec sizeof_words = function
| Tarray (t', n) -> Z.mul (sizeof_words t') (Z.max Z0 n)
| Tstruct (_, fld) -> sizeof_struct_words fld Z0
| _ -> Zpos Coq_xH

(** val sizeof_struct_words : fieldlist -> coq_Z -> coq_Z **)

and sizeof_struct_words fld pos =
  match fld with
  | Fnil -> pos
  | Fcons (_, t, fld') ->
    sizeof_struct_words fld' (Z.add pos (sizeof_words t))

(** val struct_field : fieldlist -> ident -> (nat, coq_type) prod option **)

let rec struct_field fld id =
  match fld with
  | Fnil -> None
  | Fcons (id', t, fld') ->
    (match ident_eq id' id with
     | Coq_left -> Some (Coq_pair (O, t))
     | Coq_right ->
       bind2 (Obj.magic coq_Monad_option) (struct_field fld' id)
         (fun offset t' -> Some (Coq_pair ((S offset), t'))))
