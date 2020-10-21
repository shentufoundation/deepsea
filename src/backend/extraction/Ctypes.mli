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

val sizeof_words : coq_type -> coq_Z

val sizeof_struct_words : fieldlist -> coq_Z -> coq_Z

val struct_field : fieldlist -> ident -> (nat, coq_type) prod option
