open AST
open BinInt
open BinNums
open Ctypes
open Datatypes
open Integers

val sp : Int256.int

val sb : Int256.int

val sizeof : coq_type -> Int256.int

val offset : fieldlist -> ident -> Int256.int option

val frame_size : (ident, coq_type) prod list -> Int256.int

val mkfieldlist : (ident, coq_type) prod list -> fieldlist
