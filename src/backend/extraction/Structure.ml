open Datatypes
open Integers
open Types
open Values

type __ = Obj.t

type sx =
| Unsigned
| Signed

module IntOp =
 struct
  type unop =
  | Clz
  | Ctz
  | Popcnt

  (** val unop_rect : 'a1 -> 'a1 -> 'a1 -> unop -> 'a1 **)

  let unop_rect f f0 f1 = function
  | Clz -> f
  | Ctz -> f0
  | Popcnt -> f1

  (** val unop_rec : 'a1 -> 'a1 -> 'a1 -> unop -> 'a1 **)

  let unop_rec f f0 f1 = function
  | Clz -> f
  | Ctz -> f0
  | Popcnt -> f1

  type binop =
  | Add
  | Sub
  | Mul
  | Div of sx
  | Rem of sx
  | And
  | Or
  | Xor
  | Shl
  | Shr of sx
  | Rotl
  | Rotr

  (** val binop_rect :
      'a1 -> 'a1 -> 'a1 -> (sx -> 'a1) -> (sx -> 'a1) -> 'a1 -> 'a1 -> 'a1 ->
      'a1 -> (sx -> 'a1) -> 'a1 -> 'a1 -> binop -> 'a1 **)

  let binop_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 = function
  | Add -> f
  | Sub -> f0
  | Mul -> f1
  | Div x -> f2 x
  | Rem x -> f3 x
  | And -> f4
  | Or -> f5
  | Xor -> f6
  | Shl -> f7
  | Shr x -> f8 x
  | Rotl -> f9
  | Rotr -> f10

  (** val binop_rec :
      'a1 -> 'a1 -> 'a1 -> (sx -> 'a1) -> (sx -> 'a1) -> 'a1 -> 'a1 -> 'a1 ->
      'a1 -> (sx -> 'a1) -> 'a1 -> 'a1 -> binop -> 'a1 **)

  let binop_rec f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 = function
  | Add -> f
  | Sub -> f0
  | Mul -> f1
  | Div x -> f2 x
  | Rem x -> f3 x
  | And -> f4
  | Or -> f5
  | Xor -> f6
  | Shl -> f7
  | Shr x -> f8 x
  | Rotl -> f9
  | Rotr -> f10

  (** val testop_rect : 'a1 -> 'a1 **)

  let testop_rect f =
    f

  (** val testop_rec : 'a1 -> 'a1 **)

  let testop_rec f =
    f

  type relop =
  | Eq
  | Ne
  | Lt of sx
  | Gt of sx
  | Le of sx
  | Ge of sx

  (** val relop_rect :
      'a1 -> 'a1 -> (sx -> 'a1) -> (sx -> 'a1) -> (sx -> 'a1) -> (sx -> 'a1)
      -> relop -> 'a1 **)

  let relop_rect f f0 f1 f2 f3 f4 = function
  | Eq -> f
  | Ne -> f0
  | Lt x -> f1 x
  | Gt x -> f2 x
  | Le x -> f3 x
  | Ge x -> f4 x

  (** val relop_rec :
      'a1 -> 'a1 -> (sx -> 'a1) -> (sx -> 'a1) -> (sx -> 'a1) -> (sx -> 'a1)
      -> relop -> 'a1 **)

  let relop_rec f f0 f1 f2 f3 f4 = function
  | Eq -> f
  | Ne -> f0
  | Lt x -> f1 x
  | Gt x -> f2 x
  | Le x -> f3 x
  | Ge x -> f4 x

  type cvtop =
  | Wrap_i64
  | Extend_i32 of sx
  | Trunc_f32 of sx
  | Trunc_f64 of sx
  | Reinterpret

  (** val cvtop_rect :
      'a1 -> (sx -> 'a1) -> (sx -> 'a1) -> (sx -> 'a1) -> 'a1 -> cvtop -> 'a1 **)

  let cvtop_rect f f0 f1 f2 f3 = function
  | Wrap_i64 -> f
  | Extend_i32 x -> f0 x
  | Trunc_f32 x -> f1 x
  | Trunc_f64 x -> f2 x
  | Reinterpret -> f3

  (** val cvtop_rec :
      'a1 -> (sx -> 'a1) -> (sx -> 'a1) -> (sx -> 'a1) -> 'a1 -> cvtop -> 'a1 **)

  let cvtop_rec f f0 f1 f2 f3 = function
  | Wrap_i64 -> f
  | Extend_i32 x -> f0 x
  | Trunc_f32 x -> f1 x
  | Trunc_f64 x -> f2 x
  | Reinterpret -> f3

  type memop =
  | Store
  | Load

  (** val memop_rect : 'a1 -> 'a1 -> memop -> 'a1 **)

  let memop_rect f f0 = function
  | Store -> f
  | Load -> f0

  (** val memop_rec : 'a1 -> 'a1 -> memop -> 'a1 **)

  let memop_rec f f0 = function
  | Store -> f
  | Load -> f0
 end

module FloatOp =
 struct
  type unop =
  | Abs
  | Neg
  | Sqrt
  | Ceil
  | Floor
  | Trunc
  | Nearest

  (** val unop_rect :
      'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> unop -> 'a1 **)

  let unop_rect f f0 f1 f2 f3 f4 f5 = function
  | Abs -> f
  | Neg -> f0
  | Sqrt -> f1
  | Ceil -> f2
  | Floor -> f3
  | Trunc -> f4
  | Nearest -> f5

  (** val unop_rec :
      'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> unop -> 'a1 **)

  let unop_rec f f0 f1 f2 f3 f4 f5 = function
  | Abs -> f
  | Neg -> f0
  | Sqrt -> f1
  | Ceil -> f2
  | Floor -> f3
  | Trunc -> f4
  | Nearest -> f5

  type binop =
  | Add
  | Sub
  | Mul
  | Div
  | Min
  | Max
  | Copysign

  (** val binop_rect :
      'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> binop -> 'a1 **)

  let binop_rect f f0 f1 f2 f3 f4 f5 = function
  | Add -> f
  | Sub -> f0
  | Mul -> f1
  | Div -> f2
  | Min -> f3
  | Max -> f4
  | Copysign -> f5

  (** val binop_rec :
      'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> binop -> 'a1 **)

  let binop_rec f f0 f1 f2 f3 f4 f5 = function
  | Add -> f
  | Sub -> f0
  | Mul -> f1
  | Div -> f2
  | Min -> f3
  | Max -> f4
  | Copysign -> f5

  (** val testop_rect : __ -> 'a1 **)

  let testop_rect _ =
    assert false (* absurd case *)

  (** val testop_rec : __ -> 'a1 **)

  let testop_rec _ =
    assert false (* absurd case *)

  type relop =
  | Eq
  | Ne
  | Lt
  | Gt
  | Le
  | Ge

  (** val relop_rect :
      'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> relop -> 'a1 **)

  let relop_rect f f0 f1 f2 f3 f4 = function
  | Eq -> f
  | Ne -> f0
  | Lt -> f1
  | Gt -> f2
  | Le -> f3
  | Ge -> f4

  (** val relop_rec :
      'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> relop -> 'a1 **)

  let relop_rec f f0 f1 f2 f3 f4 = function
  | Eq -> f
  | Ne -> f0
  | Lt -> f1
  | Gt -> f2
  | Le -> f3
  | Ge -> f4

  type cvtop =
  | Demote_f64
  | Promote_f32
  | Convert_i32 of sx
  | Convert_i64 of sx
  | Reinterpret

  (** val cvtop_rect :
      'a1 -> 'a1 -> (sx -> 'a1) -> (sx -> 'a1) -> 'a1 -> cvtop -> 'a1 **)

  let cvtop_rect f f0 f1 f2 f3 = function
  | Demote_f64 -> f
  | Promote_f32 -> f0
  | Convert_i32 x -> f1 x
  | Convert_i64 x -> f2 x
  | Reinterpret -> f3

  (** val cvtop_rec :
      'a1 -> 'a1 -> (sx -> 'a1) -> (sx -> 'a1) -> 'a1 -> cvtop -> 'a1 **)

  let cvtop_rec f f0 f1 f2 f3 = function
  | Demote_f64 -> f
  | Promote_f32 -> f0
  | Convert_i32 x -> f1 x
  | Convert_i64 x -> f2 x
  | Reinterpret -> f3

  type memop =
  | Store
  | Load

  (** val memop_rect : 'a1 -> 'a1 -> memop -> 'a1 **)

  let memop_rect f f0 = function
  | Store -> f
  | Load -> f0

  (** val memop_rec : 'a1 -> 'a1 -> memop -> 'a1 **)

  let memop_rec f f0 = function
  | Store -> f
  | Load -> f0
 end

module IOp32 = IntOp

module IOp64 = IntOp

module FOp32 = FloatOp

module FOp64 = FloatOp

type unop = (IOp32.unop, IOp64.unop, FOp32.unop, FOp64.unop) op

type binop = (IOp32.binop, IOp64.binop, FOp32.binop, FOp64.binop) op

type testop = (__, __, __, __) op

type relop = (IOp32.relop, IOp64.relop, FOp32.relop, FOp64.relop) op

type cvtop = (IOp32.cvtop, IOp64.cvtop, FOp32.cvtop, FOp64.cvtop) op

type memop = (IOp32.memop, IOp64.memop, FOp32.memop, FOp64.memop) op

type blocktype =
| BT_typeidx of nat
| BT_valtype of valtype option

type instr =
| Const of coq_val
| Const_nat of nat
| Const_256 of Int256.int
| Unop of unop
| Binop of binop
| Testop of testop
| Relop of relop
| Memop of memop
| Cvtop of cvtop
| Drop
| Select
| Local_get of nat
| Local_set of nat
| Local_tee of nat
| Global_get of nat
| Global_set of nat
| Nop
| Unreachable
| Block of blocktype * instr list
| Loop of blocktype * instr list
| If of blocktype * instr list * instr list
| Br of nat
| Br_if of nat
| Br_table of nat list * nat
| Return
| Call of nat
| Call_indirect of nat

type expr = instr list

type func = { coq_F_type : nat; coq_F_locals : valtype list; coq_F_body : expr }

type table =
  tabletype
  (* singleton inductive, whose constructor was Build_table *)

type mem = memtype
  (* singleton inductive, whose constructor was Build_mem *)

type global = { coq_G_type : globaltype; coq_G_init : expr }

type elem = { coq_EL_table : nat; coq_EL_offset : expr; coq_EL_init : nat list }

type data = { coq_D_data : nat; coq_D_offset : expr; coq_D_init : byte list }

type start = nat
  (* singleton inductive, whose constructor was Build_start *)

type exportdesc =
| EXD_func of nat
| EXD_table of nat
| EXD_mem of nat
| EXD_global of nat

type export = { coq_EX_name : name; coq_EX_desc : exportdesc }

type importdesc =
| IMD_func of nat
| IMD_table of nat
| IMD_mem of nat
| IMD_global of nat

type import = { coq_IM_module : name; coq_IM_name : name;
                coq_IM_desc : importdesc }

type coq_module = { coq_M_types : functype list; coq_M_funcs : func list;
                    coq_M_tables : table list; coq_M_mems : mem list;
                    coq_M_globals : global list; coq_M_elem : elem list;
                    coq_M_data : data list; coq_M_start : start option;
                    coq_M_imports : import list; coq_M_exports : export list }
