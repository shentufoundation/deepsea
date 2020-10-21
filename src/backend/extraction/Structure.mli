open Datatypes
open Integers
open Types
open Values

type __ = Obj.t

type sx =
| Unsigned
| Signed

module IntOp :
 sig
  type unop =
  | Clz
  | Ctz
  | Popcnt

  val unop_rect : 'a1 -> 'a1 -> 'a1 -> unop -> 'a1

  val unop_rec : 'a1 -> 'a1 -> 'a1 -> unop -> 'a1

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

  val binop_rect :
    'a1 -> 'a1 -> 'a1 -> (sx -> 'a1) -> (sx -> 'a1) -> 'a1 -> 'a1 -> 'a1 ->
    'a1 -> (sx -> 'a1) -> 'a1 -> 'a1 -> binop -> 'a1

  val binop_rec :
    'a1 -> 'a1 -> 'a1 -> (sx -> 'a1) -> (sx -> 'a1) -> 'a1 -> 'a1 -> 'a1 ->
    'a1 -> (sx -> 'a1) -> 'a1 -> 'a1 -> binop -> 'a1

  val testop_rect : 'a1 -> 'a1

  val testop_rec : 'a1 -> 'a1

  type relop =
  | Eq
  | Ne
  | Lt of sx
  | Gt of sx
  | Le of sx
  | Ge of sx

  val relop_rect :
    'a1 -> 'a1 -> (sx -> 'a1) -> (sx -> 'a1) -> (sx -> 'a1) -> (sx -> 'a1) ->
    relop -> 'a1

  val relop_rec :
    'a1 -> 'a1 -> (sx -> 'a1) -> (sx -> 'a1) -> (sx -> 'a1) -> (sx -> 'a1) ->
    relop -> 'a1

  type cvtop =
  | Wrap_i64
  | Extend_i32 of sx
  | Trunc_f32 of sx
  | Trunc_f64 of sx
  | Reinterpret

  val cvtop_rect :
    'a1 -> (sx -> 'a1) -> (sx -> 'a1) -> (sx -> 'a1) -> 'a1 -> cvtop -> 'a1

  val cvtop_rec :
    'a1 -> (sx -> 'a1) -> (sx -> 'a1) -> (sx -> 'a1) -> 'a1 -> cvtop -> 'a1

  type memop =
  | Store
  | Load

  val memop_rect : 'a1 -> 'a1 -> memop -> 'a1

  val memop_rec : 'a1 -> 'a1 -> memop -> 'a1
 end

module FloatOp :
 sig
  type unop =
  | Abs
  | Neg
  | Sqrt
  | Ceil
  | Floor
  | Trunc
  | Nearest

  val unop_rect : 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> unop -> 'a1

  val unop_rec : 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> unop -> 'a1

  type binop =
  | Add
  | Sub
  | Mul
  | Div
  | Min
  | Max
  | Copysign

  val binop_rect :
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> binop -> 'a1

  val binop_rec :
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> binop -> 'a1

  val testop_rect : __ -> 'a1

  val testop_rec : __ -> 'a1

  type relop =
  | Eq
  | Ne
  | Lt
  | Gt
  | Le
  | Ge

  val relop_rect : 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> relop -> 'a1

  val relop_rec : 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> relop -> 'a1

  type cvtop =
  | Demote_f64
  | Promote_f32
  | Convert_i32 of sx
  | Convert_i64 of sx
  | Reinterpret

  val cvtop_rect :
    'a1 -> 'a1 -> (sx -> 'a1) -> (sx -> 'a1) -> 'a1 -> cvtop -> 'a1

  val cvtop_rec :
    'a1 -> 'a1 -> (sx -> 'a1) -> (sx -> 'a1) -> 'a1 -> cvtop -> 'a1

  type memop =
  | Store
  | Load

  val memop_rect : 'a1 -> 'a1 -> memop -> 'a1

  val memop_rec : 'a1 -> 'a1 -> memop -> 'a1
 end

module IOp32 :
 sig
  type unop = IntOp.unop =
  | Clz
  | Ctz
  | Popcnt

  val unop_rect : 'a1 -> 'a1 -> 'a1 -> unop -> 'a1

  val unop_rec : 'a1 -> 'a1 -> 'a1 -> unop -> 'a1

  type binop = IntOp.binop =
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

  val binop_rect :
    'a1 -> 'a1 -> 'a1 -> (sx -> 'a1) -> (sx -> 'a1) -> 'a1 -> 'a1 -> 'a1 ->
    'a1 -> (sx -> 'a1) -> 'a1 -> 'a1 -> binop -> 'a1

  val binop_rec :
    'a1 -> 'a1 -> 'a1 -> (sx -> 'a1) -> (sx -> 'a1) -> 'a1 -> 'a1 -> 'a1 ->
    'a1 -> (sx -> 'a1) -> 'a1 -> 'a1 -> binop -> 'a1

  val testop_rect : 'a1 -> 'a1

  val testop_rec : 'a1 -> 'a1

  type relop = IntOp.relop =
  | Eq
  | Ne
  | Lt of sx
  | Gt of sx
  | Le of sx
  | Ge of sx

  val relop_rect :
    'a1 -> 'a1 -> (sx -> 'a1) -> (sx -> 'a1) -> (sx -> 'a1) -> (sx -> 'a1) ->
    relop -> 'a1

  val relop_rec :
    'a1 -> 'a1 -> (sx -> 'a1) -> (sx -> 'a1) -> (sx -> 'a1) -> (sx -> 'a1) ->
    relop -> 'a1

  type cvtop = IntOp.cvtop =
  | Wrap_i64
  | Extend_i32 of sx
  | Trunc_f32 of sx
  | Trunc_f64 of sx
  | Reinterpret

  val cvtop_rect :
    'a1 -> (sx -> 'a1) -> (sx -> 'a1) -> (sx -> 'a1) -> 'a1 -> cvtop -> 'a1

  val cvtop_rec :
    'a1 -> (sx -> 'a1) -> (sx -> 'a1) -> (sx -> 'a1) -> 'a1 -> cvtop -> 'a1

  type memop = IntOp.memop =
  | Store
  | Load

  val memop_rect : 'a1 -> 'a1 -> memop -> 'a1

  val memop_rec : 'a1 -> 'a1 -> memop -> 'a1
 end

module IOp64 :
 sig
  type unop = IntOp.unop =
  | Clz
  | Ctz
  | Popcnt

  val unop_rect : 'a1 -> 'a1 -> 'a1 -> unop -> 'a1

  val unop_rec : 'a1 -> 'a1 -> 'a1 -> unop -> 'a1

  type binop = IntOp.binop =
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

  val binop_rect :
    'a1 -> 'a1 -> 'a1 -> (sx -> 'a1) -> (sx -> 'a1) -> 'a1 -> 'a1 -> 'a1 ->
    'a1 -> (sx -> 'a1) -> 'a1 -> 'a1 -> binop -> 'a1

  val binop_rec :
    'a1 -> 'a1 -> 'a1 -> (sx -> 'a1) -> (sx -> 'a1) -> 'a1 -> 'a1 -> 'a1 ->
    'a1 -> (sx -> 'a1) -> 'a1 -> 'a1 -> binop -> 'a1

  val testop_rect : 'a1 -> 'a1

  val testop_rec : 'a1 -> 'a1

  type relop = IntOp.relop =
  | Eq
  | Ne
  | Lt of sx
  | Gt of sx
  | Le of sx
  | Ge of sx

  val relop_rect :
    'a1 -> 'a1 -> (sx -> 'a1) -> (sx -> 'a1) -> (sx -> 'a1) -> (sx -> 'a1) ->
    relop -> 'a1

  val relop_rec :
    'a1 -> 'a1 -> (sx -> 'a1) -> (sx -> 'a1) -> (sx -> 'a1) -> (sx -> 'a1) ->
    relop -> 'a1

  type cvtop = IntOp.cvtop =
  | Wrap_i64
  | Extend_i32 of sx
  | Trunc_f32 of sx
  | Trunc_f64 of sx
  | Reinterpret

  val cvtop_rect :
    'a1 -> (sx -> 'a1) -> (sx -> 'a1) -> (sx -> 'a1) -> 'a1 -> cvtop -> 'a1

  val cvtop_rec :
    'a1 -> (sx -> 'a1) -> (sx -> 'a1) -> (sx -> 'a1) -> 'a1 -> cvtop -> 'a1

  type memop = IntOp.memop =
  | Store
  | Load

  val memop_rect : 'a1 -> 'a1 -> memop -> 'a1

  val memop_rec : 'a1 -> 'a1 -> memop -> 'a1
 end

module FOp32 :
 sig
  type unop = FloatOp.unop =
  | Abs
  | Neg
  | Sqrt
  | Ceil
  | Floor
  | Trunc
  | Nearest

  val unop_rect : 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> unop -> 'a1

  val unop_rec : 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> unop -> 'a1

  type binop = FloatOp.binop =
  | Add
  | Sub
  | Mul
  | Div
  | Min
  | Max
  | Copysign

  val binop_rect :
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> binop -> 'a1

  val binop_rec :
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> binop -> 'a1

  val testop_rect : __ -> 'a1

  val testop_rec : __ -> 'a1

  type relop = FloatOp.relop =
  | Eq
  | Ne
  | Lt
  | Gt
  | Le
  | Ge

  val relop_rect : 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> relop -> 'a1

  val relop_rec : 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> relop -> 'a1

  type cvtop = FloatOp.cvtop =
  | Demote_f64
  | Promote_f32
  | Convert_i32 of sx
  | Convert_i64 of sx
  | Reinterpret

  val cvtop_rect :
    'a1 -> 'a1 -> (sx -> 'a1) -> (sx -> 'a1) -> 'a1 -> cvtop -> 'a1

  val cvtop_rec :
    'a1 -> 'a1 -> (sx -> 'a1) -> (sx -> 'a1) -> 'a1 -> cvtop -> 'a1

  type memop = FloatOp.memop =
  | Store
  | Load

  val memop_rect : 'a1 -> 'a1 -> memop -> 'a1

  val memop_rec : 'a1 -> 'a1 -> memop -> 'a1
 end

module FOp64 :
 sig
  type unop = FloatOp.unop =
  | Abs
  | Neg
  | Sqrt
  | Ceil
  | Floor
  | Trunc
  | Nearest

  val unop_rect : 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> unop -> 'a1

  val unop_rec : 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> unop -> 'a1

  type binop = FloatOp.binop =
  | Add
  | Sub
  | Mul
  | Div
  | Min
  | Max
  | Copysign

  val binop_rect :
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> binop -> 'a1

  val binop_rec :
    'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> binop -> 'a1

  val testop_rect : __ -> 'a1

  val testop_rec : __ -> 'a1

  type relop = FloatOp.relop =
  | Eq
  | Ne
  | Lt
  | Gt
  | Le
  | Ge

  val relop_rect : 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> relop -> 'a1

  val relop_rec : 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> relop -> 'a1

  type cvtop = FloatOp.cvtop =
  | Demote_f64
  | Promote_f32
  | Convert_i32 of sx
  | Convert_i64 of sx
  | Reinterpret

  val cvtop_rect :
    'a1 -> 'a1 -> (sx -> 'a1) -> (sx -> 'a1) -> 'a1 -> cvtop -> 'a1

  val cvtop_rec :
    'a1 -> 'a1 -> (sx -> 'a1) -> (sx -> 'a1) -> 'a1 -> cvtop -> 'a1

  type memop = FloatOp.memop =
  | Store
  | Load

  val memop_rect : 'a1 -> 'a1 -> memop -> 'a1

  val memop_rec : 'a1 -> 'a1 -> memop -> 'a1
 end

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
