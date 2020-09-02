open BinInt
open BinNums
open BinPos
open Coqlib
open Datatypes
open List0
open Specif
open Zpower

type comparison =
| Ceq
| Cne
| Clt
| Cle
| Cgt
| Cge

module type WORDSIZE =
 sig
  val wordsize : nat
 end

module Make =
 functor (WS:WORDSIZE) ->
 struct
  (** val wordsize : nat **)

  let wordsize =
    WS.wordsize

  (** val zwordsize : coq_Z **)

  let zwordsize =
    Z.of_nat wordsize

  (** val modulus : coq_Z **)

  let modulus =
    two_power_nat wordsize

  (** val half_modulus : coq_Z **)

  let half_modulus =
    Z.div modulus (Zpos (Coq_xO Coq_xH))

  (** val max_unsigned : coq_Z **)

  let max_unsigned =
    Z.sub modulus (Zpos Coq_xH)

  (** val max_signed : coq_Z **)

  let max_signed =
    Z.sub half_modulus (Zpos Coq_xH)

  (** val min_signed : coq_Z **)

  let min_signed =
    Z.opp half_modulus

  type int = coq_Z
    (* singleton inductive, whose constructor was mkint *)

  (** val intval : int -> coq_Z **)

  let intval i =
    i

  (** val coq_P_mod_two_p : positive -> nat -> coq_Z **)

  let rec coq_P_mod_two_p p = function
  | O -> Z0
  | S m ->
    (match p with
     | Coq_xI q -> Z.succ_double (coq_P_mod_two_p q m)
     | Coq_xO q -> Z.double (coq_P_mod_two_p q m)
     | Coq_xH -> Zpos Coq_xH)

  (** val coq_Z_mod_modulus : coq_Z -> coq_Z **)

  let coq_Z_mod_modulus = function
  | Z0 -> Z0
  | Zpos p -> coq_P_mod_two_p p wordsize
  | Zneg p ->
    let r = coq_P_mod_two_p p wordsize in
    (match zeq r Z0 with
     | Coq_left -> Z0
     | Coq_right -> Z.sub modulus r)

  (** val unsigned : int -> coq_Z **)

  let unsigned =
    intval

  (** val signed : int -> coq_Z **)

  let signed n =
    let x = unsigned n in
    (match zlt x half_modulus with
     | Coq_left -> x
     | Coq_right -> Z.sub x modulus)

  (** val repr : coq_Z -> int **)

  let repr =
    coq_Z_mod_modulus

  (** val zero : int **)

  let zero =
    repr Z0

  (** val one : int **)

  let one =
    repr (Zpos Coq_xH)

  (** val mone : int **)

  let mone =
    repr (Zneg Coq_xH)

  (** val iwordsize : int **)

  let iwordsize =
    repr zwordsize

  (** val eq_dec : int -> int -> sumbool **)

  let eq_dec =
    zeq

  (** val eq : int -> int -> bool **)

  let eq x y =
    match zeq (unsigned x) (unsigned y) with
    | Coq_left -> Coq_true
    | Coq_right -> Coq_false

  (** val lt : int -> int -> bool **)

  let lt x y =
    match zlt (signed x) (signed y) with
    | Coq_left -> Coq_true
    | Coq_right -> Coq_false

  (** val ltu : int -> int -> bool **)

  let ltu x y =
    match zlt (unsigned x) (unsigned y) with
    | Coq_left -> Coq_true
    | Coq_right -> Coq_false

  (** val neg : int -> int **)

  let neg x =
    repr (Z.opp (unsigned x))

  (** val add : int -> int -> int **)

  let add x y =
    repr (Z.add (unsigned x) (unsigned y))

  (** val sub : int -> int -> int **)

  let sub x y =
    repr (Z.sub (unsigned x) (unsigned y))

  (** val mul : int -> int -> int **)

  let mul x y =
    repr (Z.mul (unsigned x) (unsigned y))

  (** val divs : int -> int -> int **)

  let divs x y =
    repr (Z.quot (signed x) (signed y))

  (** val mods : int -> int -> int **)

  let mods x y =
    repr (Z.rem (signed x) (signed y))

  (** val divu : int -> int -> int **)

  let divu x y =
    repr (Z.div (unsigned x) (unsigned y))

  (** val modu : int -> int -> int **)

  let modu x y =
    repr (Z.modulo (unsigned x) (unsigned y))

  (** val coq_and : int -> int -> int **)

  let coq_and x y =
    repr (Z.coq_land (unsigned x) (unsigned y))

  (** val coq_or : int -> int -> int **)

  let coq_or x y =
    repr (Z.coq_lor (unsigned x) (unsigned y))

  (** val xor : int -> int -> int **)

  let xor x y =
    repr (Z.coq_lxor (unsigned x) (unsigned y))

  (** val not : int -> int **)

  let not x =
    xor x mone

  (** val shl : int -> int -> int **)

  let shl x y =
    repr (Z.shiftl (unsigned x) (unsigned y))

  (** val shru : int -> int -> int **)

  let shru x y =
    repr (Z.shiftr (unsigned x) (unsigned y))

  (** val shr : int -> int -> int **)

  let shr x y =
    repr (Z.shiftr (signed x) (unsigned y))

  (** val rol : int -> int -> int **)

  let rol x y =
    let n = Z.modulo (unsigned y) zwordsize in
    repr
      (Z.coq_lor (Z.shiftl (unsigned x) n)
        (Z.shiftr (unsigned x) (Z.sub zwordsize n)))

  (** val ror : int -> int -> int **)

  let ror x y =
    let n = Z.modulo (unsigned y) zwordsize in
    repr
      (Z.coq_lor (Z.shiftr (unsigned x) n)
        (Z.shiftl (unsigned x) (Z.sub zwordsize n)))

  (** val rolm : int -> int -> int -> int **)

  let rolm x a m =
    coq_and (rol x a) m

  (** val shrx : int -> int -> int **)

  let shrx x y =
    divs x (shl one y)

  (** val mulhu : int -> int -> int **)

  let mulhu x y =
    repr (Z.div (Z.mul (unsigned x) (unsigned y)) modulus)

  (** val mulhs : int -> int -> int **)

  let mulhs x y =
    repr (Z.div (Z.mul (signed x) (signed y)) modulus)

  (** val negative : int -> int **)

  let negative x =
    match lt x zero with
    | Coq_true -> one
    | Coq_false -> zero

  (** val add_carry : int -> int -> int -> int **)

  let add_carry x y cin =
    match zlt (Z.add (Z.add (unsigned x) (unsigned y)) (unsigned cin)) modulus with
    | Coq_left -> zero
    | Coq_right -> one

  (** val add_overflow : int -> int -> int -> int **)

  let add_overflow x y cin =
    let s = Z.add (Z.add (signed x) (signed y)) (signed cin) in
    (match match proj_sumbool (zle min_signed s) with
           | Coq_true -> proj_sumbool (zle s max_signed)
           | Coq_false -> Coq_false with
     | Coq_true -> zero
     | Coq_false -> one)

  (** val sub_borrow : int -> int -> int -> int **)

  let sub_borrow x y bin =
    match zlt (Z.sub (Z.sub (unsigned x) (unsigned y)) (unsigned bin)) Z0 with
    | Coq_left -> one
    | Coq_right -> zero

  (** val sub_overflow : int -> int -> int -> int **)

  let sub_overflow x y bin =
    let s = Z.sub (Z.sub (signed x) (signed y)) (signed bin) in
    (match match proj_sumbool (zle min_signed s) with
           | Coq_true -> proj_sumbool (zle s max_signed)
           | Coq_false -> Coq_false with
     | Coq_true -> zero
     | Coq_false -> one)

  (** val shr_carry : int -> int -> int **)

  let shr_carry x y =
    match match lt x zero with
          | Coq_true -> negb (eq (coq_and x (sub (shl one y) one)) zero)
          | Coq_false -> Coq_false with
    | Coq_true -> one
    | Coq_false -> zero

  (** val coq_Zshiftin : bool -> coq_Z -> coq_Z **)

  let coq_Zshiftin b x =
    match b with
    | Coq_true -> Z.succ_double x
    | Coq_false -> Z.double x

  (** val coq_Zzero_ext : coq_Z -> coq_Z -> coq_Z **)

  let coq_Zzero_ext n x =
    Z.iter n (fun rec0 x0 -> coq_Zshiftin (Z.odd x0) (rec0 (Z.div2 x0)))
      (fun _ -> Z0) x

  (** val coq_Zsign_ext : coq_Z -> coq_Z -> coq_Z **)

  let coq_Zsign_ext n x =
    Z.iter (Z.pred n) (fun rec0 x0 ->
      coq_Zshiftin (Z.odd x0) (rec0 (Z.div2 x0))) (fun x0 ->
      match Z.odd x0 with
      | Coq_true -> Zneg Coq_xH
      | Coq_false -> Z0) x

  (** val zero_ext : coq_Z -> int -> int **)

  let zero_ext n x =
    repr (coq_Zzero_ext n (unsigned x))

  (** val sign_ext : coq_Z -> int -> int **)

  let sign_ext n x =
    repr (coq_Zsign_ext n (unsigned x))

  (** val coq_Z_one_bits : nat -> coq_Z -> coq_Z -> coq_Z list **)

  let rec coq_Z_one_bits n x i =
    match n with
    | O -> Coq_nil
    | S m ->
      (match Z.odd x with
       | Coq_true ->
         Coq_cons (i, (coq_Z_one_bits m (Z.div2 x) (Z.add i (Zpos Coq_xH))))
       | Coq_false -> coq_Z_one_bits m (Z.div2 x) (Z.add i (Zpos Coq_xH)))

  (** val one_bits : int -> int list **)

  let one_bits x =
    map repr (coq_Z_one_bits wordsize (unsigned x) Z0)

  (** val is_power2 : int -> int option **)

  let is_power2 x =
    match coq_Z_one_bits wordsize (unsigned x) Z0 with
    | Coq_nil -> None
    | Coq_cons (i, l) ->
      (match l with
       | Coq_nil -> Some (repr i)
       | Coq_cons (_, _) -> None)

  (** val cmp : comparison -> int -> int -> bool **)

  let cmp c x y =
    match c with
    | Ceq -> eq x y
    | Cne -> negb (eq x y)
    | Clt -> lt x y
    | Cle -> negb (lt y x)
    | Cgt -> lt y x
    | Cge -> negb (lt x y)

  (** val cmpu : comparison -> int -> int -> bool **)

  let cmpu c x y =
    match c with
    | Ceq -> eq x y
    | Cne -> negb (eq x y)
    | Clt -> ltu x y
    | Cle -> negb (ltu y x)
    | Cgt -> ltu y x
    | Cge -> negb (ltu x y)

  (** val notbool : int -> int **)

  let notbool x =
    match eq x zero with
    | Coq_true -> one
    | Coq_false -> zero

  (** val divmodu2 : int -> int -> int -> (int, int) prod option **)

  let divmodu2 nhi nlo d =
    match eq_dec d zero with
    | Coq_left -> None
    | Coq_right ->
      let Coq_pair (q, r) =
        Z.div_eucl (Z.add (Z.mul (unsigned nhi) modulus) (unsigned nlo))
          (unsigned d)
      in
      (match zle q max_unsigned with
       | Coq_left -> Some (Coq_pair ((repr q), (repr r)))
       | Coq_right -> None)

  (** val divmods2 : int -> int -> int -> (int, int) prod option **)

  let divmods2 nhi nlo d =
    match eq_dec d zero with
    | Coq_left -> None
    | Coq_right ->
      let Coq_pair (q, r) =
        Z.quotrem (Z.add (Z.mul (signed nhi) modulus) (unsigned nlo))
          (signed d)
      in
      (match match proj_sumbool (zle min_signed q) with
             | Coq_true -> proj_sumbool (zle q max_signed)
             | Coq_false -> Coq_false with
       | Coq_true -> Some (Coq_pair ((repr q), (repr r)))
       | Coq_false -> None)

  (** val testbit : int -> coq_Z -> bool **)

  let testbit x i =
    Z.testbit (unsigned x) i

  (** val powerserie : coq_Z list -> coq_Z **)

  let rec powerserie = function
  | Coq_nil -> Z0
  | Coq_cons (x, xs) -> Z.add (two_p x) (powerserie xs)

  (** val int_of_one_bits : int list -> int **)

  let rec int_of_one_bits = function
  | Coq_nil -> zero
  | Coq_cons (a, b) -> add (shl one a) (int_of_one_bits b)

  (** val no_overlap : int -> coq_Z -> int -> coq_Z -> bool **)

  let no_overlap ofs1 sz1 ofs2 sz2 =
    let x1 = unsigned ofs1 in
    let x2 = unsigned ofs2 in
    (match match proj_sumbool (zlt (Z.add x1 sz1) modulus) with
           | Coq_true -> proj_sumbool (zlt (Z.add x2 sz2) modulus)
           | Coq_false -> Coq_false with
     | Coq_true ->
       (match proj_sumbool (zle (Z.add x1 sz1) x2) with
        | Coq_true -> Coq_true
        | Coq_false -> proj_sumbool (zle (Z.add x2 sz2) x1))
     | Coq_false -> Coq_false)

  (** val coq_Zsize : coq_Z -> coq_Z **)

  let coq_Zsize = function
  | Zpos p -> Zpos (Pos.size p)
  | _ -> Z0

  (** val size : int -> coq_Z **)

  let size x =
    coq_Zsize (unsigned x)
 end

module Wordsize_32 =
 struct
  (** val wordsize : nat **)

  let wordsize =
    S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
      (S (S (S (S (S (S (S O)))))))))))))))))))))))))))))))
 end

module Int = Make(Wordsize_32)

module Wordsize_256 =
 struct
  (** val wordsize : nat **)

  let wordsize =
    S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
      (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
      (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
      (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
      (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
      (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
      (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
      (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
      (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
      (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
      (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
      O)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 end

module Int256 = Make(Wordsize_256)
