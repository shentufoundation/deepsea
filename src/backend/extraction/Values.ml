open AST
open BinNums
open Integers
open Specif

type ident_ext =
| Iident of ident
| Ihash of ident_ext * Int256.int

module IdentExtIndexed =
 struct
  (** val eq : ident_ext -> ident_ext -> sumbool **)

  let eq x y =
    match x with
    | Iident i ->
      let rec f i0 x0 =
        match i0 with
        | Iident i1 ->
          (match x0 with
           | Iident i2 ->
             let rec f0 p x1 =
               match p with
               | Coq_xI p0 ->
                 (match x1 with
                  | Coq_xI p1 -> f0 p0 p1
                  | _ -> Coq_right)
               | Coq_xO p0 ->
                 (match x1 with
                  | Coq_xO p1 -> f0 p0 p1
                  | _ -> Coq_right)
               | Coq_xH -> (match x1 with
                            | Coq_xH -> Coq_left
                            | _ -> Coq_right)
             in f0 i1 i2
           | Ihash (_, _) -> Coq_right)
        | Ihash (i1, i2) ->
          (match x0 with
           | Iident _ -> Coq_right
           | Ihash (i3, i4) ->
             (match f i1 i3 with
              | Coq_left -> Int256.eq_dec i2 i4
              | Coq_right -> Coq_right))
      in f (Iident i) y
    | Ihash (x0, i) ->
      let rec f i0 x1 =
        match i0 with
        | Iident i1 ->
          (match x1 with
           | Iident i2 ->
             let rec f0 p x2 =
               match p with
               | Coq_xI p0 ->
                 (match x2 with
                  | Coq_xI p1 -> f0 p0 p1
                  | _ -> Coq_right)
               | Coq_xO p0 ->
                 (match x2 with
                  | Coq_xO p1 -> f0 p0 p1
                  | _ -> Coq_right)
               | Coq_xH -> (match x2 with
                            | Coq_xH -> Coq_left
                            | _ -> Coq_right)
             in f0 i1 i2
           | Ihash (_, _) -> Coq_right)
        | Ihash (i1, i2) ->
          (match x1 with
           | Iident _ -> Coq_right
           | Ihash (i3, i4) ->
             (match f i1 i3 with
              | Coq_left -> Int256.eq_dec i2 i4
              | Coq_right -> Coq_right))
      in f (Ihash (x0, i)) y
 end

type coq_val =
| Vunit
| Vint of Int256.int
| Vptr of ident_ext
| Vhash of coq_val
| Vhash2 of coq_val * coq_val
