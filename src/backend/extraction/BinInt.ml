open BinNat
open BinNums
open BinPos
open Datatypes
open Specif

module Z =
 struct
  (** val double : coq_Z -> coq_Z **)

  let double = function
  | Z0 -> Z0
  | Zpos p -> Zpos (Coq_xO p)
  | Zneg p -> Zneg (Coq_xO p)

  (** val succ_double : coq_Z -> coq_Z **)

  let succ_double = function
  | Z0 -> Zpos Coq_xH
  | Zpos p -> Zpos (Coq_xI p)
  | Zneg p -> Zneg (Pos.pred_double p)

  (** val pred_double : coq_Z -> coq_Z **)

  let pred_double = function
  | Z0 -> Zneg Coq_xH
  | Zpos p -> Zpos (Pos.pred_double p)
  | Zneg p -> Zneg (Coq_xI p)

  (** val pos_sub : positive -> positive -> coq_Z **)

  let rec pos_sub x y =
    match x with
    | Coq_xI p ->
      (match y with
       | Coq_xI q -> double (pos_sub p q)
       | Coq_xO q -> succ_double (pos_sub p q)
       | Coq_xH -> Zpos (Coq_xO p))
    | Coq_xO p ->
      (match y with
       | Coq_xI q -> pred_double (pos_sub p q)
       | Coq_xO q -> double (pos_sub p q)
       | Coq_xH -> Zpos (Pos.pred_double p))
    | Coq_xH ->
      (match y with
       | Coq_xI q -> Zneg (Coq_xO q)
       | Coq_xO q -> Zneg (Pos.pred_double q)
       | Coq_xH -> Z0)

  (** val add : coq_Z -> coq_Z -> coq_Z **)

  let add x y =
    match x with
    | Z0 -> y
    | Zpos x' ->
      (match y with
       | Z0 -> x
       | Zpos y' -> Zpos (Pos.add x' y')
       | Zneg y' -> pos_sub x' y')
    | Zneg x' ->
      (match y with
       | Z0 -> x
       | Zpos y' -> pos_sub y' x'
       | Zneg y' -> Zneg (Pos.add x' y'))

  (** val opp : coq_Z -> coq_Z **)

  let opp = function
  | Z0 -> Z0
  | Zpos x0 -> Zneg x0
  | Zneg x0 -> Zpos x0

  (** val pred : coq_Z -> coq_Z **)

  let pred x =
    add x (Zneg Coq_xH)

  (** val sub : coq_Z -> coq_Z -> coq_Z **)

  let sub m n =
    add m (opp n)

  (** val mul : coq_Z -> coq_Z -> coq_Z **)

  let mul x y =
    match x with
    | Z0 -> Z0
    | Zpos x' ->
      (match y with
       | Z0 -> Z0
       | Zpos y' -> Zpos (Pos.mul x' y')
       | Zneg y' -> Zneg (Pos.mul x' y'))
    | Zneg x' ->
      (match y with
       | Z0 -> Z0
       | Zpos y' -> Zneg (Pos.mul x' y')
       | Zneg y' -> Zpos (Pos.mul x' y'))

  (** val compare : coq_Z -> coq_Z -> comparison **)

  let compare x y =
    match x with
    | Z0 -> (match y with
             | Z0 -> Eq
             | Zpos _ -> Lt
             | Zneg _ -> Gt)
    | Zpos x' -> (match y with
                  | Zpos y' -> Pos.compare x' y'
                  | _ -> Gt)
    | Zneg x' ->
      (match y with
       | Zneg y' -> coq_CompOpp (Pos.compare x' y')
       | _ -> Lt)

  (** val leb : coq_Z -> coq_Z -> bool **)

  let leb x y =
    match compare x y with
    | Gt -> Coq_false
    | _ -> Coq_true

  (** val ltb : coq_Z -> coq_Z -> bool **)

  let ltb x y =
    match compare x y with
    | Lt -> Coq_true
    | _ -> Coq_false

  (** val max : coq_Z -> coq_Z -> coq_Z **)

  let max n m =
    match compare n m with
    | Lt -> m
    | _ -> n

  (** val of_nat : nat -> coq_Z **)

  let of_nat = function
  | O -> Z0
  | S n0 -> Zpos (Pos.of_succ_nat n0)

  (** val of_N : coq_N -> coq_Z **)

  let of_N = function
  | N0 -> Z0
  | Npos p -> Zpos p

  (** val iter : coq_Z -> ('a1 -> 'a1) -> 'a1 -> 'a1 **)

  let iter n f x =
    match n with
    | Zpos p -> Pos.iter f x p
    | _ -> x

  (** val pos_div_eucl : positive -> coq_Z -> (coq_Z, coq_Z) prod **)

  let rec pos_div_eucl a b =
    match a with
    | Coq_xI a' ->
      let Coq_pair (q, r) = pos_div_eucl a' b in
      let r' = add (mul (Zpos (Coq_xO Coq_xH)) r) (Zpos Coq_xH) in
      (match ltb r' b with
       | Coq_true -> Coq_pair ((mul (Zpos (Coq_xO Coq_xH)) q), r')
       | Coq_false ->
         Coq_pair ((add (mul (Zpos (Coq_xO Coq_xH)) q) (Zpos Coq_xH)),
           (sub r' b)))
    | Coq_xO a' ->
      let Coq_pair (q, r) = pos_div_eucl a' b in
      let r' = mul (Zpos (Coq_xO Coq_xH)) r in
      (match ltb r' b with
       | Coq_true -> Coq_pair ((mul (Zpos (Coq_xO Coq_xH)) q), r')
       | Coq_false ->
         Coq_pair ((add (mul (Zpos (Coq_xO Coq_xH)) q) (Zpos Coq_xH)),
           (sub r' b)))
    | Coq_xH ->
      (match leb (Zpos (Coq_xO Coq_xH)) b with
       | Coq_true -> Coq_pair (Z0, (Zpos Coq_xH))
       | Coq_false -> Coq_pair ((Zpos Coq_xH), Z0))

  (** val div_eucl : coq_Z -> coq_Z -> (coq_Z, coq_Z) prod **)

  let div_eucl a b =
    match a with
    | Z0 -> Coq_pair (Z0, Z0)
    | Zpos a' ->
      (match b with
       | Z0 -> Coq_pair (Z0, Z0)
       | Zpos _ -> pos_div_eucl a' b
       | Zneg b' ->
         let Coq_pair (q, r) = pos_div_eucl a' (Zpos b') in
         (match r with
          | Z0 -> Coq_pair ((opp q), Z0)
          | _ -> Coq_pair ((opp (add q (Zpos Coq_xH))), (add b r))))
    | Zneg a' ->
      (match b with
       | Z0 -> Coq_pair (Z0, Z0)
       | Zpos _ ->
         let Coq_pair (q, r) = pos_div_eucl a' b in
         (match r with
          | Z0 -> Coq_pair ((opp q), Z0)
          | _ -> Coq_pair ((opp (add q (Zpos Coq_xH))), (sub b r)))
       | Zneg b' ->
         let Coq_pair (q, r) = pos_div_eucl a' (Zpos b') in
         Coq_pair (q, (opp r)))

  (** val div : coq_Z -> coq_Z -> coq_Z **)

  let div a b =
    let Coq_pair (q, _) = div_eucl a b in q

  (** val modulo : coq_Z -> coq_Z -> coq_Z **)

  let modulo a b =
    let Coq_pair (_, r) = div_eucl a b in r

  (** val quotrem : coq_Z -> coq_Z -> (coq_Z, coq_Z) prod **)

  let quotrem a b =
    match a with
    | Z0 -> Coq_pair (Z0, Z0)
    | Zpos a0 ->
      (match b with
       | Z0 -> Coq_pair (Z0, a)
       | Zpos b0 ->
         let Coq_pair (q, r) = N.pos_div_eucl a0 (Npos b0) in
         Coq_pair ((of_N q), (of_N r))
       | Zneg b0 ->
         let Coq_pair (q, r) = N.pos_div_eucl a0 (Npos b0) in
         Coq_pair ((opp (of_N q)), (of_N r)))
    | Zneg a0 ->
      (match b with
       | Z0 -> Coq_pair (Z0, a)
       | Zpos b0 ->
         let Coq_pair (q, r) = N.pos_div_eucl a0 (Npos b0) in
         Coq_pair ((opp (of_N q)), (opp (of_N r)))
       | Zneg b0 ->
         let Coq_pair (q, r) = N.pos_div_eucl a0 (Npos b0) in
         Coq_pair ((of_N q), (opp (of_N r))))

  (** val quot : coq_Z -> coq_Z -> coq_Z **)

  let quot a b =
    fst (quotrem a b)

  (** val rem : coq_Z -> coq_Z -> coq_Z **)

  let rem a b =
    snd (quotrem a b)

  (** val odd : coq_Z -> bool **)

  let odd = function
  | Z0 -> Coq_false
  | Zpos p -> (match p with
               | Coq_xO _ -> Coq_false
               | _ -> Coq_true)
  | Zneg p -> (match p with
               | Coq_xO _ -> Coq_false
               | _ -> Coq_true)

  (** val div2 : coq_Z -> coq_Z **)

  let div2 = function
  | Z0 -> Z0
  | Zpos p -> (match p with
               | Coq_xH -> Z0
               | _ -> Zpos (Pos.div2 p))
  | Zneg p -> Zneg (Pos.div2_up p)

  (** val testbit : coq_Z -> coq_Z -> bool **)

  let testbit a = function
  | Z0 -> odd a
  | Zpos p ->
    (match a with
     | Z0 -> Coq_false
     | Zpos a0 -> Pos.testbit a0 (Npos p)
     | Zneg a0 -> negb (N.testbit (Pos.pred_N a0) (Npos p)))
  | Zneg _ -> Coq_false

  (** val shiftl : coq_Z -> coq_Z -> coq_Z **)

  let shiftl a = function
  | Z0 -> a
  | Zpos p -> Pos.iter (mul (Zpos (Coq_xO Coq_xH))) a p
  | Zneg p -> Pos.iter div2 a p

  (** val shiftr : coq_Z -> coq_Z -> coq_Z **)

  let shiftr a n =
    shiftl a (opp n)

  (** val coq_lor : coq_Z -> coq_Z -> coq_Z **)

  let coq_lor a b =
    match a with
    | Z0 -> b
    | Zpos a0 ->
      (match b with
       | Z0 -> a
       | Zpos b0 -> Zpos (Pos.coq_lor a0 b0)
       | Zneg b0 -> Zneg (N.succ_pos (N.ldiff (Pos.pred_N b0) (Npos a0))))
    | Zneg a0 ->
      (match b with
       | Z0 -> a
       | Zpos b0 -> Zneg (N.succ_pos (N.ldiff (Pos.pred_N a0) (Npos b0)))
       | Zneg b0 ->
         Zneg (N.succ_pos (N.coq_land (Pos.pred_N a0) (Pos.pred_N b0))))

  (** val coq_land : coq_Z -> coq_Z -> coq_Z **)

  let coq_land a b =
    match a with
    | Z0 -> Z0
    | Zpos a0 ->
      (match b with
       | Z0 -> Z0
       | Zpos b0 -> of_N (Pos.coq_land a0 b0)
       | Zneg b0 -> of_N (N.ldiff (Npos a0) (Pos.pred_N b0)))
    | Zneg a0 ->
      (match b with
       | Z0 -> Z0
       | Zpos b0 -> of_N (N.ldiff (Npos b0) (Pos.pred_N a0))
       | Zneg b0 ->
         Zneg (N.succ_pos (N.coq_lor (Pos.pred_N a0) (Pos.pred_N b0))))

  (** val coq_lxor : coq_Z -> coq_Z -> coq_Z **)

  let coq_lxor a b =
    match a with
    | Z0 -> b
    | Zpos a0 ->
      (match b with
       | Z0 -> a
       | Zpos b0 -> of_N (Pos.coq_lxor a0 b0)
       | Zneg b0 -> Zneg (N.succ_pos (N.coq_lxor (Npos a0) (Pos.pred_N b0))))
    | Zneg a0 ->
      (match b with
       | Z0 -> a
       | Zpos b0 -> Zneg (N.succ_pos (N.coq_lxor (Pos.pred_N a0) (Npos b0)))
       | Zneg b0 -> of_N (N.coq_lxor (Pos.pred_N a0) (Pos.pred_N b0)))

  (** val eq_dec : coq_Z -> coq_Z -> sumbool **)

  let eq_dec x y =
    match x with
    | Z0 -> (match y with
             | Z0 -> Coq_left
             | _ -> Coq_right)
    | Zpos x0 -> (match y with
                  | Zpos p0 -> Pos.eq_dec x0 p0
                  | _ -> Coq_right)
    | Zneg x0 -> (match y with
                  | Zneg p0 -> Pos.eq_dec x0 p0
                  | _ -> Coq_right)
 end
