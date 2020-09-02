open BinNums
open BinPos
open Datatypes

module N =
 struct
  (** val succ_double : coq_N -> coq_N **)

  let succ_double = function
  | N0 -> Npos Coq_xH
  | Npos p -> Npos (Coq_xI p)

  (** val double : coq_N -> coq_N **)

  let double = function
  | N0 -> N0
  | Npos p -> Npos (Coq_xO p)

  (** val succ_pos : coq_N -> positive **)

  let succ_pos = function
  | N0 -> Coq_xH
  | Npos p -> Pos.succ p

  (** val sub : coq_N -> coq_N -> coq_N **)

  let sub n m =
    match n with
    | N0 -> N0
    | Npos n' ->
      (match m with
       | N0 -> n
       | Npos m' ->
         (match Pos.sub_mask n' m' with
          | Pos.IsPos p -> Npos p
          | _ -> N0))

  (** val compare : coq_N -> coq_N -> comparison **)

  let compare n m =
    match n with
    | N0 -> (match m with
             | N0 -> Eq
             | Npos _ -> Lt)
    | Npos n' -> (match m with
                  | N0 -> Gt
                  | Npos m' -> Pos.compare n' m')

  (** val leb : coq_N -> coq_N -> bool **)

  let leb x y =
    match compare x y with
    | Gt -> Coq_false
    | _ -> Coq_true

  (** val pos_div_eucl : positive -> coq_N -> (coq_N, coq_N) prod **)

  let rec pos_div_eucl a b =
    match a with
    | Coq_xI a' ->
      let Coq_pair (q, r) = pos_div_eucl a' b in
      let r' = succ_double r in
      (match leb b r' with
       | Coq_true -> Coq_pair ((succ_double q), (sub r' b))
       | Coq_false -> Coq_pair ((double q), r'))
    | Coq_xO a' ->
      let Coq_pair (q, r) = pos_div_eucl a' b in
      let r' = double r in
      (match leb b r' with
       | Coq_true -> Coq_pair ((succ_double q), (sub r' b))
       | Coq_false -> Coq_pair ((double q), r'))
    | Coq_xH ->
      (match b with
       | N0 -> Coq_pair (N0, (Npos Coq_xH))
       | Npos p ->
         (match p with
          | Coq_xH -> Coq_pair ((Npos Coq_xH), N0)
          | _ -> Coq_pair (N0, (Npos Coq_xH))))

  (** val coq_lor : coq_N -> coq_N -> coq_N **)

  let coq_lor n m =
    match n with
    | N0 -> m
    | Npos p -> (match m with
                 | N0 -> n
                 | Npos q -> Npos (Pos.coq_lor p q))

  (** val coq_land : coq_N -> coq_N -> coq_N **)

  let coq_land n m =
    match n with
    | N0 -> N0
    | Npos p -> (match m with
                 | N0 -> N0
                 | Npos q -> Pos.coq_land p q)

  (** val ldiff : coq_N -> coq_N -> coq_N **)

  let rec ldiff n m =
    match n with
    | N0 -> N0
    | Npos p -> (match m with
                 | N0 -> n
                 | Npos q -> Pos.ldiff p q)

  (** val coq_lxor : coq_N -> coq_N -> coq_N **)

  let coq_lxor n m =
    match n with
    | N0 -> m
    | Npos p -> (match m with
                 | N0 -> n
                 | Npos q -> Pos.coq_lxor p q)

  (** val testbit : coq_N -> coq_N -> bool **)

  let testbit a n =
    match a with
    | N0 -> Coq_false
    | Npos p -> Pos.testbit p n
 end
