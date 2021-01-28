open AST
open Ascii
open BinNums
open BinPos
open Datatypes
open ExpCintptr
open Globalenvs
open List0
open Maps0
open Monad
open Nat0
open OptErrMonad
open PeanoNat
open StmtCGraph
open String0
open Trees

type set = bool PTree.t

type coq_LVDomain = set

(** val set_empty : set **)

let set_empty =
  PTree.empty

(** val set_union' : positive list -> set -> set -> set -> set **)

let rec set_union' keys x y r =
  match keys with
  | Coq_nil -> r
  | Coq_cons (hd, rest) ->
    set_union' rest x y
      (PTree.set hd
        (match PTree.get_default Coq_false hd x with
         | Coq_true -> Coq_true
         | Coq_false -> PTree.get_default Coq_false hd y) r)

(** val set_union : set -> set -> set **)

let set_union x y =
  set_union' (app (PTree.xkeys x Coq_xH) (PTree.xkeys y Coq_xH)) x y
    PTree.empty

(** val set_minus' : positive list -> set -> set -> set -> set **)

let rec set_minus' keys x y r =
  match keys with
  | Coq_nil -> r
  | Coq_cons (hd, rest) ->
    set_minus' rest x y
      (PTree.set hd
        (match PTree.get_default Coq_false hd x with
         | Coq_true -> negb (PTree.get_default Coq_false hd y)
         | Coq_false -> Coq_false) r)

(** val set_minus : bool PTree.t -> set -> set **)

let set_minus x y =
  set_minus' (PTree.xkeys x Coq_xH) x y PTree.empty

(** val set_add : ident -> set -> set **)

let rec set_add i r =
  PTree.set i Coq_true r

(** val set_in : ident -> set -> bool **)

let rec set_in i r =
  PTree.get_default Coq_false i r

(** val set_card : set -> nat **)

let rec set_card x =
  PTree.fold1 (fun acc e ->
    match e with
    | Coq_true -> add acc (S O)
    | Coq_false -> acc) x O

(** val set_eq : set -> set -> bool **)

let rec set_eq x y =
  match Nat.eqb (set_card (set_minus x y)) O with
  | Coq_true -> Nat.eqb (set_card (set_minus y x)) O
  | Coq_false -> Coq_false

(** val set_fold_left : ('a1 -> ident -> 'a1) -> set -> 'a1 -> 'a1 **)

let set_fold_left f m v =
  PTree.fold (fun acc i e ->
    match e with
    | Coq_true -> f acc i
    | Coq_false -> acc) m v

(** val get_use_expr : expr -> coq_LVDomain **)

let rec get_use_expr = function
| Etempvar (i, _) -> set_add i set_empty
| Esload (e0, _) -> get_use_expr e0
| Emload (e0, _) -> get_use_expr e0
| Eaddr (e0, _) -> get_use_expr e0
| Eunop (_, e0, _) -> get_use_expr e0
| Ebinop (_, e1, e2, _) -> set_union (get_use_expr e1) (get_use_expr e2)
| Ecall1 (_, e0, _) -> get_use_expr e0
| _ -> set_empty

(** val get_use_exprs : expr list -> coq_LVDomain **)

let rec get_use_exprs = function
| Coq_nil -> set_empty
| Coq_cons (e, rest) -> set_union (get_use_expr e) (get_use_exprs rest)

(** val get_use : statement -> coq_LVDomain **)

let get_use = function
| Smassign (lv, rv, _) -> set_union (get_use_expr lv) (get_use_expr rv)
| Ssassign (lv, rv, _) -> set_union (get_use_expr lv) (get_use_expr rv)
| Sset (_, rv, _) -> get_use_expr rv
| Scall (_, _, args, _) -> get_use_exprs args
| Scond (cond, _, _) -> get_use_expr cond
| Sreturn (val0, _) ->
  (match val0 with
   | Some e -> set_add e set_empty
   | None -> set_empty)
| Shash (e1, e2, eo, _) ->
  let e1' = get_use_expr e1 in
  let e2' = get_use_expr e2 in
  (match eo with
   | Some eoo ->
     let eoo' = get_use_expr eoo in set_union (set_union e1' e2') eoo'
   | None -> set_union e1' e2')
| Stransfer (a, v, _, _) -> set_union (get_use_expr a) (get_use_expr v)
| Scallmethod (a, _, _, v, _, args, _, _) ->
  set_union (set_union (get_use_expr a) (get_use_expr v)) (get_use_exprs args)
| Slog (topics, args, _) ->
  set_union (get_use_exprs topics) (get_use_exprs args)
| _ -> set_empty

(** val get_successor : statement -> node list **)

let get_successor = function
| Sskip n -> Coq_cons (n, Coq_nil)
| Smassign (_, _, n) -> Coq_cons (n, Coq_nil)
| Ssassign (_, _, n) -> Coq_cons (n, Coq_nil)
| Sset (_, _, n) -> Coq_cons (n, Coq_nil)
| Scall (_, _, _, n) -> Coq_cons (n, Coq_nil)
| Scond (_, ntrue, nfalse) -> Coq_cons (ntrue, (Coq_cons (nfalse, Coq_nil)))
| Sreturn (_, n) -> Coq_cons (n, Coq_nil)
| Shash (_, _, _, n) -> Coq_cons (n, Coq_nil)
| Stransfer (_, _, nfail, n) -> Coq_cons (nfail, (Coq_cons (n, Coq_nil)))
| Scallmethod (_, _, _, _, _, _, nfail, n) ->
  Coq_cons (nfail, (Coq_cons (n, Coq_nil)))
| Slog (_, _, n) -> Coq_cons (n, Coq_nil)
| _ -> Coq_nil

(** val get_def : statement -> coq_LVDomain **)

let get_def = function
| Sset (id, _, _) -> set_add id set_empty
| Scall (retval, _, _, _) ->
  (match retval with
   | Some rv -> set_add rv set_empty
   | None -> set_empty)
| Scallmethod (_, rvs, _, _, _, _, _, _) ->
  fold_left (fun acc e -> set_add e acc) rvs set_empty
| _ -> set_empty

type coq_CD = coq_LVDomain PTree.t

(** val f_b : code -> node -> coq_CD -> coq_LVDomain **)

let f_b cfg block cd =
  match PTree.get block cfg with
  | Some s ->
    let oUT =
      fold_left (fun acc e ->
        set_union acc (PTree.get_default set_empty e cd)) (get_successor s)
        set_empty
    in
    set_union (get_use s) (set_minus oUT (get_def s))
  | None -> set_empty

(** val update_at : code -> coq_CD -> node -> coq_CD **)

let update_at cfg cd b =
  PTree.set b (f_b cfg b cd) cd

(** val lv_top : code -> coq_LVDomain **)

let lv_top cfg =
  PTree.fold (fun s _ stmt -> set_union s (get_use stmt)) cfg set_empty

(** val cd_top : code -> coq_CD **)

let cd_top cfg =
  PTree.map1 (fun _ -> lv_top cfg) cfg

(** val get_lv_once : code -> node list -> coq_CD -> coq_CD **)

let get_lv_once cfg nodes cd =
  fold_left (update_at cfg) nodes cd

(** val get_lv_mfp : code -> node list -> nat -> coq_CD -> coq_CD **)

let rec get_lv_mfp cfg blocks iteration cd =
  match iteration with
  | O -> cd
  | S x ->
    let new_cd = get_lv_once cfg blocks cd in
    (match PTree.beq set_eq cd new_cd with
     | Coq_true -> new_cd
     | Coq_false -> get_lv_mfp cfg blocks x new_cd)

(** val compute_livevar : code -> nat -> coq_CD optErr **)

let compute_livevar cfg precision =
  let blocks = map fst (PTree.elements cfg) in
  ret (Obj.magic coq_Monad_optErr)
    (get_lv_mfp cfg blocks precision (cd_top cfg))

type tvs = set

type clashg = tvs PTree.t

(** val create_clash_graph : coq_CD -> clashg optErr **)

let rec create_clash_graph lv =
  ret (Obj.magic coq_Monad_optErr)
    (PTree.fold1 (fun acc e ->
      set_fold_left (fun acc' e' ->
        PTree.set e' (set_union (PTree.get_default set_empty e' acc') e) acc')
        e acc) lv PTree.empty)

(** val remove_node_graph : ident -> clashg -> clashg **)

let rec remove_node_graph t0 g =
  let g' = PTree.remove t0 g in
  PTree.fold (fun acc i e ->
    PTree.set i (set_minus e (set_add t0 set_empty)) acc) g' g'

(** val least_degree : clashg -> ident **)

let rec least_degree g =
  let Coq_pair (id, _) =
    PTree.fold (fun acc i e ->
      let Coq_pair (_, mn) = acc in
      let c = set_card e in
      (match Nat.leb c mn with
       | Coq_true -> Coq_pair (i, c)
       | Coq_false -> acc)) g (Coq_pair ((Pos.of_nat O), (S (S (S (S (S (S (S
      (S (S (S (S (S (S (S (S (S O))))))))))))))))))
  in
  id

(** val pop_graph : nat -> clashg -> ident list -> ident list optErr **)

let rec pop_graph i g stack =
  match i with
  | O ->
    (match Nat.eqb (length (PTree.elements g)) O with
     | Coq_true -> ret (Obj.magic coq_Monad_optErr) stack
     | Coq_false ->
       Error (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
         Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
         (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
         Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
         Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
         (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
         ((Ascii (Coq_true, Coq_true, Coq_true, Coq_false, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
         ((Ascii (Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
         Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
         Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
         Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true,
         Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
         Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
         Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
         ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
         Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
         Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
         Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
         ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
         Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
         Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_true,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
         Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
         ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_true, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
         ((Ascii (Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
         Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_false,
         Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
         Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
         Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_false,
         Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
         Coq_true, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
         Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
         Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
         Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
         EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
  | S n ->
    let ldn = least_degree g in
    pop_graph n (remove_node_graph ldn g) (Coq_cons (ldn, stack))

type colorMap = nat PTree.t

(** val get_assigned_colors : colorMap -> tvs -> set **)

let get_assigned_colors c s =
  PTree.fold (fun acc i e ->
    match e with
    | Coq_true ->
      (match PTree.get i c with
       | Some cl -> set_add (Pos.of_nat cl) acc
       | None -> acc)
    | Coq_false -> acc) s set_empty

(** val colorMapFull : set **)

let colorMapFull =
  set_add (Pos.of_nat (S O))
    (set_add (Pos.of_nat (S (S O)))
      (set_add (Pos.of_nat (S (S (S O))))
        (set_add (Pos.of_nat (S (S (S (S O)))))
          (set_add (Pos.of_nat (S (S (S (S (S O))))))
            (set_add (Pos.of_nat (S (S (S (S (S (S O)))))))
              (set_add (Pos.of_nat (S (S (S (S (S (S (S O))))))))
                (set_add (Pos.of_nat (S (S (S (S (S (S (S (S O)))))))))
                  (set_add (Pos.of_nat (S (S (S (S (S (S (S (S (S O))))))))))
                    (set_add
                      (Pos.of_nat (S (S (S (S (S (S (S (S (S (S O)))))))))))
                      (set_add
                        (Pos.of_nat (S (S (S (S (S (S (S (S (S (S (S
                          O))))))))))))
                        (set_add
                          (Pos.of_nat (S (S (S (S (S (S (S (S (S (S (S (S
                            O)))))))))))))
                          (set_add
                            (Pos.of_nat (S (S (S (S (S (S (S (S (S (S (S (S
                              (S O))))))))))))))
                            (set_add
                              (Pos.of_nat (S (S (S (S (S (S (S (S (S (S (S (S
                                (S (S O)))))))))))))))
                              (set_add
                                (Pos.of_nat (S (S (S (S (S (S (S (S (S (S (S
                                  (S (S (S (S O)))))))))))))))) set_empty))))))))))))))

(** val max_reg : nat **)

let max_reg =
  S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S O)))))))))))))))

(** val default_reg : nat **)

let default_reg =
  S O

(** val assign_color : ident list -> clashg -> colorMap -> colorMap optErr **)

let rec assign_color stack g c =
  match stack with
  | Coq_nil -> ret (Obj.magic coq_Monad_optErr) c
  | Coq_cons (x, xs) ->
    (match PTree.get x g with
     | Some lk ->
       let assignable = set_minus colorMapFull (get_assigned_colors c lk) in
       (match Nat.ltb O (set_card assignable) with
        | Coq_true ->
          (match PTree.fold (fun acc i e ->
                   match e with
                   | Coq_true -> Some i
                   | Coq_false -> acc) assignable None with
           | Some reg -> assign_color xs g (PTree.set x (Pos.to_nat reg) c)
           | None ->
             Error (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true,
               Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
               (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
               Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
               Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
               Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
               Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
               ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true,
               Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
               Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
               Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
               Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
               ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
               Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
               Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
               Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
               Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
               Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
               Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
               ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
               Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
               Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
               Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
               Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
               Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
               Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
               ((Ascii (Coq_true, Coq_true, Coq_true, Coq_false, Coq_false,
               Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
               Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
               Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
               Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
               ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
               Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
               Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
               Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_false,
               Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
               ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true,
               Coq_true, Coq_true, Coq_false)),
               EmptyString)))))))))))))))))))))))))))))))))))))))))))))
        | Coq_false ->
          Error (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_true,
            Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
            (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
            Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
            Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
            (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
            Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
            (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
            Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
            Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
            (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
            Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
            (Coq_true, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
            Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
            Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
            (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
            Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
            (Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
            Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
            Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
            (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false,
            Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
            (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
            Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
            Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
            (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false,
            Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
            (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
            Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
            Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
            (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
            Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
            (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
            Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
            Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
            (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true,
            Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
            (Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
            Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
            Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
            (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
            Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
            (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
            Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
            Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
            (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
            Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
            (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
            Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
            Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
            (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_true,
            Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
            (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
            Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
            Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
            Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
            Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
            ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
            Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
            Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
            Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
            Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
            ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
            Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_false,
            Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
            Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
            Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
            ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
            Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
            Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
            Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
            Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
            ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
            Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
            Coq_true, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
            Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
            Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
            ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_true,
            Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
            Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
            Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
            Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
            ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
            Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_false,
            Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
            Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
            Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
            ((Ascii (Coq_true, Coq_true, Coq_true, Coq_false, Coq_false,
            Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
            Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
            Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
            Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
            ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
            Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
            Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
            Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_false,
            Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
            ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true,
            Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
            Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
            Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
            Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
            ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
            Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_false,
            Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_false,
            Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
            Coq_false, Coq_true, Coq_true, Coq_false, Coq_false)), (String
            ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_true,
            Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true,
            Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_false,
            Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
            Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
            ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
            Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
            Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
            Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
            Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
            ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
            Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
            Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
            Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
            Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
            ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
            Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
            Coq_true, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
            Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
            Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
            ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_true,
            Coq_false, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
            Coq_true, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
            Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
            Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
            ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false,
            Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
            Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
            Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_false,
            Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
            EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
     | None ->
       Error (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false,
         Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
         (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
         Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
         Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
         (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
         Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
         (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
         Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
         Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
         (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false,
         Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
         (Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
         Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
         Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
         (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false,
         Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
         (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
         Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
         Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
         (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
         Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
         Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
         ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
         Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
         ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
         ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
         ((Ascii (Coq_true, Coq_true, Coq_true, Coq_false, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
         Coq_false)),
         EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

(** val recolor_expr : expr -> colorMap -> expr optErr **)

let rec recolor_expr e c =
  match e with
  | Etempvar (i, t0) ->
    ret (Obj.magic coq_Monad_optErr) (Etempvar
      ((Pos.of_nat (PTree.get_default default_reg i c)), t0))
  | Esload (e0, t0) ->
    bind (Obj.magic coq_Monad_optErr) (recolor_expr e0 c) (fun e' ->
      ret (Obj.magic coq_Monad_optErr) (Esload (e', t0)))
  | Emload (e0, t0) ->
    bind (Obj.magic coq_Monad_optErr) (recolor_expr e0 c) (fun e' ->
      ret (Obj.magic coq_Monad_optErr) (Emload (e', t0)))
  | Eaddr (e0, t0) ->
    bind (Obj.magic coq_Monad_optErr) (recolor_expr e0 c) (fun e' ->
      ret (Obj.magic coq_Monad_optErr) (Eaddr (e', t0)))
  | Eunop (o, e0, t0) ->
    bind (Obj.magic coq_Monad_optErr) (recolor_expr e0 c) (fun e' ->
      ret (Obj.magic coq_Monad_optErr) (Eunop (o, e', t0)))
  | Ebinop (o, e1, e2, t0) ->
    bind (Obj.magic coq_Monad_optErr) (recolor_expr e1 c) (fun e1' ->
      bind (Obj.magic coq_Monad_optErr) (recolor_expr e2 c) (fun e2' ->
        ret (Obj.magic coq_Monad_optErr) (Ebinop (o, e1', e2', t0))))
  | Ecall1 (b, e0, t0) ->
    bind (Obj.magic coq_Monad_optErr) (recolor_expr e0 c) (fun e' ->
      ret (Obj.magic coq_Monad_optErr) (Ecall1 (b, e', t0)))
  | _ -> ret (Obj.magic coq_Monad_optErr) e

(** val recolor_exprs : expr list -> colorMap -> expr list optErr **)

let rec recolor_exprs es c =
  match es with
  | Coq_nil -> ret (Obj.magic coq_Monad_optErr) Coq_nil
  | Coq_cons (e, rest) ->
    bind (Obj.magic coq_Monad_optErr) (Obj.magic recolor_expr e c) (fun e' ->
      bind (Obj.magic coq_Monad_optErr) (recolor_exprs rest c) (fun rest' ->
        ret (Obj.magic coq_Monad_optErr) (Coq_cons (e', rest'))))

(** val recolor_regs :
    ident list -> colorMap -> ident list -> ident list optErr **)

let rec recolor_regs l c r =
  match l with
  | Coq_nil -> ret (Obj.magic coq_Monad_optErr) r
  | Coq_cons (x, xs) ->
    recolor_regs xs c (Coq_cons
      ((Pos.of_nat (PTree.get_default default_reg x c)), r))

(** val recolor_reg : ident -> colorMap -> ident **)

let recolor_reg i c =
  Pos.of_nat (PTree.get_default default_reg i c)

(** val recolor_expr_opt : expr option -> colorMap -> expr option optErr **)

let recolor_expr_opt e c =
  match e with
  | Some e' ->
    (match recolor_expr e' c with
     | Success s -> Success (Some s)
     | Error s -> Error s)
  | None -> Success None

(** val recolor :
    code -> set -> nat -> node -> colorMap -> (code -> code) -> (code ->
    code) optErr **)

let rec recolor cfg t0 rd tn c cont =
  match rd with
  | O ->
    Error (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true,
      Coq_false, Coq_false, Coq_true, Coq_false)), (String ((Ascii
      (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
      Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
      Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
      Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
      Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
      Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
      (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
      Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
      ((Ascii (Coq_true, Coq_true, Coq_false, Coq_true, Coq_false, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
      Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
      Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
      Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
      Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
      (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
      Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
      (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
      Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
      (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
      Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
      (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
      Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
      Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
      ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
      Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
      (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
      Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
      (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
      Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
      ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
      Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
      (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
      Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
      (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_false, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
      Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
      (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
      Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
      (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
      Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
      ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
      Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
      Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
      Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
      Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
      Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
      (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
      Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
      (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
      Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
      ((Ascii (Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
      Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
      (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_true, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
      Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
      ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
      Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
      Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
      Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
      ((Ascii (Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
      Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
      (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
      Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
      (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
      Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
      (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
      Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
      (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
      Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
      ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
      Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
      ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
      Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
      Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
      (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
      Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
      (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
      Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
      EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
  | S x ->
    (match PTree.get tn cfg with
     | Some s ->
       (match set_in tn t0 with
        | Coq_true -> ret (Obj.magic coq_Monad_optErr) cont
        | Coq_false ->
          let t' = set_union t0 (set_add tn set_empty) in
          let recolor' = recolor cfg t' x in
          (match s with
           | Sskip n -> recolor' n c cont
           | Smassign (lv, rv, n) ->
             bind (Obj.magic coq_Monad_optErr) (Obj.magic recolor_expr lv c)
               (fun lv' ->
               bind (Obj.magic coq_Monad_optErr)
                 (Obj.magic recolor_expr rv c) (fun rv' ->
                 recolor' n c (fun g ->
                   PTree.set tn (Smassign (lv', rv', n)) (cont g))))
           | Ssassign (lv, rv, n) ->
             bind (Obj.magic coq_Monad_optErr) (Obj.magic recolor_expr lv c)
               (fun lv' ->
               bind (Obj.magic coq_Monad_optErr)
                 (Obj.magic recolor_expr rv c) (fun rv' ->
                 recolor' n c (fun g ->
                   PTree.set tn (Ssassign (lv', rv', n)) (cont g))))
           | Sset (id, rv, n) ->
             bind (Obj.magic coq_Monad_optErr) (Obj.magic recolor_expr rv c)
               (fun rv' ->
               recolor' n c (fun g ->
                 PTree.set tn (Sset ((recolor_reg id c), rv', n)) (cont g)))
           | Scall (retval, lab, args, n) ->
             bind (Obj.magic coq_Monad_optErr)
               (Obj.magic recolor_exprs args c) (fun args' ->
               match retval with
               | Some rv ->
                 recolor' n c (fun g ->
                   PTree.set tn (Scall ((Some (recolor_reg rv c)), lab,
                     args', n)) (cont g))
               | None ->
                 recolor' n c (fun g ->
                   PTree.set tn (Scall (retval, lab, args', n)) (cont g)))
           | Scond (cond, ntrue, nfalse) ->
             bind (Obj.magic coq_Monad_optErr)
               (Obj.magic recolor_expr cond c) (fun cond' ->
               bind (Obj.magic coq_Monad_optErr)
                 (recolor' ntrue c (fun g -> g)) (fun cont' ->
                 recolor' nfalse c (fun g ->
                   PTree.set tn (Scond (cond', ntrue, nfalse))
                     (cont' (cont g)))))
           | Sreturn (val0, n) ->
             (match val0 with
              | Some v ->
                recolor' n c (fun g ->
                  PTree.set tn (Sreturn ((Some (recolor_reg v c)), n))
                    (cont g))
              | None -> recolor' n c cont)
           | Shash (e1, e2, eo, n) ->
             bind (Obj.magic coq_Monad_optErr) (Obj.magic recolor_expr e1 c)
               (fun e1' ->
               bind (Obj.magic coq_Monad_optErr)
                 (Obj.magic recolor_expr e2 c) (fun e2' ->
                 match eo with
                 | Some eoo ->
                   bind (Obj.magic coq_Monad_optErr)
                     (Obj.magic recolor_expr eoo c) (fun eoo' ->
                     recolor' n c (fun g ->
                       PTree.set tn (Shash (e1', e2', (Some eoo'), n))
                         (cont g)))
                 | None ->
                   recolor' n c (fun g ->
                     PTree.set tn (Shash (e1', e2', None, n)) (cont g))))
           | Stransfer (a, v, nfail, n) ->
             bind (Obj.magic coq_Monad_optErr) (Obj.magic recolor_expr a c)
               (fun a' ->
               bind (Obj.magic coq_Monad_optErr) (Obj.magic recolor_expr v c)
                 (fun v' ->
                 bind (Obj.magic coq_Monad_optErr)
                   (recolor' nfail c (fun g -> g)) (fun cont' ->
                   recolor' n c (fun g ->
                     PTree.set tn (Stransfer (a', v', nfail, n))
                       (cont' (cont g))))))
           | Scallmethod (a, rvs, sig0, v, gas, args, nfail, n) ->
             bind (Obj.magic coq_Monad_optErr)
               (Obj.magic recolor_regs rvs c Coq_nil) (fun rvs' ->
               bind (Obj.magic coq_Monad_optErr) (Obj.magic recolor_expr a c)
                 (fun a' ->
                 bind (Obj.magic coq_Monad_optErr)
                   (Obj.magic recolor_expr v c) (fun v' ->
                   bind (Obj.magic coq_Monad_optErr)
                     (Obj.magic recolor_expr_opt gas c) (fun gas' ->
                     bind (Obj.magic coq_Monad_optErr)
                       (Obj.magic recolor_exprs args c) (fun args' ->
                       bind (Obj.magic coq_Monad_optErr)
                         (recolor' nfail c (fun g -> g)) (fun cont' ->
                         recolor' n c (fun g ->
                           PTree.set tn (Scallmethod (a', rvs', sig0, v',
                             gas', args', nfail, n)) (cont' (cont g)))))))))
           | Slog (topics, args, n) ->
             bind (Obj.magic coq_Monad_optErr)
               (Obj.magic recolor_exprs topics c) (fun topics' ->
               bind (Obj.magic coq_Monad_optErr)
                 (Obj.magic recolor_exprs args c) (fun args' ->
                 recolor' n c (fun g ->
                   PTree.set tn (Slog (topics', args', n)) (cont g))))
           | _ -> ret (Obj.magic coq_Monad_optErr) cont))
     | None ->
       Error (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true,
         Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
         (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
         Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
         Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
         (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
         Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
         (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
         Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
         Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
         (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
         ((Ascii (Coq_false, Coq_true, Coq_true, Coq_false, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
         Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
         Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)), (String
         ((Ascii (Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
         Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
         Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_false,
         Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
         Coq_true, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
         Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
         Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_false,
         Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
         Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
         Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
         Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
         Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
         ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
         Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true,
         Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
         Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
         Coq_false, Coq_false, Coq_false, Coq_true, Coq_false)), (String
         ((Ascii (Coq_true, Coq_true, Coq_true, Coq_false, Coq_false,
         Coq_false, Coq_true, Coq_false)),
         EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

(** val color_graph :
    code -> nat -> node -> clashg -> (code, colorMap) prod optErr **)

let color_graph cfg maxnode nentry cg =
  match PTree.fold (fun acc _ e ->
          match acc with
          | Coq_true -> Nat.ltb (set_card e) max_reg
          | Coq_false -> Coq_false) cg Coq_true with
  | Coq_true ->
    let num_tv = length (PTree.elements cg) in
    bind (Obj.magic coq_Monad_optErr)
      (Obj.magic pop_graph (length (PTree.elements cg)) cg Coq_nil)
      (fun stack ->
      let unique_nodes =
        Nat.eqb
          (set_card (fold_left (fun acc e -> set_add e acc) stack set_empty))
          num_tv
      in
      (match unique_nodes with
       | Coq_true ->
         bind (Obj.magic coq_Monad_optErr)
           (Obj.magic assign_color stack cg PTree.empty) (fun colors ->
           match Nat.eqb (length (PTree.elements colors)) num_tv with
           | Coq_true ->
             bind (Obj.magic coq_Monad_optErr)
               (Obj.magic recolor cfg set_empty maxnode nentry colors
                 (fun g -> g)) (fun f ->
               ret (Obj.magic coq_Monad_optErr) (Coq_pair ((f cfg), colors)))
           | Coq_false ->
             Error (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
               Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
               (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
               Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
               Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
               Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
               Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
               ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
               Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
               Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
               Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
               Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
               ((Ascii (Coq_true, Coq_true, Coq_true, Coq_false, Coq_false,
               Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
               Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
               Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
               Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
               Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
               Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
               ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
               Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
               Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
               Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
               Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
               Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
               Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
               ((Ascii (Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
               Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
               Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
               Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
               Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
               (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false,
               Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
               (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
               Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
               Coq_true, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
               Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
               Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
               ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
               Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
               Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
               Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
               Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
               ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
               Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
               Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
               Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
               Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
               (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true,
               Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
               (Coq_true, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
               Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
               Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
               Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_false,
               Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
               ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
               Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
               Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
               Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
               Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
               (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true,
               Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
               (Coq_false, Coq_true, Coq_true, Coq_false, Coq_false,
               Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
               Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
               Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
               Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
               Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
               Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
               ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false,
               Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
               Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
               Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
               Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
               ((Ascii (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false,
               Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
               Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
               Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
               Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
               ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
               Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
               (Coq_false, Coq_true, Coq_true, Coq_false, Coq_false,
               Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
               Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
               Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
               Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
               ((Ascii (Coq_true, Coq_false, Coq_true, Coq_true, Coq_false,
               Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
               Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
               Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
               Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
               Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
               Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
               ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
               Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
               Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
               Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
               Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
               ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
               Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
               (Coq_true, Coq_true, Coq_true, Coq_false, Coq_false, Coq_true,
               Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
               Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
               Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
               Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
               ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
               Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
               Coq_false, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
               Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
               Coq_true, Coq_false, Coq_true, Coq_false, Coq_false)), (String
               ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
               Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
               (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
               Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
               Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
               Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
               Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
               ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
               Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
               (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
               Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
               Coq_false, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
               Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
               Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
               ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
               Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
               (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
               Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
               Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
               Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
               Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
               ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
               Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
               Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
               Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
               Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
               Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
               Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String
               ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
               Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
               Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
               Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
               Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
               (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false,
               Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
               (Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
               Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
               Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
               Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
               Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
               (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
               Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
               (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
               Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
               Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
               Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true,
               Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
               ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
               Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
               Coq_false, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
               Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_true,
               Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
               EmptyString)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
       | Coq_false ->
         Error (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
           Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
           Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
           (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
           Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_false,
           Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
           Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
           Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
           (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_true,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_true, Coq_false, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false,
           Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
           Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
           Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_true, Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
           Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_true,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false,
           Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
           Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
           Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false,
           Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
           Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
           (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
           (Coq_true, Coq_true, Coq_true, Coq_false, Coq_false, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
           Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
           (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
           (Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
           Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
           Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
           EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
  | Coq_false ->
    Error (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_true,
      Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true,
      Coq_true, Coq_true, Coq_true, Coq_false, Coq_true, Coq_true,
      Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_false,
      Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
      Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
      (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
      Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
      (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_true, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
      Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
      (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
      Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
      Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
      ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true,
      Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
      (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
      Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
      (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
      Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
      ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_false,
      Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
      Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
      Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
      ((Ascii (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
      Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
      Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
      (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
      Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
      ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
      Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
      Coq_false, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)),
      (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
      Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
      ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
      Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
      (String ((Ascii (Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
      Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
      (String ((Ascii (Coq_true, Coq_false, Coq_true, Coq_true, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
      Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
      (String ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
      Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_false,
      Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_true,
      Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
      Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_true,
      Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false, Coq_true,
      Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
      Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
      (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
      Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
      ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_true, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
      Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_false, Coq_false, Coq_true, Coq_true, Coq_false, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
      Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
      (String ((Ascii (Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
      Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
      (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_false, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_true, Coq_false,
      Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
      (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
      Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
      ((Ascii (Coq_true, Coq_false, Coq_true, Coq_false, Coq_false, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_false,
      Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_true, Coq_true, Coq_false, Coq_false, Coq_true, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
      Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
      (String ((Ascii (Coq_true, Coq_true, Coq_false, Coq_false, Coq_false,
      Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_false,
      Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_false,
      Coq_false)), (String ((Ascii (Coq_false, Coq_false, Coq_false,
      Coq_true, Coq_false, Coq_true, Coq_false, Coq_false)), (String ((Ascii
      (Coq_true, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
      Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_true,
      Coq_false, Coq_true, Coq_true, Coq_false, Coq_false)), (String ((Ascii
      (Coq_true, Coq_false, Coq_false, Coq_true, Coq_false, Coq_true,
      Coq_false, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
      Coq_false, Coq_false, Coq_false, Coq_true, Coq_false, Coq_false)),
      (String ((Ascii (Coq_true, Coq_false, Coq_false, Coq_false, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
      Coq_true, Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String
      ((Ascii (Coq_false, Coq_false, Coq_false, Coq_false, Coq_false,
      Coq_true, Coq_false, Coq_false)), (String ((Ascii (Coq_true, Coq_true,
      Coq_false, Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)),
      (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false,
      Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
      Coq_true, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)), (String
      ((Ascii (Coq_true, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_true, Coq_false,
      Coq_false, Coq_true, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_true, Coq_true, Coq_true, Coq_true, Coq_true, Coq_false, Coq_true,
      Coq_false)), (String ((Ascii (Coq_true, Coq_true, Coq_true, Coq_false,
      Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii (Coq_false,
      Coq_true, Coq_false, Coq_false, Coq_true, Coq_true, Coq_true,
      Coq_false)), (String ((Ascii (Coq_true, Coq_false, Coq_false,
      Coq_false, Coq_false, Coq_true, Coq_true, Coq_false)), (String ((Ascii
      (Coq_false, Coq_false, Coq_false, Coq_false, Coq_true, Coq_true,
      Coq_true, Coq_false)), (String ((Ascii (Coq_false, Coq_false,
      Coq_false, Coq_true, Coq_false, Coq_true, Coq_true, Coq_false)),
      EmptyString))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

(** val variable_coalescing :
    code -> nat -> node -> nat -> (code, colorMap) prod optErr **)

let variable_coalescing cfg maxnode nentry precision =
  bind (Obj.magic coq_Monad_optErr) (Obj.magic compute_livevar cfg precision)
    (fun lvs ->
    bind (Obj.magic coq_Monad_optErr) (Obj.magic create_clash_graph lvs)
      (fun cg -> color_graph cfg maxnode nentry cg))

(** val get_clash_graph :
    code -> nat -> (positive, positive list) prod list optErr **)

let get_clash_graph cfg precision =
  bind (Obj.magic coq_Monad_optErr) (Obj.magic compute_livevar cfg precision)
    (fun lvs ->
    bind (Obj.magic coq_Monad_optErr) (Obj.magic create_clash_graph lvs)
      (fun cg ->
      ret (Obj.magic coq_Monad_optErr)
        (map (fun x ->
          let Coq_pair (id, tvss) = x in
          Coq_pair (id,
          (PTree.fold (fun acc i ind ->
            match ind with
            | Coq_true -> Coq_cons (i, acc)
            | Coq_false -> acc) tvss Coq_nil))) (PTree.elements cg))))

(** val max_iteration : nat **)

let max_iteration =
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
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S
    O))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

(** val copt_function : coq_function -> coq_function optErr **)

let copt_function f =
  match variable_coalescing f.fn_code (sub (Pos.to_nat f.fn_nextnode) (S O))
          f.fn_entrypoint max_iteration with
  | Success p ->
    let Coq_pair (ncfg, cmap) = p in
    ret (Obj.magic coq_Monad_optErr) { fn_return = f.fn_return; fn_params =
      (map (fun e ->
        let Coq_pair (id, ty) = e in
        Coq_pair ((Pos.of_nat (PTree.get_default (S O) id cmap)), ty))
        f.fn_params); fn_temps =
      (map (fun e ->
        let Coq_pair (id, ty) = e in
        Coq_pair ((Pos.of_nat (PTree.get_default (S O) id cmap)), ty))
        f.fn_temps); fn_locals = f.fn_locals; fn_code = ncfg; fn_entrypoint =
      f.fn_entrypoint; fn_nextnode = f.fn_nextnode }
  | Error msg -> Error msg

(** val copt_constructor :
    coq_function option -> coq_function option optErr **)

let copt_constructor f = match f with
| Some f0 ->
  bind (Obj.magic coq_Monad_optErr) (Obj.magic copt_function f0) (fun cf ->
    ret (Obj.magic coq_Monad_optErr) (Some cf))
| None -> ret (Obj.magic coq_Monad_optErr) f

(** val clash_function :
    coq_function -> (positive, positive list) prod list optErr **)

let clash_function f =
  get_clash_graph f.fn_code max_iteration

(** val copt_functions :
    coq_function PTree.t -> coq_function PTree.t optErr **)

let copt_functions defs =
  transl_tree copt_function defs

(** val copt_methoddefs :
    coq_function option IntMap.t -> coq_function option IntMap.t optErr **)

let copt_methoddefs defs =
  transl_map copt_function defs

(** val graphviz_helper :
    coq_function -> ((positive, statement) prod list, positive) prod **)

let graphviz_helper f =
  Coq_pair ((PTree.elements f.fn_code), f.fn_entrypoint)

(** val clash_viz :
    genv -> (positive, positive list) prod list list optErr **)

let clash_viz ge =
  fold_left (fun acc x ->
    match acc with
    | Success acc' ->
      bind (Obj.magic coq_Monad_optErr) (Obj.magic clash_function x)
        (fun x' -> ret (Obj.magic coq_Monad_optErr) (Coq_cons (x', acc')))
    | Error msg -> Error msg) (Genv.all_functions ge) (Success Coq_nil)

(** val cgraph_viz :
    genv -> ((positive, statement) prod list, positive) prod list optErr **)

let cgraph_viz ge =
  fold_left (fun acc x ->
    match acc with
    | Success acc' ->
      bind (Obj.magic coq_Monad_optErr) (Obj.magic copt_function x)
        (fun x' ->
        ret (Obj.magic coq_Monad_optErr) (Coq_cons ((graphviz_helper x'),
          acc')))
    | Error msg -> Error msg) (Genv.all_functions ge) (Success Coq_nil)

(** val copt_genv : genv -> genv optErr **)

let copt_genv ge =
  let vars = ge.Genv.genv_vars in
  let names = ge.Genv.genv_funcs in
  let fundefs = ge.Genv.genv_fundefs in
  let sigs = ge.Genv.genv_methods in
  let defs = ge.Genv.genv_defs in
  let methoddefs = ge.Genv.genv_methoddefs in
  let constructor = ge.Genv.genv_constructor in
  bind (Obj.magic coq_Monad_optErr) (Obj.magic copt_functions fundefs)
    (fun fundefs0 ->
    bind (Obj.magic coq_Monad_optErr) (Obj.magic copt_methoddefs methoddefs)
      (fun methoddefs0 ->
      bind (Obj.magic coq_Monad_optErr)
        (Obj.magic copt_constructor constructor) (fun constructor0 ->
        ret (Obj.magic coq_Monad_optErr) { Genv.genv_vars = vars;
          Genv.genv_funcs = names; Genv.genv_methods = sigs; Genv.genv_defs =
          defs; Genv.genv_fundefs = fundefs0; Genv.genv_methoddefs =
          methoddefs0; Genv.genv_constructor = constructor0 })))
