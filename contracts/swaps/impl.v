Require Import BinPos.
Require Import DeepSpec.Runtime.
Require Import cclib.Maps. 


Definition addr := int256.
Definition timeunit := int256. 
Definition e_maps := Int256Map.t (list addr). 
Definition b_maps := Int256Map.t bool.

(* Record Digraph := {
  leaders : list addr;o
  vertices : list addr; 
  edges : Int256Map.t (list addr);
}.*) 
Compute ((Int256.add Int256.one Int256.one)).


Definition edges := Int256Map.init (nil : (list addr)). 
Compute (Int256Map.get  Int256.zero edges).

Definition dummy_set := Int256Map.init (false).
Fixpoint in_list (a: addr) (lst: list addr) : bool :=
  match lst with 
  | nil => false
  | (cons it rest) => if (Int256.eq a it) then true
        else (in_list a rest)
  end. 

Compute (Int256Map.set Int256.zero nil edges). 
Compute (Int256.lt Int256.zero Int256.one).

Fixpoint num_unvisited (vertices: list addr) (visited: b_maps) : nat := 
  match vertices with 
  | nil => 0%nat
  | (cons a rest) => let rest_res := (num_unvisited rest visited) in 
                     match (Int256Map.get a visited) with 
                      | true => rest_res+1%nat
                      | false => rest_res 
                     end 
  end.

Fixpoint app_to_lst (f: addr->int256) (lst: list addr) := 
  match lst with 
  | nil => Int256.zero (* never happens *)
  | (cons a rest) => let rest_maxl := (app_to_lst f rest) in 
                     let a_l := (f a) in 
                      if (Int256.lt rest_maxl a_l) then a_l else rest_maxl
  end. 

(* this is i will fix that later. :( *)
Program Fixpoint compute_longest_path 
  (src: addr) (dst: addr) (vertices: list addr) (edges: e_maps) (visited: b_maps) (l: int256) (dummy: nat) (node: addr)
  :
int256 
:= match dummy with 
  | (S new_dummy) => 
   (
    if (Int256.eq node dst) then l
    else match (Int256Map.get node visited) with 
    | true => Int256.zero (* loop that doesn't have dst in it *)
    | false => 
      (let new_list := (Int256Map.get node edges) in 
        let new_visited := (Int256Map.set node true visited) in 
       match new_list with
      | nil => Int256.zero (* hit a deadend *)
      | _ => (app_to_lst (compute_longest_path src dst vertices edges new_visited (Int256.add l Int256.one) new_dummy) new_list)
      end)
    end) 
  | O => Int256.zero
  end
.

Check compute_longest_path. 
(*
with helper (src: addr) (dst: addr) (vertices: list addr) (lst: list addr) (edges: e_maps) (visited: b_maps) (l: int256) : int256 
:= match lst with 
  | nil => (* means recursive call to self was [a] *) Int256.zero 
  | (cons it rest) => let newl_1 := (compute_longest_path src dst vertices it edges visited l) in 
                      let newl_2 := (helper src dst vertices rest edges visited l) in 
                      if (Int256.lt newl_2 newl_1) then newl_1 else newl_2 
  end. 
*) 

Fixpoint merge l1 l2 :=
let fix merge_aux l2 :=
match l1, l2 with
| nil, _ => l2
| _, nil => l1
| x1::l1', x2::l2' =>
if  x1 <? x2 then x1::merge l1' l2
else x2::merge_aux l2'
end
in merge_aux l2.


Definition a:=
(fix in_list_aux a lds := match lds with | nil => false | (cons d rest) => if (Int256.eq a d) then true else (in_list_aux a rest) end).


Check a. 

Definition b := (Int256Map.t int256).
Definition d : b := (Int256Map.init int.zero). 




