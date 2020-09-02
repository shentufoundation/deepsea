open Ascii
open BinNums
open Datatypes
open Globalenvs
open Maps0
open Monad
open OptErrMonad
open String0

val map_error : ('a1 -> 'a2 optErr) -> 'a1 list -> 'a2 list optErr

val xtransl_tree :
  ('a1 -> 'a2 optErr) -> (positive, 'a1) prod list -> 'a2 PTree.t optErr

val transl_tree : ('a1 -> 'a2 optErr) -> 'a1 PTree.t -> 'a2 PTree.t optErr

val partial_f : ('a1 -> 'a2 optErr) -> 'a1 option -> 'a2 option optErr

val transl_map :
  ('a1 -> 'a2 optErr) -> 'a1 option IntMap.t -> 'a2 option IntMap.t optErr

val xtransl_tree_keys_move :
  (positive -> 'a1 -> (positive, 'a2) prod optErr) -> (positive, 'a1) prod
  list -> 'a2 PTree.t optErr

val transl_tree_keys_move :
  (positive -> 'a1 -> (positive, 'a2) prod optErr) -> 'a1 PTree.t -> 'a2
  PTree.t optErr
