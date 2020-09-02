open BinNums

module Pos :
 sig
  type mask =
  | IsNul
  | IsPos of positive
  | IsNeg
 end
