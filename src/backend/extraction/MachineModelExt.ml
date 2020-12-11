open MachineModel

let show_builtin0 = function
  | Baddress -> "address"
  | Borigin -> "origin"
  | Bcaller -> "caller"
  | Bcallvalue -> "callvalue"
  | Bcoinbase -> "coinbase"
  | Btimestamp -> "timestamp"
  | Bnumber -> "number"
  | Bchainid -> "chainid"
  | Bselfbalance -> "selfbalance"

let show_builtin1 = function
  | Bbalance -> "balance"
  | Bblockhash -> "blockhash"
