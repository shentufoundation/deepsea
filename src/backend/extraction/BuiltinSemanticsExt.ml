let show0 = function
  | MachineModel.Baddress   -> "address"
  | MachineModel.Borigin    -> "origin"
  | MachineModel.Bcaller    -> "caller"
  | MachineModel.Bcallvalue -> "callvalue"
  | MachineModel.Bcoinbase  -> "coinbase"
  | MachineModel.Btimestamp -> "timestamp"
  | MachineModel.Bnumber    -> "number"

let show1 = function
  | MachineModel.Bbalance   -> "balance"
  | MachineModel.Bblockhash -> "blockhash"
