/- test case -/

signature TOKEN{

  storage supply : UInt;

  map balances : (Address) => UInt;

  constructor c : UInt -> void;
  method balanceOf : (int) -> UInt;
}


/- implementation -/

constructor c (s : UInt){
  storage
    supply                |-> s;
    balances[Env.sender]  |-> s;
  returns void;
}

method balanceOf (a : int){
  guard{
    Env.value == 0;
  }
  storage{}
  effects{}
  returns balances[(a)];
  }