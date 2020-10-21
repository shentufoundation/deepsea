/- test case -/

signature TOKEN{

  storage supply : UInt;

  map balances : (Bool) => UInt;

  constructor c : UInt -> void;
  method balanceOf : (Bool) -> UInt;
}


/- implementation -/

constructor c (s : UInt){
  storage
    supply                |-> s;
    balances[Env.sender]  |-> s;
  returns void;
}

method balanceOf (a : Bool){
  guard{
    Env.value == 0;
  }
  storage{}
  effects{}
  returns balances[(a)];
  }