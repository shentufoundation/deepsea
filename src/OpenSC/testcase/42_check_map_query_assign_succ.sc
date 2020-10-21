/- test case -/

signature TOKEN{

  storage supply : UInt;

  map balances : (Address) => UInt;

  constructor c : UInt -> void;
  method balanceOf : (Address) -> UInt;
}


/- implementation -/

constructor c (s : UInt){
  storage
    supply                |-> s;
    balances[Env.sender]  |-> s;
  returns void;
}

method balanceOf (a : Address){
  guard{
    Env.value == 0;
  }
  storage{
      balances[(a)] |-> supply;
  }
  effects{}
  returns balances[(a)];
  }