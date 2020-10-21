/- 
  token implementation satisfying the ERC20 standard:
   https://eips.ethereum.org/EIPS/eip-20

   interface
-/

signature TOKEN{

  storage supply : UInt;

  map balances : (Address) => UInt;

  constructor c : (UInt) -> void;
  
  method initialize : (void) -> UInt;
  method set : (Address, UInt) -> UInt;
  method totalSupply : (void) -> UInt;
  method balanceOf : (Address) -> UInt;
  method transfer : (Address, UInt) -> Bool;
}

/- implementation -/

constructor c (s : UInt){
  storage
    supply                |-> s;
    balances[Env.sender]  |-> s;
  returns void;
}

method initialize () {
  guard{}
  storage{
    balances[Env.sender] |-> 100000;
  }
  effects{}
  returns balances[Env.sender];
}

method set (a:Address, v: UInt){
  guard{}
  storage{
    balances[a] |-> v;
  }
  effects{}
  returns balances[a];
}

method totalSupply (){
  guard{
    Env.value == 0;
  }
  storage{}
  effects{}
  returns supply;
}

method balanceOf (a : Address){
  guard{
    Env.value == 0;
  }
  storage{}
  effects{}
  returns balances[a];
  }

method transfer (a : Address, v : UInt){
  guard{
    Env.value == 0;
    
    /- overflow checking -/
    balances[Env.sender] >= v;
  }
  storage{
    balances[Env.sender] |-> balances[Env.sender] - v;
    balances[a]          |-> (balances[a] + v);
  }
  effects{}
  returns True;
}
