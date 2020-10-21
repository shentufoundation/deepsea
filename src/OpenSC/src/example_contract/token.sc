/- 
  token implementation satisfying the ERC20 standard:
   https://eips.ethereum.org/EIPS/eip-20

   interface
-/

signature TOKEN{

  storage supply : UInt;

  map balances : (Address) => UInt;
  map allowances : (Address, Address) => UInt;

  event Transfer = Transfer of (Address, Address, UInt);
  event Approval = Approval of (Address, Address, UInt);

  constructor c : (UInt) -> void;
  method totalSupply : (void) -> UInt;
  method balanceOf : (Address) -> UInt;
  method transfer : (Address, UInt) -> Bool;
  method transferFrom : (Address, Address, UInt) -> Bool;
  method approve : (Address, UInt) -> Bool;
  method allowance : (Address, Address) -> UInt;
}


/- implementation -/

constructor c (s : UInt){
  storage
    supply                |-> s;
    balances[Env.sender]  |-> s;
  returns void;
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

method allowance (owner : Address, spender : Address){
  guard{
    Env.value == 0;
  }
  storage{}
  effects{}
  returns allowances[spender, owner];
}

method transfer (a : Address, v : UInt){

  guard{
    Env.value == 0;
    balances[Env.sender] >= v;
    /- overflow checking -/
    balances[a] > balances[a] - v;
    balances[Env.sender] > balances[Env.sender] + v;
  }
  storage{
    balances[Env.sender] |-> balances[Env.sender] - v;
    balances[a]          |-> (balances[a] + v);
  }
  effects{

    logs Transfer (Env.sender, a, v);
  }
  returns True;
}

method approve (spender : Address, v : UInt){

  guard{
    Env.value == 0;
  }
  storage{
    allowances[spender, Env.sender] |-> v;
  }
  effects{
    logs Approval (Env.sender, spender, v);
  }
  returns True;
}

method transferFrom (from : Address, to : Address, v : UInt){

  guard{ 
    Env.value == 0;
    balances[from] >= v;
    allowances[Env.sender, from] >= v;
    
    /- overflow checking -/
    
    allowances[Env.sender, from] - v < allowances[Env.sender, from];
    balances[from] - v < balances[from];
    balances[to] + v > balances[to];
  }
  storage{
    allowances[Env.sender, from]  |-> allowances[Env.sender, from] - v;
    balances[from]                  |-> balances[from] - v;
    balances[to]                    |-> balances[to] + v;
  }
  effects{}
  returns True;
}
