/- simple open auction -/
/- https://solidity.readthedocs.io/en/v0.4.24/solidity-by-example.html#simple-open-auction -/


signature AUCTION{
  /- parameters -/
  storage beneficiary : Address;
  storage end : UInt;

  /- current state of the auction -/
  storage leader : Address;  /- leading bidder; -/
  storage lead : UInt;       /- highest bid; -/

  /- allowed withdrawals of previous bids -/
  map withdrawals : (Address) => UInt;

  /- events -/
  event HighestBidIncreased = HighestBidIncreased of (Address, UInt);
  event AuctionEnded        = AuctionEnded  of (Address, UInt);

  /- methods -/
  constructor c : (UInt, Address) -> void;
  method bid : (void) -> void;
  method withdraw : (void) -> void;
  method terminate : (void) -> void;

}


constructor c (t : UInt, a : Address){
  storage
    end          |-> (Env.now) + t; 
    beneficiary  |-> a;
  returns void;
}


method bid (){
  guard{
    Env.now    <= end;
    Env.value  >  lead;
    withdrawals[leader] >= withdrawals[leader] - lead;
  }
  storage{
    withdrawals[leader]  |-> withdrawals[leader] + lead;
    leader               |-> Env.sender;
    lead                 |-> Env.value;
  }
  effects{
    logs HighestBidIncreased (Env.sender, Env.value);
  }
  returns voidlit;
}

method withdraw (){
  guard{
    withdrawals[Env.sender]  != 0;
  }
  storage{
    withdrawals[Env.sender]  |-> 0;
  }
  effects{
    /- sends Env.sender withdrawals[Env.sender]; -/
  }
  returns voidlit;
}

method terminate (){
  guard{
    Env.now  >= end;
  }
  storage{}
  effects{
    logs AuctionEnded (leader, lead);
  }
  returns voidlit;
}
