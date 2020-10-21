/- A simple storage program -/

signature SimpleStorage {
    storage amount : UInt;
    map balances : (Address) => UInt;
	constructor c : (void) -> void;
	method set : (Address) -> UInt;
}

constructor c (){
	storage
	returns void;
}

method set(x: Address) {
	guard{
        True;
    }
	storage{
        amount |-> balances[(x)]
    }
	effects{}
	returns amount;
}
