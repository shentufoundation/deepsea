signature SimpleStorage {
    storage storedData : UInt;
    constructor c : UInt -> void;
    method set : (UInt, UInt) -> void;
}

/- implementation -/

constructor c (s : UInt){
  storage
    storedData              |-> s;
  returns void;
}

method set(x: UInt, y: Bool) {
	guard{
        x > 0;
    }
	storage{
        storedData     |-> y;
    }
	effects{}
	returns voidlit;
}

