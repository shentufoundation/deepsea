/- test case: find variable in method successful -/

signature SimpleStorage {
    storage storedData : UInt;
    constructor c : UInt -> void;
    method set : (int, UInt) -> void;
}

/- implementation -/

constructor c (s : UInt){
  storage
    storedData              |-> s;
  returns void;
}

method set(x: int, y: UInt) {
	guard{
        x > 0;
    }
	storage{
        storedData     |-> y;
    }
	effects{}
	returns voidlit;
}

