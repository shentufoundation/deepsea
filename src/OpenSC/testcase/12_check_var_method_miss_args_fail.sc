/- test case: find variable in method successful -/

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

method set(x: UInt) {
	guard{
        x > 0;
    }
	storage{}
	effects{}
	returns voidlit;
}

