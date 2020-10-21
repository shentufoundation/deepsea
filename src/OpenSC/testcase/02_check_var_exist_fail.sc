/- test case: find variable successful -/

signature SimpleStorage {
    storage storedData : UInt;
    constructor c : UInt -> void;
    method set : (UInt) -> void;
}

/- implementation -/

constructor c (s : UInt){
  storage
    storedData                |-> s;
  returns void;
}

method set(x: UInt) {
	guard{
    supply > 5;
  }
	storage{}
	effects{}
	returns voidlit;
}
