/- test case -/

signature SimpleStorage {
    storage storedData : int;
    constructor c : UInt -> void;
    method set : (Bool, Bool) -> void;
}

/- implementation -/

constructor c (s : UInt){
  storage
    storedData              |-> s;
  returns void;
}

method set(x: Bool, y: Bool) {
	guard{
        (x and y) or y;
    }
	storage{}
	effects{}
	returns voidlit;
}