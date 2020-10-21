/- test case -/

signature SimpleStorage {
    storage storedData : int;
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
        (x == 5 * y) and (y != 10) or True;
    }
	storage{}
	effects{}
	returns voidlit;
}