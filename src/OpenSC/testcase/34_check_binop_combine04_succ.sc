/- test case -/

signature SimpleStorage {
    storage storedData : int;
    storage tag : Bool;
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
	guard{}
	storage{
        tag |-> (x == 5 * y) and (y != 10) or True;
    }
	effects{}
	returns voidlit;
}