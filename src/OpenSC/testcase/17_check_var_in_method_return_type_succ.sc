/- test case: find variable in method successful -/

signature SimpleStorage {
    storage storedData : UInt;
    constructor c : UInt -> void;
    method set : (int, UInt) -> int;
}

/- implementation -/

constructor c (s : UInt){
  storage
    storedData              |-> s;
  returns void;
}

method set(x: int, y: UInt) {
	guard{}
	storage{}
	effects{}
	returns x;
}