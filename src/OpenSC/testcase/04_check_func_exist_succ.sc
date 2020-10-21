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
  }
	storage{
    storedData                |-> x;
  }
	effects{}
	returns voidlit;
}