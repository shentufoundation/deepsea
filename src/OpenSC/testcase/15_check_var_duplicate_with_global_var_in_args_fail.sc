signature SimpleStorage {
    storage storedData : UInt;
    constructor c : UInt -> void;
    method set : (UInt) -> void;
}

constructor c (s : UInt){
  storage
    storedData              |-> s;
  returns void;
}

method set(storedData: UInt) {
	guard{}
	storage{}
	effects{}
	returns voidlit;
}
