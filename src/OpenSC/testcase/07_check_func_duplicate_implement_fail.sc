/- A simple storage program -/

signature SimpleStorage {
    storage storedData : UInt;
    constructor c : UInt -> void;
    method set : (UInt) -> void;
}

constructor c (s : UInt){
	storage
	returns void;
}

method set(x: UInt) {
	guard{}
	storage{}
	effects{}
	returns voidlit;
}

method set(y: UInt) {
	guard{}
	storage{}
	effects{}
	returns voidlit;
}