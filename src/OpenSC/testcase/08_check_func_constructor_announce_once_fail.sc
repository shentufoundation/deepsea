
signature SimpleStorage {
    storage storedData : UInt;
    constructor c : UInt -> void;
    constructor c2 : UInt -> void;
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
