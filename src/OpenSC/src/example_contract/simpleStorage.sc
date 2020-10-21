/- A simple storage program -/

signature SimpleStorage {
    storage storedData : int;

	constructor c : (void) -> void;
	method get : () -> int;
	method set : (int) -> void;
}

constructor c (){
	storage
	returns void;
}

method get(){
	guard{}
	storage{}
	effects{}
	returns storedData;
}

method set(x: int) {
	guard{
		x > 0;
	}
	storage{
    	storedData |-> x;
	}
	effects{}
	returns voidlit;
}
