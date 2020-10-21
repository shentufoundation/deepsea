/- test case: find variable fail -/

signature SimpleStorage {
    storage storedData : UInt;
    storage storedData : Bool;
    constructor c : UInt -> void;
}

/- implementation -/

constructor c (s : UInt){
  storage
    storedData              |-> s;
  returns void;
}
