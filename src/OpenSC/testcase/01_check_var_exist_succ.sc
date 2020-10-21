/- test case: find variable successful -/

signature SimpleStorage {
    storage storedData : UInt;
    constructor c : UInt -> void;
}

/- implementation -/

constructor c (s : UInt){
  storage
    storedData                |-> s;
  returns void;
}
