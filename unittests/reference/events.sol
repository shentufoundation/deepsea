pragma solidity ^0.5.12;

contract O {
    event E1 (uint v);
    event E2 (uint v1, bool v2);
    event IE1 (uint indexed v);
    event IE2 (uint v1, bool indexed v2);
    event Transfer (uint indexed fromA, uint indexed toA, uint value);
    event Approval (uint indexed owner, uint indexed spender, uint value);
    

    function f() public {
       emit E1(42);
       emit E2(1, false);
       emit IE1(13);
       emit IE2(2, false);
       emit Transfer(1, 2, 42);
       emit Approval(1, 2, 42);
    }

}
