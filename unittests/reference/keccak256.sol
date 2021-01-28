pragma solidity ^0.5.12;

contract O {
    function hash1(uint x ) public pure returns (uint) {
       return uint256(keccak256(abi.encodePacked(x))); 
    }
    
    function hash2(uint x, uint y) public pure returns (uint) {
       return uint256(keccak256(abi.encodePacked(x,y))); 
    }

    function hash1a(address x) public pure returns (uint) {
       return uint256(keccak256(abi.encodePacked(uint(x)))); 
    }

}
