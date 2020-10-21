open Integers

type coq_val =
| Vunit
| Vint of Int256.int
| Vhash of coq_val
| Vhash2 of coq_val * coq_val
