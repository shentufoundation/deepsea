open Datatypes
open EVM
open Monad
open OptErrMonad
open String0

type compiled = evm list optErr

val command_compiled : evm -> compiled

val error_compiled : string -> compiled

val empty_compiled : compiled

val append_compiled : evm -> compiled -> compiled

val concatenate_compiled : compiled -> compiled -> compiled
