open Ascii

type string =
| EmptyString
| String of ascii * string

val append : string -> string -> string
