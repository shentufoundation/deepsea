open Cryptokit
open Yojson.Basic
open Std

let keccak s =
  let hash = hash_string (Hash.keccak 256) s in
  let l = List.init (String.length hash) (String.get hash) in
  let hex = List.map (function x ->
    Printf.sprintf "%02x" (Char.code x)
  ) l in
  String.concat "" hex

let metadata version compiler_filename filename abi =
  let file = Std.input_file filename in
  (to_string (`Assoc [
      "version",
     `String "1";
     "language",
     `String "DeepSEA";
     "compiler",
     `Assoc [
       "version",
       `String version;
       "keccak256",
       `String (keccak (Std.input_file compiler_filename));
     ];
      "sources",
      `Assoc [
        Filename.basename filename,
        `Assoc [
          "keccak256",
          `String (keccak file);
          "content",
          `String (Std.input_file filename)
        ]
      ];
      "output",
      `Assoc [
        "abi",
        from_string abi
      ]
    ]
  ))
