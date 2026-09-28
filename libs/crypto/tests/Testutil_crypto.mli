(* the tests' helpers: bytes written in hexadecimal, as the standards
 * print their vectors (spaces and line breaks ignored) *)
val unhex : string -> string
val hex : string -> string

(* [check_hex name expected actual]: bytes compared, shown in hex *)
val check_hex : string -> string -> string -> unit
