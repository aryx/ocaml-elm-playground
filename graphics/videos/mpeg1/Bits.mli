(* Bits: a string read a few bits at a time, the most significant bit
 * of each byte first -- how every MPEG syntax element is laid out, of
 * any width, with no regard for byte boundaries (JPEG's reader,
 * Jpeg.ml, has the same idea, with its byte stuffing).

   A **start code** is the one thing aligned: 00 00 01 then a byte
   saying what starts (B3 a sequence header, B8 a group of pictures, 00
   a picture, 01 to AF a slice, B7 the end). Twenty-three zero bits in
   a row appear nowhere else in the data (the codes are built so), so a
   decoder lost in corrupt data, or one that wants to start in the
   middle, finds its way by looking for them. Past the end, zeros. *)

type t

val of_string : string -> t

(* [read b n]: the next [n] bits (up to 24), as a number *)
val read : t -> int -> int

(* [peek b n]: the same, without moving *)
val peek : t -> int -> int

val skip : t -> int -> unit

(* where we are, in bits from the start; and there again *)
val position : t -> int
val seek : t -> int -> unit

val at_end : t -> bool

(* [next_start_code b]: to the next byte boundary, then to the next 00
 * 00 01; the code byte after it, the reader just past it, or None at
 * the end *)
val next_start_code : t -> int option
