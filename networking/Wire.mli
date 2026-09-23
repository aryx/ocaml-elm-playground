(* Wire: values as bytes and back, the way a network needs them.

   Two machines can't share an OCaml value: a message is bytes, written
   by one program and read by another -- maybe another version, another
   language, or someone who wants to crash it. Marshal (Checksum.mli)
   won't do: its bytes change between OCaml versions and between native
   code and js_of_ocaml, and reading untrusted Marshal bytes can crash
   the program. So a protocol spells its bytes out, with a few rules
   worth stating, since breaking them is how protocols rot:

   - **fixed sizes, a fixed byte order**: a u16 is two bytes, the most
     significant first (big-endian, "network byte order", RFC 1700);
   - **variable-length integers** where sizes vary: 7 bits per byte,
     the most significant first, the high bit set on every byte but the
     last -- MIDI's delta times (1983, Midi.ml), the same idea as
     UTF-8's continuation bytes and Protocol Buffers' varints. Small
     numbers, the common case, take one byte:

          0 -> 00        127 -> 7F        128 -> 81 00
        300 -> 82 2C   16383 -> FF 7F   16384 -> 81 80 00

     at most 4 bytes (28 bits, MIDI's limit, and what an int holds in
     a browser too);
   - **signed numbers zigzagged** first (Protocol Buffers' trick), so
     that small negative numbers stay small: 0, -1, 1, -2, 2... become
     0, 1, 2, 3, 4...;
   - **anything that doesn't parse is refused**, never half-trusted:
     bytes missing, a varint that doesn't end, bytes left over at the
     end, a varint written longer than it needs (81 00 is 128, but
     80 81 00 is refused). A parser fed by the Internet is the most
     attacked code there is; refusing the long forms also gives *one
     value, one encoding*, so re-encoding what was read gives back the
     same bytes (the tests check it on random garbage).

   Writing and reading mirror each other: [put_u8] and [get_u8], and a
   message's reader is the same sequence of [get_]s as its writer's
   [put_]s. The [get_]s raise inside [parse] only, which turns every
   failure into an Error.

   Worked example (checked by the tests), the input message of lockstep
   (notes_networking.md section 2): its type (1), the tick it is about
   (300), and the last three inputs, a byte each (a bit per key):

       put_u8 1; put_varint 300; put_string "\x05\x05\x04"

       01  82 2C  03  05 05 04        7 bytes
       type tick  length, inputs

   Sent 20 times a second in UDP, each packet also carries 28 bytes of
   IP and UDP headers: (7 + 28) x 20 = 700 bytes a second -- the
   headers, not the game, are most of the cost, which is why lockstep
   sends a few inputs per packet (the tutorial's arithmetic).

   References: RFC 1700 (network byte order, 1994); the Standard MIDI
   File specification (1988), variable-length quantities; Protocol
   Buffers' encoding documentation (Google, 2008), varints and zigzag. *)

(*****************************************************************************)
(* Writing *)
(*****************************************************************************)

type writer

(* the bytes [write] puts *)
val to_bytes : (writer -> unit) -> string

(* 0 to 255; raises Invalid_argument outside *)
val put_u8 : writer -> int -> unit

(* 0 to 65535, big-endian; raises Invalid_argument outside *)
val put_u16 : writer -> int -> unit

(* 0 to 2^28 - 1, in 1 to 4 bytes; raises Invalid_argument outside *)
val put_varint : writer -> int -> unit

(* -2^27 to 2^27 - 1, zigzagged then as a varint *)
val put_signed : writer -> int -> unit

(* its length as a varint, then its bytes *)
val put_string : writer -> string -> unit

(*****************************************************************************)
(* Reading *)
(*****************************************************************************)

type reader

(* [parse read bytes]: [read]'s value, if [read] took exactly all the
 * bytes; Error saying what was wrong and where otherwise *)
val parse : (reader -> 'a) -> string -> ('a, string) result

val get_u8 : reader -> int
val get_u16 : reader -> int
val get_varint : reader -> int
val get_signed : reader -> int
val get_string : reader -> string

(* for a message's reader: refuse it (e.g., an unknown type) *)
val fail : reader -> string -> 'a
