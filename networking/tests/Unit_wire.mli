(* Wire: the .mli's worked example (lockstep's input
 * message, 7 bytes), MIDI's varint table, zigzag, and the garbage
 * refused -- by hand, then 10,000 random strings: never an exception,
 * and whatever parses re-encodes to the same bytes *)
val tests : Testo.t list
