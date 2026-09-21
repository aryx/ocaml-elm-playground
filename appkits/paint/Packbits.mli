(* PackBits: the run-length compression of the Macintosh (Apple, 1984),
 * with which MacPaint wrote a picture to a 400 KB floppy -- each row of
 * the picture compressed on its own.
 *
 * Pictures are mostly white, and white is bytes of 0x00 in a row; a
 * run of identical bytes is written as a count and the byte once, and
 * what does not repeat is written as a count and the bytes as they
 * are. The count is one signed byte n:
 *
 *   n = 0 .. 127      the next n + 1 bytes, literally
 *   n = -1 .. -127    the next byte, repeated 1 - n times
 *   n = -128          nothing (skipped)
 *
 * Apple's own example (Technical Note TN1023), 24 bytes to 15:
 *
 *   AA AA AA 80 00 2A AA AA AA AA 80 00 2A 22 AA AA AA AA AA AA AA AA AA AA
 *   FE AA                 -> AA three times (FE is -2)
 *   02 80 00 2A           -> three bytes as they are
 *   FD AA                 -> AA four times
 *   03 80 00 2A 22        -> four bytes as they are
 *   F7 AA                 -> AA ten times
 *
 * A run of two is not worth a count of its own in the middle of
 * literals (two bytes either way), so it stays literal there; that is
 * why "AA AA" inside a literal stretch costs nothing extra. At worst,
 * incompressible bytes grow by one byte in 128. *)

val encode : Bytes.t -> Bytes.t

(* [decode s ~pos ~len]: [len] bytes decoded from [s] starting at
 * [pos], and the position after what was read -- the rows of a
 * picture follow each other with no separator, so the reader knows
 * where a row ends only by counting what it has produced *)
val decode : Bytes.t -> pos:int -> len:int -> Bytes.t * int
