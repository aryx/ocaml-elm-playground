(* Brotli's static dictionary: words every decoder already has, and 121
   ways of changing one.

   LZ77 (Inflate.mli) copies from what was already sent, so the start
   of a message, and a short message whole, has nothing to copy from:
   a web page of 2 KB is mostly "first times". zlib and Zstandard
   (Zstd.mli) let the two sides agree beforehand on a dictionary of
   their own; Brotli (Brotli.mli), made for the Web, ships one with the
   format: 122,784 bytes that Google's Jyrki Alakuijala and Zoltán
   Szabadka drew from a corpus of web pages -- words of English,
   Spanish, Chinese, Hindi, Russian and Arabic, and pieces of HTML and
   JavaScript.

   The words have 4 to 24 bytes, and those of one length are stored
   together with nothing between them, so a word is found by its length
   and its number alone:

     length   4     5     6     7     8  ...  22   23   24
     words  1024  1024  2048  2048  1024 ...  64   32   32     (13,504 in all)

     timedownlifeleftbackcodedatashow ...  firstvideolightworldmedia ...
     ^ the words of 4 bytes, at 0          ^ of 5 bytes, at 4 * 1024

   The number of words of a length is a power of two, so that a word's
   number is so many bits of an *id*, and the bits above them choose a
   *transform*: a prefix, a change to the word, a suffix. 13,504 words
   become 1,633,984 strings:

     id = transform * (words of that length) + number

     transform 0    ""  word          ""       world
     transform 1    ""  word          " "      world_
     transform 5    ""  word          " the "  world the_
     transform 9    ""  Capitalized   ""       World
     transform 12   ""  less its last byte ""  worl
     transform 44   ""  IN CAPITALS   ""       WORLD
     transform 58   ""  Capitalized   ", "     World,_
     transform 72   ".com/" word      ""       .com/world

   The changes are: none; capitals for the first character or for all
   (the RFC's "Ferment", the pun of a format named after a bread roll;
   it knows a to z, and guesses for the characters of 2 and 3 bytes of
   UTF-8); the first 1 to 9 bytes left out; the last 1 to 9 left out.

   How a stream names one, with no new syntax: a copy whose distance
   reaches *further back than there is data* is a word, its length the
   copy's length, its id the distance less the furthest distance
   possible, less one (Brotli.mli's worked example).

   The worked examples: "world" is the word 3 of length 5, so
   [word ~length:5 ~id:3] is "world", and with transform 20 (a "."
   after it), [~id:((20 * 1024) + 3)] is "world."; "hello" is the word
   719, and transform 58 makes it "Hello, ".

   The bytes are not here: [word] is given them, Brotli_words.bytes, a
   library of its own that a program links only if it wants them.

   Reference: Jyrki Alakuijala and Zoltán Szabadka, RFC 7932, "Brotli
   Compressed Data Format" (2016), section 8 and Appendices A and B. *)

(* the dictionary's length in bytes, 122,784 *)
val size : int

(* what a transform does to the word, between its prefix and suffix *)
type change =
  | Identity
  (* the first character to its capital; all of them *)
  | Ferment_first
  | Ferment_all
  (* the word less its first, its last, 1 to 9 bytes *)
  | Omit_first of int
  | Omit_last of int

(* the 121 transforms: prefix, change, suffix (RFC 7932, Appendix B) *)
val transforms : (string * change * string) array

(* [transform t w]: the word [w] changed by the transform [t]. Raises
 * Failure if there is no transform [t]. *)
val transform : int -> string -> string

(* [word dictionary ~length ~id]: the string that a length and an id
 * name, [dictionary] being Brotli_words.bytes. Raises Failure if
 * [dictionary] hasn't the dictionary's size, if [length] is not 4 to
 * 24, or if [id] names no transform. *)
val word : string -> length:int -> id:int -> string
