(* Base64: bytes written as ordinary characters.

   A PNG is bytes, and bytes are awkward to carry around in text: in a
   source file, in a URL, in an e-mail. Base64 is the usual answer --
   take the bits 6 at a time instead of 8, and write each group as one
   of 64 characters, A-Z a-z 0-9 + / :

       3 bytes                24 bits                4 characters
       +--------+--------+--------+
       |01001101|01100001|01101110|     "Man", the example of
       +--------+--------+--------+     every explanation since
       |010011|010110|000101|101110|    RFC 1521 (1993)
       +------+------+------+------+
          19     22      5      46
          T      W       F      u       -> "TWFu"

   3 bytes in, 4 characters out: a third bigger, and worth it when
   what carries them only takes text. If the last group is short, it
   is padded with '=' (one or two).

   Here it is what lets a texture live inside the program: a dune rule
   turns minecraft.png into an OCaml string of base64 (like
   graphics/font/dune does for the Hershey font), the game hands that
   string to Playground3d.embedded_texture, and the backends turn it
   back into pixels -- [decode] here for the ones that decode images
   themselves, and the browser's own "data:" URL for the WebGL one,
   which wants exactly this encoding.

   Reference: RFC 4648, "The Base16, Base32, and Base64 Data
   Encodings" (Simon Josefsson, 2006). *)

(* [decode s]: the bytes [s] stands for. Padding ('='), spaces and
 * newlines are ignored, so a string cut into lines decodes the same;
 * any other character is skipped too, rather than raising -- the
 * strings this decodes are generated, not typed. *)
val decode : string -> string

(* [encode s]: [s] as base64, no line breaks. (The build-time
 * generator has its own copy, since it runs before this library is
 * built; this one is here for the tests, and for symmetry.) *)
val encode : string -> string
