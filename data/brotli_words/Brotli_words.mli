(* Brotli's static dictionary, the bytes themselves: 13,504 words and
   fragments of six languages, of HTML and of JavaScript, 122,784 bytes
   that every Brotli decoder carries (Brotli_dictionary.mli says how
   they are laid out and used; RFC 7932's Appendix A prints them).

   Apart from the library compression so that they are not linked by
   default: a program adds this library, compression_brotli_words, and
   writes

     Brotli.decompress ~dictionary:Brotli_words.bytes s

   Without it Brotli.decompress still decodes every stream that doesn't
   use a word of the dictionary, and refuses the others. *)

val bytes : string
