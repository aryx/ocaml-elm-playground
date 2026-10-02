(* Brotli: decoding DEFLATE pushed as far as Huffman codes go -- a code
   chosen by what came before, and a dictionary every decoder has.

   Where it stands among the others here. DEFLATE (Inflate.mli) is LZ77
   matches written with Huffman codes (Huffman.mli). At Google in
   Zürich, Jyrki Alakuijala first squeezed the format itself: Zopfli
   (2013, with Lode Vandevenne) is an encoder that searches an hour for
   the best DEFLATE stream, and gains 5%. The format was the limit, so
   with Zoltán Szabadka he made a new one, first for the fonts of web
   pages (WOFF2, 2013), then for everything a web server sends: Brotli
   (2015; RFC 7932, 2016; Brötli and Zöpfli are Swiss breads), HTTP's
   "Content-Encoding: br", read by every browser since 2017.

   Yann Collet's Zstandard (Zstd.mli) is of the same year and went the
   other way. Zstandard changed the *coder* -- FSE's fractions of a bit
   -- and arranged its streams for the processor: it decodes several
   times faster than either. Brotli kept Huffman's whole bits and
   changed the *model*, what the codes are made from: at its best
   setting it makes web text smaller than Zstandard does, small files
   above all, decodes at about DEFLATE's speed, and takes very long to
   encode. Hence their places: Brotli for pages, scripts and styles
   compressed once and served a million times, Zstandard for the rest.

   What it keeps of DEFLATE: matches and literals; canonical Huffman
   codes sent as their lengths, the lengths themselves coded (with 16
   and 17 for repeats); codes with extra bits; bits least significant
   first; the copy going one byte at a time over what it just wrote.
   What changed, and why:

   1. A window of up to 16 MB (32 KB in DEFLATE).

   2. Commands. DEFLATE codes "a literal" or "a length" one at a time.
      A Brotli *command* is "insert so many literals, then copy so many
      bytes from so far back", and the two lengths are *one* symbol of
      one alphabet of 704: they go together (after few literals, a long
      copy), and a joint code says so in fewer bits than two. The
      symbols under 128 mean besides "the same distance as last time",
      with no distance sent.

   3. The last distances, as Zstandard's repeated offsets: the distance
      symbols 0 to 3 are the four last ones, 4 to 15 the last or the
      one before, less or plus 1, 2, 3 -- records of nearly the same
      length.

   4. Context modelling, the idea that is only here. In English "u"
      follows "q", a capital follows ". ": the next byte depends on the
      ones before. The adaptive compressors (PPM, Cleary and Witten,
      1984) exploit it with a table of counts for each context, slowly.
      Brotli does it with static Huffman codes: the two bytes before a
      literal give one of 64 *contexts* ([context]: for text, "after a
      space", "after a lowercase vowel", "after a digit"...), and a
      *context map* sent in the header says which of the block's
      literal codes each context uses -- the encoder having grouped the
      contexts that behave alike, so that a few codes serve 64
      contexts. The distances have 4 contexts, the copy's length: a
      short copy is from nearby.

   5. Block switches. A file is not one thing: a web page is markup,
      then a script, then text. The literals of a meta-block are cut
      into *blocks*, each of a *type*, and the type chooses the row of
      the context map (the codes); a block's end says the next one's
      type and length. The commands and the distances are cut into
      blocks of their own, independently. One header, many codes.

   6. A static dictionary (Brotli_dictionary.mli): a copy from further
      back than there is data is a word that the format itself ships.

   And no magic number, no checksum, no length: a Brotli stream is not
   a file format, HTTP says what it is. A damaged stream may decode to
   wrong bytes.

   The format. A stream is a window size, then *meta-blocks*, of at
   most 16 MB each:

     WBITS            1, 4 or 7 bits: the window is 2^WBITS - 16 bytes
     meta-blocks      each:
       ISLAST         1 bit; if set, 1 bit more: an empty last one?
       MNIBBLES       2 bits: the length takes 4, 5 or 6 nibbles (or
                      "no data": bytes to skip, for someone else)
       MLEN - 1       the bytes this meta-block makes
       ISUNCOMPRESSED 1 bit, unless last: the bytes as they are follow
       the header     for the literals, the commands, the distances:
                        how many block types, 1 to 256; if several, the
                        codes of the types and of the counts, and the
                        first block's count
                      NPOSTFIX and NDIRECT, 2 and 4 bits: how the
                        distances are coded
                      a context mode for each literal block type, 2
                        bits: 0 and 1 the last byte's low or high 6
                        bits, 2 UTF-8 text, 3 signed numbers
                      how many literal codes, and the literals'
                        context map; the same for the distances
                      the codes: literals, commands, distances
       the commands   each: its symbol; the extra bits of its insert
                      length, of its copy length; its literals; its
                      distance and extra bits -- and before any of the
                      three, a block switch when its block has ended

   A prefix code is sent in one of two ways: *simple*, 1 to 4 symbols
   named (a code of a single symbol then takes no bit at all); or
   *complex*, the lengths as in DEFLATE's dynamic blocks.

   The first worked example, "hi", not compressed:

     10 00 10 68 69 03

     10 00 10   0          WBITS: 16 (one bit)
                 0         ISLAST: no
                  00       MNIBBLES: 4
                    1 0... MLEN - 1 = 1, in 16 bits
                    1      ISUNCOMPRESSED (bit 4 of the third byte)
     68 69      h i
     03         ISLAST 1, and empty 1

   The second, "Hello, world." in 14 bytes without a letter of it, made
   by hand: two commands, each a word of the dictionary changed.

     82 01 00 00 04 40 0C 52 2B 7A 5A E5 00 01

     bits, in the order read              what
     0                                    WBITS 16
     1, 0                                 last, not empty
     00, 12 in 16 bits                    MNIBBLES 4, MLEN - 1 = 12
     0, 0, 0                              one block type each
     00, 0000                             NPOSTFIX 0, NDIRECT 0
     00                                   context mode 0
     0, 0                                 one literal code, one for distances
     1, 0, 0 in 8 bits                    literals: simple, 1 symbol: 0 (not used)
     1, 0, 131 in 10 bits                 commands: simple, 1 symbol: 131
     1, 1, 43 and 40 in 6 bits            distances: simple, 2 symbols:
                                            40 is the code 0, 43 the code 1

     command 131, in no bit: cell 2 (insert codes from 0, copy codes
       from 0, a distance follows), insert code 0, copy code 3:
       insert 0 literals, copy 5 bytes
     1, 10963 in 14 bits                  distance symbol 43: 14 extra
                                            bits, 3 * 2^14 - 4 + 10963 + 1 = 60112
       60112 back, when nothing is written: a word of 5 bytes, its id
       60112 - 0 - 1 = 60111 = 58 * 1024 + 719: "hello", transform 58
       (the capital, then ", ")           Hello,_
     command 131 again, in no bit
     0, 4110 in 13 bits                   distance symbol 40: 13 extra
                                            bits, 2 * 2^13 - 4 + 4110 + 1 = 20491
       20491 back, when 7 bytes are written: the id 20491 - 7 - 1 =
       20483 = 20 * 1024 + 3: "world", transform 20 ("." after)
                                          world.

   Checks what a decoder must: codes that are complete, symbols in their
   alphabets, runs and repeats that stay in their tables, lengths that
   end exactly at the meta-block's, a distance of zero, padding bits
   that are zeros, data ending early or going on. Doesn't read the
   later "large window" and "shared dictionary" variants (RFC 9841).
   Like Inflate, made to be read: a bit at a time.

   References: Jyrki Alakuijala and Zoltán Szabadka, RFC 7932, "Brotli
   Compressed Data Format" (2016); Jyrki Alakuijala, Andrea Farruggia,
   Paolo Ferragina, Eugene Kliuchnikov, Robert Obryk, Zoltán Szabadka
   and Lode Vandevenne, "Brotli: A General-Purpose Data Compressor",
   ACM Transactions on Information Systems 37 (2018); John Cleary and
   Ian Witten, "Data Compression Using Adaptive Coding and Partial
   String Matching", IEEE Transactions on Communications 32 (1984)
   (PPM, contexts); Jon Bentley, Daniel Sleator, Robert Tarjan and
   Victor Wei, "A Locally Adaptive Data Compression Scheme",
   Communications of the ACM 29 (1986) (move-to-front, which the
   context maps are sent with); Jacob Ziv and Abraham Lempel (1977) and
   David Huffman (1952), as in Inflate.mli and Huffman.mli. *)

(* [context ~mode ~p1 ~p2]: the context of a literal, 0 to 63, from the
 * byte before it [p1] and the one before that [p2] (0 at the start of
 * the stream), in the block's [mode]:
 *   0  the low 6 bits of [p1]
 *   1  its high 6 bits
 *   2  UTF-8 text: 4 bits for what [p1] is (a space, a quote, a digit,
 *      an uppercase vowel, a lowercase consonant...), 2 for [p2]
 *      (nothing, punctuation, uppercase or digit, lowercase)
 *   3  signed numbers: how big each of the two is, 3 bits each *)
val context : mode:int -> p1:int -> p2:int -> int

(* [decompress ?dictionary s]: the bytes of the Brotli stream [s].
 * [dictionary] is the static dictionary, Brotli_words.bytes (a library
 * of its own, compression_brotli_words: 120 KB that a program links
 * only if it asks); without it, a stream that uses a word of the
 * dictionary is refused. Raises Failure on a corrupt stream. *)
val decompress : ?dictionary:string -> string -> string
