(* Zstandard: decoding DEFLATE's successor, LZ77 matches again, written
   for the processors and the memory of 2015.

   Where it stands among the others here. Ziv and Lempel's two papers
   gave two families. LZ78 builds a dictionary of the sequences seen
   and sends their numbers: LZW (Lzw.mli), Unix's compress and GIF. LZ77
   sends "copy [length] bytes from [distance] back" and won, because of
   what Phil Katz added to it in 1993: DEFLATE (Inflate.mli) writes the
   literals, lengths and distances with Huffman codes (Huffman.mli), and
   zip, gzip (Gzip.mli), zlib (Zlib.mli), PNG and HTTP made it the
   compression of everything for twenty years. After it, formats traded
   speed for size or size for speed: Igor Pavlov's LZMA (7-Zip, xz,
   1998) compresses much better, with a window of megabytes and an
   arithmetic coder deciding bit by bit, and is ten times slower;
   Yann Collet's LZ4 (2011) has no entropy coder at all, only matches
   in whole bytes, and decodes at the speed of memory. Collet's
   Zstandard (2015, at Facebook from 2016; RFC 8478 then 8878) is the
   one that does not trade: it decodes three to five times faster than
   DEFLATE *and* compresses better, at every level. It is now in the
   Linux kernel (btrfs, squashfs, the kernel's own image), in the
   packages of Arch, Debian and Fedora, and in HTTP ("Content-Encoding:
   zstd", in Chrome and Firefox since 2024).

   It is still DEFLATE's two passes, and the decoder still undoes
   Huffman then LZ77, the copy still going one byte at a time over what
   it just wrote (Inflate.mli's "abcabcabcabc"). What changed, and why:

   1. A window of megabytes, where DEFLATE's distances stop at 32 KB, a
      1993 PC's memory. A match may be megabytes back: an offset's code
      is simply its number of extra bits, with no last code.

   2. Literals apart from matches. DEFLATE has one alphabet for both
      and one stream: read a code, *test* whether it is a literal or a
      length, go on -- a branch the processor guesses wrong every few
      bytes. A zstd block is two sections: all its literals together,
      then its *sequences*, each "[literal_length] literals, then
      [match_length] bytes from [offset] back", so each section is
      decoded by a loop of its own that never asks what comes next.

   3. Two entropy coders, each where it is best. The sequences' three
      codes are written with FSE (Fse.mli), which gives a symbol a
      fraction of a bit: codes are few and very unequal (most offsets
      repeat, most lengths are small), where Huffman's whole bits waste
      the most. The literals keep Huffman codes, the fastest there are,
      of at most 11 bits so that a decoder's table fits in the cache --
      and in *four streams* decoded side by side, since one Huffman
      stream can't be read faster than a symbol after the other.

   4. Repeated offsets. The three last distances used are kept, and the
      offset's values 1, 2, 3 mean "that one again": the fields of
      records, the columns of a table, a text matched but for one
      letter all repeat a distance, now for a fraction of a bit.

   5. Tables only when they pay. A DEFLATE block has fixed codes or
      sends its own; each zstd table (literal lengths, offsets, match
      lengths) is one of four: *predefined* (given by the format, good
      for small data), *RLE* (a single symbol, no bit at all), *FSE*
      (sent, as counts), or *repeat* (the previous block's). The
      Huffman code may be the previous block's too.

   6. Whole bytes and little-endian fields where DEFLATE counts bits,
      and bit streams read backwards (Fse.mli says why).

   7. XXH64 (Xxhash.mli) as the checksum, optional, where gzip has
      CRC-32 and zlib Adler-32: faster than either.

   (And dictionaries, made beforehand from samples so that even a small
   message has something to match: not read here.)

   The format. A file is frames, one after the other; a frame is a
   header and blocks, of at most 128 KB each:

     28 B5 2F FD        the magic number (FD2FB528, little-endian)
     descriptor         1 byte: bits 7-6 how many bytes the content's
                        size takes (0, 2, 4, 8); 5 "single segment": no
                        window byte, a size of at least 1 byte; 2 a
                        checksum follows the blocks; 1-0 the bytes of
                        the dictionary's number (0, 1, 2, 4)
     window             1 byte, how far back a match may go (not needed
                        here, where the whole output is kept)
     dictionary, size   as the descriptor says
     blocks             each 3 bytes of header: bit 0 the last block?,
                        bits 2-1 its type, the rest its size
                          0 raw: the bytes as they are
                          1 RLE: one byte, [size] times
                          2 compressed: literals, then sequences
     checksum           4 bytes, the low half of the content's XXH64

     a compressed block:

     literals  | 1 to 5 bytes: bits 1-0 the type -- 0 raw, 1 RLE, 2
               |   Huffman codes, 3 the previous block's codes; bits
               |   3-2 how wide the sizes that follow, and one stream
               |   or four
               | the Huffman code, as weights: a weight w is a code of
               |   max + 1 - w bits, 0 no code; 4 bits each, or
               |   FSE-coded, and the last one left out (it is what
               |   completes the others)
               | the sizes of the first three streams, the streams
     sequences | how many, in 1 to 3 bytes
               | a byte: bits 7-6, 5-4, 3-2 how each table comes
               |   (predefined, RLE, FSE, repeat); the tables sent
               | one backwards stream: the three first states, then for
               |   each sequence the extra bits of its offset, of its
               |   match length, of its literal length, and the bits
               |   that take the three states to the next sequence

   A frame whose magic number is 184D2A50 to 5F is "skippable": 4 bytes
   of length, then data for someone else.

   The first worked example, "hi" in a raw block, as Gzip.mli's:

     28 B5 2F FD | 04 | 58 | 11 00 00 | 68 69 | FA 38 26 EA
                   ^ a checksum  ^ last, raw,   ^ XXH64 ("hi") ends
                        ^ window   2 bytes        in EA2638FA

   The second, "hello hello hello hello\n" (zstd --no-check, from a
   pipe): 24 bytes in 22, one sequence, the predefined tables.

     28 B5 2F FD | 00 | 58 | 6D 00 00 | 38 68 65 6C 6C 6F 20 0A |
                             ^ last, compressed, 13 bytes
                                        ^ raw literals, 7: "hello \n"
     01 | 00 | 99 4B 11
     ^ one sequence
          ^ three predefined tables

     the stream, backwards:   11       4B        99
                           0001 0001 0100 1011 1001 1001
                              ^ the mark
                                000 101             literal lengths: state 5, code 6
                                       0 0101       offsets: state 5, code 3
                                             1 1001 1    match lengths: state 51, code 14
                                                     001   the offset's 3 extra bits

     literal length: code 6 is 6, no extra bit
     match length:   code 14 is 17, no extra bit
     offset:         code 3 is 2^3 + 3 extra bits = 8 + 1 = 9; above 3,
                     so not a repeated one: a distance of 9 - 3 = 6

     6 literals         hello_
     17 bytes, 6 back   hello_hello_hello_hello     (the copy overlaps)
     the literal left   \n

   Huffman in a mirror. zstd's Huffman code is canonical as DEFLATE's
   is, rebuilt from the lengths alone, but counted the other way: the
   *longest* codes get the lowest numbers, where DEFLATE starts with
   the shortest. With A 1 bit, B 2, C 3, D 3:

     DEFLATE   A 0    B 10    C 110   D 111
     zstd      A 1    B 01    C 000   D 001

   Flip every bit of zstd's and they are DEFLATE's codes for the
   alphabet backwards (D C B A: C 111 and D 110). So the literals are
   decoded by Huffman.of_lengths and Huffman.decode as they are, the
   symbols numbered from the end and each bit flipped.

   Checks what a decoder must: the magic number, the reserved bits, the
   content's size and checksum when the frame has them, a distance
   further back than the frame's data, streams not read exactly to
   their first bit, data ending early. Doesn't read dictionaries (a
   frame that names one is refused) nor the formats before 1.0, and
   doesn't hold matches to the window the frame announces. Like
   Inflate, made to be read: a bit at a time where zstd looks up eleven,
   megabytes a second where zstd does a gigabyte.

   References: Yann Collet and Murray Kucherawy, RFC 8878, "Zstandard
   Compression and the 'application/zstd' Media Type" (2021); Jacob Ziv
   and Abraham Lempel, "A Universal Algorithm for Sequential Data
   Compression", IEEE Transactions on Information Theory 23 (1977) (and
   "Compression of Individual Sequences via Variable-Rate Coding", 24
   (1978), LZ78); David Huffman, "A Method for the Construction of
   Minimum-Redundancy Codes", Proceedings of the IRE 40 (1952); Jarek
   Duda, "Asymmetric numeral systems: entropy coding combining speed of
   Huffman coding with compression rate of arithmetic coding",
   arXiv:1311.2540 (2013); Yann Collet's blog,
   fastcompression.blogspot.com (LZ4, FSE and zstd as he made them);
   zstd's doc/educational_decoder/zstd_decompress.c (this module
   follows it, as Inflate follows puff.c). *)

(* [decompress s]: the bytes of the zstd stream [s], all its frames,
 * the skippable ones skipped. Raises Failure on a corrupt stream, a
 * wrong size or checksum, a frame that needs a dictionary, or bytes
 * that are not a frame. *)
val decompress : string -> string
