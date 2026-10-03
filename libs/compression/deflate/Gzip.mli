(* gzip: a DEFLATE stream with a header, a checksum and a length.

   Jean-loup Gailly and Mark Adler's gzip (1992) replaced Unix's
   compress, whose LZW was patented; its file format (RFC 1952) is the
   one of .gz files, of .tar.gz archives, and of the Web: what a
   server answers "Content-Encoding: gzip" with when a browser has sent
   "Accept-Encoding: gzip". It is zlib's idea (Zlib.mli) with a longer
   header and another checksum, ten bytes before the DEFLATE data
   (Inflate), eight after:

     1F 8B   the magic number
     CM      the method, 8 = deflate
     FLG     what follows the header, a bit each:
               1 FTEXT     "probably text" (information only)
               2 FHCRC     2 bytes, the header's CRC-16
               4 FEXTRA    2 bytes of length, then so many bytes
               8 FNAME     the file's name, ended by a 0
              16 FCOMMENT  a comment, ended by a 0
     MTIME   4 bytes, the file's date (seconds since 1970), or 0
     XFL     2 = compressed hardest, 4 = fastest (information only)
     OS      3 = Unix, 255 = unknown
     ...     FLG's fields, in the order extra, name, comment, CRC-16
     ...     the DEFLATE blocks
     the CRC-32 (Crc32.mli) of the decompressed bytes, little-endian
     their length modulo 2^32, little-endian

   That is a "member"; a gzip stream may be several, one after the
   other (cat a.gz b.gz), which decompress to their texts one after the
   other.

   The worked example, "hi" in one stored block, as Zlib.mli's:

     1F 8B 08 00 | 00 00 00 00 | 00 FF | 01 02 00 FD FF 68 69 |
     ^ deflate,    ^ no date     ^ XFL,  ^ BFINAL 1, stored; LEN 2,
       no field                    OS      NLEN = not LEN, 'h' 'i'
     AC 2A 93 D8 | 02 00 00 00
     ^ CRC-32 ("hi") = D8932AAC    ^ 2 bytes

   zlib's stream is not gzip's: HTTP's "deflate" coding is the first
   (Zlib.decompress), its "gzip" this one.

   Reference: Peter Deutsch, RFC 1952, "GZIP file format specification
   version 4.3" (1996). *)

(* [decompress s]: the bytes of the gzip stream [s], all its members.
 * Raises Failure on a bad header, a corrupt DEFLATE stream, a wrong
 * CRC-32 or length, or bytes after the last member that are not one. *)
val decompress : string -> string

(* [compress s]: [s] as a gzip stream of one member, the worked
 * example's ten bytes (no field, no date) then Deflate.deflate's block
 * then the CRC-32 and the length *)
val compress : string -> string
