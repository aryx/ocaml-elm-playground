(* Charset: which encoding a page's bytes are in, and the page in UTF-8.

   HTML is text, but what arrives is bytes, and the bytes do not say how
   they encode the text. "é" is one byte in Latin-1 (ISO 8859-1, the
   web's default until HTML5), two in UTF-8:

     "café"   Latin-1   63 61 66 E9
              UTF-8     63 61 66 C3 A9

   Read the UTF-8 bytes as Latin-1 and C3 A9 are two characters, "Ã©":
   "cafÃ©", mojibake, the web's commonest bug for twenty years. So
   before anything else, a browser decides the encoding, and decodes
   the page into the one it works in -- UTF-8 here, as in OCaml's
   strings everywhere else in the repository.

   Deciding, in the WHATWG's order ("Encoding", and HTML's "determining
   the character encoding"), simplified:

     1. a byte order mark: EF BB BF starts a UTF-8 file;
     2. the HTTP header: "Content-Type: text/html; charset=ISO-8859-1";
     3. the page itself, near its top: <meta charset="utf-8">, or the
        older <meta http-equiv="Content-Type" content="text/html;
        charset=utf-8"> (a page naming its own encoding in the encoding
        it names: it works because both are ASCII there);
     4. a guess: bytes that are valid UTF-8 and not all ASCII are UTF-8
        (a Latin-1 text is almost never valid UTF-8 by accident: an é
        followed by a letter is not a UTF-8 sequence); anything else is
        the default, Windows-1252.

   **Windows-1252 for Latin-1**: the WHATWG decodes a page labelled
   ISO-8859-1 as Windows-1252, and so do we. The two agree except on
   bytes 80 to 9F, control characters in Latin-1 that no page meant,
   and in Windows-1252 the characters Word typed: the curly quotes
   (93 and 94: "“" and "”"), the dashes (96, 97), "€" (80), "…" (85).
   Pages labelled Latin-1 in the 1990s were written in Windows-1252 on
   Windows, and a browser that believed the label showed boxes where
   the quotes were.

     bytes 93 68 69 94, labelled "iso-8859-1"  ->  "“hi”"

   Not done: the other encodings (Shift_JIS, GB18030, KOI8-R... the
   WHATWG lists 39; ours are the two a Mosaic-era page and a modern one
   need), UTF-16, and the spec's statistical guessers.

   Reference: WHATWG, "Encoding" (encoding.spec.whatwg.org), section 4.2
   (the labels) and the index windows-1252; WHATWG HTML, 13.2.3
   "The input byte stream" (13.2.3.2, the prescan of <meta>). *)

type t = Utf_8 | Windows_1252

(* an encoding by one of its labels, case and surrounding spaces
 * ignored: "utf-8", "utf8", "unicode-1-1-utf-8"; "iso-8859-1",
 * "latin1", "us-ascii", "windows-1252", "cp1252" and the rest of the
 * WHATWG's labels for it; None for what we don't decode *)
val of_label : string -> t option

(* the charset parameter of a Content-Type:
 * "text/html; charset=ISO-8859-1" -> Some Windows_1252 *)
val of_content_type : string -> t option

(* a <meta> naming the encoding, in the first 1024 bytes (the
 * WHATWG's prescan, simplified: the first "charset=" inside a <meta>
 * tag, quoted or not) *)
val of_meta : string -> t option

(* the bytes are UTF-8: every sequence well formed, no overlong
 * encoding, no surrogate, nothing above U+10FFFF *)
val is_utf_8 : string -> bool

(* the encoding, in the order above; [content_type] is the header's
 * value, if there was one *)
val detect : ?content_type:string -> string -> t

(* the bytes decoded into UTF-8: Windows-1252 byte by byte; UTF-8 kept,
 * a byte order mark dropped, and each malformed sequence replaced by
 * U+FFFD ("�", the replacement character) *)
val to_utf_8 : t -> string -> string

(* detect, then to_utf_8: what a browser does with a page's bytes *)
val decode : ?content_type:string -> string -> string

(* the Unicode code point of a Windows-1252 byte (0x93 -> 0x201C; the
 * five bytes it leaves undefined, 81 8D 8F 90 9D, are themselves, as
 * the WHATWG's index says), for Entities' &#147; too *)
val windows_1252 : int -> int
