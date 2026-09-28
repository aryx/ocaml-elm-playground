(* Entities: the characters a page names instead of typing.

   HTML reserves "<" and "&", and a 1991 keyboard (and many a 1995
   one) had no "é". So a page can name a character, by a word or by
   its number in Unicode (in Latin-1, for the first 256, which are the
   same):

     &lt;      <          reserved: would start a tag
     &amp;     &          reserved: would start an entity
     &eacute;  é          named: HTML 2.0's Latin-1 set (RFC 1866, 1995)
     &#233;    é          by number, in decimal
     &#xE9;    é          and in hexadecimal (HTML 4)

   A reference ends at its ";". [decode] replaces every one it knows in
   a text; an unknown name ("&foo;") and an "&" that starts nothing
   ("AT&T", "fish & chips") are left as they are, which is what
   browsers do, and why pages that forgot to write "&amp;" still work.

   Worked example (the tests'):

     Caf&eacute; &amp; cr&#232;me &#147;br&ucirc;l&eacute;e&#148; &foo; AT&T
     Café & crème “brûlée” &foo; AT&T

   The two numbers in the middle are Windows-1252's curly quotes, bytes
   93 and 94, written as if they were Unicode, which they are not
   (U+0093 is a control character). Pages did it all the time --
   Word's quotes, pasted into an HTML editor on Windows -- so the WHATWG
   says to read &#128; to &#159; through Windows-1252
   (Charset.windows_1252), and we do. Other numbers that name no
   character (0, a surrogate, above U+10FFFF) become U+FFFD.

   The names are HTML 4's 252 (1997: HTML 2.0's Latin-1 ones, 96 of
   them, then the "special" set -- the quotes, dashes, the euro -- and
   the "symbols": Greek, arrows, mathematics) and XHTML's &apos;.
   HTML5 has 2,231, adding MathML's; an exercise. A name needs its ";"
   here; HTML5 accepts the old Latin-1 ones without it ("&copy 1995"),
   but not in an attribute value followed by "=" ("?a=1&copy=2" is a
   URL, not a copyright sign) -- a rule worth reading in the spec to
   see what compatibility costs.

   Reference: RFC 1866 (HTML 2.0), section 9.7.2 (the Latin-1 entity
   set); HTML 4.01, section 24 ("Character entity references in HTML
   4"); WHATWG HTML, 13.2.5 (the tokenizer's character reference
   states) and 13.5 (the named character references). *)

(* the character a name stands for, in UTF-8: "eacute" -> "é" *)
val lookup : string -> string option

(* how many names [lookup] knows (253: HTML 4's 252, and apos) *)
val count : int

(* the character of a numeric reference, in UTF-8, with the WHATWG's
 * repairs: 128-159 through Windows-1252, 0 or a surrogate or above
 * U+10FFFF as U+FFFD *)
val of_code : int -> string

(* every reference in a text replaced by its character (the text is
 * UTF-8, and so is the result) *)
val decode : string -> string
