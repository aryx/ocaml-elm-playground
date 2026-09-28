(* Mime: everything that is not a short English letter, put into mail
   without changing mail (Nathaniel Borenstein and Ned Freed, RFC 1341,
   1992; RFCs 2045-2047, 1996).

   A message stays what Mail.mli says -- headers, an empty line, lines
   of 7-bit text -- and MIME adds a few header fields saying how to
   read the body:

     Content-Type: multipart/mixed; boundary="xyz"     what it is: a
                                                       type/subtype, and
                                                       parameters
     Content-Transfer-Encoding: base64                 how its bytes were
                                                       made into lines
     Content-Disposition: attachment; filename="a.png" to show, or to save

   A multipart body is cut by its boundary into parts, each a message of
   its own (headers, empty line, body) -- and a part can be multipart
   again, so a message is a tree whose leaves are the text and the
   attachments:

       --xyz                           <- a part starts
       Content-Type: text/plain
                                       <- (a part with no header is text)
       Here are the photos.
       --xyz
       Content-Type: image/png
       Content-Transfer-Encoding: base64

       iVBORw0KGgoAAAANSUhEUgAA...     <- the bytes, as lines of base64
       --xyz--                         <- the last part has ended

   The two encodings: *base64* (core's Base64: three bytes as four
   letters, for what is not text) and *quoted-printable*, for text that
   is mostly ASCII -- it stays readable, and what is not ASCII is "="
   and two hex digits:

       caf=C3=A9 au lait, and a line too long to be sent as it is, so=
        cut here                 -> "café au lait, and a line too long
                                     to be sent as it is, so cut here"

   (é is two bytes in UTF-8, C3 A9; the "=" at a line's end is a *soft*
   line break, removed.) And a header, which has no Content-Type of its
   own, holds non-ASCII in *encoded words* (RFC 2047):
   "=?utf-8?Q?caf=C3=A9?=" is "café", "=?utf-8?B?Y2Fmw6k=?=" too (Q is
   quoted-printable with "_" for a space, B is base64).

   Text is given back as UTF-8: ISO-8859-1 (and Windows-1252, read as
   it) converted, UTF-8 and US-ASCII as they are.

   Worked examples (checked by the tests): the quoted-printable above;
   the two encoded words; [parameters "multipart/mixed;
   boundary=\"xyz\""] is ("multipart/mixed", [("boundary", "xyz")]);
   the multipart above is two leaves, the text and a picture.png of
   image/png, its bytes decoded.

   References: RFC 2045 (the fields, the encodings: 6.7
   quoted-printable, 6.8 base64), RFC 2046 (the media types, 5.1
   multipart), RFC 2047 (encoded words), RFC 2183 (Content-Disposition);
   Nathaniel Borenstein, "The Hidden Value of MIME" (the first
   attachment, 1992, was a picture of his barbershop quartet). *)

(* "text/plain; charset=utf-8": the type, lowercased, and the
 * parameters, their names lowercased and their values unquoted *)
val parameters : string -> string * (string * string) list

(* a message's (or a part's) Content-Type: text/plain if it says none,
 * message/rfc822 inside a multipart/digest *)
val content_type : ?digest:bool -> Mail.t -> string * (string * string) list

(* the parts of a multipart body, each a message; [] when it is not
 * multipart *)
val parts : Mail.t -> Mail.t list

val quoted_printable_decode : string -> string

(* lines of at most 76 characters, soft breaks where needed *)
val quoted_printable_encode : string -> string

(* the body's bytes, its Content-Transfer-Encoding undone *)
val decoded : Mail.t -> string

(* [to_utf8 charset s] *)
val to_utf8 : string -> string -> string

(* a header's encoded words decoded, to UTF-8 *)
val decode_words : string -> string

(* a header value as one encoded word if it is not ASCII *)
val encode_words : string -> string

(* a leaf of the tree: its type, its file name if it has one, its
 * bytes decoded (and, for text, converted to UTF-8), and the part
 * itself *)
type leaf = { mime : string; filename : string option; data : string; part : Mail.t }

(* the leaves, depth first; a message/rfc822 part is a leaf (a message
 * forwarded whole, or in a digest), [data] its text *)
val leaves : Mail.t -> leaf list

(* what to read: the text leaves that are not attachments, one after
 * the other; an enclosed message introduced by its From: and Subject: *)
val text : Mail.t -> string

(* the leaves to save: those with a file name, or not text *)
val attachments : Mail.t -> leaf list

(*****************************************************************************)
(* {1 Writing} *)
(*****************************************************************************)

(* The other way, for a message being sent: each part a Mail.t, its
   headers saying how to read it, as the reading side above expects. *)

(* a text part, UTF-8: as it is if it is ASCII (7bit), else in
 * quoted-printable *)
val text_part : string -> Mail.t

(* [attachment ~filename bytes]: a part in base64, cut in lines of 76,
 * its type from the name's extension (.png, .gif, .jpg, .txt, .mbox;
 * application/octet-stream else) *)
val attachment : filename:string -> string -> Mail.t

(* the type an extension gives *)
val type_of_filename : string -> string

(* [multipart ~boundary parts]: the headers and the body of a
 * multipart/mixed holding [parts] -- the boundary must appear in none
 * of them (a caller makes it unique, as mailers do, from the time and
 * a counter) *)
val multipart : boundary:string -> Mail.t list -> (string * string) list * string
