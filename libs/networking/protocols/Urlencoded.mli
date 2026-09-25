(* Urlencoded: a form's fields as one string -- what a browser sends
   when a form is submitted, in the URL (GET) or as the body (POST),
   and what the server's program reads back.

     q = "café au lait", lang = "fr"
       -> q=caf%C3%A9+au+lait&lang=fr

   The fields in order, each name=value, joined by '&'. In a name or a
   value, the letters, the digits and "*-._" stay as they are, a space
   is '+', and every other byte is '%' and its two hexadecimal digits,
   upper case: "é" is its two UTF-8 bytes, %C3%A9, and a '&' or a '='
   in a value is %26 or %3D, so they only ever separate. The format of
   Mosaic's forms (1993) and of every form since, its name the
   Content-Type a POST says: application/x-www-form-urlencoded.

   GET or POST is the page's choice (<form method=post>): with GET the
   fields are the URL's query ("/search?q=caf%C3%A9+au+lait&lang=fr"),
   so they can be bookmarked, and end in the server's log; with POST
   they are the request's body, for what should not be (a password)
   or is not a question but an action (an order).

   [decode] is the other way, lenient as servers are: a '+' is a space,
   a '%' not followed by two hexadecimal digits is itself, a field with
   no '=' has the value "".

   Reference: WHATWG URL Standard, section 5 ("application/x-www-form-
   urlencoded"); HTML 2.0 (RFC 1866), section 8.2.1, the form's
   submission. *)

(* the fields, encoded and joined: the query or the body *)
val encode : (string * string) list -> string

(* the fields of a query or a body, decoded *)
val decode : string -> (string * string) list

(* one name or value, encoded (a space as '+') *)
val escape : string -> string

(* one name or value, decoded *)
val unescape : string -> string
