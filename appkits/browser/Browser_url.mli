(* Browser_url: what a browser does with the URLs of a page -- a link
 * resolved against the page it is on, and its parts the server never
 * sees (the #fragment) or that a form fills (the ?query) apart.
 *
 *   base  http://info.cern.ch/hypertext/WWW/TheProject.html
 *   href  "Help.html#people"
 *   -> http://info.cern.ch/hypertext/WWW/Help.html#people
 *   -> fetched: http://info.cern.ch/hypertext/WWW/Help.html
 *      then scrolled to "people" (the fragment is the browser's)
 *
 * Over networking's Url (RFC 3986); notes_browser.md section 1. *)

(* a link's href, relative to the page it is on (RFC 3986); the href as
 * it is if either does not parse, or if it is a data: URL *)
val resolve : string -> string -> string

(* a URL and its #fragment, apart *)
val split_fragment : string -> string * string option

(* a URL and its ?query, apart *)
val split_query : string -> string * string option

(* a data: URL's bytes (RFC 2397: "data:image/svg+xml,<svg ...>", its
 * payload percent-encoded, or ";base64,"): a picture inside a style
 * sheet, had at once; None for any other URL *)
val data_url : string -> string option

(* [s] begins with [prefix] *)
val starts_with : string -> string -> bool
