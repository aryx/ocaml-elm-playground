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
 * it is if either does not parse *)
val resolve : string -> string -> string

(* a URL and its #fragment, apart *)
val split_fragment : string -> string * string option

(* a URL and its ?query, apart *)
val split_query : string -> string * string option

(* [s] begins with [prefix] *)
val starts_with : string -> string -> bool
