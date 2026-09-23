(* Url: the address of a thing on the network, and the relative ones.

   A URL (Tim Berners-Lee, 1994; the grammar as it stands is RFC 3986,
   2005, which calls them URIs) is five parts, each optional but the
   path, which may be empty:

       http://elm-lang.org:80/images/turtle.gif?size=96#top
       \__/   \_____________/\_________________/ \_____/ \_/
      scheme     authority          path          query  fragment
                 = host:port

   The scheme says which protocol to speak ("http": Http.mli), the
   authority whom to speak it to (a name for DNS, or an address, and a
   port, else the scheme's default: 80 for http, 443 for https), the
   path and the query what to ask them for. The fragment is never sent:
   it is for the client, a place in the page.

   Splitting a string into the five parts is easy, because the RFC
   chose the separators so that it is: its appendix B even gives the
   regular expression that does it, for any URL, valid or not,

       {|^(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\?([^#]*))?(#(.*))?|}
             scheme        authority    path      query     fragment

   and [parse] is that regexp written out by hand: the scheme up to a
   ':' that comes before any of "/?#", the authority after "//" up to
   the next of "/?#", the path up to "?" or "#", and so on. (The tests
   check that the two agree, the regexp run by OCaml's Str.)

   The subtle part is the *relative* reference. A web page says
   <img src="../images/turtle.gif">, and a server answering "moved"
   says "Location: /new/place" (Http.mli): both mean a URL relative to
   the one they came from, the *base*, and [resolve] turns the pair
   into an absolute URL, with RFC 3986's section 5.2 algorithm: a part
   present in the reference wins, and the parts before it come from the
   base; a relative path is *merged* with the base's (the base's path
   up to its last '/', then the reference's), and "." and ".." are then
   removed as a file system would (remove_dot_segments):

       base      http://a/b/c/d;p?q
       reference ../g
       merged    /b/c/../g        (the base's path up to "c/", then "../g")
       result    http://a/b/g     (".." removed "c/")

   Worked examples (checked by the tests): all of RFC 3986's own, its
   section 5.4, 23 normal ones and 19 abnormal ones from the same base,
   e.g. "g" gives http://a/b/c/g, "//g" http://g, "?y"
   http://a/b/c/d;p?y, "../../../g" http://a/g (a ".." above the root
   stays at the root), "g?y/../x" http://a/b/c/g?y/../x (dots in a
   query are not segments); and remove_dot_segments' two traces from
   section 5.2.4, "/a/b/c/./../../g" to "/a/g" and "mid/content=5/../6"
   to "mid/6".

   Not done: percent-encoding ("%20" for a space) is kept as it is,
   neither decoded nor checked -- a URL is sent as it was written, which
   is what an HTTP client needs; and no normalization (case of the
   percent escapes, default ports removed: section 6).

   Reference: RFC 3986, "Uniform Resource Identifier (URI): Generic
   Syntax", Tim Berners-Lee, Roy Fielding and Larry Masinter (2005),
   sections 3 (the parts), 5 (resolving) and appendix B (the regexp);
   RFC 1738 (1994), the first URL specification. *)

(* userinfo@host:port; the host as written, lowercased if a name
 * ("elm-lang.org", "127.0.0.1", "[::1]"); the port as written, if any *)
type authority = { userinfo : string option; host : string; port : int option }

(* a URL, or a reference relative to one (then without a scheme, and
 * maybe without an authority): the five parts of the diagram above. A
 * part absent (None) is not the same as a part empty (Some ""): "?"
 * has an empty query, and resolves differently from "" (section 5.2.2) *)
type t = {
  scheme : string option; (* lowercased: "http" *)
  authority : authority option;
  path : string; (* "/images/turtle.gif", "../g", or "" *)
  query : string option; (* without its '?' *)
  fragment : string option; (* without its '#' *)
}

(* the five parts of a string, as appendix B's regexp finds them; Error
 * only for an authority's port that isn't a number *)
val parse : string -> (t, string) result

(* back to a string, section 5.3 (to_string (parse s) = s, but for the
 * case of the scheme and the host, and an empty port dropped) *)
val to_string : t -> string

(* [resolve base reference]: the absolute URL the [reference] means,
 * seen from [base] (which must have a scheme), section 5.2.2 (strict:
 * "http:g" is not relative, even with the base's scheme) *)
val resolve : t -> t -> t

(* section 5.2.4: "." and ".." segments removed from a path *)
val remove_dot_segments : string -> string

(* the port to connect to: the one written, else the scheme's (80 for
 * http, 443 for https); None if neither *)
val port : t -> int option

(* what an HTTP request asks for (its request line's target): the path
 * and the query, "/" for an empty path, no fragment (never sent) *)
val request_target : t -> string
