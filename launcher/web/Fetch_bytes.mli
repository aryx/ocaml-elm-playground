(* Fetch_bytes: a file of the page's site fetched as bytes, by an
   XMLHttpRequest, for the web pages that read a big one: tinybox's
   menu (its sources, for the code map) and a directory's code map
   (Codemap_web, its bundle).

   The bytes are made an OCaml string by the browser a slice at a time
   (String.fromCharCode over 32 KB), then taken as they are: for 10 MB,
   a few dozen ms, where a byte at a time froze the page 1.4 s. *)

(* [get url ~ok ~failed]: [ok] with the bytes, or [failed] with why not *)
val get : string -> ok:(string -> unit) -> failed:(string -> unit) -> unit
