(* Web_http: the web platform's requests, each an XMLHttpRequest whose
 * answer comes back later, to a continuation; nothing blocks, a page
 * having one thread.
 *
 * The browser has its own rules about where a page may ask: its own
 * site, or another that allows it (CORS). A request refused that way
 * looks like a network that does not answer.
 *)

(* a file's bytes, for Audio.loop_from (Audio.set_fetcher): a plain name
 * from the page's own server, or a URL; None if it could not be had *)
val fetch_web : string -> (string option -> unit) -> unit

(* Cmd.Http_get and Cmd.Http_post performed: a GET, or with [post] (the
 * content type and the body) a POST; the response with its status and
 * headers, or the error, as natively (Commands) *)
val fetch_response : ?post:string * string -> string -> ((Cmd.http_response, Cmd.http_error) result -> unit) -> unit
