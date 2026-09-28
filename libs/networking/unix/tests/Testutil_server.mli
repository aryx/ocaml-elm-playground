(* A tiny HTTP server on localhost, for the tests: forked, so that the
 * test process is the client. *)

(* [with_server handle f]: [f port] with a server listening on
 * 127.0.0.1:[port], in a child process, calling [handle port
 * request_line fd] for each connection ("GET /x HTTP/1.1"), which
 * writes the answer on [fd]; the connection is closed after it. The
 * child is killed when [f] returns. *)
val with_server : (int -> string -> Unix.file_descr -> unit) -> (int -> unit) -> unit

(* canned answers: /old redirects (relatively) to /new?v=2, chunked
 * "Wikipedia"; /loop redirects to itself; /secure to https://127.0.0.1:1/
 * (nobody there); others
 * are 404 "not here\n" *)
val site : int -> string -> string

(* a handler answering [site]'s answer at once *)
val respond : (int -> string -> string) -> int -> string -> Unix.file_descr -> unit

(* "http://127.0.0.1:port/path" *)
val url : int -> string -> string
