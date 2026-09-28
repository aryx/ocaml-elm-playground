(* Http_server: the other end of Http_request -- a web server's event
   loop, many connections, no threads.

   A web server is the simplest server there is: a connection comes,
   a request is read, an answer written, the connection closed (HTTP/1.0's
   way, and ours: "Connection: close"; keep-alive, several requests on
   one connection, is what HTTP/1.1 added for speed). What the answer
   is, is the caller's [handler]'s business: tiny_httpd's gives files
   from a directory, a test's a canned page.

       select --> accept new ones --> read what arrived --> a whole request?
         ^                                                   (Http.parse_request)
         |                                                      | yes:
         |    close the ones answered <-- write what the     handler ->
         '-------- wait ----------------  sockets take   <-- the response's bytes

   The same shape as Server.mli's (WebSocket) and Http_request.mli's
   (the client): every socket non-blocking, [step] doing what can be
   done without waiting, [wait] sleeping in select. A slow client --
   one sending its request a byte a second, or reading the answer as
   slowly -- holds up no other, which is the one thing the first web
   servers (CERN httpd, 1990, and NCSA's, 1993) did by forking a
   process per connection instead.

   Safe by default: it listens on 127.0.0.1 unless given another
   address; a request that is garbage, or longer than its limits
   (Http.parse_request), is answered 400 and the connection closed.

   Reference: RFC 9112, sections 2-3 (the request's message);
   W. Richard Stevens, "UNIX Network Programming", volume 1, chapter 6
   (select), the loop above. *)

type t

(* a server listening on [bind]:[port] (0: a free port), and the port
 * it got *)
val listen : < Cap.network ; .. > -> bind:string -> port:int -> t * int

(* what the handler is given: the client's address, the request, its
 * body *)
type handler = peer:string -> Http.request -> string -> Http.response

(* everything that can be done without waiting: connections accepted,
 * what they sent read, each whole request answered by [handler], what
 * the sockets take written, the connections answered closed *)
val step : t -> handler -> unit

(* sleep until a socket has something, or [timeout] seconds *)
val wait : t -> float -> unit

(* the connections open *)
val connections : t -> int
