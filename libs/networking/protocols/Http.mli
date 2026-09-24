(* Http: the web's protocol, a request and its answer, as text.

   HTTP (Tim Berners-Lee, 1991; HTTP/1.1 since 1997, today RFC 9110 for
   the meaning and RFC 9112 for the bytes) is the simplest protocol in
   daily use, and the model of the ones after it: a client connects (a
   TCP connection, Tcp.mli), sends a *request*, lines of text ending in
   an empty line, and reads the *response*: a status line, header lines,
   an empty line, and the body -- the bytes of the file.

     client -> server                     server -> client
     GET /images/turtle.gif HTTP/1.1      HTTP/1.1 200 OK
     Host: elm-lang.org                   Content-Type: image/gif
     User-Agent: elm_playground           Content-Length: 1523
     Connection: close                    Connection: close
     (empty line)                         (empty line)
                                          GIF89a... (1523 bytes)

   Every line ends with CR LF ("\r\n", the network's line end since
   Telnet; a lone LF is accepted when reading, as RFC 9112 section 2.2
   allows). A header is a name, a ':', a value; names are
   case-insensitive ("content-length" is "Content-Length"). "Host:" is
   HTTP/1.1's one mandatory header -- one address may serve many sites
   -- and "Connection: close" asks the server to close the connection
   after answering, so we can read until the end and parse everything
   at once: [parse_response] is a pure function of the bytes received.

   The one subtle part is the question every protocol on a byte stream
   must answer: **where does the message end?** Headers end at the
   empty line; for the body, HTTP/1.1's answers are tried in this order
   (RFC 9112 section 6.3; [body] is that list):

   1. no body at all, whatever the headers say, for the statuses that
      can't have one (1xx, 204 No Content, 304 Not Modified);
   2. "Transfer-Encoding: chunked": the body comes in pieces, each
      preceded by its size in hexadecimal, and a piece of size 0 ends
      it -- for a server that doesn't know the size when it starts (a
      page generated as it is sent);
   3. "Content-Length: n": exactly n bytes;
   4. neither: the body is everything until the server closes the
      connection (HTTP/1.0's only way; a truncated file then can't be
      told from a whole one, which is why 1.1 added the other two).

   Chunked, worked example (checked by the tests, the classic one from
   Wikipedia's article):

       4\r\n              a chunk of 4 bytes
       Wiki\r\n
       5\r\n              of 5
       pedia\r\n
       E\r\n              of 14 (hex E): the CR LFs inside are data
        in\r\n\r\nchunks.\r\n
       0\r\n              the last chunk, empty
       \r\n               (no trailer headers)

   is the body "Wikipedia in\r\n\r\nchunks.". A chunk size may be
   followed by ";name=value" extensions, ignored.

   The status says what happened: 2xx it worked, 3xx look elsewhere
   (the new place in "Location:", maybe relative to the old one:
   Url.resolve), 4xx the client asked wrong (404 Not Found), 5xx the
   server failed. Following redirections is the client's job
   (Http_client.mli), not this module's.

   Not done: keep-alive (several requests on one connection, the reason
   for 1.1's body framings: each message must end without the
   connection ending); compression (we don't send "Accept-Encoding", so
   a server must not compress, and a "Content-Encoding" other than
   identity is refused; gzip would be compression's Inflate); caching;
   HTTP/2 (2015: the same messages as binary frames, many requests at
   once on one connection) and HTTP/3 (2022: the same over QUIC, over
   UDP) --
   what the web moved to, for speed, keeping this module's meaning.

   Reference: RFC 9110 "HTTP Semantics" and RFC 9112 "HTTP/1.1", Roy
   Fielding, Mark Nottingham and Julian Reschke (2022), which replaced
   RFC 2616 (1999) and RFC 2068 (1997); Tim Berners-Lee, "The Original
   HTTP as defined in 1991" (HTTP/0.9: "GET /path", and the file, no
   headers at all). *)

(* a header's name and value, the value without its surrounding spaces *)
type header = string * string

(* the value of the first header with this name (case-insensitive) *)
val header : string -> header list -> string option

(*****************************************************************************)
(* The request *)
(*****************************************************************************)

type request = {
  meth : string; (* "GET" *)
  target : string; (* "/images/turtle.gif": Url.request_target *)
  headers : header list;
}

(* a GET of [target] from [host] ("elm-lang.org", or "localhost:8001"
 * for a port that isn't the default), with the headers above: Host,
 * User-Agent, Connection: close *)
val get : host:string -> string -> request

(* the bytes to send: the request line, the headers, the empty line *)
val request_to_string : request -> string

(*****************************************************************************)
(* The response *)
(*****************************************************************************)

type response = {
  version : string; (* "HTTP/1.1" *)
  status : int; (* 200 *)
  reason : string; (* "OK", for people only *)
  headers : header list;
  body : string;
}

(* "HTTP/1.1 200 OK" -> ("HTTP/1.1", 200, "OK") *)
val parse_status_line : string -> (string * int * string, string) result

(* the body of a chunked transfer: the chunks' data joined, from the
 * start of [s] (Error if [s] ends before the last chunk) *)
val dechunk : string -> (string, string) result

(* the body, from what follows the empty line, by the rules 1 to 4
 * above, for this status and these headers *)
val body : status:int -> header list -> string -> (string, string) result

(* everything the server sent until it closed the connection, parsed *)
val parse_response : string -> (response, string) result

(* 301, 302, 303, 307, 308: the answer is elsewhere, in "Location:" *)
val is_redirect : int -> bool
