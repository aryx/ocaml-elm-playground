(* Http_client: getting a URL, the three layers put together.

     "http://elm-lang.org/images/turtle.gif"
        | Url.parse
        v
     scheme http, host elm-lang.org, port 80, target /images/turtle.gif
        | Http.get, Http.request_to_string
        v
     "GET /images/turtle.gif HTTP/1.1\r\nHost: elm-lang.org\r\n..."
        | Tcp.exchange (DNS, connect, send, read until closed)
        v
     "HTTP/1.1 301 Moved Permanently\r\nLocation: https://...\r\n..."
        | Http.parse_response
        v
     status 301: again, with Url.resolve of the Location, at most
     [max_redirects] times (5 by default; Firefox and Chrome stop at 20)

   https:// is the same request inside TLS, our own TLS 1.3
   (Tls_client.mli, Tls13.mli): the server's certificate checked with
   the system's roots, then the same bytes, encrypted. A URL of
   another scheme is refused.
   This one blocks: the program waits, doing nothing else, until the
   answer is in -- the simple version, fine for a file loaded once
   (Download.mli). Http_request.mli is the same request that doesn't
   block, for a program that must go on drawing its frames. *)

(* the final response (whatever its status, 404 included: the caller
 * decides), or why there is none: a URL we can't get, a network error,
 * a response that doesn't parse, too many redirections *)
val get : ?max_redirects:int -> ?timeout:float -> < Cap.network ; .. > -> string -> (Http.response, string) result

(* the same, and the URL the redirections led to; with [post] (its
 * content type and body), the first request a POST *)
val fetch : ?post:string * string -> ?max_redirects:int -> ?timeout:float -> < Cap.network ; .. > -> string -> (string * Http.response, string) result

(* what to connect to and what to send for [url]: the host for the
 * resolver, the port, the request's bytes (a GET; a POST of [post], its
 * content type and body); Error for a URL that isn't http:// or
 * https:// (the message says why) *)
val prepare : ?post:string * string -> Url.t -> (string * int * string, string) result
