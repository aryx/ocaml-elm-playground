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

   A URL of another scheme is refused, https:// included: HTTP inside
   TLS (RFC 8446), the encryption and the server's certificate checked,
   is a protocol of its own not written yet
   (plan_dependencies_remaining.md, section 2). A redirection to
   https:// -- what most http:// sites answer today -- is refused the
   same way, with the new URL in the message. *)

(* the final response (whatever its status, 404 included: the caller
 * decides), or why there is none: a URL we can't get, a network error,
 * a response that doesn't parse, too many redirections *)
val get : ?max_redirects:int -> ?timeout:float -> string -> (Http.response, string) result
