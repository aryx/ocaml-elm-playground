(* Http_request: an HTTP GET that never blocks, stepped every frame.

   Http_client.get blocks: connect waits for the server, read waits for
   the bytes, and a program drawing 60 frames a second freezes meanwhile
   (a quarter second to a server across the ocean is 15 frames lost). A
   game can't wait, so the request is turned inside out: instead of a
   function that runs until the answer is in, a *state* that a step
   advances as far as it can *without waiting*, then returns; the frame
   loop calls [step] once a frame, and draws in between.

                 connect: EINPROGRESS           the request written
     start --> Connecting ------------> Sending ------------> Receiving
                 (writable: connected)    (writable: write      | (readable:
                                           what fits)           |  read what's
                                                                |  there)
                          a redirection: again, at the new URL  | read = 0:
               <------------------------------------------------| the server
                                                                v closed
                                                               Done

   Each step asks the kernel, for the one socket, "can I write? can I
   read?" without waiting (select with a timeout of 0: 4.2BSD, 1983),
   and only does what won't block: the socket is *non-blocking*
   (O_NONBLOCK), so even a mistake gives EAGAIN, not a freeze. That is
   the **event loop** -- the shape of every server (one thread, many
   sockets, select or its descendants poll and epoll), of every
   browser, of Node.js -- here with one socket and a frame loop around
   it. The bytes arrive in pieces, whenever they do; they are kept
   until the server closes, then parsed at once by
   Http.parse_response, as the blocking client does, so the two give
   the same responses (the tests check it).

   The one step that still blocks: resolving the host's name
   (getaddrinfo, in [start] and after a redirection), usually a few
   milliseconds from the system's cache, and nothing for an address
   ("127.0.0.1"). Browsers put DNS on threads of its own for that
   reason; asking a DNS server ourselves, over a non-blocking UDP
   socket, is the other way, a module of its own
   (plan_dependencies_remaining.md, section 2).

   Reference: W. Richard Stevens, "UNIX Network Programming", volume 1
   (third edition, 2003), chapters 6 (select) and 16 (nonblocking I/O,
   its section 16.3 on nonblocking connect: EINPROGRESS, then writable,
   then SO_ERROR says whether it worked). *)

type error =
  | Bad_url of string (* can't be parsed, or not http:// *)
  | Timeout
  | Failed of string (* refused, reset, a response that doesn't parse... *)

type t

(* the request started (the name resolved, the connection begun);
 * [timeout] (30 s) counts from now to the end, redirections included *)
val start : ?max_redirects:int -> ?timeout:float -> < Cap.network ; .. > -> string -> t

(* advance as far as possible without waiting; nothing once done *)
val step : t -> unit

(* the final response (a 404 is one), once done *)
val result : t -> (Http.response, error) result option

(* the URL asked last: the last redirection's, once done -- a page's
 * links are relative to it, not to the URL first asked *)
val url : t -> string

(* wait until one of the requests can advance, or [timeout] seconds:
 * for a program with nothing else to do between steps (the tests) *)
val wait : t list -> float -> unit
