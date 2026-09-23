(* Server: many WebSocket connections, one event loop, no threads.

   What Relay.mli and Universe_server.mli share: a server that clients
   connect to with WebSocket (Websocket.mli) -- from a browser or a
   native program (Relay_client.mli) -- each connection given an id as it
   comes, its binary messages handed to the caller as events, and the
   caller's answers sent back. What to do with a message is the caller's
   business: the relay copies it to the others, a universe runs the
   program's handlers.

   One program, many connections, no threads: an *event loop*
   (Http_request.mli), every socket non-blocking, [wait] sleeping in
   select until one of them has something, [step] doing whatever each
   one can do without waiting -- accept, read, answer the handshake,
   decode the frames, write. What can't be written yet (a slow client)
   waits in its connection's outbox, and a slow client never holds the
   others up.

       select --> accept new ones --> read what arrived --> handshake?
         ^                                                     |
         |      write what the sockets take <-- frames: events -'
         '------------------ wait -----------------------------'

   Safe by default: it listens on 127.0.0.1 unless given another
   address; garbage closes the connection that sent it; a message is
   at most a megabyte (Websocket.decode). *)

type t

(* a server listening on [bind]:[port] (0: a free port), and the port
 * it got *)
val listen : < Cap.network ; .. > -> bind:string -> port:int -> t * int

type event =
  | Joined of int (* a client connected (its handshake done), with its id *)
  | Message of int * string (* a binary message from a client *)
  | Left of int (* a client went away *)

(* everything that can be done without waiting, and what happened *)
val step : t -> event list

(* a binary message to a client (queued, written as it can be) *)
val send : t -> int -> string -> unit

(* write what the sockets take now: after answering the events, so that
 * the answers leave in the same step *)
val flush : t -> unit

(* close a client, once what it was sent is written *)
val close : t -> int -> unit

(* sleep until a socket has something, or [timeout] seconds *)
val wait : t -> float -> unit

(* the clients connected, by id *)
val clients : t -> int list
