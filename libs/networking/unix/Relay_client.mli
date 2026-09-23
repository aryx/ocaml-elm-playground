(* Relay_client: a native program's way to a relay (Relay.mli).

   What a browser's WebSocket does for a web page, done by hand over
   Tcp.mli: connect, send the handshake's request, check the answer's
   accept (Websocket.accept of the key sent), then binary frames both
   ways, each masked, as a client's must be. The relay's first message,
   02 then a number, says which player this is; every other message is
   a packet from another player.

   The socket is non-blocking once connected (the connect itself waits,
   as Tcp.connect does): [send] queues a frame and writes what the
   socket takes, [receive] reads what has arrived, and neither waits --
   a frame loop calls them every frame. What is sent before the
   handshake's answer waits behind the request, in order.

   The mask is taken from a Lehmer sequence seeded by the clock: it
   should be unpredictable to a script in a page (Websocket.mli), and
   there is no script here, only this program. *)

(* a transport through the relay at [host]:[port]; raises
 * Unix.Unix_error or Failure if it can't connect *)
val connect : < Cap.network ; .. > -> host:string -> port:int -> Transport.t
