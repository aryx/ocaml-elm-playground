(* Relay: a server that knows nothing of the game, and copies packets.

   Two computers playing over UDP (Udp.mli) need one of them to be
   reachable: the host listens on a port, and the other one must be
   able to reach it. That fails in two common cases: a browser can't
   listen at all (a web page only makes connections, never accepts
   them), and a computer behind a home router (NAT) can't be reached
   from outside unless the router is told to forward a port. What both
   can always do is *connect out*, to a server with a public address.
   So the players all connect to one, and it passes their packets on:

       player 0 ---\                   /---> player 1
                    \                 /
       player 1 ------>   relay   ---------> player 0, player 2
                    /                 \
       player 2 ---/                   \---> player 0, player 1

   The relay doesn't run the game, doesn't read the packets, doesn't
   decide anything: each packet from a player is copied to all the
   others. The game is still peer-to-peer lockstep or rollback
   (Lockstep.mli, Rollback.mli) -- only the route changes, through a
   middleman every player can reach. (A server that runs the game
   itself, and sends the players what they see, is the other
   architecture, client-server, notes_networking.md section 6.) The
   cost: a packet travels to the relay and back out, a longer way than
   directly -- the price of reaching everyone.

   The players connect with WebSocket (Websocket.mli), because it is
   what a browser has; a native program speaks it too (Relay_client.mli),
   so a browser and a native program play together. After the
   handshake, the relay tells each player its number, lowest free first,
   in a binary message of two bytes, 02 then the number; a player
   beyond [players] is told no (a close frame). Every other binary
   message is a packet, copied to the other players.

   The connections are Server.mli's (one event loop, non-blocking
   sockets); this module is only the relay's rule, a seat for who
   comes and each packet copied to the others.

   Safe by default, like Udp: it listens on 127.0.0.1 unless given
   another address; garbage closes the connection that sent it.

   Reference: RFC 6455 (WebSocket); for NAT and why relays exist,
   notes_networking.md section 7 (and TURN, RFC 8656, the relay of
   WebRTC). *)

type t

(* a relay for [players], listening on [bind]:[port] (0: a free port),
 * and the port it got *)
val listen : < Cap.network ; .. > -> bind:string -> port:int -> players:int -> t * int

(* everything that can be done without waiting *)
val step : t -> unit

(* sleep until a socket has something, or [timeout] seconds *)
val wait : t -> float -> unit

(* the players connected, by number *)
val players : t -> int list

(* the packets copied so far *)
val forwarded : t -> int
