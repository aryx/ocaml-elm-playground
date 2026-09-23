(* Udp: packets between two computers, the way games send them.

   TCP (Tcp.mli) is a stream: every byte arrives, in order, and a lost
   packet holds up all the ones after it until it is sent again -- a
   stall of a round trip or more, for a game input that is stale by
   then anyway. UDP (David Reed, RFC 768, 1980) is the thin layer over
   IP that doesn't: *datagrams*, each on its own, maybe lost, maybe
   duplicated, maybe reordered -- exactly what Sim_net.mli simulates,
   and what Lockstep.mli is written to survive (each packet carries
   every input not yet acknowledged). Doom (1993) used IPX, Quake
   (1996) UDP, and every action game since.

     host                                    join
     socket, bind 127.0.0.1:7777
                                             socket (any port)
     recvfrom  <---------------------------  sendto 127.0.0.1:7777
     (now it knows the player's address)
     sendto player  ------------------------>  recvfrom

   No connection, no handshake: the host learns its player's address
   from the first datagram that arrives, and answers there. Every
   socket is non-blocking (Http_request.mli): a frame asks for what
   has arrived and never waits.

   Like Tcp.mli's, these take the capability to reach the network
   (Cap.network), asked for the address before the socket.

   Safe by default: the host listens on 127.0.0.1, this computer only,
   unless told another address (bind=0.0.0.0 for every network the
   computer is on: a LAN party). A datagram from anyone but the player
   is ignored, and what arrives is never trusted (Wire.mli refuses
   garbage).

   Reference: RFC 768 (UDP, 1980); W. Richard Stevens, "UNIX Network
   Programming" volume 1 (third edition, 2003), chapter 8, "Elementary
   UDP Sockets". *)

(* a host's transport, bound to [bind]:[port], and the port it got
 * (port 0 asks the system for a free one: the tests); raises
 * Unix.Unix_error (the port taken, the address not this computer's) *)
val host : < Cap.network ; .. > -> bind:string -> port:int -> Transport.t * int

(* a player's transport, sending to [host]:[port]; raises
 * Unix.Unix_error or Failure (a name that doesn't resolve) *)
val join : < Cap.network ; .. > -> host:string -> port:int -> Transport.t

(* the transport for a role: what Multiplayer.set_connect wants *)
val connect : < Cap.network ; .. > -> Transport.role -> (Transport.t, string) result
