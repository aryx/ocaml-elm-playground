(* Transport: where a peer's packets go, whatever carries them.

   Lockstep (Lockstep.mli) makes packets and reads packets; it doesn't
   care how they travel. Sim_net carries them inside one program;
   networking/unix/Udp.mli between two computers; a browser would need
   WebSockets and a relay (plan_networking_teaching.md, phase 5). This
   is what they have in common, for Multiplayer.mli: send to the other
   side, take what arrived, and say how it goes -- a record of
   functions, so that the pure libraries can use a transport a
   platform opens (the sockets are the operating system's).

   Over UDP, two players, one on each side; through a relay
   (Relay.mli), any number, the relay forwarding each packet to all the
   others -- and saying which player you are, which you learn once
   connected. *)

type t = {
  send : string -> unit; (* a packet to the other side (dropped if it isn't known yet) *)
  receive : unit -> string list; (* the packets arrived since last time, never waiting *)
  status : unit -> string; (* for the screen: "hosting on 127.0.0.1:7777, waiting for a player" *)
  player : unit -> int option; (* which player I am, once known *)
}

(* how to find the other side *)
type role =
  | Host of { bind : string; port : int } (* wait for a player on this address and port *)
  | Join of { host : string; port : int } (* play with the host there *)
  | Relay of { host : string; port : int } (* play through the relay there *)

(* opening a transport is the operating system's (sockets natively, the
 * browser's WebSocket in a page): each platform installs how
 * (Connect.connect natively), and Multiplayer and Universe call it;
 * without one installed, an Error saying so *)
val set_connect : (Cap.network -> role -> (t, string) result) -> unit
val connect : < Cap.network ; .. > -> role -> (t, string) result
