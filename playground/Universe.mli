(* Universe: world programs that talk, HtDP's big-bang with a mailbox.

   A world of Bigbang.mli, one more thing: it can send messages to a
   universe (Universe_server.mli, a program running somewhere), and
   receive the universe's. In How to Design Programs' 2htdp/universe
   (https://htdp.org), any handler may return a *package* instead of a
   world -- the world, and a message to send -- and [on-receive] is
   called when one arrives:

     Racket (2htdp/universe)                OCaml
     (big-bang 'resting                     Universe.big_bang Resting
       [to-draw draw]                         ~to_draw:draw
       [on-tick fall]                         ~on_tick:fall
       [on-receive catch]                     ~on_receive:catch
       [register LOCALHOST])                  ~register:"localhost" ~network:caps ()

     (make-package world message)           (world, [ message ])
     world, sending nothing                 (world, [])

   Here every handler returns a package, a tuple, the messages a list
   (none most of the time): OCaml has no "a world or a package", and a
   list says "several" as well as "one". Messages are strings (see
   Universe_server.mli for why). [register] is the universe's host;
   the flags host= and port= override it, to point the same program at
   another computer. Connecting takes the program's capability to reach
   the network (Cap.network, plan_caps.md).

   The messages sent before the universe answered wait, and leave once
   it has; the screen says how the connection goes while it isn't made.
   Natively, the worlds reach the universe with WebSocket
   (Relay_client.mli); in a browser, with its own.

   Worth having beside Multiplayer.mli? They teach two things: there,
   one game simulated on every computer, deterministic, the netcodes
   the subject; here, many little programs sending each other mail, no
   determinism needed -- how HtDP gets beginners to networking, and the
   right shape for a chat, a whiteboard, a turn-based game. *)

open Playground

(* a world, and the messages to send *)
type 'w package = 'w * string list

(* big_bang's model: the world, the connection, what it keeps to make
 * events *)
type 'w world

(* as Bigbang.big_bang, each handler returning a package; [on_receive]
 * a message from the universe; [register] its host ("localhost") on
 * [port] (4567, HtDP's) *)
val big_bang :
  'w ->
  to_draw:('w -> Bigbang.image) ->
  ?on_tick:('w -> 'w package) ->
  ?tick_rate:number ->
  ?on_key:('w -> string -> 'w package) ->
  ?on_mouse:('w -> number -> number -> string -> 'w package) ->
  ?on_receive:('w -> string -> 'w package) ->
  ?stop_when:('w -> bool) ->
  ?register:string ->
  ?port:int ->
  network:< Cap.network ; .. > ->
  unit ->
  ('w world game, msg) app
