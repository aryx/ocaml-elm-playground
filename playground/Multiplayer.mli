(* Multiplayer: a game for several players, on one computer or several.

   Evan's playground has a [computer]: your keyboard, your mouse. A
   game for two needs one more idea, the *player*: everyone's input,
   where [computer] is yours. So a multiplayer game's [update] gets the
   list of players, each with their keyboard, and never reads the
   computer's:

     let update computer players model =
       let pilot n = (List.nth players n).keyboard in
       { model with wedge = fly (pilot 0) model.wedge; needle = fly (pilot 1) model.needle }

     let app = Multiplayer.game ~players:2 view update initial_model

   The same game then runs in several ways, chosen by a flag
   (Playground.flags), without a line of it changing:

   - [net=local] (the default): both players on one keyboard, as
     Spacewar! in 1962 -- player 0 the arrows (and space, enter, shift),
     player 1 w, a, s, d;
   - [net=simulate]: two computers in one window, side by side, each
     peer its own model, their inputs going through a fake network
     (Sim_net.mli) with [latency=] and [loss=] (milliseconds, percent;
     also [jitter=], [delay=], [seed=]), and keys to change them while
     it runs: [ and ] the latency, - and = the loss. The game
     keeps agreeing with itself under any network; the network only
     changes how late the keys answer, and whether the game stalls.
     Each half shows what that computer shows, and the checksums say
     whether the two games still agree (Checksum.mli);
   - [net=host] and [net=join]: two real computers, over UDP (natively
     only: Udp.mli), for a program granted the network: it passes
     [~network:caps], from its Cap.main (only the network of them is used)
     (plan_caps.md); without it, these two modes say so on the screen,
     and the program's type says it can't reach the network. The host is player 0 and waits, on port 7777
     ([port=]), of this computer only unless given [bind=0.0.0.0] (a
     LAN); the other one joins, [net=join host=192.168.1.12], and is
     player 1. Each plays with its arrows. No handshake: the host plays
     its first [delay] ticks and stalls until the first inputs arrive.

   Underneath is Lockstep.mli: each tick, every player's input (a
   byte: the arrows, space, enter and shift, a bit each -- the letters
   don't travel), applied [delay] ticks after it is read, on every
   peer. It works only if [update] is *deterministic*: the same inputs
   give the same model, on every computer. So the [computer] it gets is
   cleaned of what differs between computers: no keyboard, no mouse
   (they are in the players), a fixed screen (1000 x 1000, whatever the
   window's size), the time counted in ticks (1/60 s each, never the
   clock), and the flags. Randomness must come from a seed in the model
   (Playground.random), never from Random.

   A player's [pressed] is the keys that went down this tick (their
   rising edge, what Scene2d.pressed gives a one-player game), computed
   from the inputs themselves, so every peer agrees on it.

   Open, and said so: what a peer shows while it waits for a late input
   (here, the last model, frozen: a stall); how a game's seed is shared
   (here, the same flags for both halves; over a real network, the host
   picks it and sends it first). *)

type player = {
  id : int; (* 0 to players - 1 *)
  keyboard : Playground.keyboard; (* the keys held this tick *)
  pressed : Playground.keyboard; (* the keys that went down this tick *)
}

type 'model state

(* how net=host and net=join reach the other computer: installed by a
 * platform that has sockets (the native ones, Udp.connect); without
 * it, the modes say so on the screen *)
val set_connect : (Cap.network -> Transport.role -> (Transport.t, string) result) -> unit

(* [game ~players view update model]: [view computer n model] draws
 * what player n sees (the real computer: its screen, its time);
 * [update computer players model] is one tick, everyone's input *)
val game :
  ?network:< Cap.network ; .. > ->
  players:int ->
  (Playground.computer -> int -> 'model -> Playground.shape list) ->
  (Playground.computer -> player list -> 'model -> 'model) ->
  'model ->
  ('model state Playground.game, Playground.msg) Playground.app
