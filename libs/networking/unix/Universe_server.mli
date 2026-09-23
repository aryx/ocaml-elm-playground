(* Universe_server: HtDP's universe, the server of world programs.

   How to Design Programs' 2htdp/universe (Matthias Felleisen, Robert
   Bruce Findler, Matthew Flatt and Shriram Krishnamurthi; the book free
   at https://htdp.org) goes one step past big-bang (Bigbang.mli):
   several world programs, on several computers, and a server between
   them, the universe, which they send messages to and which sends
   messages back. The library's documentation begins with a ball passed
   around: the universe hands it to one world, which lets it fall, and
   says so at the bottom; the universe gives it to the next
   (examples/UniverseBall.ml).

     Racket (2htdp/universe)               OCaml
     (universe '()                         Universe_server.universe caps []
       [on-new add-world]                    ~on_new:add_world
       [on-msg pass-the-ball])               ~on_msg:pass_the_ball ()

     (make-bundle state mails drops)       (state, mails, drops)
     (make-mail iworld message)            (iworld, message)

   The universe is a state, like a world, and handlers that make the
   next one: [on_new] when a world joins, [on_msg] when one sends a
   message, [on_disconnect] when one leaves. Each returns a *bundle*:
   the new state, the mails to post (to which world, what), and the
   worlds to disconnect -- Racket's make-bundle and make-mail are tuples
   here, as Bigbang dropped pinholes and "solid"/"outline" strings.

   Nothing here needs determinism, checksums or rollback (Lockstep.mli,
   Rollback.mli): messages arrive when they arrive, and each world
   decides what to do with them -- the right first lesson, and the right
   shape for a chat, a whiteboard, a turn-based game; the wrong one for
   Spacewar!, which is exactly the comparison notes_networking.md
   section 9 makes. The two ways (Multiplayer.mli, one simulation
   everywhere, and this, many worlds and one postbox) share the
   transport underneath.

   It is Server.mli with the program's handlers where the relay
   (Relay.mli) has its fixed rule, on HtDP's port, 4567; the worlds
   connect with Universe.mli's [big_bang ~register]. Messages are
   strings: Racket sends S-expressions and checks them at run time,
   OCaml can't, and a string is honest for a chat or a board game (a
   typed message, encoded with Wire.mli, is the next step when a game
   needs it).

   Reference: How to Design Programs, second edition (MIT Press, 2018,
   https://htdp.org); the 2htdp/universe library's documentation, in
   Racket's teachpacks (https://docs.racket-lang.org/teachpack/). *)

(* a world connected to the universe: HtDP's iworld *)
type iworld = int

(* the new state, the mails to post, the worlds to disconnect *)
type 'u bundle = 'u * (iworld * string) list * iworld list

type 'u t

(* a universe listening on [bind]:[port] (127.0.0.1:4567), and the port
 * it got; [step] it, or [run] it forever *)
val create :
  < Cap.network ; .. > ->
  ?bind:string ->
  ?port:int ->
  'u ->
  ?on_new:('u -> iworld -> 'u bundle) ->
  ?on_msg:('u -> iworld -> string -> 'u bundle) ->
  ?on_disconnect:('u -> iworld -> 'u bundle) ->
  unit ->
  'u t * int

(* the events of now, handled; the mails posted *)
val step : 'u t -> unit

(* the universe's state *)
val state : 'u t -> 'u

(* the universe forever, as a program's main *)
val universe :
  < Cap.network ; .. > ->
  ?bind:string ->
  ?port:int ->
  'u ->
  ?on_new:('u -> iworld -> 'u bundle) ->
  ?on_msg:('u -> iworld -> string -> 'u bundle) ->
  ?on_disconnect:('u -> iworld -> 'u bundle) ->
  unit ->
  unit
