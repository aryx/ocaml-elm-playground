(* Game AI for the playground: what an enemy wants, in one line.

   Five families, in the order a game meets them: steering (a
   character that moves by wanting to go somewhere), ways through a
   map, an opponent for a turn game, what a character is doing (a mode
   and what changes it), and a bot -- a mind that plays through the
   same inputs as the player. Underneath each is a module of [ai/],
   where the algorithm is written out with its diagram and its
   references; up here there are only verbs and values, and no search,
   queue, tree or seed is ever named.

   The first family is steering: characters that move by wanting to go
   somewhere, as verbs on a [Physics.body] next to [fall] and [push].
   Each adds the steering force of one behaviour to what pushes the
   body, so they combine with each other and with the rest of the
   physics, and [Physics.step] comes last:

     fish |> flocking school |> avoiding rocks |> Physics.step

   A fish is one line of [update]:

     let update _ school = List.map (fun f -> f |> flocking school |> Physics.step) school

   Every verb takes the body's top speed, [speed] (200 pixels per
   second by default), and how hard it may turn, [force] (400 pixels per
   second, per second): a heavy barge and a darting fly are the same
   behaviours with different numbers. Underneath is ai/Steering.mli
   (Craig Reynolds, 1999) and ai/Flock.mli (Reynolds, 1987), where the
   forces are written out. *)

open Playground

(* [seek x y b]: [b] steered straight at (x, y), at top speed (and
 * overshooting it, like a homing missile: see [arrive]) *)
val seek : ?speed:number -> ?force:number -> number -> number -> Physics.body -> Physics.body

(* [flee x y b]: straight away from (x, y) *)
val flee : ?speed:number -> ?force:number -> number -> number -> Physics.body -> Physics.body

(* [arrive x y b]: to (x, y), slowing down within [slowing] (100
 * pixels) of it, and stopping there *)
val arrive : ?speed:number -> ?force:number -> ?slowing:number -> number -> number -> Physics.body -> Physics.body

(* [chase target b]: towards where [target] will be when [b] gets there *)
val chase : ?speed:number -> ?force:number -> Physics.body -> Physics.body -> Physics.body

(* [escaping target b]: away from where [target] will be *)
val escaping : ?speed:number -> ?force:number -> Physics.body -> Physics.body -> Physics.body

(* [wandering time b]: an idle stroll, curving one way then the other.
 * [time] drives it (computer.time, say, plus something different for
 * each body so that they don't all turn together): a point on a circle
 * ahead of [b] drifts with it, smoothly, and [b] follows the point *)
val wandering : ?speed:number -> ?force:number -> number -> Physics.body -> Physics.body

(* [avoiding rocks b]: turned away from whichever of the circles (x, y,
 * radius) is in front of it, and not otherwise *)
val avoiding : ?speed:number -> ?force:number -> (number * number * number) list -> Physics.body -> Physics.body

(* [flocking others b]: Reynolds's three rules among [others] within
 * [radius] (100 pixels) -- [others] may be the whole flock, [b]
 * included: away from the ones too close, the way the neighbours go,
 * towards where they are, weighted [separation] (1.5), [alignment] (1),
 * [cohesion] (1); 0 turns one off *)
val flocking :
  ?speed:number ->
  ?force:number ->
  ?radius:number ->
  ?separation:number ->
  ?alignment:number ->
  ?cohesion:number ->
  Physics.body list ->
  Physics.body ->
  Physics.body

(* [following path b]: along the polyline [path], kept within [width]
 * (20 pixels) of it, like a car keeping to its road *)
val following : ?speed:number -> ?force:number -> ?width:number -> (number * number) list -> Physics.body -> Physics.body

(* [facing b]: [b] pointing the way it goes (its angle, for [draw]):
 * a fish nose first *)
val facing : Physics.body -> Physics.body

(*****************************************************************************)
(* {1 Ways through a map} *)
(*****************************************************************************)
(* A way from one tile to another, over whatever grid the game already
   has -- a Tilemap, an array of arrays, a function. The game says
   which tiles can be walked and nothing else: no graph to build, no
   frontier, no queue (ai/Pathfind.mli has all three written out).

     let step = Ai.way ~walkable:(free level) monster.cell door

   Tiles are (column, row) in the game's own numbering, and nothing
   here knows where a tile is on screen. *)

(* [way ~walkable from to_]: the shortest way, [from] excluded and
 * [to_] included, or [] if there is none (or [to_] cannot be walked).
 * Four neighbours; [diagonal] adds the corners, at their honest price
 * of sqrt 2 steps. *)
val way : walkable:(int * int -> bool) -> ?diagonal:bool -> int * int -> int * int -> (int * int) list

(* [way_over ~cost from to_]: the same, where some ground is slower
 * than other ground -- [cost] is what entering a tile is worth, at
 * least 1 (a road 1, mud 3), and infinity for a tile that cannot be
 * entered at all. The cheapest way is not the shortest one: it goes
 * around the swamp. *)
val way_over : cost:(int * int -> number) -> ?diagonal:bool -> int * int -> int * int -> (int * int) list

(* Where fifty monsters all want the same door, fifty searches are
   forty-nine too many: one search from the door outward gives every
   tile the way to go, and each monster only reads its own tile. Build
   it when the map changes, not when a monster moves. *)
type flow

val flow : walkable:(int * int -> bool) -> ?diagonal:bool -> int * int -> flow

(* [next_step f tile]: the neighbour to walk to, None on the goal
 * itself and on a tile the goal cannot be reached from *)
val next_step : flow -> int * int -> (int * int) option

(* [steps_to_go f tile]: what it costs from there, for drawing the
 * field as colours (and None off it) *)
val steps_to_go : flow -> int * int -> number option

(*****************************************************************************)
(* {1 An opponent} *)
(*****************************************************************************)
(* A computer to play a turn game against, from its rules alone. The
   game says what the moves are, what a move does, whose turn it is
   and what a position is worth to the machine; the search
   (ai/Minimax.mli, ai/Deepening.mli, ai/Mcts.mli) stays behind the
   door.

     let reply = Ai.best_move (Ai.thinking_ahead 4 othello) board *)

type ('state, 'move) rules = {
  moves : 'state -> 'move list; (* [] when the game is over *)
  play : 'state -> 'move -> 'state;
  score : 'state -> number; (* the machine's view: above 0 it is winning *)
  my_turn : 'state -> bool; (* the machine to play, not the human *)
}

type ('state, 'move) opponent

(* [thinking_ahead depth rules]: it looks [depth] moves ahead and
 * believes [score] at the end of them -- alpha-beta, with iterative
 * deepening so that it can be stopped (see [pondering]) *)
val thinking_ahead : int -> ('state, 'move) rules -> ('state, 'move) opponent

(* [playing_out n rules]: it plays the position out at random [n]
 * times and counts the wins, which needs no [score] worth writing --
 * only its sign at the end of a game. For a game nobody can write an
 * evaluation function for (Go), or one whose moves are too many to
 * search. *)
val playing_out : int -> ('state, 'move) rules -> ('state, 'move) opponent

(* [hinting order o]: the moves [order] puts first are tried first,
 * which is most of what makes a search deep (the middle columns in
 * Connect 4). Nothing for [playing_out]. *)
val hinting : ('state -> 'move list -> 'move list) -> ('state, 'move) opponent -> ('state, 'move) opponent

(* [best_move o state]: its answer, thought out in one go -- fine for
 * a small game (a board of nine, a search four moves deep), and a
 * frozen frame for a big one, which is what [pondering] is for *)
val best_move : ('state, 'move) opponent -> 'state -> 'move option

(* [thoughts o state]: what it makes of each of its moves, to draw --
 * the position's value after it for [thinking_ahead], the share of
 * random games it won for [playing_out] (0 to 1). It costs a search
 * per move rather than one search: a cut branch never learns its own
 * value, only that it was not worth the trouble, so a search that
 * says what it thinks of everything is a slower search. *)
val thoughts : ('state, 'move) opponent -> 'state -> ('move * number) list

(* {2 Thinking while the game draws}

   A game at 60 frames a second cannot stop for a search, so the
   thinking is spread over frames: start one when it is the machine's
   turn, give it a frame's worth in every [update], and play [answer]
   when it has [settled]. The board answers the mouse throughout, and
   a game can draw what it is thinking ([so_far]). *)

type ('state, 'move) pondering

val pondering : ('state, 'move) opponent -> 'state -> ('state, 'move) pondering

(* a frame's worth of thought *)
val ponder : ('state, 'move) pondering -> ('state, 'move) pondering

(* it has looked as far as it was asked to *)
val settled : ('state, 'move) pondering -> bool

(* the best move so far: a complete answer from the first frame on,
 * only a shallower one *)
val answer : ('state, 'move) pondering -> 'move option

(* what it makes of the moves so far, as [thoughts] *)
val so_far : ('state, 'move) pondering -> ('move * number) list

(* [a_frame_of n o]: how much thought fits in one frame -- positions
 * for [thinking_ahead] (20,000 by default), random games for
 * [playing_out] (12). Only the game knows what its own moves cost:
 * 12 playouts is a frame of 9x9 Go and would be a frame of
 * tic-tac-toe fifty times over. *)
val a_frame_of : int -> ('state, 'move) opponent -> ('state, 'move) opponent

(*****************************************************************************)
(* {1 What a character is doing} *)
(*****************************************************************************)
(* A state machine, in the shape a game wants it: a list of changes of
   mind, a mode now, and how long it has been in it (ai/Fsm.mli).

     let changes = [ Ai.on Chase (fun g -> lost g) Search;
                     Ai.after 120 Search Home ]
     let ghost = { g with mind = Ai.deciding changes g g.mind } *)

type ('mode, 'context) change
type 'mode mind

(* [on from when_ to_]: in [from], when the world looks like that, go
 * to [to_]. [why] is a few words for drawing the machine. *)
val on : ?why:string -> 'mode -> ('context -> bool) -> 'mode -> ('mode, 'context) change

(* [after frames from to_]: [from] runs out after so many frames *)
val after : ?why:string -> int -> 'mode -> 'mode -> ('mode, 'context) change

val mind : 'mode -> 'mode mind
val doing : 'mode mind -> 'mode

(* frames spent in the mode it is in (0 the frame it changed) *)
val doing_for : 'mode mind -> int

(* [deciding changes context m]: one frame of it. The first change
 * whose test holds wins, so order is the priority. *)
val deciding : ('mode, 'context) change list -> 'context -> 'mode mind -> 'mode mind

(* it changed its mind this frame, and what made it *)
val changed : 'mode mind -> string option

(* the modes a list of changes mentions, and each change as (from,
 * why, to): what Ai_debug draws a machine from *)
val modes : ('mode, 'context) change list -> 'mode list
val links : ('mode, 'context) change list -> ('mode * string * 'mode) list

(*****************************************************************************)
(* {1 A bot} *)
(*****************************************************************************)
(* A mind that plays the game the way you do: it sees what a player
   could see, and it fills the game's own input record -- which is why
   [senses] and [intent] are the game's types and not ours
   (ai/Bot.mli, ai/Sense.mli).

     let intent = if p.human then keys computer else Ai.thinks brain world p.playing *)

type ('world, 'senses, 'intent) bot
type ('senses, 'intent) playing

(* [bot ~senses decide]: what it is allowed to know, and what it does
 * with it. [senses] is given what it knew last frame, because part of
 * sensing is remembering (a target's last known place). *)
val bot : senses:('senses option -> 'world -> 'senses) -> ('senses -> 'intent) -> ('world, 'senses, 'intent) bot

(* [reacting_in frames b]: it acts on what it saw that many frames ago
 * (0 by default: a machine; 12 is a person's fifth of a second) *)
val reacting_in : int -> ('world, 'senses, 'intent) bot -> ('world, 'senses, 'intent) bot

(* [deciding_every frames b]: it changes its mind that often, and
 * repeats itself in between -- what keeps a bot from twitching *)
val deciding_every : int -> ('world, 'senses, 'intent) bot -> ('world, 'senses, 'intent) bot

(* [skill k b]: both of the above from one number, 0 (a beginner: it
 * reacts in 12 frames and decides every 6) to 1 (a machine: 0 and 1).
 * Difficulty without cheating -- the bot never learns more, it only
 * answers slower. *)
val skill : number -> ('world, 'senses, 'intent) bot -> ('world, 'senses, 'intent) bot

val playing : 'intent -> ('senses, 'intent) playing

(* [thinks b world p]: the intent for this frame, and what to keep *)
val thinks : ('world, 'senses, 'intent) bot -> 'world -> ('senses, 'intent) playing -> 'intent * ('senses, 'intent) playing

(* what it sensed on the last frame -- to draw what a bot knows, which
 * is the only honest way to see whether it is cheating *)
val noticed : ('senses, 'intent) playing -> 'senses option

(* [aim_error ~spread ~seen_for ~seed ()]: how far off its aim is, in
 * the game's own unit: [spread] when a target appears, halving every
 * [settle] frames (30) it stays in sight. A smooth wobble of
 * [seen_for] and [seed], not a random number, so a game replays the
 * same. *)
val aim_error : spread:number -> ?settle:number -> seen_for:int -> seed:int -> unit -> number
