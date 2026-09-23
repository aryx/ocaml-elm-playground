(* Seeing what it thinks: the four things an AI does drawn as ordinary
 * shapes (notes_ai.md section 11).
 *
 * Every algorithm in [Ai] is invisible when it works and a mystery
 * when it does not -- a monster walks into a wall and the code that
 * sent it there is a list of tiles nobody can read. Drawn, it is
 * obvious in a second: the way it is taking, the field everyone is
 * following, what the opponent makes of each move, what mode a
 * character is in and what will take it out of that mode.
 *
 * Unlike Audio_debug, this is not the backend's to draw -- the
 * platform knows the samples it plays, but only the game knows what
 * its enemies are thinking. So these are shapes for a game to draw,
 * behind a key of its own ("p" in TinyTowerDefense, say), and they
 * come back centred on the origin (or where [at] puts them) for the
 * game to [move] where it likes. *)

open Playground

(* [way ~at path]: the tiles it means to walk, joined, with a ring on
 * the last. [at] is the game's own tile-to-screen, the one it draws
 * everything else with. *)
val way : ?color:color -> ?dot:number -> at:(int * int -> number * number) -> (int * int) list -> shape list

(* [field ~at flow tiles]: an arrow per tile, the way a crowd would go
 * from it, fading with the distance still to walk -- the whole plan of
 * a hundred monsters in one picture. [tiles] is which tiles to draw
 * (the visible ones, usually). *)
val field : ?color:color -> ?arrow:number -> at:(int * int -> number * number) -> Ai.flow -> (int * int) list -> shape list

(* [thoughts ~naming moves]: a bar per move, longest first, the best
 * one bright -- [Ai.thoughts] or [Ai.so_far] drawn. Values of any
 * scale (a search's -1 to 1, a playout's 0 to 1) are shown against the
 * largest of them. *)
val thoughts :
  ?color:color -> ?best:color -> ?size:number -> ?width:number -> naming:('move -> string) -> ('move * number) list -> shape list

(* [machine ~naming changes mind]: the modes in a ring, the one it is
 * in lit and counting its frames, an arrow for each change of mind
 * with the few words that trigger it ([Ai.on ~why]). A state machine
 * is a drawing that was written down as a list; this puts it back. *)
val machine :
  ?color:color ->
  ?radius:number ->
  naming:('mode -> string) ->
  ('mode, 'context) Ai.change list ->
  'mode Ai.mind ->
  shape list
