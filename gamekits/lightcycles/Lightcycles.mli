(* The light cycles of Tron, as a model and an update, with no picture:
   the arena, the cycles, their trails, the crashes, the rounds, the
   computer players, the title and winner scenes.

   The pictures are the games': TinyTron.ml draws it in 2D, from
   above, a pixel per cell; TinyTron3d.ml in 3D, the trails as
   walls, seen from behind a cycle, from inside it, from above, or from
   far away. The same model, the same update, two views: that's the Elm
   architecture's promise (the view is a function of the model, and
   nothing else), and the reason this kit exists.

   The arena is a Tilemap of characters: ' ' free, '#' the walls, '1'
   to '4' the trails. A move is Tilemap.set, a crash a Tilemap.get that
   isn't ' '. A cycle also keeps the corners of its trail, where it
   turned: a trail is then a few straight lines, which a 3D view draws
   as a few walls instead of a box per cell.

   Two to four riders, the last one riding winning the round; the
   arenas' obstacles written as text ([layouts]); a boost; and two
   computers ([brain]): the flood fill ([computer_turn]: the way with
   the most room) and a search ([search_turn]: alpha-beta a few moves
   ahead, scoring a position by the cells each rider reaches first).
   See TinyTron.ml's header for the history.

   A kit (gamekits/, see docs/claude_notes/plan_games.md): a layer on top of
   the playground for the games of a family.
*)

open Playground

(*****************************************************************************)
(* {1 The arena} *)
(*****************************************************************************)

(* 90 cells each side, walls included *)
val size : int

(* the size of a cell in the Tilemap, in pixels: the 2D view's *)
val cell : number

(* An arena's obstacles, written at the grid's own scale: ten rows of
 * ten characters, one per square of 9 x 9 cells (the squares a view's
 * grid lines draw), '#' a block, anything else free. The border's
 * walls are always there. *)
type layout = string * string list

(* the open grid, the film's; pillars; rings round the middle *)
val layouts : layout list

(* the arena of a layout, its walls and blocks as '#' *)
val arena_of : layout -> Tilemap.t

type dir = Up | Down | Left | Right

(* [delta Up] = (0, -1): rows going down, as a Tilemap's *)
val delta : dir -> int * int

val opposite : dir -> dir

type cycle = {
  col : int;
  row : int;
  dir : dir;
  wanted : dir; (* the last direction asked for: a turn happens at the next step *)
  mark : char; (* its trail's character, '1' to '4' *)
  corners : (int * int) list; (* where it turned, the last first, and where it started *)
  alive : bool; (* false once crashed: its trail stays *)
  energy : int; (* the boost's, see [boost_max] *)
  boosting : bool; (* this frame *)
}

(*****************************************************************************)
(* {1 The model} *)
(*****************************************************************************)

type round = {
  arena : Tilemap.t;
  cycles : cycle list; (* rider 0 first *)
  over : int list option; (* when over, the round's point for each rider: 1 to the last one riding *)
  pause : int; (* then, the frames to wait before the next round *)
  frames : int;
}

(* the computer: the flood fill, counting up to [n] cells of room, or a
 * search [n] moves ahead (each rider's move counts one) *)
type brain = Room of int | Search of int

(* how a game is played: how many riders, how many of them at the keys
 * (rider 0 on the arrows, rider 1 on w/a/s/d), the computer's brain,
 * the arenas taken in turn *)
type settings = { riders : int; humans : int; brain : brain; arenas : layout list }

type game = { round : round; scores : int list; settings : settings; round_no : int }

(* the title (1: against the computer, 2: two players), the game, the
 * winner (space: back to the title) *)
type scene = Title | Playing of game | Winner of game

type model = scene Scene2d.t

val initial_model : model

(* a cycle moves one cell every [step] frames, every frame while it
 * boosts; [rounds_to_win] wins *)
val step : int

val rounds_to_win : int

(* the boost: [boost_max] of energy, spent 4 a frame of boosting and
 * won back 1 a frame: a second of boost, four to recover *)
val boost_max : int

(*****************************************************************************)
(* {1 The update} *)
(*****************************************************************************)

val new_game : settings -> game

(* one frame of a game: the keys for the humans, the brain for the
 * others *)
val update_game : keyboard -> game -> game

(* the game's winner, if someone has won *)
val winner : game -> int option

(* the kit's own title and scenes: 1 against the computer, 2 two
 * players, two riders on the open grid, the flood fill (TinyTron3d's) *)
val update : computer -> model -> model

(*****************************************************************************)
(* {1 The computers} *)
(*****************************************************************************)

(* [room arena (col, row) limit]: the free cells reachable from (col,
 * row), counted by a breadth-first flood fill, up to [limit] *)
val room : Tilemap.t -> int * int -> int -> int

(* the way with the most room for the cycle, straight on when it's as
 * good; room counted up to [limit] cells (600) *)
val computer_turn : ?limit:int -> Tilemap.t -> cycle -> dir

(* [voronoi arena me foe]: the cells [me] reaches strictly before
 * [foe], minus the ones [foe] reaches first, looking up to 24 cells
 * away (both heads' cells taken) *)
val voronoi : (int * int -> bool) -> int * int -> int * int -> int

(* [search_turn ~depth arena cycles i]: rider [i]'s way, alpha-beta
 * [depth] moves ahead (Minimax.mli) against the nearest rider still
 * riding, the others standing still as walls, the positions scored by
 * [voronoi] *)
val search_turn : depth:int -> Tilemap.t -> cycle list -> int -> dir
