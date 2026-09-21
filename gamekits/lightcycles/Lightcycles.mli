(* The light cycles of Tron, as a model and an update, with no picture:
   the arena, the two cycles, their trails, the crashes, the rounds, the
   computer player, the title and winner scenes.

   The pictures are the games': games/TinyTron.ml draws it in 2D, from
   above, a pixel per cell; games3d/TinyTron3d.ml in 3D, the trails as
   walls, seen from behind a cycle, from inside it, from above, or from
   far away. The same model, the same update, two views: that's the Elm
   architecture's promise (the view is a function of the model, and
   nothing else), and the reason this kit exists.

   The arena is a Tilemap of characters: ' ' free, '#' the walls, '1'
   and '2' the trails. A move is Tilemap.set, a crash a Tilemap.get that
   isn't ' '. A cycle also keeps the corners of its trail, where it
   turned: a trail is then a few straight lines, which a 3D view draws
   as a few walls instead of a box per cell.

   The computer ([computer_turn]) takes, at each step, the way leading
   to the most room: the free cells it could still reach from there, a
   flood fill ([room]); straight on when it's as good. See
   games/TinyTron.ml's header for the history.

   A kit (gamekits/, see docs/claude_notes/plan_games.md): a layer on top of
   the playground for the games of a family.
*)

open Playground

(* {1 The arena} *)

(* 90 cells each side, walls included *)
val size : int

(* the size of a cell in the Tilemap, in pixels: the 2D view's *)
val cell : number

type dir = Up | Down | Left | Right

(* [delta Up] = (0, -1): rows going down, as a Tilemap's *)
val delta : dir -> int * int

val opposite : dir -> dir

type cycle = {
  col : int;
  row : int;
  dir : dir;
  wanted : dir; (* the last direction asked for: a turn happens at the next step *)
  mark : char; (* its trail's character, '1' or '2' *)
  corners : (int * int) list; (* where it turned, the last first, and where it started *)
}

(* {1 The model} *)

type round = {
  arena : Tilemap.t;
  p1 : cycle; (* blue, the arrows *)
  p2 : cycle; (* orange, w/a/s/d or the computer *)
  over : (int * int) option; (* when over, the round's points (0 or 1 each) *)
  pause : int; (* then, the frames to wait before the next round *)
  frames : int;
}

type game = { round : round; score1 : int; score2 : int; computer : bool }

(* the title (1: against the computer, 2: two players), the game, the
 * winner (space: back to the title) *)
type scene = Title | Playing of game | Winner of game

type model = scene Scene2d.t

val initial_model : model

(* a cycle moves one cell every [step] frames; [rounds_to_win] wins *)
val step : int

val rounds_to_win : int

(* {1 The update} *)

val update : computer -> model -> model

(* {1 The computer} *)

(* [room arena (col, row) limit]: the free cells reachable from (col,
 * row), counted by a breadth-first flood fill, up to [limit] *)
val room : Tilemap.t -> int * int -> int -> int

(* the way with the most room for the cycle, straight on when it's as
 * good *)
val computer_turn : Tilemap.t -> cycle -> dir
