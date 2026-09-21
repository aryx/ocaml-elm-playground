(* Moving along the corridors of a grid, the way Pac-Man and Bomberman
   move: always on the lines between tile centers, turning only at a
   tile's center, one pixel at a time.

       +---+---+---+
       | . | . | . |      a mover goes from center to center (the dots);
       +---+---+---+      at a center, it may turn; between two centers,
       | . |###| . |      it can only go on, or turn back
       +---+---+---+

   And the trick that makes such games feel good: the turn you ask for
   is remembered ([wanted]) until the corridor allows it. You press up
   *before* the junction, and the character turns up when it gets
   there, instead of you having to press at the exact frame. Pac-Man
   (Namco, 1980) did it (and even let Pac-Man cut corners, turning a few
   pixels early: an exercise); games without it feel stiff.

   Positions are integers, in pixels from the center of tile (0, 0), so
   that "at the center of a tile" is exact: with tiles of 40, tile
   (col, row)'s center is at (40 * col, 40 * row), rows going down (the
   order the rows of a Tilemap are typed in).

   Part of the maze kit (gamekits/maze/), a layer on top of the playground
   for grid games; see Chase.mli for the other movers' choices, and
   docs/claude_notes/plan_games.md, "kits". Reference: Jamey Pittman,
   "The Pac-Man Dossier" (2009), on Pac-Man's movement ("cornering").
*)

open Playground

type dir = Up | Down | Left | Right | Stop

(* [delta Up] = (0, -1): one tile in that direction, rows going down *)
val delta : dir -> int * int
val opposite : dir -> dir

(* The grid: its tiles' size in pixels, and its size in tiles. Moving
 * off one side comes back on the other, horizontally (Pac-Man's tunnel:
 * a maze closed by walls just never does it). *)
type grid = { tile : int; cols : int; rows : int }

(* A mover: its position, where it goes, where it's been asked to go *)
type mover = { gx : int; gy : int; dir : dir; wanted : dir }

(* still, at the center of tile (col, row) *)
val mover_at : grid -> int * int -> mover

val at_center : grid -> mover -> bool

(* the tile the mover is on: the one whose center is the nearest *)
val tile_of : grid -> mover -> int * int

(* the tile next to the mover's, in a direction *)
val next_tile : grid -> mover -> dir -> int * int

(* [slide grid ~choose speed m]: [speed] pixels, one at a time; at each
 * tile center reached, [choose] may change the direction (or stop the
 * mover, with Stop). Speeds need not divide the tile's size: going
 * pixel by pixel, no center is ever skipped. *)
val slide : grid -> choose:(mover -> mover) -> int -> mover -> mover

(* [steer grid ~open_ m], the player's choice at a center: the [wanted]
 * way if it's open, else straight on if it's open, else stop; [open_
 * (col, row)] says whether a tile can be entered *)
val steer : grid -> open_:(int * int -> bool) -> mover -> mover

(* [move_player grid ~open_ speed m]: [slide] with [steer], after an
 * immediate U-turn if [wanted] is the way back (allowed anywhere, not
 * only at centers) *)
val move_player : grid -> open_:(int * int -> bool) -> int -> mover -> mover

(* [to_world grid bounds m]: the mover's position in the world, for a
 * grid drawn as the Tilemap whose bounds are [bounds] (the map's
 * top-left tile at its top-left corner) *)
val to_world : grid -> Camera2d.rect -> mover -> number * number
