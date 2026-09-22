(* Crush: a level in three dimensions that is crushed flat, to walk
   across what was far apart -- the rules of Crush (Zoë Mode, Sega,
   2007), whatever draws them.

   The level is a stack of 2D slices, front to back, and each state of
   the game is a 2D platformer on one tile map built from them
   ([plane]):

     uncrushed: the slice the player stands in, alone;
     crushed:   the union of all the slices along the camera's axis --
                a cell is solid if any cell behind it is ([project]).

         depth 2   ....####......          crushed
         depth 1   ..............    =>   ..########..##
         depth 0   ..####....##..         (the bridge and the gap
                                          become one floor)

   so the running and jumping are gamekits/platformer's Tile_move on
   either map, unchanged; only the map changes. The camera looks along
   z (the front view) or along x (the side view, [turn]), and a crush
   is along the way it looks. The two rules that make it a game:

     - a crush is refused where it would put Danny inside a block
       ([crush]: "no room");
     - uncrushing puts Danny at the depth of the block he stands on,
       the one nearest the depth he was at ([uncrush]): crushed, he
       walked on a platform that was really somewhere else, and
       uncrushed, he is there.

   A crush is not instant: [squash] goes from 1 (uncrushed) to 0
   (crushed) over a few frames, for the game's picture to show the
   depths coming together, and the play waits meanwhile.

   Part of the crush kit (gamekits/crush/); used by TinyCrush, which
   draws the level in a cabinet projection on the 2D playground, and
   TinyCrush3d, which draws it in 3D. *)

open Playground

(* {1 The levels} *)

(* the slices, front first, each a list of rows from the top: '#' a
 * block, 'E' the exit, 'P' where Danny starts (in the front slice) *)
type level = { name : string; hint : string; slices : string list list }

val levels : level array

(* the size of a cell, in the plane's coordinates *)
val tile : number

(* the grid's size: across, up, deep (x, y, z) *)
val nx : level -> int
val ny : level -> int
val nz : level -> int

(* [cell l x y z]: the cell there, y the row from the top, z the slice
 * from the front; outside the grid, '.' *)
val cell : level -> int -> int -> int -> char

(* {1 The camera's two ways} *)

(* The view: 0 looks along z (the plane is x across), 1 along x (the
 * plane is z across). [across] and [deep_n] are the grid's sizes that
 * way; [at view u d] turns (across, depth) into the grid's (x, z). *)
val across : level -> int -> int
val deep_n : level -> int -> int
val at : int -> int -> int -> int * int

(* [project l view u y]: a cell of the crushed plane: '#' if any depth
 * has a block, else 'E' if one has the exit, else '.' *)
val project : level -> int -> int -> int -> char

(* [plane l view crushed depth]: the tile map Danny plays on: the slice
 * at [depth], or, [crushed], all of them projected. E.g. the first
 * level crushed from the front: its bottom-but-one row is a floor all
 * across, the bridge of slice 2 filling the gap of slice 0. *)
val plane : level -> int -> bool -> int -> Tilemap.t

(* {1 The play} *)

type play = {
  level : int;
  view : int;
  crushed : bool;
  squash : number; (* 1 uncrushed, 0 crushed; in between, the crush drawn *)
  (* Danny: across the plane and up, in the plane's coordinates, and the
   * depth of the slice he is in (the plane's, uncrushed) *)
  u : number;
  y : number;
  depth : int;
  vx : number;
  vy : number;
  ground : bool;
  message : string;
}

(* Danny's box *)
val size : number * number

(* [enter i]: level [i], Danny at its 'P', the front view, uncrushed *)
val enter : int -> play

(* the plane Danny is on now *)
val current : play -> Tilemap.t

(* the rules above: a crush (or its refusal), an uncrush, a quarter
 * turn of the camera (the depth becomes the position across, and the
 * other way round) *)
val crush : play -> play
val uncrush : play -> play
val turn : play -> play

(* {1 A frame} *)

(* what the player does this frame, which is all [step] needs: the
 * games read it off the keyboard ([read_input]), the tests make it up *)
type input = { dx : number; jump : bool; step_deep : int; crush_key : bool; turn_key : bool }

val nothing : input

(* [read_input pressed k]: arrows to run and to step in depth, space to
 * jump, c to crush, tab to turn; [pressed] says whether a key went down
 * this frame (Scene2d.pressed) *)
val read_input : ((keyboard -> bool) -> bool) -> keyboard -> input

(* One frame: the crush drawn if one is under way (and nothing else);
 * else a crush, an uncrush or a turn if asked for, on the ground; else
 * a step in depth (uncrushed, if the next slice has room), then running
 * and jumping on the plane, one pixel at a time (Tile_move). *)
val step : input -> play -> play

(* fallen out of the level; at the exit (the crush over) *)
val fell : play -> bool
val at_exit : play -> bool

(* [frame i p]: [step], then what it leads to: a fall starts the level
 * again, the exit leads to the next one, or to the end *)
type outcome = Going of play | Next_level of play | Finished

val frame : input -> play -> outcome
