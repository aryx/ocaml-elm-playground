(* Tile_move: moving a box against the tiles of a map.

   A platformer's hero, its enemies, a rolling boulder, are boxes moving
   through a level typed as tiles (see Tilemap.mli), and stopped by the
   solid ones. The simplest way that never goes wrong: move one pixel at
   a time, stopping before the first step into a solid tile. That's how
   Celeste and TowerFall move their characters (Maddy Thorson, "Celeste
   and TowerFall Physics", 2017): slow in theory, but a character moves
   only a few pixels per frame, and it can't go through a thin wall even
   when fast (a big step could jump over it: the "tunneling" of physics
   engines, see physics/2d/Collide.mli's swept tests).

   Call it once for x, then once for y ([move_by] (dx, 0.), then (0.,
   dy)): running into a wall while falling then stops only the running,
   and the hero slides down along the wall.

         +----+                 falling, running right into a wall:
         |    |-->  ####        x stopped at the wall, y still falls:
         +----+     ####        the hero slides down it
            |       ####
            v

   A box is given by its center and its size (w, h): the playground's
   way, every shape centered on its position.

   Part of the platformer kit (gamekits/platformer/), with Ladder.mli; used
   by games/TinyMario, games/TinyLodeRunner and games/TinyRick. *)

open Playground

(* [hits solid map (w, h) x y]: the w x h box centered on (x, y) enters
 * a [solid] tile (Tilemap.hits: touching one isn't entering it) *)
val hits : (char -> bool) -> Tilemap.t -> number * number -> number -> number -> bool

(* [move_by solid map (w, h) (x, y) (dx, dy)]: the box moved by (dx, dy),
 * at most one pixel at a time, stopped before the first step into a
 * [solid] tile; where it stopped, and whether it hit something. E.g.
 * a 20 x 20 box at (0, 0) moving (100, 0) towards a wall starting at x
 * = 50 stops at (40, 0), true: its right side against the wall. *)
val move_by : (char -> bool) -> Tilemap.t -> number * number -> number * number -> number * number -> (number * number) * bool

(* [on_ground solid map (w, h) x y]: a [solid] tile right under the box:
 * one pixel lower, it would enter it *)
val on_ground : (char -> bool) -> Tilemap.t -> number * number -> number -> number -> bool
