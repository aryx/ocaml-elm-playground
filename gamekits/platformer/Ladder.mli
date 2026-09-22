(* Ladder: climbing ladder tiles.

   Space Panic (Universal, 1980) and Donkey Kong (1981) had ladders
   before they had platformers' jumps; Lode Runner (1983) and Rick
   Dangerous (1989) are made of them. On tiles, a ladder is a tile you
   can be inside, and three rules make it work:

     - to climb, the body must reach a ladder: the tile at its center,
       or the one just below its feet -- the second, so that standing
       on top of a ladder you can climb down it ([reach]);
     - climbing, the body is on the ladder's column (its x set to the
       column's center: a player half a tile off still climbs), and
       moves up or down while it still reaches the ladder: it stops with
       its feet on the ladder's top, and at the floor below ([climb]);
     - the top of a ladder is a floor: a body there doesn't fall
       ([standing]).

        ....     .. the body climbing up the ladder (H) stops when its
        .@..        feet are on the ladder's top: one pixel higher, it
        .H..        would reach no ladder (its center out of it, and
        .H..        nothing below its feet); there it stands, as on a
        ####        floor, and can walk off sideways

   Positions and boxes as in Tile_move.mli: a body is its center and
   its size (w, h).

   Part of the platformer kit (gamekits/platformer/), with Tile_move.mli;
   used by TinyLodeRunner and TinyRick. *)

open Playground

(* [reach is_ladder map (w, h) x y]: the x of the center of the column
 * of the ladder the body can climb, if any: the tile at its center, or
 * the one just below its feet *)
val reach : (char -> bool) -> Tilemap.t -> number * number -> number -> number -> number option

(* [standing solid is_ladder map (w, h) x y]: something holds it up: a
 * [solid] tile right under it, or a ladder (the top of one, or one it's
 * on) *)
val standing : (char -> bool) -> (char -> bool) -> Tilemap.t -> number * number -> number -> number -> bool

(* [on_top is_ladder map (w, h) x y]: its feet on the top of a ladder
 * (a ladder just below them, none at its center): for a game where
 * only the top is a floor, a hero jumping across a ladder not caught
 * by it (TinyRick; in TinyLodeRunner, [standing]: a runner
 * on a ladder doesn't fall) *)
val on_top : (char -> bool) -> Tilemap.t -> number * number -> number -> number -> bool

(* [climb solid is_ladder map (w, h) (x, y) dy]: on the ladder's column,
 * moved by [dy] one pixel at a time, as long as it still reaches a
 * ladder and doesn't enter a [solid] tile; the body where it stopped,
 * unchanged when it reaches no ladder. E.g. with tiles of 10, a ladder
 * two tiles high on a floor whose top is at y = -10 (so the ladder's
 * top is at y = 10), a 6 x 10 body standing at its foot, (5, -5),
 * climbing 100 up stops at (5, 15): its feet on the ladder's top. *)
val climb : (char -> bool) -> (char -> bool) -> Tilemap.t -> number * number -> number * number -> number -> number * number
