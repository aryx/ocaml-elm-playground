(* Orders: how a strategy game's units are told where to go, and how
   they get there.

   A real-time strategy game is mostly this: a tile map where some tiles
   can be walked on, units standing between tiles at fractional
   positions, and orders that turn a place on the map into a way to walk
   there. The searches themselves are ai/Pathfind's; this is the layer
   between them and a game -- the grid as a search problem, and the
   walking.

   Three kinds of order, and each game uses all three:

     one unit, one place     [path]: an A* to that tile. What a click on
                             the ground means for a single unit.
     one unit, any place     [nearest]: an A* whose goal is a question
                             ("a tile with spice", "a tile beside a
                             forest"), so the search stops at whichever
                             is closest through the rocks. What a
                             harvester or a peasant does by itself.
     a crowd, one place      [field] once, then [downhill] per unit and
                             per step: one Dijkstra from the place gives
                             the cost from it to every tile, and any
                             number of units walk down it. Ten units
                             cost one search instead of ten, and a unit
                             pushed off its way doesn't need a new one.

   The walking is deliberately simple: a unit slides toward the center
   of the next tile at [speed] tiles a frame ([toward]), and [advance]
   drops a tile from the path once it stands on it. Positions are in
   tiles, not pixels, so a game can draw its tiles at any size.

        path = [(3,4); (4,4); (5,4)]     the unit is at (3.6, 4.0):
        . . . o-->o . .                  advance drops (3,4) once it
        . . . . u . . .                  reaches (4,4), and aims at (5,4)

   What is *not* here, because the two games do it differently: what a
   unit does when it arrives (TinyDune2's harvester digs where it
   stands, TinyWarcraft2's peasant works the tile beside it), and how a
   blow lands (a shell at range, a sword next to you).

   Part of the RTS kit (gamekits/rts/); used by TinyDune2 and
   TinyWarcraft2. *)

open Playground

(* [problem ~walkable target]: the search problem of a grid where
   [walkable] says which tiles a unit may stand on, aimed at [target]
   (the Manhattan distance as A*'s estimate, since units move in the
   four directions) *)
val problem : walkable:(int * int -> bool) -> int * int -> (int * int) Pathfind.problem

(* [path ~walkable ~from target]: the tiles from [from] to [target],
   both included; [] when there's no way (or the unit is walled in) *)
val path : walkable:(int * int -> bool) -> from:int * int -> int * int -> (int * int) list

(* [nearest ~walkable ~from wanted]: the way to the closest tile [wanted]
   accepts, [] if none can be reached. No estimate guides this search:
   where it's going isn't known until it gets there. *)
val nearest : walkable:(int * int -> bool) -> from:int * int -> (int * int -> bool) -> (int * int) list

(* [field ~walkable target]: the cost from [target] to every tile that
   can reach it -- the crowd's order, computed once *)
val field : walkable:(int * int -> bool) -> int * int -> ((int * int) * float) list

(* [downhill ~walkable field cell]: the neighbour of [cell] that the
   field says is closer, None where the field doesn't reach (or at its
   target) *)
val downhill : walkable:(int * int -> bool) -> ((int * int) * float) list -> int * int -> (int * int) option

(* [toward ~speed (x, y) cell]: the position moved [speed] tiles toward
   the center of [cell], stopping exactly on it *)
val toward : speed:number -> number * number -> int * int -> number * number

(* [advance ~speed (x, y) path]: one frame of walking along [path]: the
   new position, and the path with the tiles already reached dropped. An
   empty path leaves the unit where it is. *)
val advance : speed:number -> number * number -> (int * int) list -> (number * number) * (int * int) list

(* [cell_of (x, y)]: the tile a unit standing at (x, y) is on *)
val cell_of : number * number -> int * int
