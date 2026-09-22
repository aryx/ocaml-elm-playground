(* Sectors: a level the way Doom (id Software, 1993) describes one, a
   floor plan of sectors, each a floor height, a ceiling height and a
   light, with the walls between them.

   From above, a level is flat; each sector is a region of it, a polygon
   (with holes: a pillar in a hall is a hole in the hall's sector), and
   the third dimension is only numbers: how high its floor and its
   ceiling are. No room is ever above another -- hence "2.5D":

         the plan, from above              seen from the side
     +----------+---+                        ceiling 192   +---+ 256
     | hall     |st.|  upstairs               +------------+   |
     |  floor 0 | 16|  floor 64               | hall     st|air|
     +----------+---+                        +---floor 0--+---+ 64

   The walls are found, not written ([make]): an edge of a sector's
   polygon that another sector also has, the other way round, is a
   two-sided line (a step, a window: what's between two sectors is
   drawn only where their floors or ceilings differ); an edge no other
   sector has is a one-sided line, a wall. A line's front is the sector
   on its right, going from its first point to its second, Doom's
   convention (its linedefs' "right side"). So a sector's polygon goes
   clockwise (with the y axis up), and a hole counterclockwise: the
   sector is always on the right. Edges needn't meet at the same points:
   a hall's side is cut where the stairs start ([make] splits an edge at
   every other polygon's point lying on it, as a map editor does).

   What the games do with a level is theirs: TinyDoom draws it the
   way Doom did, with a BSP tree and no z-buffer; TinyDoom3d
   turns it into polygons for a z-buffer. What they share is here: the
   level, finding the sector at a point, and moving the player in it.

   A kit (gamekits/sectors/), a layer on top of nothing, only numbers, like
   Road.mli is for racing games (see docs/claude_notes/plans/
   plan_games3d.md, section 4).

   References: the Doom source code (released by id in 1997), its data
   (the "Unofficial Doom Specs", Matthew Fell, 1994: THINGS, LINEDEFS,
   SIDEDEFS, VERTEXES, SECTORS); Fabien Sanglard, "Game Engine Black
   Book: Doom" (2018). Ken Silverman's Build engine (Duke Nukem 3D, 1996)
   used sectors too, joined by portals instead of a BSP.
*)

(* {1 Levels} *)

type point = float * float

type sector = {
  floor : float;
  ceiling : float;
  light : float; (* 0 (dark) to 1 *)
  floor_rgb : int * int * int;
  ceiling_rgb : int * int * int;
  wall_rgb : int * int * int; (* the walls on its side *)
  loops : point list list; (* its outline clockwise, its holes counterclockwise *)
}

(* A line, between two points; [front] (a sector's index) is on its
 * right; [back], if any, on its left: a two-sided line *)
type line = { x1 : float; y1 : float; x2 : float; y2 : float; front : int; back : int option }

type level = {
  sectors : sector array;
  lines : line array;
  start : float * float * float; (* where the player starts: x, y, angle in degrees *)
  exit : int; (* the sector to reach *)
}

(* [rect x1 y1 x2 y2]: a rectangle clockwise, for a sector's outline;
 * [hole x1 y1 x2 y2] the same counterclockwise, for a hole in one *)
val rect : float -> float -> float -> float -> point list
val hole : float -> float -> float -> float -> point list

(* [make sectors ~start ~exit]: the level, its lines found from the
 * sectors' edges. E.g. two squares side by side, [rect 0 0 10 10] and
 * [rect 10 0 20 10]: 7 lines, one of them two-sided (x = 10, from
 * (10, 10) down to (10, 0), the first square in front); and with the
 * second only half as tall, [rect 10 0 20 5], the first's right side
 * is cut at (10, 5): 8 lines, its upper half one-sided. Fails on
 * an edge two sectors have the same way round (overlapping sectors). *)
val make : sector list -> start:float * float * float -> exit:int -> level

(* the level of TinyDoom and TinyDoom3d: a hall with a
 * pillar, stairs up to a room, a corridor, stairs down to a dark room
 * with the exit, seen from the hall through a window *)
val outpost : level

(* {1 Where things are} *)

(* [side line x y]: < 0 when (x, y) is on the line's front side (its
 * right), > 0 on its back side, 0 on it *)
val side : line -> float -> float -> float

(* [distance line x y]: from (x, y) to the nearest point of the line
 * (its segment, not the infinite line), e.g. 5 from (5, 5) to the line
 * from (0, 0) to (10, 0), and 5 from (13, 4) (to its end (10, 0)) *)
val distance : line -> float -> float -> float

(* [inside sector x y]: (x, y) is in the sector: in its outline, not in
 * a hole (a point crosses its loops an odd number of times going right) *)
val inside : sector -> float -> float -> bool

(* [sector_at level x y]: the sector (x, y) is in, by trying them all
 * (the simple way; Doom walked its BSP tree, see TinyDoom); 0 if
 * none *)
val sector_at : level -> float -> float -> int

(* {1 Moving} *)

(* the player: a circle of radius 16, 56 high, who can climb steps of
 * up to 24 (Doom's numbers, in its map units) *)
val radius : float
val height : float
val step : float

(* [move level sector_at (x, y) (dx, dy)]: where the player at (x, y)
 * ends up trying to move by (dx, dy): there if no line blocks, else
 * sliding along x or y alone, else not moving. A line blocks if it's a
 * wall, or if the sector on its other side is a step higher than
 * [step] or has less than [height] between floor and ceiling. The
 * sector the player is in by [sector_at] (e.g. [sector_at level], or a
 * BSP's). E.g. in [outpost], walking at the pillar stops 16 short of
 * it. *)
val move : level -> (float -> float -> int) -> float * float -> float * float -> float * float
