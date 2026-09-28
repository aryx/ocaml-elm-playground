(* A mine, the way Descent describes one: a level as a handful of
   boxes, and the openings where two of them touch.

   Doom's levels are a floor plan (gamekits/sectors/Sectors.mli): rooms on
   one floor, seen from above, each with a floor and a ceiling height.
   Descent (Parallax, 1995) made the first level a player could fly
   through in any direction, upside down included, and describes it
   the other way round: not a plan, but a list of closed cells --
   "segments", cubes in Descent, boxes here -- stuck to each other.
   Each side of a box is either rock, or a hole into the box next
   door:

       +--------+                a mine, seen from the side:
       |        |   the shaft    two rooms and a shaft between
       |   0    +---+            them; the sides marked + are
       |        | 1 |            rock, the gaps are openings
       +----+---+   |
            | 2     |
            +-------+

   Two boxes that touch share a rectangle: that rectangle is the
   opening, the rest of the side is rock ([walls] cuts the side around
   it). So a room can open into a corridor a quarter of its size, and
   into several corridors at once, without anything being cut by hand:
   [make] finds every opening from the boxes alone.

   What this gives a game, and why Descent's renderer wants it:
     - the mine is a set of {b convex} cells, each one entirely visible
       from anywhere inside it;
     - the cells are connected only through their openings, which are
       flat rectangles: "portals".
   So from the cell holding the eye, everything else is seen through a
   portal, and through a portal of that one, and so on -- which is how
   TinyDescent draws the mine, nearest cell last, with no
   z-buffer. (TinyDescent3d hands all of it to a z-buffer
   instead; it's the same mine.)

   Moving is the cells' job too ([move]): a point is in the mine if
   some box holds it, so a wall stops the ship and an opening lets it
   through, with no list of walls to test against -- the same service
   Sectors gives Doom, in three dimensions.

   Not Descent's, deliberately: its segments are any six-sided shape,
   not boxes (its tunnels bend and slope), and its sides are made of
   two triangles each. Boxes keep every test to a comparison of
   coordinates.

   Reference: the Descent source code (Parallax Software, released in
   1997): SEGMENT in inferno.h, and the render list of render.c. *)

type vec = float * float * float

(* a box, x0 < x1, y0 < y1, z0 < z1; y is up *)
type box = { x0 : float; y0 : float; z0 : float; x1 : float; y1 : float; z1 : float }

type segment = {
  box : box;
  (* the rock's color *)
  rgb : int * int * int;
  (* how lit this cell is, about 0.3 to 1: a mine is dark *)
  light : float;
}

(* A side of a box: which axis it is across (0 = x, 1 = y, 2 = z) and
 * which end of it ([positive]: the x1, y1 or z1 side). A side is drawn
 * from inside its own box, so its normal points {b into} the box. *)
type side = { axis : int; positive : bool }

(* A flat rectangle of a side, in world coordinates: its 4 corners,
 * counterclockwise seen from inside the box, and the way it faces
 * (into the box). *)
type quad = { corners : vec list; normal : vec; side : side }

(* An opening: the rectangle shared with the box next door. *)
type opening = { into : int; quad : quad }

type level = {
  segments : segment array;
  (* per segment: the rock of its 6 sides, the openings cut out *)
  walls : quad list array;
  openings : opening list array;
  start : vec;
  (* the segment to reach *)
  exit : int;
}

(* [make segments ~start ~exit]: the openings found, wherever two
 * boxes touch on a face and their rectangles overlap. Boxes must not
 * overlap. *)
val make : segment list -> start:vec -> exit:int -> level

(* the mine of TinyDescent and TinyDescent3d: a
 * start room, two ways round to the reactor room (a corridor and a
 * shaft each), and the exit beyond it *)
val mine : level

(*****************************************************************************)
(* {1 Asking} *)
(*****************************************************************************)

(* the segment a point is in, None outside the mine *)
val segment_at : level -> vec -> int option

(* [inside level ~radius p]: a ball of [radius] around [p] fits in the
 * mine: [p] is in a segment, and every side of it within [radius] is an
 * opening wide enough around [p] *)
val inside : level -> radius:float -> vec -> bool

(* [move level ~radius p delta]: [p] moved by [delta], one axis at a
 * time, each step taken only if it keeps the ball inside ([inside]):
 * so a ship sliding along a wall keeps the speed it has along it,
 * which is what flying in a tunnel feels like *)
val move : level -> radius:float -> vec -> vec -> vec

(* [clear level a b]: the straight line from [a] to [b] stays in the
 * mine (tested every unit): a line of sight, or a shot's path *)
val clear : level -> vec -> vec -> bool
