(* Slope: curved ground, the way Sonic the Hedgehog does it.

   Tile_move.mli moves a box against solid tiles: a tile is in or out,
   so the ground is flat, and a hero can only stand on top of it. That
   is Mario's world. A hero who runs up a hill, round a loop and along a
   ceiling needs two things instead:

   - a tile that is not a block but a *shape*: a little bitmap of which
     of its pixels are solid, plus the angle of the surface those pixels
     make. A slope, a curve or a step is then data, not geometry:

         a tile of a 45 degrees hill,        . . . . . . . #
         its 8 x 8 bitmap and angle = 45     . . . . . . # #
                                             . . . . . # # #
         (Sonic stored two arrays per        . . . . # # # #
         tile instead, the heights seen      . . . # # # # #
         from above and the widths seen      . . # # # # # #
         from the side, to save memory;      . # # # # # # #
         a bitmap says the same thing        # # # # # # # #
         and every sensor can read it
         along its own axis)

   - sensors: instead of a box that must not overlap a tile, a few
     points that each look for the surface along a line and say where it
     is and which way it faces ([ground]). The hero is then *placed on*
     the surface, not pushed out of it.

   The angle is what makes a loop work. The hero has a [mode] --
   standing on a floor, on a right wall, under a ceiling, on a left wall
   -- and the mode follows the angle of the ground it stands on
   ([mode_of]). All four modes run the same code with the axes turned;
   in the wall modes "down" points sideways, so running fast along a
   curve simply keeps the hero on it. There is no special case for the
   loop anywhere: it's tiles with angles, like a hill.

               ,--''''--.        going round: the angle passes 0, 90,
             ,'          `.      180, 270, and the mode with it. Too
            /     loop     \\     slow, and the hero falls off, which
           |                |    is the same rule, not another one.
            \\              /
             `.          ,'
       ________`--,,,,--'________

   The falling off comes for free from speed, not from a test of where
   the hero is: on the ground the speed is a scalar along the surface,
   and the game gives it up when it is too small on a steep angle (see
   games/TinySonic's [slip]).

   Reference: the Sonic Physics Guide (the Sonic Retro community's
   reverse engineering of Sonic 1 to 3, 2010s), which named the sensors
   and the modes; Sonic 1 is Yuji Naka, Hirokazu Yasuhara and Naoto
   Ohshima, Sega, 1991.

   Part of the platformer kit (kits/platformer/), with Tile_move.mli
   (blocks) and Ladder.mli; used by games/TinySonic. *)

open Playground

(* A tile's shape: [solid] is its size x size pixels, row by row from
   the tile's bottom-left corner, and [angle] is the direction the
   surface they make faces (its normal), in degrees counter-clockwise:
   0 for a floor, 90 for a wall on the hero's right, 180 for a ceiling. *)
type surface = { solid : bool array; angle : number }

(* Which way is "down" for the hero: the mode it walks in *)
type mode = Floor | Right_wall | Ceiling | Left_wall

(* [mode_of angle]: the mode of a hero standing on ground of this angle:
   a floor from -45 to 45 degrees, then a wall, a ceiling, the other
   wall. *)
val mode_of : number -> mode

(* [down mode]: the unit vector the hero is pulled along in this mode,
   e.g. (0, -1) on a floor, (-1, 0) on a right wall *)
val down : mode -> number * number

(* [ground ~tiles ~size mode (x, y)]: what a sensor at (x, y) finds
   along [down mode] -- where the hero's feet rest, as a coordinate on
   that axis, and the angle of the tile the surface belongs to. None
   when there is no solid pixel within a tile either way.

   The sensor looks both ways, which is what keeps a hero on the ground
   over a bump: forwards for the first solid pixel (it is above the
   ground, walking off a slope), and backwards out of the solid it
   already stands in (it is inside the ground, walking into one).

   [reach] is how far it looks, a tile by default (Sonic's own sensors
   look 16 pixels). A game whose tiles are coarse, or whose hero can end
   up deep inside the ground after a fast landing, can ask for more: the
   search is a pixel walk, so the cost is that many steps. *)
val ground :
  tiles:(int * int -> surface option) -> size:int -> ?reach:int -> mode -> number * number -> (number * number) option

(* [block size]: every pixel solid: the inside of the ground, a wall *)
val block : int -> surface

(* [slope size ~from_ ~to_]: solid below a line rising evenly from
   [from_] pixels on the tile's left to [to_] on its right, with the
   angle that goes with it ([slope 16 ~from_:0 ~to_:16] is a 45 degrees
   hill, [slope 16 ~from_:16 ~to_:16] a flat floor) *)
val slope : int -> from_:int -> to_:int -> surface

(* [ring size ~cx ~cy ~radius ~thickness ~inside]: the tile's pixels
   that fall in the band between [radius] and [radius + thickness] of a
   circle centered on ([cx], [cy]) -- given in the tile's own pixels, so
   the center usually falls outside it. A loop is the tiles of one such
   band: a ring of ground, sky on both sides of it. [inside] says which
   face the hero walks on, and so which way the angle points: inward for
   a loop he runs round the inside of, outward for a ball he runs over.
   The angle is the circle's where it crosses the middle of the tile. *)
val ring : int -> cx:number -> cy:number -> radius:number -> thickness:number -> inside:bool -> surface

(* [empty size]: no solid pixel: the sky. [tiles] may return None
   instead. *)
val empty : int -> surface
