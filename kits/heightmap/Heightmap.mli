(* A terrain as a grid of heights: an island, made up at startup, its
   height anywhere, and its colors.

   Comanche (NovaLogic, 1992) flew a helicopter over hills no game had
   shown before: not polygons, a "height map", a picture whose every
   pixel is a height, and a "color map", its colors, 1024 x 1024 each.
   A terrain as a height map is a grid of numbers, one per cell:

        j                          a cell's height, the ground there;
        ^   0  0  1  2  1          between the cells, the heights
        |   0  1  3  4  2          around, mixed ([height]): a
        |   0  2  6  5  2          surface, not columns
        |   0  1  3  2  1
        +-------------> i

   It can't have a cave or an overhang (one height per point: "2.5D",
   like Doom's sectors, kits/sectors/Sectors.mli), but it's compact, and
   everything is quick to ask: the ground under a point, a line of
   sight. Drawn two ways: games2.5d/TinyComanche.ml, Comanche's "Voxel Space"
   (each cell a column, drawn screen column by screen column), and
   games3d/TinyComanche3d.ml, the same grid as triangles, the way flight
   simulators drew it.

   The heights are made up ([generate]), by "diamond-square" (Alain
   Fournier, Don Fussell, Loren Carpenter, "Computer Rendering of
   Stochastic Models", CACM 1982; Gavin Miller's "The Definition and
   Rendering of Terrain Maps", SIGGRAPH 1986, named the steps): a coarse
   grid's heights, then the middle of each square and of each edge set
   to its corners' average plus a random bump, the bumps halved (times
   [roughness]) at each finer step. Big bumps make the mountains, small
   ones their slopes: the same look at every scale, a "fractal", like a
   real coast. Comanche's maps were made the same way, then painted.
   Then an island: the heights lowered with the distance to the middle,
   and everything under the sea level flat, the sea.

   The colors ([kind], [light], [color]): by height, sea, sand, grass,
   forest, rock, snow; lit by the slope, facing the sun or not, like
   Comanche's color maps (the shading painted in, once, not computed per
   frame).

   The randomness is a hash of the seed and the cell ([random]), not
   Random: the same island for a seed, native and on the web. *)

type t = {
  (* the grid's side, a power of 2: the cells are (i, j), 0 <= i, j < size *)
  size : int;
  (* the heights, row after row: the cell (i, j) at [j * size + i] *)
  cells : float array;
  (* the highest, and the sea's level: no height below it *)
  top : float;
  sea : float;
}

(* [generate ~seed ~size ~top ~roughness]: an island of [size] x [size]
 * cells, its heights from [sea] to [top] (the sea a fifth of the way
 * up). [roughness], 0 to 1: how much of a step's bumps the next finer
 * step keeps (0.5: half, the usual; more: rougher). *)
val generate : seed:int -> size:int -> top:float -> roughness:float -> t

(* the cell's height; outside the grid, the sea's *)
val cell : t -> int -> int -> float

(* [height t x y]: the ground at the point (x, y), a cell a unit wide:
 * the four cells around mixed by how near each is ("bilinear"), e.g.
 * halfway between a cell at 0 and its neighbor at 4, 2 *)
val height : t -> float -> float -> float

(* [clear t (x1, y1, z1) (x2, y2, z2)]: nothing between the two points,
 * the segment above the ground all along (tested at every cell's
 * length) *)
val clear : t -> float * float * float -> float * float * float -> bool

(* {1 Colors} *)

type kind = Sea | Sand | Grass | Forest | Rock | Snow

(* all of them, lowest first *)
val kinds : kind list

(* by the cell's height: the sea at the sea's level, sand just above,
 * then grass, forest, rock, snow on the tops *)
val kind : t -> int -> int -> kind

(* 0: facing the sun (from the west), 1: flat, 2: away from it; by the
 * cell's height minus its west neighbor's *)
val light : t -> int -> int -> int

(* [color kind light]: (red, green, blue), 0..255, the kind's color,
 * lighter or darker by [light] *)
val color : kind -> int -> int * int * int

(* {1 The pieces} *)

(* [random seed i j]: between -1 and 1, always the same for the same
 * arguments, on every platform (a hash, computed on 30 bits: js_of_ocaml's
 * ints have 32, native ones 63) *)
val random : int -> int -> int -> float
