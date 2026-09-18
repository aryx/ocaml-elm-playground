(* Tile maps: worlds as grids of tiles, written as strings.

   Most 2D games' worlds are grids: a level is rows of cells, each cell
   one tile from a small set (a wall, a floor, a brick, a coin), and the
   level's author types it like text:

     let level = Tilemap.of_strings 50. [
       "#                  #";
       "#   ?B?      $ $   #";
       "#  @       #####   #";
       "####################" ]

   A tile map gives a game three things for the price of a few strings:
     - the world's picture: [view] draws each character with a shape;
     - the world's data: which tile is at a point ([tile_at]), where the
       player starts ([find] '@'), a coin taken ([set] ' ');
     - collisions, cheaply: a box can only hit the few tiles under it
       ([hits]), found by a division, where a world of polygons would
       have to test them all (what the physics plan calls the broad
       phase, here for free).

   Coordinates: the map is centered on (0, 0), like the playground's
   screen, so a map that fits on the screen needs no camera. Cells are
   numbered by (col, row), from the top-left, the order they are typed
   in, rows going down while the world's y goes up; e.g. the 6x3 map
   below, with tiles of size 10, is 60 wide, 30 high:

        x = -30   -20   -10    0     10    20    30
     y = 15 +-----+-----+-----+-----+-----+-----+
            | 0,0 | 1,0 | 2,0 | 3,0 | 4,0 | 5,0 |    "#....#"
          5 +-----+-----+-----+-----+-----+-----+
            | 0,1 | 1,1 | @   | 3,1 | 4,1 | 5,1 |    "#.@..#"
         -5 +-----+-----+-----+-----+-----+-----+
            | 0,2 | 1,2 | 2,2 | 3,2 | 4,2 | 5,2 |    "######"
        -15 +-----+-----+-----+-----+-----+-----+

   The '@' is in cell (2, 1), whose center is at (-5, 0) (see [center]);
   the point (-5, 0), or (-9, 4), is in cell (2, 1) (see [cell]).

   This module is a layer on top of Playground and Camera2d (for
   [Camera2d.rect]): a big map is shown through a camera, and drawn only
   where the camera looks ([view_visible]).

   A bit of history. Tiles started as a hardware trick: memory was too
   expensive for a frame buffer, so arcade boards and consoles stored the
   screen as a grid of tile numbers, plus the few pixel patterns (8x8)
   the tiles are made of, and drew the pixels on the fly. Galaxian
   (Namco, 1979) had tiles for the background and sprites for what
   moves; the maze of Pac-Man (Namco, 1980) is 28x36 tiles, and a
   pac-dot is a tile; the NES's "name tables" are 32x30 tiles, which the
   scroll registers (see Camera2d) slide across. Super Mario Bros.
   (1985) builds its levels from 16x16 blocks of tiles (a brick, a '?'
   block, a pipe), The Legend of Zelda (1986) its overworld from screens
   of 16x11 of them.

   Levels as text are as old: Rogue (1980) *is* its map of characters
   ('#' a corridor, '@' the player), and Sokoban (Thinking Rabbit, 1982)
   levels are still exchanged in its text format, the one of our
   example: '#' a wall, '@' the player, '$' a box, '.' a goal.

   Related work:
     - Microsoft MakeCode Arcade (2019), a game platform for teaching
       (blocks or TypeScript, on the web and on cheap handhelds), has
       tile maps at its center: tiles.setTilemap, tiles.getTileLocation,
       and a camera following a sprite (scene.cameraFollowSprite);
     - Thomas Ball, Stefania Druga, et al., "TileCode: Creation of Video
       Games on Gaming Handhelds" (Microsoft Research, 2020): a game is
       a tile map plus rules, each rule a 3x3 pattern around a sprite
       ("when") and what the sprite then does ("do"), inspired by board
       games like checkers; 10-15 rules suffice for a variety of games.
       Close to PuzzleScript (Stephen Lavelle, 2013), whose games are
       text levels plus rewrite rules;
     - PICO-8's map, mget(x, y), mset(x, y, tile) (our [view], [get],
       [set]), and its sprite flags to tell which tiles are solid (our
       [hits]' predicate);
     - Tiled (Thorbjørn Lindeijer, 2008), the map editor most engines
       read the maps of, and its TMX format: layers of tile numbers;
     - Rodrigo Monteiro, "The guide to implementing 2D platformers"
       (2012): tile-based collisions, from the simplest to Mario's;
     - Maddy Thorson, "Celeste and TowerFall Physics" (2017): moving
       one pixel at a time against a grid (see games/Platformer.ml).
*)

open Playground

type t

(* [of_strings size rows]: a map with square tiles of [size], one row per
 * string, from the top; short rows are completed with ' ' *)
val of_strings : number -> string list -> t

(* the size of a tile, and of the map: its number of columns (the
 * longest row) and rows, and its world coordinates (e.g. for the example
 * map above: left -30, right 30, bottom -15, top 15) *)
val size : t -> number
val cols : t -> int
val rows : t -> int
val bounds : t -> Camera2d.rect

(* {1 Cells} *)

(* [get map col row]: the tile in cell (col, row), [None] outside the map;
 * [set map col row c]: a new map, with [c] in that cell (the map is a
 * value, like the rest of a model); outside the map, the same map *)
val get : t -> int -> int -> char option
val set : t -> int -> int -> char -> t

(* [find map c]: the cells holding [c], row after row, e.g. the player's
 * start ('@'), the enemies, the coins to count *)
val find : t -> char -> (int * int) list

(* [center map col row]: the world coordinates of the cell's center; the
 * other way round, [cell map x y] is the cell a world point is in (maybe
 * outside the map, e.g. (-1, 0) left of it); [tile_at map x y] its tile.
 * With the example map above: center (2, 1) = (-5, 0), cell (-9, 4) =
 * (2, 1), tile_at (-9, 4) = Some '@', tile_at (100, 0) = None.
 * A point on the border between two cells is in the right one, or the
 * lower one. *)
val center : t -> int -> int -> number * number
val cell : t -> number -> number -> int * int
val tile_at : t -> number -> number -> char option

(* {1 Drawing} *)

(* [view tile map]: the map's picture, [tile c] drawn centered on each
 * cell holding [c], e.g.
 *
 *   Tilemap.view (function
 *     | '#' -> square brown 50.
 *     | '$' -> circle yellow 15.
 *     | _ -> group []) level
 *
 * ' ' is the empty tile: never drawn. *)
val view : (char -> shape) -> t -> shape

(* [view_visible rect tile map]: the same, but only the cells touching
 * [rect], e.g. [Camera2d.visible screen cam]: what's off-screen isn't
 * drawn at all ("culling"). A 200x20 level has 4000 cells, but a
 * 1000x1000 screen shows only 21x20 tiles of 50 (the partial ones
 * included); drawing them all every frame is what makes a big level
 * slow, especially on the software rasterizer. *)
val view_visible : Camera2d.rect -> (char -> shape) -> t -> shape

(* {1 Collisions} *)

(* [hits solid map x y w h]: whether the w x h box centered on (x, y)
 * overlaps a cell whose tile is [solid]. Only the cells under the box
 * are looked at: a handful for a box the size of a tile, whatever the
 * map's size. A box touching a tile, but not entering it, doesn't hit
 * it: a player standing on the ground (their bottom = the ground's top)
 * isn't stuck in it. Outside the map, nothing is solid (a game falls off
 * a level, or walls it).
 *
 * With the example map above and ((=) '#') as [solid], a 10x10 box: at
 * (-5, 0), exactly on the '@' cell, it hits nothing (it touches the
 * floor below, and the walls are not next to it); at (-5, -1) it enters
 * the floor's row: it hits; at (-16, 0) it enters the left wall's
 * column: it hits.
 *
 * What to do when a move hits is the game's business: a common answer,
 * e.g. Celeste's, is to move one pixel at a time, stopping before the
 * first one that hits (see games/Platformer.ml). *)
val hits : (char -> bool) -> t -> number -> number -> number -> number -> bool
