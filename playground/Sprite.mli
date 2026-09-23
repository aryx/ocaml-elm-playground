(* Sprites: pixel art typed as strings, and animation frames.

   A sprite is a small picture that moves over the background: a
   character, an enemy, a bullet. Games drew them as pixel art, a few
   colors in a small grid, which can be typed as strings, one character
   per pixel, the way Tilemap types a level:

     let crab = Sprite.pixels 6. [ ('#', green) ] [
       "..#.....#..";
       "...#...#...";
       "..#######..";
       ".##.###.##.";
       "###########";
       "#.#######.#";
       "#.#.....#.#";
       "...##.##..." ]

   Each character of the palette is a colored square of the given size (6
   here: the 11x8 crab is 66x48); any other character ('.' by convention)
   is transparent. The sprite is centered on (0, 0), like every shape, so
   [crab |> move x y] puts its center at (x, y).

   Two techniques, each in its own function:
     - run-length encoding ([runs]): a row's consecutive pixels of the
       same color are drawn as one rectangle, not one square each (the
       crab: 46 pixels, but only 18 rectangles); the classic compression
       of pictures, from fax machines (1980) to the PCX and BMP formats;
     - animation ([cycle], [frame]): a character is a few pictures shown
       one after the other, like a flip book.

   This module is a layer on top of Playground: only its rectangles, so
   it works on every backend. (Images, [Playground.image], are the other
   way to draw a sprite, and animated GIFs animate by themselves: see
   making_sprites.mld; [frame] picks among images too.)

   A bit of history. The name "sprite" is usually credited to engineers
   at Texas Instruments, whose TMS9918 video chip (1979) drew small
   pictures floating over the background like fairies, in hardware:
   the game only gave their position, as our shapes have. Space Invaders
   (Tomohiro Nishikado, 1978) had no such chip: its aliens are 1-bit
   bitmaps, 8 to 12 pixels wide and 8 high, each with two frames that
   alternate as the formation marches (see TinyInvaders.ml). The
   NES's sprites are 8x8 tiles of 3 colors plus transparency (Mario is
   four of them); Susan Kare drew the Macintosh's icons (1984) on graph
   paper, 32x32 pixels, a "happy Mac" among them. The limits made the
   style: pixel art is still a genre of its own (Celeste, Shovel Knight).

   Alternatives, and why strings:
     - image files ([Playground.image], PNGs, GIFs): the way to go for
       real artwork, drawn in an editor (Aseprite, Piskel), but a file
       per picture, loaded at run time, and not editable in the code;
       a sprite *sheet* (all the frames in one image, PICO-8's and most
       engines' way) would need [image] to draw a part of an image, a
       change to Playground itself and every backend
       (plan_playground_other.md section 4);
     - vector shapes (polygons, circles): Asteroids' and Battlezone's
       way (their vector displays had no pixels at all), and already
       the playground's; fine for geometry, not for a face;
     - bitmaps as numbers: what the games really stored, e.g. a Space
       Invaders row as one byte, the NES's CHR data as two bit planes
       (2 bits per pixel: 4 colors), PICO-8's sprites as hexadecimal
       digits; compact, but unreadable in source code;
     - strings (ours): readable, editable in any editor, and the picture
       is visible in the code itself.
   And for drawing them, beyond [runs]: merging rectangles across rows
   too (the "greedy meshing" of voxel games, Mikola Lysenko, "Meshing in
   a Minecraft Game", 0fps.net, 2012: fewer, bigger rectangles), or
   drawing the sprite once into an offscreen image and reusing it (the
   fastest, but it needs a backend API, like playground3d's cached3d).

   Related work:
     - Microsoft MakeCode Arcade's image literals, img`. . 2 2 . .`, one
       character per pixel, each a palette color, in the TypeScript
       source, like ours;
     - PICO-8's sprite sheet (8x8 sprites, a fixed 16-color palette) and
       its spr(n, x, y, w, h, flip_x) (our [pixels], [flip]);
     - STOS Basic (François Lionet, Constantin Sotiropoulos, Mandarin
       Software, 1988) on the Atari ST, and its Amiga successor AMOS
       (1990): a BASIC for writing games, with a sprite editor, a map
       editor, and sprites moved by commands, a generation's first game
       programming environment -- what the playground wants to be;
     - Pyxel (Python), a retro engine in the same spirit, whose images
       are also defined from strings.
*)

open Playground

(*****************************************************************************)
(* {1 Pixel art} *)
(*****************************************************************************)

(* [pixels size palette rows]: the sprite whose pixels are the [rows],
 * from the top, each character a [size] x [size] square of its color
 * in [palette], or transparent if it's not in the palette; drawn with
 * [runs]. Short rows are completed with transparent pixels. *)
val pixels : number -> (char * color) list -> string list -> shape

(* [pixels_squares size palette rows]: the same picture, but one square
 * per pixel, without [runs]: the simple version, to compare (same
 * pixels, more shapes to draw) *)
val pixels_squares : number -> (char * color) list -> string list -> shape

(* [runs row]: the row's runs, (first column, length, character), the
 * transparent ones included, e.g. "..###.#" -> [(0, 2, '.'); (2, 3,
 * '#'); (5, 1, '.'); (6, 1, '#')]. *)
val runs : string -> (int * int * char) list

(* [flip rows]: the rows mirrored left-right, e.g. "##.." -> "..##": a
 * character facing the other way, without drawing it twice (as the
 * NES's sprite attribute bit did, and PICO-8's flip_x). Short rows are
 * completed first, so the columns stay aligned. *)
val flip : string list -> string list

(*****************************************************************************)
(* {1 Two looks: the artwork flag} *)
(*****************************************************************************)

(* A game here can be drawn two ways: with its pixel art, or with the
   plain shapes of the playground -- a red square for a plumber, a
   circle for a ghost. The shapes are the teaching version: the game is
   all there, in a few dozen lines, and nothing is hidden behind a
   picture. The artwork is what the player of the time saw.

   Which one a game draws by default is a question of history, not of
   taste: each draws in the medium its original really used. Space
   Invaders' aliens (1978) were bitmaps and Pac-Man's ghosts (1980)
   were sprites, so those games draw their pixel art; Asteroids (1979),
   Battlezone (1980) and Tron ran on vector displays, and Pong and
   Breakout were rectangles, so those games draw shapes -- and have no
   artwork to switch to. The flag [artwork] changes it either way:

     dune exec games/arcade/TinyPacman.exe -- artwork=shapes
     (in a browser, TinyPacman.html?artwork=sprites)

   A game with two looks reads the flag once, in its view, and says in
   its header which one is its default and why. *)

(* [artwork ~default flags]: whether to draw the pixel art, the flag
 * artwork=sprites or artwork=shapes deciding, and [default] (the
 * original's own medium) when the flag is absent or is neither. *)
val artwork : default:bool -> flags -> bool

(*****************************************************************************)
(* {1 Files} *)
(*****************************************************************************)

(* A sprite drawn in an editor is saved as an XPM file (graphics/images/
 * xpm/Xpm.mli): a palette and rows of characters, what [pixels] takes,
 * and a file GIMP and ImageMagick open too. A game embeds the file at
 * build time (games/README-tools.md), e.g.
 *
 *   let palette, rows = Sprite.of_xpm Mario_sprites.stand
 *   let stand = Sprite.pixels 4. palette rows
 *)

(* [of_xpm text]: the palette and the rows of an XPM file, for [pixels];
 * the transparent colors (None) are left out of the palette, so their
 * characters draw nothing. Raises Failure (see Xpm.parse). *)
val of_xpm : string -> (char * color) list * string list

(* [to_xpm name palette rows]: the XPM file, the C array called [name];
 * the characters of [rows] not in [palette] are written as transparent
 * (None), first, and short rows completed with '.'. [of_xpm] gives the
 * palette and rows back. *)
val to_xpm : string -> (char * color) list -> string list -> string

(*****************************************************************************)
(* {1 Animation} *)
(*****************************************************************************)

(* [cycle n frames]: frame [n] of an animation that loops, e.g. [cycle 5
 * [a; b]] = b, the frame for the n-th step of something moving step by
 * step (an alien marching, a character walking one tile). *)
val cycle : int -> 'a list -> 'a

(* [frame fps time frames]: the frame of a looping animation at [time],
 * showing [fps] frames per second, e.g. at 4 fps, the time 1.3 seconds
 * (5.2 frames since 0) gives frame 5, the second of [a; b]: for things
 * animated by the clock rather than by steps (a flame, a coin turning).
 * [frames] can be shapes, or image urls for [Playground.image]. *)
val frame : number -> time -> 'a list -> 'a
