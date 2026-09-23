(* Drawing at a lower resolution, then showing it big: each pixel of a
   small framebuffer copied into a k x k block of the window's.

   Most of a software renderer's work is per pixel -- filling, testing
   depths, shading, sampling textures -- so drawing a 1000 x 1000 window
   at a third of its resolution, 334 x 334, does about 9 times less of
   it. The game doesn't know: the picture is drawn smaller (the screen
   transform scaled by 1/k), and [nearest] blows it up again:

       small (3 x 2)        the window, k = 3
       +---+---+---+        +---------+---------+---------+
       | a | b | c |        | a  a  a | b  b  b | c  c  c |
       +---+---+---+   ->   | a  a  a | b  b  b | c  c  c |
       | d | e | f |        | a  a  a | b  b  b | c  c  c |
       +---+---+---+        +---------+---------+---------+
                            | d  d  d | e  e  e | f  f  f |  ...

   The name: to "pixelate" an image is to show it with its pixels big
   enough to be seen, square blocks of one color. It's the look of the
   low-resolution games, and also the photo effect that hides a face or
   a license plate behind a few big blocks: the same operation, a small
   image blown up without smoothing.

   No interpolation, on purpose: the big square pixels are the look of
   the games of 320 x 200 screens. Doom (1993) had the same trade-off on
   a key: F5, "low detail", halved its horizontal resolution (each
   column drawn twice as wide) to keep the frame rate up on slower PCs.

   What it doesn't make faster: the work per shape or per vertex
   (projecting, clipping, sorting, the game's own [view]), the same at
   any resolution; and work per row (filling a polygon row by row) only
   k times less, not k^2. How much a scene speeds up says which kind of
   work it spends its time on.

   The upscale itself is a copy, a pixel read per pixel written; the
   same idea as a GPU's texture magnification with the "nearest" filter
   (GL_NEAREST), and Blit.sample_nearest, here with an integer factor,
   so no coordinates to compute but a division.
*)

(*****************************************************************************)
(* {1 The setting} *)
(*****************************************************************************)

(* The factor: 1, full resolution (the default), 2, 3 or 4 -- the "r"
 * debug key of the software backends (e.g.
 * playground/software/Playground_platform.ml); a global like
 * Opti.enabled, for the same reason: a switch on the renderer, not on
 * the game. *)
val factor : int ref

(* the next factor, 1 -> 2 -> 3 -> 4 -> 1 *)
val next : unit -> unit

(* e.g. "334x334, x3" for a 1000-pixel window at factor 3, "full" at 1 *)
val name : width:int -> height:int -> string

(*****************************************************************************)
(* {1 Drawing} *)
(*****************************************************************************)

(* [draw big render]: [render] drawing the frame, at the factor's
 * resolution: at 1, [render big ~scale:1.] (nothing changes); else
 * [render small ~scale:(1 / factor)] into a small framebuffer (kept from
 * frame to frame), then [nearest] into [big]. [scale] is for the
 * renderer's screen transform: how many pixels a window pixel is. *)
val draw : Framebuffer.t -> (Framebuffer.t -> scale:float -> unit) -> unit

(*****************************************************************************)
(* {1 The pieces} *)
(*****************************************************************************)

(* [small_size ~factor n]: the side of the small framebuffer for a
 * window side of [n], rounded up so that it covers the window, e.g.
 * 334 for 1000 at factor 3 (334 x 3 = 1002: the last pixels' blocks are
 * cut) *)
val small_size : factor:int -> int -> int

(* the simple version of [nearest], a pixel at a time (see Opti) *)
val nearest_simple : factor:int -> Framebuffer.t -> Framebuffer.t -> unit

(* [nearest ~factor small big]: every pixel (x, y) of [big] set to the
 * pixel (x / factor, y / factor) of [small]. E.g. at factor 2, a 2 x 1
 * small [a b] makes a 4 x 2 big [a a b b; a a b b]. *)
val nearest : factor:int -> Framebuffer.t -> Framebuffer.t -> unit
