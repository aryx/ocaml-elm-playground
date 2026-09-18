(* A framebuffer: the grid of pixels an image is made of, as one big
 * array of numbers in memory -- the thing every other module in raster/
 * writes into, and the thing the screen displays.
 *
 * Pixel (x, y) is column x, row y, with (0, 0) the top-left corner and
 * y going *down* (the convention of screens, image files, and SDL --
 * not Elm's, see Affine for the conversion). Each pixel is one 32-bit
 * integer 0xAARRGGBB: 8 bits each of alpha, red, green, blue. For
 * example 0xFF_CC_00_00 is an opaque red (Playground's [red],
 * "#cc0000"). The alpha byte is only there because that's the layout
 * SDL's window surface uses; we always store 0xFF (opaque) in it.
 *
 * Colors passed to the functions below are 0xRRGGBB ints (no alpha
 * byte; e.g. 0xcc0000), and transparency is a separate [alpha] float,
 * from 0. (invisible) to 1. (opaque), like Playground's [fade]. *)

(* Rows of pixels: pixels.{y, x}. Using exactly SDL's own memory layout
 * means drawing into a framebuffer *is* drawing into the window, with
 * no copy (see [of_pixels]). *)
type pixels = (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array2.t

type t = { width : int; height : int; pixels : pixels }

(* A new framebuffer, all white; for offscreen drawing and tests *)
val create : width:int -> height:int -> t

(* A framebuffer on top of existing memory, e.g. an SDL window surface's
 * pixels (see playground/software/Playground_platform.ml) *)
val of_pixels : pixels -> t

(* Fill the whole framebuffer with one (opaque) color *)
val clear : t -> rgb:int -> unit

(* The 0xRRGGBB color of pixel (x, y) (for tests and debugging) *)
val get_rgb : t -> x:int -> y:int -> int

(* [fill_span fb ~y ~x0 ~x1 ~rgb ~alpha] paints the horizontal run of
 * pixels x0, x0+1, ..., x1-1 of row y ("span" is the classic name for
 * it). Everything that fills an area -- rectangles, polygons, circles --
 * ends up as a series of spans, one per row; the difference between
 * those shapes is only how each row's [x0, x1) is computed. Parts
 * outside the framebuffer are silently skipped (clipped), so callers
 * don't have to care. With [alpha] < 1., the color is blended over what
 * is already there (see [blend]). *)
val fill_span : t -> y:int -> x0:int -> x1:int -> rgb:int -> alpha:float -> unit

(* [blend ~src ~dst ~alpha] is the color you get by painting [src] with
 * opacity [alpha] over an opaque [dst], i.e. Porter & Duff's "src over
 * dst", channel by channel: src * alpha + dst * (1 - alpha). For
 * example, half-transparent red over white:
 *   blend ~src:0xff0000 ~dst:0xffffff ~alpha:0.5 = 0xff8080 (pink)
 *
 * Reference: Thomas Porter, Tom Duff, "Compositing Digital Images",
 * SIGGRAPH '84 (Computer Graphics 18(3):253-259). *)
val blend : src:int -> dst:int -> alpha:float -> int
