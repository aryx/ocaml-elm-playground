(* A magnifying glass for looking at individual pixels: the debugging
 * tool for everything else in raster/. At normal size you can't tell
 * which pixels a shape's edge really covers, or whether two shapes
 * leave a 1-pixel gap between them; magnified 8 times, with a grid
 * between pixels, you can.
 *
 * [draw fb ~cx ~cy] copies the size x size square of pixels centered on
 * pixel (cx, cy) (e.g. under the mouse) into a size*zoom x size*zoom
 * inset in the top-right corner of [fb], each pixel becoming a
 * zoom x zoom block (the simplest image enlargement there is, "nearest
 * neighbor"), with grid lines between blocks. The magnified area is
 * outlined on screen too, and pixel (cx, cy) in the inset.
 *
 *   +----------------------------------+
 *   |                     +----------+ |
 *   |                     | inset:   | |
 *   |    +--+             | 8x zoom  | |
 *   |    |  | <- area     | of area  | |
 *   |    +--+             +----------+ |
 *   +----------------------------------+
 *)
val draw : ?size:int -> ?zoom:int -> Framebuffer.t -> cx:int -> cy:int -> unit
