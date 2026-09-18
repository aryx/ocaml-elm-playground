(* Drawing an image (a sprite, a photo) moved, scaled, and rotated: an
 * affine transform of its pixels onto the framebuffer's pixels.
 *
 * "Blit" comes from BitBLT, "bit block transfer", the operation at the
 * heart of the Xerox Alto and Smalltalk graphics (Dan Ingalls, 1970s):
 * copying a rectangle of pixels onto another. Here it's generalized to
 * any affine transform, what the literature calls image warping.
 *
 * Forward or inverse? The obvious way, going through the image's
 * pixels and computing where each lands on the screen ("forward
 * mapping"), leaves holes as soon as the image is enlarged or rotated:
 * 2 image pixels side by side land 2 screen pixels apart, and nobody
 * paints the one in between.
 *
 *    image        forward: holes       inverse: every screen pixel
 *    +--+--+      +--+--+--+--+        +--+--+--+--+
 *    |A |B |  x2  |A |  |B |  |        |A |A |B |B |
 *    +--+--+ ---> +--+--+--+--+        +--+--+--+--+
 *                 |  |  |  |  |        |A |A |B |B |
 *                 +--+--+--+--+        +--+--+--+--+
 *
 * So we go the other way ("inverse mapping"): through the *screen's*
 * pixels, those the transformed image may cover, and for each, find
 * where its center comes from in the image, with the inverse transform
 * (Affine.invert). Every screen pixel gets exactly one color.
 *
 * That point in the image generally falls between image pixels; which
 * color to take is "filtering" (see [sample_nearest], [sample_bilinear]).
 *
 * References:
 * - Paul S. Heckbert, "Fundamentals of Texture Mapping and Image
 *   Warping", Master's thesis, UC Berkeley, 1989 (forward vs inverse
 *   mapping, filtering).
 * - Edwin Catmull, "A Subdivision Algorithm for Computer Display of
 *   Curved Surfaces", PhD thesis, University of Utah, 1974 (the first
 *   texture mapping: an image drawn onto surfaces).
 * - Adele Goldberg, David Robson, "Smalltalk-80: The Language and its
 *   Implementation", Addison-Wesley, 1983 (BitBlt).
 *)

(* An image: width x height pixels, row by row from the top, 4 bytes per
 * pixel, red, green, blue, alpha (0 = transparent, 255 = opaque, not
 * premultiplied). The layout stb_image decodes to, so images loaded by
 * graphics/images/Image_decode.ml can be used as they are. *)
type image = {
  width : int;
  height : int;
  rgba : (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t;
}

(* Colors with their alpha, from 0. to 1. *)
type color = { rgb : int; a : float }

(* [sample_nearest image (u, v)]: the color of the image pixel
 * containing the point (u, v), in image pixel coordinates (pixel (i, j)
 * is the square from (i, j) to (i+1, j+1)). The simplest filter, and
 * the blockiest: enlarged, each image pixel becomes a visible square,
 * the look of pixel art. *)
val sample_nearest : image -> float * float -> color

(* [sample_bilinear image (u, v)]: a mix of the 4 image pixels whose
 * centers surround (u, v), each weighted by how close it is, first
 * along x, then along y (hence "bi"-linear). E.g. at the point marked
 * *, 30% of the way from the centers of A and C to those of B and D,
 * and 50% of the way from A and B to C and D:
 *
 *    A-----*--------B        top:    70% A + 30% B
 *    |     |        |
 *    |     *        |        result: 50% top + 50% bottom
 *    |     |        |
 *    C-----*--------D        bottom: 70% C + 30% D
 *
 * Enlarged, the image looks smooth (blurry for pixel art) instead of
 * blocky. At the image's borders the missing neighbors are replaced by
 * the nearest border pixels. *)
val sample_bilinear : image -> float * float -> color

(* Which of the two samplers above to use *)
type filter = Nearest | Bilinear

(* [draw fb image m ~filter ~alpha]: draw [image], [m] mapping image
 * pixel coordinates to framebuffer pixel coordinates, taking colors
 * with [filter], and fading the result by [alpha]. A framebuffer pixel
 * is covered if its center comes from inside the image.
 *
 * Two implementations, switched by Opti.enabled: the simple one (a
 * matrix product per pixel, then [sample_nearest] or
 * [sample_bilinear]), and an optimized one computing the same pixels
 * (forward differencing, samplers inlined; see Blit.ml and
 * docs/claude_notes/notes_opti.md). *)
val draw : Framebuffer.t -> image -> Affine.t -> filter:filter -> alpha:float -> unit
