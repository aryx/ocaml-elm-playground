(* Texture mapping: the color of a surface taken from an image, through
 * each vertex's texture coordinates (u, v), interpolated across the
 * triangle (see Interpolate). (u, v) = (0, 0) is the image's top-left
 * corner, (1, 1) its bottom-right one:
 *
 *    image                          a face using all of it
 *    (0,0) +-------+ (1,0)          (0,0) +-------------+ (1,0)
 *          |  :)   |        ->             \    :)       \
 *    (0,1) +-------+ (1,1)            (0,1) +-------------+ (1,1)
 *
 * A pixel's (u, v) generally falls between the image's pixels (the
 * "texels"); which color to take is "filtering", the same question as
 * for 2D images: see graphics/core/Blit.mli (with a picture) and
 * notes_2d.md section 8. No mipmaps yet: a texture seen from far away
 * is sampled at a few of its texels, and shimmers.
 *
 * Example: a 2x1 texture, a black texel and a white one; at u = 0.5,
 * the border between them, nearest gives the white texel (the one
 * containing u) and bilinear the mix half way between the two texel
 * centers, (128, 128, 128).
 *
 * References:
 * - Edwin Catmull, "A Subdivision Algorithm for Computer Display of
 *   Curved Surfaces", PhD thesis, University of Utah, 1974 (the first
 *   texture mapping).
 * - Paul S. Heckbert, "Fundamentals of Texture Mapping and Image
 *   Warping", Master's thesis, UC Berkeley, 1989. *)

(* An image, the same layout as Blit.image: width x height pixels, row
 * by row from the top, 4 bytes per pixel, red, green, blue, alpha (the
 * layout graphics/images/Rgba.ml gives) *)
type image = {
  width : int;
  height : int;
  rgba : (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t;
}

(* nearest-neighbor sampling: the texel containing (u, v), (u, v)
 * clamped to the image *)
val sample_nearest : image -> u:float -> v:float -> int * int * int

(* bilinear filtering, like Blit.sample_bilinear for 2D images: mix the
 * 4 texels whose centers surround (u, v), each weighted by how close
 * it is (see Blit.mli for a picture); at the borders, the missing
 * texels are the nearest border ones. Rounds once, at the end, like
 * Blit.draw's optimized path, where Blit.sample_bilinear rounds after
 * each of its 3 mixes (so the two can differ by 1). *)
val sample_bilinear : image -> u:float -> v:float -> int * int * int
