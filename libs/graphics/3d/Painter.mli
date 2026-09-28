(* The painter's algorithm: draw the faces from the farthest to the
 * nearest, each simply over what's there, like a painter who paints
 * the background first and the foreground last. No depth per pixel
 * needed (Triangle.fill without a z-buffer). See notes_3d.md section 6
 * for the full history/trade-off.
 *
 * It only works if one order is right for every pixel, which a single
 * number per face (here its center's distance to the eye) can't always
 * give: faces that intersect (or overlap cyclically, A over B over C
 * over A) have no correct order at all. Two faces crossing each other,
 * seen from above, the eye at the bottom:
 *
 *            \  A
 *             \
 *        ------\------ B      A's upper half is behind B, its lower
 *               \             half in front of it: drawing A first
 *                \            hides its front half, drawing B first
 *                             covers B where A is behind it
 *              eye
 *
 * which is the historical reason the z-buffer approach (see Zbuffer)
 * won out. Run PaintersAlgorithmFail3d.ml and toggle "z" to
 * see it fail: its two genuinely intersecting boxes cannot be correctly
 * ordered by any single per-face decision. (Cubes3d's grid of separate,
 * same-size, non-overlapping cubes doesn't stress it enough: the order
 * is right almost everywhere.)
 *
 * Example: faces whose centers are 5, 1 and 3 away from the eye are
 * drawn in the order 5, 3, 1.
 *
 * Reference: M. E. Newell, R. G. Newell, T. L. Sancha, "A Solution to
 * the Hidden Surface Problem", ACM National Conference, 1972 (which
 * also splits the faces that can't be ordered; not done here). *)

(* [sort_far_to_near ~eye points_of faces]: [faces] sorted by the
 * distance from [eye] to their center ([points_of] gives a face's
 * points), the farthest first *)
val sort_far_to_near : eye:Vec3.t -> ('face -> Vec3.t list) -> 'face list -> 'face list
