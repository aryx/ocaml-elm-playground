(* Backface culling: skipping the faces turned away from the camera,
 * before doing any work on them. See notes_3d.md section 5.
 *
 * A closed solid's faces are wound counterclockwise when seen from
 * outside, so their normal (Vec3.face_normal) points out of the solid.
 * A face whose normal points away from the eye is on the far side of
 * the solid, hidden by the near side: drop it.
 *
 *                  eye
 *                   :
 *                 normal
 *                   ^
 *                   |   front face: normal . (eye - center) > 0, kept
 *              +----------+
 *              |          |
 *              |  solid   |
 *              |          |
 *              +----------+
 *                   |   back face: normal . (eye - center) < 0, culled
 *                   v
 *                 normal
 *
 * In filled mode, with a z-buffer, culling makes NO visual difference
 * at all -- only a performance one (roughly half the triangles to
 * rasterize for a closed solid like a cube). This isn't a limitation,
 * it's fundamental to what culling does: the z-buffer independently
 * decides, per pixel, which triangle is nearest, and for a closed
 * solid that decision always agrees with what culling would have
 * picked anyway (a back face can never win the z-test against the
 * front face covering the same pixels) -- so culling only ever saves
 * work there, it can never change the picture. In wireframe mode,
 * which has no per-pixel visibility resolution of any kind, culling
 * off shows the extra edges of each shape's hidden faces -- e.g. on a
 * single cube, the 3 short edges that meet at its far, otherwise
 * entirely hidden corner.
 *
 * Example: the face (-1,-1,1) (1,-1,1) (1,1,1) (-1,1,1) of a cube
 * centered on the origin, counterclockwise seen from +z, so its normal
 * is (0, 0, 1): an eye at (0, 0, 5) sees it; an eye at (0, 0, -5), on
 * the other side of the cube, doesn't.
 *
 * Reference: Ivan E. Sutherland, Robert F. Sproull, Robert A.
 * Schumacker, "A Characterization of Ten Hidden-Surface Algorithms",
 * ACM Computing Surveys, 1974 (back-face elimination, the first and
 * cheapest step). *)

(* [faces_camera ~eye points]: is the face [points] (a counterclockwise
 * polygon, seen from its front) turned towards [eye]? *)
val faces_camera : eye:Vec3.t -> Vec3.t list -> bool
