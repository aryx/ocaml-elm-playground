(* Near-plane clipping: keeping only the part of a triangle that is in
 * front of the camera.
 *
 * The perspective divides by the depth z (see Camera.ndc): a point at
 * z = 0, in the camera's plane, would be at infinity, and one behind it
 * (z < 0) would land on the wrong side of the screen, upside down. So
 * only what's in front of the "near plane", z >= near, can be
 * projected. The simplest renderer drops every triangle with a vertex
 * behind it; but a big triangle that goes behind the camera, like the
 * floor under your feet, is mostly in front of it, and dropping it
 * leaves a hole (see Corridor3d.ml, "c" to toggle). So: cut
 * it along the near plane, and keep the part in front.
 *
 * Seen from above, a triangle crossing the near plane:
 *
 *              A              in front: a quad A, B, B', C'
 *             / \             (then drawn as 2 triangles)
 *            /   \
 *           /     B
 *   -------C'----B'-------    near plane, z = near
 *         /    /
 *        /  /                 behind: cut off
 *       C/
 *
 *             eye
 *
 * Sutherland and Hodgman's algorithm clips a polygon against one plane
 * at a time: walk its edges, from each vertex to the next, and output
 *  - the next vertex, if it's in front;
 *  - the edge's intersection with the plane, if the edge crosses it
 *    (before the next vertex, when entering).
 * A triangle becomes nothing (all behind), a smaller triangle (one
 * vertex in front), or a quad (two in front). The new vertices get
 * their texture coordinates and normal interpolated along the cut
 * edge, as a fraction t = (near - z0) / (z1 - z0) of the way.
 *
 * Example, with near = 1: the triangle A = (0, 0, 3), B = (2, 0, 3),
 * C = (0, 0, -1) (C behind the camera): A to B, both in front, gives
 * B; B to C crosses the plane half way (t = (1 - 3) / (-1 - 3) = 0.5),
 * giving (1, 0, 1); C to A enters it half way too, giving (0, 0, 1),
 * then A. The quad B, (1, 0, 1), (0, 0, 1), A.
 *
 * Only the near plane: triangles crossing the far plane are still
 * dropped whole (far away, they're small), and so are those partly off
 * the sides of the screen, clipped pixel by pixel by the triangle
 * loop's bounding box instead.
 *
 * References:
 * - Ivan E. Sutherland and Gary W. Hodgman, "Reentrant Polygon
 *   Clipping", Communications of the ACM, 1974.
 * - James F. Blinn and Martin E. Newell, "Clipping Using Homogeneous
 *   Coordinates", SIGGRAPH 1978 (all six planes, before the
 *   perspective divide, as GPUs do). *)

(* A vertex in view coordinates (see Camera.view: x right, y up, z the
 * depth), with its texture coordinates (u, v) and its normal *)
type vertex = Vec3.t * (float * float) * Vec3.t

(* [near_plane ~near polygon]: the part of the convex [polygon] with
 * z >= near; the new vertices are exactly at z = near; [polygon] itself
 * when it's all in front *)
val near_plane : near:float -> vertex list -> vertex list
