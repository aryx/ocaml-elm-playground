(* From a 3D point to a vertex the rasterizer can draw: where it lands
 * on the screen, in pixels, and what it needs to interpolate across a
 * triangle (its depth, its texture coordinates, its normal).
 *
 * Three steps: Camera.view (the point in the camera's coordinates),
 * Camera.ndc (the perspective divide, to -1..1 on each axis), and the
 * "viewport" mapping from -1..1 to pixels, y flipped since the screen's
 * rows go down:
 *
 *     ndc                                 pixels
 *     (-1, 1) +--------+ (1, 1)           (0, 0) +--------+ (width, 0)
 *             |   y    |                         |        |
 *             |   ^    |          ->             |  (w/2, |
 *             |   +->x |                         |   h/2) |
 *    (-1, -1) +--------+ (1, -1)     (0, height) +--------+
 *
 * Example: a camera at (0, 0, 10) looking at the origin with a 90
 * degree field of view (see Camera.mli's example), on a 400x400
 * screen: the point (0, 5, 0) is at ndc (0, 0.5), so at pixel
 * (200, 100), half way up from the center.
 *
 * Reference: Lawrence G. Roberts, "Machine Perception of
 * Three-Dimensional Solids", MIT PhD thesis, 1963. *)

(* A vertex ready for the rasterizer, carrying EVERY version of its
 * depth/texture-coordinate data that either interpolation strategy
 * (see Interpolate) needs, computed once here so the triangle loop
 * never has to recompute anything, just pick which fields to read:
 *
 *   - vx, vy: where this vertex lands on screen, in pixels. Always
 *     interpolated the ordinary (linear) way -- there's nothing to
 *     debate here, this is just "where is it".
 *   - z: the vertex's plain view-space depth (how far in front of the
 *     camera it is). u, v: the vertex's plain texture coordinates.
 *     These are what you'd naively interpolate across a triangle if
 *     you'd never heard of the problem explained in Interpolate.mli --
 *     see its "Linear" mode.
 *   - inv_z (= 1/z), u_over_z (= u/z), v_over_z (= v/z): the SAME
 *     depth/texture information, but pre-divided by z. These are what
 *     you interpolate instead if you *have* heard of the problem -- see
 *     its "Perspective_correct" mode. *)
type vertex = {
  vx : float;
  vy : float;
  z : float;
  u : float;
  v : float;
  inv_z : float;
  u_over_z : float;
  v_over_z : float;
  normal : Vec3.t;
      (** the vertex's own normal, in world space, untouched by
          projection (a normal is a direction, not a screen position --
          nothing about "where on screen is this" applies to it). Used
          by Shading for Gouraud (blended per vertex) and Phong (blended
          per pixel) shading. *)
}

(* [vertex camera ~width ~height (point, (u, v), normal)]: [point] on a
 * [width] x [height] screen; None when it's not between the camera's
 * near and far planes (the triangles using it are then dropped, unless
 * clipped first, see Clip) *)
val vertex : Camera.t -> width:int -> height:int -> Vec3.t * (float * float) * Vec3.t -> vertex option

(* The same, for a point already in view coordinates (Camera.view),
 * e.g. one Clip created *)
val vertex_of_view : Camera.t -> width:int -> height:int -> Vec3.t * (float * float) * Vec3.t -> vertex option
