(* From the 3 vertices of a triangle to the depth and texture
 * coordinates (z, u, v) at one of its pixels, given the pixel's
 * barycentric weights l0, l1, l2 (how much of each vertex the pixel is
 * made of, summing to 1; computed by the triangle loop from its edge
 * functions).
 *
 * The problem: perspective projection computes screen position by
 * dividing by depth (screen_x is proportional to view_x / view_z, see
 * Camera.ndc), which makes screen position a NONLINEAR function of 3D
 * position. z, u, and v, by contrast, are each defined to vary
 * LINEARLY across the 3D triangle. So interpolating z/u/v linearly
 * using screen-space barycentric weights (the obvious thing to try) is
 * only an approximation: exact at the 3 corners, increasingly wrong
 * towards the interior, and more wrong the more a triangle's depth
 * varies across itself (i.e. the more obliquely/close-up it's viewed).
 * This is visible in practice as a texture's own detail appearing to
 * swim/warp as a shape rotates -- the classic "affine texture mapping"
 * artifact, notorious from the original PlayStation's 3D rendering
 * (which used exactly this shortcut for speed).
 *
 * The fix: unlike z/u/v themselves, 1/z, u/z, and v/z genuinely ARE
 * linear in screen space, so linearly interpolating THEM is exact, not
 * approximate; dividing back out afterwards (the "perspective divide")
 * recovers the true z/u/v at that pixel.
 *
 * Example: an edge from a vertex at depth z = 1 with u = 0 to one at
 * z = 3 with u = 1, and the pixel half way between them on the screen
 * (l0 = l1 = 0.5). Linear says u = 0.5. But the far half of the edge
 * looks smaller on screen, so the pixel really shows a point nearer
 * the first vertex: 1/z = 0.5 * 1 + 0.5 * 1/3 = 2/3, u/z = 0.5 * 0 +
 * 0.5 * 1/3 = 1/6, so z = 1.5 and u = (1/6) / (2/3) = 0.25:
 *
 *     screen:   v0 +-----------*-----------+ v1
 *                              half way
 *     3D:       v0 +--------*---------------------------+ v1
 *                  z=1      z=1.5                       z=3
 *                  u=0      u=0.25 (not 0.5)            u=1
 *
 * References:
 * - Paul S. Heckbert and Henry P. Moreton, "Interpolation for Polygon
 *   Texture Mapping and Shading", in State of the Art in Computer
 *   Graphics: Visualization and Modeling, Springer, 1991.
 * - Jim Blinn, "Hyperbolic Interpolation", IEEE Computer Graphics and
 *   Applications, 1992. *)

(* "Linear" is the naive, WRONG (but simpler-looking, if you don't know
 * why it's wrong) interpolation: blend z/u/v directly, the same way
 * vx/vy are blended. "Perspective_correct" is the fix -- blend
 * inv_z/u_over_z/v_over_z instead, then divide back out. *)
type mode = Perspective_correct | Linear

(* [make mode v0 v1 v2]: called once per triangle (not once per pixel
 * -- "decide once, apply per pixel"), a little function that does
 * whichever of the two [mode] says; from the pixel loop's point of
 * view it's just "call it to turn barycentric weights into a
 * (z, u, v)", with no visible difference between the two modes at that
 * call site. *)
val make :
  mode -> Project.vertex -> Project.vertex -> Project.vertex -> l0:float -> l1:float -> l2:float -> float * float * float
