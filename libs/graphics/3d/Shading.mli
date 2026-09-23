(* How bright each pixel of a triangle is: where, and how often, the
 * lighting formula (Lighting.brightness_of_normal) is applied. See
 * notes_3d_shading.md.
 *
 *  - Flat_color: no lighting at all, brightness is always 1 (a
 *    constant function, ignoring the weights entirely).
 *  - Flat_shading: one brightness value for the *whole triangle*,
 *    from v0's normal (all 3 vertices share the same normal on a flat
 *    face, so it doesn't matter which one is picked).
 *  - Gouraud: brightness computed once per *vertex* (3 calls to
 *    brightness_of_normal, one per vertex's own normal), then those 3
 *    numbers blended per pixel via the same barycentric weights
 *    everything else uses.
 *  - Phong: the vertices' *normals themselves* (not a brightness
 *    number) are blended per pixel first, renormalized (a blend of
 *    unit vectors generally isn't itself unit length), and only then
 *    turned into a brightness -- so, unlike Gouraud, a fresh lighting
 *    calculation happens at every single pixel, not just at the 3
 *    vertices.
 *
 * Gouraud/Phong only look any different from Flat_shading on a shape
 * built from faces with genuinely varying per-vertex normals -- i.e. a
 * curved shape like a sphere; on a cube/box/plane (independent flat
 * faces, no shared/varying vertex normals) all 4 modes render
 * identically except for Flat_color.
 *
 * Example: a triangle whose v0 faces the light (brightness 1) and
 * whose v1 and v2 face away from it (brightness 0.25, the ambient
 * floor), at its center (l0 = l1 = l2 = 1/3). Gouraud blends the 3
 * brightnesses: (1 + 0.25 + 0.25) / 3 = 0.5. Phong blends the normals:
 * n0 + n1 + n2 = light - 2 light, pointing away from the light, so
 * 0.25. Gouraud can't show a highlight or a shadow that falls between
 * the vertices; Phong can, at the cost of lighting every pixel.
 *
 * Gouraud and Phong both interpolate *linearly* here (not
 * perspective-correctly like Interpolate's u/v/z can) -- see
 * plan_gouraud_phong.md's "Simplifications" for why that's an
 * acceptable simplification for now.
 *
 * References:
 * - Henri Gouraud, "Continuous Shading of Curved Surfaces", IEEE
 *   Transactions on Computers, 1971.
 * - Bui Tuong Phong, "Illumination for Computer Generated Pictures",
 *   Communications of the ACM, 1975. *)

type mode = Flat_color | Flat_shading | Gouraud | Phong

(* [make mode v0 v1 v2]: called once per triangle, the brightness at a
 * pixel from its barycentric weights -- the same "decide once per
 * triangle, apply once per pixel" shape as Interpolate.make *)
val make : mode -> Project.vertex -> Project.vertex -> Project.vertex -> l0:float -> l1:float -> l2:float -> float
