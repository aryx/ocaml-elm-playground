(* The 3D rendering pipeline: from faces in the world to pixels in a
 * framebuffer, one stage per module of graphics/3d/ (the 3D twin of
 * playground/platforms/software/Shape_render_software.mli):
 *
 *   faces (polygons in world coordinates, each with a color or texture)
 *     |
 *     |  Painter: sort them far to near  (only without a z-buffer)
 *     |  Cull: drop those turned away from the eye  (backface_culling)
 *     v
 *   front faces
 *     |  fan them into triangles: (p0,p1,p2), (p0,p2,p3), ...
 *     |  Clip: cut them to their part in front of the camera  (clipping)
 *     |  Project: each vertex to its pixel (Camera: view, then
 *     |    perspective), with its depth and texture coordinates
 *     |    (dropping the triangles with a vertex still behind the
 *     |    camera)
 *     v
 *   triangles on the screen
 *     |  Triangle.fill: the pixels inside (edge functions), each one
 *     |    - at its depth, uv (Interpolate) and brightness (Shading,
 *     |      Lighting), colored by the face's color or Texture,
 *     |    - drawn if nearer than what's there (Zbuffer), or always
 *     |      (the painter's algorithm)
 *     |  or Triangle.outline: only the edges  (wireframe)
 *     v
 *   pixels
 *
 * Suggested reading order for the modules: Camera, Project, Triangle,
 * Zbuffer, Interpolate, Shading (with Lighting), Texture, Cull, Painter,
 * Clip, then this one. *)

(* What a face is painted with: a 0xRRGGBB color, or a texture (see
 * Texture), both then darkened by the lighting *)
type paint = Color of int | Texture of Texture.image

(* A convex polygon in world coordinates, counterclockwise seen from its
 * front (see Cull), each point with its texture coordinates (u, v)
 * (ignored for a Color) and its normal (see Shading).
 *
 * claude: and its [material], how shiny or glassy it is: carried, not
 * read. The rasterizer has no reflected or refracted rays to use it
 * with; the ray tracer (graphics/3d/raytrace/), which reads the same
 * faces, has (Material.mli). *)
type face = { paint : paint; points : (Vec3.t * (float * float) * Vec3.t) list; material : Material.t }

(* Which faces are visible where: the z-buffer (see Zbuffer), or the
 * painter's algorithm (see Painter) *)
type visibility = Z_buffer | Painters_algorithm

(* Rendering choices that can be changed, to see what each one does
 * (see the keys in the software backend's Playground3d_platform.ml) *)
type options = {
  shading : Shading.mode;
  interpolation : Interpolate.mode;
  visibility : visibility;
  (* true: only the triangles' edges, with graphics/2d/Line *)
  wireframe : bool;
  (* false: draw every face, whichever way it points; in filled mode
   * with a z-buffer, no visual difference at all, only a performance
   * one, see Cull.mli *)
  backface_culling : bool;
  (* textures: true = bilinear filtering, false = nearest texel *)
  bilinear : bool;
  (* true: triangles crossing the near plane are cut to their part in
   * front of the camera (see Clip); false: dropped whole, which leaves
   * holes near the camera *)
  clipping : bool;
  (* which triangle gets the pixels on a shared edge, see Triangle.mli *)
  fill_rule : Triangle.fill_rule;
}

(* Phong, perspective-correct, z-buffer, filled, culling, bilinear,
 * clipping, the epsilon fill rule *)
val default_options : options

(* [render ?options fb zbuffer camera faces] draws [faces] as seen by
 * [camera] into [fb], over what's there (clear it first for a new
 * frame); [zbuffer], [fb]'s size, is cleared first *)
val render : ?options:options -> Framebuffer.t -> Zbuffer.t -> Camera.t -> face list -> unit
