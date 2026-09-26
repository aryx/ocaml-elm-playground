(* Draws a 3D Playground scene into a framebuffer with graphics/3d's
 * software rasterizer (Render): turns the Playground's shape3d tree and
 * camera into Render's faces and Camera.t -- the 3D counterpart of
 * playground/platforms/software/Shape_render_software.mli. *)

(* The Playground's camera, as graphics/3d's *)
val camera : Playground3d.camera -> Camera.t

(* Every polygon of a shape3d tree, as Render's faces: colors as
 * 0xRRGGBB, textures loaded (cached by Texture_decode; a texture that
 * fails to load is drawn magenta) *)
val faces : Playground3d.shape3d -> Render.face list

val render :
  ?options:Render.options -> Framebuffer.t -> Zbuffer.t -> Playground3d.camera -> Playground3d.shape3d -> unit

(* claude: the other renderer (graphics/3d/raytrace/,
 * plan_raytracing_teaching.md). The same faces, fanned into the ray
 * tracer's triangles, each point with its normal and its face's
 * material; a textured face is grey for now (textures at the hit
 * point are phase 7) *)
val solids : Playground3d.shape3d -> Solid.t list

(* the frame drawn by the ray tracer instead of the rasterizer, on the
 * same white background and in the same light, Lighting's sun and
 * ambient: the "y" key and -raytrace *)
val raytrace : ?options:Raytrace.options -> Framebuffer.t -> Playground3d.camera -> Playground3d.shape3d -> unit
