(* Draws a 3D Playground scene into a framebuffer with graphics/3d's
 * software rasterizer (Render): turns the Playground's shape3d tree and
 * camera into Render's faces and Camera.t -- the 3D counterpart of
 * playground/software/Shape_render_software.mli. *)

(* The Playground's camera, as graphics/3d's *)
val camera : Playground3d.camera -> Camera.t

(* Every polygon of a shape3d tree, as Render's faces: colors as
 * 0xRRGGBB, textures loaded (cached by Texture_decode; a texture that
 * fails to load is drawn magenta) *)
val faces : Playground3d.shape3d -> Render.face list

val render :
  ?options:Render.options -> Framebuffer.t -> Zbuffer.t -> Playground3d.camera -> Playground3d.shape3d -> unit
