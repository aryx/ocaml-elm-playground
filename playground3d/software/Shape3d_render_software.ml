(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Shape3d_render_software.mli *)

let camera (camera : Playground3d.camera) : Camera.t =
  { eye = camera.eye; target = camera.target; fov = camera.fov; near = camera.near; far = camera.far }

(*****************************************************************************)
(* Colors and textures *)
(*****************************************************************************)

let rgb_of_color (color : Playground.color) : int =
  match color with
  | Color.Rgb (r, g, b) -> (r lsl 16) lor (g lsl 8) lor b
  | Color.Hex s ->
      let s = String.lowercase_ascii s in
      let component i = int_of_string ("0x" ^ String.sub s i 2) in
      (component 1 lsl 16) lor (component 3 lsl 8) lor component 5

(* a bright, unmistakable "this texture failed to load" color -- the
 * same convention (a magenta/checkerboard placeholder) many game
 * engines use, rather than silently falling back to something that
 * could be mistaken for an intentional color *)
let missing_texture_color = 0xFF00FF

(* Real per-pixel texture sampling (see graphics/3d/Texture) -- this is
 * the one thing the web backend (see
 * Playground3d.placeholder_texture_color) can't do, since it has no
 * per-pixel access to anything. Loading (a local file path or an
 * http(s) URL, with caching and a preload queue) lives in
 * graphics/images/Texture_decode.ml, the same split as
 * playground/native's Playground_platform.ml and
 * graphics/images/Image_decode.ml.
 *
 * claude: Texture_decode gives RGBA textures, whatever the file's
 * channels (Rgba.of_stb_image; not Stb_image.load ~channels:4, which
 * the pinned binding gets wrong, see Rgba.mli), with no offset and no
 * padding between rows: exactly Texture.image's layout *)
let paint_of_texture (src : string) : Render.paint =
  match Texture_decode.load src with
  | Some img ->
      assert (img.channels = 4 && img.offset = 0 && img.stride = img.width * 4);
      Texture { width = img.width; height = img.height; rgba = img.data }
  | None -> Color missing_texture_color

(*****************************************************************************)
(* Faces *)
(*****************************************************************************)

(* every leaf face's points, tagged with (uv, normal) -- a flat
 * Polygon3d/TexturedPolygon3d face repeats the SAME winding-based
 * normal at every one of its points (which is exactly what makes flat
 * shading uniform across a face: see graphics/3d/Shading.ml), while a
 * SmoothPolygon3d face already has its own distinct normal per point.
 * The winding-based normal is Newell's method (Vec3.face_normal),
 * robust to repeated points like a sphere's pole; see there for the
 * bug the obvious formula caused (spheres drawn "cut" at the top). *)
let rec faces (shape : Playground3d.shape3d) : Render.face list =
  match shape.form with
  | Polygon3d (color, points) ->
      let normal = Vec3.face_normal points in
      [ { paint = Color (rgb_of_color color); points = List.map (fun p -> (p, (0., 0.), normal)) points } ]
  | TexturedPolygon3d (src, points) ->
      let normal = Vec3.face_normal (List.map fst points) in
      [ { paint = paint_of_texture src; points = List.map (fun (p, uv) -> (p, uv, normal)) points } ]
  | SmoothPolygon3d (color, points) ->
      [ { paint = Color (rgb_of_color color); points = List.map (fun (p, n) -> (p, (0., 0.), n)) points } ]
  | Hud _ -> [] (* collected separately by Playground3d.collect_hud_shapes, contributes no geometry *)
  | Group3d shapes -> List.concat_map faces shapes

let render ?options (fb : Framebuffer.t) (zbuffer : Zbuffer.t) (cam : Playground3d.camera)
    (shape : Playground3d.shape3d) : unit =
  Render.render ?options fb zbuffer (camera cam) (faces shape)
