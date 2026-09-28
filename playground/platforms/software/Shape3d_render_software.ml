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
  { eye = camera.eye; target = camera.target; up = camera.up; fov = camera.fov; ortho = camera.ortho; near = camera.near; far = camera.far }

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
 * playground/platforms/native's Playground_platform.ml and
 * graphics/images/Image_decode.ml.
 *
 * claude: Texture_decode gives Rgba_image.t textures, whatever the
 * file's channels: exactly Texture.image's layout *)
let paint_of_texture (src : string) : Render.paint =
  match
    match Playground3d.embedded src with
    | Some base64 -> Texture_decode.load_base64 ~key:src ~base64
    | None -> Texture_decode.load src
  with
  | Some img ->
      Texture { width = img.width; height = img.height; rgba = img.rgba }
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
  (* claude: shiny and glassy, which only the ray tracer reads *)
  let material : Material.t = { shiny = shape.material.shiny; glassy = shape.material.glassy } in
  match shape.form with
  | Polygon3d (color, points) ->
      let normal = Vec3.face_normal points in
      [ { paint = Color (rgb_of_color color); points = List.map (fun p -> (p, (0., 0.), normal)) points; material } ]
  | TexturedPolygon3d (src, points) ->
      let normal = Vec3.face_normal (List.map fst points) in
      [ { paint = paint_of_texture src; points = List.map (fun (p, uv) -> (p, uv, normal)) points; material } ]
  | SmoothPolygon3d (color, points) ->
      [ { paint = Color (rgb_of_color color); points = List.map (fun (p, n) -> (p, (0., 0.), n)) points; material } ]
  | Hud _ -> [] (* collected separately by Playground3d.collect_hud_shapes, contributes no geometry *)
  | Group3d shapes -> List.concat_map faces shapes
  (* claude: a group like any other here: the rasterizer draws every face
   * every frame anyway (see Playground3d.cached3d) *)
  | Cached3d c -> faces c.content

let render ?options (fb : Framebuffer.t) (zbuffer : Zbuffer.t) (cam : Playground3d.camera)
    (shape : Playground3d.shape3d) : unit =
  Render.render ?options fb zbuffer (camera cam) (faces shape)

(*****************************************************************************)
(* The ray tracer *)
(*****************************************************************************)

let solids ?(bilinear = true) (shape : Playground3d.shape3d) : Solid.t list =
  faces shape
  |> List.concat_map (fun (face : Render.face) ->
         let surface : Solid.surface =
           match face.paint with
           | Color c -> { color = c; pattern = Plain; material = face.material }
           | Texture img ->
               (* claude: sampled at the hit's (u, v) by the rasterizer's
                * own sampling, so that the two draw the same texels *)
               let sample = if bilinear then Texture.sample_bilinear else Texture.sample_nearest in
               let f ~u ~v =
                 let r, g, b = sample img ~u ~v in
                 (r lsl 16) lor (g lsl 8) lor b
               in
               { color = 0; pattern = Uv_function f; material = face.material }
         in
         (* fanned as the rasterizer does: (p0,p1,p2), (p0,p2,p3), ... *)
         match face.points with
         | [] -> []
         | (p0, uv0, n0) :: rest ->
             let rec fan = function
               | (p1, uv1, n1) :: ((p2, uv2, n2) :: _ as rest) ->
                   Solid.Triangle { points = (p0, p1, p2); normals = (n0, n1, n2); uvs = (uv0, uv1, uv2); surface }
                   :: fan rest
               | _ -> []
             in
             fan rest)

(* claude: the rasterizer's light as the ray tracer's: Lighting's
 * brightness, ambient + (1 - ambient) max (0, n . light_dir), is one sun
 * of strength 1 - ambient *)
let sun : Raytrace.light =
  let s = 1. -. Lighting.ambient in
  Sun { towards = Lighting.light_dir; color = (s, s, s) }

let raytrace ?options ?bilinear ?(from_x = 0) (fb : Framebuffer.t) (cam : Playground3d.camera)
    (shape : Playground3d.shape3d) : unit =
  let scene : Raytrace.scene =
    { camera = camera cam; solids = solids ?bilinear shape; lights = [ sun ]; ambient = Lighting.ambient; background = 0xFFFFFF }
  in
  let world = Raytrace.world ?options scene in
  for y = 0 to fb.height - 1 do
    for x = from_x to fb.width - 1 do
      Framebuffer.plot fb ~x ~y ~rgb:(Raytrace.pixel world ~width:fb.width ~height:fb.height ~x ~y) ~alpha:1.
    done
  done
