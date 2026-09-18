(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* The GPU-API-independent half of a GPU playground3d backend: the
 * shape3d -> per-material vertex list flattening, with no OpenGL/WebGL
 * call at all (the camera matrices are graphics/3d/geometry/Mat4's),
 * shared by playground3d/opengl/ and the planned playground3d/webgl/
 * (see docs/claude_notes/plan_webgl.md).
 *
 * It lives in elm_playground_3d itself, as an ordinary (non-virtual)
 * module next to Playground3d, rather than in its own helper library
 * like Native_loop: it pattern-matches on Playground3d.shape3d, and
 * a separate library depending on the virtual elm_playground_3d would
 * give each backend a second path to it, which dune rejects (see
 * Native_loop.mli). *)

(*****************************************************************************)
(* Vec3: graphics/3d/geometry/Vec3 *)
(*****************************************************************************)

type vec3 = Vec3.t

let rgb_of_color (color : Playground.color) : int * int * int =
  match color with
  | Color.Rgb (r, g, b) -> (r, g, b)
  | Color.Hex s ->
      let s = String.lowercase_ascii s in
      let component i = int_of_string ("0x" ^ String.sub s i 2) in
      (component 1, component 3, component 5)

(*****************************************************************************)
(* Flattening a shape3d tree into per-material vertex lists *)
(*****************************************************************************)
(* Deliberately simpler than the native rasterizer's flatten_faces
 * (playground3d/software/Playground3d_platform.ml): a "material" here is
 * only ever Flat or Textured (no separate texture-sampling closure --
 * that's the backend's fragment shader's job now), and it exists at
 * all only because a GPU draw call can bind at most one texture at a
 * time, so faces have to be grouped by material before drawing
 * (group_by_material below), unlike native where every face picks its
 * own fill closure independently. *)

type material = Flat | Textured of string

(* position, normal, color, uv -- a color is baked directly into every
 * vertex (the vertex attribute a fragment shader reads with zero
 * extra code); uv is (0,0) and unused for a Flat vertex, see the
 * OpenGL backend's fragment shader's uUseTexture. *)
type vertex_data = vec3 * vec3 * Playground.color * (float * float)

(* claude: Newell's method (Vec3.face_normal) -- this copy still had
 * the old first-three-points formula, which gives a (0, 0, 0) normal
 * when two of them coincide (see Vec3.face_normal) *)
let face_normal = Vec3.face_normal

(* fan-triangulate a (convex, e.g. a cube face or a plane) polygon:
 * (p0,p1,p2), (p0,p2,p3), (p0,p3,p4), ... -- same as native's, and
 * genuinely polymorphic (works on plain points, (point, normal) pairs,
 * or (point, uv) pairs alike), so SmoothPolygon3d/TexturedPolygon3d
 * below reuse it directly. *)
let rec fan_triangles = function
  | p0 :: p1 :: p2 :: rest -> (p0, p1, p2) :: fan_triangles (p0 :: p2 :: rest)
  | _ -> []

(* claude: a textured face's own color is irrelevant (the fragment
 * shader ignores vColor when uUseTexture is set) -- white is just a
 * harmless, deterministic filler for the vertex attribute. *)
let white = Playground.rgb 255 255 255

(* every triangle's 3 corners, each already carrying its own
 * (position, normal, color, uv) -- a Polygon3d/TexturedPolygon3d face
 * repeats the same winding-based face_normal at all 3 of a triangle's
 * corners (flat shading across that face, uniform within it but still
 * smoothly lit pixel by pixel against the light, via the fragment
 * shader's own dot product); a SmoothPolygon3d face already has its
 * own distinct normal per point (see Playground3d.sphere), which is
 * what makes Phong lighting actually look smooth/curved across it. *)
let rec collect_batches (shape : Playground3d.shape3d) : (material * vertex_data list) list =
  match shape.form with
  | Polygon3d (color, points) ->
      let normal = face_normal points in
      let verts =
        fan_triangles points
        |> List.concat_map (fun (a, b, c) ->
               [ (a, normal, color, (0., 0.)); (b, normal, color, (0., 0.)); (c, normal, color, (0., 0.)) ])
      in
      [ (Flat, verts) ]
  | TexturedPolygon3d (src, points) ->
      let bare = List.map fst points in
      let normal = face_normal bare in
      let verts =
        fan_triangles points
        |> List.concat_map (fun ((pa, uva), (pb, uvb), (pc, uvc)) ->
               [ (pa, normal, white, uva); (pb, normal, white, uvb); (pc, normal, white, uvc) ])
      in
      [ (Textured src, verts) ]
  | SmoothPolygon3d (color, points) ->
      let verts =
        fan_triangles points
        |> List.concat_map (fun ((pa, na), (pb, nb), (pc, nc)) ->
               [ (pa, na, color, (0., 0.)); (pb, nb, color, (0., 0.)); (pc, nc, color, (0., 0.)) ])
      in
      [ (Flat, verts) ]
  | Hud _ -> [] (* collected separately by Playground3d.collect_hud_shapes, contributes no geometry *)
  | Group3d shapes -> List.concat_map collect_batches shapes

(* claude: merges every shape's batches into at most one Flat group
 * (all non-textured geometry, drawn in a single call) plus one group
 * per distinct texture src (each needs its own texture bound before
 * its draw call). O(batches * distinct materials), fine at this
 * project's scene sizes (a handful to a few dozen faces) -- simplicity
 * over cleverness, per this project's own established preference. *)
let group_by_material (shapes : Playground3d.shape3d list) : (material * vertex_data list) list =
  let batches = List.concat_map collect_batches shapes in
  let flat = batches |> List.filter_map (function (Flat, vs) -> Some vs | _ -> None) |> List.concat in
  let texture_srcs =
    batches |> List.filter_map (function (Textured src, _) -> Some src | _ -> None) |> List.sort_uniq compare
  in
  let textured_groups =
    texture_srcs
    |> List.map (fun src ->
           let vs =
             batches
             |> List.filter_map (function (Textured s, vs) when s = src -> Some vs | _ -> None)
             |> List.concat
           in
           (Textured src, vs))
  in
  (Flat, flat) :: textured_groups

let floats_per_vertex = 11 (* position (3) + normal (3) + color (3) + uv (2) *)

(* claude: rebuilds and re-uploads the WHOLE scene's vertex data every
 * frame -- no per-shape caching of a GPU-side buffer across frames.
 * Stated up front as a known v1 simplification (see plan_opengl.md):
 * matches this project's "measure before optimizing" convention
 * (notes_3d_opti.md) rather than assuming caching is needed before an
 * FPS counter actually says so. *)
let vertex_floats_of_group (vertices : vertex_data list) : float array * int =
  let vertex_count = List.length vertices in
  let data = Array.make (vertex_count * floats_per_vertex) 0. in
  List.iteri
    (fun i ((px, py, pz), (nx, ny, nz), color, (u, v)) ->
      let (r, g, b) = rgb_of_color color in
      let base = i * floats_per_vertex in
      data.(base) <- px;
      data.(base + 1) <- py;
      data.(base + 2) <- pz;
      data.(base + 3) <- nx;
      data.(base + 4) <- ny;
      data.(base + 5) <- nz;
      data.(base + 6) <- float_of_int r /. 255.;
      data.(base + 7) <- float_of_int g /. 255.;
      data.(base + 8) <- float_of_int b /. 255.;
      data.(base + 9) <- u;
      data.(base + 10) <- v)
    vertices;
  (data, vertex_count)

(* claude: the same light_dir as the software rasterizer's, both from
 * graphics/3d/Lighting.ml -- computed in OCaml and uploaded as a
 * uniform, rather than re-typed as a GLSL literal, so there's no risk
 * of a copy-paste/rounding mismatch between the two backends' "same
 * light" claim. *)
let light_dir : vec3 = Lighting.light_dir
