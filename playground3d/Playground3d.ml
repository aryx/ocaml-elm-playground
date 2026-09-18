(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)
open Basics (* elm-core: float +, -, *, /, degrees_to_radians, etc. *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* First version of a 3D playground on top of elm_playground. See
 * docs/claude_notes/plan_playground3d.md for the overall design
 * rationale. Kept deliberately simple (single flat color per shape, no
 * near-plane clipping, painter's-algorithm depth sort instead of a
 * proper z-buffer at this compile-down-to-2D level -- the native
 * backend's own rasterizer is where a real z-buffer lives) -- this is a
 * first version, meant to be improved incrementally.
 *
 * Design credit: the overall shape3d/form3d split, the world-space
 * (rather than local-transform-header) move3d/rotate3d/scale3d/fade3d,
 * the cube-as-6-explicit-faces construction, the eye/target camera
 * (rather than an exposed matrix), and the "project a 3D scene down to
 * plain 2D Playground shapes" trick used by the web backend, are all
 * taken from Luca Mugnaini's (lucamug's) elm-playground-3d:
 * https://github.com/lucamug/elm-playground-3d -- itself built on top
 * of Evan Czaplicki's elm-playground (the same library playground/
 * ports to OCaml). This version diverges from it in a few deliberate
 * ways, noted inline where relevant: real backface culling, a
 * painter's-algorithm depth sort (lucamug's has neither), and a real
 * from-scratch software rasterizer with a z-buffer for the native
 * backend instead of projecting to 2D everywhere.
 *)

type number = Playground.number

(*****************************************************************************)
(* Vec3 (not exposed, just tuples) *)
(*****************************************************************************)

type vec3 = number * number * number

let sub ((ax, ay, az) : vec3) ((bx, by, bz) : vec3) : vec3 =
  (ax - bx, ay - by, az - bz)

let add ((ax, ay, az) : vec3) ((bx, by, bz) : vec3) : vec3 =
  (ax + bx, ay + by, az + bz)

let dot ((ax, ay, az) : vec3) ((bx, by, bz) : vec3) : number =
  (ax * bx) + (ay * by) + (az * bz)

let cross ((ax, ay, az) : vec3) ((bx, by, bz) : vec3) : vec3 =
  ((ay * bz) - (az * by), (az * bx) - (ax * bz), (ax * by) - (ay * bx))

let scale_vec3 (s : number) ((x, y, z) : vec3) : vec3 = (s * x, s * y, s * z)

let norm (v : vec3) : number = sqrt (dot v v)

let normalize (v : vec3) : vec3 =
  let n = norm v in
  if n = 0. then v else scale_vec3 (1. / n) v

(*****************************************************************************)
(* Shapes *)
(*****************************************************************************)

type shape3d = { alpha : number; form : form3d }
and form3d =
  | Polygon3d of Playground.color * vec3 list
  | TexturedPolygon3d of string * (vec3 * (number * number)) list
  | SmoothPolygon3d of Playground.color * (vec3 * vec3) list
  | Hud of Playground.shape
  | Group3d of shape3d list

let polygon3d color points =
  if List.length points < 3 then failwith "polygon3d needs at least 3 points";
  { alpha = 1.; form = Polygon3d (color, points) }

let group3d shapes = { alpha = 1.; form = Group3d shapes }

let hud (s : Playground.shape) : shape3d = { alpha = 1.; form = Hud s }

let textured_quad src p0 p1 p2 p3 =
  { alpha = 1.; form = TexturedPolygon3d (src, [ (p0, (0., 0.)); (p1, (1., 0.)); (p2, (1., 1.)); (p3, (0., 1.)) ]) }

(*-------------------------------------------------------------------*)
(* Basic 3D shapes *)
(*-------------------------------------------------------------------*)

(* claude: 6 explicit faces, all with CCW winding as seen from outside
 * (so the outward normal, computed as (p1-p0) x (p2-p0), points away
 * from the box) -- this is what makes backface culling in
 * render3d_to_2d work. Shared by box, cube, and textured_cube so they
 * stay in sync.
 *
 * claude: a flat single polygon (e.g. polygon3d) only has a *front*
 * face -- backface culling makes it vanish once it rotates edge-on or
 * past that towards the camera, which looks fine for an actual solid's
 * surface (you were never meant to see its back either) but is a
 * visible bug for anything meant to look like a thin line/marker from
 * any angle (e.g. an axis indicator in a scene the user can freely
 * rotate). A thin box, unlike a flat polygon, always has *some* face
 * pointing towards the camera from any direction, which is why this is
 * exposed as a real primitive rather than leaving box_faces private. *)
let box_faces (width : number) (height : number) (depth : number) : vec3 list list =
  let hx = width / 2. and hy = height / 2. and hz = depth / 2. in
  let p000 = (-.hx, -.hy, -.hz)
  and p001 = (-.hx, -.hy, hz)
  and p010 = (-.hx, hy, -.hz)
  and p011 = (-.hx, hy, hz)
  and p100 = (hx, -.hy, -.hz)
  and p101 = (hx, -.hy, hz)
  and p110 = (hx, hy, -.hz)
  and p111 = (hx, hy, hz) in
  [ [ p100; p110; p111; p101 ] (* +X *)
  ; [ p001; p011; p010; p000 ] (* -X *)
  ; [ p010; p011; p111; p110 ] (* +Y *)
  ; [ p000; p100; p101; p001 ] (* -Y *)
  ; [ p001; p101; p111; p011 ] (* +Z *)
  ; [ p000; p010; p110; p100 ] (* -Z *)
  ]

let box color width height depth = group3d (box_faces width height depth |> List.map (polygon3d color))
let cube color size = box color size size size

let textured_cube src size =
  group3d
    (box_faces size size size
    |> List.map (function
         | [ p0; p1; p2; p3 ] -> textured_quad src p0 p1 p2 p3
         | _ -> assert false (* box_faces always returns 4-point faces *)))

let plane color width depth =
  let w = width / 2. and d = depth / 2. in
  polygon3d color
    [ (-.w, 0., -.d); (-.w, 0., d); (w, 0., d); (w, 0., -.d) ]

(* claude: a UV-sphere, built the same way as e.g. Blender's default
 * sphere: [lat_segments] horizontal rings between the poles,
 * [lon_segments] vertical slices around, each cell a
 * (near-)quadrilateral (except right at the poles, where two of a
 * cell's 4 corners coincide -- a degenerate, zero-area quad; harmless,
 * see box_faces-style fan-triangulation downstream, which just skips a
 * zero-area triangle). Fixed resolution (no parameter), same "no knobs"
 * style as box/cube.
 *
 * Since the sphere is centered on the origin, a point's own outward
 * normal is simply its own (normalized) position -- no need for the
 * general "average the normals of every face touching this vertex"
 * technique real meshes use elsewhere; see SmoothPolygon3d's doc
 * comment and plan_gouraud_phong.md. *)
let sphere color radius =
  let lat_segments = 8 and lon_segments = 12 in
  let point_at lat lon =
    let theta = Float.pi * float_of_int lat / float_of_int lat_segments in
    let phi = 2. * Float.pi * float_of_int lon / float_of_int lon_segments in
    (sin theta * cos phi, cos theta, sin theta * sin phi)
  in
  let faces = ref [] in
  for lat = 0 to lat_segments -.. 1 do
    for lon = 0 to lon_segments -.. 1 do
      let corners =
        [ point_at lat lon; point_at lat (lon +.. 1); point_at (lat +.. 1) (lon +.. 1); point_at (lat +.. 1) lon ]
      in
      let points_and_normals = corners |> List.map (fun p -> (scale_vec3 radius p, p (* already unit length *))) in
      faces := { alpha = 1.; form = SmoothPolygon3d (color, points_and_normals) } :: !faces
    done
  done;
  group3d !faces

(*-------------------------------------------------------------------*)
(* Move/rotate/scale/fade shapes *)
(*-------------------------------------------------------------------*)
(* Unlike Playground.shape (a local-transform header + form), a shape3d
 * has no transform header: move3d/rotate3d/scale3d transform the raw
 * world-space points directly, recursing through Group3d, the same
 * design as lucamug's elm-playground-3d. Simpler to reason about (no
 * matrix stack to expose), at the cost of walking the whole tree on
 * every combinator call -- fine at playground scale.
 *)

(* transforms only the points of a shape, leaving any per-point normal
 * (SmoothPolygon3d) untouched -- correct for move3d/scale3d, since a
 * normal is a direction, not a position: translating or uniformly
 * scaling a shape doesn't change which way its surface faces. *)
let rec map_points (f : vec3 -> vec3) (shape : shape3d) : shape3d =
  match shape.form with
  | Polygon3d (color, points) ->
      { shape with form = Polygon3d (color, List.map f points) }
  | TexturedPolygon3d (src, points) ->
      { shape with form = TexturedPolygon3d (src, List.map (fun (p, uv) -> (f p, uv)) points) }
  | SmoothPolygon3d (color, points) ->
      { shape with form = SmoothPolygon3d (color, List.map (fun (p, n) -> (f p, n)) points) }
  (* claude: a Hud shape is screen-space, not scene-space -- move3d/
   * scale3d (both built on map_points) are deliberately no-ops on it,
   * even nested inside a moved/scaled Group3d, since the whole point
   * of a HUD is that it stays fixed on screen. See Hud's doc comment
   * in Playground3d.mli. *)
  | Hud _ -> shape
  | Group3d shapes -> { shape with form = Group3d (List.map (map_points f) shapes) }

(* like map_points, but also applies [f] to each point's normal (for
 * SmoothPolygon3d) -- correct for rotate3d specifically, since rotating
 * a shape *does* rotate which way its surface faces. Only safe to reuse
 * the same [f] for both because [f] here is always a pure rotation (no
 * translation component -- rotate3d always rotates around the origin),
 * which is exactly the kind of transform that's equally valid to apply
 * to a direction as to a position. *)
let rec map_points_and_normals (f : vec3 -> vec3) (shape : shape3d) : shape3d =
  match shape.form with
  | SmoothPolygon3d (color, points) ->
      { shape with form = SmoothPolygon3d (color, List.map (fun (p, n) -> (f p, f n)) points) }
  | Group3d shapes -> { shape with form = Group3d (List.map (map_points_and_normals f) shapes) }
  | Hud _ -> shape (* same no-op as map_points, see there *)
  | Polygon3d _ | TexturedPolygon3d _ -> map_points f shape

let move3d dx dy dz shape = map_points (fun p -> add p (dx, dy, dz)) shape
let move_x3d dx shape = move3d dx 0. 0. shape
let move_y3d dy shape = move3d 0. dy 0. shape
let move_z3d dz shape = move3d 0. 0. dz shape

(* rotate around the origin, first around X, then Y, then Z (same order
 * as lucamug's geometryRotate) *)
let rotate3d dx dy dz shape =
  let rotate_x a (x, y, z) =
    let a = degrees_to_radians a in
    (x, (y * cos a) - (z * sin a), (y * sin a) + (z * cos a))
  in
  let rotate_y a (x, y, z) =
    let a = degrees_to_radians a in
    ((x * cos a) + (z * sin a), y, (-.x * sin a) + (z * cos a))
  in
  let rotate_z a (x, y, z) =
    let a = degrees_to_radians a in
    ((x * cos a) - (y * sin a), (x * sin a) + (y * cos a), z)
  in
  shape
  |> map_points_and_normals (rotate_x dx)
  |> map_points_and_normals (rotate_y dy)
  |> map_points_and_normals (rotate_z dz)

let scale3d s shape = map_points (scale_vec3 s) shape

let rec fade3d alpha shape =
  match shape.form with
  | Polygon3d _ | TexturedPolygon3d _ | SmoothPolygon3d _ | Hud _ -> { shape with alpha }
  | Group3d shapes -> { shape with form = Group3d (List.map (fade3d alpha) shapes) }

(* shared by both backends -- see Hud's doc comment in Playground3d.mli
 * and docs/claude_notes/done/plan_hud.md. [Playground.fade shape.alpha s]
 * reuses the exact per-leaf alpha fade3d already sets, so a Hud shape
 * fades the same way every other leaf does, with no special-casing
 * needed in fade3d itself above. *)
let rec collect_hud_shapes (shape : shape3d) : Playground.shape list =
  match shape.form with
  | Hud s -> [ Playground.fade shape.alpha s ]
  | Group3d shapes -> List.concat_map collect_hud_shapes shapes
  | Polygon3d _ | TexturedPolygon3d _ | SmoothPolygon3d _ -> []

(*****************************************************************************)
(* Camera *)
(*****************************************************************************)

type camera = { eye : vec3; target : vec3; fov : number; near : number; far : number }

let camera ~eye ~target ?(fov = 60.) ?(near = 0.1) ?(far = 1000.) () =
  { eye; target; fov; near; far }

(* world "up"; a camera looking straight up/down would make forward and
 * up_hint parallel, which is out of scope for this first version *)
let up_hint : vec3 = (0., 1., 0.)

(*****************************************************************************)
(* Project (3D -> 2D pipeline) *)
(*****************************************************************************)

let project (camera : camera) (screen : Playground.screen) (point : vec3) :
    (number * number) option =
  let forward = normalize (sub camera.target camera.eye) in
  let right = normalize (cross forward up_hint) in
  let up = cross right forward in
  let relative = sub point camera.eye in
  let view_x = dot relative right in
  let view_y = dot relative up in
  let view_z = dot relative forward in
  if view_z <= camera.near || view_z >= camera.far then None
  else
    let aspect = screen.width / screen.height in
    let f = 1. / tan (degrees_to_radians camera.fov / 2.) in
    let ndc_x = f * view_x / aspect / view_z in
    let ndc_y = f * view_y / view_z in
    Some (ndc_x * (screen.width / 2.), ndc_y * (screen.height / 2.))

let face_centroid (points : vec3 list) : vec3 =
  let (sx, sy, sz) =
    List.fold_left (fun (ax, ay, az) (x, y, z) -> (ax + x, ay + y, az + z)) (0., 0., 0.) points
  in
  let n = float_of_int (List.length points) in
  (sx / n, sy / n, sz / n)

(* claude: bugfix -- Newell's method instead of the cross product of the
 * first two edges, which is NaN once normalized when two of the first
 * three points coincide, e.g. at a sphere's pole: the top cap of every
 * sphere was culled. See the identical face_normal in
 * playground3d/software/Playground3d_platform.ml for the details. *)
let face_normal (points : vec3 list) : vec3 =
  match points with
  | [] | [ _ ] | [ _; _ ] -> failwith "polygon3d needs at least 3 points"
  | first :: rest ->
      let edges = List.combine points (rest @ [ first ]) in
      normalize
        (List.fold_left
           (fun (nx, ny, nz) ((x0, y0, z0), (x1, y1, z1)) ->
             (nx + ((y0 - y1) * (z0 + z1)), ny + ((z0 - z1) * (x0 + x1)), nz + ((x0 - x1) * (y0 + y1))))
           (0., 0., 0.) edges)

(* claude: the web backend can't warp an image onto an arbitrary
 * projected quad (Playground.image only draws an upright rectangle),
 * so a TexturedPolygon3d renders as this flat placeholder here instead
 * of the real texture -- see textured_quad's doc comment. The native
 * backend samples the real texture per pixel instead, so it does not
 * go through this function at all. *)
let placeholder_texture_color = Playground.gray

(* flatten a shape3d tree down to its leaf faces, dropping Group3d nodes
 * (their own alpha field is never read -- fade3d already pushed alpha
 * down into every leaf, same as lucamug's shape3dto2d) *)
let rec flatten_faces (shape : shape3d) : (Playground.color * vec3 list * number) list =
  match shape.form with
  | Polygon3d (color, points) -> [ (color, points, shape.alpha) ]
  | TexturedPolygon3d (_src, points) ->
      [ (placeholder_texture_color, List.map fst points, shape.alpha) ]
  | SmoothPolygon3d (color, points) ->
      (* claude: the web backend never does any per-pixel shading (see
       * Playground3d_platform for native, which does) -- a
       * SmoothPolygon3d face is just a flat-colored polygon here, same
       * as Polygon3d, so a sphere still renders (faceted, unlit) on
       * web, just without the smooth-shading point of having it. *)
      [ (color, List.map fst points, shape.alpha) ]
  | Hud _ -> [] (* collected separately by collect_hud_shapes, contributes no 3D geometry *)
  | Group3d shapes -> List.concat_map flatten_faces shapes

(* claude: rendering hints, see Playground3d.mli *)
type shading = No_lighting | Flat | Smooth

type rendering = { shading : shading; backface_culling : bool; smooth_textures : bool }

let default_rendering = { shading = Smooth; backface_culling = true; smooth_textures = true }

(* claude: flat shading for render3d_to_2d (the web backend): the same
 * sun and formula as the software backend's brightness_of_normal and
 * the OpenGL backend's fragment shader (Gpu_scene.light_dir), repeated
 * here because Gpu_scene depends on this module: the brightness is
 * how much the face turns towards the light, never below [ambient] *)
let light_dir : vec3 = normalize (1., 1.3, 0.6)

let ambient = 0.25

let brightness_of_normal (normal : vec3) : number =
  ambient + ((1. - ambient) * Float.max 0. (dot normal light_dir))

(* [color] darkened to [brightness] (0. black, 1. unchanged) *)
let shade_color (color : Playground.color) (brightness : number) : Playground.color =
  let (r, g, b) =
    match color with
    | Rgb (r, g, b) -> (r, g, b)
    | Hex s ->
        let channel i = int_of_string ("0x" ^ String.sub s (Stdlib.( + ) 1 (Stdlib.( * ) 2 i)) 2) in
        (channel 0, channel 1, channel 2)
  in
  let scale c = int_of_float (float_of_int c * brightness) in
  Playground.rgb (scale r) (scale g) (scale b)

let render3d_to_2d ?(rendering = default_rendering) (camera : camera) (screen : Playground.screen)
    (shape : shape3d) : Playground.shape =
  let faces =
    flatten_faces shape
    |> List.map (fun ((color, points, alpha) as face) ->
           match rendering.shading with
           | No_lighting -> face
           (* one color per SVG polygon: Smooth can only be Flat here *)
           | Flat | Smooth -> (shade_color color (brightness_of_normal (face_normal points)), points, alpha))
  in
  let visible =
    faces
    |> List.filter (fun (_color, points, _alpha) ->
           let normal = face_normal points in
           let centroid = face_centroid points in
           (not rendering.backface_culling) || dot normal (sub camera.eye centroid) > 0.)
  in
  let dist_to_eye points =
    let (dx, dy, dz) = sub camera.eye (face_centroid points) in
    (dx * dx) + (dy * dy) + (dz * dz)
  in
  (* painter's algorithm: farthest first, so nearer faces get drawn on
   * top -- lucamug's version has no such sort at all *)
  let sorted =
    visible |> List.sort (fun (_, p1, _) (_, p2, _) -> compare (dist_to_eye p2) (dist_to_eye p1))
  in
  let shapes2d =
    sorted
    |> List.filter_map (fun (color, points, alpha) ->
           let projected = points |> List.filter_map (project camera screen) in
           if List.length projected < 3 then None
           else Some (Playground.polygon color projected |> Playground.fade alpha))
  in
  (* claude: appended after the depth-sorted 3D-derived shapes so a Hud
   * shape paints on top of them (later elements in a Playground.group
   * are drawn on top, same convention relied on elsewhere) -- reaches
   * elm_playground_web's existing, unmodified SVG renderer exactly the
   * way any other Playground.shape already does, zero new rendering
   * code needed on this backend. *)
  let hud_shapes = collect_hud_shapes shape in
  Playground.group (shapes2d @ hud_shapes)

(*****************************************************************************)
(* App *)
(*****************************************************************************)

type ('model, 'msg) app3d = {
  init3d_ : unit -> 'model;
  update3d_ : Playground.computer -> 'model -> 'model;
  view3d_ : Playground.computer -> 'model -> camera * shape3d list;
}

let game3d view_memory update_memory initial_memory =
  { init3d_ = (fun () -> initial_memory); update3d_ = update_memory; view3d_ = view_memory }

let init3d app = app.init3d_
let update3d app = app.update3d_
let view3d app = app.view3d_
