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
 * docs/claude_notes/playground3d_plan.md for the overall design
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
  | Group3d of shape3d list

let polygon3d color points =
  if List.length points < 3 then failwith "polygon3d needs at least 3 points";
  { alpha = 1.; form = Polygon3d (color, points) }

let group3d shapes = { alpha = 1.; form = Group3d shapes }

let textured_quad src p0 p1 p2 p3 =
  { alpha = 1.; form = TexturedPolygon3d (src, [ (p0, (0., 0.)); (p1, (1., 0.)); (p2, (1., 1.)); (p3, (0., 1.)) ]) }

(*-------------------------------------------------------------------*)
(* Basic 3D shapes *)
(*-------------------------------------------------------------------*)

(* claude: 6 explicit faces, all with CCW winding as seen from outside
 * (so the outward normal, computed as (p1-p0) x (p2-p0), points away
 * from the cube) -- this is what makes backface culling in
 * render3d_to_2d work. Shared by cube and textured_cube so the two
 * stay in sync. *)
let cube_faces (size : number) : vec3 list list =
  let h = size / 2. in
  let p000 = (-.h, -.h, -.h)
  and p001 = (-.h, -.h, h)
  and p010 = (-.h, h, -.h)
  and p011 = (-.h, h, h)
  and p100 = (h, -.h, -.h)
  and p101 = (h, -.h, h)
  and p110 = (h, h, -.h)
  and p111 = (h, h, h) in
  [ [ p100; p110; p111; p101 ] (* +X *)
  ; [ p001; p011; p010; p000 ] (* -X *)
  ; [ p010; p011; p111; p110 ] (* +Y *)
  ; [ p000; p100; p101; p001 ] (* -Y *)
  ; [ p001; p101; p111; p011 ] (* +Z *)
  ; [ p000; p010; p110; p100 ] (* -Z *)
  ]

let cube color size = group3d (cube_faces size |> List.map (polygon3d color))

let textured_cube src size =
  group3d
    (cube_faces size
    |> List.map (function
         | [ p0; p1; p2; p3 ] -> textured_quad src p0 p1 p2 p3
         | _ -> assert false (* cube_faces always returns 4-point faces *)))

let plane color width depth =
  let w = width / 2. and d = depth / 2. in
  polygon3d color
    [ (-.w, 0., -.d); (-.w, 0., d); (w, 0., d); (w, 0., -.d) ]

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

let rec map_points (f : vec3 -> vec3) (shape : shape3d) : shape3d =
  match shape.form with
  | Polygon3d (color, points) ->
      { shape with form = Polygon3d (color, List.map f points) }
  | TexturedPolygon3d (src, points) ->
      { shape with form = TexturedPolygon3d (src, List.map (fun (p, uv) -> (f p, uv)) points) }
  | Group3d shapes -> { shape with form = Group3d (List.map (map_points f) shapes) }

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
  shape |> map_points (rotate_x dx) |> map_points (rotate_y dy) |> map_points (rotate_z dz)

let scale3d s shape = map_points (scale_vec3 s) shape

let rec fade3d alpha shape =
  match shape.form with
  | Polygon3d _ | TexturedPolygon3d _ -> { shape with alpha }
  | Group3d shapes -> { shape with form = Group3d (List.map (fade3d alpha) shapes) }

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

let face_normal (points : vec3 list) : vec3 =
  match points with
  | p0 :: p1 :: p2 :: _ -> normalize (cross (sub p1 p0) (sub p2 p0))
  | _ -> failwith "polygon3d needs at least 3 points"

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
  | Group3d shapes -> List.concat_map flatten_faces shapes

let render3d_to_2d (camera : camera) (screen : Playground.screen) (shape : shape3d) :
    Playground.shape =
  let faces = flatten_faces shape in
  let visible =
    faces
    |> List.filter (fun (_color, points, _alpha) ->
           let normal = face_normal points in
           let centroid = face_centroid points in
           dot normal (sub camera.eye centroid) > 0.)
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
  Playground.group shapes2d

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
