(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Modeler.mli *)

type v3 = float * float * float
type shape = Cube | Sphere | Cylinder | Cone | Torus of float | Ground
type look = Plain | Checker | Marble | Wood
type material = { color : int; look : look; mirror : float; glass : bool }
type kind = Mesh of shape * material | Point_light of int | Sun_light of int | Camera
type boolean = Difference | Union | Intersect

type obj = {
  name : string;
  kind : kind;
  location : v3;
  rotation : v3;
  scale : v3;
  hidden : bool;
  modifier : (boolean * string) option;
}

type t = obj list

let grey = { color = 0xcccccc; look = Plain; mirror = 0.; glass = false }
let placed name kind location = { name; kind; location; rotation = (0., 0., 0.); scale = (1., 1., 1.); hidden = false; modifier = None }

let default =
  [
    placed "Cube" (Mesh (Cube, grey)) (0., 0., 0.);
    placed "Light" (Point_light 0xffffff) (4.1, 1., 5.9);
    placed "Camera" Camera (7.4, -6.9, 5.);
  ]

let find t name = List.find_opt (fun o -> o.name = name) t

let fresh_name base t =
  if find t base = None then base
  else
    let rec go i =
      let n = Printf.sprintf "%s.%03d" base i in
      if find t n = None then n else go (i + 1)
    in
    go 1

let add kind base t =
  let name = fresh_name base t in
  (t @ [ placed name kind (0., 0., 0.) ], name)

let update name f t = List.map (fun o -> if o.name = name then f o else o) t

let remove names t =
  List.filter_map
    (fun o ->
      if List.mem o.name names then None
      else Some (match o.modifier with Some (_, c) when List.mem c names -> { o with modifier = None } | _ -> o))
    t

let duplicate names t =
  List.fold_left
    (fun (t, fresh) o ->
      if List.mem o.name names then
        let base = match String.index_opt o.name '.' with Some i -> String.sub o.name 0 i | None -> o.name in
        let name = fresh_name base t in
        (t @ [ { o with name } ], fresh @ [ name ])
      else (t, fresh))
    (t, []) t

let is_cutter t name = List.exists (fun o -> match o.modifier with Some (_, c) -> c = name | None -> false) t

(*****************************************************************************)
(* Transforms *)
(*****************************************************************************)

let rad a = a *. Float.pi /. 180.

let rot_x a (x, y, z) =
  let c = Float.cos (rad a) and s = Float.sin (rad a) in
  (x, (c *. y) -. (s *. z), (s *. y) +. (c *. z))

let rot_y a (x, y, z) =
  let c = Float.cos (rad a) and s = Float.sin (rad a) in
  ((c *. x) +. (s *. z), y, (-.s *. x) +. (c *. z))

let rot_z a (x, y, z) =
  let c = Float.cos (rad a) and s = Float.sin (rad a) in
  ((c *. x) -. (s *. y), (s *. x) +. (c *. y), z)

let transform o (px, py, pz) =
  let sx, sy, sz = o.scale and rx, ry, rz = o.rotation and lx, ly, lz = o.location in
  let x, y, z = rot_z rz (rot_y ry (rot_x rx (sx *. px, sy *. py, sz *. pz))) in
  (lx +. x, ly +. y, lz +. z)

let to_world (x, y, z) = (x, z, -.y)

let translate (dx, dy, dz) o =
  let x, y, z = o.location in
  { o with location = (x +. dx, y +. dy, z +. dz) }

let rotate_point axis a p = match axis with 0 -> rot_x a p | 1 -> rot_y a p | _ -> rot_z a p

let turn axis a o =
  let x, y, z = o.rotation in
  { o with rotation = (match axis with 0 -> (x +. a, y, z) | 1 -> (x, y +. a, z) | _ -> (x, y, z +. a)) }

let resize (kx, ky, kz) o =
  let x, y, z = o.scale in
  { o with scale = (x *. kx, y *. ky, z *. kz) }

(*****************************************************************************)
(* Wireframes *)
(*****************************************************************************)

let add3 (a, b, c) (x, y, z) = (a +. x, b +. y, c +. z)
let sub3 (a, b, c) (x, y, z) = (a -. x, b -. y, c -. z)
let scale3 k (x, y, z) = (k *. x, k *. y, k *. z)
let cross3 (a, b, c) (x, y, z) = ((b *. z) -. (c *. y), (c *. x) -. (a *. z), (a *. y) -. (b *. x))

let norm3 v =
  let x, y, z = v in
  let l = Float.sqrt ((x *. x) +. (y *. y) +. (z *. z)) in
  if l = 0. then v else scale3 (1. /. l) v

(* a closed polyline as its segments *)
let loop ps = List.mapi (fun i p -> (p, List.nth ps ((i + 1) mod List.length ps))) ps

let circle ?(n = 24) f = loop (List.init n (fun i -> f (2. *. Float.pi *. float_of_int i /. float_of_int n)))

(* the primitive's edges, in its own space *)
let edges = function
  | Cube ->
      let c = [ (-1., -1.); (1., -1.); (1., 1.); (-1., 1.) ] in
      let at z = loop (List.map (fun (x, y) -> (x, y, z)) c) in
      at (-1.) @ at 1. @ List.map (fun (x, y) -> ((x, y, -1.), (x, y, 1.))) c
  | Sphere ->
      let ring lat = circle (fun a -> (Float.cos lat *. Float.cos a, Float.cos lat *. Float.sin a, Float.sin lat)) in
      let meridian lon = circle (fun a -> (Float.cos a *. Float.cos lon, Float.cos a *. Float.sin lon, Float.sin a)) in
      List.concat_map ring [ -1.; -0.5; 0.; 0.5; 1. ]
      @ List.concat_map (fun i -> meridian (float_of_int i *. Float.pi /. 4.)) [ 0; 1; 2; 3 ]
  | Cylinder ->
      let at z = circle (fun a -> (Float.cos a, Float.sin a, z)) in
      at (-1.) @ at 1.
      @ List.init 8 (fun i ->
            let a = float_of_int i *. Float.pi /. 4. in
            ((Float.cos a, Float.sin a, -1.), (Float.cos a, Float.sin a, 1.)))
  | Cone ->
      circle (fun a -> (Float.cos a, Float.sin a, -1.))
      @ List.init 8 (fun i ->
            let a = float_of_int i *. Float.pi /. 4. in
            ((Float.cos a, Float.sin a, -1.), (0., 0., 1.)))
  | Torus r ->
      let ring rr z = circle ~n:32 (fun a -> (rr *. Float.cos a, rr *. Float.sin a, z)) in
      let tube i =
        let b = float_of_int i *. Float.pi /. 4. in
        circle ~n:12 (fun a -> ((1. +. (r *. Float.cos a)) *. Float.cos b, (1. +. (r *. Float.cos a)) *. Float.sin b, r *. Float.sin a))
      in
      ring (1. +. r) 0. @ ring (1. -. r) 0. @ ring 1. r @ ring 1. (-.r) @ List.concat_map tube [ 0; 1; 2; 3; 4; 5; 6; 7 ]
  | Ground ->
      List.concat_map
        (fun i ->
          let v = float_of_int i in
          [ ((v, -10., 0.), (v, 10., 0.)); ((-10., v, 0.), (10., v, 0.)) ])
        [ -10; -8; -6; -4; -2; 0; 2; 4; 6; 8; 10 ]

let wires ?(target = (0., 0., 0.)) o =
  let at = o.location in
  match o.kind with
  | Mesh (Ground, _) ->
      (* the ground only rises and falls: it is the plane z = height *)
      let _, _, h = at in
      List.map (fun (a, b) -> (add3 a (0., 0., h), add3 b (0., 0., h))) (edges Ground)
  | Mesh (s, _) -> List.map (fun (a, b) -> (transform o a, transform o b)) (edges s)
  | Point_light _ ->
      let r = 0.25 in
      circle ~n:12 (fun a -> add3 at (r *. Float.cos a, r *. Float.sin a, 0.))
      @ List.map (fun d -> (add3 at (scale3 r d), add3 at (scale3 (2.2 *. r) d)))
          [ (1., 0., 0.); (-1., 0., 0.); (0., 1., 0.); (0., -1., 0.); (0., 0., 1.); (0., 0., -1.) ]
  | Sun_light _ ->
      let d = norm3 (sub3 target at) in
      circle ~n:12 (fun a -> add3 at (0.3 *. Float.cos a, 0.3 *. Float.sin a, 0.)) @ [ (at, add3 at (scale3 2. d)) ]
  | Camera ->
      (* a pyramid from the eye, its base a unit ahead, a triangle above
         it saying which way is up *)
      let f = norm3 (sub3 target at) in
      let r = norm3 (cross3 f (0., 0., 1.)) in
      let u = cross3 r f in
      let corner x y = add3 at (add3 f (add3 (scale3 x r) (scale3 y u))) in
      let c = [ corner (-0.5) (-0.35); corner 0.5 (-0.35); corner 0.5 0.35; corner (-0.5) 0.35 ] in
      loop c @ List.map (fun p -> (at, p)) c @ loop [ corner (-0.3) 0.45; corner 0.3 0.45; corner 0. 0.7 ]
