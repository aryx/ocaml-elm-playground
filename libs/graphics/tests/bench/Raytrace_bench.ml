(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* The ray tracer's speed, brute force against the BVH (Bvh.mli), on
 * two scenes (see dune for how to run it):
 *
 * - "cubes": examples/Cubes3d's 25 cubes (300 triangles), on a floor,
 *   in the sun, with shadow rays: small enough for brute force;
 * - "spheres": 100 spheres of 1,024 triangles each, 102,400 triangles
 *   (what a ray tracer fed meshes meets): the BVH only, brute force
 *   would take hours.
 *
 * For each: the time to build the tree, to trace the picture, camera
 * rays a second, and per camera ray the solids tested and the boxes
 * entered (shadow rays included). CPU time (Sys.time), so that it
 * means the same under node. *)

let matte (color : int) : Solid.surface = { color; pattern = Plain; material = Material.matte }

(* a cube as 12 triangles, as Playground3d.cube's 6 faces fanned *)
let cube (color : int) ((cx, cy, cz) : Vec3.t) (s : float) : Solid.t list =
  let h = s /. 2. in
  let p x y z = (cx +. (x *. h), cy +. (y *. h), cz +. (z *. h)) in
  let quad n a b c d =
    [ Solid.Triangle { points = (a, b, c); normals = (n, n, n); surface = matte color };
      Solid.Triangle { points = (a, c, d); normals = (n, n, n); surface = matte color } ]
  in
  List.concat
    [ quad (0., 0., 1.) (p (-1.) (-1.) 1.) (p 1. (-1.) 1.) (p 1. 1. 1.) (p (-1.) 1. 1.);
      quad (0., 0., -1.) (p 1. (-1.) (-1.)) (p (-1.) (-1.) (-1.)) (p (-1.) 1. (-1.)) (p 1. 1. (-1.));
      quad (1., 0., 0.) (p 1. (-1.) 1.) (p 1. (-1.) (-1.)) (p 1. 1. (-1.)) (p 1. 1. 1.);
      quad (-1., 0., 0.) (p (-1.) (-1.) (-1.)) (p (-1.) (-1.) 1.) (p (-1.) 1. 1.) (p (-1.) 1. (-1.));
      quad (0., 1., 0.) (p (-1.) 1. 1.) (p 1. 1. 1.) (p 1. 1. (-1.)) (p (-1.) 1. (-1.));
      quad (0., -1., 0.) (p (-1.) (-1.) (-1.)) (p 1. (-1.) (-1.)) (p 1. (-1.) 1.) (p (-1.) (-1.) 1.) ]

(* a sphere as a latitude-longitude mesh of 2 x rings x segments
 * triangles, its normals the sphere's *)
let mesh_sphere (color : int) (c : Vec3.t) (r : float) ~(rings : int) ~(segments : int) : Solid.t list =
  let at i j =
    let theta = Float.pi *. float_of_int i /. float_of_int rings in
    let phi = 2. *. Float.pi *. float_of_int j /. float_of_int segments in
    let n = (sin theta *. cos phi, cos theta, sin theta *. sin phi) in
    (Vec3.add c (Vec3.scale r n), n)
  in
  (* a loop onto a list, not List.init and List.concat, whose recursion
   * overflows node's stack at 100,000 triangles *)
  let acc = ref [] in
  for i = rings - 1 downto 0 do
    for j = segments - 1 downto 0 do
      let (a, na) = at i j and (b, nb) = at (i + 1) j and (cc, nc) = at (i + 1) (j + 1) and (d, nd) = at i (j + 1) in
      acc :=
        Solid.Triangle { points = (a, b, cc); normals = (na, nb, nc); surface = matte color }
        :: Solid.Triangle { points = (a, cc, d); normals = (na, nc, nd); surface = matte color }
        :: !acc
    done
  done;
  !acc

let sun : Raytrace.light = Sun { towards = Vec3.normalize (1., 1.3, 0.6); color = (0.75, 0.75, 0.75) }
let floor = Solid.Plane ((0., 1., 0.), -1., matte 0xC0C0C0)

let cubes : Raytrace.scene =
  { camera = { eye = (0., 6., 10.); target = (0., 0., 0.); up = (0., 1., 0.); fov = 60.; ortho = 0.; near = 0.1; far = 100. };
    solids =
      floor
      :: List.concat
           (List.init 25 (fun i -> cube (0x3060C0 + (i * 0x050301)) (float_of_int (i mod 5 - 2) *. 2., 0., float_of_int (i / 5 - 2) *. 2.) 1.));
    lights = [ sun ]; ambient = 0.25; background = 0xFFFFFF }

let spheres : Raytrace.scene =
  { cubes with
    solids =
      floor
      :: List.rev
           (List.fold_left
              (fun acc i ->
                List.rev_append
                  (mesh_sphere (0xC03030 + (i * 0x000203))
                     (float_of_int (i mod 10 - 5) *. 1.2, 0., float_of_int (i / 10 - 5) *. 1.2)
                     0.5 ~rings:16 ~segments:32)
                  acc)
              [] (List.init 100 Fun.id)) }

let run (name : string) (scene : Raytrace.scene) ~(width : int) ~(height : int) (acceleration : Raytrace.acceleration) =
  let t0 = Sys.time () in
  let w = Raytrace.world ~options:{ Raytrace.default_options with acceleration } scene in
  let t1 = Sys.time () in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      let ray, min_t, max_t = Raytrace.camera_ray scene.camera ~width ~height ~x ~y in
      ignore (Raytrace.trace w ray ~min_t ~max_t)
    done
  done;
  let t2 = Sys.time () in
  let rays = float_of_int (width * height) in
  Printf.printf "%-8s %-12s %6d solids  build %6.2f s  trace %7.2f s  %9.0f rays/s  %8.1f tests/ray  %6.1f boxes/ray\n%!" name
    (match acceleration with Brute_force -> "brute force" | Bvh Median -> "BVH median" | Bvh Sah -> "BVH SAH")
    (List.length scene.solids) (t1 -. t0) (t2 -. t1) (rays /. (t2 -. t1))
    (float_of_int (Raytrace.tests w) /. rays) (float_of_int (Raytrace.boxes w) /. rays)

let () =
  Printf.printf "%d x %d, %s\n%!" 400 300 (if Sys.backend_type = Other "js_of_ocaml" then "JavaScript (node)" else "native");
  List.iter (run "cubes" cubes ~width:400 ~height:300) [ Brute_force; Bvh Median; Bvh Sah ];
  List.iter (run "spheres" spheres ~width:400 ~height:300) [ Bvh Median; Bvh Sah ]
