(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* graphics/3d/raytrace/: Solid's hits, the camera rays, and the A/B
 * test the ray tracer is for -- the same faces drawn by it and by the
 * rasterizer (graphics/3d/Render), the same picture. *)

let t = Testo.create
let near msg expected actual = Alcotest.(check (float 1e-9)) msg expected actual
let matte (color : int) : Solid.surface = { color; pattern = Plain; material = Material.matte }

(*****************************************************************************)
(* Solid *)
(*****************************************************************************)

let test_hit () =
  let ball = Solid.Sphere ((0., 0., 0.), 1., matte 0) in
  let from z dz = Ray.make (0., 0., z) (0., 0., dz) in
  near "from outside: where it enters" 4. (Option.get (Solid.hit (from 5. (-1.)) ball));
  near "from inside: where it leaves" 1. (Option.get (Solid.hit (from 0. (-1.)) ball));
  Alcotest.(check bool) "behind: not seen" true (Solid.hit (from 5. 1.) ball = None);
  near "beyond min_t: the far side" 6. (Option.get (Solid.hit ~min_t:4.5 (from 5. (-1.)) ball));
  let floor = Solid.Plane ((0., 1., 0.), 0., matte 0) in
  let down = Ray.make (0., 5., 0.) (0., -1., 0.) in
  near "the plane y = 0 from 5 above" 5. (Option.get (Solid.hit down floor));
  near "moved down by 3: y = -3, 8 below" 8. (Option.get (Solid.hit down (Solid.move (0., -3., 0.) floor)));
  near "a sphere moved: the centre" 2. (Option.get (Solid.hit (from 5. (-1.)) (Solid.move (0., 0., 2.) ball)))

let test_camera_ray () =
  (* looking along -z: right = forward x up = +x *)
  let camera : Camera.t =
    { eye = (0., 0., 0.); target = (0., 0., -1.); up = (0., 1., 0.); fov = 90.; ortho = 0.; near = 0.1; far = 100. }
  in
  (* the .mli's example: fov 90 (f = 1), a square picture, the middle
   * of the right edge -- the centre of pixel (1, 0) of a 2 x 1 picture
   * is not quite there, so take a 1000 x 1000 one's pixel (999, 499),
   * whose centre is at ndc (0.999, 0.001) *)
  let ray, _, _ = Raytrace.camera_ray camera ~width:1000 ~height:1000 ~x:999 ~y:499 in
  let dx, dy, dz = ray.direction in
  Alcotest.(check (float 1e-6)) "45 degrees to the right: x / depth = 0.999" 0.999 (dx /. -.dz);
  Alcotest.(check (float 1e-6)) "and a hair up" 0.001 (dy /. -.dz);
  (* a wide picture, 4 x 2: pixel (3, 0)'s centre is at ndc (0.75, 0.5),
   * aspect 2, so the ray goes along forward + 1.5 right + 0.5 up *)
  let ray, near_t, far_t = Raytrace.camera_ray camera ~width:4 ~height:2 ~x:3 ~y:0 in
  let dx, dy, dz = ray.direction in
  near "x / depth = ndc_x aspect / f" 1.5 (dx /. -.dz);
  near "y / depth = ndc_y / f" 0.5 (dy /. -.dz);
  (* the depth along the ray is t cos angle, cos angle = -dz *)
  near "the near plane, further along a slanted ray" (0.1 /. -.dz) near_t;
  near "and the far one" (100. /. -.dz) far_t;
  (* orthographic: parallel rays, from points across the picture *)
  let ray, _, _ = Raytrace.camera_ray { camera with ortho = 2. } ~width:4 ~height:2 ~x:3 ~y:0 in
  Alcotest.(check (triple (float 1e-9) (float 1e-9) (float 1e-9))) "orthographic: along forward" (0., 0., -1.) ray.direction;
  (* 2 high, so 4 wide: ndc (0.75, 0.5) is (1.5, 0.5) from the eye *)
  Alcotest.(check (triple (float 1e-9) (float 1e-9) (float 1e-9))) "from the pixel's own point" (1.5, 0.5, 0.) ray.origin

(*****************************************************************************)
(* The A/B test *)
(*****************************************************************************)

(* three overlapping squares at different depths, and a triangle through
 * two of them *)
let faces : Render.face list =
  let quad color (cx, cy, z) s =
    let n = (0., 0., 1.) in
    ({ paint = Color color; material = Material.matte;
       points = List.map (fun p -> (p, (0., 0.), n))
           [ (cx -. s, cy -. s, z); (cx +. s, cy -. s, z); (cx +. s, cy +. s, z); (cx -. s, cy +. s, z) ] }
      : Render.face)
  in
  [ quad 0xFF0000 (0., 0., 0.) 1.5; quad 0x00FF00 (1., 0.5, 1.) 1.; quad 0x0000FF (-1., -0.5, -1.) 1.2;
    { paint = Color 0xFFFF00; material = Material.matte;
      points = List.map (fun p -> (p, (0., 0.), (1., 0., 0.))) [ (0., -2., -2.); (0., 2., 2.); (0., -2., 2.) ] } ]

(* fanned as the rasterizer does, as Shape3d_render_software.solids *)
let solids_of (faces : Render.face list) : Solid.t list =
  List.concat_map
    (fun (face : Render.face) ->
      let color = match face.paint with Color c -> c | Texture _ -> 0 in
      match face.points with
      | [] -> []
      | (p0, _, n0) :: rest ->
          let rec fan = function
            | (p1, _, n1) :: ((p2, _, n2) :: _ as rest) ->
                Solid.Triangle { points = (p0, p1, p2); normals = (n0, n1, n2); surface = matte color } :: fan rest
            | _ -> []
          in
          fan rest)
    faces

(* the rasterizer's sun, as Shape3d_render_software gives it to the ray
 * tracer *)
let sun : Raytrace.light =
  let s = 1. -. Lighting.ambient in
  Sun { towards = Lighting.light_dir; color = (s, s, s) }

(* [lit]: the rasterizer's Phong shading against Lambert's light, the
 * same formula; otherwise no lighting against ray casting *)
let differing_pixels ?(lit = false) ?(interpolation = Interpolate.Perspective_correct) (camera : Camera.t) ~width
    ~height : int * int =
  let fb = Framebuffer.create ~width ~height in
  Framebuffer.clear fb ~rgb:0xFFFFFF;
  Render.render
    ~options:
      { Render.default_options with
        shading = (if lit then Shading.Phong else Shading.Flat_color); backface_culling = false; interpolation }
    fb (Zbuffer.create ~width ~height) camera faces;
  let img =
    Raytrace.render
      ~options:{ Raytrace.default_options with algorithm = (if lit then Lambert else Ray_casting) }
      { camera; solids = solids_of faces; lights = [ sun ]; ambient = Lighting.ambient; background = 0xFFFFFF }
      ~width ~height
  in
  let differ = ref 0 in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      let i = 4 * ((y * width) + x) in
      let rgb = (img.rgba.{i} lsl 16) lor (img.rgba.{i + 1} lsl 8) lor img.rgba.{i + 2} in
      if rgb <> Framebuffer.get_rgb fb ~x ~y then incr differ
    done
  done;
  (!differ, width * height)

(* The two renderers agree but for a pixel or two, where a pixel's
 * centre falls exactly on an edge shared by two triangles and the
 * rasterizer's fill rule and Moller-Trumbore's tests settle the tie
 * differently.
 *
 * Except, it turned out, with an orthographic camera: the first run of
 * this test found 162 pixels apart, all where the yellow triangle
 * crosses the green square, the rasterizer drawing the one behind. Its
 * perspective-correct interpolation (the default) interpolates 1/z,
 * which is right for a perspective camera and wrong for an
 * orthographic one, whose depth is linear on the screen: with
 * Interpolate.Linear the two agree again. A rasterizer bug found by
 * the ray tracer, which is what the A/B is for (reported in
 * plan_raytracing_teaching.md, phase 1). *)
let test_same_picture () =
  let camera : Camera.t =
    { eye = (3., 2., 6.); target = (0., 0., 0.); up = (0., 1., 0.); fov = 60.; ortho = 0.; near = 0.1; far = 100. }
  in
  let at_most_a_few msg (differ, _total) =
    if differ > 5 then Alcotest.failf "%s: %d pixels differ" msg differ
  in
  at_most_a_few "perspective" (differing_pixels camera ~width:160 ~height:120);
  at_most_a_few "orthographic, interpolated linearly"
    (differing_pixels ~interpolation:Linear { camera with ortho = 6. } ~width:160 ~height:120);
  (* lit: Lambert's light is Lighting's formula, as the rasterizer's *)
  at_most_a_few "lit, Phong against Lambert" (differing_pixels ~lit:true camera ~width:160 ~height:120)

(*****************************************************************************)
(* Light and shadow: the three classic bugs *)
(*****************************************************************************)

(* a grey floor, y = 0, seen from above; the colour seen straight down
 * at (x, z) *)
let grey = 0x808080
let floor = Solid.Plane ((0., 1., 0.), 0., matte grey)
let from_above : Raytrace.light = Sun { towards = (0., 1., 0.); color = (1., 1., 1.) }

let seen ?(options = Raytrace.default_options) ?(ambient = 0.) (lights : Raytrace.light list) (solids : Solid.t list)
    (x : float) (z : float) : int =
  let scene : Raytrace.scene =
    { camera = { eye = (0., 5., 0.); target = (0., 0., 0.); up = (0., 0., -1.); fov = 60.; ortho = 0.; near = 0.1; far = 100. };
      solids; lights; ambient; background = 0xFFFFFF }
  in
  Raytrace.trace (Raytrace.world ~options scene) (Ray.make (x, 5., z) (0., -1., 0.)) ~min_t:0. ~max_t:infinity

let test_lit () =
  Alcotest.(check int) "a floor facing the sun: its own colour" grey (seen [ from_above ] [ floor ] 0. 0.);
  Alcotest.(check int) "without the sun, the ambient's share" 0x404040 (seen ~ambient:0.5 [] [ floor ] 0. 0.);
  let slanted = Raytrace.Sun { towards = Vec3.normalize (1., 1., 0.); color = (1., 1., 1.) } in
  (* 128 cos 45 = 90.5 *)
  Alcotest.(check int) "the sun at 45 degrees: cos 45 of it" 0x5A5A5A (seen [ slanted ] [ floor ] 0. 0.);
  Alcotest.(check int) "ray casting: the colour, unlit" grey
    (seen ~options:{ Raytrace.default_options with algorithm = Ray_casting } [] [ floor ] 0. 0.)

(* a ball above the floor casts its shadow; Lambert's algorithm,
 * without shadow rays, does not *)
let test_shadow () =
  let ball = Solid.Sphere ((0., 2., 0.), 1., matte 0xFF0000) in
  (* the sun from the upper right, so that the shadow falls beside the
   * ball, where a ray from above sees the floor *)
  let sideways = Raytrace.Sun { towards = Vec3.normalize (1., 1., 0.); color = (1., 1., 1.) } in
  (* the ball's shadow, sun from the upper right, falls at x = -2 *)
  Alcotest.(check int) "in the ball's shadow: nothing but the ambient" 0x202020
    (seen ~ambient:0.25 [ sideways ] [ floor; ball ] (-2.) 0.);
  Alcotest.(check int) "Lambert alone knows no shadow" (seen ~ambient:0.25 [ sideways ] [ floor ] (-2.) 0.)
    (seen ~options:{ Raytrace.default_options with algorithm = Lambert } ~ambient:0.25 [ sideways ] [ floor; ball ] (-2.) 0.)

(* bug 1: with no epsilon, the shadow ray starting on the floor meets
 * the floor itself about half the time. Not the floor y = 0, though:
 * against it the arithmetic happens to come out exact, or a hair above
 * (0 points of 10000, tried) -- a tilted plane, whose n.p sums three
 * rounded products, is what a real scene is made of *)
let test_acne () =
  let n = Vec3.normalize (0.3, 1., 0.2) in
  let floor = Solid.Plane (n, 0.7, matte grey) in
  let from_above : Raytrace.light = Sun { towards = n; color = (1., 1., 1.) } in
  let count options =
    let n = ref 0 in
    for i = 0 to 99 do
      for j = 0 to 99 do
        (* odd, irregular points, whose y comes out a hair off 0 *)
        let x = (float_of_int i *. 0.0731) -. 3.3 and z = (float_of_int j *. 0.0917) -. 4.1 in
        let origin = (x +. 0.37, 4.3, z +. 0.19) in
        let ray = Ray.make origin (Vec3.sub (x, 0., z) origin) in
        let scene : Raytrace.scene =
          { camera = { eye = origin; target = (x, 0., z); up = (0., 1., 0.); fov = 60.; ortho = 0.; near = 0.; far = 100. };
            solids = [ floor ]; lights = [ from_above ]; ambient = 0.; background = 0xFFFFFF }
        in
        if Raytrace.trace (Raytrace.world ~options scene) ray ~min_t:0. ~max_t:infinity <> grey then incr n
      done
    done;
    !n
  in
  let acne = count { Raytrace.default_options with epsilon = 0. } in
  Printf.printf "epsilon 0: %d of 10000 points in their own shadow\n" acne;
  Alcotest.(check bool) "epsilon 0: a stippled floor" true (acne > 1000);
  Alcotest.(check int) "with the epsilon, not a point" 0 (count Raytrace.default_options)

(* bug 2: three lights on a grey point: 3 x 128 is past 255, and a
 * byte would wrap round to 128 *)
let test_clamp () =
  Alcotest.(check int) "three suns: white, not wrapped" 0xFFFFFF (seen [ from_above; from_above; from_above ] [ floor ] 0. 0.)

(* bug 3: the sun is infinitely far; a lamp is not *)
let test_sun_infinitely_far () =
  let cloud = Solid.Sphere ((0., 1000., 0.), 100., matte 0xFFFFFF) in
  Alcotest.(check int) "a sphere 1000 up still shades the floor from the sun" 0
    (seen [ from_above ] [ floor; cloud ] 0.5 0.);
  let lamp = Raytrace.Lamp { position = (0., 10., 0.); color = (1., 1., 1.) } in
  Alcotest.(check int) "but not from a lamp below it" grey (seen [ lamp ] [ floor; cloud ] 0. 0.)

(*****************************************************************************)
(* The picture, a slice at a time *)
(*****************************************************************************)

(* two balls, one on a floor, a lamp and a sun: shadows, and every
 * kind of solid *)
let small_scene : Raytrace.scene =
  { camera = { eye = (0., 2., 6.); target = (0., 0.5, 0.); up = (0., 1., 0.); fov = 50.; ortho = 0.; near = 0.1; far = 100. };
    solids =
      [ Solid.Plane ((0., 1., 0.), 0., matte 0xC0C0C0); Solid.Sphere ((-0.8, 1., 0.), 1., matte 0xCC3333);
        Solid.Sphere ((1.2, 0.6, 0.8), 0.6, matte 0x3333CC) ];
    lights = [ Sun { towards = Vec3.normalize (1., 2., 1.); color = (0.6, 0.6, 0.6) };
               Lamp { position = (-2., 3., 2.); color = (0.5, 0.4, 0.3) } ];
    ambient = 0.2; background = 0x88AAFF }

let bytes (img : Rgba_image.t) : string =
  String.init (Bigarray.Array1.dim img.rgba) (fun i -> Char.chr img.rgba.{i})

(* sizes that are not multiples of 8: the last blocks are cut *)
let test_same_bytes () =
  List.iter
    (fun (width, height) ->
      let expected = bytes (Raytrace.render small_scene ~width ~height) in
      List.iter
        (fun slice ->
          let p = Raytrace.start small_scene ~width ~height in
          while not (Raytrace.finished p) do
            Raytrace.advance p ~rays:slice
          done;
          Alcotest.(check bool)
            (Printf.sprintf "%d x %d, %d rays at a time: the same bytes as render" width height slice)
            true
            (bytes (Raytrace.picture p) = expected);
          Alcotest.(check int) "and no ray shot twice: one per pixel" (width * height) (Raytrace.rays_shot p))
        [ 1; 7; 64; 1000; max_int ])
    [ (37, 23); (64, 48); (1, 1); (9, 17) ]

let test_passes () =
  let width = 40 and height = 30 in
  let p = Raytrace.start small_scene ~width ~height in
  Alcotest.(check int) "at first, the coarsest pass" 8 (Raytrace.pass p);
  (* pass 8 is a ray per 8 x 8 block: 5 x 4 of them *)
  Raytrace.advance p ~rays:20;
  let img = Raytrace.picture p in
  let transparent = ref 0 in
  for i = 0 to (width * height) - 1 do
    if img.rgba.{(4 * i) + 3} = 0 then incr transparent
  done;
  Alcotest.(check int) "20 rays, and the whole picture is there, in blocks" 0 !transparent;
  Alcotest.(check bool) "the picture unchanged: the same image" true (Raytrace.picture p == img);
  Raytrace.advance p ~rays:1;
  Alcotest.(check int) "then pass 4" 4 (Raytrace.pass p);
  Alcotest.(check bool) "a ray more: a new image" true (Raytrace.picture p != img)

(*****************************************************************************)
(* The BVH: the same hits as brute force *)
(*****************************************************************************)

(* a random scene: spheres, triangles (some sharing edges: a strip),
 * and a plane or two, from an explicit seed *)
let random_scene (rng : Random.State.t) (n : int) : Solid.t list =
  let f lo hi = lo +. Random.State.float rng (hi -. lo) in
  let point () = (f (-10.) 10., f (-10.) 10., f (-10.) 10.) in
  let solid i =
    match i mod 3 with
    | 0 -> Solid.Sphere (point (), f 0.1 2., matte i)
    | _ ->
        let a = point () in
        let near () = Vec3.add a (f (-2.) 2., f (-2.) 2., f (-2.) 2.) in
        Solid.Triangle { points = (a, near (), near ()); normals = ((0., 1., 0.), (0., 1., 0.), (0., 1., 0.)); surface = matte i }
  in
  (* a strip of triangles sharing their edges: ties along them *)
  let strip =
    List.init 8 (fun k ->
        let x = float_of_int (k / 2) in
        let p0 = (x, -1., -5.) and p1 = (x +. 1., -1., -5.) and p2 = (x, 1., -5.) and p3 = (x +. 1., 1., -5.) in
        Solid.Triangle
          { points = (if k mod 2 = 0 then (p0, p1, p2) else (p1, p3, p2));
            normals = ((0., 0., 1.), (0., 0., 1.), (0., 0., 1.)); surface = matte (1000 + k) })
  in
  List.init n solid @ strip
  @ if n mod 2 = 0 then [ Solid.Plane ((0., 1., 0.), -12., matte 7) ] else []

let test_bvh_same_hits () =
  let rng = Random.State.make [| 2000 |] in
  List.iter
    (fun n ->
      let solids = random_scene rng n in
      List.iter
        (fun split ->
          let bvh = Bvh.build ~split solids in
          for _ = 1 to 400 do
            let origin = (Random.State.float rng 30. -. 15., Random.State.float rng 30. -. 15., 20.) in
            (* half the rays aimed at the strip's shared edges and corners *)
            let target =
              if Random.State.bool rng then (float_of_int (Random.State.int rng 5), float_of_int (Random.State.int rng 3) -. 1., -5.)
              else (Random.State.float rng 20. -. 10., Random.State.float rng 20. -. 10., Random.State.float rng 20. -. 10.)
            in
            let ray = Ray.make origin (Vec3.sub target origin) in
            let brute = Raytrace.nearest ~min_t:0. ~max_t:infinity ray solids in
            let tree = Bvh.nearest bvh ~min_t:0. ~max_t:infinity ray in
            (match (brute, tree) with
            | None, None -> ()
            | Some (t1, s1), Some (t2, s2) ->
                if not (t1 = t2 && s1 == s2) then
                  Alcotest.failf "%d solids: brute force meets %s at %g, the BVH %s at %g" n
                    (Printf.sprintf "%06x" (Solid.surface s1).color) t1 (Printf.sprintf "%06x" (Solid.surface s2).color) t2
            | _ -> Alcotest.failf "%d solids: one found a hit, the other none" n);
            (* and a shadow ray's question, with a limit *)
            let max_t = Random.State.float rng 40. in
            let exists = List.exists (fun s -> match Solid.hit ~min_t:1e-4 ray s with Some t -> t < max_t | None -> false) solids in
            Alcotest.(check bool) "any = exists" exists (Bvh.any bvh ~min_t:1e-4 ~max_t ray)
          done)
        [ Bvh.Median; Bvh.Sah ])
    [ 0; 1; 2; 5; 17; 100; 500 ]

let test_bvh_same_picture () =
  let scene = { small_scene with solids = small_scene.solids @ random_scene (Random.State.make [| 7 |]) 60 } in
  let picture acceleration =
    bytes (Raytrace.render ~options:{ Raytrace.default_options with acceleration } scene ~width:64 ~height:48)
  in
  let brute = picture Brute_force in
  Alcotest.(check bool) "median split: the same bytes as brute force" true (picture (Bvh Median) = brute);
  Alcotest.(check bool) "surface area heuristic: the same bytes" true (picture (Bvh Sah) = brute)

(* the point of it: far fewer solids tested *)
let test_bvh_saves () =
  let scene = { small_scene with solids = small_scene.solids @ random_scene (Random.State.make [| 11 |]) 1000 } in
  let tests acceleration =
    let w = Raytrace.world ~options:{ Raytrace.default_options with acceleration } scene in
    for y = 0 to 39 do
      for x = 0 to 39 do
        let ray, min_t, max_t = Raytrace.camera_ray scene.camera ~width:40 ~height:40 ~x ~y in
        ignore (Raytrace.trace w ray ~min_t ~max_t)
      done
    done;
    Raytrace.tests w
  in
  let brute = tests Brute_force and median = tests (Bvh Median) and sah = tests (Bvh Sah) in
  Printf.printf "1000 solids, 1600 pixels: solids tested, brute force %d, median %d, SAH %d\n" brute median sah;
  Alcotest.(check bool) "the median split: less than a tenth" true (median * 10 < brute);
  Alcotest.(check bool) "SAH: no more than the median split" true (sah <= median)

(*****************************************************************************)
(* Whitted: mirrors and glass *)
(*****************************************************************************)

let deg (d : float) : float = d *. Float.pi /. 180.
let vec = Alcotest.(triple (float 1e-9) (float 1e-9) (float 1e-9))

let test_optics () =
  Alcotest.check vec "reflected: down onto a floor, up" (1., 1., 0.) (Raytrace.reflect (1., -1., 0.) (0., 1., 0.));
  (* into glass at 30 degrees from the normal (0, 1, 0), going down *)
  let d = (sin (deg 30.), -.cos (deg 30.), 0.) in
  (match Raytrace.refract d (0., 1., 0.) ~eta:(1. /. 1.5) with
  | Some ((x, _, _), _) -> near "Snell: sin t = sin 30 / 1.5 = 1/3" (1. /. 3.) x
  | None -> Alcotest.fail "air into glass always goes through");
  let out_at a = Raytrace.refract (sin (deg a), -.cos (deg a), 0.) (0., 1., 0.) ~eta:1.5 in
  Alcotest.(check bool) "out of glass at 40 degrees: through" true (out_at 40. <> None);
  Alcotest.(check bool) "at 45, past the critical 41.8: total internal reflection" true (out_at 45. = None);
  (match Raytrace.refract (0., -1., 0.) (0., 1., 0.) ~eta:1. with
  | Some (dir, cos_t) ->
      Alcotest.check vec "an index of 1: not bent" (0., -1., 0.) dir;
      near "and straight on" 1. cos_t
  | None -> Alcotest.fail "eta 1 goes through");
  near "Schlick: glass head on, 4%" 0.04 (Raytrace.schlick ~n1:1. ~n2:1.5 1.);
  near "at a grazing angle, all of it" 1. (Raytrace.schlick ~n1:1. ~n2:1.5 0.)

(* a ray from above, straight down onto a floor, and what it sees;
 * ambient 1 and no light, so that a colour is itself *)
let whitted_scene ?(options = Raytrace.default_options) (solids : Solid.t list) : int =
  let scene : Raytrace.scene =
    { camera = { eye = (0., 5., 0.); target = (0., 0., 0.); up = (0., 0., -1.); fov = 60.; ortho = 0.; near = 0.; far = 100. };
      solids; lights = []; ambient = 1.; background = 0x0000FF }
  in
  Raytrace.trace (Raytrace.world ~options scene) (Ray.make (0., 5., 0.) (0., -1., 0.)) ~min_t:0. ~max_t:infinity

let mirror s : Solid.surface = { (matte 0x808080) with material = { Material.matte with shiny = s } }
let glass n : Solid.surface = { (matte 0xFFFFFF) with material = { Material.matte with glassy = Some n } }
(* a red ball above the eye: only a mirror below shows it *)
let red_ball = Solid.Sphere ((0., 10., 0.), 1., matte 0xFF0000)

let test_mirror () =
  Alcotest.(check int) "a perfect mirror: the ball above, red" 0xFF0000
    (whitted_scene [ Solid.Plane ((0., 1., 0.), 0., mirror 1.); red_ball ]);
  (* half a grey mirror: 128 / 2 + 255 / 2 red, 128 / 2 green and blue *)
  Alcotest.(check int) "half a mirror: half its grey, half the red" 0xBF4040
    (whitted_scene [ Solid.Plane ((0., 1., 0.), 0., mirror 0.5); red_ball ]);
  Alcotest.(check int) "shadow rays: no mirror, the grey" 0x808080
    (whitted_scene ~options:{ Raytrace.default_options with algorithm = Shadow_rays }
       [ Solid.Plane ((0., 1., 0.), 0., mirror 1.); red_ball ]);
  Alcotest.(check int) "depth 0: no bounce, the grey" 0x808080
    (whitted_scene ~options:{ Raytrace.default_options with depth = 0 } [ Solid.Plane ((0., 1., 0.), 0., mirror 1.); red_ball ])

let test_glass () =
  let floor = Solid.Plane ((0., 1., 0.), 0., matte 0x00FF00) in
  (* index 1, head on: F = 0, all of it through, unbent *)
  Alcotest.(check int) "glass of index 1: the floor, as if not there" 0x00FF00
    (whitted_scene [ floor; Solid.Sphere ((0., 2., 0.), 1., glass 1.) ]);
  (* index 1.5, head on, on the ball lens's axis: 4% reflected where the
   * ray enters (the blue sky, straight back up), the rest through and
   * out onto the floor: green mostly, a little blue *)
  let c = whitted_scene [ floor; Solid.Sphere ((0., 2., 0.), 1., glass 1.5) ] in
  Alcotest.(check bool) "glass of 1.5: mostly the floor, a little of the sky" true
    ((c lsr 8) land 0xFF > 200 && c land 0xFF > 0 && c land 0xFF < 30)

let test_cutoff () =
  let scene : Raytrace.scene =
    { camera = { eye = (0., 5., 0.); target = (0., 0., 0.); up = (0., 0., -1.); fov = 60.; ortho = 0.; near = 0.; far = 100. };
      solids = [ Solid.Plane ((0., 1., 0.), 0., mirror 0.001); red_ball ]; lights = []; ambient = 1.; background = 0 }
  in
  let w = Raytrace.world scene in
  ignore (Raytrace.trace w (Ray.make (0., 5., 0.) (0., -1., 0.)) ~min_t:0. ~max_t:infinity);
  Alcotest.(check int) "a thousandth of a mirror: under 1/256, not shot" 1 (Raytrace.saved_rays w);
  Alcotest.(check int) "and nothing reflected" 0 (Raytrace.secondary_rays w);
  (* two perfect mirrors face to face: the depth stops them *)
  let w =
    Raytrace.world
      { scene with solids = [ Solid.Plane ((0., 1., 0.), 0., mirror 1.); Solid.Plane ((0., 1., 0.), 8., mirror 1.) ] }
  in
  ignore (Raytrace.trace w (Ray.make (0., 5., 0.) (0., -1., 0.)) ~min_t:0. ~max_t:infinity);
  Alcotest.(check int) "facing mirrors: 3 bounces, the depth" 3 (Raytrace.secondary_rays w)

(*****************************************************************************)
(* The other solids, transforms, CSG *)
(*****************************************************************************)

let pairs msg (expected : (float * float) list) (l : Solid.t Csg.interval list) =
  Alcotest.(check (list (pair (float 1e-6) (float 1e-6)))) msg expected
    (List.map (fun ((i : Solid.t Csg.boundary), (o : Solid.t Csg.boundary)) -> (i.t, o.t)) l)

let placed ?(transform = Transform.identity) primitive : Solid.t = Placed { primitive; transform; surface = matte 0 }
let from_x = Ray.make (5., 0., 0.) (-1., 0., 0.)
let from_y = Ray.make (0., 5., 0.) (0., -1., 0.)

let test_primitives () =
  pairs "cube, along x: x = 1 to -1" [ (4., 6.) ] (Solid.intervals from_x (placed Cube));
  pairs "cylinder, across: its side" [ (4., 6.) ] (Solid.intervals from_x (placed Cylinder));
  pairs "cylinder, down its axis: its caps" [ (4., 6.) ] (Solid.intervals from_y (placed Cylinder));
  pairs "cone, down its axis: apex to base" [ (4., 6.) ] (Solid.intervals from_y (placed Cone));
  pairs "cone, across at y = 0: radius (1 - 0) / 2" [ (4.5, 5.5) ] (Solid.intervals from_x (placed Cone));
  pairs "torus, across: the tube twice" [ (3.75, 4.25); (5.75, 6.25) ] (Solid.intervals from_x (placed (Torus 0.25)));
  pairs "torus, down its hole: nothing" [] (Solid.intervals from_y (placed (Torus 0.25)));
  pairs "half-space, from above: from y = 0 down, for ever" [ (5., infinity) ] (Solid.intervals from_y (placed Half_space))

let test_transform () =
  let tr =
    Transform.compose (Transform.translate (1., -2., 3.))
      (Transform.compose (Transform.rotate 1 30.) (Transform.compose (Transform.scale (2., 0.5, 3.)) (Transform.rotate 0 70.)))
  in
  let p = (0.3, -1.7, 2.9) in
  Alcotest.check vec "there and back" p (Transform.inverse_point tr (Transform.point tr p));
  Alcotest.check vec "back and there" p (Transform.point tr (Transform.inverse_point tr p));
  (* a normal stays perpendicular to the surface's tangents *)
  let n = Vec3.normalize (1., 2., -1.) and tangent = (2., -1., 0.) in
  near "the inverse transpose: perpendicular still" 0. (Vec3.dot (Transform.normal tr n) (Transform.direction tr tangent));
  (* a sphere stretched by (2, 1, 1): an ellipsoid, x^2 / 4 + y^2 <= 1 *)
  let ellipsoid = Solid.transform (Transform.scale (2., 1., 1.)) (Solid.Sphere ((0., 0., 0.), 1., matte 0)) in
  near "the ellipsoid along x: at x = 2" 3. (Option.get (Solid.hit from_x ellipsoid));
  (* at (2 cos 45, sin 45), its normal is the gradient (x / 2, 2 y), (1, 2) *)
  let p = (2. *. cos (deg 45.), sin (deg 45.), 0.) and out = Vec3.normalize (1., 2., 0.) in
  let ray = Ray.make (Vec3.add p (Vec3.scale 3. out)) (Vec3.scale (-1.) out) in
  let t = Option.get (Solid.hit ray ellipsoid) in
  near "hit where aimed" 3. t;
  Alcotest.check vec "its normal: the gradient's, not the sphere's" out (Solid.normal ellipsoid ray t)

(* the blind hole of Csg.mli *)
let test_csg () =
  let cube = Solid.Placed { primitive = Cube; transform = Transform.identity; surface = matte 0xFF0000 } in
  let hole =
    Solid.transform
      (Transform.compose (Transform.translate (1., 0., 0.)) (Transform.compose (Transform.rotate 2 90.) (Transform.scale (0.5, 1., 0.5))))
      (Solid.Placed { primitive = Cylinder; transform = Transform.identity; surface = matte 0x0000FF })
  in
  let drilled = Solid.csg Diff cube hole in
  let ray = Ray.make (3., 0., 0.) (-1., 0., 0.) in
  pairs "the cube" [ (2., 4.) ] (Solid.intervals ray cube);
  pairs "the hole: x = 2 to 0" [ (1., 3.) ] (Solid.intervals ray hole);
  pairs "cube - hole: from the hole's bottom on" [ (3., 4.) ] (Solid.intervals ray drilled);
  (match Solid.first_hit ray drilled with
  | Some b ->
      near "seen: the bottom of the hole, at x = 0" 3. b.t;
      Alcotest.(check bool) "the cylinder's surface" true (b.leaf == hole);
      Alcotest.(check bool) "flipped" true b.flipped;
      Alcotest.check vec "its normal, turned round: towards the eye" (1., 0., 0.)
        (Vec3.scale (-1.) (Solid.normal b.leaf ray b.t))
  | None -> Alcotest.fail "the hole has a bottom");
  pairs "off the hole, at y = 0.75: the whole cube" [ (2., 4.) ] (Solid.intervals (Ray.make (3., 0.75, 0.) (-1., 0., 0.)) drilled);
  Alcotest.check_raises "a triangle in a difference: refused"
    (Invalid_argument "Solid.csg: a triangle has no inside, it cannot be intersected or subtracted") (fun () ->
      ignore (Solid.csg Diff cube (Solid.Triangle { points = ((0., 0., 0.), (1., 0., 0.), (0., 1., 0.)); normals = ((0., 0., 1.), (0., 0., 1.), (0., 0., 1.)); surface = matte 0 })))

(* random solids: a transformed primitive, or a CSG of two *)
let rec random_solid (rng : Random.State.t) (depth : int) : Solid.t =
  let f lo hi = lo +. Random.State.float rng (hi -. lo) in
  if depth = 0 || Random.State.int rng 3 = 0 then
    let primitive : Solid.primitive =
      match Random.State.int rng 6 with 0 -> Ball | 1 -> Cube | 2 -> Cylinder | 3 -> Cone | 4 -> Torus (f 0.1 0.6) | _ -> Ball
    in
    let tr =
      Transform.compose (Transform.translate (f (-2.) 2., f (-2.) 2., f (-2.) 2.))
        (Transform.compose (Transform.rotate (Random.State.int rng 3) (f 0. 360.)) (Transform.scale (f 0.5 2., f 0.5 2., f 0.5 2.)))
    in
    Placed { primitive; transform = tr; surface = matte (Random.State.int rng 0xFFFFFF) }
  else
    let op : Csg.op = match Random.State.int rng 3 with 0 -> Union | 1 -> Inter | _ -> Diff in
    Solid.csg op (random_solid rng (depth - 1)) (random_solid rng (depth - 1))

(* inside by the intervals = inside by each solid's definition, at the
 * middles of the intervals and of the gaps between them *)
let test_membership () =
  let rng = Random.State.make [| 1982 |] in
  let checked = ref 0 in
  for _ = 1 to 300 do
    let solid = random_solid rng 3 in
    for _ = 1 to 20 do
      let origin = (Random.State.float rng 16. -. 8., Random.State.float rng 16. -. 8., Random.State.float rng 16. -. 8.) in
      let ray = Ray.make origin (Vec3.sub (Random.State.float rng 2. -. 1., Random.State.float rng 2. -. 1., Random.State.float rng 2. -. 1.) origin) in
      let l = Solid.intervals ray solid in
      let ends = List.concat_map (fun ((i : Solid.t Csg.boundary), (o : Solid.t Csg.boundary)) -> [ i.t; o.t ]) l in
      let finite = List.filter Float.is_finite ends in
      let samples =
        let rec mids = function a :: (b :: _ as rest) -> ((a +. b) /. 2.) :: mids rest | _ -> [] in
        mids finite @ (match finite with [] -> [ 0.; 10. ] | _ -> [ List.hd finite -. 1.; List.nth finite (List.length finite - 1) +. 1. ])
      in
      List.iter
        (fun t ->
          incr checked;
          if Csg.inside l t <> Solid.contains solid (Ray.at ray t) then
            Alcotest.failf "at t = %g: the intervals say %b, the solid %b" t (Csg.inside l t) (not (Csg.inside l t)))
        samples
    done
  done;
  Printf.printf "%d points classified the same both ways\n" !checked

(* the BVH, with boxes carried through transforms and CSG *)
let test_bvh_csg () =
  let rng = Random.State.make [| 1986 |] in
  let solids = List.init 60 (fun _ -> random_solid rng 2) in
  let bvh = Bvh.build ~split:Sah solids in
  for _ = 1 to 1000 do
    let origin = (Random.State.float rng 20. -. 10., Random.State.float rng 20. -. 10., 12.) in
    let ray = Ray.make origin (Vec3.sub (Random.State.float rng 6. -. 3., Random.State.float rng 6. -. 3., Random.State.float rng 6. -. 3.) origin) in
    match (Raytrace.nearest ray solids, Bvh.nearest bvh ~min_t:0. ~max_t:infinity ray) with
    | None, None -> ()
    | Some (t1, s1), Some (t2, s2) when t1 = t2 && s1 == s2 -> ()
    | _ -> Alcotest.fail "the BVH and brute force disagree on a CSG scene"
  done

let test_spot () =
  let floor = Solid.Plane ((0., 1., 0.), 0., matte 0x808080) in
  let spot : Raytrace.light = Spot { position = (0., 4., 0.); aim = (0., -1., 0.); angle = 30.; falloff = 0.; color = (1., 1., 1.) } in
  Alcotest.(check int) "under the spot: lit" 0x808080 (seen [ spot ] [ floor ] 0. 0.);
  (* 4 below and 3 aside: 37 degrees off its axis, outside its 30 *)
  Alcotest.(check int) "outside its cone: dark" 0 (seen [ spot ] [ floor ] 3. 0.)

let tests =
  Testo.categorize "Raytrace"
    [ t "Solid.hit: in front, from inside, moved" test_hit; t "the camera rays" test_camera_ray;
      t "the rasterizer's picture, ray cast" test_same_picture; t "Lambert's light" test_lit;
      t "shadow rays" test_shadow; t "bug 1, shadow acne: the epsilon" test_acne;
      t "bug 2, too much light: the clamp" test_clamp; t "bug 3, the sun is infinitely far" test_sun_infinitely_far;
      t "progressive: the same bytes, whatever the slices" test_same_bytes;
      t "progressive: coarse to fine" test_passes;
      t "BVH: the same hits as brute force, random scenes" test_bvh_same_hits;
      t "BVH: the same picture" test_bvh_same_picture; t "BVH: fewer tests" test_bvh_saves;
      t "Whitted: reflect, Snell, Schlick" test_optics; t "Whitted: mirrors" test_mirror;
      t "Whitted: glass" test_glass; t "Whitted: the cutoff and the depth" test_cutoff;
      t "the unit primitives, along a ray" test_primitives; t "Transform, the ellipsoid's normal" test_transform;
      t "CSG: the blind hole" test_csg; t "CSG: point membership, random solids" test_membership;
      t "CSG: the BVH" test_bvh_csg; t "spots" test_spot ]
