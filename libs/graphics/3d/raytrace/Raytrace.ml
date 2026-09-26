(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Raytrace.mli *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type rgb = float * float * float
type light =
  | Sun of { towards : Vec3.t; color : rgb }
  | Lamp of { position : Vec3.t; color : rgb }
  | Spot of { position : Vec3.t; aim : Vec3.t; angle : float; falloff : float; color : rgb }

type scene = { camera : Camera.t; solids : Solid.t list; lights : light list; ambient : float; background : int }

type algorithm = Ray_casting | Lambert | Shadow_rays | Whitted

let algorithms = [ Ray_casting; Lambert; Shadow_rays; Whitted ]
let latest = Whitted

let name (algorithm : algorithm) : string =
  match algorithm with
  | Ray_casting -> "ray casting"
  | Lambert -> "Lambert's light"
  | Shadow_rays -> "shadow rays"
  | Whitted -> "Whitted: mirrors and glass"

type acceleration = Brute_force | Bvh of Bvh.split

type options = {
  algorithm : algorithm;
  epsilon : float;
  acceleration : acceleration;
  depth : int;
  cutoff : float;
  samples : int;
}

let default_options =
  { algorithm = latest; epsilon = 1e-4; acceleration = Bvh Sah; depth = 3; cutoff = 1. /. 256.; samples = 1 }

(*****************************************************************************)
(* Camera rays *)
(*****************************************************************************)

(* through the point (px, py) of the picture, in pixels from its top
 * left corner: a pixel's centre is (x + 0.5, y + 0.5) *)
let camera_ray_through (camera : Camera.t) ~(width : int) ~(height : int) (px : float) (py : float) :
    Ray.t * float * float =
  let right, up, forward = Camera.basis ~up:camera.up ~eye:camera.eye ~target:camera.target () in
  let w = float_of_int width and h = float_of_int height in
  let aspect = w /. h in
  let ndc_x = (px -. (w /. 2.)) /. (w /. 2.) in
  let ndc_y = ((h /. 2.) -. py) /. (h /. 2.) in
  let along (sx : float) (sy : float) : Vec3.t = Vec3.add (Vec3.scale sx right) (Vec3.scale sy up) in
  let ray =
    if camera.ortho > 0. then
      let half = camera.ortho /. 2. in
      Ray.make (Vec3.add camera.eye (along (ndc_x *. aspect *. half) (ndc_y *. half))) forward
    else
      let f = Camera.focal camera in
      Ray.make camera.eye (Vec3.add forward (along (ndc_x *. aspect /. f) (ndc_y /. f)))
  in
  (* depth = t cos angle, so the depth d is at t = d / cos angle *)
  let cos_angle = Vec3.dot ray.direction forward in
  (ray, camera.near /. cos_angle, camera.far /. cos_angle)

(* the pixel's centre, as Triangle.fill samples it *)
let camera_ray (camera : Camera.t) ~(width : int) ~(height : int) ~(x : int) ~(y : int) : Ray.t * float * float =
  camera_ray_through camera ~width ~height (float_of_int x +. 0.5) (float_of_int y +. 0.5)

(*****************************************************************************)
(* What a ray meets *)
(*****************************************************************************)

let nearest ?(min_t = 0.) ?(max_t = infinity) (ray : Ray.t) (solids : Solid.t list) : (float * Solid.t) option =
  List.fold_left
    (fun best solid ->
      match Solid.hit ~min_t ray solid with
      | Some t when t < max_t && (match best with None -> true | Some (t', _) -> t < t') -> Some (t, solid)
      | _ -> best)
    None solids

(* the direction towards the light from a point, and how far it is *)
let towards_light (point : Vec3.t) (light : light) : Vec3.t * float =
  match light with
  (* the sun: infinitely far, the third classic bug if not *)
  | Sun { towards; _ } -> (towards, infinity)
  | Lamp { position; _ } | Spot { position; _ } ->
      let d = Vec3.sub position point in
      (Vec3.normalize d, Vec3.length d)

(* how much of a light reaches a point, whatever stands between: all of
 * a sun's and a lamp's; of a spot's, none outside its cone, and inside
 * cos^falloff of the angle off its aim (GML's spotlight) *)
let spot_factor (point : Vec3.t) (light : light) : float =
  match light with
  | Sun _ | Lamp _ -> 1.
  | Spot { position; aim; angle; falloff; _ } ->
      let cos = Vec3.dot (Vec3.normalize (Vec3.sub point position)) aim in
      if cos < Float.cos (angle *. Float.pi /. 180.) then 0. else cos ** falloff

(* any solid at all in the way will do: no need for the nearest *)
let shadowed ~(epsilon : float) (solids : Solid.t list) (point : Vec3.t) (light : light) : bool =
  let dir, distance = towards_light point light in
  let ray = Ray.make point dir in
  List.exists
    (fun solid -> match Solid.hit ~min_t:epsilon ray solid with Some t -> t < distance | None -> false)
    solids

(*****************************************************************************)
(* The world *)
(*****************************************************************************)

type world = {
  options : options;
  scene : scene;
  bvh : Bvh.t option;
  (* brute force's own count of the solids it tests *)
  mutable brute_tests : int;
  (* Whitted's rays: the reflected and refracted ones shot, and those
   * the cutoff did not shoot *)
  mutable secondary : int;
  mutable saved : int;
}

let world ?(options = default_options) (scene : scene) : world =
  let bvh = match options.acceleration with Brute_force -> None | Bvh split -> Some (Bvh.build ~split scene.solids) in
  { options; scene; bvh; brute_tests = 0; secondary = 0; saved = 0 }

let tests (w : world) : int = match w.bvh with Some bvh -> (Bvh.stats bvh).tests | None -> w.brute_tests
let boxes (w : world) : int = match w.bvh with Some bvh -> (Bvh.stats bvh).boxes | None -> 0
let secondary_rays (w : world) : int = w.secondary
let saved_rays (w : world) : int = w.saved

(* [nearest] and [shadowed], by the world's acceleration *)
let find_nearest (w : world) ~(min_t : float) ~(max_t : float) (ray : Ray.t) : (float * Solid.t) option =
  match w.bvh with
  | Some bvh -> Bvh.nearest bvh ~min_t ~max_t ray
  | None ->
      w.brute_tests <- w.brute_tests + List.length w.scene.solids;
      nearest ~min_t ~max_t ray w.scene.solids

let blocked (w : world) (point : Vec3.t) (light : light) : bool =
  match w.bvh with
  | Some bvh ->
      let dir, distance = towards_light point light in
      Bvh.any bvh ~min_t:w.options.epsilon ~max_t:distance (Ray.make point dir)
  | None ->
      w.brute_tests <- w.brute_tests + List.length w.scene.solids;
      shadowed ~epsilon:w.options.epsilon w.scene.solids point light

(*****************************************************************************)
(* The colour seen *)
(*****************************************************************************)

(* A colour on its way: three channels, 0. to 255. like a 0xRRGGBB's,
 * but floats, and not yet clamped -- a sum of lights, or a mirror's
 * share of what it reflects, can pass 255 on the way *)
type color = float * float * float

let color_of_int (rgb : int) : color =
  (float_of_int ((rgb lsr 16) land 0xFF), float_of_int ((rgb lsr 8) land 0xFF), float_of_int (rgb land 0xFF))

(* the second bug's clamp, once, at the end: a channel stops at 255 *)
let int_of_color ((r, g, b) : color) : int =
  let channel c = Stdlib.min 255 (int_of_float c) in
  (channel r lsl 16) lor (channel g lsl 8) lor channel b

let add ((r1, g1, b1) : color) ((r2, g2, b2) : color) : color = (r1 +. r2, g1 +. g2, b1 +. b2)
let times (k : float) ((r, g, b) : color) : color = (k *. r, k *. g, k *. b)

(* the point's own light: its colour times the ambient and each light
 * that reaches it (Lambert), as the rasterizer's Render.scale_channel *)
let lit (w : world) (point : Vec3.t) (n : Vec3.t) (color : int) : color =
  let r, g, b =
    List.fold_left
      (fun ((r, g, b) as sum) light ->
        let dir, _ = towards_light point light in
        let cos = Vec3.dot n dir *. spot_factor point light in
        if cos <= 0. then sum
        else if w.options.algorithm <> Lambert && blocked w point light then sum
        else
          let lr, lg, lb = match light with Sun { color; _ } | Lamp { color; _ } | Spot { color; _ } -> color in
          (r +. (lr *. cos), g +. (lg *. cos), b +. (lb *. cos)))
      (0., 0., 0.) w.scene.lights
  in
  let a = w.scene.ambient in
  let cr, cg, cb = color_of_int color in
  (cr *. (a +. r), cg *. (a +. g), cb *. (a +. b))

(* [d] reflected by a surface of normal [n]: d - 2 (d . n) n *)
let reflect (d : Vec3.t) (n : Vec3.t) : Vec3.t = Vec3.sub d (Vec3.scale (2. *. Vec3.dot d n) n)

(* [refract d n ~eta]: [d] bent through a surface of normal [n] (facing
 * [d]'s side), [eta] = n1 / n2, the index it leaves over the index it
 * enters (Snell: n1 sin i = n2 sin t), and the cosine of the angle it
 * leaves at; [None] past the critical angle, where no light goes
 * through: total internal reflection *)
let refract (d : Vec3.t) (n : Vec3.t) ~(eta : float) : (Vec3.t * float) option =
  let cos_i = -.Vec3.dot d n in
  let k = 1. -. (eta *. eta *. (1. -. (cos_i *. cos_i))) in
  if k < 0. then None
  else
    let cos_t = sqrt k in
    Some (Vec3.add (Vec3.scale eta d) (Vec3.scale ((eta *. cos_i) -. cos_t) n), cos_t)

(* Schlick's (1994) Fresnel: the share of light reflected at a surface
 * between indices n1 and n2, [cos] the angle's cosine on the side of
 * the lower index -- 4% for glass seen head on, all of it at grazing *)
let schlick ~(n1 : float) ~(n2 : float) (cos : float) : float =
  let r0 = ((n1 -. n2) /. (n1 +. n2)) ** 2. in
  r0 +. ((1. -. r0) *. ((1. -. cos) ** 5.))

(* the colour seen along a ray: [depth] the bounces so far, [weight]
 * the share of the pixel this ray's colour will be *)
let rec radiance (w : world) (ray : Ray.t) ~(min_t : float) ~(max_t : float) ~(depth : int) ~(weight : float) : color =
  match find_nearest w ~min_t ~max_t ray with
  | None -> color_of_int w.scene.background
  | Some (t, solid) -> (
      (* the leaf whose surface is seen there, and whether its normal is
       * turned round: a CSG's hole (Csg.mli); a leaf is itself *)
      let leaf, flipped =
        match Solid.first_hit ~min_t ray solid with
        | Some b when b.t = t -> (b.leaf, b.flipped)
        | _ -> (solid, false)
      in
      let surface = Solid.surface leaf in
      let point = Ray.at ray t in
      let color = Solid.color leaf ray t in
      match w.options.algorithm with
      | Ray_casting -> color_of_int color
      | Lambert | Shadow_rays | Whitted -> (
          (* the side the eye sees: a plane from below, a sphere from
           * inside *)
          let n = Solid.normal leaf ray t in
          let n = if flipped then Vec3.scale (-1.) n else n in
          let inside = Vec3.dot n ray.direction > 0. in
          let n = if inside then Vec3.scale (-1.) n else n in
          let local = lit w point n color in
          let m = surface.material in
          if w.options.algorithm <> Whitted || depth >= w.options.depth then local
          else
            (* one more ray, if what it can add is worth it: the
             * attenuation cutoff (Camls 'R Us, ICFP 2000) *)
            let bounce (dir : Vec3.t) (share : float) : color =
              if weight *. share < w.options.cutoff then begin
                w.saved <- w.saved + 1;
                (0., 0., 0.)
              end
              else begin
                w.secondary <- w.secondary + 1;
                radiance w (Ray.make point dir) ~min_t:w.options.epsilon ~max_t:infinity ~depth:(depth + 1)
                  ~weight:(weight *. share)
              end
            in
            match m.glassy with
            | Some index -> (
                (* leaving the glass, or entering it *)
                let n1, n2 = if inside then (index, 1.) else (1., index) in
                match refract ray.direction n ~eta:(n1 /. n2) with
                | None -> bounce (reflect ray.direction n) 1.
                | Some (through, cos_t) ->
                    let cos_i = -.Vec3.dot ray.direction n in
                    let f = schlick ~n1 ~n2 (if n1 <= n2 then cos_i else cos_t) in
                    (* the glass's colour filters what goes through it *)
                    let tr, tg, tb = bounce through (1. -. f) and cr, cg, cb = color_of_int color in
                    add
                      (times f (bounce (reflect ray.direction n) f))
                      (times (1. -. f) (tr *. cr /. 255., tg *. cg /. 255., tb *. cb /. 255.)))
            | None ->
                if m.shiny <= 0. then local
                else add (times (1. -. m.shiny) local) (times m.shiny (bounce (reflect ray.direction n) m.shiny))))

let trace (w : world) (ray : Ray.t) ~(min_t : float) ~(max_t : float) : int =
  int_of_color (radiance w ray ~min_t ~max_t ~depth:0 ~weight:1.)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let set_pixel (img : Rgba_image.t) ~(x : int) ~(y : int) (rgb : int) : unit =
  let i = 4 * ((y * img.width) + x) in
  img.rgba.{i} <- (rgb lsr 16) land 0xFF;
  img.rgba.{i + 1} <- (rgb lsr 8) land 0xFF;
  img.rgba.{i + 2} <- rgb land 0xFF;
  img.rgba.{i + 3} <- 0xFF

(* a pixel's colour: its centre's ray, or with n samples the n x n
 * cells of a grid over it, a ray through each cell's centre, averaged
 * (stratified: one sample in each cell, rather than n^2 anywhere) *)
let pixel (w : world) ~(width : int) ~(height : int) ~(x : int) ~(y : int) : int =
  let n = w.options.samples in
  if n <= 1 then
    let ray, min_t, max_t = camera_ray w.scene.camera ~width ~height ~x ~y in
    trace w ray ~min_t ~max_t
  else
    let sum = ref (0., 0., 0.) in
    for j = 0 to n - 1 do
      for i = 0 to n - 1 do
        let px = float_of_int x +. ((float_of_int i +. 0.5) /. float_of_int n)
        and py = float_of_int y +. ((float_of_int j +. 0.5) /. float_of_int n) in
        let ray, min_t, max_t = camera_ray_through w.scene.camera ~width ~height px py in
        (* each sample clamped first, as one ray's pixel would be: three
         * suns on a sample must not make it count three times *)
        let r, g, b = radiance w ray ~min_t ~max_t ~depth:0 ~weight:1. in
        sum := add !sum (Float.min r 255., Float.min g 255., Float.min b 255.)
      done
    done;
    int_of_color (times (1. /. float_of_int (n * n)) !sum)

let render ?options (scene : scene) ~(width : int) ~(height : int) : Rgba_image.t =
  let w = world ?options scene in
  let img = Rgba_image.create ~width ~height in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      set_pixel img ~x ~y (pixel w ~width ~height ~x ~y)
    done
  done;
  img

(*****************************************************************************)
(* The picture, a slice at a time *)
(*****************************************************************************)

let passes = [ 8; 4; 2; 1 ]

type progress = {
  world : world;
  img : Rgba_image.t;
  (* the passes to go, the one under way first *)
  mutable todo : int list;
  (* the next corner of the pass under way, counted in reading order
   * over its grid *)
  mutable next : int;
  mutable rays : int;
  (* the pixels have changed since [picture] last copied them *)
  mutable changed : bool;
  mutable shown : Rgba_image.t option;
}

let start ?options (scene : scene) ~(width : int) ~(height : int) : progress =
  { world = world ?options scene; img = Rgba_image.create ~width ~height; todo = passes; next = 0; rays = 0; changed = false;
    shown = None }

let finished (p : progress) : bool = p.todo = []
let rays_shot (p : progress) : int = p.rays
let pass (p : progress) : int = match p.todo with s :: _ -> s | [] -> 1

let advance (p : progress) ~(rays : int) : unit =
  let width = p.img.width and height = p.img.height in
  let budget = ref rays in
  while !budget > 0 && not (finished p) do
    match p.todo with
    | [] -> ()
    | s :: rest ->
        let cols = (width + s - 1) / s and rows = (height + s - 1) / s in
        if p.next >= cols * rows then begin
          p.todo <- rest;
          p.next <- 0
        end
        else begin
          let x = p.next mod cols * s and y = p.next / cols * s in
          p.next <- p.next + 1;
          (* a corner of the pass before (twice the size) has its ray *)
          let done_before = s < List.hd passes && x mod (2 * s) = 0 && y mod (2 * s) = 0 in
          if not done_before then begin
            let rgb = pixel p.world ~width ~height ~x ~y in
            for by = y to Int.min height (y + s) - 1 do
              for bx = x to Int.min width (x + s) - 1 do
                set_pixel p.img ~x:bx ~y:by rgb
              done
            done;
            p.rays <- p.rays + (p.world.options.samples * p.world.options.samples);
            p.changed <- true;
            budget := !budget - (p.world.options.samples * p.world.options.samples)
          end
        end
  done

let world_of (p : progress) : world = p.world

let picture (p : progress) : Rgba_image.t =
  match p.shown with
  | Some img when not p.changed -> img
  | _ ->
      let copy = Rgba_image.create ~width:p.img.width ~height:p.img.height in
      Bigarray.Array1.blit p.img.rgba copy.rgba;
      p.shown <- Some copy;
      p.changed <- false;
      copy
