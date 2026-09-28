(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Povray.mli *)

let rgb_of_color (color : Playground.color) : int =
  match color with
  | Color.Rgb (r, g, b) -> (r lsl 16) lor (g lsl 8) lor b
  | Color.Hex s ->
      let component i = int_of_string ("0x" ^ String.sub s i 2) in
      (component 1 lsl 16) lor (component 3 lsl 8) lor component 5

(*****************************************************************************)
(* The camera *)
(*****************************************************************************)

type camera = Camera.t

let camera ?(fov = 60.) ~(eye : Vec3.t) ~(target : Vec3.t) () : camera =
  (* near and far clip a rasterizer's triangles; here nothing is too
   * near, and nothing too far *)
  { eye; target; up = (0., 1., 0.); fov; ortho = 0.; near = 0.; far = infinity }

(*****************************************************************************)
(* The solids *)
(*****************************************************************************)

type surface = Solid.surface
type obj = Solid.t

let color (c : Playground.color) : surface = { color = rgb_of_color c; pattern = Plain; material = Material.matte }

let checker ?(size = 1.) (c1 : Playground.color) (c2 : Playground.color) : surface =
  { (color c1) with pattern = Checker (rgb_of_color c2, size) }

let marble ?(size = 1.) (c1 : Playground.color) (c2 : Playground.color) : surface =
  { (color c1) with pattern = Marble (rgb_of_color c2, size) }

let wood ?(size = 0.2) (c1 : Playground.color) (c2 : Playground.color) : surface =
  { (color c1) with pattern = Wood (rgb_of_color c2, size) }

let pattern (f : x:float -> y:float -> z:float -> Playground.color) : surface =
  { (color Color.white) with pattern = Solid_function (fun (x, y, z) -> rgb_of_color (f ~x ~y ~z)) }

let shiny (s : float) (surface : surface) : surface = { surface with material = { surface.material with shiny = s } }
let glassy (index : float) (surface : surface) : surface =
  { surface with material = { surface.material with glassy = Some index } }
let sphere (s : surface) : obj = Sphere ((0., 0., 0.), 1., s)
let plane (s : surface) : obj = Plane ((0., 1., 0.), 0., s)
let placed (primitive : Solid.primitive) (surface : surface) : obj = Placed { primitive; transform = Transform.identity; surface }
let box = placed Cube
let cylinder = placed Cylinder
let cone = placed Cone
let torus (r : float) = placed (Torus r)

(* claude: a list folded into a tree of pairs; [inter []] and
 * [union []] are refused, as an empty intersection is everything *)
let rec fold (op : Csg.op) (name : string) (objs : obj list) : obj =
  match objs with
  | [] -> invalid_arg ("Povray." ^ name ^ ": no solid")
  | [ o ] -> o
  | o :: rest -> Solid.csg op o (fold op name rest)

let union = fold Union "union"
let inter = fold Inter "inter"
let diff (a : obj) (b : obj) : obj = Solid.csg Diff a b
let move (x : float) (y : float) (z : float) (o : obj) : obj = Solid.move (x, y, z) o
let scale (x : float) (y : float) (z : float) (o : obj) : obj = Solid.transform (Transform.scale (x, y, z)) o

let rotate (x : float) (y : float) (z : float) (o : obj) : obj =
  Solid.transform (Transform.compose (Transform.rotate 2 z) (Transform.compose (Transform.rotate 1 y) (Transform.rotate 0 x))) o

(*****************************************************************************)
(* The lights *)
(*****************************************************************************)

(* claude: data only in phase 1; phase 2 shades with them *)
type light = Sun of int * Vec3.t | Lamp of int * Vec3.t * float | Spot of int * Vec3.t * Vec3.t * float * float

let sun (c : Playground.color) x y z : light = Sun (rgb_of_color c, (x, y, z))
let lamp (c : Playground.color) x y z : light = Lamp (rgb_of_color c, (x, y, z), 0.)

let area_lamp (radius : float) (l : light) : light =
  match l with Lamp (c, p, _) -> Lamp (c, p, radius) | other -> other

let spot ?(falloff = 1.) (c : Playground.color) ~(at : Vec3.t) ~(towards : Vec3.t) (angle : float) : light =
  Spot (rgb_of_color c, at, towards, angle, falloff)

(*****************************************************************************)
(* The scene *)
(*****************************************************************************)

type scene = { camera : camera; lights : light list; ambient : float; sky : int; solids : obj list }

let scene ?(ambient = 0.2) ?(sky = Color.white) ~(camera : camera) (lights : light list) (solids : obj list) : scene =
  { camera; lights; ambient; sky = rgb_of_color sky; solids }

let channels (rgb : int) : Raytrace.rgb =
  let c shift = float_of_int ((rgb lsr shift) land 0xFF) /. 255. in
  (c 16, c 8, c 0)

(* the way's scene, as the ray tracer's *)
let raytrace_scene (scene : scene) : Raytrace.scene =
  let light = function
    (* GML's and POV-Ray's direction is the one the light goes, the ray
     * tracer's the one towards it *)
    | Sun (rgb, along) -> Raytrace.Sun { towards = Vec3.normalize (Vec3.scale (-1.) along); color = channels rgb }
    | Lamp (rgb, position, radius) -> Raytrace.Lamp { position; radius; color = channels rgb }
    | Spot (rgb, position, target, angle, falloff) ->
        Raytrace.Spot { position; aim = Vec3.normalize (Vec3.sub target position); angle; falloff; color = channels rgb }
  in
  { camera = scene.camera; solids = scene.solids; lights = List.map light scene.lights; ambient = scene.ambient;
    background = scene.sky }

(*****************************************************************************)
(* The app *)
(*****************************************************************************)

type model = {
  (* the scene's camera, turned by [orbit]'s mouse *)
  camera : camera;
  (* the algorithm [still] shows, moved by the arrows *)
  algorithm : Raytrace.algorithm;
  (* rays a pixel, n x n: the keys 1 to 4 *)
  samples : int;
  (* the pictures under way or made, by algorithm; each a mutable
   * Raytrace.progress, advanced in place -- the one thing in this
   * model that is not a value, because a picture half made is large
   * and made a few thousand pixels at a time *)
  progresses : (Raytrace.algorithm * Raytrace.progress) list;
  (* the keys held the frame before: a press is its rising edge *)
  keys_before : string Set_.t;
  (* the time spent making the pictures so far, in seconds, and the
   * clock at the last update *)
  elapsed : float;
  clock : float option;
  (* what the last "s" did *)
  said : string;
  (* the pointer the frame before, when the button was down: a drag is
   * the difference (mdx, mdy are for a captured mouse, and a -script's
   * moves do not set them) *)
  dragged_from : (float * float) option;
}

let evolution (computer : Playground.computer) : bool = List.mem_assoc "evolution" computer.flags

(* the algorithm before or after [a], if any *)
let step (a : Raytrace.algorithm) (by : int) : Raytrace.algorithm =
  let all = Raytrace.algorithms in
  let rec index i = function [] -> 0 | x :: rest -> if x = a then i else index (i + 1) rest in
  List.nth all (Int.max 0 (Int.min (List.length all - 1) (index 0 all + by)))

let initial ?(algorithm = Raytrace.default_algorithm) ?(samples = 1) (scene : scene) : model =
  { camera = scene.camera; algorithm; samples; progresses = []; keys_before = Set_.empty; elapsed = 0.;
    clock = None; said = ""; dragged_from = None }

(* all the pictures thrown away, to be made again from the coarsest
 * pass: space, and any move of [orbit]'s camera *)
let restart (model : model) : model = { model with progresses = []; elapsed = 0.; said = "" }

(* [orbit]: dragging turns the camera around its target (left and right
 * around the vertical, up and down towards the pole), the wheel brings
 * it nearer or further *)
let orbit_camera (computer : Playground.computer) (dragged_from : (float * float) option) (camera : camera) :
    camera option =
  let m = computer.mouse in
  let dx, dy = match dragged_from with Some (x, y) when m.mdown -> (m.mx -. x, m.my -. y) | _ -> (0., 0.) in
  let drag = dx <> 0. || dy <> 0. in
  if not (drag || m.mwheel <> 0.) then None
  else
    let x, y, z = Vec3.sub camera.eye camera.target in
    let r = Vec3.length (x, y, z) in
    let yaw = atan2 x z and pitch = asin (y /. r) in
    (* the scene follows the hand: drag right, it turns right *)
    let yaw = yaw -. (dx *. 0.01) in
    let pitch = Float.max (-1.5) (Float.min 1.5 (pitch -. (dy *. 0.01))) in
    let r = r *. (0.9 ** m.mwheel) in
    let offset = (r *. cos pitch *. sin yaw, r *. sin pitch, r *. cos pitch *. cos yaw) in
    Some { camera with eye = Vec3.add camera.target offset }

let update ~(orbit : bool) ?export ~(file : string) ~(rays_per_frame : int) ?size (scene : scene)
    (computer : Playground.computer) (model : model) : model =
  let keys = computer.keyboard.keys in
  let pressed k = Set_.mem k keys && not (Set_.mem k model.keys_before) in
  let model =
    if pressed "ArrowLeft" then { model with algorithm = step model.algorithm (-1) }
    else if pressed "ArrowRight" then { model with algorithm = step model.algorithm 1 }
    else model
  in
  let model = if pressed "space" then restart model else model in
  (* 1 to 4: rays a pixel, n x n, the pictures made again *)
  let model =
    match List.find_opt (fun n -> pressed (string_of_int n)) [ 1; 2; 3; 4 ] with
    | Some n when n <> model.samples -> restart { model with samples = n }
    | _ -> model
  in
  let model =
    match if orbit then orbit_camera computer model.dragged_from model.camera else None with
    | Some camera -> restart { model with camera }
    | None -> model
  in
  (* the pictures wanted, started if not yet *)
  let wanted = if evolution computer then Raytrace.algorithms else [ model.algorithm ] in
  let width, height =
    match size with
    | Some size -> size
    | None -> (int_of_float computer.screen.width, int_of_float computer.screen.height)
  in
  let progresses =
    model.progresses
    @ List.filter_map
        (fun a ->
          if List.mem_assoc a model.progresses then None
          else
            let options = { Raytrace.default_options with algorithm = a; samples = model.samples } in
            Some (a, Raytrace.start ~options (raytrace_scene { scene with camera = model.camera }) ~width ~height))
        wanted
  in
  (* this frame's rays, shared by the pictures wanted and not finished *)
  let working = List.filter (fun (a, p) -> List.mem a wanted && not (Raytrace.finished p)) progresses in
  List.iter (fun (_, p) -> Raytrace.advance p ~rays:(rays_per_frame / List.length working)) working;
  let (Time now) = computer.time in
  (* a frame's time, when it is one: the first update's clock is the
   * Playground's initial 0, the next one the real time, and a gap of
   * more than a second is a window that was not being drawn *)
  let elapsed =
    match model.clock with
    | Some before when working <> [] && now -. before >= 0. && now -. before < 1. -> model.elapsed +. (now -. before)
    | _ -> model.elapsed
  in
  let said =
    match export with
    | Some caps when pressed "s" ->
        let p = List.assoc model.algorithm progresses in
        Playground_platform.export caps file (Png.encode (Raytrace.picture p));
        if Raytrace.finished p then "saved " ^ file else "saved " ^ file ^ ", unfinished"
    | _ -> model.said
  in
  let dragged_from = if computer.mouse.mdown then Some (computer.mouse.mx, computer.mouse.my) else None in
  { model with progresses; keys_before = keys; elapsed; clock = Some now; said; dragged_from }

(* a picture, the largest that fits a w x h box with its caption under
 * it, centred at (x, y) *)
let captioned (x : float) (y : float) (w : float) (h : float) (img : Rgba_image.t) (caption : string) :
    Playground.shape list =
  let caption_h = 30. in
  let iw = float_of_int img.width and ih = float_of_int img.height in
  let scale = 0.95 *. Float.min (w /. iw) ((h -. caption_h) /. ih) in
  (* a thin frame: a picture's white can meet the window's *)
  [ Playground.move x (y +. (caption_h /. 2.))
      (Playground.rectangle (Color.rgb 160 160 160) ((iw *. scale) +. 4.) ((ih *. scale) +. 4.));
    Playground.move x (y +. (caption_h /. 2.)) (Playground.bitmap (iw *. scale) (ih *. scale) img);
    Playground.move x (y +. (caption_h /. 2.) -. (ih *. scale /. 2.) -. (caption_h /. 2.))
      (Playground.words Color.black caption) ]

(* e.g. "pass 4 x 4, 12,000 rays, 0.4 s" *)
let status (model : model) : string =
  let rays = List.fold_left (fun n (_, p) -> n + Raytrace.rays_shot p) 0 model.progresses in
  let done_ = List.for_all (fun (_, p) -> Raytrace.finished p) model.progresses in
  let pass = List.fold_left (fun s (_, p) -> Int.max s (Raytrace.pass p)) 1 model.progresses in
  let rec thousands n = if n < 1000 then string_of_int n else thousands (n / 1000) ^ Printf.sprintf ",%03d" (n mod 1000) in
  let sum f = List.fold_left (fun n (_, p) -> n + f (Raytrace.world_of p)) 0 model.progresses in
  let secondary = sum Raytrace.secondary_rays and saved = sum Raytrace.saved_rays in
  Printf.sprintf "%s, %s rays%s%s, %.1f s%s" (if done_ then "done" else Printf.sprintf "pass %d x %d" pass pass)
    (thousands rays)
    (if secondary + saved = 0 then ""
     else Printf.sprintf " + %s reflected or refracted (%s saved by the cutoff)" (thousands secondary) (thousands saved))
    (if model.samples = 1 then "" else Printf.sprintf " (%d x %d a pixel)" model.samples model.samples)
    model.elapsed (if model.said = "" then "" else " -- " ^ model.said)

let view ~(orbit : bool) ~(export : bool) (computer : Playground.computer) (model : model) : Playground.shape list =
  let screen = computer.screen in
  let numbered a =
    let rec index i = function [] -> 0 | x :: rest -> if x = a then i else index (i + 1) rest in
    Printf.sprintf "%d. %s" (index 1 Raytrace.algorithms) (Raytrace.name a)
  in
  let pictures =
    if evolution computer then
      (* all of them, the oldest first, in a row -- or a grid when they
       * are many *)
      let n = List.length Raytrace.algorithms in
      let cols = if n <= 3 then n else int_of_float (Float.ceil (sqrt (float_of_int n))) in
      let rows = (n + cols - 1) / cols in
      let cw = screen.width /. float_of_int cols and ch = (screen.height -. 60.) /. float_of_int rows in
      List.concat
        (List.mapi
           (fun i a ->
             match List.assoc_opt a model.progresses with
             | None -> []
             | Some p ->
                 let x = screen.left +. (cw *. (float_of_int (i mod cols) +. 0.5)) in
                 let y = screen.top -. (ch *. (float_of_int (i / cols) +. 0.5)) in
                 captioned x y cw ch (Raytrace.picture p) (numbered a))
           Raytrace.algorithms)
    else
      match List.assoc_opt model.algorithm model.progresses with
      | None -> []
      | Some p ->
          let arrows =
            (if model.algorithm = List.hd Raytrace.algorithms then "   " else "<- ")
            ^ numbered model.algorithm
            ^ if model.algorithm = Raytrace.latest then "" else " ->"
          in
          captioned 0. 30. screen.width (screen.height -. 60.) (Raytrace.picture p) arrows
  in
  let keys =
    String.concat "   "
      ((if evolution computer then [] else [ "left/right: the algorithm" ])
      @ [ "space: again"; "1-4: rays a pixel" ]
      @ (if export then [ "s: save" ] else [])
      @ if orbit then [ "drag: turn, wheel: nearer" ] else [])
  in
  pictures
  @ [ Playground.move 0. (screen.bottom +. 45.) (Playground.words Color.black (status model));
      Playground.move 0. (screen.bottom +. 20.) (Playground.words (Color.rgb 120 120 120) keys) ]

let app ~orbit ?export ?(file = "povray.png") ?(rays_per_frame = 20_000) ?size ?algorithm ?samples (scene : scene) =
  Playground.game
    (view ~orbit ~export:(export <> None))
    (update ~orbit ?export ~file ~rays_per_frame ?size scene)
    (initial ?algorithm ?samples scene)

let still ?export ?file ?rays_per_frame ?size ?algorithm ?samples (scene : scene) =
  app ~orbit:false ?export ?file ?rays_per_frame ?size ?algorithm ?samples scene

let orbit ?export ?file ?rays_per_frame ?size ?algorithm ?samples (scene : scene) =
  app ~orbit:true ?export ?file ?rays_per_frame ?size ?algorithm ?samples scene

(*****************************************************************************)
(* A scene in a program of its own *)
(*****************************************************************************)

let pick (scene : scene) ~(width : int) ~(height : int) (x : float) (y : float) : obj option =
  let ray, near, far = Raytrace.camera_ray_through scene.camera ~width ~height x y in
  Option.map snd (Raytrace.nearest ~min_t:near ~max_t:far ray scene.solids)
