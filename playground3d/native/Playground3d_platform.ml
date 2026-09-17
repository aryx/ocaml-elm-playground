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
open Tsdl

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Native backend of the 3D playground: unlike playground/native (Cairo
 * drawing 2D vector paths), this is a real, from-scratch software
 * rasterizer. SDL is used only for the window, the event loop, and
 * presenting the final image (by writing straight into the window
 * surface's own pixel buffer, the same trick playground/native uses,
 * just without Cairo in front of it): the triangle rasterization, the
 * z-buffer depth test, and the perspective projection are all
 * hand-written OCaml below.
 *
 * First version, deliberately simple: single flat color per triangle
 * (alpha/fade3d is not honored here, unlike the web backend -- true
 * alpha blending would need back-to-front ordering, which the z-buffer
 * approach doesn't give us for free), no near-plane clipping (a
 * triangle with any vertex behind the near plane is dropped whole,
 * rather than clipped into visible sub-triangles), linear (not
 * perspective-correct) depth interpolation across a triangle. Good
 * enough for the modest scenes this library targets so far; revisit if
 * needed.
 *)
open Playground
open Playground3d

(*****************************************************************************)
(* Vec3 (duplicated from Playground3d.ml, which keeps it private -- this
 * backend needs its own view/projection transform that keeps per-vertex
 * depth for the z-buffer, unlike Playground3d.project which only
 * returns a 2D point for the web backend's compile-down-to-2D trick) *)
(*****************************************************************************)

type vec3 = float * float * float

let sub ((ax, ay, az) : vec3) ((bx, by, bz) : vec3) : vec3 = (ax -. bx, ay -. by, az -. bz)
let dot ((ax, ay, az) : vec3) ((bx, by, bz) : vec3) : float = (ax *. bx) +. (ay *. by) +. (az *. bz)

let cross ((ax, ay, az) : vec3) ((bx, by, bz) : vec3) : vec3 =
  ((ay *. bz) -. (az *. by), (az *. bx) -. (ax *. bz), (ax *. by) -. (ay *. bx))

let norm (v : vec3) : float = sqrt (dot v v)

let normalize (v : vec3) : vec3 =
  let n = norm v in
  if n = 0. then v else let (x, y, z) = v in (x /. n, y /. n, z /. n)

let degrees_to_radians d = d *. Float.pi /. 180.

let up_hint : vec3 = (0., 1., 0.)

(*****************************************************************************)
(* Colors *)
(*****************************************************************************)

let rgb_of_color (color : Playground.color) : int * int * int =
  match color with
  | Color.Rgb (r, g, b) -> (r, g, b)
  | Color.Hex s ->
      let s = String.lowercase_ascii s in
      let component i = int_of_string ("0x" ^ String.sub s i 2) in
      (component 1, component 3, component 5)

(*****************************************************************************)
(* Projection (with depth, for the z-buffer -- see Playground3d.project
 * for the depth-less 2D version used by the web backend) *)
(*****************************************************************************)

let view_space (camera : Playground3d.camera) (point : vec3) : vec3 =
  let forward = normalize (sub camera.target camera.eye) in
  let right = normalize (cross forward up_hint) in
  let up = cross right forward in
  let relative = sub point camera.eye in
  (dot relative right, dot relative up, dot relative forward)

(* returns (pixel_x, pixel_y, view_z), with (pixel_x, pixel_y) in
 * framebuffer coordinates: origin top-left, y going down (unlike
 * Playground's centered/y-up convention) *)
let project_depth (camera : Playground3d.camera) ~(sx : int) ~(sy : int) (point : vec3) :
    vec3 option =
  let (vx, vy, vz) = view_space camera point in
  if vz <= camera.near || vz >= camera.far then None
  else
    let fsx = float_of_int sx and fsy = float_of_int sy in
    let aspect = fsx /. fsy in
    let f = 1. /. tan (degrees_to_radians camera.fov /. 2.) in
    let ndc_x = f *. vx /. aspect /. vz in
    let ndc_y = f *. vy /. vz in
    Some ((fsx /. 2.) +. (ndc_x *. (fsx /. 2.)), (fsy /. 2.) -. (ndc_y *. (fsy /. 2.)), vz)

(*****************************************************************************)
(* Flatten + backface cull *)
(*****************************************************************************)

let rec flatten_faces (shape : Playground3d.shape3d) : (Playground.color * vec3 list) list =
  match shape.form with
  | Polygon3d (color, points) -> [ (color, points) ]
  | Group3d shapes -> List.concat_map flatten_faces shapes

let face_centroid (points : vec3 list) : vec3 =
  let (sx, sy, sz) =
    List.fold_left (fun (ax, ay, az) (x, y, z) -> (ax +. x, ay +. y, az +. z)) (0., 0., 0.) points
  in
  let n = float_of_int (List.length points) in
  (sx /. n, sy /. n, sz /. n)

let face_normal (points : vec3 list) : vec3 =
  match points with
  | p0 :: p1 :: p2 :: _ -> normalize (cross (sub p1 p0) (sub p2 p0))
  | _ -> failwith "polygon3d needs at least 3 points"

(* fan-triangulate a (convex, e.g. a cube face or a plane) polygon:
 * (p0,p1,p2), (p0,p2,p3), (p0,p3,p4), ... *)
let rec fan_triangles = function
  | p0 :: p1 :: p2 :: rest -> (p0, p1, p2) :: fan_triangles (p0 :: p2 :: rest)
  | _ -> []

(*****************************************************************************)
(* Rasterize a single triangle into the framebuffer + z-buffer *)
(*****************************************************************************)

let rasterize_triangle
    (framebuffer : (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t)
    (zbuffer : float array) ~(sx : int) ~(sy : int) (pixel : int32)
    ((x0, y0, z0) : vec3) ((x1, y1, z1) : vec3) ((x2, y2, z2) : vec3) : unit =
  let min_x = max 0 (int_of_float (Float.round (Stdlib.min x0 (Stdlib.min x1 x2)))) in
  let max_x = min (sx - 1) (int_of_float (Float.round (Stdlib.max x0 (Stdlib.max x1 x2)))) in
  let min_y = max 0 (int_of_float (Float.round (Stdlib.min y0 (Stdlib.min y1 y2)))) in
  let max_y = min (sy - 1) (int_of_float (Float.round (Stdlib.max y0 (Stdlib.max y1 y2)))) in
  let edge (ax, ay) (bx, by) (px, py) = ((bx -. ax) *. (py -. ay)) -. ((by -. ay) *. (px -. ax)) in
  let area = edge (x0, y0) (x1, y1) (x2, y2) in
  if area <> 0. then
    for py = min_y to max_y do
      for px = min_x to max_x do
        let p = (float_of_int px +. 0.5, float_of_int py +. 0.5) in
        let w0 = edge (x1, y1) (x2, y2) p in
        let w1 = edge (x2, y2) (x0, y0) p in
        let w2 = edge (x0, y0) (x1, y1) p in
        let inside =
          if area > 0. then w0 >= 0. && w1 >= 0. && w2 >= 0.
          else w0 <= 0. && w1 <= 0. && w2 <= 0.
        in
        if inside then begin
          let l0 = w0 /. area and l1 = w1 /. area and l2 = w2 /. area in
          let z = (l0 *. z0) +. (l1 *. z1) +. (l2 *. z2) in
          let idx = (py * sx) + px in
          if z < Array.unsafe_get zbuffer idx then begin
            Array.unsafe_set zbuffer idx z;
            Bigarray.Array1.unsafe_set framebuffer idx pixel
          end
        end
      done
    done

(*****************************************************************************)
(* Render one frame *)
(*****************************************************************************)

let g_pixel_format : Sdl.pixel_format option ref = ref None

let get_pixel_format () =
  match !g_pixel_format with
  | None -> failwith "no pixel format (run_app3d not started yet?)"
  | Some pf -> pf

let pixel_of_color (color : Playground.color) : int32 =
  let (r, g, b) = rgb_of_color color in
  Sdl.map_rgb (get_pixel_format ()) r g b

let render_shape3d
    (framebuffer : (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t)
    (zbuffer : float array) ~(sx : int) ~(sy : int) (camera : Playground3d.camera)
    (shape : Playground3d.shape3d) : unit =
  flatten_faces shape
  |> List.iter (fun (color, points) ->
         let normal = face_normal points in
         let centroid = face_centroid points in
         (* backface cull: keep only faces whose (outward, CCW-winding)
          * normal points roughly towards the camera *)
         if dot normal (sub camera.eye centroid) > 0. then begin
           let pixel = pixel_of_color color in
           fan_triangles points
           |> List.iter (fun (pa, pb, pc) ->
                  match
                    ( project_depth camera ~sx ~sy pa,
                      project_depth camera ~sx ~sy pb,
                      project_depth camera ~sx ~sy pc )
                  with
                  | Some v0, Some v1, Some v2 -> rasterize_triangle framebuffer zbuffer ~sx ~sy pixel v0 v1 v2
                  | _ -> (* a vertex is behind the near plane: drop the whole
                          * triangle rather than clip it -- see the module
                          * doc comment above *)
                      ())
         end)

(*****************************************************************************)
(* Computer bookkeeping (keyboard/mouse), duplicated from Playground.ml
 * since it keeps these helpers private -- small enough not to be worth
 * exposing just for this *)
(*****************************************************************************)

let mouse_move mx my (mouse : Playground.mouse) : Playground.mouse = { mouse with mx; my }
let mouse_down mdown (mouse : Playground.mouse) : Playground.mouse = { mouse with mdown }

let update_keyboard (is_down : bool) (key : string) (keyboard : Playground.keyboard) :
    Playground.keyboard =
  let keys = if is_down then Set_.add key keyboard.keys else Set_.remove key keyboard.keys in
  match key with
  | "ArrowUp" -> { keyboard with keys; kup = is_down }
  | "ArrowDown" -> { keyboard with keys; kdown = is_down }
  | "ArrowLeft" -> { keyboard with keys; kleft = is_down }
  | "ArrowRight" -> { keyboard with keys; kright = is_down }
  | "w" -> { keyboard with keys; kw = is_down }
  | "s" -> { keyboard with keys; ks = is_down }
  | "a" -> { keyboard with keys; ka = is_down }
  | "d" -> { keyboard with keys; kd = is_down }
  | "space" -> { keyboard with keys; kspace = is_down }
  | _ -> { keyboard with keys }

let scancode_to_keystring = function
  | "Left" -> "ArrowLeft"
  | "Right" -> "ArrowRight"
  | "Up" -> "ArrowUp"
  | "Down" -> "ArrowDown"
  | "Q" -> exit 0
  | s -> String.lowercase_ascii s

(*****************************************************************************)
(* Run app *)
(*****************************************************************************)

let ( let* ) o f =
  match o with
  | Error (`Msg msg) -> failwith (Printf.sprintf "TSDL error: %s" msg)
  | Ok x -> f x

let run_app3d (app3d : ('model, 'msg) Playground3d.app3d) : unit =
  let sx = int_of_float Playground.default_width in
  let sy = int_of_float Playground.default_height in

  let* () = Sdl.init Sdl.Init.(video + events) in
  let* sdl_window =
    Sdl.create_window ~w:sx ~h:sy "Playground3D (software rasterizer)" Sdl.Window.shown
  in
  let sdl_event = Sdl.Event.create () in
  let* window_surface = Sdl.get_window_surface sdl_window in

  let pixels = Sdl.get_surface_pixels window_surface Bigarray.int32 in
  assert (Bigarray.Array1.dim pixels = sx * sy);

  let* pixel_format = Sdl.alloc_format (Sdl.get_surface_format_enum window_surface) in
  g_pixel_format := Some pixel_format;

  let zbuffer = Array.make (sx * sy) infinity in
  let background_pixel = pixel_of_color Playground.white in

  let model = ref (Playground3d.init3d app3d ()) in
  let computer = ref Playground.initial_computer in

  let target_fps = 60. in
  let target_frame_time = 1. /. target_fps in

  while true do
    let frame_start = Unix.gettimeofday () in

    let rec drain_sdl_events () =
      if Sdl.poll_event (Some sdl_event) then begin
        let event_type = Sdl.Event.get sdl_event Sdl.Event.typ in
        (match event_type with
        | x when x = Sdl.Event.mouse_motion ->
            let mx = Sdl.Event.(get sdl_event mouse_motion_x) in
            let my = Sdl.Event.(get sdl_event mouse_motion_y) in
            let px = float_of_int mx -. (float_of_int sx /. 2.) in
            let py = (float_of_int sy /. 2.) -. float_of_int my in
            computer := { !computer with mouse = mouse_move px py (!computer).mouse }
        | x when x = Sdl.Event.mouse_button_down ->
            computer := { !computer with mouse = mouse_down true (!computer).mouse }
        | x when x = Sdl.Event.mouse_button_up ->
            computer := { !computer with mouse = mouse_down false (!computer).mouse }
        | x when x = Sdl.Event.key_down ->
            let key = Sdl.(get_key_name Event.(get sdl_event keyboard_keycode)) in
            let str = scancode_to_keystring key in
            computer := { !computer with keyboard = update_keyboard true str (!computer).keyboard }
        | x when x = Sdl.Event.key_up ->
            let key = Sdl.(get_key_name Event.(get sdl_event keyboard_keycode)) in
            let str = scancode_to_keystring key in
            computer := { !computer with keyboard = update_keyboard false str (!computer).keyboard }
        | x when x = Sdl.Event.quit -> exit 0
        | _ -> ());
        drain_sdl_events ()
      end
    in
    drain_sdl_events ();

    computer := { !computer with time = Playground.Time (Unix.gettimeofday ()) };
    model := Playground3d.update3d app3d !computer !model;

    let (camera, shapes) = Playground3d.view3d app3d !computer !model in

    Bigarray.Array1.fill pixels background_pixel;
    Array.fill zbuffer 0 (sx * sy) infinity;
    render_shape3d pixels zbuffer ~sx ~sy camera (Playground3d.group3d shapes);

    let elapsed = Unix.gettimeofday () -. frame_start in
    Sdl.set_window_title sdl_window
      (Printf.sprintf "Playground3D -- %dx%d -- %.0f fps" sx sy (1. /. Stdlib.max 0.001 elapsed));
    let* () = Sdl.update_window_surface sdl_window in

    if elapsed < target_frame_time then Unix.sleepf (target_frame_time -. elapsed)
  done
