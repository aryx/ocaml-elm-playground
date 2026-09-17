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
(* Textures *)
(*****************************************************************************)
(* Real per-pixel texture sampling -- this is the one thing the web
 * backend (see Playground3d.placeholder_texture_color) can't do, since
 * it has no per-pixel access to anything. Loading (a local file path or
 * an http(s) URL, with caching and a preload queue) lives in
 * Texture_native, the same split as playground/native's
 * Playground_platform.ml/Image_native.ml.
 *
 * claude: Texture_native deliberately does NOT force a channel count
 * when calling Stb_image.load -- Stb_image.load ~channels:N with N
 * different from the source's own channel count corrupts the decoded
 * buffer in this project's pinned stb_image version (confirmed by
 * hand: forcing a 3-channel PNG to 4 silently produces a buffer whose
 * *content* is laid out as if 4 channels/pixel while
 * img.channels/img.stride still report 3, which desyncs every
 * sample_texture read after the first pixel). Loading at the image's
 * native channel count (3 for RGB, 4 for RGBA -- both fine, since
 * sample_texture below reads img.channels dynamically) sidesteps the
 * bug entirely; only 1- or 2-channel (grayscale[+alpha]) textures are
 * unsupported as a result, which no real texture image is likely to
 * be. *)

(* a bright, unmistakable "this texture failed to load" color -- the
 * same convention (a magenta/checkerboard placeholder) many game
 * engines use, rather than silently falling back to something that
 * could be mistaken for an intentional color *)
let missing_texture_color = Playground.rgb 255 0 255

(* nearest-neighbor sampling (no filtering/mipmaps yet); (u, v) = (0, 0)
 * is the image's top-left corner, matching textured_quad's convention *)
let sample_texture (img : Stb_image.int8 Stb_image.t) ~(u : float) ~(v : float) : int * int * int =
  let clamp01 x = if x < 0. then 0. else if x > 1. then 1. else x in
  let x = min (img.width - 1) (int_of_float (clamp01 u *. float_of_int img.width)) in
  let y = min (img.height - 1) (int_of_float (clamp01 v *. float_of_int img.height)) in
  let idx = img.offset + (y * img.stride) + (x * img.channels) in
  let data = img.data in
  ( Bigarray.Array1.unsafe_get data idx,
    Bigarray.Array1.unsafe_get data (idx + 1),
    Bigarray.Array1.unsafe_get data (idx + 2) )

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

(* a rasterizer-ready vertex: screen-space (sx, sy), view-space depth
 * (sz, for the z-buffer), and texture coordinates (u, v; unused/(0,0)
 * for flat-colored faces) -- all four are linearly interpolated across
 * a triangle by rasterize_triangle below. *)
type vertex = { vx : float; vy : float; vz : float; vu : float; vv : float }

(* returns None if [point] is at or behind the near plane -- see the
 * module doc comment above about not clipping *)
let project_vertex (camera : Playground3d.camera) ~(sx : int) ~(sy : int)
    ((point, (u, v)) : vec3 * (float * float)) : vertex option =
  let (px, py, pz) = view_space camera point in
  if pz <= camera.near || pz >= camera.far then None
  else
    let fsx = float_of_int sx and fsy = float_of_int sy in
    let aspect = fsx /. fsy in
    let f = 1. /. tan (degrees_to_radians camera.fov /. 2.) in
    let ndc_x = f *. px /. aspect /. pz in
    let ndc_y = f *. py /. pz in
    Some
      { vx = (fsx /. 2.) +. (ndc_x *. (fsx /. 2.));
        vy = (fsy /. 2.) -. (ndc_y *. (fsy /. 2.));
        vz = pz;
        vu = u;
        vv = v;
      }

(*****************************************************************************)
(* Flatten + backface cull *)
(*****************************************************************************)

type material = Flat of Playground.color | Textured of string

let rec flatten_faces (shape : Playground3d.shape3d) :
    (material * (vec3 * (float * float)) list) list =
  match shape.form with
  | Polygon3d (color, points) -> [ (Flat color, List.map (fun p -> (p, (0., 0.))) points) ]
  | TexturedPolygon3d (src, points) -> [ (Textured src, points) ]
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

(* [fill] resolves a pixel's final color from its interpolated (u, v):
 * a constant closure for a flat-colored face, a texture sample for a
 * textured one -- see render_shape3d below. *)
let rasterize_triangle
    (framebuffer : (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t)
    (zbuffer : float array) ~(sx : int) ~(sy : int) ~(fill : u:float -> v:float -> int32)
    (v0 : vertex) (v1 : vertex) (v2 : vertex) : unit =
  let min_x = max 0 (int_of_float (Float.round (Stdlib.min v0.vx (Stdlib.min v1.vx v2.vx)))) in
  let max_x = min (sx - 1) (int_of_float (Float.round (Stdlib.max v0.vx (Stdlib.max v1.vx v2.vx)))) in
  let min_y = max 0 (int_of_float (Float.round (Stdlib.min v0.vy (Stdlib.min v1.vy v2.vy)))) in
  let max_y = min (sy - 1) (int_of_float (Float.round (Stdlib.max v0.vy (Stdlib.max v1.vy v2.vy)))) in
  let edge (ax, ay) (bx, by) (px, py) = ((bx -. ax) *. (py -. ay)) -. ((by -. ay) *. (px -. ax)) in
  let p0 = (v0.vx, v0.vy) and p1 = (v1.vx, v1.vy) and p2 = (v2.vx, v2.vy) in
  let area = edge p0 p1 p2 in
  (* claude: bugfix -- was a strict ">= 0."/"<= 0." test here, which is
   * exactly correct in real-number math but not in floating point, and
   * caused a visible bug: a rectangular face (e.g. one face of a
   * `box`) is always split into 2 triangles sharing a diagonal edge
   * (see fan_triangles), and for a pixel sitting exactly on that
   * shared edge, both triangles compute an edge-function value that is
   * mathematically exactly 0 -- so with a strict ">= 0." test, *both*
   * triangles would consider that pixel "inside" and draw it (harmless
   * double-drawing, not a bug). In practice, floating-point rounding
   * (the two triangles reach that shared edge via different vertex
   * triples, e.g. edge p1-p2 for one triangle vs. edge p0-p2 for
   * dealing with the same physical line, so the arithmetic isn't
   * bit-for-bit identical) can nudge the computed value to something
   * like -1e-10 instead of exactly 0 for *both* triangles at once, at
   * that same pixel -- so *neither* draws it, leaving a 1-pixel-wide
   * gap exactly along the diagonal. This is a well-known rasterizer
   * artifact usually called a "crack" or "T-junction gap". It's
   * angle-dependent (only shows up for the specific projected
   * orientations where rounding happens to tip a shared-edge value
   * across zero), which is why it only appeared "sometimes, when the
   * camera moves" instead of being reliably reproducible on both
   * sides.
   *
   * The fix: nudge the boundary very slightly towards "inside" (an
   * epsilon tolerance) instead of testing against exactly 0, so a
   * shared edge is now *reliably* inside for both triangles even after
   * rounding error, trading a theoretical, invisible sub-pixel amount
   * of double-drawing for the elimination of the gap. (Real GPU
   * rasterizers instead use a "top-left fill rule" -- a tie-breaking
   * convention that assigns each shared-edge pixel to exactly one of
   * the two triangles, so there is neither a gap nor double-drawing at
   * all -- but that's a fair amount of extra bookkeeping for a problem
   * this epsilon already fixes invisibly at our scale.) *)
  let epsilon = 1e-4 in
  if area <> 0. then
    for py = min_y to max_y do
      for px = min_x to max_x do
        let p = (float_of_int px +. 0.5, float_of_int py +. 0.5) in
        let w0 = edge p1 p2 p in
        let w1 = edge p2 p0 p in
        let w2 = edge p0 p1 p in
        let inside =
          if area > 0. then w0 >= -.epsilon && w1 >= -.epsilon && w2 >= -.epsilon
          else w0 <= epsilon && w1 <= epsilon && w2 <= epsilon
        in
        if inside then begin
          let l0 = w0 /. area and l1 = w1 /. area and l2 = w2 /. area in
          let z = (l0 *. v0.vz) +. (l1 *. v1.vz) +. (l2 *. v2.vz) in
          let idx = (py * sx) + px in
          if z < Array.unsafe_get zbuffer idx then begin
            Array.unsafe_set zbuffer idx z;
            let u = (l0 *. v0.vu) +. (l1 *. v1.vu) +. (l2 *. v2.vu) in
            let v = (l0 *. v0.vv) +. (l1 *. v1.vv) +. (l2 *. v2.vv) in
            Bigarray.Array1.unsafe_set framebuffer idx (fill ~u ~v)
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

let pixel_of_rgb (r, g, b) : int32 = Sdl.map_rgb (get_pixel_format ()) r g b
let pixel_of_color (color : Playground.color) : int32 = pixel_of_rgb (rgb_of_color color)

(* the [fill] closure for a material: computed once per face, not once
 * per pixel, except for the actual texture sampling (genuinely
 * per-pixel, since the color varies across the face) *)
let fill_of_material (material : material) : u:float -> v:float -> int32 =
  match material with
  | Flat color ->
      let pixel = pixel_of_color color in
      fun ~u:_ ~v:_ -> pixel
  | Textured src -> (
      match Texture_native.load src with
      | Some img -> fun ~u ~v -> pixel_of_rgb (sample_texture img ~u ~v)
      | None ->
          let pixel = pixel_of_color missing_texture_color in
          fun ~u:_ ~v:_ -> pixel)

let render_shape3d
    (framebuffer : (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t)
    (zbuffer : float array) ~(sx : int) ~(sy : int) (camera : Playground3d.camera)
    (shape : Playground3d.shape3d) : unit =
  flatten_faces shape
  |> List.iter (fun (material, points) ->
         let bare_points = List.map fst points in
         let normal = face_normal bare_points in
         let centroid = face_centroid bare_points in
         (* backface cull: keep only faces whose (outward, CCW-winding)
          * normal points roughly towards the camera *)
         if dot normal (sub camera.eye centroid) > 0. then begin
           let fill = fill_of_material material in
           fan_triangles points
           |> List.iter (fun (pa, pb, pc) ->
                  match
                    (project_vertex camera ~sx ~sy pa, project_vertex camera ~sx ~sy pb, project_vertex camera ~sx ~sy pc)
                  with
                  | Some v0, Some v1, Some v2 -> rasterize_triangle framebuffer zbuffer ~sx ~sy ~fill v0 v1 v2
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

let preload_texture = Texture_native.preload

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

  (* claude: unlike playground/native's run_app, no "Loading..." message
   * first -- this rasterizer has no text-drawing capability at all, so
   * the window just stays whatever the OS shows it as (typically blank)
   * until the first frame is ready. Fine for now (a texture-heavy game
   * should mostly preload_texture everything it needs up front anyway,
   * making this a short, one-time pause), revisit if it's ever
   * noticeable. *)
  Texture_native.load_queued ();

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
