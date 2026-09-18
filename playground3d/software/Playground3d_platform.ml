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
open Playground3d

(*****************************************************************************)
(* Vec3: graphics/3d/geometry/Vec3, under this file's short names *)
(*****************************************************************************)

type vec3 = Vec3.t

let sub = Vec3.sub

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
(* Shading *)
(*****************************************************************************)
(* Pluggable: edit shading_mode's initial value below, or press "m" at
 * runtime to cycle through modes while a game is running (a debug
 * toggle in the same spirit as e.g. Quake's r_drawflat console
 * variable, or a "wireframe view" hotkey -- see the key_down handling
 * in run_app3d). The 4 modes, and where they differ, are in
 * graphics/3d/Shading.mli. *)

(* claude: a ref, not a plain constant, so it can be changed at
 * runtime (see cycle_shading_mode and the "m" key below) -- the same
 * kind of debug toggle many game engines/games expose (e.g. Quake's
 * r_drawflat console variable, or a "wireframe view" hotkey), handy
 * for comparing shading modes side by side without restarting. *)
let shading_mode : Shading.mode ref = ref Shading.Flat_shading

let cycle_shading_mode () =
  shading_mode :=
    (match !shading_mode with
    | Shading.Flat_color -> Shading.Flat_shading
    | Flat_shading -> Gouraud
    | Gouraud -> Phong
    | Phong -> Flat_color)

let scale_channel (c : int) (brightness : float) : int = int_of_float (float_of_int c *. brightness)

(*****************************************************************************)
(* Textures *)
(*****************************************************************************)
(* Real per-pixel texture sampling -- this is the one thing the web
 * backend (see Playground3d.placeholder_texture_color) can't do, since
 * it has no per-pixel access to anything. Loading (a local file path or
 * an http(s) URL, with caching and a preload queue) lives in
 * graphics/images/Texture_decode.ml, the same split as
 * playground/native's Playground_platform.ml and
 * graphics/images/Image_decode.ml.
 *
 * The sampling itself (nearest, bilinear) is graphics/3d/Texture's. *)

(* a bright, unmistakable "this texture failed to load" color -- the
 * same convention (a magenta/checkerboard placeholder) many game
 * engines use, rather than silently falling back to something that
 * could be mistaken for an intentional color *)
let missing_texture_color = Playground.rgb 255 0 255

(* claude: Playground3d.rendering's smooth_textures (the starting value
 * comes from run_app3d's ?rendering), "i" to toggle at runtime *)
let smooth_textures : bool ref = ref true

(* claude: Texture_decode gives RGBA textures, whatever the file's
 * channels (Rgba.of_stb_image; not Stb_image.load ~channels:4, which
 * the pinned binding gets wrong, see Rgba.mli), with no offset and no
 * padding between rows: exactly Texture.image's layout *)
let texture_of_stb_image (img : Stb_image.int8 Stb_image.t) : Texture.image =
  assert (img.channels = 4 && img.offset = 0 && img.stride = img.width * 4);
  { width = img.width; height = img.height; rgba = img.data }

let sample_texture (img : Texture.image) ~(u : float) ~(v : float) : int * int * int =
  if !smooth_textures then Texture.sample_bilinear img ~u ~v else Texture.sample_nearest img ~u ~v

(*****************************************************************************)
(* Projection (with depth, for the z-buffer -- see Playground3d.project
 * for the depth-less 2D version used by the web backend) *)
(*****************************************************************************)

(* claude: the view and perspective steps are graphics/3d/geometry/Camera's *)
let camera_of (camera : Playground3d.camera) : Camera.t =
  { eye = camera.eye; target = camera.target; fov = camera.fov; near = camera.near; far = camera.far }

(* claude: a 3D point to a rasterizer-ready vertex (its pixel, and its
 * depth and texture coordinates in both the forms Interpolate needs) is
 * graphics/3d/Project's *)
type vertex = Project.vertex

(*****************************************************************************)
(* Perspective-correct vs linear interpolation (pluggable: "p" to
 * toggle at runtime, see key_down below) *)
(*****************************************************************************)
(* The two modes, and why the obvious one is wrong, are in
 * graphics/3d/Interpolate.mli. Both rasterize_triangle and
 * rasterize_triangle_painters below call make_interpolator once per
 * triangle (not once per pixel -- same "decide once, apply per pixel"
 * shape as fill_of_material's [fill] closure). *)

let interpolation_mode : Interpolate.mode ref = ref Interpolate.Perspective_correct

let cycle_interpolation_mode () =
  interpolation_mode :=
    (match !interpolation_mode with Interpolate.Perspective_correct -> Interpolate.Linear | Linear -> Perspective_correct)

let make_interpolator (v0 : vertex) (v1 : vertex) (v2 : vertex) = Interpolate.make !interpolation_mode v0 v1 v2

(*****************************************************************************)
(* Gouraud/Phong: how brightness is computed across a triangle
 * (pluggable via shading_mode above -- "m" to cycle at runtime) *)
(*****************************************************************************)
(* A third "decide once per triangle, apply once per pixel" strategy
 * function, the same shape as fill_of_material (what color) and
 * make_interpolator (how to interpolate depth/UV) -- this one answers
 * "how bright is this pixel", see graphics/3d/Shading.mli *)
let make_shader (v0 : vertex) (v1 : vertex) (v2 : vertex) = Shading.make !shading_mode v0 v1 v2

(*****************************************************************************)
(* Flatten + backface cull *)
(*****************************************************************************)

type material = Flat of Playground.color | Textured of string

(* claude: Newell's method, robust to repeated points like a sphere's
 * pole; see Vec3.face_normal for the bug the obvious formula caused
 * (spheres drawn "cut" at the top), explained at length *)
let face_normal = Vec3.face_normal

(* every leaf face's points, tagged with (uv, normal) -- a flat
 * Polygon3d/TexturedPolygon3d face repeats the SAME winding-based
 * face_normal at every one of its points (which is exactly what makes
 * flat_shading uniform across a face: see make_shader), while a
 * SmoothPolygon3d face already has its own distinct normal per point. *)
let rec flatten_faces (shape : Playground3d.shape3d) :
    (material * (vec3 * (float * float) * vec3) list) list =
  match shape.form with
  | Polygon3d (color, points) ->
      let normal = face_normal points in
      [ (Flat color, List.map (fun p -> (p, (0., 0.), normal)) points) ]
  | TexturedPolygon3d (src, points) ->
      let normal = face_normal (List.map fst points) in
      [ (Textured src, List.map (fun (p, uv) -> (p, uv, normal)) points) ]
  | SmoothPolygon3d (color, points) -> [ (Flat color, List.map (fun (p, n) -> (p, (0., 0.), n)) points) ]
  | Hud _ -> [] (* collected separately by Playground3d.collect_hud_shapes, contributes no geometry *)
  | Group3d shapes -> List.concat_map flatten_faces shapes

let face_centroid = Vec3.centroid

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
    (zbuffer : Zbuffer.t) ~(sx : int) ~(sy : int)
    ~(fill : u:float -> v:float -> brightness:float -> int32) (v0 : vertex) (v1 : vertex) (v2 : vertex) : unit =
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
  (* claude: perf -- barycentric coordinates are "w / area" for each of
   * w0/w1/w2 (3 divisions per pixel); computing 1/area once here and
   * multiplying by it instead (inv_area, 1 division total + 3 cheaper
   * multiplications per pixel) is behaviorally identical, just avoids
   * redoing the same division 3 times per pixel. Just an algebraic
   * rewrite of "w /. area" as "w *. (1. /. area)", not a change in what
   * is computed -- feel free to inline it back to "w0 /. area" etc.
   * below if this ever gets in the way of reading the simpler
   * per-pixel math. *)
  let inv_area = 1. /. area in
  (* claude: pluggable, "p" to toggle at runtime -- see
   * make_interpolator's doc comment above for what Linear vs
   * Perspective_correct actually means and why it matters. Computed
   * once per triangle (not once per pixel), like [fill] below. *)
  let interpolate = make_interpolator v0 v1 v2 in
  (* claude: pluggable, "m" to toggle at runtime -- see make_shader's doc
   * comment above. Computed once per triangle, like [interpolate]/[fill]. *)
  let shade_pixel = make_shader v0 v1 v2 in
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
          let l0 = w0 *. inv_area and l1 = w1 *. inv_area and l2 = w2 *. inv_area in
          let (z, u, v) = interpolate ~l0 ~l1 ~l2 in
          if Zbuffer.test_and_set zbuffer ~x:px ~y:py z then begin
            let idx = (py * sx) + px in
            let brightness = shade_pixel ~l0 ~l1 ~l2 in
            Bigarray.Array1.unsafe_set framebuffer idx (fill ~u ~v ~brightness)
          end
        end
      done
    done

(*****************************************************************************)
(* Painter's algorithm (pluggable alternative to the z-buffer above --
 * "z" to toggle at runtime, see visibility_mode below; notes_3d.md
 * section 6 has the full history/trade-off) *)
(*****************************************************************************)
(* The exact same triangle-fill approach as rasterize_triangle above
 * (bounding box + edge functions + perspective-correct u/v), but with
 * NO per-pixel depth test at all: whichever triangle is drawn LAST
 * simply overwrites whatever was there before, unconditionally. This
 * only gives correct results if faces were already sorted back-to-front
 * before calling this (see render_shape3d's depth-sort step below,
 * only performed in this mode) -- and even then, painter's algorithm
 * can't handle intersecting or cyclically-overlapping geometry
 * correctly, since no single global sort order can be right for all of
 * it at once, which is the historical reason the z-buffer approach won
 * out. Deliberately its own separate function (rather than one
 * rasterize_triangle with a runtime branch in the middle) so each
 * approach can be read start to finish on its own. *)
let rasterize_triangle_painters
    (framebuffer : (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t) ~(sx : int)
    ~(sy : int) ~(fill : u:float -> v:float -> brightness:float -> int32) (v0 : vertex) (v1 : vertex)
    (v2 : vertex) : unit =
  let min_x = max 0 (int_of_float (Float.round (Stdlib.min v0.vx (Stdlib.min v1.vx v2.vx)))) in
  let max_x = min (sx - 1) (int_of_float (Float.round (Stdlib.max v0.vx (Stdlib.max v1.vx v2.vx)))) in
  let min_y = max 0 (int_of_float (Float.round (Stdlib.min v0.vy (Stdlib.min v1.vy v2.vy)))) in
  let max_y = min (sy - 1) (int_of_float (Float.round (Stdlib.max v0.vy (Stdlib.max v1.vy v2.vy)))) in
  let edge (ax, ay) (bx, by) (px, py) = ((bx -. ax) *. (py -. ay)) -. ((by -. ay) *. (px -. ax)) in
  let p0 = (v0.vx, v0.vy) and p1 = (v1.vx, v1.vy) and p2 = (v2.vx, v2.vy) in
  let area = edge p0 p1 p2 in
  let epsilon = 1e-4 (* same crack fix as rasterize_triangle, see there *) in
  let inv_area = 1. /. area in
  (* claude: same pluggable interpolation choice as rasterize_triangle
   * above (see make_interpolator's doc comment) -- this function just
   * doesn't need the "z" part of what it returns, since it has no
   * z-buffer to compare against. *)
  let interpolate = make_interpolator v0 v1 v2 in
  let shade_pixel = make_shader v0 v1 v2 in
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
          let l0 = w0 *. inv_area and l1 = w1 *. inv_area and l2 = w2 *. inv_area in
          let (_z, u, v) = interpolate ~l0 ~l1 ~l2 in
          let idx = (py * sx) + px in
          let brightness = shade_pixel ~l0 ~l1 ~l2 in
          Bigarray.Array1.unsafe_set framebuffer idx (fill ~u ~v ~brightness)
        end
      done
    done

type visibility = Z_buffer | Painters_algorithm

(* claude: run examples3d/PaintersAlgorithmFail3d.ml and toggle "z" to
 * actually see the difference this makes -- Cubes3d.ml's grid of
 * separate, same-size, non-overlapping cubes doesn't stress this
 * enough to visibly break under Painters_algorithm (a whole-face
 * centroid-distance sort happens to get the order right almost
 * everywhere for that scene), whereas PaintersAlgorithmFail3d.ml's two
 * genuinely intersecting boxes cannot be correctly ordered by any
 * single per-face decision, so it reliably shows a visible glitch. *)
let visibility_mode : visibility ref = ref Z_buffer

let cycle_visibility_mode () =
  visibility_mode := (match !visibility_mode with Z_buffer -> Painters_algorithm | Painters_algorithm -> Z_buffer)

(*****************************************************************************)
(* Wireframe (pluggable: "f" to toggle at runtime, see render_mode below) *)
(*****************************************************************************)
(* Draw only a triangle's 3 edges, as plain lines, instead of filling
 * its interior -- deliberately much simpler code than rasterize_triangle
 * above (no edge-function/barycentric/z-buffer machinery at all, just
 * walk each of the 3 edges pixel by pixel). This is a basic DDA
 * ("digital differential analyzer") line-drawer: step along whichever
 * axis the line is longer in, one pixel per step, linearly interpolating
 * the other axis -- simpler (if slightly less precise) to read than the
 * classic integer-only Bresenham algorithm, and plenty fast enough for
 * drawing a handful of triangle edges. *)
let draw_line
    (framebuffer : (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t) ~(sx : int)
    ~(sy : int) (pixel : int32) ((x0, y0) : float * float) ((x1, y1) : float * float) : unit =
  let steps = Stdlib.max 1 (int_of_float (Stdlib.max (abs_float (x1 -. x0)) (abs_float (y1 -. y0)))) in
  for i = 0 to steps do
    let t = float_of_int i /. float_of_int steps in
    let px = int_of_float (Float.round (x0 +. (t *. (x1 -. x0)))) in
    let py = int_of_float (Float.round (y0 +. (t *. (y1 -. y0)))) in
    if px >= 0 && px < sx && py >= 0 && py < sy then
      Bigarray.Array1.unsafe_set framebuffer ((py * sx) + px) pixel
  done

let draw_triangle_wireframe
    (framebuffer : (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t) ~(sx : int)
    ~(sy : int) (pixel : int32) (v0 : vertex) (v1 : vertex) (v2 : vertex) : unit =
  let p0 = (v0.vx, v0.vy) and p1 = (v1.vx, v1.vy) and p2 = (v2.vx, v2.vy) in
  draw_line framebuffer ~sx ~sy pixel p0 p1;
  draw_line framebuffer ~sx ~sy pixel p1 p2;
  draw_line framebuffer ~sx ~sy pixel p2 p0

type render_mode = Filled | Wireframe

let render_mode : render_mode ref = ref Filled
let cycle_render_mode () = render_mode := (match !render_mode with Filled -> Wireframe | Wireframe -> Filled)

(*****************************************************************************)
(* Optimization: fast RGB -> pixel packing *)
(*****************************************************************************)
(* Isolated in its own section, separate from the simple "Render one
 * frame" plumbing below, precisely so that section can stay a plain
 * read of what a frame does without this optimization's bookkeeping in
 * the way. pixel_of_color (the only thing the rest of the file calls)
 * behaves identically whether or not the fast path below applies. *)

let g_pixel_format : Sdl.pixel_format option ref = ref None

let get_pixel_format () =
  match !g_pixel_format with
  | None -> failwith "no pixel format (run_app3d not started yet?)"
  | Some pf -> pf

(* The simple, obviously-correct way to turn an (r, g, b) triple into
 * this window's native pixel encoding: just ask SDL. This alone is
 * plenty fast for a *flat*-colored face (pixel_of_color is called once
 * per face, not per pixel), but a *textured* face has a different
 * color at every pixel, so this ends up called once per pixel of every
 * textured triangle -- profiling that (via TexturedCube3d.ml) found it
 * to be the dominant cost of rendering a textured scene: ~17fps versus
 * ~87fps for a same-size flat-colored one (Sdl.map_rgb is a call into
 * the C library, and that per-pixel call overhead adds up fast).
 *
 * pixel_of_rgb below tries a direct bit-shift instead (see
 * g_fast_rgb_shifts), which needs no call into SDL at all -- but it
 * only works for the very common case of a 32-bit-per-pixel format
 * with 8 bits for each of red/green/blue, and would compute outright
 * wrong colors for anything else (e.g. a 16-bit 5-6-5 format, or an
 * indexed/paletted one). So this exact function is kept as the
 * fallback pixel_of_rgb reaches for whenever g_fast_rgb_shifts couldn't
 * confirm the fast path is safe -- i.e. the "slow but always correct"
 * path is a real, reachable branch of the code, not just a comment. *)
let pixel_of_rgb_via_sdl (r, g, b) : int32 = Sdl.map_rgb (get_pixel_format ()) r g b

(* the right bit-shift for each 8-bit channel in this window's pixel
 * format, e.g. { r_shift = 16; g_shift = 8; b_shift = 0 } for the
 * common 0xAARRGGBB layout -- None if the format isn't the simple
 * 32-bit/8-bit-per-channel case this optimization handles, in which
 * case pixel_of_rgb below always falls back to pixel_of_rgb_via_sdl.
 * Computed once in run_app3d, not touched again afterwards. *)
let g_fast_rgb_shifts : (int * int * int) option ref = ref None

(* how many bits are set in a mask, e.g. 0x0000FF00 -> 8 *)
let popcount (mask : int32) : int =
  let rec go mask acc =
    if mask = 0l then acc
    else go (Int32.shift_right_logical mask 1) (acc + Int32.to_int (Int32.logand mask 1l))
  in
  go mask 0

(* how many trailing zero bits a mask has, e.g. 0x0000FF00 -> 8 -- the
 * shift amount needed to move an 8-bit channel value into position *)
let rec trailing_zeros (mask : int32) : int =
  if mask = 0l || Int32.logand mask 1l <> 0l then 0 else 1 + trailing_zeros (Int32.shift_right_logical mask 1)

let fast_rgb_shifts_of_masks ((bpp, rmask, gmask, bmask, _amask) : int * int32 * int32 * int32 * int32) :
    (int * int * int) option =
  if bpp = 32 && popcount rmask = 8 && popcount gmask = 8 && popcount bmask = 8 then
    Some (trailing_zeros rmask, trailing_zeros gmask, trailing_zeros bmask)
  else None

let pixel_of_rgb (r, g, b) : int32 =
  match !g_fast_rgb_shifts with
  | None -> pixel_of_rgb_via_sdl (r, g, b)
  | Some (r_shift, g_shift, b_shift) ->
      Int32.logor
        (Int32.logor (Int32.shift_left (Int32.of_int r) r_shift) (Int32.shift_left (Int32.of_int g) g_shift))
        (Int32.shift_left (Int32.of_int b) b_shift)

let pixel_of_color (color : Playground.color) : int32 = pixel_of_rgb (rgb_of_color color)

(*****************************************************************************)
(* Render one frame *)
(*****************************************************************************)

(* the [fill] closure for a material: computed once per face, not once
 * per pixel, except for the actual texture sampling (genuinely
 * per-pixel, since the color varies across the face) and the
 * [brightness] scaling (genuinely per-pixel too now, for gouraud/phong
 * -- see make_shader above, which is what actually computes it; this
 * function no longer knows or cares which shading mode produced it). *)
let fill_of_material (material : material) : u:float -> v:float -> brightness:float -> int32 =
  let shade (r, g, b) ~brightness : int32 =
    pixel_of_rgb (scale_channel r brightness, scale_channel g brightness, scale_channel b brightness)
  in
  match material with
  | Flat color ->
      let (r, g, b) = rgb_of_color color in
      fun ~u:_ ~v:_ ~brightness -> shade (r, g, b) ~brightness
  | Textured src -> (
      match Texture_decode.load src with
      | Some img ->
          let img = texture_of_stb_image img in
          fun ~u ~v ~brightness -> shade (sample_texture img ~u ~v) ~brightness
      | None ->
          let (r, g, b) = rgb_of_color missing_texture_color in
          fun ~u:_ ~v:_ ~brightness -> shade (r, g, b) ~brightness)

(* claude: pluggable, "b" to toggle at runtime (see key_down below) --
 * off is the simplest possible code (draw every face regardless of
 * which way it points), on is backface culling as described in
 * notes_3d.md section 5 and graphics/3d/Cull.mli. Toggling it live in
 * *filled* mode (render_mode = Filled, the default) shows NO visual
 * difference at all, only a performance one (watch the fps counter);
 * switch to wireframe first ("f") to actually *see* culling do
 * something -- Cull.mli explains why. *)
let backface_culling_enabled = ref true

let render_shape3d
    (framebuffer : (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t)
    (zbuffer : Zbuffer.t) ~(sx : int) ~(sy : int) (camera : Playground3d.camera)
    (shape : Playground3d.shape3d) : unit =
  let view_camera = camera_of camera in
  let faces = flatten_faces shape in
  (* claude: only in Painter's_algorithm mode -- rasterize_triangle_painters
   * has no per-pixel depth test at all, so *draw order* is the only
   * thing that determines what ends up on top; sorting faces
   * farthest-from-the-camera-first here, so nearer faces are drawn
   * later and end up covering farther ones, is what makes that mode
   * look right at all (still not correct for intersecting/cyclically-
   * overlapping geometry -- see that function's doc comment). The
   * z-buffer mode needs no such sort: its per-pixel depth test makes
   * the result correct regardless of draw order. *)
  let faces =
    match !visibility_mode with
    | Z_buffer -> faces
    | Painters_algorithm ->
        let dist2_to_eye points =
          let (dx, dy, dz) = sub camera.eye (face_centroid (List.map (fun (p, _uv, _n) -> p) points)) in
          (dx *. dx) +. (dy *. dy) +. (dz *. dz)
        in
        faces |> List.sort (fun (_, pts1) (_, pts2) -> compare (dist2_to_eye pts2) (dist2_to_eye pts1))
  in
  faces
  |> List.iter (fun (material, points) ->
         let bare_points = List.map (fun (p, _uv, _n) -> p) points in
         if (not !backface_culling_enabled) || Cull.faces_camera ~eye:camera.eye bare_points then begin
           let fill = fill_of_material material in
           let projected =
             fan_triangles points
             |> List.map (fun (pa, pb, pc) ->
                    ( Project.vertex view_camera ~width:sx ~height:sy pa,
                      Project.vertex view_camera ~width:sx ~height:sy pb,
                      Project.vertex view_camera ~width:sx ~height:sy pc ))
           in
           match !render_mode with
           | Wireframe ->
               (* one representative, unlit color for the whole face
                * (sampled at the texture's center for a textured one)
                * -- wireframe mode draws bare edges, not shaded pixels,
                * so brightness is always 1. here regardless of
                * shading_mode. *)
               let pixel = fill ~u:0.5 ~v:0.5 ~brightness:1. in
               projected
               |> List.iter (function
                    | Some v0, Some v1, Some v2 -> draw_triangle_wireframe framebuffer ~sx ~sy pixel v0 v1 v2
                    | _ -> ())
           | Filled ->
               projected
               |> List.iter (function
                    | Some v0, Some v1, Some v2 -> (
                        match !visibility_mode with
                        | Z_buffer -> rasterize_triangle framebuffer zbuffer ~sx ~sy ~fill v0 v1 v2
                        | Painters_algorithm -> rasterize_triangle_painters framebuffer ~sx ~sy ~fill v0 v1 v2)
                    | _ -> (* a vertex is behind the near plane: drop the whole
                            * triangle rather than clip it -- see the module
                            * doc comment above *)
                        ())
         end)

(*****************************************************************************)
(* Run app *)
(*****************************************************************************)
(* The SDL event loop/Playground.computer bookkeeping/frame-pacing itself
 * lives in Native_loop now, shared with a future OpenGL backend (see
 * docs/claude_notes/plan_opengl.md) -- this module only creates the
 * window/pixel buffer and supplies the [draw] callback that turns a
 * (camera, shape3d list) into pixels via the software rasterizer above. *)
open Native_loop

let preload_texture = Texture_decode.preload

let run_app3d ?(rendering = Playground3d.default_rendering) (app3d : ('model, 'msg) Playground3d.app3d) :
    unit =
  (* claude: -v, -debug, and the -fixed-time/-keys/-dump-frame flags (see
   * Native_loop) *)
  Native_loop.parse_cli_and_setup_logging ();
  (* claude: the app's choices are the starting values of the modes
   * below; the debug keys can still change them (e.g. "m" also cycles
   * through Gouraud, which the portable hints don't name) *)
  shading_mode :=
    (match rendering.shading with
    | No_lighting -> Shading.Flat_color
    | Flat -> Shading.Flat_shading
    | Smooth -> Shading.Phong);
  backface_culling_enabled := rendering.backface_culling;
  smooth_textures := rendering.smooth_textures;
  let sx = int_of_float Playground.default_width in
  let sy = int_of_float Playground.default_height in

  let* () = Sdl.init Sdl.Init.(video + events) in
  let* sdl_window =
    Sdl.create_window ~w:sx ~h:sy "Playground3D (software rasterizer)" Sdl.Window.shown
  in
  let* window_surface = Sdl.get_window_surface sdl_window in

  let pixels = Sdl.get_surface_pixels window_surface Bigarray.int32 in
  assert (Bigarray.Array1.dim pixels = sx * sy);
  (* claude: a 2D *view* onto the same underlying memory as [pixels]
   * (Bigarray.reshape shares data, it doesn't copy) -- Cairo.Image.
   * create_for_data32 wants Array2.t, same shape the 2D backend's own
   * Playground_platform.ml reshapes into; [pixels] itself stays the
   * flat Array1.t the rasterizer indexes into directly. Only needed
   * by the HUD pass below, but cheap to build once here regardless. *)
  let pixels_2d = Bigarray.reshape_2 (Bigarray.genarray_of_array1 pixels) sy sx in

  let* pixel_format = Sdl.alloc_format (Sdl.get_surface_format_enum window_surface) in
  g_pixel_format := Some pixel_format;
  let* masks = Sdl.pixel_format_enum_to_masks (Sdl.get_surface_format_enum window_surface) in
  g_fast_rgb_shifts := fast_rgb_shifts_of_masks masks;

  (* claude: unlike playground/native's run_app, no "Loading..." message
   * first -- this rasterizer has no text-drawing capability at all, so
   * the window just stays whatever the OS shows it as (typically blank)
   * until the first frame is ready. Fine for now (a texture-heavy game
   * should mostly preload_texture everything it needs up front anyway,
   * making this a short, one-time pause), revisit if it's ever
   * noticeable. *)
  Texture_decode.load_queued ();

  let zbuffer = Zbuffer.create ~width:sx ~height:sy in
  let background_pixel = pixel_of_color Playground.white in

  (* debug toggles for comparing rendering strategies live, see each
   * one's own doc comment above: "m" shading mode, "b" backface
   * culling, "f" wireframe/filled, "z" painter's algorithm/z-buffer,
   * "p" perspective-correct/linear interpolation, "i" bilinear/nearest
   * texture filtering *)
  let on_key_press str =
    if str = "m" then cycle_shading_mode ();
    if str = "b" then backface_culling_enabled := not !backface_culling_enabled;
    if str = "f" then cycle_render_mode ();
    if str = "z" then cycle_visibility_mode ();
    if str = "p" then cycle_interpolation_mode ();
    if str = "i" then smooth_textures := not !smooth_textures
  in
  let draw (_computer : Playground.computer) ((camera, shapes) : Playground3d.camera * Playground3d.shape3d list)
      : unit =
    Bigarray.Array1.fill pixels background_pixel;
    Zbuffer.clear zbuffer;
    let group = Playground3d.group3d shapes in
    render_shape3d pixels zbuffer ~sx ~sy camera group;
    (* claude: a HUD pass, once the 3D scene above is fully rasterized
     * into [pixels] for this frame -- reuses the exact same trick the
     * 2D backend already uses (Cairo.Image.create_for_data32 pointed
     * directly at an SDL window surface's own pixel Bigarray), just
     * applied as an extra pass on top instead of the only pass. No
     * Cairo.paint/clear here (unlike the 2D backend's per-frame reset)
     * -- this must only add pixels on top, never erase the 3D frame
     * underneath. See docs/claude_notes/done/plan_hud.md. *)
    match Playground3d.collect_hud_shapes group with
    | [] -> ()
    | hud_shapes ->
        let surface = Cairo.Image.create_for_data32 ~w:sx ~h:sy pixels_2d in
        let cr = Cairo.create surface in
        Cairo.identity_matrix cr;
        Cairo.translate cr (float_of_int sx /. 2.) (float_of_int sy /. 2.);
        Shape_render_native.render cr hud_shapes;
        Cairo.Surface.flush surface
  in
  let present () =
    let* () = Sdl.update_window_surface sdl_window in
    ()
  in
  (* claude: -dump-frame (see Native_loop): the frame as a binary PPM
   * image, the simplest image format there is (a header, then r, g, b
   * bytes for each pixel), whatever the window's pixel format *)
  let dump_frame file =
    let oc = open_out_bin file in
    Printf.fprintf oc "P6\n%d %d\n255\n" sx sy;
    for i = 0 to (sx * sy) - 1 do
      let r, g, b = Sdl.get_rgb (get_pixel_format ()) pixels.{i} in
      output_byte oc r;
      output_byte oc g;
      output_byte oc b
    done;
    close_out oc
  in
  Native_loop.run ~sdl_window ~sx ~sy ~title_prefix:"Playground3D" ~on_key_press
    ~init:(Playground3d.init3d app3d) ~update:(Playground3d.update3d app3d) ~view:(Playground3d.view3d app3d)
    ~draw ~present ~dump_frame ()
