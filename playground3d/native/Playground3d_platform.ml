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
(* Shading *)
(*****************************************************************************)
(* Pluggable: edit shading_mode's initial value below, or press "m" at
 * runtime to cycle through modes while a game is running (a debug
 * toggle in the same spirit as e.g. Quake's r_drawflat console
 * variable, or a "wireframe view" hotkey -- see the key_down handling
 * in run_app3d). Only 2 of the 4 modes from
 * docs/claude_notes/notes_3d_shading.md are
 * implemented: flat_color (no lighting at all, every face/texel drawn
 * exactly as given -- what this file always did before) and
 * flat_shading (one brightness value per FACE, from its already-
 * computed normal -- see render_shape3d, which already computes a
 * face's normal for backface culling and just passes it along here
 * too, at no extra cost). Gouraud and Phong both need a normal *per
 * vertex* (Gouraud blends per-vertex lighting across a face; Phong
 * interpolates per-vertex normals and lights every pixel), which our
 * shapes don't have any use for yet: cube/box/plane are made of flat
 * faces whose corners aren't shared with neighboring faces, so a
 * "per-vertex" normal would just equal that face's own flat normal --
 * Gouraud/Phong would render pixel-for-pixel identical to flat_shading
 * until a curved primitive (e.g. a future sphere, tessellated from many
 * small faces with genuinely varying vertex normals) exists to make
 * per-vertex normal blending visible at all. *)

type shading = Flat_color | Flat_shading

(* claude: a ref, not a plain constant, so it can be changed at
 * runtime (see cycle_shading_mode and the "m" key below) -- the same
 * kind of debug toggle many game engines/games expose (e.g. Quake's
 * r_drawflat console variable, or a "wireframe view" hotkey), handy
 * for comparing shading modes side by side without restarting. *)
let shading_mode : shading ref = ref Flat_shading

let cycle_shading_mode () =
  shading_mode :=
    (match !shading_mode with
    | Flat_color -> Flat_shading
    | Flat_shading -> Flat_color)

(* claude: this is a DIRECTIONAL light -- a "sun" -- not a light at a
 * position. There are 3 common kinds of light in 3D graphics, in
 * increasing order of realism/cost:
 *  - directional (what this is): infinitely far away, so its rays are
 *    effectively parallel everywhere in the scene -- there is no
 *    "origin point" to specify, only a *direction* it shines from,
 *    the same for every face regardless of where that face is. The
 *    real sun works this way for all practical purposes (it's ~150
 *    million km away), which is why this is the natural choice for an
 *    outdoor scene. Cheapest to compute: one constant vector, reused
 *    for every face, no per-face distance/attenuation math at all.
 *  - point light: sits at an actual 3D position (e.g. a lightbulb or
 *    torch); the direction *to* it, and therefore how a face is lit,
 *    is different for every face depending on where that face is
 *    relative to the light, and realistically its brightness also
 *    falls off with distance ("attenuation"). More expensive (a
 *    per-face, or per-pixel, direction+distance calculation instead of
 *    one shared constant) and not implemented here.
 *  - spotlight: a point light further restricted to a cone (a
 *    direction plus a cutoff angle) -- even more parameters, also not
 *    implemented here.
 *
 * The vector itself is a DIRECTION, not a position: by convention here
 * it points FROM a lit surface TOWARDS the light (so
 * [dot normal light_dir] below is large/positive exactly when a face's
 * normal points roughly *at* the light, i.e. is well-lit -- see
 * brightness_of_normal). (1., 1.3, 0.6) reads as "the sun sits up and
 * off to the +X/+Z side" -- an arbitrary but reasonable-looking choice,
 * not derived from anything; e.g. changing it to (0., 1., 0.) would put
 * the sun straight overhead instead (top faces bright, sides dimmer,
 * undersides at the `ambient` floor below). Not exposed to the public
 * API yet -- a game can't configure this per scene, only by editing
 * this constant and recompiling. *)
let light_dir : vec3 = normalize (1., 1.3, 0.6)

(* claude: never fully black (a face directly facing away from the
 * light stays at least at ambient brightness) -- a real scene has some
 * ambient/bounced light even on surfaces not directly facing the sun,
 * and a fully-black face would look like a hole rather than a shaded
 * surface *)
let ambient = 0.25

let brightness_of_normal (normal : vec3) : float =
  match !shading_mode with
  | Flat_color -> 1.
  | Flat_shading ->
      let lit = Stdlib.max 0. (dot normal light_dir) in
      ambient +. ((1. -. ambient) *. lit)

let scale_channel (c : int) (brightness : float) : int = int_of_float (float_of_int c *. brightness)

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

(* A rasterizer-ready vertex: screen-space (vx, vy), plus inv_z/
 * u_over_z/v_over_z -- NOT the raw view-space depth and texture
 * coordinates, on purpose. Perspective projection divides by depth
 * (screen_x is proportional to view_x / view_z -- see
 * Playground3d.project's doc comment), which makes it a *nonlinear*
 * function of 3D position; a vertex attribute like z or a texture's
 * (u, v), by contrast, is defined to vary *linearly* across the 3D
 * triangle. Linearly interpolating such an attribute using screen-space
 * barycentric weights (as rasterize_triangle does, the standard/obvious
 * thing to do) is therefore only an approximation -- exact at the 3
 * corners, increasingly wrong towards the interior, and *more* wrong
 * the more a triangle's depth varies across itself (i.e. the more
 * obliquely/close-up it's viewed). This is visible in practice: a
 * texture's own internal detail appears to swim/warp as a shape
 * rotates and its faces' obliqueness keeps changing -- the classic
 * "affine texture mapping" artifact, notorious from the original
 * PlayStation's 3D rendering (which used exactly this shortcut for
 * speed).
 *
 * The standard fix (a classic graphics result -- see e.g. Heckbert &
 * Moreton 1991 on perspective texture mapping): 1/z and (any
 * 3D-linear attribute)/z, unlike z and that attribute individually,
 * genuinely *are* linear in screen space, so linearly interpolating
 * *them* is exact, not approximate. So a vertex here stores 1/z and
 * u/z, v/z instead of z, u, v directly; rasterize_triangle
 * interpolates those (exactly as it would the raw versions -- no
 * change to how interpolation itself works), and only right before
 * using an interpolated value does it divide back out to recover the
 * true z/u/v at that pixel (see "perspective divide" below). *)
type vertex = { vx : float; vy : float; inv_z : float; u_over_z : float; v_over_z : float }

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
    let inv_z = 1. /. pz in
    Some
      { vx = (fsx /. 2.) +. (ndc_x *. (fsx /. 2.));
        vy = (fsy /. 2.) -. (ndc_y *. (fsy /. 2.));
        inv_z;
        u_over_z = u *. inv_z;
        v_over_z = v *. inv_z;
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
          (* claude: perspective-correct interpolation -- see vertex's
           * doc comment above for why inv_z/u_over_z/v_over_z (not z/
           * u/v themselves) are what's safe to interpolate linearly
           * here. inv_z is genuinely linear in screen space, so this
           * interpolation is exact, not an approximation; z (below) is
           * then recovered from it (also exact), same as u and v. *)
          let inv_z = (l0 *. v0.inv_z) +. (l1 *. v1.inv_z) +. (l2 *. v2.inv_z) in
          let z = 1. /. inv_z in
          let idx = (py * sx) + px in
          if z < Array.unsafe_get zbuffer idx then begin
            Array.unsafe_set zbuffer idx z;
            let u_over_z = (l0 *. v0.u_over_z) +. (l1 *. v1.u_over_z) +. (l2 *. v2.u_over_z) in
            let v_over_z = (l0 *. v0.v_over_z) +. (l1 *. v1.v_over_z) +. (l2 *. v2.v_over_z) in
            (* the "perspective divide": undo the /. inv_z we multiplied
             * by back in project_vertex, now that interpolation is done *)
            let u = u_over_z /. inv_z and v = v_over_z /. inv_z in
            Bigarray.Array1.unsafe_set framebuffer idx (fill ~u ~v)
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
    ~(sy : int) ~(fill : u:float -> v:float -> int32) (v0 : vertex) (v1 : vertex) (v2 : vertex) : unit =
  let min_x = max 0 (int_of_float (Float.round (Stdlib.min v0.vx (Stdlib.min v1.vx v2.vx)))) in
  let max_x = min (sx - 1) (int_of_float (Float.round (Stdlib.max v0.vx (Stdlib.max v1.vx v2.vx)))) in
  let min_y = max 0 (int_of_float (Float.round (Stdlib.min v0.vy (Stdlib.min v1.vy v2.vy)))) in
  let max_y = min (sy - 1) (int_of_float (Float.round (Stdlib.max v0.vy (Stdlib.max v1.vy v2.vy)))) in
  let edge (ax, ay) (bx, by) (px, py) = ((bx -. ax) *. (py -. ay)) -. ((by -. ay) *. (px -. ax)) in
  let p0 = (v0.vx, v0.vy) and p1 = (v1.vx, v1.vy) and p2 = (v2.vx, v2.vy) in
  let area = edge p0 p1 p2 in
  let epsilon = 1e-4 (* same crack fix as rasterize_triangle, see there *) in
  let inv_area = 1. /. area in
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
          let inv_z = (l0 *. v0.inv_z) +. (l1 *. v1.inv_z) +. (l2 *. v2.inv_z) in
          let u_over_z = (l0 *. v0.u_over_z) +. (l1 *. v1.u_over_z) +. (l2 *. v2.u_over_z) in
          let v_over_z = (l0 *. v0.v_over_z) +. (l1 *. v1.v_over_z) +. (l2 *. v2.v_over_z) in
          let u = u_over_z /. inv_z and v = v_over_z /. inv_z in
          let idx = (py * sx) + px in
          Bigarray.Array1.unsafe_set framebuffer idx (fill ~u ~v)
        end
      done
    done

type visibility = Z_buffer | Painters_algorithm

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
 * per-pixel, since the color varies across the face). [normal] is the
 * face's normal (already computed by render_shape3d's caller for
 * backface culling, just passed along) -- brightness_of_normal turns
 * it into a single per-face brightness scalar (1.0, i.e. a no-op, in
 * flat_color mode), applied here to whichever color this material
 * would otherwise have produced. *)
let fill_of_material (material : material) (normal : vec3) : u:float -> v:float -> int32 =
  let brightness = brightness_of_normal normal in
  let shade (r, g, b) : int32 = pixel_of_rgb (scale_channel r brightness, scale_channel g brightness, scale_channel b brightness) in
  match material with
  | Flat color ->
      let pixel = shade (rgb_of_color color) in
      fun ~u:_ ~v:_ -> pixel
  | Textured src -> (
      match Texture_native.load src with
      | Some img -> fun ~u ~v -> shade (sample_texture img ~u ~v)
      | None ->
          let pixel = shade (rgb_of_color missing_texture_color) in
          fun ~u:_ ~v:_ -> pixel)

(* claude: pluggable, "b" to toggle at runtime (see key_down below) --
 * off is the simplest possible code (draw every face regardless of
 * which way it points), on is backface culling as described in
 * notes_3d.md section 5. Toggling it live shows both the "x-ray"
 * effect of seeing the inside of solids, and the performance cost of
 * *not* culling (roughly twice the triangles to rasterize for a closed
 * solid like a cube). *)
let backface_culling_enabled = ref true

let render_shape3d
    (framebuffer : (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t)
    (zbuffer : float array) ~(sx : int) ~(sy : int) (camera : Playground3d.camera)
    (shape : Playground3d.shape3d) : unit =
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
          let (dx, dy, dz) = sub camera.eye (face_centroid (List.map fst points)) in
          (dx *. dx) +. (dy *. dy) +. (dz *. dz)
        in
        faces |> List.sort (fun (_, pts1) (_, pts2) -> compare (dist2_to_eye pts2) (dist2_to_eye pts1))
  in
  faces
  |> List.iter (fun (material, points) ->
         let bare_points = List.map fst points in
         let normal = face_normal bare_points in
         let centroid = face_centroid bare_points in
         (* backface cull: keep only faces whose (outward, CCW-winding)
          * normal points roughly towards the camera *)
         if (not !backface_culling_enabled) || dot normal (sub camera.eye centroid) > 0. then begin
           let fill = fill_of_material material normal in
           let projected =
             fan_triangles points
             |> List.map (fun (pa, pb, pc) ->
                    (project_vertex camera ~sx ~sy pa, project_vertex camera ~sx ~sy pb, project_vertex camera ~sx ~sy pc))
           in
           match !render_mode with
           | Wireframe ->
               (* one representative color for the whole face (sampled
                * at the texture's center for a textured one), same
                * idea as sampling anywhere else -- wireframe mode
                * doesn't need a different color per pixel *)
               let pixel = fill ~u:0.5 ~v:0.5 in
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
  let* masks = Sdl.pixel_format_enum_to_masks (Sdl.get_surface_format_enum window_surface) in
  g_fast_rgb_shifts := fast_rgb_shifts_of_masks masks;

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
            (* claude: one-shot actions on key-down (not tied to
             * computer.keyboard's held-key state, which update3d has
             * no reason to know about) -- debug toggles for comparing
             * rendering strategies live, see each one's own doc
             * comment above: "m" shading mode, "b" backface culling,
             * "f" wireframe/filled, "z" painter's algorithm/z-buffer *)
            if str = "m" then cycle_shading_mode ();
            if str = "b" then backface_culling_enabled := not !backface_culling_enabled;
            if str = "f" then cycle_render_mode ();
            if str = "z" then cycle_visibility_mode ();
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
