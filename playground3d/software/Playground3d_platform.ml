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
 * in run_app3d). All 4 modes from docs/claude_notes/notes_3d_shading.md
 * are implemented: flat_color (no lighting at all, every face/texel
 * drawn exactly as given), flat_shading (one brightness value per FACE,
 * from its winding-based normal), gouraud (one brightness value per
 * VERTEX, blended across each triangle), and phong (the vertex NORMALS
 * themselves blended per pixel, brightness computed at every pixel).
 * See make_shader below for where the 4 actually differ. Gouraud/Phong
 * only look any different from flat_shading on a shape built from
 * SmoothPolygon3d faces with genuinely varying per-vertex normals --
 * i.e. a curved shape like sphere; on cube/box/plane (independent flat
 * faces, no shared/varying vertex normals) all 4 modes render
 * identically except for flat_color. *)

type shading = Flat_color | Flat_shading | Gouraud | Phong

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
    | Flat_shading -> Gouraud
    | Gouraud -> Phong
    | Phong -> Flat_color)

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

(* claude: pure -- "how lit is a surface facing this direction",
 * independent of shading_mode. It used to also special-case
 * Flat_color (returning a constant 1. instead of computing this),
 * which conflated "the lighting physics" with "which shading
 * strategy/granularity is active" -- that decision now lives in
 * make_shader below, which is the one place that actually needs to
 * know the mode (whether to call this once per face, once per vertex,
 * or once per pixel with an interpolated normal -- or not call it at
 * all, for Flat_color). *)
let brightness_of_normal (normal : vec3) : float =
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
 * Playground_platform.ml and graphics/images/Image_decode.ml.
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

(* texture sampling: (u, v) = (0, 0) is the image's top-left corner,
 * matching textured_quad's convention; no mipmaps yet *)

(* claude: Playground3d.rendering's smooth_textures (the starting value
 * comes from run_app3d's ?rendering), "i" to toggle at runtime *)
let smooth_textures : bool ref = ref true

(* claude: bilinear filtering, like graphics/core/Blit.sample_bilinear
 * for 2D images: mix the 4 texels whose centers surround (u, v), each
 * weighted by how close it is (see Blit.mli for a picture) *)
let sample_texture_bilinear (img : Stb_image.int8 Stb_image.t) ~(u : float) ~(v : float) : int * int * int =
  let clamp lo hi x = if x < lo then lo else if x > hi then hi else x in
  (* texel i's center is at i + 0.5 *)
  let x = (u *. float_of_int img.width) -. 0.5 and y = (v *. float_of_int img.height) -. 0.5 in
  let i = int_of_float (Float.floor x) and j = int_of_float (Float.floor y) in
  let tx = x -. Float.floor x and ty = y -. Float.floor y in
  let texel i j =
    let i = clamp 0 (img.width - 1) i and j = clamp 0 (img.height - 1) j in
    let idx = img.offset + (j * img.stride) + (i * img.channels) in
    fun k -> float_of_int (Bigarray.Array1.unsafe_get img.data (idx + k))
  in
  let t00 = texel i j and t10 = texel (i + 1) j and t01 = texel i (j + 1) and t11 = texel (i + 1) (j + 1) in
  let channel k =
    let top = (t00 k *. (1. -. tx)) +. (t10 k *. tx) and bottom = (t01 k *. (1. -. tx)) +. (t11 k *. tx) in
    int_of_float ((top *. (1. -. ty)) +. (bottom *. ty) +. 0.5)
  in
  (channel 0, channel 1, channel 2)

(* nearest-neighbor sampling: the texel containing (u, v) *)
let sample_texture_nearest (img : Stb_image.int8 Stb_image.t) ~(u : float) ~(v : float) : int * int * int =
  let clamp01 x = if x < 0. then 0. else if x > 1. then 1. else x in
  let x = min (img.width - 1) (int_of_float (clamp01 u *. float_of_int img.width)) in
  let y = min (img.height - 1) (int_of_float (clamp01 v *. float_of_int img.height)) in
  let idx = img.offset + (y * img.stride) + (x * img.channels) in
  let data = img.data in
  ( Bigarray.Array1.unsafe_get data idx,
    Bigarray.Array1.unsafe_get data (idx + 1),
    Bigarray.Array1.unsafe_get data (idx + 2) )

let sample_texture (img : Stb_image.int8 Stb_image.t) ~(u : float) ~(v : float) : int * int * int =
  if !smooth_textures then sample_texture_bilinear img ~u ~v else sample_texture_nearest img ~u ~v

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
(* A vertex ready for the rasterizer, carrying EVERY version of its
 * depth/texture-coordinate data that either interpolation strategy
 * below needs, computed once here so rasterize_triangle never has to
 * recompute anything, just pick which fields to read:
 *
 *   - vx, vy: where this vertex lands on screen, in pixels. Always
 *     interpolated the ordinary (linear) way -- there's nothing to
 *     debate here, this is just "where is it".
 *   - z: the vertex's plain view-space depth (how far in front of the
 *     camera it is). u, v: the vertex's plain texture coordinates.
 *     These are what you'd naively interpolate across a triangle if
 *     you'd never heard of the problem explained below -- see "Linear"
 *     mode.
 *   - inv_z (= 1/z), u_over_z (= u/z), v_over_z (= v/z): the SAME
 *     depth/texture information, but pre-divided by z. These are what
 *     you interpolate instead if you *have* heard of the problem -- see
 *     "Perspective_correct" mode, and the paragraph below for why.
 *
 * The problem, in short: perspective projection computes screen
 * position by dividing by depth (screen_x is proportional to
 * view_x / view_z), which makes screen position a NONLINEAR function
 * of 3D position. z, u, and v, by contrast, are each defined to vary
 * LINEARLY across the 3D triangle. So interpolating z/u/v linearly
 * using screen-space barycentric weights (the obvious thing to try) is
 * only an approximation: exact at the 3 corners, increasingly wrong
 * towards the interior, and more wrong the more a triangle's depth
 * varies across itself (i.e. the more obliquely/close-up it's viewed).
 * This is visible in practice as a texture's own detail appearing to
 * swim/warp as a shape rotates -- the classic "affine texture mapping"
 * artifact, notorious from the original PlayStation's 3D rendering
 * (which used exactly this shortcut for speed).
 *
 * The fix (a classic graphics result -- see e.g. Heckbert & Moreton,
 * 1991, on perspective texture mapping): unlike z/u/v themselves,
 * 1/z, u/z, and v/z genuinely ARE linear in screen space, so linearly
 * interpolating THEM is exact, not approximate; dividing back out
 * afterwards (see "perspective divide" in make_interpolator below)
 * recovers the true z/u/v at that pixel. *)
type vertex = {
  vx : float;
  vy : float;
  z : float;
  u : float;
  v : float;
  inv_z : float;
  u_over_z : float;
  v_over_z : float;
  normal : vec3;
      (** the vertex's own normal, in world space, untouched by
          projection (a normal is a direction, not a screen position --
          nothing about "where on screen is this" applies to it). Used
          by make_shader below for Gouraud (blended per vertex) and
          Phong (blended per pixel) shading -- see flatten_faces for
          where this comes from: the same winding-based normal repeated
          at every point of a flat Polygon3d/TexturedPolygon3d face, or
          each point's own distinct normal for a SmoothPolygon3d one. *)
}

(* returns None if [point] is at or behind the near plane -- see the
 * module doc comment above about not clipping *)
let project_vertex (camera : Playground3d.camera) ~(sx : int) ~(sy : int)
    ((point, (u, v), normal) : vec3 * (float * float) * vec3) : vertex option =
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
        z = pz;
        u;
        v;
        inv_z;
        u_over_z = u *. inv_z;
        v_over_z = v *. inv_z;
        normal;
      }

(*****************************************************************************)
(* Perspective-correct vs linear interpolation (pluggable: "p" to
 * toggle at runtime, see key_down below) *)
(*****************************************************************************)
(* "Linear" is the naive, WRONG (but simpler-looking, if you don't know
 * why it's wrong) interpolation described in vertex's doc comment
 * above: blend z/u/v directly, the same way vx/vy are blended.
 * "Perspective_correct" is the fix -- blend inv_z/u_over_z/v_over_z
 * instead, then divide back out (the "perspective divide"). Both
 * rasterize_triangle and rasterize_triangle_painters below call
 * make_interpolator once per triangle (not once per pixel -- same
 * "decide once, apply per pixel" shape as fill_of_material's [fill]
 * closure) to get a little function that does whichever of the two is
 * currently selected; from the pixel loop's point of view it's just
 * "call interpolate to turn barycentric weights into a (z, u, v)",
 * with no visible difference between the two modes at that call site. *)

type interpolation = Perspective_correct | Linear

let interpolation_mode : interpolation ref = ref Perspective_correct

let cycle_interpolation_mode () =
  interpolation_mode := (match !interpolation_mode with Perspective_correct -> Linear | Linear -> Perspective_correct)

let make_interpolator (v0 : vertex) (v1 : vertex) (v2 : vertex) :
    l0:float -> l1:float -> l2:float -> float * float * float =
  match !interpolation_mode with
  | Linear ->
      fun ~l0 ~l1 ~l2 ->
        let z = (l0 *. v0.z) +. (l1 *. v1.z) +. (l2 *. v2.z) in
        let u = (l0 *. v0.u) +. (l1 *. v1.u) +. (l2 *. v2.u) in
        let v = (l0 *. v0.v) +. (l1 *. v1.v) +. (l2 *. v2.v) in
        (z, u, v)
  | Perspective_correct ->
      fun ~l0 ~l1 ~l2 ->
        let inv_z = (l0 *. v0.inv_z) +. (l1 *. v1.inv_z) +. (l2 *. v2.inv_z) in
        let u_over_z = (l0 *. v0.u_over_z) +. (l1 *. v1.u_over_z) +. (l2 *. v2.u_over_z) in
        let v_over_z = (l0 *. v0.v_over_z) +. (l1 *. v1.v_over_z) +. (l2 *. v2.v_over_z) in
        (* the "perspective divide": undo the *. inv_z we multiplied by
         * back in project_vertex, now that interpolation is done *)
        (1. /. inv_z, u_over_z /. inv_z, v_over_z /. inv_z)

(*****************************************************************************)
(* Gouraud/Phong: how brightness is computed across a triangle
 * (pluggable via shading_mode above -- "m" to cycle at runtime) *)
(*****************************************************************************)
(* A third "decide once per triangle, apply once per pixel" strategy
 * function, the same shape as fill_of_material (what color) and
 * make_interpolator (how to interpolate depth/UV) -- this one answers
 * "how bright is this pixel", from whichever of the 4 shading_mode
 * strategies is selected:
 *  - Flat_color: no lighting at all, brightness is always 1 (a
 *    constant function, ignoring the weights entirely).
 *  - Flat_shading: one brightness value for the *whole triangle*,
 *    from v0's normal (all 3 vertices share the same normal on a flat
 *    face -- see flatten_faces -- so it doesn't matter which one is
 *    picked).
 *  - Gouraud: brightness computed once per *vertex* (3 calls to
 *    brightness_of_normal, one per vertex's own normal), then those 3
 *    numbers blended per pixel via the same barycentric weights
 *    everything else uses.
 *  - Phong: the vertices' *normals themselves* (not a brightness
 *    number) are blended per pixel first, renormalized (a blend of
 *    unit vectors generally isn't itself unit length), and only then
 *    turned into a brightness -- so, unlike Gouraud, a fresh lighting
 *    calculation happens at every single pixel, not just at the 3
 *    vertices.
 * Gouraud and Phong both interpolate *linearly* here (not
 * perspective-correctly like make_interpolator's u/v/z can) -- see
 * plan_gouraud_phong.md's "Simplifications" for why that's an
 * acceptable simplification for now. *)
let make_shader (v0 : vertex) (v1 : vertex) (v2 : vertex) : l0:float -> l1:float -> l2:float -> float =
  match !shading_mode with
  | Flat_color -> fun ~l0:_ ~l1:_ ~l2:_ -> 1.
  | Flat_shading ->
      let brightness = brightness_of_normal v0.normal in
      fun ~l0:_ ~l1:_ ~l2:_ -> brightness
  | Gouraud ->
      let b0 = brightness_of_normal v0.normal
      and b1 = brightness_of_normal v1.normal
      and b2 = brightness_of_normal v2.normal in
      fun ~l0 ~l1 ~l2 -> (l0 *. b0) +. (l1 *. b1) +. (l2 *. b2)
  | Phong ->
      let (n0x, n0y, n0z) = v0.normal and (n1x, n1y, n1z) = v1.normal and (n2x, n2y, n2z) = v2.normal in
      fun ~l0 ~l1 ~l2 ->
        let nx = (l0 *. n0x) +. (l1 *. n1x) +. (l2 *. n2x)
        and ny = (l0 *. n0y) +. (l1 *. n1y) +. (l2 *. n2y)
        and nz = (l0 *. n0z) +. (l1 *. n1z) +. (l2 *. n2z) in
        brightness_of_normal (normalize (nx, ny, nz))

(*****************************************************************************)
(* Flatten + backface cull *)
(*****************************************************************************)

type material = Flat of Playground.color | Textured of string

(* claude: bugfix: spheres drawn "cut" at the top.
 *
 * The symptom: every sphere was missing its top cap, the ring of faces
 * around the north pole; you could see through it to the white
 * background. The bottom cap was fine, the OpenGL backend was fine,
 * and it didn't depend on the shading mode.
 *
 * The cause: the face normal used to be computed from the polygon's
 * first three points, as the cross product of its first two edges:
 *
 *   normalize (cross (sub p1 p0) (sub p2 p0))
 *
 * That's right for any real triangle, but Playground3d.sphere builds
 * its faces as latitude/longitude quads, and at the pole a "quad" has
 * two corners at the very same point, the pole itself:
 *
 *           p0 = p1 = pole (0, 1, 0)       the top row of faces:
 *                /\                        each quad's first two
 *               /  \                       corners are both the
 *              /    \                      pole, so it's really a
 *            p3------p2                    triangle p0 p2 p3
 *
 * so p1 - p0 = (0, 0, 0), the cross product is (0, 0, 0), and
 * normalizing it divides by its length, 0: the normal is (NaN, NaN,
 * NaN). The bottom row is fine because there the pole comes *last*
 * (p2 = p3), after three distinct points.
 *
 * Why the faces vanished instead of, say, being drawn black: NaN
 * poisons every computation it touches, and every comparison with NaN
 * is false. The backface test, "draw if dot normal (eye - centroid) >
 * 0.", was false for every top face, so they were all culled, silently:
 * no exception, no warning, just missing faces. The web backend had
 * the same bug (same code, in Playground3d.face_normal). The OpenGL
 * backend didn't: the GPU culls each *triangle* by the order its
 * corners appear on screen, clockwise or not, and never computes a
 * face normal.
 *
 * The fix: Newell's method. Each coordinate of the normal is computed
 * from *all* the edges, going around the polygon:
 *
 *   nx = sum over the edges (p, q) of (p.y - q.y) * (p.z + q.z)
 *   ny = sum over the edges (p, q) of (p.z - q.z) * (p.x + q.x)
 *   nz = sum over the edges (p, q) of (p.x - q.x) * (p.y + q.y)
 *
 * Each sum is the shoelace formula (see graphics/2d/Stroke.signed_area):
 * twice the signed area of the polygon's shadow on one of the three
 * coordinate planes (nz: on the xy plane, looking down z), and those
 * three areas together are the plane's direction, scaled by the
 * polygon's area. An edge from a point to the same point adds exactly
 * 0 to every sum, so a repeated pole changes nothing, and a triangle
 * with a repeated corner gets the normal of the triangle it really is.
 * For a counterclockwise triangle it agrees with the cross product,
 * e.g. (0,0,0), (1,0,0), (0,1,0):
 *
 *   nz = (0-1)*(0+0) + (1-0)*(0+1) + (0-0)*(1+0) = 1, so (0, 0, 1)
 *
 * and for a quad that isn't quite flat (4 points need not lie in one
 * plane) it gives a sensible average normal, where the first three
 * points would only see one corner of it.
 *
 * The lesson, i.e. why such a bug survives: the old code is textbook
 * code, and it *looks* obviously right. Its hidden assumption is that
 * the first three points are distinct and not on one line. That's true
 * of every polygon written by hand (a cube's faces, a triangle), which
 * is how it got tested, and false for polygons generated by formulas,
 * where degenerate cases come naturally: a sphere's pole, a cylinder's
 * or cone's tip, or a quad with a corner in the middle of an edge (the
 * first three points in a straight line: their cross product is
 * (0, 0, 0) too). And the failure is silent: no exception, just a few
 * faces not drawn, which in a busy scene is easy to take for "some
 * rendering artifact". The notch was visible in screenshots for a
 * while and was dismissed as exactly that before anyone asked why.
 * Two habits that would have caught it: treat any NaN as a bug to
 * explain (a NaN check in debug mode, or here a failwith on a zero
 * normal), and when a picture looks slightly off, find out why.
 *
 * Found by eye, on a screenshot of examples3d/Spheres3d.exe.
 *
 * Reference: Martin Newell's method, as described in Ivan Sutherland,
 * Robert Sproull, Robert Schumacker, "A Characterization of Ten
 * Hidden-Surface Algorithms", ACM Computing Surveys 6(1):1-55, 1974. *)
let face_normal (points : vec3 list) : vec3 =
  match points with
  | [] | [ _ ] | [ _; _ ] -> failwith "polygon3d needs at least 3 points"
  | first :: rest ->
      (* each point with the next one, the last with the first *)
      let edges = List.combine points (rest @ [ first ]) in
      normalize
        (List.fold_left
           (fun (nx, ny, nz) ((x0, y0, z0), (x1, y1, z1)) ->
             ( nx +. ((y0 -. y1) *. (z0 +. z1)),
               ny +. ((z0 -. z1) *. (x0 +. x1)),
               nz +. ((x0 -. x1) *. (y0 +. y1)) ))
           (0., 0., 0.) edges)

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

let face_centroid (points : vec3 list) : vec3 =
  let (sx, sy, sz) =
    List.fold_left (fun (ax, ay, az) (x, y, z) -> (ax +. x, ay +. y, az +. z)) (0., 0., 0.) points
  in
  let n = float_of_int (List.length points) in
  (sx /. n, sy /. n, sz /. n)

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
    (zbuffer : float array) ~(sx : int) ~(sy : int)
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
          let idx = (py * sx) + px in
          if z < Array.unsafe_get zbuffer idx then begin
            Array.unsafe_set zbuffer idx z;
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
      match Texture_native.load src with
      | Some img -> fun ~u ~v ~brightness -> shade (sample_texture img ~u ~v) ~brightness
      | None ->
          let (r, g, b) = rgb_of_color missing_texture_color in
          fun ~u:_ ~v:_ ~brightness -> shade (r, g, b) ~brightness)

(* claude: pluggable, "b" to toggle at runtime (see key_down below) --
 * off is the simplest possible code (draw every face regardless of
 * which way it points), on is backface culling as described in
 * notes_3d.md section 5.
 *
 * Toggling it live in *filled* mode (render_mode = Filled, the
 * default) shows NO visual difference at all -- only a performance one
 * (watch the fps counter: roughly double the triangles to rasterize
 * for a closed solid like a cube). This isn't a limitation, it's
 * fundamental to what culling does: in filled mode the z-buffer
 * independently decides, per pixel, which triangle is nearest, and for
 * a closed solid that decision always agrees with what culling would
 * have picked anyway (a back face can never win the z-test against the
 * front face covering the same pixels) -- so culling only ever saves
 * work there, it can never change the picture.
 *
 * To actually *see* culling do something, switch to wireframe first
 * ("f"): wireframe has no per-pixel visibility resolution of any kind
 * (see render_mode/draw_triangle_wireframe below), so with culling off
 * you'll see extra edges from each shape's hidden/inside faces that
 * culling normally removes before they're ever drawn -- e.g. on a
 * single cube, the 3 short edges that meet at its far, otherwise
 * entirely hidden corner. *)
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
          let (dx, dy, dz) = sub camera.eye (face_centroid (List.map (fun (p, _uv, _n) -> p) points)) in
          (dx *. dx) +. (dy *. dy) +. (dz *. dz)
        in
        faces |> List.sort (fun (_, pts1) (_, pts2) -> compare (dist2_to_eye pts2) (dist2_to_eye pts1))
  in
  faces
  |> List.iter (fun (material, points) ->
         let bare_points = List.map (fun (p, _uv, _n) -> p) points in
         (* claude: this winding-based normal is ONLY for backface
          * culling (an independent, whole-face notion of "which way
          * does this face point") -- it is unrelated to the per-point
          * normals already carried in [points] (used for shading by
          * make_shader instead, via project_vertex/vertex.normal
          * below). For a Polygon3d/TexturedPolygon3d face these two
          * normals happen to have the same value; for a
          * SmoothPolygon3d (e.g. sphere) they don't, since each of its
          * points has its own, different normal. *)
         let normal = face_normal bare_points in
         let centroid = face_centroid bare_points in
         (* backface cull: keep only faces whose (outward, CCW-winding)
          * normal points roughly towards the camera *)
         if (not !backface_culling_enabled) || dot normal (sub camera.eye centroid) > 0. then begin
           let fill = fill_of_material material in
           let projected =
             fan_triangles points
             |> List.map (fun (pa, pb, pc) ->
                    (project_vertex camera ~sx ~sy pa, project_vertex camera ~sx ~sy pb, project_vertex camera ~sx ~sy pc))
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

let preload_texture = Texture_native.preload

let run_app3d ?(rendering = Playground3d.default_rendering) (app3d : ('model, 'msg) Playground3d.app3d) :
    unit =
  (* claude: the app's choices are the starting values of the modes
   * below; the debug keys can still change them (e.g. "m" also cycles
   * through Gouraud, which the portable hints don't name) *)
  shading_mode :=
    (match rendering.shading with No_lighting -> Flat_color | Flat -> Flat_shading | Smooth -> Phong);
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
  Texture_native.load_queued ();

  let zbuffer = Array.make (sx * sy) infinity in
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
    Array.fill zbuffer 0 (sx * sy) infinity;
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
  Native_loop.run ~sdl_window ~sx ~sy ~title_prefix:"Playground3D" ~on_key_press
    ~init:(Playground3d.init3d app3d) ~update:(Playground3d.update3d app3d) ~view:(Playground3d.view3d app3d)
    ~draw ~present
