(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
open Js_of_ocaml

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* The fourth backend of the 3D playground: to the web (SVG) backend
 * what the OpenGL one is to the software rasterizer, i.e. the same
 * shape3d/camera scenes handed to a real GPU, here through WebGL 1
 * (js_of_ocaml's WebGL module). See docs/claude_notes/done/plan_webgl.md.
 *
 * No event loop of its own: like the web backend, run_app3d builds an
 * ordinary Playground.game and gives it to elm_playground_web's
 * run_app, which owns the requestAnimationFrame loop, the Ticks, the
 * key/mouse listeners, and the Playground.computer bookkeeping. The
 * twist is in view2d: it draws the 3D scene into a WebGL <canvas> as a
 * side effect, and returns only the HUD shapes, which run_app draws as
 * usual in its <svg>, on top of the canvas:
 *
 *    +----------------------------+  <svg>, position fixed, 100%x100%,
 *    |  Score: 3                  |  no background: transparent except
 *    |                            |  for the HUD shapes; gets the mouse
 *    |   +------------------------+-+
 *    |   |                          |  <canvas>, position fixed,
 *    +---|       WebGL scene        |  100%x100%, z-index -1: under
 *        |                          |  the <svg>
 *        +--------------------------+
 *
 * This works because run_app calls app.view exactly once per frame,
 * after the Ticks of that frame.
 *
 * The drawing itself is the OpenGL backend's, in WebGL 1's dialect:
 * the same Gpu_scene vertex data (one draw call per material), the
 * same Mat4 camera matrices, the same lighting formula in the fragment
 * shader, the same rendering hints and debug keys. Textures are loaded
 * by the browser (see the Textures section). The native backends'
 * command-line flags are URL parameters here (see Page parameters). *)

(*****************************************************************************)
(* Shaders *)
(*****************************************************************************)
(* GLSL ES 1.00, WebGL 1's, rather than OpenGL's GLSL 3.30: attribute/
 * varying instead of in/out, gl_FragColor instead of an out variable,
 * texture2D instead of texture, and a float precision the fragment
 * shader must choose itself (the vertex shader has highp by default).
 * Otherwise the same two programs as the OpenGL backend's; see its
 * comments for the lighting and the flat-shading trick. *)

let vertex_shader_source =
  "attribute vec3 aPos;\n\
   attribute vec3 aNormal;\n\
   attribute vec3 aColor;\n\
   attribute vec2 aUv;\n\
   varying vec3 vNormal;\n\
   varying vec3 vColor;\n\
   varying vec2 vUv;\n\
   varying vec3 vPos;\n\
   uniform mat4 uMVP;\n\
   void main() {\n\
  \  gl_Position = uMVP * vec4(aPos, 1.0);\n\
  \  vPos = aPos;\n\
  \  vNormal = aNormal;\n\
  \  vColor = aColor;\n\
  \  vUv = aUv;\n\
   }\n"

(* [derivatives]: whether the OES_standard_derivatives extension is
 * there. Flat shading needs its dFdx/dFdy (core in OpenGL's GLSL 3.30,
 * an extension in WebGL 1); without it, Flat falls back to the vertex
 * normals, i.e. to Smooth, which only differs on curved shapes.
 *
 * highp when the GPU has it (nearly all do): mediump is only required
 * to have 10 bits of mantissa, too coarse for dFdx of positions.
 *
 * uAmbient is a uniform rather than a literal, so Lighting.ambient is
 * the only place the value is written. *)
let fragment_shader_source ~(derivatives : bool) : string =
  let flat_normal = if derivatives then "normalize(cross(dFdx(vPos), dFdy(vPos)))" else "normalize(vNormal)" in
  (if derivatives then "#extension GL_OES_standard_derivatives : enable\n" else "")
  ^ "#ifdef GL_FRAGMENT_PRECISION_HIGH\n\
     precision highp float;\n\
     #else\n\
     precision mediump float;\n\
     #endif\n\
     varying vec3 vNormal;\n\
     varying vec3 vColor;\n\
     varying vec2 vUv;\n\
     varying vec3 vPos;\n\
     uniform vec3 uLightDir;\n\
     uniform float uAmbient;\n\
     uniform bool uUseTexture;\n\
     uniform sampler2D uTexture;\n\
     uniform int uShading;\n\
     void main() {\n\
    \  vec3 n = uShading == 1 ? "
  ^ flat_normal
  ^ " : normalize(vNormal);\n\
    \  float lit = max(dot(n, uLightDir), 0.0);\n\
    \  float brightness = uShading == 0 ? 1.0 : uAmbient + (1.0 - uAmbient) * lit;\n\
    \  vec3 baseColor = uUseTexture ? texture2D(uTexture, vUv).rgb : vColor;\n\
    \  gl_FragColor = vec4(baseColor * brightness, 1.0);\n\
     }\n"

(* the fragment shader's uShading *)
let shading_code (s : Playground3d.shading) : int = match s with No_lighting -> 0 | Flat -> 1 | Smooth -> 2

(*****************************************************************************)
(* Page parameters *)
(*****************************************************************************)
(* A page has no command line: the native backends' flags become
 * parameters of its URL, e.g. Cubes3d.html?debug-keys&fixed-time=1000:
 *  - debug-keys: like -debug-keys, the keys of the next section;
 *  - keys=k: like -keys k, the debug keys k pressed before the first
 *    frame (e.g. keys=mb: shading and culling toggled; implies
 *    debug-keys), e.g. for a screenshot in headless Chrome, which can't
 *    press keys;
 *  - fixed-time=t: like -fixed-time t, every frame at the time t (in
 *    seconds), so a frame is deterministic, e.g. to compare it with the
 *    software rasterizer's golden frame (tests/3d/golden/, made at
 *    1000x1000 with -fixed-time 1000: open the page in a 1000x1000
 *    window).
 * The URL is read by elm_playground_web's Playground_platform.flags, the
 * same function that gives the app its flags. *)

(*****************************************************************************)
(* Debug keys *)
(*****************************************************************************)
(* With ?debug-keys, the OpenGL backend's keys: "m" shading, "b"
 * backface culling, "i" texture filtering, "f" wireframe, "o" the mesh
 * cache (see Meshes). Off by default, like on native, so a game can use
 * any key. Their starting values come from run_app3d's ?rendering, and
 * the page's title shows their current state, as the native window's
 * title does. *)

let shading : Playground3d.shading ref = ref Playground3d.Smooth
let backface_culling = ref true
let smooth_textures = ref true
let wireframe = ref false
let use_cache = ref true

let current_rendering () : Playground3d.rendering =
  { shading = !shading; backface_culling = !backface_culling; smooth_textures = !smooth_textures }

let show_state_in_title () : unit =
  let on_off b = if b then "on" else "off" in
  let shading_name = match !shading with No_lighting -> "no lighting" | Flat -> "flat" | Smooth -> "smooth" in
  Dom_html.document##.title :=
    Js.string
      (Printf.sprintf "Playground3D (WebGL) -- m: %s, b: culling %s, i: smooth textures %s, f: wireframe %s, o: cache %s"
         shading_name (on_off !backface_culling) (on_off !smooth_textures) (on_off !wireframe) (on_off !use_cache))

let on_key_press (key : string) : unit =
  (match key with
  | "m" -> shading := (match !shading with No_lighting -> Flat | Flat -> Smooth | Smooth -> No_lighting)
  | "b" -> backface_culling := not !backface_culling
  | "i" -> smooth_textures := not !smooth_textures
  | "f" -> wireframe := not !wireframe
  | "o" -> use_cache := not !use_cache
  | _ -> ());
  show_state_in_title ()

(* our own listener on window, next to elm_playground_web's (which still
 * gives every key to the app too, as on native) *)
let listen_to_debug_keys () : unit =
  Dom_html.addEventListener Dom_html.window Dom_html.Event.keydown
    (Dom_html.handler (fun (evt : Dom_html.keyboardEvent Js.t) ->
         (* a key held down repeats its keydown: one toggle per press
          * (not in js_of_ocaml's keyboardEvent, hence Js.Unsafe) *)
         let repeat = Js.to_bool (Js.Unsafe.get evt "repeat") in
         (match Js.Optdef.to_option evt##.key with
         | Some key when not repeat -> on_key_press (Js.to_string key)
         | _ -> ());
         Js._true))
    Js._false
  |> ignore

(* Like OpenGL, WebGL reports a shader compile or link error only through
 * a status to check and a log to fetch: without these checks, a GLSL
 * typo is a silently blank canvas. *)
let compile_shader (gl : WebGL.renderingContext Js.t) (kind : WebGL.shaderType) (source : string) :
    WebGL.shader Js.t =
  let shader = gl##createShader kind in
  gl##shaderSource shader (Js.string source);
  gl##compileShader shader;
  if not (Js.to_bool (gl##getShaderParameter shader gl##._COMPILE_STATUS_)) then
    failwith (Printf.sprintf "WebGL shader compile error:\n%s" (Js.to_string (gl##getShaderInfoLog shader)));
  shader

let link_program (gl : WebGL.renderingContext Js.t) ~(vertex_source : string) ~(fragment_source : string) :
    WebGL.program Js.t =
  let vs = compile_shader gl gl##._VERTEX_SHADER_ vertex_source in
  let fs = compile_shader gl gl##._FRAGMENT_SHADER_ fragment_source in
  let program = gl##createProgram in
  gl##attachShader program vs;
  gl##attachShader program fs;
  gl##linkProgram program;
  if not (Js.to_bool (gl##getProgramParameter program gl##._LINK_STATUS_)) then
    failwith (Printf.sprintf "WebGL program link error:\n%s" (Js.to_string (gl##getProgramInfoLog program)));
  gl##deleteShader vs;
  gl##deleteShader fs;
  program

(*****************************************************************************)
(* The canvas *)
(*****************************************************************************)

let create_canvas () : Dom_html.canvasElement Js.t =
  let canvas = Dom_html.createCanvas Dom_html.document in
  let style = canvas##.style in
  style##.position := Js.string "fixed";
  style##.top := Js.string "0";
  style##.left := Js.string "0";
  style##.width := Js.string "100%";
  style##.height := Js.string "100%";
  (* under run_app's <svg>, whatever their order in the page: a
   * positioned element with a negative z-index is painted before
   * (under) the ones with z-index auto, like the <svg> *)
  style##.zIndex := Js.string "-1";
  canvas

(* run_app empties <body> before inserting its <svg> on the first
 * frame, i.e. just after our first draw: put the canvas (or the
 * no-WebGL message) back when it has been removed (a no-op on every
 * other frame) *)
let ensure_in_page (elt : #Dom.node Js.t) : unit =
  if not (Js.Opt.test elt##.parentNode) then Dom.appendChild Dom_html.document##.body elt

(* instead of a blank page when the browser has no WebGL (too old, or
 * turned off) *)
let no_webgl_message : Dom_html.paragraphElement Js.t Lazy.t =
  lazy
    (let p = Dom_html.createP Dom_html.document in
     p##.textContent :=
       Js.some
         (Js.string
            "This page needs WebGL, which this browser doesn't provide (or has turned off). The examples also \
             have an SVG version, which doesn't need it.");
     let style = p##.style in
     style##.position := Js.string "fixed";
     style##.top := Js.string "40%";
     style##.width := Js.string "100%";
     style##.textAlign := Js.string "center";
     style##.fontFamily := Js.string "sans-serif";
     p)

(* The canvas has two sizes: its size on the page (clientWidth/Height,
 * 100% of the window, in CSS pixels, see create_canvas) and the size
 * of its drawing buffer (canvas##.width/height, in real pixels), which
 * we keep equal to the former times devicePixelRatio, for a sharp
 * picture on a HiDPI screen; if they differed, the browser would
 * stretch the picture to the page size, distorting it. Checked every
 * frame, to follow the window's resizes. *)
let resize_to_window (canvas : Dom_html.canvasElement Js.t) : int * int =
  (* claude: Js.to_float, not the number as is: js_of_ocaml's
   * Js.number_t is an abstract Javascript number (js_of_ocaml >= 6),
   * not an OCaml float. *)
  let dpr = Js.to_float Dom_html.window##.devicePixelRatio in
  let w = int_of_float (float_of_int canvas##.clientWidth *. dpr) in
  let h = int_of_float (float_of_int canvas##.clientHeight *. dpr) in
  if canvas##.width <> w then canvas##.width := w;
  if canvas##.height <> h then canvas##.height := h;
  (w, h)

(* The part of the canvas where the scene goes: the same centered,
 * aspect-preserving rectangle as the one where run_app's <svg> shows
 * its viewBox (the default preserveAspectRatio, "xMidYMid meet"), so
 * the scene and the HUD line up.
 *
 *    canvas_w
 *   +---------+-------------------+---------+
 *   |         |                   |         |
 *   | x       |  screen.width *   |         | canvas_h
 *   |<------->|  scale            |         |
 *   |         |                   |         |
 *   +---------+-------------------+---------+
 *
 * scale is the largest one where the screen fits, so the margins are
 * either left and right (as drawn) or at the top and bottom. *)
let letterbox ~(canvas_w : int) ~(canvas_h : int) (screen : Playground.screen) : int * int * int * int =
  let scale = Float.min (float_of_int canvas_w /. screen.width) (float_of_int canvas_h /. screen.height) in
  let w = int_of_float (screen.width *. scale) in
  let h = int_of_float (screen.height *. scale) in
  ((canvas_w - w) / 2, (canvas_h - h) / 2, w, h)

(*****************************************************************************)
(* Textures *)
(*****************************************************************************)
(* The other backends decode their textures themselves
 * (graphics/images/Texture_decode, stb_image, blocking); here the
 * browser does it, from an <img>, which also means **asynchronously**:
 * setting img.src starts the download and returns at once. So a
 * texture goes through two steps, in two caches:
 *
 *  - [images]: src -> its <img>, created on first request (by
 *    preload_texture, or the first frame that draws the texture),
 *    before any GL context exists, so a preload can start early;
 *  - [gl_state.textures]: src -> its GL texture, created on first draw
 *    with the other backends' 1x1 magenta "missing texture" pixel, and
 *    replaced by the image, once, on the first frame after the image
 *    is complete. Until then (or forever, if the image can't be
 *    loaded) the faces are magenta.
 *
 * Checking [complete] every frame, rather than uploading from the
 * image's onload handler, keeps all GL calls inside [draw]: no GL
 * context yet when a preload's image arrives is then not a case to
 * handle. *)

let images : (string, Dom_html.imageElement Js.t) Hashtbl.t = Hashtbl.create 8

let is_http_url (src : string) : bool =
  String.starts_with ~prefix:"http://" src || String.starts_with ~prefix:"https://" src

(* the <img> of [src], its download started if this is the first request *)
let image_of (src : string) : Dom_html.imageElement Js.t =
  match Hashtbl.find_opt images src with
  | Some img -> img
  | None ->
      let img = Dom_html.createImg Dom_html.document in
      (* WebGL refuses to read the pixels of an image from another site
       * unless that site allows it (CORS), and the request must ask for
       * it; not for a relative path, where the attribute would instead
       * break loading from a file:// page. (Not in js_of_ocaml's
       * imageElement, hence Js.Unsafe.) *)
      if is_http_url src then Js.Unsafe.set img "crossOrigin" (Js.string "anonymous");
      (* claude: a texture the program carries with it
       * (Playground3d.embedded_texture) is already base64: a "data:"
       * URL is exactly that, and the browser decodes it itself, with no
       * file to find and no request to make *)
      let url = match Playground3d.embedded src with Some base64 -> "data:image/png;base64," ^ base64 | None -> src in
      img##.src := Js.string url;
      Hashtbl.replace images src img;
      img

type texture = { tex : WebGL.texture Js.t; mutable uploaded : bool }

let magenta_pixel () = new%js Typed_array.uint8Array_fromArray (Js.array [| 255; 0; 255; 255 |])

let create_texture (gl : WebGL.renderingContext Js.t) : WebGL.texture Js.t =
  let tex = gl##createTexture in
  gl##bindTexture gl##._TEXTURE_2D_ tex;
  gl##texImage2D_fromView gl##._TEXTURE_2D_ 0 gl##._RGBA 1 1 0 gl##._RGBA gl##._UNSIGNED_BYTE_ (magenta_pixel ());
  (* WebGL 1 can only sample a texture whose size isn't a power of 2
   * (e.g. a 100x60 image) with this wrap mode and no mipmaps (the
   * filters set in draw_group don't use any) *)
  gl##texParameteri gl##._TEXTURE_2D_ gl##._TEXTURE_WRAP_S_ gl##._CLAMP_TO_EDGE_;
  gl##texParameteri gl##._TEXTURE_2D_ gl##._TEXTURE_WRAP_T_ gl##._CLAMP_TO_EDGE_;
  tex

(* No v-flip: from an <img>, texImage2D puts the image's top row at
 * v = 0 (UNPACK_FLIP_Y_WEBGL is false by default), the convention of
 * the playground's UVs (see Playground3d.textured_quad), like the
 * OpenGL backend's upload of stb_image's rows.
 *
 * A complete image with no width failed to load (e.g. a 404): it stays
 * magenta. texImage2D itself can fail too, with a SecurityError, on an
 * image the browser considers from another origin, which includes any
 * image of a page opened as file:// in Chrome: then too, magenta, and
 * a message in the console (serve the page over http instead, e.g.
 * with 'make serve-build'). Either way the texture is marked uploaded, so each
 * problem is reported once, not every frame. *)
let try_upload (gl : WebGL.renderingContext Js.t) (src : string) (t : texture) : unit =
  let img = image_of src in
  (* claude: a plain int, not an optdef, since js_of_ocaml 6 *)
  let loaded = img##.naturalWidth > 0 in
  if Js.to_bool img##.complete then begin
    t.uploaded <- true;
    if loaded then begin
      gl##bindTexture gl##._TEXTURE_2D_ t.tex;
      try gl##texImage2D_fromImage gl##._TEXTURE_2D_ 0 gl##._RGBA gl##._RGBA gl##._UNSIGNED_BYTE_ img
      with exn ->
        Console.console##warn
          (Js.string
             (Printf.sprintf "playground3d webgl: can't use texture %s (%s)" src (Printexc.to_string exn)))
    end
    else Console.console##warn (Js.string (Printf.sprintf "playground3d webgl: can't load texture %s" src))
  end

(*****************************************************************************)
(* GL state *)
(*****************************************************************************)

(* a cached3d's GPU buffers: per material, a buffer and its vertex count
 * (see Meshes below) *)
type mesh = (Gpu_scene.material * WebGL.buffer Js.t * int) list

type gl_state = {
  canvas : Dom_html.canvasElement Js.t;
  gl : WebGL.renderingContext Js.t;
  program : WebGL.program Js.t;
  buffer : WebGL.buffer Js.t;
  (* each vertex attribute's location, size and offset, in floats (see
   * set_attribute_pointers) *)
  attributes : (int * int * int) list;
  meshes : mesh Mesh_cache.t;
  mvp_location : [ `mat4 ] WebGL.uniformLocation Js.t;
  shading_location : int WebGL.uniformLocation Js.t;
  use_texture_location : int WebGL.uniformLocation Js.t;
  textures : (string, texture) Hashtbl.t;
}

(* the GL texture of [src], magenta until its image is there *)
let gl_texture (st : gl_state) (src : string) : WebGL.texture Js.t =
  let t =
    match Hashtbl.find_opt st.textures src with
    | Some t -> t
    | None ->
        let t = { tex = create_texture st.gl; uploaded = false } in
        Hashtbl.replace st.textures src t;
        t
  in
  if not t.uploaded then try_upload st.gl src t;
  t.tex

let float32_array (data : float array) : Typed_array.float32Array Js.t =
  new%js Typed_array.float32Array_fromArray (Js.array data)

(* The attribute layout of Gpu_scene.vertex_floats_of_group: position
 * (3 floats), normal (3), color (3), uv (2), interleaved, for the buffer
 * currently bound to ARRAY_BUFFER. Set again each time another buffer
 * is drawn (the scene's, or a cached3d's, see Meshes): WebGL 1 has no
 * VAOs to remember it per buffer, unlike the OpenGL backend. *)
let set_attribute_pointers (gl : WebGL.renderingContext Js.t) (attributes : (int * int * int) list) : unit =
  let stride = Gpu_scene.floats_per_vertex * 4 (* bytes per float *) in
  attributes
  |> List.iter (fun (loc, size, offset) ->
         gl##vertexAttribPointer loc size gl##._FLOAT Js._false stride (offset * 4);
         gl##enableVertexAttribArray loc)

(* Error when the browser has no WebGL; a shader that doesn't compile is
 * our bug, not the browser's, and still a failwith *)
let init_gl () : (gl_state, string) result =
  let canvas = create_canvas () in
  let attrs = WebGL.defaultContextAttributes in
  (* the default, but the z-buffer is the point of this backend *)
  attrs##.depth := Js._true;
  match Js.Opt.to_option (WebGL.getContextWithAttributes canvas attrs) with
  | None -> Error "WebGL is not available in this browser"
  | Some gl ->
  (* getExtension both tells whether the extension is there and turns
   * it on *)
  let derivatives = Js.Opt.test (gl##getExtension (Js.string "OES_standard_derivatives")) in
  let program =
    link_program gl ~vertex_source:vertex_shader_source ~fragment_source:(fragment_shader_source ~derivatives)
  in
  gl##useProgram program;
  let uniform name = gl##getUniformLocation program (Js.string name) in
  let (lx, ly, lz) = Gpu_scene.light_dir in
  (* claude: Js.float, not the OCaml float as is: a WebGL.clampf is an
   * abstract Javascript number (js_of_ocaml >= 6). *)
  gl##uniform3f (uniform "uLightDir") (Js.float lx) (Js.float ly) (Js.float lz);
  gl##uniform1f (uniform "uAmbient") (Js.float Lighting.ambient);
  (* one texture bound at a time (one draw call per material), always
   * on texture unit 0 *)
  gl##uniform1i (uniform "uTexture") 0;
  gl##activeTexture gl##._TEXTURE0;

  gl##enable gl##._DEPTH_TEST_;
  (* which faces to cull, when culling is on (see draw) *)
  gl##cullFace gl##._BACK;

  (* GLSL ES 1.00 has no layout (location = N): the linker tells where
   * it put each attribute (-1 if the shader doesn't use it). *)
  let attributes =
    [ ("aPos", 3, 0); ("aNormal", 3, 3); ("aColor", 3, 6); ("aUv", 2, 9) ]
    |> List.filter_map (fun (name, size, offset) ->
           let loc = gl##getAttribLocation program (Js.string name) in
           if loc >= 0 then Some (loc, size, offset) else None)
  in
  Ok
    {
      canvas;
      gl;
      program;
      buffer = gl##createBuffer;
      attributes;
      meshes = Mesh_cache.create ();
      mvp_location = uniform "uMVP";
      shading_location = uniform "uShading";
      use_texture_location = uniform "uUseTexture";
      textures = Hashtbl.create 8;
    }

(*****************************************************************************)
(* Drawing *)
(*****************************************************************************)

let use_material (st : gl_state) (rendering : Playground3d.rendering) (material : Gpu_scene.material) : unit =
  let gl = st.gl in
  match material with
  | Flat -> gl##uniform1i st.use_texture_location 0
  | Textured src ->
      gl##uniform1i st.use_texture_location 1;
      gl##bindTexture gl##._TEXTURE_2D_ (gl_texture st src);
      (* smooth_textures: the GPU's bilinear filtering, or the
       * nearest texel *)
      let filter = if rendering.smooth_textures then gl##._LINEAR else gl##._NEAREST in
      gl##texParameteri gl##._TEXTURE_2D_ gl##._TEXTURE_MIN_FILTER_ filter;
      gl##texParameteri gl##._TEXTURE_2D_ gl##._TEXTURE_MAG_FILTER_ filter

(* Wireframe. WebGL has no polygon mode (OpenGL's one-line wireframe,
 * glPolygonMode(GL_LINE)), so each triangle's 3 edges are drawn as 3
 * lines instead: its vertices a b c, from Gpu_scene's triangle list,
 * copied as a b, b c, c a, for drawArrays LINES:
 *
 *        c                  triangles:  a b c  d e f  ...
 *       / \
 *      /   \                lines:      a b  b c  c a  d e  e f  f d ...
 *     a-----b
 *
 * Unlike the polygon mode, lines are never culled (culling is about
 * which side of a triangle faces the camera, and a line has no sides),
 * so "b" makes no difference in wireframe here. *)
let lines_of_triangles (data : float array) : float array =
  let n = Gpu_scene.floats_per_vertex in
  let triangles = Array.length data / (3 * n) in
  let lines = Array.make (triangles * 6 * n) 0. in
  for t = 0 to triangles - 1 do
    [ 0; 1; 1; 2; 2; 0 ] |> List.iteri (fun k v -> Array.blit data (((t * 3) + v) * n) lines (((t * 6) + k) * n) n)
  done;
  lines

let draw_group (st : gl_state) (rendering : Playground3d.rendering)
    ((material, vertices) : Gpu_scene.material * Gpu_scene.vertex_data list) : unit =
  let gl = st.gl in
  let (data, vertex_count) = Gpu_scene.vertex_floats_of_group vertices in
  if vertex_count > 0 then begin
    use_material st rendering material;
    if !wireframe then begin
      gl##bufferData gl##._ARRAY_BUFFER_ (float32_array (lines_of_triangles data)) gl##._DYNAMIC_DRAW_;
      gl##drawArrays gl##._LINES 0 (vertex_count * 2)
    end
    else begin
      gl##bufferData gl##._ARRAY_BUFFER_ (float32_array data) gl##._DYNAMIC_DRAW_;
      gl##drawArrays gl##._TRIANGLES 0 vertex_count
    end
  end

(* Meshes: the GPU side of Playground3d.cached3d (see Mesh_cache): each
 * material group of a cached3d uploaded once, into its own buffer, with
 * STATIC_DRAW (a hint that the data won't change, so the browser can
 * keep it in GPU memory); on later frames, only drawArrays again. *)
let build_mesh (st : gl_state) (c : Playground3d.cached) () : mesh =
  let gl = st.gl in
  Gpu_scene.group_by_material [ c.content ]
  |> List.filter (fun (_, vertices) -> vertices <> [])
  |> List.map (fun (material, vertices) ->
         let (data, vertex_count) = Gpu_scene.vertex_floats_of_group vertices in
         let buffer = gl##createBuffer in
         gl##bindBuffer gl##._ARRAY_BUFFER_ buffer;
         gl##bufferData gl##._ARRAY_BUFFER_ (float32_array data) gl##._STATIC_DRAW_;
         (material, buffer, vertex_count))

let draw_mesh (st : gl_state) (rendering : Playground3d.rendering) (mesh : mesh) : unit =
  let gl = st.gl in
  mesh
  |> List.iter (fun (material, buffer, vertex_count) ->
         gl##bindBuffer gl##._ARRAY_BUFFER_ buffer;
         set_attribute_pointers gl st.attributes;
         use_material st rendering material;
         gl##drawArrays gl##._TRIANGLES 0 vertex_count)

let free_mesh (st : gl_state) (mesh : mesh) : unit =
  mesh |> List.iter (fun (_material, buffer, _vertex_count) -> st.gl##deleteBuffer buffer)

let draw (st : gl_state) (rendering : Playground3d.rendering) (computer : Playground.computer)
    (camera : Playground3d.camera) (shapes : Playground3d.shape3d list) : unit =
  let gl = st.gl in
  ensure_in_page st.canvas;
  let (canvas_w, canvas_h) = resize_to_window st.canvas in
  let (x, y, w, h) = letterbox ~canvas_w ~canvas_h computer.screen in
  gl##viewport x y w h;
  gl##clearColor (Js.float 1.) (Js.float 1.) (Js.float 1.) (Js.float 1.);
  gl##clear (gl##._COLOR_BUFFER_BIT_ lor gl##._DEPTH_BUFFER_BIT_);
  gl##useProgram st.program;
  let aspect = computer.screen.width /. computer.screen.height in
  let view = Mat4.look_at ~up:camera.up ~eye:camera.eye ~target:camera.target () in
  let projection = (if camera.ortho > 0. then Mat4.orthographic ~height:camera.ortho ~aspect ~near:camera.near ~far:camera.far
       else Mat4.perspective ~fov_degrees:camera.fov ~aspect ~near:camera.near ~far:camera.far) in
  (* WebGL 1 wants column-major, and can't transpose itself (its
   * [transpose] argument must be false) *)
  let mvp = Mat4.transpose (Mat4.mul projection view) in
  gl##uniformMatrix4fv_typed st.mvp_location Js._false (float32_array mvp);
  gl##uniform1i st.shading_location (shading_code rendering.shading);
  if rendering.backface_culling then gl##enable gl##._CULL_FACE_ else gl##disable gl##._CULL_FACE_;
  (* the cached3d shapes set aside, the rest drawn from the scene's
   * buffer, then the cached ones from their meshes; unless the cache is
   * off ("o"), or in wireframe (the meshes only have triangles): then
   * the cached3d shapes are flattened with the rest, every frame *)
  let cached = ref [] in
  let on_cached = if !use_cache && not !wireframe then Some (fun c -> cached := c :: !cached) else None in
  let groups = Gpu_scene.group_by_material ?on_cached shapes in
  gl##bindBuffer gl##._ARRAY_BUFFER_ st.buffer;
  set_attribute_pointers gl st.attributes;
  List.iter (draw_group st rendering) groups;
  List.rev !cached
  |> List.iter (fun (c : Playground3d.cached) ->
         draw_mesh st rendering (Mesh_cache.find_or_build st.meshes c.id (build_mesh st c)));
  Mesh_cache.sweep st.meshes ~free:(free_mesh st)

(*****************************************************************************)
(* Run app *)
(*****************************************************************************)

(* capture_mouse, with the browser's Pointer Lock API: a page may only
 * lock the pointer in answer to a click, so a click while it's not
 * locked asks for the lock, and is only that: stopped before
 * elm_playground_web's listeners (registered later, on the same
 * window, so called after this one) see it as a click for the app.
 * The browser gives the pointer back on Escape by itself. Once
 * locked, mousemove events keep coming, with movementX/Y (mdx/mdy)
 * but a frozen clientX/Y. *)
let capture_mouse_on_click () : unit =
  Dom_html.addEventListener Dom_html.window Dom_html.Event.mousedown
    (Dom_html.handler (fun (evt : Dom_html.mouseEvent Js.t) ->
         let locked = Js.Opt.test (Js.Unsafe.get Dom_html.document "pointerLockElement") in
         if locked then Js._true
         else begin
           ignore (Js.Unsafe.meth_call Dom_html.document##.body "requestPointerLock" [||]);
           ignore (Js.Unsafe.meth_call evt "stopImmediatePropagation" [||]);
           Js._false
         end))
    Js._true
  |> ignore

let run_app3d ?(rendering = Playground3d.default_rendering) ?(capture_mouse = false) ?flags
    (app3d : ('model, 'msg) Playground3d.app3d) : unit =
  if capture_mouse then capture_mouse_on_click ();
  shading := rendering.shading;
  backface_culling := rendering.backface_culling;
  smooth_textures := rendering.smooth_textures;
  (* the URL's parameters, all of them, whatever the app was given as
   * its flags: the page parameters are the backend's own, like the
   * native backends' dashed options *)
  let params = Playground_platform.flags () in
  let keys = Option.value (List.assoc_opt "keys" params) ~default:"" in
  if List.mem_assoc "debug-keys" params || keys <> "" then begin
    listen_to_debug_keys ();
    String.iter (fun c -> on_key_press (String.make 1 c)) keys;
    show_state_in_title ()
  end;
  let fixed_time = Option.bind (List.assoc_opt "fixed-time" params) float_of_string_opt in
  let at_fixed_time (computer : Playground.computer) : Playground.computer =
    match fixed_time with None -> computer | Some t -> { computer with time = Playground.Time t }
  in
  (* created on the first frame, i.e. once the page is loaded (run_app
   * waits for the onload event) *)
  let gl_state =
    lazy
      (let r = init_gl () in
       (match r with
       | Error msg -> Console.console##error (Js.string ("playground3d webgl: " ^ msg))
       | Ok _ -> ());
       r)
  in
  let view2d (computer : Playground.computer) (model : 'model) : Playground.shape list =
    let computer = at_fixed_time computer in
    let (camera, shapes) = Playground3d.view3d app3d computer model in
    (match Lazy.force gl_state with
    | Ok st -> draw st (current_rendering ()) computer camera shapes
    | Error _ -> ensure_in_page (Lazy.force no_webgl_message));
    Playground3d.collect_hud_shapes (Playground3d.group3d shapes)
  in
  let update2d (computer : Playground.computer) (model : 'model) : 'model =
    Playground3d.update3d app3d (at_fixed_time computer) model
  in
  let initial = Playground3d.init3d app3d () in
  Playground_platform.run_app ?flags (Playground.game view2d update2d initial)

(* starts the download (see Textures), so the texture can be there
 * when first drawn; doesn't wait for it (a page can't block) *)
let preload_texture (src : string) : unit = ignore (image_of src)
