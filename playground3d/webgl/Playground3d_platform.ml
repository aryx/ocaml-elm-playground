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
 * (js_of_ocaml's WebGL module). See docs/claude_notes/plan_webgl.md.
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
 * shader, the same rendering hints. Not yet: textures (drawn with a
 * magenta placeholder, see plan_webgl.md's Phase 4), wireframe (WebGL
 * has no polygon mode), and the debug keys. *)

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
 * frame, i.e. just after our first draw: put the canvas back when it
 * has been removed (a no-op on every other frame) *)
let ensure_in_page (canvas : Dom_html.canvasElement Js.t) : unit =
  if not (Js.Opt.test canvas##.parentNode) then Dom.appendChild Dom_html.document##.body canvas

(* The canvas has two sizes: its size on the page (clientWidth/Height,
 * 100% of the window, in CSS pixels, see create_canvas) and the size
 * of its drawing buffer (canvas##.width/height, in real pixels), which
 * we keep equal to the former times devicePixelRatio, for a sharp
 * picture on a HiDPI screen; if they differed, the browser would
 * stretch the picture to the page size, distorting it. Checked every
 * frame, to follow the window's resizes. *)
let resize_to_window (canvas : Dom_html.canvasElement Js.t) : int * int =
  let dpr = Dom_html.window##.devicePixelRatio in
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
(* GL state *)
(*****************************************************************************)

type gl_state = {
  canvas : Dom_html.canvasElement Js.t;
  gl : WebGL.renderingContext Js.t;
  program : WebGL.program Js.t;
  buffer : WebGL.buffer Js.t;
  mvp_location : [ `mat4 ] WebGL.uniformLocation Js.t;
  shading_location : int WebGL.uniformLocation Js.t;
  use_texture_location : int WebGL.uniformLocation Js.t;
  (* until textures are loaded (Phase 4), every textured face shows
   * this, the same magenta as the other backends' missing texture *)
  placeholder_texture : WebGL.texture Js.t;
}

let float32_array (data : float array) : Typed_array.float32Array Js.t =
  new%js Typed_array.float32Array_fromArray (Js.array data)

let create_placeholder_texture (gl : WebGL.renderingContext Js.t) : WebGL.texture Js.t =
  let tex = gl##createTexture in
  gl##bindTexture gl##._TEXTURE_2D_ tex;
  let magenta = new%js Typed_array.uint8Array_fromArray (Js.array [| 255; 0; 255; 255 |]) in
  gl##texImage2D_fromView gl##._TEXTURE_2D_ 0 gl##._RGBA 1 1 0 gl##._RGBA gl##._UNSIGNED_BYTE_ magenta;
  (* WebGL 1 can only sample a texture whose size isn't a power of 2
   * with this wrap mode and no mipmaps; this one is 1x1, but real ones
   * (Phase 4) will be set up the same way *)
  gl##texParameteri gl##._TEXTURE_2D_ gl##._TEXTURE_WRAP_S_ gl##._CLAMP_TO_EDGE_;
  gl##texParameteri gl##._TEXTURE_2D_ gl##._TEXTURE_WRAP_T_ gl##._CLAMP_TO_EDGE_;
  tex

let init_gl () : gl_state =
  let canvas = create_canvas () in
  let attrs = WebGL.defaultContextAttributes in
  (* the default, but the z-buffer is the point of this backend *)
  attrs##.depth := Js._true;
  let gl =
    match Js.Opt.to_option (WebGL.getContextWithAttributes canvas attrs) with
    | Some gl -> gl
    | None -> failwith "WebGL is not available in this browser"
  in
  (* getExtension both tells whether the extension is there and turns
   * it on *)
  let derivatives = Js.Opt.test (gl##getExtension (Js.string "OES_standard_derivatives")) in
  let program =
    link_program gl ~vertex_source:vertex_shader_source ~fragment_source:(fragment_shader_source ~derivatives)
  in
  gl##useProgram program;
  let uniform name = gl##getUniformLocation program (Js.string name) in
  let (lx, ly, lz) = Gpu_scene.light_dir in
  gl##uniform3f (uniform "uLightDir") lx ly lz;
  gl##uniform1f (uniform "uAmbient") Lighting.ambient;
  (* one texture bound at a time (one draw call per material), always
   * on texture unit 0 *)
  gl##uniform1i (uniform "uTexture") 0;
  gl##activeTexture gl##._TEXTURE0;

  gl##enable gl##._DEPTH_TEST_;
  (* which faces to cull, when culling is on (see draw) *)
  gl##cullFace gl##._BACK;

  (* The attribute layout of Gpu_scene.vertex_floats_of_group: position
   * (3 floats), normal (3), color (3), uv (2), interleaved. One buffer,
   * so the attribute pointers are set once, here (WebGL 1 has no VAOs
   * to remember them; it doesn't need to, the state stays as set).
   * GLSL ES 1.00 has no layout (location = N): the linker tells where
   * it put each attribute (-1 if the shader doesn't use it). *)
  let buffer = gl##createBuffer in
  gl##bindBuffer gl##._ARRAY_BUFFER_ buffer;
  let stride = Gpu_scene.floats_per_vertex * 4 (* bytes per float *) in
  [ ("aPos", 3, 0); ("aNormal", 3, 3); ("aColor", 3, 6); ("aUv", 2, 9) ]
  |> List.iter (fun (name, size, offset) ->
         let loc = gl##getAttribLocation program (Js.string name) in
         if loc >= 0 then begin
           gl##vertexAttribPointer loc size gl##._FLOAT Js._false stride (offset * 4);
           gl##enableVertexAttribArray loc
         end);
  {
    canvas;
    gl;
    program;
    buffer;
    mvp_location = uniform "uMVP";
    shading_location = uniform "uShading";
    use_texture_location = uniform "uUseTexture";
    placeholder_texture = create_placeholder_texture gl;
  }

(*****************************************************************************)
(* Drawing *)
(*****************************************************************************)

let draw_group (st : gl_state) (rendering : Playground3d.rendering)
    ((material, vertices) : Gpu_scene.material * Gpu_scene.vertex_data list) : unit =
  let gl = st.gl in
  let (data, vertex_count) = Gpu_scene.vertex_floats_of_group vertices in
  if vertex_count > 0 then begin
    gl##bufferData gl##._ARRAY_BUFFER_ (float32_array data) gl##._DYNAMIC_DRAW_;
    (match material with
    | Flat -> gl##uniform1i st.use_texture_location 0
    | Textured _src ->
        gl##uniform1i st.use_texture_location 1;
        gl##bindTexture gl##._TEXTURE_2D_ st.placeholder_texture;
        (* smooth_textures: the GPU's bilinear filtering, or the
         * nearest texel *)
        let filter = if rendering.smooth_textures then gl##._LINEAR else gl##._NEAREST in
        gl##texParameteri gl##._TEXTURE_2D_ gl##._TEXTURE_MIN_FILTER_ filter;
        gl##texParameteri gl##._TEXTURE_2D_ gl##._TEXTURE_MAG_FILTER_ filter);
    gl##drawArrays gl##._TRIANGLES 0 vertex_count
  end

let draw (st : gl_state) (rendering : Playground3d.rendering) (computer : Playground.computer)
    (camera : Playground3d.camera) (shapes : Playground3d.shape3d list) : unit =
  let gl = st.gl in
  ensure_in_page st.canvas;
  let (canvas_w, canvas_h) = resize_to_window st.canvas in
  let (x, y, w, h) = letterbox ~canvas_w ~canvas_h computer.screen in
  gl##viewport x y w h;
  gl##clearColor 1. 1. 1. 1.;
  gl##clear (gl##._COLOR_BUFFER_BIT_ lor gl##._DEPTH_BUFFER_BIT_);
  gl##useProgram st.program;
  let aspect = computer.screen.width /. computer.screen.height in
  let view = Mat4.look_at ~eye:camera.eye ~target:camera.target in
  let projection = Mat4.perspective ~fov_degrees:camera.fov ~aspect ~near:camera.near ~far:camera.far in
  (* WebGL 1 wants column-major, and can't transpose itself (its
   * [transpose] argument must be false) *)
  let mvp = Mat4.transpose (Mat4.mul projection view) in
  gl##uniformMatrix4fv_typed st.mvp_location Js._false (float32_array mvp);
  gl##uniform1i st.shading_location (shading_code rendering.shading);
  if rendering.backface_culling then gl##enable gl##._CULL_FACE_ else gl##disable gl##._CULL_FACE_;
  Gpu_scene.group_by_material shapes |> List.iter (draw_group st rendering)

(*****************************************************************************)
(* Run app *)
(*****************************************************************************)

let run_app3d ?(rendering = Playground3d.default_rendering) (app3d : ('model, 'msg) Playground3d.app3d) : unit =
  (* created on the first frame, i.e. once the page is loaded (run_app
   * waits for the onload event) *)
  let gl_state = lazy (init_gl ()) in
  let view2d (computer : Playground.computer) (model : 'model) : Playground.shape list =
    let (camera, shapes) = Playground3d.view3d app3d computer model in
    draw (Lazy.force gl_state) rendering computer camera shapes;
    Playground3d.collect_hud_shapes (Playground3d.group3d shapes)
  in
  let update2d (computer : Playground.computer) (model : 'model) : 'model =
    Playground3d.update3d app3d computer model
  in
  let initial = Playground3d.init3d app3d () in
  Playground_platform.run_app (Playground.game view2d update2d initial)

(* claude: no-op until textures (plan_webgl.md, Phase 4) *)
let preload_texture (_src : string) : unit = ()
