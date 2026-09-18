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
 * PHASE 2 of the plan: the pipeline end to end (canvas, context,
 * shaders, a vertex buffer, one draw call) on a hardcoded triangle; the
 * scene itself is ignored for now. *)

(*****************************************************************************)
(* Shaders *)
(*****************************************************************************)
(* GLSL ES 1.00, WebGL 1's, rather than OpenGL's GLSL 3.30: attribute/
 * varying instead of in/out, gl_FragColor instead of an out variable,
 * and a default float precision the fragment shader must choose itself
 * (the vertex shader has highp by default). *)

let vertex_shader_source =
  "attribute vec2 aPos;\n\
   attribute vec3 aColor;\n\
   varying vec3 vColor;\n\
   void main() {\n\
  \  gl_Position = vec4(aPos, 0.0, 1.0);\n\
  \  vColor = aColor;\n\
   }\n"

let fragment_shader_source =
  "precision mediump float;\n\
   varying vec3 vColor;\n\
   void main() {\n\
  \  gl_FragColor = vec4(vColor, 1.0);\n\
   }\n"

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
  pos_location : int;
  color_location : int;
}

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
  let program = link_program gl ~vertex_source:vertex_shader_source ~fragment_source:fragment_shader_source in
  (* GLSL ES 1.00 has no layout (location = N): ask the linker where it
   * put each attribute *)
  let pos_location = gl##getAttribLocation program (Js.string "aPos") in
  let color_location = gl##getAttribLocation program (Js.string "aColor") in
  let buffer = gl##createBuffer in
  { canvas; gl; program; buffer; pos_location; color_location }

(*****************************************************************************)
(* Drawing *)
(*****************************************************************************)

(* x, y (in normalized device coordinates, -1..1), r, g, b *)
let hello_triangle : float array =
  [| -0.5; -0.5; 1.; 0.; 0.; 0.5; -0.5; 0.; 1.; 0.; 0.; 0.5; 0.; 0.; 1. |]

let floats_per_vertex = 5

let draw (st : gl_state) (computer : Playground.computer) : unit =
  let gl = st.gl in
  ensure_in_page st.canvas;
  let (canvas_w, canvas_h) = resize_to_window st.canvas in
  let (x, y, w, h) = letterbox ~canvas_w ~canvas_h computer.screen in
  gl##viewport x y w h;
  gl##clearColor 1. 1. 1. 1.;
  gl##clear (gl##._COLOR_BUFFER_BIT_ lor gl##._DEPTH_BUFFER_BIT_);
  gl##useProgram st.program;
  let data = new%js Typed_array.float32Array_fromArray (Js.array hello_triangle) in
  gl##bindBuffer gl##._ARRAY_BUFFER_ st.buffer;
  gl##bufferData gl##._ARRAY_BUFFER_ data gl##._DYNAMIC_DRAW_;
  let stride = floats_per_vertex * 4 (* bytes per float *) in
  gl##vertexAttribPointer st.pos_location 2 gl##._FLOAT Js._false stride 0;
  gl##enableVertexAttribArray st.pos_location;
  gl##vertexAttribPointer st.color_location 3 gl##._FLOAT Js._false stride (2 * 4);
  gl##enableVertexAttribArray st.color_location;
  gl##drawArrays gl##._TRIANGLES 0 (Array.length hello_triangle / floats_per_vertex)

(*****************************************************************************)
(* Run app *)
(*****************************************************************************)

let run_app3d ?(rendering = Playground3d.default_rendering) (app3d : ('model, 'msg) Playground3d.app3d) : unit =
  ignore rendering;
  (* created on the first frame, i.e. once the page is loaded (run_app
   * waits for the onload event) *)
  let gl_state = lazy (init_gl ()) in
  let view2d (computer : Playground.computer) (model : 'model) : Playground.shape list =
    let (_cam, shapes) = Playground3d.view3d app3d computer model in
    draw (Lazy.force gl_state) computer;
    Playground3d.collect_hud_shapes (Playground3d.group3d shapes)
  in
  let update2d (computer : Playground.computer) (model : 'model) : 'model =
    Playground3d.update3d app3d computer model
  in
  let initial = Playground3d.init3d app3d () in
  Playground_platform.run_app (Playground.game view2d update2d initial)

(* claude: no-op until textures (plan_webgl.md, Phase 4) *)
let preload_texture (_src : string) : unit = ()
