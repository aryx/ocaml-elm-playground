(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* See Webgl_canvas.mli *)

open Js_of_ocaml

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
