(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* See Web_events.mli *)

module E = Sub
open Js_browser

(* can also use Printf.printf I think *)
let log s = 
  Js_browser.Console.log Js_browser.console (Ojs.string_to_js s)

(*****************************************************************************)
(* Event management *)
(*****************************************************************************)

(* claude: convert the mouse position of a JavaScript mouse event into
 * playground coordinates.
 *
 * There are 2 coordinate systems involved:
 *  - "client" coordinates, which the browser gives us in mouse events
 *    (Event.client_x/client_y): pixels from the top-left corner of the
 *    browser window, y going down.
 *  - the <svg> "user" coordinates, the ones we draw in, set by the
 *    viewBox attribute in [render]: here x from -500 (left) to 500
 *    (right), y from -500 (top) to 500 (bottom), since render_transform
 *    negates y. The <svg> is stretched to fill the whole window
 *    (width/height 100%), but by default (preserveAspectRatio) the
 *    browser keeps the drawing square and centered, so if the window is
 *    wider than tall there are empty bands on the left and right (or at
 *    the top/bottom otherwise).
 *
 * The problem with the old code: it computed the scaling from the
 * bounding box (position and size on the page) of Event.target, which
 * is the *element under the mouse pointer*, not necessarily the <svg>.
 * In examples/Mouse.ml, most of the time the pointer is over the big
 * yellow rectangle, so the math was roughly right; but as soon as the
 * purple circle reached the pointer, the target became the circle,
 * whose bounding box is small and elsewhere, so the computed position
 * was wrong, the circle moved away, the next event was again over the
 * rectangle, the circle moved back, etc. -> flickering and wrong
 * positions. It also ignored the empty bands mentioned above.
 *
 * The new code: always use the root <svg> element, and ask the browser
 * itself for the conversion. svg.getScreenCTM() returns the matrix
 * converting svg user coordinates to client coordinates (taking into
 * account the viewBox, the window size, the empty bands, the scrolling,
 * ...); its inverse converts the other way, which is what we need.
 * js_browser has no binding for those SVG functions, so we call them
 * via Ojs (see set_attr above); the code is the OCaml version of
 * this JavaScript:
 *
 *   let pt = svg.createSVGPoint();   // a {x, y} point object
 *   pt.x = client_x; pt.y = client_y;
 *   pt = pt.matrixTransform(svg.getScreenCTM().inverse());
 *   return [pt.x, -pt.y];
 *
 * (Ojs.call obj "meth" [|args|] is obj.meth(args), and
 * Ojs.float_to_js/float_of_js convert between OCaml and JS numbers.)
 *)
let adjust_x_y (svg : Element.t) (client_x : float) (client_y : float) =
  let svg = Element.t_to_js svg in
  let pt = Ojs.call svg "createSVGPoint" [||] in
  Ojs.set_prop_ascii pt "x" (Ojs.float_to_js client_x);
  Ojs.set_prop_ascii pt "y" (Ojs.float_to_js client_y);
  let ctm = Ojs.call svg "getScreenCTM" [||] in
  let inv = Ojs.call ctm "inverse" [||] in
  let pt = Ojs.call pt "matrixTransform" [| inv |] in
  let x = Ojs.float_of_js (Ojs.get_prop_ascii pt "x") in
  let y = Ojs.float_of_js (Ojs.get_prop_ascii pt "y") in
  (* the svg y axis goes down but the playground one goes up
   * (see render_transform) *)
  x, -. y

let adjust_key key = 
  log (Printf.sprintf "key = '%s'" key);
  match key with
  | " " -> "space"
  | _ -> key

let js_event_to_event evt (svg_opt : Element.t option) = 
  let ty = Event.type_ evt in
  match ty, svg_opt with
  | "mousemove", None -> None
  | "mousemove", Some svg ->
      let x, y = adjust_x_y svg (Event.client_x evt) (Event.client_y evt) in
      Some (E.EMouseMove (int_of_float x, int_of_float y))
  (* claude: [button] (not in vdom's binding, hence Ojs) is the button
   * that changed: 0 the left (main) one, 2 the right one; [buttons] is
   * a bitmask of those still held, 1 for the left one *)
  | "mousedown", _ when Ojs.int_of_js (Ojs.get_prop_ascii (Event.t_to_js evt) "button") = 2 ->
      Some (E.ERightMouseButton true)
  | "mouseup", _ when Ojs.int_of_js (Ojs.get_prop_ascii (Event.t_to_js evt) "button") = 2 ->
      Some (E.ERightMouseButton false)
  (* claude: 1 is the middle one (the wheel pressed) *)
  | "mousedown", _ when Ojs.int_of_js (Ojs.get_prop_ascii (Event.t_to_js evt) "button") = 1 ->
      Some (E.EMiddleMouseButton true)
  | "mouseup", _ when Ojs.int_of_js (Ojs.get_prop_ascii (Event.t_to_js evt) "button") = 1 ->
      Some (E.EMiddleMouseButton false)
  | ("mousedown" | "mouseup"), _ ->
      let b = Event.buttons evt land 1 <> 0 in
      Some (E.EMouseButton b)

  | "keydown", _ ->
      let key = Event.key evt in
      let key = adjust_key key in
      Some (E.EKeyChanged (true, key))
  | "keyup", _ ->
      let key = Event.key evt in
      let key = adjust_key key in
      Some (E.EKeyChanged (false, key))

  (* claude: the wheel and the double click, for applications rather
   * than games (plan_gui_teaching.md, phase 0). The browser's deltaY
   * is in pixels, lines or pages (deltaMode) and grows downwards,
   * where the playground's mwheel is notches and grows upwards, so
   * normalize: a notch is about 100 pixels or 3 lines. *)
  | "wheel", _ ->
      let get prop = Ojs.float_of_js (Ojs.get_prop_ascii (Event.t_to_js evt) prop) in
      let delta = get "deltaY" in
      let mode = int_of_float (get "deltaMode") in
      let notches = match mode with 0 -> delta /. 100. | 1 -> delta /. 3. | _ -> delta in
      Some (E.EMouseWheel (-. notches))
  | "dblclick", _ -> Some E.EMouseDouble

  | _ -> None

(* claude: is this keydown's [key] a character the person typed, rather
 * than a named key? The browser gives the character itself for
 * character keys ("a", "A" with shift, "e" with an accent from a dead
 * key, whatever a layout puts there) and an ASCII word otherwise
 * ("Shift", "ArrowUp", "Backspace", "F1"), so: one byte, or a
 * non-ASCII first byte (an accented character is several UTF-8 bytes).
 * No IME support, which is out of this plan's scope and said so. *)
let typed_of_key (key : string) : string option =
  if key = "" then None
  else if String.length key = 1 || Char.code key.[0] >= 0x80 then Some key
  else None
