(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Code_view.mli.
 *
 * The screen, for the menu's 1000 by 1000:
 *
 *   games/platform/TinyMario.ml                       1234 lines
 *   +-------+  +---------------------------------------------+
 *   |=====  |  |  1  let move (p : point) ~dx =             |
 *   |==     |  |  2    Point.add p dx                        |
 *   |[====] |  |  3                                           |
 *   |[==   ]|  |     the code, a character per cell           |
 *   |===    |  |                                             |
 *   +-------+  +---------------------------------------------+
 *   the overview        arrows scroll ... esc back
 *
 * The overview is one bitmap, made once per file; the code is a
 * [words] per character, on a grid, as Teletype draws a terminal (the
 * playground's font is not a fixed-width one): some 2000 shapes a
 * frame. A font of our own, glyphs blitted into one image, is the
 * plan's step 2.
 *)

open Playground

(*****************************************************************************)
(* Layout *)
(*****************************************************************************)

let top_y = 420. (* the panels' top edge *)
let bottom_y = -440.
let map_left = -480.
let map_w = 150.
let code_left = -310.
let code_right = 490.
let line_h = 16.
let visible = int_of_float ((top_y -. bottom_y) /. line_h)
let gutter = 5 (* the line numbers' columns *)
let cols = 100 (* shown; the rest of a longer line is cut *)
let cell_w = (code_right -. code_left) /. float_of_int (cols + gutter + 1)
let font = 13.

(*****************************************************************************)
(* Model *)
(*****************************************************************************)

type t = {
  file : Code_file.t;
  lines : Highlight_code.span list array;
  overview : Rgba_image.t;
  top : int; (* the first line shown, from 0 *)
}

let color_of ((r, g, b) : int * int * int) : color = rgb r g b

(* SeeSoft's picture: a pixel per character, its category's colour *)
let overview_of (f : Code_file.t) : Rgba_image.t =
  let n = max 1 (Code_file.nlines f) in
  let img = Rgba_image.create ~width:Code_file.cols ~height:n in
  for y = 0 to n - 1 do
    for x = 0 to Code_file.cols - 1 do
      let r, g, b = match Code_file.at f y x with Some c -> Highlight_code.rgb c | None -> Highlight_code.background in
      let i = 4 * ((y * Code_file.cols) + x) in
      Bigarray.Array1.unsafe_set img.rgba i r;
      Bigarray.Array1.unsafe_set img.rgba (i + 1) g;
      Bigarray.Array1.unsafe_set img.rgba (i + 2) b;
      Bigarray.Array1.unsafe_set img.rgba (i + 3) 255
    done
  done;
  img

let make ?(line = 0) (file : Code_file.t) : t =
  let v = { file; lines = file.lines; overview = overview_of file; top = 0 } in
  { v with top = max 0 (min (Array.length v.lines - visible) (line - (visible / 2))) }

let clamp (v : t) (top : int) : t = { v with top = max 0 (min (Array.length v.lines - visible) top) }

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

(* the overview's height on screen: a line at most 4 pixels high *)
let map_h (v : t) : number = Float.min (top_y -. bottom_y) (4. *. float_of_int (Array.length v.lines))

let update (computer : computer) ~(pressed : string -> bool) ~(arrow : string option) (v : t) : t =
  let v =
    match arrow with
    | Some "ArrowUp" -> clamp v (v.top - 1)
    | Some "ArrowDown" -> clamp v (v.top + 1)
    | _ -> v
  in
  let v =
    if pressed "PageDown" || pressed " " then clamp v (v.top + visible - 2)
    else if pressed "PageUp" then clamp v (v.top - visible + 2)
    else if pressed "Home" then clamp v 0
    else if pressed "End" then clamp v max_int
    else v
  in
  let mouse = computer.mouse in
  let v = if mouse.mwheel <> 0. then clamp v (v.top - int_of_float (Float.round (mouse.mwheel *. 3.))) else v in
  (* a click or a drag on the overview: the line there in the middle *)
  let h = map_h v in
  if (mouse.mdown || mouse.mclick) && mouse.mx >= map_left && mouse.mx <= map_left +. map_w && mouse.my <= top_y && mouse.my >= top_y -. h
  then
    let line = int_of_float ((top_y -. mouse.my) /. h *. float_of_int (Array.length v.lines)) in
    clamp v (line - (visible / 2))
  else v

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let ink = rgb 228 228 240
let dim = rgb 140 140 180
let yellow = rgb 255 215 70
let cyan = rgb 0 225 255

(* words at [size], their left end at [x], by the menu's estimate of a
 * character's width (Tinybox_menu's em) *)
let text ?(size = 16.) (color : color) (x : number) (y : number) (s : string) : shape =
  let w = 0.47 *. size *. float_of_int (String.length s) in
  words color s |> scale (size /. words_font_size) |> move (x +. (w /. 2.)) y

(* a character in the cell of column [col] (from 0) of the row at [y] *)
let glyph (color : color) (col : int) (y : number) (c : char) : shape =
  words color (String.make 1 c)
  |> scale (font /. words_font_size)
  |> move (code_left +. ((float_of_int (col + gutter + 1) +. 0.5) *. cell_w)) y

let code_lines (computer : computer) (v : t) : shape list =
  let mouse = computer.mouse in
  List.concat
    (List.init (min visible (Array.length v.lines - v.top)) (fun r ->
         let n = v.top + r in
         let y = top_y -. ((float_of_int r +. 0.5) *. line_h) in
         (* the line under the mouse, lit *)
         let under = mouse.mx >= code_left && mouse.mx <= code_right && Float.abs (mouse.my -. y) < line_h /. 2. in
         let number = string_of_int (n + 1) in
         (if under then [ rectangle (rgb 70 110 110) (code_right -. code_left) line_h |> move ((code_left +. code_right) /. 2.) y ] else [])
         @ List.mapi (fun k c -> glyph (rgb 110 140 140) (k - String.length number - 1) y c) (List.of_seq (String.to_seq number))
         @ List.concat_map
             (fun (s : Highlight_code.span) ->
               let color = color_of (Highlight_code.rgb s.category) in
               List.concat
                 (List.mapi
                    (fun k c -> if c = ' ' || c = '\t' || s.col + k >= cols then [] else [ glyph color (s.col + k) y c ])
                    (List.of_seq (String.to_seq s.text))))
             v.lines.(n)))

let overview (v : t) : shape list =
  let h = map_h v in
  let n = float_of_int (max 1 (Array.length v.lines)) in
  let frame_top = top_y -. (float_of_int v.top /. n *. h) in
  let frame_h = Float.min h (float_of_int visible /. n *. h) in
  let cx = map_left +. (map_w /. 2.) in
  [
    bitmap map_w h v.overview |> move cx (top_y -. (h /. 2.));
    (* the part shown *)
    rectangle white map_w frame_h |> move cx (frame_top -. (frame_h /. 2.)) |> fade 0.2;
    rectangle yellow map_w 2. |> move cx frame_top;
    rectangle yellow map_w 2. |> move cx (frame_top -. frame_h);
  ]

let view (computer : computer) (v : t) : shape list =
  let screen = computer.screen in
  let br, bg, bb = Highlight_code.background in
  [
    rectangle (rgb 12 10 28) screen.width screen.height;
    rectangle (rgb br bg bb) (code_right -. code_left) (top_y -. bottom_y) |> move ((code_left +. code_right) /. 2.) ((top_y +. bottom_y) /. 2.);
    text ~size:22. yellow map_left 452. v.file.path;
    text ~size:14. dim 330. 452. (Printf.sprintf "%d lines" (Array.length v.lines));
  ]
  @ overview v @ code_lines computer v
  @ [
      text ~size:13. dim map_left (-475.)
        "arrows wheel pgup pgdn home end scroll   click the overview to go there   esc back to the map";
      text ~size:11. cyan (map_left +. map_w +. 10.) (-455.)
        (Printf.sprintf "%d-%d" (v.top + 1) (min (Array.length v.lines) (v.top + visible)));
    ]
