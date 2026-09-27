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
 *   |[==   ]|  |     the code, the VGA's 8 by 16 font          |
 *   |===    |  |                                             |
 *   +-------+  +---------------------------------------------+
 *   the overview        arrows scroll ... esc back
 *
 * The overview is one bitmap, made once per file; the page of code is
 * another, its characters copied from the VGA's font (Vga_font), a
 * screen pixel per glyph pixel -- 100 columns of 8 pixels, 53 lines of
 * 16 -- made again only when it scrolls.
 *)

open Playground

(*****************************************************************************)
(* Layout *)
(*****************************************************************************)

let top_y = 420. (* the panels' top edge *)
let map_left = -480.
let map_w = 110.
let gutter = 5 (* the line numbers' columns *)
let cols = 100 (* shown; the rest of a longer line is cut *)
let visible = 53
let line_h = float_of_int Vga_font.height
let page_w = (cols + gutter + 1) * Vga_font.width
let page_h = visible * Vga_font.height
let code_left = -360.
let code_right = code_left +. float_of_int page_w
let bottom_y = top_y -. float_of_int page_h

(*****************************************************************************)
(* Model *)
(*****************************************************************************)

type t = {
  file : Code_file.t;
  lines : Highlight_code.span list array;
  overview : Rgba_image.t;
  top : int; (* the first line shown, from 0 *)
  mutable page : (int * Rgba_image.t) option; (* the page from a top line *)
}

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
  let v = { file; lines = file.lines; overview = overview_of file; top = 0; page = None } in
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

let dim = rgb 140 140 180
let yellow = rgb 255 215 70
let cyan = rgb 0 225 255

(* words at [size], their left end at [x], by the menu's estimate of a
 * character's width (Tinybox_menu's em) *)
let text ?(size = 16.) (color : color) (x : number) (y : number) (s : string) : shape =
  let w = 0.47 *. size *. float_of_int (String.length s) in
  words color s |> scale (size /. words_font_size) |> move (x +. (w /. 2.)) y

(* the character [c] (code page 437) copied into [img] at the cell
 * ([col], [row]), its ink in [rgb] *)
let blit (img : Rgba_image.t) (col : int) (row : int) ((r, g, b) : int * int * int) (c : int) : unit =
  for y = 0 to Vga_font.height - 1 do
    let bits = Vga_font.row c y in
    if bits <> 0 then
      for x = 0 to Vga_font.width - 1 do
        if (bits lsr (7 - x)) land 1 = 1 then begin
          let i = 4 * ((((row * Vga_font.height) + y) * img.width) + (col * Vga_font.width) + x) in
          Bigarray.Array1.unsafe_set img.rgba i r;
          Bigarray.Array1.unsafe_set img.rgba (i + 1) g;
          Bigarray.Array1.unsafe_set img.rgba (i + 2) b
        end
      done
  done

(* the lines from [v.top], numbered, into one image *)
let page_of (v : t) : Rgba_image.t =
  let img = Rgba_image.create ~width:page_w ~height:page_h in
  let br, bg, bb = Highlight_code.background in
  for i = 0 to (page_w * page_h) - 1 do
    Bigarray.Array1.unsafe_set img.rgba (4 * i) br;
    Bigarray.Array1.unsafe_set img.rgba ((4 * i) + 1) bg;
    Bigarray.Array1.unsafe_set img.rgba ((4 * i) + 2) bb;
    Bigarray.Array1.unsafe_set img.rgba ((4 * i) + 3) 255
  done;
  for r = 0 to min visible (Array.length v.lines - v.top) - 1 do
    let n = v.top + r in
    let number = string_of_int (n + 1) in
    String.iteri (fun k c -> blit img (gutter - String.length number + k) r (110, 140, 140) (Char.code c)) number;
    List.iter
      (fun (s : Highlight_code.span) ->
        let rgb = Highlight_code.rgb s.category in
        (* a column per byte, as Code_file's grids: a character of several
         * bytes in its first *)
        let k = ref 0 in
        while !k < String.length s.text do
          let c, len = Vga_font.decode s.text !k in
          let col = s.col + !k in
          if col < cols && s.text.[!k] <> '\t' then blit img (gutter + 1 + col) r rgb c;
          k := !k + len
        done)
      v.lines.(n)
  done;
  img

let code_lines (computer : computer) (v : t) : shape list =
  let img =
    match v.page with
    | Some (top, img) when top = v.top -> img
    | _ ->
        let img = page_of v in
        v.page <- Some (v.top, img);
        img
  in
  let mouse = computer.mouse in
  let cx = (code_left +. code_right) /. 2. in
  (* the line under the mouse, lit *)
  let r = int_of_float ((top_y -. mouse.my) /. line_h) in
  [ bitmap (float_of_int page_w) (float_of_int page_h) img |> move cx ((top_y +. bottom_y) /. 2.) ]
  @
  if mouse.mx >= code_left && mouse.mx <= code_right && mouse.my <= top_y && r >= 0 && r < visible then
    [ rectangle white (code_right -. code_left) line_h |> move cx (top_y -. ((float_of_int r +. 0.5) *. line_h)) |> fade 0.12 ]
  else []

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
  [
    rectangle (rgb 12 10 28) screen.width screen.height;
    text ~size:22. yellow map_left 452. v.file.path;
    text ~size:14. dim 330. 452. (Printf.sprintf "%d lines" (Array.length v.lines));
  ]
  @ overview v @ code_lines computer v
  @ [
      text ~size:13. dim map_left (-470.)
        "arrows wheel pgup pgdn home end scroll   click the overview to go there   esc back to the map";
      text ~size:11. cyan (map_left +. map_w +. 10.) (bottom_y -. 14.)
        (Printf.sprintf "%d-%d" (v.top + 1) (min (Array.length v.lines) (v.top + visible)));
    ]
