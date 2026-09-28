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
 * another, its characters from the VGA's font (Vga_font) -- 100 columns
 * of 8 units, 53 lines of 16 -- painted at the window's resolution
 * (page_of) and made again only when it scrolls.
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

(* a character's size in the window's pixels, at pixel ratio [q] (the
 * page's, page_of below) *)
let cell_px (q : float) : int * int =
  (max 1 (int_of_float (Float.round (float_of_int Vga_font.width *. q))), max 1 (int_of_float (Float.round (float_of_int Vga_font.height *. q))))

(* the window's pixels a unit, as the page is painted *)
let ratio () : float = Float.max 0.5 (Float.min 3. (Playground_platform.pixel_ratio ()))

(* claude: a character's cell on the screen, in units: a whole number of
 * the window's pixels, so a little more or less than 8 by 16 *)
let cell_units () : float * float =
  let q = ratio () in
  let cw, ch = cell_px q in
  (float_of_int cw /. q, float_of_int ch /. q)

(* the line (from 0, the file's) and column under a point of the page,
 * if on the code *)
let code_at (top : int) (x : number) (y : number) : (int * int) option =
  let cw, ch = cell_units () in
  let r = int_of_float (Float.floor ((top_y -. y) /. ch)) and c = int_of_float (Float.floor ((x -. code_left) /. cw)) - (gutter + 1) in
  if r >= 0 && r < visible && c >= 0 && c < cols then Some (top + r, c) else None

(*****************************************************************************)
(* Model *)
(*****************************************************************************)

type t = {
  file : Code_file.t;
  lines : Highlight_code.span list array;
  overview : Rgba_image.t;
  top : int; (* the first line shown, from 0 *)
  lit : int option; (* claude: a line lit (the tour's stop) *)
  mutable page : (int * float * Rgba_image.t) option; (* the page from a top line, at a pixel ratio *)
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

let make ?(line = 0) ?lit (file : Code_file.t) : t =
  let v = { file; lines = file.lines; overview = overview_of file; top = 0; lit; page = None } in
  (* claude: a lit line near the top, what follows it below; else in
   * the middle *)
  let top = match lit with Some _ -> line - 2 | None -> line - (visible / 2) in
  { v with top = max 0 (min (Array.length v.lines - visible) top) }

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
  else
    (* claude: a click on a name bound in the file (a parameter, a local,
     * a top-level definition): to its binding, lit (plan_codemap_naming.md,
     * levels 1 and 2), where it is if on the page, else near the top *)
    match if mouse.mclick then Option.bind (code_at v.top mouse.mx mouse.my) (fun (l, c) -> Code_file.name_at v.file l c) else None with
    | Some o ->
        let line, _ = o.bound_at in
        if line >= v.top && line < v.top + visible then { v with lit = Some line } else { (clamp v (line - 2)) with lit = Some line }
    | None -> v

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

(* The page: the lines from [v.top], numbered, into one image, painted
 * at the window's resolution.
 *
 * claude: why not at the screen's, 8 by 16 pixels a character: the
 * platform enlarges a bitmap to fill the window, smoothing it, so on a
 * monitor with more pixels than the screen's units (tinybox's 1778 by
 * 1000 on a 4K monitor: 2.16 of the window's pixels a unit) the VGA font
 * came out enlarged and blurred. So the page is made
 * Playground_platform.pixel_ratio times bigger, one of its pixels one of
 * the window's.
 *
 * And fast: the page is made again at every line scrolled, a drag on the
 * overview or Page Down (2 million pixels at 4K). Sampling the font 4
 * times a pixel for the whole page was too slow for a drag. So a
 * character is a whole number of pixels, cell_px (17 by 35 at 2.16,
 * rather than 17.28 by 34.56: the page a few percent bigger, and still
 * one of its pixels one of the window's), and each of the 256 glyphs is
 * anti-aliased once at that size, into a mask of how much of each pixel
 * is ink (glyph_masks: 2 by 2 samples, a box filter -- a font cache, as a
 * font rasterizer keeps one). A page is then only masks copied in their
 * colours: a few milliseconds. *)

(* the 256 glyphs' masks at a cell size, each pixel's ink from 0 to 4
 * samples, made once per size *)
let masks : ((int * int) * Bytes.t array) option ref = ref None

let glyph_masks ((cw, ch) : int * int) : Bytes.t array =
  match !masks with
  | Some (size, m) when size = (cw, ch) -> m
  | _ ->
      let sx = float_of_int Vga_font.width /. float_of_int cw and sy = float_of_int Vga_font.height /. float_of_int ch in
      let m =
        Array.init 256 (fun c ->
            let b = Bytes.make (cw * ch) '\000' in
            for y = 0 to ch - 1 do
              for x = 0 to cw - 1 do
                let hits = ref 0 in
                for ky = 0 to 1 do
                  for kx = 0 to 1 do
                    let gx = int_of_float ((float_of_int x +. ((float_of_int kx +. 0.5) /. 2.)) *. sx)
                    and gy = int_of_float ((float_of_int y +. ((float_of_int ky +. 0.5) /. 2.)) *. sy) in
                    if Vga_font.bit c (min 7 gx) (min 15 gy) then incr hits
                  done
                done;
                Bytes.set b ((y * cw) + x) (Char.chr !hits)
              done
            done;
            b)
      in
      masks := Some ((cw, ch), m);
      m

let page_of (v : t) (q : float) : Rgba_image.t =
  let cw, ch = cell_px q in
  let gcols = cols + gutter + 1 in
  let w = gcols * cw and h = visible * ch in
  let img = Rgba_image.create ~width:w ~height:h in
  let rgba = img.rgba in
  let br, bg, bb = Highlight_code.background in
  for i = 0 to (w * h) - 1 do
    Bigarray.Array1.unsafe_set rgba (4 * i) br;
    Bigarray.Array1.unsafe_set rgba ((4 * i) + 1) bg;
    Bigarray.Array1.unsafe_set rgba ((4 * i) + 2) bb;
    Bigarray.Array1.unsafe_set rgba ((4 * i) + 3) 255
  done;
  let m = glyph_masks (cw, ch) in
  (* a character's mask, in its colour over the background, at a cell *)
  let put col row ((r, g, b) : int * int * int) (c : int) =
    if col >= 0 && col < gcols && c <> 0 && c <> 32 then begin
      let mask = m.(c land 255) in
      for y = 0 to ch - 1 do
        let base = ((((row * ch) + y) * w) + (col * cw)) * 4 in
        for x = 0 to cw - 1 do
          let k = Char.code (Bytes.unsafe_get mask ((y * cw) + x)) in
          if k > 0 then begin
            let i = base + (4 * x) in
            Bigarray.Array1.unsafe_set rgba i (br + ((r - br) * k / 4));
            Bigarray.Array1.unsafe_set rgba (i + 1) (bg + ((g - bg) * k / 4));
            Bigarray.Array1.unsafe_set rgba (i + 2) (bb + ((b - bb) * k / 4))
          end
        done
      done
    end
  in
  for r = 0 to min visible (Array.length v.lines - v.top) - 1 do
    let n = v.top + r in
    let number = string_of_int (n + 1) in
    String.iteri (fun k c -> put (gutter - String.length number + k) r (110, 140, 140) (Char.code c)) number;
    List.iter
      (fun (s : Highlight_code.span) ->
        let rgb = Highlight_code.rgb s.category in
        (* a column per byte, as Code_file's grids: a character of several
         * bytes in its first *)
        let k = ref 0 in
        while !k < String.length s.text do
          let c, len = Vga_font.decode s.text !k in
          let col = s.col + !k in
          if col < cols && s.text.[!k] <> '\t' then put (gutter + 1 + col) r rgb c;
          k := !k + len
        done)
      v.lines.(n)
  done;
  img

(* claude: the name under the mouse, bound in the file: its binding
 * framed brighter, its uses on the page lit *)
let name_lit (computer : computer) (v : t) : shape list =
  let mouse = computer.mouse in
  match Option.bind (code_at v.top mouse.mx mouse.my) (fun (l, c) -> Code_file.name_at v.file l c) with
  | None -> []
  | Some o ->
      let cw, ch = cell_units () in
      List.filter_map
        (fun (u : Highlight_code.occurrence) ->
          if u.line < v.top || u.line >= v.top + visible || u.col >= cols then None
          else
            let w = float_of_int (min u.len (cols - u.col)) *. cw in
            let x = code_left +. (float_of_int (gutter + 1 + u.col) *. cw) +. (w /. 2.) in
            let y = top_y -. ((float_of_int (u.line - v.top) +. 0.5) *. ch) in
            let binding = (u.line, u.col) = o.bound_at in
            Some (rectangle (if binding then cyan else yellow) w ch |> move x y |> fade (if binding then 0.38 else 0.25)))
        (Code_file.uses v.file o)

let code_lines (computer : computer) (v : t) : shape list =
  let q = ratio () in
  let img =
    match v.page with
    | Some (top, pq, img) when top = v.top && pq = q -> img
    | _ ->
        let img = page_of v q in
        v.page <- Some (v.top, q, img);
        img
  in
  (* drawn at its size in the window's pixels over q: one of its pixels,
   * one of the window's; a line [lh] units high, 16 give or take the
   * rounding of cell_px *)
  let pw = float_of_int img.width /. q and ph = float_of_int img.height /. q in
  let lh = ph /. float_of_int visible in
  let mouse = computer.mouse in
  let cx = code_left +. (pw /. 2.) in
  (* the line under the mouse, lit *)
  let r = int_of_float ((top_y -. mouse.my) /. lh) in
  [ bitmap pw ph img |> move cx (top_y -. (ph /. 2.)) ]
  @ (match v.lit with
    | Some l when l >= v.top && l < v.top + visible ->
        [ rectangle (rgb 90 210 120) pw lh |> move cx (top_y -. ((float_of_int (l - v.top) +. 0.5) *. lh)) |> fade 0.22 ]
    | _ -> [])
  @
  if mouse.mx >= code_left && mouse.mx <= code_left +. pw && mouse.my <= top_y && r >= 0 && r < visible then
    [ rectangle white pw lh |> move cx (top_y -. ((float_of_int r +. 0.5) *. lh)) |> fade 0.12 ]
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
  @ overview v @ code_lines computer v @ name_lit computer v
  @ [
      text ~size:13. dim map_left (-470.)
        "arrows wheel pgup pgdn home end scroll   click the overview to go there, a name to its definition   esc back to the map";
      text ~size:11. cyan (map_left +. map_w +. 10.) (bottom_y -. 14.)
        (Printf.sprintf "%d-%d" (v.top + 1) (min (Array.length v.lines) (v.top + visible)));
    ]
