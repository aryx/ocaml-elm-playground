(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Code_map.mli.
 *
 * Two spaces: the layout's, where the treemap is laid out once in a
 * rectangle the map's size (units, y downwards), and the screen's, the
 * map's pixels. The camera says which unit is at the map's centre and
 * how many pixels a unit is (z); zooming and panning only change it,
 * never the layout, so every move can be eased:
 *
 *   pixel (px, py)  =  ((u - cx) * z + pw/2,  (v - cy) * z + ph/2)
 *
 * A file's rectangle holds its lines in k columns of [Code_file.cols]
 * characters, k chosen once (from the rectangle's shape, so the same at
 * every zoom) to make a character's cell about twice as high as wide,
 * the VGA font's 8 by 16; a pixel's colour is the category of the
 * character under it, and, once the cells are big enough to read, only
 * where the character's glyph has ink (paint_code).
 *)

open Playground

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type entry = { path : string; nlines : int; file : Code_file.t Lazy.t }

(* where the map is on the screen: its top left corner in the
 * playground's coordinates, its size in pixels *)
type area = { left : float; top : float; pw : int; ph : int }

(* [a] rides along: every function given the camera knows where it draws *)
type camera = { cx : float; cy : float; z : float; a : area }

(* a file's geometry in its rectangle, in units *)
type geometry = { k : int; lpc : int; (* lines per column *) colw : float; cell_w : float; cell_h : float }

type t = {
  title : string;
  marked : string list;
  entries : entry list;
  algo : Treemap.algo;
  placed : entry Treemap.placed array;
  geometry : geometry option array; (* the files' *)
  cam : camera;
  target : camera;
  drag : (float * float * camera) option; (* where the press began, and the camera then *)
  dragged : bool; (* the press moved: its release is no click *)
  before_right : bool;
  mutable painted : (camera * float * Rgba_image.t) option; (* the picture of [cam], at a pixel ratio *)
  mutable last : camera option; (* the camera the frame before: is it still? *)
  mutable lens : (camera * Rgba_image.t) option; (* the magnifying glass's last picture, and its camera *)
}

type action = Stay | Open of Code_file.t * int | Close

(*****************************************************************************)
(* Layout *)
(*****************************************************************************)

(* the layout is in a rectangle the map's size, its units its pixels at
 * the first zoom *)
let root_rect (a : area) : Treemap.rect = { x = 0.; y = 0.; w = float_of_int a.pw; h = float_of_int a.ph }

let geometry_of (r : Treemap.rect) (nlines : int) : geometry =
  let n = max 1 nlines in
  let make k =
    let lpc = (n + k - 1) / k in
    let colw = r.w /. float_of_int k in
    { k; lpc; colw; cell_w = colw /. float_of_int Code_file.cols; cell_h = r.h /. float_of_int lpc }
  in
  (* the k whose cells are closest to 2 high for 1 wide *)
  let score g = Float.abs (Float.log (g.cell_h /. g.cell_w /. 2.)) in
  let rec best k acc = if k > min n 64 then acc else best (k + 1) (let g = make k in if score g < score acc then g else acc) in
  best 2 (make 1)

let relayout (a : area) (algo : Treemap.algo) (entries : entry list) : entry Treemap.placed array * geometry option array =
  let tree =
    Treemap.fold_singletons (Treemap.of_paths (List.map (fun e -> (e.path, float_of_int (max 1 e.nlines), e)) entries))
  in
  let placed = Array.of_list (Treemap.layout algo (root_rect a) tree) in
  (placed, Array.map (fun (p : entry Treemap.placed) -> match p.node with File (_, _, e) -> Some (geometry_of p.rect e.nlines) | Dir _ -> None) placed)

let fit (a : area) (r : Treemap.rect) : camera =
  { cx = r.x +. (r.w /. 2.); cy = r.y +. (r.h /. 2.); z = 0.96 *. Float.min (float_of_int a.pw /. r.w) (float_of_int a.ph /. r.h); a }

let home (a : area) : camera = { (fit a (root_rect a)) with z = 1. }

let make ~(area : float * float * int * int) ~(title : string) ~(marked : string list) (entries : entry list) : t =
  let left, top, pw, ph = area in
  let a = { left; top; pw; ph } in
  let placed, geometry = relayout a Squarified entries in
  { title; marked; entries; algo = Squarified; placed; geometry; cam = home a; target = home a; drag = None; dragged = false;
    before_right = false; painted = None; last = None; lens = None }

(* screen <-> units *)
let to_px (c : camera) (u : float) : float = ((u -. c.cx) *. c.z) +. (float_of_int c.a.pw /. 2.)
let to_py (c : camera) (v : float) : float = ((v -. c.cy) *. c.z) +. (float_of_int c.a.ph /. 2.)
let to_u (c : camera) (px : float) : float = c.cx +. ((px -. (float_of_int c.a.pw /. 2.)) /. c.z)
let to_v (c : camera) (py : float) : float = c.cy +. ((py -. (float_of_int c.a.ph /. 2.)) /. c.z)

(* the playground's coordinates of a pixel of the map, and back *)
let sx (a : area) (px : float) : number = a.left +. px
let sy (a : area) (py : float) : number = a.top -. py
let px_of (a : area) (x : number) : float = x -. a.left
let py_of (a : area) (y : number) : float = a.top -. y
let on (a : area) (px : float) (py : float) : bool = px >= 0. && px < float_of_int a.pw && py >= 0. && py < float_of_int a.ph

let inside (r : Treemap.rect) (u : float) (v : float) : bool = u >= r.x && u < r.x +. r.w && v >= r.y && v < r.y +. r.h

(* the deepest node under a point of the layout, and its index *)
let under (t : t) (u : float) (v : float) : int option =
  let found = ref None in
  Array.iteri (fun i (p : entry Treemap.placed) -> if inside p.rect u v then found := Some i) t.placed;
  !found

(* the line under a point of a file *)
let line_at (g : geometry) (r : Treemap.rect) (u : float) (v : float) : int =
  let col = int_of_float ((u -. r.x) /. g.colw) in
  (col * g.lpc) + int_of_float ((v -. r.y) /. g.cell_h)

(*****************************************************************************)
(* Colours *)
(*****************************************************************************)

(* claude: a colour per part of the repository, as codemap's archi_code
 * colours a file by its role (its directory: Main, Test, Core...) *)
let archi (path : string) : int * int * int =
  let first = match String.index_opt path '/' with Some i -> String.sub path 0 i | None -> path in
  match first with
  | "games" -> (70, 100, 220)
  | "apps" -> (170, 80, 210)
  | "gamekits" -> (50, 170, 170)
  | "appkits" -> (60, 170, 100)
  | "playground" -> (220, 170, 50)
  | "libs" -> (200, 90, 60)
  | _ -> (120, 120, 120)

let mix ((r, g, b) : int * int * int) (a : float) ((r2, g2, b2) : int * int * int) : int * int * int =
  let f x y = int_of_float ((a *. float_of_int x) +. ((1. -. a) *. float_of_int y)) in
  (f r r2, f g g2, f b b2)

let dark = (12, 10, 28)
let file_background (path : string) = mix (archi path) 0.18 (20, 22, 30)
let dir_colour (path : string) (depth : int) = mix (archi path) (0.12 +. (0.04 *. float_of_int (min depth 4))) dark
let palette : (int * int * int) array = Array.map Highlight_code.rgb Highlight_code.all

(*****************************************************************************)
(* Painting *)
(*****************************************************************************)

(* claude: the characters drawn (Vga_font's glyphs) from a cell this high
 * on the screen; below it, a cell is a block of its category's colour,
 * SeeSoft's picture *)
let text_px = 6.

let readable (c : camera) (g : geometry) : bool = g.cell_h *. c.z >= text_px

(* claude: the camera of the picture painted with [q] of the window's
 * pixels to a unit (Playground_platform.pixel_ratio): the same view, [q]
 * times more pixels -- a bigger zoom and a bigger area, so that every
 * function given it (to_px, clip, paint_code, readable) counts the
 * window's pixels. Why: the platform enlarges a bitmap to the window;
 * painted at the screen's units, a map on a big monitor had its letters
 * squeezed into a few pixels, then blown up and blurred. Painted at the
 * window's pixels, a line 6 units high is 13 pixels on a 4K monitor, its
 * glyph drawn nearly whole, and the platform shrinks the image back by [q]:
 * one of its pixels, one of the window's. *)
let at_ratio (c : camera) (q : float) : camera =
  let n x = max 1 (int_of_float (Float.round (float_of_int x *. q))) in
  { c with z = c.z *. q; a = { c.a with pw = n c.a.pw; ph = n c.a.ph } }

let fill (img : Rgba_image.t) (x0 : int) (y0 : int) (x1 : int) (y1 : int) ((r, g, b) : int * int * int) : unit =
  for y = y0 to y1 - 1 do
    for x = x0 to x1 - 1 do
      let i = 4 * ((y * img.width) + x) in
      Bigarray.Array1.unsafe_set img.rgba i r;
      Bigarray.Array1.unsafe_set img.rgba (i + 1) g;
      Bigarray.Array1.unsafe_set img.rgba (i + 2) b;
      Bigarray.Array1.unsafe_set img.rgba (i + 3) 255
    done
  done

(* a rectangle's pixels on the map, clipped: None if off it *)
let clip (c : camera) (r : Treemap.rect) : (int * int * int * int) option =
  let x0 = max 0 (int_of_float (Float.round (to_px c r.x))) and x1 = min c.a.pw (int_of_float (Float.round (to_px c (r.x +. r.w)))) in
  let y0 = max 0 (int_of_float (Float.round (to_py c r.y))) and y1 = min c.a.ph (int_of_float (Float.round (to_py c (r.y +. r.h)))) in
  if x1 <= x0 || y1 <= y0 then None else Some (x0, y0, x1, y1)

(* A file's code, each pixel found from the layout: the cell under it
 * (its column of lines, its line, its character), then, far away, the
 * cell's colour, and near, the pixel of the character's glyph under it
 * (a cell is 8 by 16 of the glyph's pixels, scaled). So zooming in turns
 * the blocks into letters with no text drawn: the same loop, one more
 * lookup.
 *
 * claude: the letters anti-aliased. A cell is rarely 8 by 16 pixels on
 * the screen: at 13 pixels high, one sample a pixel (nearest neighbour)
 * skips 3 of the glyph's 16 rows, a different 3 on each line, and the
 * letters come out ragged, their strokes appearing and vanishing -- hard
 * to read at exactly the sizes where there is just enough room. So in
 * glyph mode each pixel takes 2 by 2 samples, and its colour is the
 * character's blended over the background by how many of the 4 hit ink:
 * a stroke half on a pixel lights it half, as a font rasterizer does
 * (supersampling, a box filter). Far away, blocks, one sample is enough. *)
let pal_r = Array.map (fun (r, _, _) -> r) palette
let pal_g = Array.map (fun (_, g, _) -> g) palette
let pal_b = Array.map (fun (_, _, b) -> b) palette

let paint_code ~(aa : bool) (img : Rgba_image.t) (c : camera) (r : Treemap.rect) (g : geometry) (f : Code_file.t)
    ((x0, y0, x1, y1) : int * int * int * int) (bg : int * int * int) : unit =
  let n = Code_file.nlines f in
  let glyphs = readable c g in
  let ss = if glyphs && aa then 2 else 1 (* samples per pixel, each way *) in
  let sub k = (float_of_int k +. 0.5) /. float_of_int ss in
  (* claude: for each sample's x, once: its column of lines, its
   * character, and the glyph's pixel column in it *)
  let nx = (x1 - x0) * ss in
  let colx = Array.make nx 0 and chx = Array.make nx 0 and gx = Array.make nx 0 in
  for i = 0 to nx - 1 do
    let u = to_u c (float_of_int (x0 + (i / ss)) +. sub (i mod ss)) -. r.x in
    let col = int_of_float (Float.floor (u /. g.colw)) in
    let fc = (u -. (float_of_int col *. g.colw)) /. g.cell_w in
    colx.(i) <- col;
    chx.(i) <- int_of_float fc;
    gx.(i) <- min (Vga_font.width - 1) (int_of_float ((fc -. Float.of_int (int_of_float fc)) *. float_of_int Vga_font.width))
  done;
  let lcs = Array.make ss 0 and gys = Array.make ss 0 in
  let br, bgc, bb = bg in
  let rgba = img.rgba and cols = Code_file.cols and lpc = g.lpc in
  (* claude: the loops below allocate nothing (no tuples, no closures
   * returning pairs): at 4K they visit 7 million pixels, 4 samples each;
   * the cell of a sample is [(col * lpc + lc) * cols + ch], its category
   * [grid]'s byte, -1 off the file *)
  let cell_of xi lc =
    let col = Array.unsafe_get colx xi and ch = Array.unsafe_get chx xi in
    let line = (col * lpc) + lc in
    if line >= 0 && line < n && lc >= 0 && lc < lpc && ch < cols && ch >= 0 then (line * cols) + ch else -1
  in
  for y = y0 to y1 - 1 do
    for k = 0 to ss - 1 do
      let fl = (to_v c (float_of_int y +. sub k) -. r.y) /. g.cell_h in
      let lc = int_of_float (Float.floor fl) in
      lcs.(k) <- lc;
      gys.(k) <- min (Vga_font.height - 1) (int_of_float ((fl -. Float.of_int lc) *. float_of_int Vga_font.height))
    done;
    for x = x0 to x1 - 1 do
      let i = 4 * ((y * img.width) + x) in
      (* the samples: how many hit ink (or, far away, a character), and
       * whose colour *)
      let hits = ref 0 and ink = ref 0 in
      for ky = 0 to ss - 1 do
        for kx = 0 to ss - 1 do
          let xi = ((x - x0) * ss) + kx in
          let cell = cell_of xi lcs.(ky) in
          if cell >= 0 then begin
            let code = Char.code (Bytes.unsafe_get f.grid cell) in
            if code <> 0 && ((not glyphs) || Vga_font.bit (Char.code (Bytes.unsafe_get f.chars cell)) gx.(xi) gys.(ky)) then begin
              incr hits;
              ink := code
            end
          end
        done
      done;
      let all = ss * ss and h = !hits in
      if h = 0 then begin
        Bigarray.Array1.unsafe_set rgba i br;
        Bigarray.Array1.unsafe_set rgba (i + 1) bgc;
        Bigarray.Array1.unsafe_set rgba (i + 2) bb
      end
      else begin
        let k = !ink - 1 in
        Bigarray.Array1.unsafe_set rgba i (br + ((pal_r.(k) - br) * h / all));
        Bigarray.Array1.unsafe_set rgba (i + 1) (bgc + ((pal_g.(k) - bgc) * h / all));
        Bigarray.Array1.unsafe_set rgba (i + 2) (bb + ((pal_b.(k) - bb) * h / all))
      end;
      Bigarray.Array1.unsafe_set rgba (i + 3) 255
    done
  done

let paint ~(aa : bool) (t : t) (c : camera) : Rgba_image.t =
  let img = Rgba_image.create ~width:c.a.pw ~height:c.a.ph in
  fill img 0 0 c.a.pw c.a.ph dark;
  Array.iteri
    (fun i (p : entry Treemap.placed) ->
      match clip c p.rect with
      | None -> ()
      | Some ((x0, y0, x1, y1) as box) -> (
          match (p.node, t.geometry.(i)) with
          | Dir _, _ -> fill img x0 y0 x1 y1 (dir_colour p.path p.depth)
          | File (_, _, e), Some g ->
              let bg = file_background p.path in
              (* claude: a file too small to show anything is not lexed:
               * the whole repository's map opens without lexing it all *)
              if (x1 - x0) * (y1 - y0) < 40 && not (Lazy.is_val e.file) then fill img x0 y0 x1 y1 (mix (archi p.path) 0.5 bg)
              else paint_code ~aa img c p.rect g (Lazy.force e.file) box bg;
              (* a dark line on its top and left edges, between files *)
              if x1 - x0 > 6 && y1 - y0 > 6 then begin
                fill img x0 y0 x1 (y0 + 1) dark;
                fill img x0 y0 (x0 + 1) y1 dark
              end
          | File _, None -> ()))
    t.placed;
  img

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let clamp_cam (c : camera) : camera = { c with z = Float.max 0.5 (Float.min 400. c.z) }

(* the camera a step nearer its target: the zoom eased in its logarithm,
 * so that going in 100 times feels as steady as going in 2 *)
let ease (c : camera) (target : camera) : camera =
  let a = 0.22 in
  let z = Float.exp (Float.log c.z +. (a *. (Float.log target.z -. Float.log c.z))) in
  (* the centre moves so that the point the zoom goes to stays put: a
   * straight line in the layout at a rate scaled by the zoom's *)
  let near = Float.abs (Float.log (z /. target.z)) < 0.002 && Float.abs (c.cx -. target.cx) *. z < 0.3 && Float.abs (c.cy -. target.cy) *. z < 0.3 in
  if near then target else { c with cx = c.cx +. (a *. (target.cx -. c.cx)); cy = c.cy +. (a *. (target.cy -. c.cy)); z }

(* the directory round the view, a size bigger: where going up goes *)
let up (t : t) : camera =
  let c = t.target in
  let vw = float_of_int c.a.pw /. c.z and vh = float_of_int c.a.ph /. c.z in
  let best = ref None in
  Array.iter
    (fun (p : entry Treemap.placed) ->
      match p.node with
      | Dir _ when inside p.rect c.cx c.cy && (p.rect.w > vw *. 1.3 || p.rect.h > vh *. 1.3) -> (
          match !best with
          | Some (b : entry Treemap.placed) when b.rect.w *. b.rect.h <= p.rect.w *. p.rect.h -> ()
          | _ -> best := Some p)
      | _ -> ())
    t.placed;
  match !best with Some p when p.depth > 0 -> fit c.a p.rect | _ -> home c.a

(* claude: the magnifying glass (below): round, a reading glass (80
 * columns), or none, o going from one to the next, one setting for every
 * map (tinybox's panel and its explorer) *)
type glass = Round | Reading | No_glass

let glass_shape = ref Round
let cycle_glass () = glass_shape := match !glass_shape with Round -> Reading | Reading -> No_glass | No_glass -> Round
let glass_name () = match !glass_shape with Round -> "round" | Reading -> "wide" | No_glass -> "none"

let update (computer : computer) ~(pressed : string -> bool) ~(arrow : string option) (t : t) : t * action =
  let mouse = computer.mouse in
  let a = t.target.a in
  let mpx = px_of a mouse.mx and mpy = py_of a mouse.my in
  let on_map = on a mpx mpy in
  let target = t.target in
  (* the keys *)
  let pan dx dy = { target with cx = target.cx +. (dx /. target.z); cy = target.cy +. (dy /. target.z) } in
  let target =
    match arrow with
    | Some "ArrowLeft" -> pan (-80.) 0.
    | Some "ArrowRight" -> pan 80. 0.
    | Some "ArrowUp" -> pan 0. (-80.)
    | Some "ArrowDown" -> pan 0. 80.
    | _ -> target
  in
  (* claude: the glass's shape, the panel's too *)
  if pressed "o" then cycle_glass ();
  let t =
    if pressed "t" then
      let algo : Treemap.algo = match t.algo with Squarified -> Slice_and_dice | Slice_and_dice -> Squarified in
      let placed, geometry = relayout a algo t.entries in
      { t with algo; placed; geometry; painted = None }
    else t
  in
  let target = if pressed "Home" || pressed "0" || pressed "t" then home a else target in
  let target = if pressed "Backspace" || (mouse.mrdown && not t.before_right) then up { t with target } else target in
  let target = if pressed "=" || pressed "+" then { target with z = target.z *. 1.5 } else if pressed "-" then { target with z = target.z /. 1.5 } else target in
  (* the wheel: zoom at the mouse, the point under it staying under it *)
  let target =
    if mouse.mwheel <> 0. && on_map then
      let u = to_u target mpx and v = to_v target mpy in
      let z = (clamp_cam { target with z = target.z *. (1.25 ** mouse.mwheel) }).z in
      { target with cx = u -. ((mpx -. (float_of_int a.pw /. 2.)) /. z); cy = v -. ((mpy -. (float_of_int a.ph /. 2.)) /. z); z }
    else target
  in
  (* a drag pans, at once *)
  let t, target, cam_now =
    match t.drag with
    | Some (x0, y0, c0) when mouse.mdown ->
        let dx = mouse.mx -. x0 and dy = mouse.my -. y0 in
        let moved = t.dragged || Float.abs dx +. Float.abs dy > 5. in
        let c = if moved then { c0 with cx = c0.cx -. (dx /. c0.z); cy = c0.cy +. (dy /. c0.z) } else target in
        ({ t with dragged = moved }, c, moved)
    | None when mouse.mdown && on_map -> ({ t with drag = Some (mouse.mx, mouse.my, target); dragged = false }, target, false)
    | _ -> (t, target, false)
  in
  let clicked = (mouse.mclick || mouse.mdouble) && on_map && not t.dragged in
  let t = if not mouse.mdown then { t with drag = None; dragged = (if mouse.mclick then false else t.dragged) } else t in
  (* a click: fly to what is under it, or open the file already there *)
  let target, action =
    if pressed "Escape" then (target, Close)
    else if clicked || pressed "Enter" then
      let u = to_u t.cam mpx and v = to_v t.cam mpy in
      match under t u v with
      | None -> (target, Stay)
      | Some i -> (
          let p = t.placed.(i) in
          match (p.node, t.geometry.(i)) with
          | File (_, _, e), Some g ->
              let there = fit a p.rect in
              let close_enough =
                readable (at_ratio t.cam (Playground_platform.pixel_ratio ())) g || Float.abs (Float.log (t.cam.z /. there.z)) < 0.1
              in
              if close_enough || mouse.mdouble || pressed "Enter" then (target, Open (Lazy.force e.file, line_at g p.rect u v)) else (there, Stay)
          | Dir _, _ -> (fit a p.rect, Stay)
          | _ -> (target, Stay))
    else (target, Stay)
  in
  let target = clamp_cam target in
  let cam = if cam_now then target else ease t.cam target in
  ({ t with target; cam; before_right = mouse.mrdown }, action)

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let yellow = rgb 255 215 70
let ink = rgb 228 228 240
let dim = rgb 140 140 180

let frame (a : area) (color : color) (x0 : float) (y0 : float) (x1 : float) (y1 : float) (th : float) : shape list =
  let w = x1 -. x0 and h = y1 -. y0 in
  let cx = sx a ((x0 +. x1) /. 2.) and cy = sy a ((y0 +. y1) /. 2.) in
  [
    rectangle color w th |> move cx (sy a y0);
    rectangle color w th |> move cx (sy a y1);
    rectangle color th h |> move (sx a x0) cy;
    rectangle color th h |> move (sx a x1) cy;
  ]

(* words centred at a pixel of the map, [size] high *)
let label (a : area) ?(alpha = 1.) (color : color) (size : float) (px : float) (py : float) (s : string) : shape =
  words color s |> scale (size /. words_font_size) |> move (sx a px) (sy a py) |> fade alpha

let basename (path : string) : string = match String.rindex_opt path '/' with Some i -> String.sub path (i + 1) (String.length path - i - 1) | None -> path

(* claude: labels placed greedily, the most important first, each only
 * inside the map where it overlaps none placed before it: the map's names never pile up
 * (codemap draws them all, over each other) *)
type candidate = { rank : float; box : float * float * float * float; shape : shape }

let place (a : area) (cands : candidate list) : shape list =
  let overlaps (a0, b0, a1, b1) (c0, d0, c1, d1) = a0 < c1 && c0 < a1 && b0 < d1 && d0 < b1 in
  let on_map (x0, y0, x1, y1) = x0 >= 0. && y0 >= 0. && x1 <= float_of_int a.pw && y1 <= float_of_int a.ph in
  let placed = ref [] in
  List.iter
    (fun c -> if on_map c.box && not (List.exists (overlaps c.box) !placed) then placed := c.box :: !placed)
    (List.stable_sort (fun a b -> compare b.rank a.rank) cands);
  List.filter_map (fun c -> if List.memq c.box !placed then Some c.shape else None) cands

(* a label's candidate, centred at (px, py), [size] high *)
let candidate (a : area) ~(rank : float) ?alpha (color : color) (size : float) (px : float) (py : float) (s : string) : candidate =
  let w = 0.5 *. size *. float_of_int (String.length s) in
  { rank; box = (px -. (w /. 2.), py -. (size /. 2.), px +. (w /. 2.), py +. (size /. 2.)); shape = label a ?alpha color size px py s }

(* the names over the map: directories', big and faint (codemap's); files';
 * and, from afar, what each file defines, bigger the more it matters *)
let labels (t : t) (c : camera) (q : float) : shape list =
  let a = c.a in
  (* readable as painted: in the window's pixels *)
  let readable c g = readable (at_ratio c q) g in
  let candidate = candidate a and label = label a in
  let dirs = ref [] and files = ref [] and defs = ref [] in
  Array.iteri
    (fun i (p : entry Treemap.placed) ->
      match clip c p.rect with
      | None -> ()
      | Some (x0, y0, x1, y1) -> (
          let w = float_of_int (x1 - x0) and h = float_of_int (y1 - y0) in
          let name = match p.node with Dir (n, _) -> n | File (n, _, _) -> n in
          let fit_size len = w /. (0.55 *. float_of_int (max 1 len)) in
          match (p.node, t.geometry.(i)) with
          | Dir (_, kids), _ when p.depth > 0 || p.path <> "" ->
              (* codemap's: the name big and faint over the directory,
               * but for one filling the map *)
              let s = Float.min (fit_size (String.length name)) (Float.min (h /. 4.) 90.) in
              if s >= 12. && w *. h < 0.4 *. float_of_int (a.pw * a.ph) then
                dirs := candidate ~rank:s ~alpha:0.35 ink s ((float_of_int x0 +. float_of_int x1) /. 2.) ((float_of_int y0 +. float_of_int y1) /. 2.) name :: !dirs;
              (* ours: its path on a tab at its top left, the way back to it in
               * the repository; only a directory with files of its own, as
               * the path says its parents' names *)
              let size = 13. in
              let tw = (0.5 *. size *. float_of_int (String.length p.path)) +. 8. in
              let has_files = List.exists (function Treemap.File _ -> true | Dir _ -> false) kids in
              if has_files && w >= tw && h >= 40. then begin
                let tx = to_px c p.rect.x +. 2. and ty = to_py c p.rect.y +. 2. in
                let tx = Float.max 0. tx and ty = Float.max 0. ty in
                let r, gg, b = archi p.path in
                let shape =
                  group
                    [
                      rectangle (rgb 12 10 28) tw (size +. 6.) |> move (sx a (tx +. (tw /. 2.))) (sy a (ty +. ((size +. 6.) /. 2.)));
                      label (rgb (min 255 (r + 60)) (min 255 (gg + 60)) (min 255 (b + 60))) size (tx +. (tw /. 2.)) (ty +. ((size +. 6.) /. 2.)) p.path;
                    ]
                in
                files := { rank = 300. -. float_of_int p.depth; box = (tx, ty, tx +. tw, ty +. size +. 6.); shape } :: !files
              end
          | File (_, _, e), Some g ->
              let s = Float.min (fit_size (String.length name)) (Float.min (h /. 3.) 20.) in
              if readable c g then
                files := candidate ~rank:100. yellow (Float.min 14. s) (to_px c p.rect.x +. (0.25 *. float_of_int (String.length name) *. Float.min 14. s) +. 4.) (to_py c p.rect.y +. 8.) name :: !files
              else if s >= 10. then
                files := candidate ~rank:(50. +. s) ~alpha:0.9 ink s ((float_of_int x0 +. float_of_int x1) /. 2.) ((float_of_int y0 +. float_of_int y1) /. 2.) name :: !files;
              (* the semantic zoom: definitions written over the code *)
              if (not (readable c g)) && Lazy.is_val e.file then
                List.iter
                  (fun (line, def, cat) ->
                    let size = Float.min 22. (g.cell_h *. c.z *. Highlight_code.emphasis cat *. 1.6) in
                    if size >= 9. && line < Code_file.nlines (Lazy.force e.file) then begin
                      let col = line / g.lpc and lc = line mod g.lpc in
                      let px = to_px c (p.rect.x +. (float_of_int col *. g.colw)) and py = to_py c (p.rect.y +. ((float_of_int lc +. 0.5) *. g.cell_h)) in
                      let wd = 0.5 *. size *. float_of_int (String.length def) in
                      if py >= 0. && py < float_of_int a.ph && px +. wd > 0. && px < float_of_int a.pw then
                        let r, gg, b = Highlight_code.rgb cat in
                        defs := candidate ~rank:(Highlight_code.emphasis cat *. size) (rgb r gg b) size (px +. (wd /. 2.)) py def :: !defs
                    end)
                  (Lazy.force e.file).defs
          | _ -> ()))
    t.placed;
  (* the directories' names faint under the rest, placed among themselves *)
  place a !dirs @ place a (!files @ !defs)

let view ?(chrome = true) (computer : computer) (t : t) : shape list =
  let c = t.cam in
  let a = c.a in
  (* claude: the picture at the window's resolution (at_ratio),
   * anti-aliased, when the camera is still; while it moves, a quick one:
   * half the screen's resolution, one sample a pixel -- a zoom repaints
   * every frame, and the sharp picture (7 million pixels at 4K, 4 samples
   * each) would make it stutter; it comes the frame after the camera
   * stops *)
  let q = Float.max 0.5 (Float.min 3. (Playground_platform.pixel_ratio ())) in
  let still = t.last = Some c in
  t.last <- Some c;
  let want = if still then q else Float.min q 0.5 in
  let img =
    match t.painted with
    | Some (pc, pq, img) when pc = c && pq = want -> img
    | _ ->
        let img = paint ~aa:still t (at_ratio c want) in
        t.painted <- Some (c, want, img);
        img
  in
  let mouse = computer.mouse in
  let mpx = px_of a mouse.mx and mpy = py_of a mouse.my in
  let u = to_u c mpx and v = to_v c mpy in
  let hovered = if on a mpx mpy then under t u v else None in
  let box color th (x0, y0, x1, y1) = frame a color (float_of_int x0) (float_of_int y0) (float_of_int x1) (float_of_int y1) th in
  let marks =
    Array.to_list t.placed
    |> List.concat_map (fun (p : entry Treemap.placed) ->
           match p.node with
           | File (_, _, e) when List.mem e.path t.marked -> ( match clip c p.rect with Some b -> box yellow 3. b | None -> [])
           | _ -> [])
  in
  let hover, status =
    match hovered with
    | Some i -> (
        let p = t.placed.(i) in
        let frame = match clip c p.rect with Some b -> box white 1.5 b | None -> [] in
        match (p.node, t.geometry.(i)) with
        | File (_, _, e), Some g ->
            let line = line_at g p.rect u v in
            let def =
              if Lazy.is_val e.file then
                List.fold_left (fun acc (l, name, _) -> if l <= line then Some name else acc) None (Lazy.force e.file).defs
              else None
            in
            (frame, Printf.sprintf "%s:%d%s   (%d lines)" e.path (line + 1) (match def with Some d -> "   " ^ d | None -> "") e.nlines)
        | _ -> (frame, p.path))
    | None -> ([], "")
  in
  let algo = match t.algo with Squarified -> "squarified" | Slice_and_dice -> "slice and dice" in
  let screen = computer.screen in
  (if chrome then [ rectangle (rgb 12 10 28) screen.width screen.height ] else [])
  @ [ bitmap (float_of_int a.pw) (float_of_int a.ph) img |> move (sx a (float_of_int a.pw /. 2.)) (sy a (float_of_int a.ph /. 2.)) ]
  @ labels t c q @ marks
  @
  if not chrome then []
  else
    hover
    @ [
        words yellow t.title |> scale (22. /. words_font_size) |> move 0. (screen.top -. 45.);
        words ink status |> scale (14. /. words_font_size) |> move 0. (screen.bottom +. 45.);
        words dim
          (Printf.sprintf "wheel zoom   drag pan   click fly in, again open   right click up   t layout (%s)   o glass (%s)   0 all   esc back" algo (glass_name ()))
        |> scale (12. /. words_font_size)
        |> move 0. (screen.bottom +. 18.);
      ]

(*****************************************************************************)
(* The magnifying glass *)
(*****************************************************************************)

(* claude: a glass over the map, the part under the cursor closer. Not a
 * zoom of the map's picture (that would only enlarge its pixels, blurred,
 * the problem pixel_ratio solved): the part under the glass painted
 * again, by the same paint, with a camera [power] times closer, at the
 * window's resolution, anti-aliased. The power is chosen for the file
 * under the cursor, so that its lines come out about 16 units high, the
 * VGA font's own size: readable whatever the file's size. The glass's
 * shape is its picture's pixels made transparent (alpha 0, a soft edge):
 * the playground has no clipping. Painted again only when the cursor
 * moves.
 *
 * Two glasses. A round one, a glance at the code under the mouse. A
 * reading glass, the rectangular
 * kind laid over a page, where one reads: 80 columns of 8 units (640,
 * and a margin) by some 16 lines, whole lines of code rather than a
 * keyhole of them. o goes from one to the other, and to none (the
 * glass always enlarges, even over code big enough to read: none is the
 * way to be rid of it). *)

(* the file under the mouse (its rectangle and geometry), and the power
 * that makes its lines 16 units high; None when the mouse is off the map *)
let under_glass (computer : computer) (t : t) =
  let c = t.cam in
  let a = c.a in
  let mouse = computer.mouse in
  let mpx = px_of a mouse.mx and mpy = py_of a mouse.my in
  if not (on a mpx mpy) then None
  else
    let u = to_u c mpx and v = to_v c mpy in
    let file = match under t u v with Some i -> ( match t.geometry.(i) with Some g -> Some (t.placed.(i).rect, g) | None -> None) | None -> None in
    let power = match file with Some (_, g) -> float_of_int Vga_font.height /. (g.cell_h *. c.z) | None -> 4. in
    Some (u, v, file, power)

(* the part of the map [lc] sees, painted, the pixels of its picture out
 * of the glass's shape transparent: [alpha w h fx fy], w and h the
 * picture's size, fx fy a pixel's centre, gives its alpha *)
let glass_picture (t : t) (lc : camera) (alpha : float -> float -> float -> float -> int option) : Rgba_image.t * float =
  let q = Float.max 0.5 (Float.min 3. (Playground_platform.pixel_ratio ())) in
  let img =
    match t.lens with
    | Some (pc, img) when pc = lc && img.width = (at_ratio lc q).a.pw -> img
    | _ ->
        let img = paint ~aa:true t (at_ratio lc q) in
        let w = float_of_int img.width and h = float_of_int img.height in
        for y = 0 to img.height - 1 do
          for x = 0 to img.width - 1 do
            match alpha w h (float_of_int x +. 0.5) (float_of_int y +. 0.5) with
            | Some al -> Bigarray.Array1.unsafe_set img.rgba ((4 * ((y * img.width) + x)) + 3) al
            | None -> ()
          done
        done;
        t.lens <- Some (lc, img);
        img
  in
  (img, q)

(* one pixel of soft edge, [dist] from a circle's centre of radius [r] *)
let soft_edge (r : float) (dist : float) : int = if dist <= r -. 1. then 255 else if dist >= r then 0 else int_of_float ((r -. dist) *. 255.)

(* the round glass: centred on the point under the cursor *)
let lens_radius = 185.

let lens (computer : computer) (t : t) : shape list =
  match under_glass computer t with
  | None -> []
  | Some (u, v, _, power) ->
      let power = Float.max 2. (Float.min 10. power) in
      let d = int_of_float (2. *. lens_radius) in
      let lc = { cx = u; cy = v; z = t.cam.z *. power; a = { (t.cam.a) with pw = d; ph = d } } in
      let img, _ =
        glass_picture t lc (fun w _ fx fy ->
            let r = w /. 2. in
            let dx = fx -. r and dy = fy -. r in
            Some (soft_edge r (Float.sqrt ((dx *. dx) +. (dy *. dy)))))
      in
      let x = computer.mouse.mx and y = computer.mouse.my in
      let r = lens_radius in
      [
        (* the handle, down and to the right, as a magnifying glass is held *)
        rectangle (rgb 90 60 30) 22. 110. |> move 0. (-.(r +. 50.)) |> rotate 45. |> move x y;
        rectangle (rgb 150 150 160) 26. 18. |> move 0. (-.(r +. 4.)) |> rotate 45. |> move x y;
        (* the rim *)
        circle (rgb 40 40 50) (r +. 9.) |> move x y;
        circle (rgb 190 190 205) (r +. 6.) |> move x y;
        circle (rgb 12 10 28) (r +. 1.) |> move x y;
        bitmap (2. *. r) (2. *. r) img |> move x y;
        (* a glint on the glass *)
        oval white (r *. 0.5) (r *. 0.18) |> rotate 35. |> move (x -. (r *. 0.45)) (y +. (r *. 0.55)) |> fade 0.12;
        words (rgb 190 190 205) (Printf.sprintf "x%.0f" power) |> scale (12. /. words_font_size) |> move (x +. (r *. 0.62)) (y -. (r *. 0.85));
      ]

(* the reading glass. Over a file, it lines up with the start of the
 * column of lines under the mouse (a file is laid out in several), so it
 * shows whole lines from their first character, not the end of one column
 * and the start of the next; up and down, the line under the mouse is
 * drawn where the mouse is. It stays on the screen. *)
let reading_w = 660.
let reading_h = 272.
let reading_corner = 22.

(* a rectangle with rounded corners, from rectangles and circles (the
 * playground has no rounded rectangle) *)
let rounded (color : color) (w : number) (h : number) (r : number) : shape =
  group
    [
      rectangle color w (h -. (2. *. r));
      rectangle color (w -. (2. *. r)) h;
      circle color r |> move ((w /. 2.) -. r) ((h /. 2.) -. r);
      circle color r |> move (-.((w /. 2.) -. r)) ((h /. 2.) -. r);
      circle color r |> move ((w /. 2.) -. r) (-.((h /. 2.) -. r));
      circle color r |> move (-.((w /. 2.) -. r)) (-.((h /. 2.) -. r));
    ]

let reading_glass (computer : computer) (t : t) : shape list =
  match under_glass computer t with
  | None -> []
  | Some (u, v, file, power) ->
      let power = Float.max 1.5 (Float.min 10. power) in
      let c = t.cam in
      let a = c.a in
      let mouse = computer.mouse in
      let z = c.z *. power in
      let margin = 10. in
      let screen = computer.screen in
      let keep lo hi x = Float.max lo (Float.min hi x) in
      let on_screen_x x = keep (screen.left +. (reading_w /. 2.) +. 4.) (screen.right -. (reading_w /. 2.) -. 4.) x in
      let gy = keep (screen.bottom +. (reading_h /. 2.) +. 4.) (screen.top -. (reading_h /. 2.) -. 4.) mouse.my in
      (* across: the column's start at the glass's left margin, the glass
       * over the column on the map; else the point under the mouse where
       * the mouse is *)
      let gx, cx =
        match file with
        | Some (r, g) ->
            let start = r.x +. (Float.floor ((u -. r.x) /. g.colw) *. g.colw) in
            (on_screen_x (sx a (to_px c start) +. (reading_w /. 2.) -. margin), start +. (((reading_w /. 2.) -. margin) /. z))
        | None ->
            let gx = on_screen_x mouse.mx in
            (gx, u +. ((gx -. mouse.mx) /. z))
      in
      (* down: the line under the mouse drawn where the mouse is *)
      let lc = { cx; cy = v -. ((gy -. mouse.my) /. z); z; a = { a with pw = int_of_float reading_w; ph = int_of_float reading_h } } in
      let img, _ =
        glass_picture t lc (fun w h fx fy ->
            (* rounded corners: the distance to the corner's centre *)
            let r = reading_corner *. (w /. reading_w) in
            let dx = Float.max 0. (Float.max (r -. fx) (fx -. (w -. r))) and dy = Float.max 0. (Float.max (r -. fy) (fy -. (h -. r))) in
            if dx > 0. && dy > 0. then Some (soft_edge r (Float.sqrt ((dx *. dx) +. (dy *. dy)))) else None)
      in
      let w = reading_w and h = reading_h and r = reading_corner in
      [
        (* the handle, from the bottom right corner, as a reading glass is held *)
        rectangle (rgb 90 60 30) 24. 120. |> move 0. (-60.) |> rotate 45. |> move (gx +. (w /. 2.) -. 10.) (gy -. (h /. 2.) +. 10.);
        (* the rim *)
        rounded (rgb 40 40 50) (w +. 18.) (h +. 18.) (r +. 9.) |> move gx gy;
        rounded (rgb 190 190 205) (w +. 12.) (h +. 12.) (r +. 6.) |> move gx gy;
        rounded (rgb 12 10 28) (w +. 2.) (h +. 2.) (r +. 1.) |> move gx gy;
        bitmap w h img |> move gx gy;
        (* a glint on the glass *)
        oval white (w *. 0.3) (h *. 0.1) |> rotate 8. |> move (gx -. (w *. 0.28)) (gy +. (h *. 0.36)) |> fade 0.1;
        words (rgb 190 190 205) (Printf.sprintf "x%.1f" power) |> scale (12. /. words_font_size) |> move (gx +. (w /. 2.) -. 24.) (gy +. (h /. 2.) +. 1.);
      ]

let glass (computer : computer) (t : t) : shape list =
  match !glass_shape with Round -> lens computer t | Reading -> reading_glass computer t | No_glass -> []
