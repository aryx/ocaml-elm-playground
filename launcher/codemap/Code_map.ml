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
  mutable painted : (camera * Rgba_image.t) option; (* the picture of [cam] *)
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
    before_right = false; painted = None }

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
 * (a cell is 8 by 16 of the glyph's pixels, scaled: nearest neighbour).
 * So zooming in turns the blocks into letters with no text drawn: the
 * same loop, one more lookup. *)
let paint_code (img : Rgba_image.t) (c : camera) (r : Treemap.rect) (g : geometry) (f : Code_file.t) ((x0, y0, x1, y1) : int * int * int * int)
    (bg : int * int * int) : unit =
  let n = Code_file.nlines f in
  let glyphs = readable c g in
  (* claude: for each pixel's x, once: its column of lines, its character,
   * and the glyph's pixel column in it *)
  let colx = Array.make (x1 - x0) 0 and chx = Array.make (x1 - x0) 0 and gx = Array.make (x1 - x0) 0 in
  for i = 0 to x1 - x0 - 1 do
    let u = to_u c (float_of_int (x0 + i) +. 0.5) -. r.x in
    let col = int_of_float (u /. g.colw) in
    let fc = (u -. (float_of_int col *. g.colw)) /. g.cell_w in
    colx.(i) <- col;
    chx.(i) <- int_of_float fc;
    gx.(i) <- int_of_float ((fc -. Float.of_int (int_of_float fc)) *. float_of_int Vga_font.width)
  done;
  let br, bgc, bb = bg in
  for y = y0 to y1 - 1 do
    let fl = (to_v c (float_of_int y +. 0.5) -. r.y) /. g.cell_h in
    let lc = int_of_float fl in
    let gy = int_of_float ((fl -. Float.of_int lc) *. float_of_int Vga_font.height) in
    for x = x0 to x1 - 1 do
      let col = colx.(x - x0) and ch = chx.(x - x0) in
      let line = (col * g.lpc) + lc in
      let cell = (line * Code_file.cols) + ch in
      let code = if line < n && lc < g.lpc && ch < Code_file.cols && ch >= 0 then Char.code (Bytes.unsafe_get f.grid cell) else 0 in
      let code =
        if code <> 0 && glyphs && not (Vga_font.bit (Char.code (Bytes.unsafe_get f.chars cell)) gx.(x - x0) gy) then 0 else code
      in
      let i = 4 * ((y * img.width) + x) in
      let r, gg, b = if code = 0 then (br, bgc, bb) else palette.(code - 1) in
      Bigarray.Array1.unsafe_set img.rgba i r;
      Bigarray.Array1.unsafe_set img.rgba (i + 1) gg;
      Bigarray.Array1.unsafe_set img.rgba (i + 2) b;
      Bigarray.Array1.unsafe_set img.rgba (i + 3) 255
    done
  done

let paint (t : t) (c : camera) : Rgba_image.t =
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
              else paint_code img c p.rect g (Lazy.force e.file) box bg;
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
              let close_enough = g.cell_h *. t.cam.z >= text_px || Float.abs (Float.log (t.cam.z /. there.z)) < 0.1 in
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
let labels (t : t) (c : camera) : shape list =
  let a = c.a in
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
  let img =
    match t.painted with
    | Some (pc, img) when pc = c -> img
    | _ ->
        let img = paint t c in
        t.painted <- Some (c, img);
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
  @ labels t c @ marks
  @
  if not chrome then []
  else
    hover
    @ [
        words yellow t.title |> scale (22. /. words_font_size) |> move 0. (screen.top -. 45.);
        words ink status |> scale (14. /. words_font_size) |> move 0. (screen.bottom +. 45.);
        words dim
          (Printf.sprintf "wheel zoom   drag pan   click fly in, again open   right click up   t layout (%s)   0 all   esc back" algo)
        |> scale (12. /. words_font_size)
        |> move 0. (screen.bottom +. 18.);
      ]
