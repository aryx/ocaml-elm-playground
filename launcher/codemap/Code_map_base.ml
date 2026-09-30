(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Code_map_base.mli.
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

(* claude: a list appended as @ does, but in constant stack: OCaml
 * 4.14's @ recurses as deep as its left list, and a browser's stack is
 * small -- a street's thousands of shapes overflowed it every frame, the
 * web page frozen (the author: a on TinyInvaders.ml). The modules
 * opening this one see it as @. *)
let ( @ ) (a : 'a list) (b : 'a list) : 'a list = match b with [] -> a | _ -> List.rev_append (List.rev a) b

(* claude: List.map and List.mapi in constant stack too, for the same
 * reason: a search's hits (Map_v2) are tens of thousands for a letter
 * typed. Seen as List.map by the modules opening this one. *)
module List = struct
  include List

  let map f l = List.rev (List.rev_map f l)
  let mapi f l = List.rev (snd (List.fold_left (fun (i, acc) x -> (i + 1, f i x :: acc)) (0, []) l))
end

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
  mutable moving : bool; (* it was not, this frame (view): no glass *)
  mutable lens : (camera * Rgba_image.t) option; (* the magnifying glass's last picture, and its camera *)
  order : (string, int) Hashtbl.t; (* claude: a file's place in the reading order, when numbered *)
  colours : (string * (int * int * int)) list; (* claude: a .codemapconfig's (archi) *)
  mutable jumped : (int * (int * int)) option; (* claude: the binding a click on a name went to: its file's index in [placed], its line and column *)
  (* claude: across files (plan_codemap_naming.md, level 3): where the
   * jumps came from (b goes back), the places to choose from when a name
   * has several as near, a word for the status line, and the last
   * search, kept while the mouse stays on its name *)
  mutable back : (camera * (int * (int * int)) option) list;
  mutable choices : Code_names.candidate list option;
  mutable note : string;
  mutable found : ((string * int * int) * (Code_names.candidate list * bool)) option;
  roots : string list; (* claude: the projects' tops (Code_names.find) *)
  style : style; (* claude: how the map is drawn (Map_classic, ...) *)
  mutable index : Code_names.index option; (* claude: its files indexed, once (index_of) *)
  mutable rank : Code_rank.t option; (* claude: its definitions' uses, once (rank_of) *)
  mutable search : search option; (* claude: the search box (/, Map_v2), while open *)
  mutable search_all : Code_search.hit array option; (* claude: what a search looks among, gathered once *)
  mutable tour_on : (Code_guide.tour * int) option; (* claude: a config's tour under way, its stop (n, p) *)
  mutable marks : mark list; (* claude: searches kept, each lit in its colour, all at once (ctrl+Enter in the search) *)
  mutable mark_group : int; (* claude: the marks lit: -1 none, 0 those kept, k the configs' k-th (m cycling) *)
  mutable layer : int; (* claude: the layer shown, the map coloured by a measure (l cycling): 0 none, 1 the call stack *)
  mutable guide_marks : (string * mark list) list option; (* claude: the configs' marks as the map's, their names, made once *)
  mutable flight : flight option; (* claude: a smooth flight under way (a search's, a jump's) *)
  mutable pointer : (float * float) option; (* claude: the layout's point under the mouse, when on the map (view's) *)
  mutable focus : int; (* claude: the unit looked at, its index in [placed] (0: the root), when the style moves by units *)
  guide : Code_guide.t; (* claude: what the directories' .codemapconfig say (plan_codemap_v2.md) *)
  mutable street : bool; (* claude: at the ground, the file with what it uses (a: Code_street) *)
  mutable street_mode : int; (* claude: 1 what it uses, on the left; 2 what uses it, on the right; 3 both (a cycling) *)
  mutable clock : float; (* claude: the frame's time (view's), for what pulses *)
  mutable xray : bool; (* claude: the skeletons shown, the rest in the shade (x: Map_v2) *)
  mutable xray_n : int; (* claude: which of the skeletons at hand the X-ray shows (x again: the next) *)
  mutable peek : (string * int * int) option; (* claude: a definition's body shown readable over the map: its file, first and last lines (a click at the ground or the street) *)
  mutable peek_scroll : int; (* claude: the peek's first line shown, a long section's scrolled by the wheel *)
  mutable peek_stack : ((string * int * int) * int) list; (* claude: the peeks under it, and their scrolls: a peek of a peek (a click on a name in one) *)
  fan_in : (string, int) Hashtbl.t Lazy.t; (* claude: each module's fan-in, the files naming it (Code_deps.fan_in): how central *)
  counted : Code_rank.t Lazy.t option; (* claude: the uses counted once for every map of the same sources (Codemap), rank_of's *)
  mutable morph : (string Transition.t * float) option; (* claude: the layout's rectangles moving from another layout's places, since a time (Transition: a folder laid out anew) *)
  mutable help : bool; (* claude: h, every key explained *)
  top_kept : bool; (* claude: a lone top directory drawn (relayout) *)
  beyond : entry list; (* claude: sources not drawn but resolved against, peeked at (a program's map: the rest of the repository) *)
  mutable wheel_debt : float; (* claude: the wheel's notches not yet a step, and when the last step was *)
  mutable wheel_at : float;
}

(* claude: a flight from one camera to another, zooming out and back in
 * (van Wijk and Nuij), from a time on (nan: the next frame's) *)
(* claude: the search box: what is typed, the hit chosen (up and down),
   among the files shown only ([here], / typed first) or all; its hits,
   kept for the query they are of *)
and search = { mutable query : string; mutable sel : int; mutable here : bool; mutable hits : (string * bool) * Code_search.hit list }

(* claude: a mark: a query kept, its colour, its hits found once *)
and mark = { mquery : string; mcolour : int * int * int; msay : string option; mutable mhits : Code_search.hit list option }

and flight = { from : camera; dest : camera; mutable start : float; duration : float }

(* claude: a style: the map's picture (the directories, the files, their
 * code) and the names over it, the rest (the camera, the names lit and
 * clicked, the glass) being every style's *)
and style = {
  sname : string;
  paint : aa:bool -> t -> camera -> Rgba_image.t;
  labels : t -> camera -> float -> shape list;
  (* claude: the definition a label under a pixel of the map names, if
   * the style's labels name any (its file, line and name) *)
  pick : t -> camera -> float -> float -> float -> (string * int * int) option; (* claude: its file, line and column (Map_v2's ground, street and region panels) *)
  (* claude: the directory or file whose name is under a pixel of the map,
     if the style's names are clickable (Map_v2's): a click flies to it *)
  unit_at : t -> camera -> float -> float -> float -> int option;
  (* claude: the camera moves a unit at a time (Code_units: Map_v2's), or
     freely (the others') *)
  units : bool;
}

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
    { k; lpc; colw; cell_w = colw /. float_of_int Code_file.shown; cell_h = r.h /. float_of_int lpc }
  in
  (* the k whose cells are closest to 2 high for 1 wide *)
  let score g = Float.abs (Float.log (g.cell_h /. g.cell_w /. 2.)) in
  let rec best k acc = if k > min n 64 then acc else best (k + 1) (let g = make k in if score g < score acc then g else acc) in
  best 2 (make 1)

let relayout ?links ?(top_kept = false) (a : area) (algo : Treemap.algo) (entries : entry list) : entry Treemap.placed array * geometry option array =
  let tree = Treemap.of_paths (List.map (fun e -> (e.path, float_of_int (max 1 e.nlines), e)) entries) in
  (* claude: [top_kept]: a lone top directory drawn, not merged into the
   * root (whose name is never drawn): a selection's files in their
   * folder, named (a search's shift+Enter, the author: "showed also in
   * their enclosing folder name") *)
  let tree =
    match tree with
    | Dir ("", [ Dir (sub, kids) ]) when top_kept -> Treemap.Dir ("", [ Treemap.fold_singletons (Dir (sub, kids)) ])
    | tree -> Treemap.fold_singletons tree
  in
  (* claude: layered by who uses whom, given the files' links *)
  let bands = Option.map (fun l -> Code_layers.compute l tree) links in
  let placed = Array.of_list (Treemap.layout ?bands algo (root_rect a) tree) in
  (placed, Array.map (fun (p : entry Treemap.placed) -> match p.node with File (_, _, e) -> Some (geometry_of p.rect e.nlines) | Dir _ -> None) placed)

let fit (a : area) (r : Treemap.rect) : camera =
  { cx = r.x +. (r.w /. 2.); cy = r.y +. (r.h /. 2.); z = 0.96 *. Float.min (float_of_int a.pw /. r.w) (float_of_int a.ph /. r.h); a }

let home (a : area) : camera = { (fit a (root_rect a)) with z = 1. }

let make ?(fan_in = lazy (Hashtbl.create 1)) ?counted ?(top_kept = false) ?(numbered = false) ?(colours = []) ?(roots = []) ?(guide = Code_guide.empty) ?(beyond = []) ~(style : style) ~(area : float * float * int * int) ~(title : string) ~(marked : string list) (entries : entry list) : t =
  let left, top, pw, ph = area in
  let a = { left; top; pw; ph } in
  let placed, geometry = relayout ~top_kept a Ordered entries in
  let order = Hashtbl.create 64 in
  if numbered then List.iteri (fun i (e : entry) -> Hashtbl.replace order e.path (i + 1)) entries;
  { title; marked; entries; algo = Ordered; placed; geometry; cam = home a; target = home a; drag = None; dragged = false;
    before_right = false; painted = None; last = None; moving = false; lens = None; order; colours; jumped = None;
    back = []; choices = None; note = ""; found = None; roots; style; index = None; rank = None; search = None; search_all = None; tour_on = None; marks = []; mark_group = 0; layer = 0; guide_marks = None; flight = None; pointer = None;
    focus = 0; wheel_debt = 0.; wheel_at = 0.; guide; street = false; street_mode = 0; clock = 0.; xray = false; xray_n = 0; peek = None; peek_scroll = 0; peek_stack = []; beyond; top_kept; fan_in; counted; morph = None; help = false }

(* claude: the map's files for Code_names and Code_rank *)
let files_of (t : t) : (string * Code_file.t Lazy.t) list = List.map (fun (e : entry) -> (e.path, e.file)) t.entries

let index_of (t : t) : Code_names.index =
  match t.index with
  | Some ix -> ix
  | None ->
      (* claude: and the sources beyond the map, resolved against *)
      let ix = Code_names.index (files_of t @ List.map (fun (e : entry) -> (e.path, e.file)) t.beyond) in
      t.index <- Some ix;
      ix

let rank_of (t : t) : Code_rank.t =
  match t.rank with
  | Some r -> r
  | None ->
      (* claude: over the files beyond too (a program's map's, a folder
       * laid out alone's): its users are wherever they are *)
      (* claude: opti: the count shared by all the maps of these sources
       * when Codemap gives it ([counted], Codemap.rank_of_sources), else
       * this map's own, the simple way *)
      let r = match t.counted with Some c -> Lazy.force c | None -> Code_rank.compute ~roots:t.roots (files_of t @ List.map (fun (e : entry) -> (e.path, e.file)) t.beyond) in
      t.rank <- Some r;
      r

(* claude: opti: the uses if counted already, for what the mouse passing
 * over shows (a unit's ties): not counted then, which would lex every
 * file not yet read, seconds in one frame (principia's first hover); a
 * map of its own (no [counted], or Opti off) counts them as before.
 * old: rank_of t *)
let rank_if_counted (t : t) : Code_rank.t option =
  match (t.rank, t.counted) with
  | Some r, _ -> Some r
  | None, Some c when not (Lazy.is_val c) -> None
  | None, _ -> Some (rank_of t)

(* claude: the lines of the files shown, for a title *)
let lines_of (entries : entry list) : int = List.fold_left (fun n (e : entry) -> n + e.nlines) 0 entries
let lines (t : t) : int = lines_of t.entries

(* 12345 as "12,345 lines" *)
let lines_text (n : int) : string =
  let s = string_of_int n in
  let len = String.length s in
  let b = Buffer.create (len + 8) in
  String.iteri (fun i c -> if i > 0 && (len - i) mod 3 = 0 then Buffer.add_char b ','; Buffer.add_char b c) s;
  Buffer.contents b ^ if n = 1 then " line" else " lines"

let files (t : t) : int = List.length t.entries

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

(* claude: a hue, its colour at the saturation and brightness of the
 * repository's parts' *)
let of_hue (h : float) : int * int * int =
  let s = 0.6 and v = 0.82 in
  let h6 = h *. 6. in
  let i = int_of_float h6 mod 6 and f = h6 -. Float.of_int (int_of_float h6) in
  let p = v *. (1. -. s) and q = v *. (1. -. (s *. f)) and t = v *. (1. -. (s *. (1. -. f))) in
  let r, g, b = match i with 0 -> (v, t, p) | 1 -> (q, v, p) | 2 -> (p, v, t) | 3 -> (p, q, v) | 4 -> (t, p, v) | _ -> (v, p, q) in
  let c x = int_of_float (x *. 255.) in
  (c r, c g, c b)

(* claude: the roles that almost every project names the same way, a
 * colour each: codemap's archi_code (Main, Test, Core, Utils...), but
 * only the names that do not mislead; the rest is a project's own *)
let role (name : string) : (int * int * int) option =
  let starts p = String.length name >= String.length p && String.sub name 0 (String.length p) = p in
  match String.lowercase_ascii name with
  | "tests" | "test" | "testsuite" | "regressions" -> Some (220, 200, 60)
  | "docs" | "doc" | "documentation" -> Some (150, 160, 190)
  | "include" | "includes" -> Some (90, 150, 210)
  | "scripts" -> Some (160, 150, 100)
  | "examples" | "samples" | "demos" -> Some (100, 180, 230)
  | "third_party" | "vendor" | "external" | "contrib" -> Some (95, 95, 95)
  | "kernel" -> Some (200, 60, 90)
  (* libs, lib_core, libc: the libraries', as ours *)
  | _ when starts "lib" -> Some (200, 90, 60)
  | _ -> None

(* claude: a colour per part of the repository, as codemap's archi_code
 * colours a file by its role (its directory: Main, Test, Core...). First
 * what the directory's .codemapconfig says ([colours], Code_config: the
 * longest path of the file's that it names); then ours by name; then a
 * role (above); then a hue of its own, from its name, the same from one
 * run to the next and in every project (the hash times the golden
 * ratio: close names far apart). A file at the top grey. *)
let archi (colours : (string * (int * int * int)) list) (path : string) : int * int * int =
  let under (p : string) = path = p || (String.length path > String.length p && String.sub path 0 (String.length p + 1) = p ^ "/") in
  let given = List.fold_left (fun best (p, c) -> if under p && (match best with Some (q, _) -> String.length p > String.length q | None -> true) then Some (p, c) else best) None colours in
  (* claude: a top folder itself (games, no slash) is coloured as what
   * it holds (the author: at the top, the matrix's folders all grey);
   * the root alone grey *)
  match (given, String.index_opt path '/') with
  | Some (_, c), _ -> c
  | None, _ when path = "" -> (120, 120, 120)
  | None, i -> (
  let first = match i with Some i -> String.sub path 0 i | None -> path in
  match first with
  | "games" -> (70, 100, 220)
  | "apps" -> (170, 80, 210)
  | "gamekits" -> (50, 170, 170)
  | "appkits" -> (60, 170, 100)
  | "playground" -> (220, 170, 50)
  | "libs" -> (200, 90, 60)
  (* claude: tinybox's own, its logo's magenta; the languages grey *)
  | "launcher" -> (220, 70, 150)
  | "languages" -> (120, 120, 120)
  | _ when role first <> None -> Option.get (role first)
  | _ ->
      let golden = 0.618033988749895 in
      let x = float_of_int (Hashtbl.hash first) *. golden in
      of_hue (x -. Float.of_int (int_of_float x)))

let mix ((r, g, b) : int * int * int) (a : float) ((r2, g2, b2) : int * int * int) : int * int * int =
  let f x y = int_of_float ((a *. float_of_int x) +. ((1. -. a) *. float_of_int y)) in
  (f r r2, f g g2, f b b2)

let dark = (12, 10, 28)
let file_background (t : t) (path : string) = mix (archi t.colours path) 0.18 (20, 22, 30)
let dir_colour (t : t) (path : string) (depth : int) = mix (archi t.colours path) (0.12 +. (0.04 *. float_of_int (min depth 4))) dark
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

(* claude: the rectangle's first row a pixel at a time, then copied to
 * the others (Bigarray's blit: a memcpy natively, a typed array's set
 * on the web, where writing the 4 bytes of every pixel one by one made
 * a code map's background -- a million and a half pixels -- a quarter
 * of its frame)
 *
 *   old: every row as the first
 *)
let fill (img : Rgba_image.t) (x0 : int) (y0 : int) (x1 : int) (y1 : int) ((r, g, b) : int * int * int) : unit =
  (* claude: clipped to the picture *)
  let x0 = max 0 x0 and y0 = max 0 y0 and x1 = min img.width x1 and y1 = min img.height y1 in
  if x1 > x0 && y1 > y0 then begin
    for x = x0 to x1 - 1 do
      let i = 4 * ((y0 * img.width) + x) in
      Bigarray.Array1.unsafe_set img.rgba i r;
      Bigarray.Array1.unsafe_set img.rgba (i + 1) g;
      Bigarray.Array1.unsafe_set img.rgba (i + 2) b;
      Bigarray.Array1.unsafe_set img.rgba (i + 3) 255
    done;
    let n = 4 * (x1 - x0) in
    let first = Bigarray.Array1.sub img.rgba (4 * ((y0 * img.width) + x0)) n in
    for y = y0 + 1 to y1 - 1 do
      Bigarray.Array1.blit first (Bigarray.Array1.sub img.rgba (4 * ((y * img.width) + x0)) n)
    done
  end

(* a rectangle's pixels on the map, clipped: None if off it *)
(* claude: opti: Int's min and max, compared inline, not Stdlib's
 * polymorphic ones (the runtime's compare, four times a unit a frame:
 * principia's map busy doing nothing).
 * old: let x0 = max 0 (...) and x1 = min c.a.pw (...) *)
let clip (c : camera) (r : Treemap.rect) : (int * int * int * int) option =
  let x0 = Int.max 0 (int_of_float (Float.round (to_px c r.x))) and x1 = Int.min c.a.pw (int_of_float (Float.round (to_px c (r.x +. r.w)))) in
  let y0 = Int.max 0 (int_of_float (Float.round (to_py c r.y))) and y1 = Int.min c.a.ph (int_of_float (Float.round (to_py c (r.y +. r.h)))) in
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
    if line >= 0 && line < n && lc >= 0 && lc < lpc && ch < Code_file.shown && ch >= 0 then (line * cols) + ch else -1
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

(* where a line of a file is in the layout: its column's left, its top *)
let line_pos (r : Treemap.rect) (g : geometry) (line : int) : float * float =
  (r.x +. (float_of_int (line / g.lpc) *. g.colw), r.y +. (float_of_int (line mod g.lpc) *. g.cell_h))

(* claude: where a name is in the layout, its top left corner *)
let name_pos (r : Treemap.rect) (g : geometry) (line : int) (col : int) : float * float =
  let x, y = line_pos r g line in
  (x +. (float_of_int col *. g.cell_w), y)

(*****************************************************************************)
(* The labels' tools *)
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

(* claude: the width of words [size] high, estimated: the font is the
 * backend's sans-serif, never measured, so each character by its class,
 * after Helvetica's widths (thousandths of the size) -- close enough to
 * align lines on their left *)
let text_width (size : float) (s : string) : float =
  let w = function
    | 'i' | 'j' | 'l' | '\'' | '!' | '|' | '.' | ',' | ':' | ';' | '`' -> 240
    | 'f' | 't' | 'r' | 'I' | '(' | ')' | '[' | ']' | '/' | '-' | ' ' -> 300
    | 'm' | 'w' | 'M' | 'W' | '@' | '%' -> 860
    | 'A' .. 'Z' -> 690
    | _ -> 530
  in
  let n = ref 0 in
  String.iter (fun c -> n := !n + w c) s;
  size *. float_of_int !n /. 1000.

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

(* claude: a name on a tab, [size] high, its box's top left corner at
 * (tx, ty) *)
let tab (a : area) ?(alpha = 0.85) (color : color) (size : float) (tx : float) (ty : float) (s : string) : (float * float * float * float) * shape =
  let tw = (0.5 *. size *. float_of_int (String.length s)) +. 8. and th = size +. 6. in
  ( (tx, ty, tx +. tw, ty +. th),
    group
      [
        rectangle (rgb 12 10 28) tw th |> move (sx a (tx +. (tw /. 2.))) (sy a (ty +. (th /. 2.))) |> fade alpha;
        label a color size (tx +. (tw /. 2.)) (ty +. (th /. 2.)) s;
      ] )

let lighter ((r, g, b) : int * int * int) : color = rgb (min 255 (r + 60)) (min 255 (g + 60)) (min 255 (b + 60))
