(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Doom (id Software, 1993), in the *2D* playground: a
 * walk through a small level -- a hall with a pillar, stairs, a window
 * onto a dark room -- to find the exit. Left/right to turn, up/down to
 * walk; the steps climb themselves.
 *
 * Doom looked 3D, but its renderer had no 3D at all, no z-buffer, no
 * polygons: a level is a floor plan (kits/sectors/Sectors.mli), and it
 * drew it column by column, like Wolfenstein 3D (games/TinyWolf.ml), but
 * with walls at any angle, floors and ceilings at any height. Its trick
 * is an order: if the walls come nearest first, each screen column only
 * needs to remember how much of it is still to be drawn, and a wall
 * draws only there. That order, from anywhere in the level, is what a
 * BSP tree gives (binary space partitioning: Henry Fuchs, Zvi Kedem,
 * Bruce Naylor, "On Visible Surface Generation by A Priori Tree
 * Structures", SIGGRAPH 1980; John Carmack read about it and made it
 * Doom's).
 *
 * 1. The node builder ([build]), once, at startup (id's was a separate
 *    program, idbsp, run on the level after editing it): the walls, cut
 *    in "segs", are split in two by the line of one of them, the
 *    partition, then each half again, until each part is convex, a
 *    "subsector":
 *
 *      +-----------+              root: the partition A
 *      |    |      |               /           \
 *      |  1 A  2   |           front: 1       back: B
 *      |    |--B---|                          /     \
 *      |    |  3   |                         2       3
 *      +-----------+
 *
 * 2. The walk ([render], R_RenderBSPNode in Doom's r_bsp.c): at each
 *    node, the side the eye is on first, then the other: the segs come
 *    out nearest first, whatever the eye. The other side is skipped if
 *    its box can't be seen: it's behind the eye, or in columns already
 *    full ([visible], R_CheckBBox) -- most of the level is never looked
 *    at, which the status bar counts.
 *
 * 3. The columns ([draw_seg], R_StoreWallRange in r_segs.c): a seg is
 *    projected (its two ends: 1/distance is linear across the screen,
 *    Doom's "scale"), and in each of its columns, from the ceiling down:
 *
 *        top clip --  +----+ the front sector's ceiling (a "flat")
 *                     |####| an upper wall (two-sided: the next
 *                     |    |  sector's ceiling is lower)
 *                     |    | the opening: the next sector, drawn later
 *                     |####| a lower wall (its floor is higher: a step)
 *     bottom clip --  +----+ the front sector's floor
 *
 *    A one-sided seg, a wall, fills its opening and closes the column; a
 *    two-sided one narrows it, the per-column [top] and [bottom] clip
 *    arrays (Doom's ceilingclip and floorclip), through which the next
 *    sectors show: a window, stairs. Nothing is ever drawn twice
 *    (TinyWolf draws the floor and ceiling first, then the walls over
 *    them), and the shapes can be drawn in any order.
 *
 * Doom's floors and ceilings were textured, drawn in rows ("visplanes",
 * r_plane.c: the columns of a flat remembered, then drawn as horizontal
 * spans, sampled like games/TinyKart's Mode 7); here, flat colors, the
 * columns of a color drawn together too ([paint]). The light is Doom's: the sector's level, darker
 * with the distance, in 32 steps like its COLORMAP, and walls along the
 * x axis darker than along y (its "fake contrast", for corners).
 *
 * The sector you're in is found by the same tree ([sector_at],
 * R_PointInSubsector): down the side the point is on, to its subsector.
 * The level, and moving in it, are the Sectors kit's, shared with
 * games3d/TinyDoom3d: the same level with polygons and a z-buffer, no
 * BSP at all. The minimap shows the BSP at work: in white, the segs
 * drawn this frame.
 *
 * Uses: the Sectors kit. Not Tilemap (walls at any angle), not Camera2d.
 *
 * References: the Doom source code (id Software, released in 1997; the
 * functions named above); Fabien Sanglard, "Game Engine Black Book:
 * Doom" (2018); Michael Abrash, "Graphics Programming Black Book" (1997),
 * chapters 59-60 on BSP trees.
 *
 * Exercises: textured walls (a seg's column is the distance along it,
 * as TinyWolf's), sprites clipped against the columns (Doom's
 * "drawsegs"), a door (a sector whose ceiling moves), textured flats in
 * spans.
 *)
open Playground

let level = Sectors.outpost
let sector (i : int) : Sectors.sector = level.sectors.(i)

(*****************************************************************************)
(* Segs *)
(*****************************************************************************)

(* A seg: a piece of a line, seen from one side, [front] the sector on
 * its right. A two-sided line gives two segs, one per side (each can be
 * seen only from its front); the node builder cuts segs in pieces. *)
type seg = { x1 : number; y1 : number; x2 : number; y2 : number; front : int; back : int option; line : Sectors.line }

let segs : seg list =
  Array.to_list level.lines
  |> List.concat_map (fun (l : Sectors.line) ->
         let s = { x1 = l.x1; y1 = l.y1; x2 = l.x2; y2 = l.y2; front = l.front; back = l.back; line = l } in
         match l.back with
         | None -> [ s ]
         | Some b -> [ s; { x1 = l.x2; y1 = l.y2; x2 = l.x1; y2 = l.y1; front = b; back = Some l.front; line = l } ])

(*****************************************************************************)
(* The node builder *)
(*****************************************************************************)

(* a partition: a point and a direction, of length 1: [side] is then the
 * distance to it, negative on its front (its right) *)
type partition = { px : number; py : number; dx : number; dy : number }

let partition_of (s : seg) : partition =
  let len = Float.hypot (s.x2 -. s.x1) (s.y2 -. s.y1) in
  { px = s.x1; py = s.y1; dx = (s.x2 -. s.x1) /. len; dy = (s.y2 -. s.y1) /. len }

let side (p : partition) (x : number) (y : number) : number = (p.dx *. (y -. p.py)) -. (p.dy *. (x -. p.px))

(* where a seg is, from a partition: in front, behind, or across it, cut
 * in two (the front piece, the back piece). On the partition's line, it
 * goes with the side it faces: in front if it goes the same way. *)
type where = Front | Back | Split of seg * seg

let classify (p : partition) (s : seg) : where =
  let eps = 0.01 in
  let a = side p s.x1 s.y1 and b = side p s.x2 s.y2 in
  if Float.abs a < eps && Float.abs b < eps then if ((s.x2 -. s.x1) *. p.dx) +. ((s.y2 -. s.y1) *. p.dy) > 0. then Front else Back
  else if a < eps && b < eps then Front
  else if a > -.eps && b > -.eps then Back
  else
    let t = a /. (a -. b) in
    let x = s.x1 +. (t *. (s.x2 -. s.x1)) and y = s.y1 +. (t *. (s.y2 -. s.y1)) in
    let first = { s with x2 = x; y2 = y } and second = { s with x1 = x; y1 = y } in
    if a < 0. then Split (first, second) else Split (second, first)

type box = { left : number; right : number; bottom : number; top : number }

(* the tree: a node's partition and its two sides, a leaf's segs (a
 * subsector); each with the box around its segs *)
type bsp = Leaf of seg list * box | Node of partition * bsp * bsp * box

let box_of (t : bsp) : box = match t with Leaf (_, b) | Node (_, _, _, b) -> b

let box_of_segs (segs : seg list) : box =
  List.fold_left
    (fun b s ->
      { left = Float.min b.left (Float.min s.x1 s.x2); right = Float.max b.right (Float.max s.x1 s.x2);
        bottom = Float.min b.bottom (Float.min s.y1 s.y2); top = Float.max b.top (Float.max s.y1 s.y2) })
    { left = infinity; right = neg_infinity; bottom = infinity; top = neg_infinity }
    segs

(* convex: every seg has all the others in front of it, the region they
 * enclose is on all their fronts *)
let convex (segs : seg list) : bool =
  List.for_all (fun s -> let p = partition_of s in List.for_all (fun o -> o == s || classify p o = Front) segs) segs

(* Splitting until convex. The partition: of the segs having some other
 * seg behind them, the one cutting the fewest segs and keeping the two
 * sides the most even (each cut costs 3 of imbalance: idbsp weighed
 * cuts the same way, more than balance). *)
let rec build (segs : seg list) : bsp =
  if convex segs then Leaf (segs, box_of_segs segs)
  else
    let score s =
      let p = partition_of s in
      let fronts, backs, cuts =
        List.fold_left
          (fun (f, b, c) o -> match classify p o with Front -> (f + 1, b, c) | Back -> (f, b + 1, c) | Split _ -> (f + 1, b + 1, c + 1))
          (0, 0, 0) segs
      in
      if backs = 0 then None else Some ((3 * cuts) + abs (fronts - backs), s)
    in
    let _, best = List.filter_map score segs |> List.fold_left (fun a b -> if fst b < fst a then b else a) (max_int, List.hd segs) in
    let p = partition_of best in
    let fronts, backs =
      List.fold_left
        (fun (f, b) o -> match classify p o with Front -> (o :: f, b) | Back -> (f, o :: b) | Split (fo, bo) -> (fo :: f, bo :: b))
        ([], []) segs
    in
    let front = build (List.rev fronts) and back = build (List.rev backs) in
    let bf = box_of front and bb = box_of back in
    Node
      ( p, front, back,
        { left = Float.min bf.left bb.left; right = Float.max bf.right bb.right; bottom = Float.min bf.bottom bb.bottom;
          top = Float.max bf.top bb.top } )

let bsp : bsp = build segs

(* how many nodes, and how many segs after the cuts *)
let rec count (t : bsp) : int * int =
  match t with
  | Leaf (s, _) -> (0, List.length s)
  | Node (_, f, b, _) ->
      let nf, sf = count f and nb, sb = count b in
      (1 + nf + nb, sf + sb)

(* the sector at (x, y): down the tree, the side the point is on, to a
 * subsector, all of whose segs have its sector in front *)
let rec sector_at (t : bsp) (x : number) (y : number) : int =
  match t with
  | Leaf (s :: _, _) -> s.front
  | Leaf ([], _) -> 0
  | Node (p, f, b, _) -> if side p x y <= 0. then sector_at f x y else sector_at b x y

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type model = {
  x : number;
  y : number;
  z : number; (* the floor under the player, followed smoothly up the steps *)
  angle : number; (* degrees, counterclockwise from +x *)
  frames : int;
  exited : int option; (* the frame the exit was reached *)
}

let initial_model : model =
  let x, y, angle = level.start in
  { x; y; z = (sector (sector_at bsp x y)).floor; angle; frames = 0; exited = None }

let axis a b = (if a then 1. else 0.) -. if b then 1. else 0.

let update (computer : computer) (m : model) : model =
  let k = computer.keyboard in
  match m.exited with
  | Some _ -> if k.kspace then initial_model else m
  | None ->
      let angle = m.angle +. (3. *. axis k.kleft k.kright) in
      let speed = if k.kup then 8. else if k.kdown then -6. else 0. in
      let a = angle *. Float.pi /. 180. in
      let x, y = Sectors.move level (sector_at bsp) (m.x, m.y) (speed *. cos a, speed *. sin a) in
      let here = sector_at bsp x y in
      let floor = (sector here).floor in
      let z = if Float.abs (floor -. m.z) < 1. then floor else m.z +. ((floor -. m.z) *. 0.3) in
      { x; y; z; angle; frames = m.frames + 1; exited = (if here = level.exit then Some m.frames else None) }

(*****************************************************************************)
(* Rendering: the columns *)
(*****************************************************************************)

(* the screen's columns, 5 pixels wide on a 1000-pixel screen; the eye
 * 41 above the floor; the status bar's height, under the view *)
let columns = 200
let eye_height = 41.
let bar = 300.

(* the spans painted in consecutive columns, of one color: from column
 * [first] to [last], their (bottom, top), the last first *)
type run = { first : int; last : int; spans : (number * number) list }

(* what a frame's rendering knows and does: the eye, where it looks
 * (a unit vector) and its focal length (a 90 degrees field of view,
 * Doom's); the clip arrays; and, as it goes, the shapes, the segs
 * drawn, the nodes visited. Doom kept all of these in globals. *)
type view = {
  ex : number;
  ey : number;
  ez : number;
  fx : number;
  fy : number;
  focal : number;
  left : number; (* the screen's left, and a column's width *)
  w : number;
  horizon : number;
  top : number array; (* in each column, the part still to draw *)
  bottom : number array;
  mutable closed : int; (* columns with nothing left to draw *)
  runs : (color, run) Hashtbl.t; (* see [paint] *)
  mutable shapes : shape list;
  mutable drawn : seg list;
  mutable visited : int;
}

(* a point of the plan in the eye's frame: its distance ahead, and to
 * the right *)
let to_view (v : view) (x : number) (y : number) : number * number =
  let rx = x -. v.ex and ry = y -. v.ey in
  ((rx *. v.fx) +. (ry *. v.fy), (rx *. v.fy) -. (ry *. v.fx))

let near = 1.

(* Doom's light: the sector's level, fading with the distance, in 32
 * steps *)
let shade ((r, g, b) : int * int * int) (light : number) : color =
  let k = Float.round (Float.max 0. (Float.min 1.2 light) *. 32.) /. 32. in
  let c x = min 255 (int_of_float (float_of_int x *. k)) in
  rgb (c r) (c g) (c b)

let fade (depth : number) : number = Float.max 0.3 (Float.min 1. (1.1 -. (depth /. 2000.)))

(* Painting column [c] from y1 up to y2. Not a rectangle each time:
 * the spans of a color in consecutive columns are kept together, a
 * [run], and drawn as one polygon, a staircase along the columns
 * ([flush]):
 *
 *        _   _                  one polygon instead of 5 rectangles:
 *      _| |_| |_                the same pixels, but the playground's
 *     |         |               rasterizers fill shapes row by row,
 *     |_   _____|               and 200 thin columns 700 rows high are
 *       |_|                     140000 rows; a few wide shapes, 700.
 *
 * Doom did the same for its flats, for another reason: a flat's columns
 * were remembered, a "visplane" per floor or ceiling, and drawn at the
 * end in horizontal spans, the way a texture on a floor is cheapest to
 * draw (r_plane.c). Its walls it drew column by column, as they come. *)
let flush (v : view) (color : color) (r : run) : unit =
  let x c = v.left +. (v.w *. float_of_int c) in
  let columns = List.mapi (fun i span -> (r.last - i, span)) r.spans in
  (* the bottom edge left to right, then the top edge right to left *)
  let bottom = List.concat_map (fun (c, (y1, _)) -> [ (x c, y1); (x (c + 1), y1) ]) (List.rev columns) in
  let top = List.concat_map (fun (c, (_, y2)) -> [ (x (c + 1), y2); (x c, y2) ]) columns in
  v.shapes <- polygon color (bottom @ top) :: v.shapes

let paint (v : view) (c : int) (color : color) (y1 : number) (y2 : number) : unit =
  if y2 > y1 then
    match Hashtbl.find_opt v.runs color with
    | Some r when r.last = c - 1 -> Hashtbl.replace v.runs color { r with last = c; spans = (y1, y2) :: r.spans }
    | r ->
        Option.iter (flush v color) r;
        Hashtbl.replace v.runs color { first = c; last = c; spans = [ (y1, y2) ] }

let close (v : view) (c : int) : unit =
  v.top.(c) <- v.bottom.(c);
  v.closed <- v.closed + 1

(* A seg's column [c], at [scale] (focal / distance): see the diagram in
 * the header, from the ceiling down. *)
let draw_column (v : view) (s : seg) (c : int) (scale : number) : unit =
  let f = sector s.front in
  let top = v.top.(c) and bottom = v.bottom.(c) in
  let y h = Float.max bottom (Float.min top (v.horizon +. ((h -. v.ez) *. scale))) in
  let yc = y f.ceiling and yf = y f.floor in
  let depth = v.focal /. scale in
  let contrast = if s.y1 = s.y2 then 0.85 else if s.x1 = s.x2 then 1.1 else 1. in
  let wall = shade f.wall_rgb (f.light *. fade depth *. contrast) in
  (* the flats between the clips and this seg, only when seen from
   * above (a floor) or below (a ceiling) *)
  if f.ceiling > v.ez then paint v c (shade f.ceiling_rgb (f.light *. fade depth)) yc top;
  if f.floor < v.ez then paint v c (shade f.floor_rgb (f.light *. fade depth)) bottom yf;
  match Option.map sector s.back with
  | Some b when b.ceiling > b.floor && b.ceiling > f.floor && b.floor < f.ceiling ->
      (* two-sided: the steps, and the opening narrowed to between them *)
      let new_top =
        if b.ceiling < f.ceiling then (
          let yb = y b.ceiling in
          paint v c wall yb yc;
          yb)
        else if f.ceiling > v.ez then yc
        else top
      in
      let new_bottom =
        if b.floor > f.floor then (
          let yb = y b.floor in
          paint v c wall yf yb;
          yb)
        else if f.floor < v.ez then yf
        else bottom
      in
      v.top.(c) <- new_top;
      v.bottom.(c) <- new_bottom;
      if new_top <= new_bottom then v.closed <- v.closed + 1
  | _ ->
      (* one-sided (or a closed sector behind it, the pillar): a wall *)
      paint v c wall yf yc;
      close v c

(* A seg, if its front faces the eye: its two ends seen from the eye,
 * cut at the [near] distance, projected, and the columns between them
 * drawn, 1 / distance going linearly from one end to the other. *)
let draw_seg (v : view) (s : seg) : unit =
  if side (partition_of s) v.ex v.ey < 0. then begin
    let d1, a1 = to_view v s.x1 s.y1 and d2, a2 = to_view v s.x2 s.y2 in
    if d1 >= near || d2 >= near then begin
      let d1, a1 = if d1 < near then (near, a1 +. ((a2 -. a1) *. (near -. d1) /. (d2 -. d1))) else (d1, a1) in
      let d2, a2 = if d2 < near then (near, a2 +. ((a1 -. a2) *. (near -. d2) /. (d1 -. d2))) else (d2, a2) in
      let sx1 = a1 *. v.focal /. d1 and sx2 = a2 *. v.focal /. d2 in
      if sx1 < sx2 then begin
        (* the columns whose centers are between sx1 and sx2 *)
        let col sx = int_of_float (ceil (((sx -. v.left) /. v.w) -. 0.5)) in
        let c1 = max 0 (col sx1) and c2 = min (columns - 1) (col sx2 - 1) in
        let drawn = ref false in
        for c = c1 to c2 do
          if v.top.(c) > v.bottom.(c) then begin
            let cx = v.left +. (v.w *. (float_of_int c +. 0.5)) in
            let inv = (1. /. d1) +. ((cx -. sx1) /. (sx2 -. sx1) *. ((1. /. d2) -. (1. /. d1))) in
            draw_column v s c (v.focal *. inv);
            drawn := true
          end
        done;
        if !drawn then v.drawn <- s :: v.drawn
      end
    end
  end

(* R_CheckBBox: could anything in the box still be seen? Not if it's all
 * behind the eye, or only in full columns (or off the screen). *)
let visible (v : view) (b : box) : bool =
  if v.ex >= b.left && v.ex <= b.right && v.ey >= b.bottom && v.ey <= b.top then true
  else
    let corners = List.map (fun (x, y) -> to_view v x y) [ (b.left, b.bottom); (b.left, b.top); (b.right, b.bottom); (b.right, b.top) ] in
    if List.for_all (fun (d, _) -> d < near) corners then false
    else if List.exists (fun (d, _) -> d < near) corners then true
    else
      let xs = List.map (fun (d, a) -> a *. v.focal /. d) corners in
      let col sx = int_of_float (floor (((sx -. v.left) /. v.w) -. 0.5)) in
      let c1 = max 0 (col (List.fold_left Float.min infinity xs)) in
      let c2 = min (columns - 1) (col (List.fold_left Float.max neg_infinity xs) + 1) in
      let rec open_ c = c <= c2 && (v.top.(c) > v.bottom.(c) || open_ (c + 1)) in
      open_ c1

(* R_RenderBSPNode: the eye's side first, then the other if visible;
 * done when every column is full *)
let rec render (v : view) (t : bsp) : unit =
  if v.closed < columns then begin
    v.visited <- v.visited + 1;
    match t with
    | Leaf (segs, _) -> List.iter (draw_seg v) segs
    | Node (p, f, b, _) ->
        let near_side, far_side = if side p v.ex v.ey <= 0. then (f, b) else (b, f) in
        render v near_side;
        if visible v (box_of far_side) then render v far_side
  end

let frame (screen : screen) (m : model) : view =
  let a = m.angle *. Float.pi /. 180. in
  let bottom = screen.bottom +. bar in
  let v =
    { ex = m.x; ey = m.y; ez = m.z +. eye_height; fx = cos a; fy = sin a; focal = screen.width /. 2.; left = screen.left;
      w = screen.width /. float_of_int columns; horizon = (screen.top +. bottom) /. 2.;
      top = Array.make columns screen.top; bottom = Array.make columns bottom; closed = 0; runs = Hashtbl.create 64;
      shapes = []; drawn = []; visited = 0 }
  in
  render v bsp;
  Hashtbl.iter (flush v) v.runs;
  v

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let line (color : color) (width : number) ((x1, y1) : number * number) ((x2, y2) : number * number) : shape =
  let dx = x2 -. x1 and dy = y2 -. y1 in
  rectangle color (Float.hypot dx dy) width |> rotate (atan2 dy dx *. 180. /. Float.pi) |> move ((x1 +. x2) /. 2.) ((y1 +. y2) /. 2.)

(* The plan, Doom's automap colors: walls red, steps brown, ceiling
 * changes yellow, the rest gray; in white, the segs drawn this frame;
 * the player and the field of view. *)
let view_minimap (screen : screen) (m : model) (v : view) : shape list =
  let k = 0.24 in
  let ox = screen.right -. 150. -. (576. *. k) and oy = screen.bottom +. 150. -. (512. *. k) in
  let at x y = (ox +. (x *. k), oy +. (y *. k)) in
  let color (l : Sectors.line) =
    match l.back with
    | None -> rgb 200 40 40
    | Some b ->
        let f = sector l.front and b = sector b in
        if f.floor <> b.floor then rgb 150 110 60 else if f.ceiling <> b.ceiling then rgb 200 200 60 else rgb 90 90 90
  in
  let a = m.angle *. Float.pi /. 180. in
  let ray d = at (m.x +. (120. *. cos (a +. d))) (m.y +. (120. *. sin (a +. d))) in
  Array.to_list (Array.map (fun (l : Sectors.line) -> line (color l) 2. (at l.x1 l.y1) (at l.x2 l.y2)) level.lines)
  @ List.map (fun s -> line white 2. (at s.x1 s.y1) (at s.x2 s.y2)) v.drawn
  @ [ line gray 1. (at m.x m.y) (ray (Float.pi /. 4.)); line gray 1. (at m.x m.y) (ray (-.Float.pi /. 4.));
      (let x, y = at m.x m.y in circle green 4. |> move x y) ]

let text color size str = words color str |> scale size

let nodes, cut_segs = count bsp

let view (computer : computer) (m : model) : shape list =
  let screen = computer.screen in
  let v = frame screen m in
  let time = float_of_int (Option.value m.exited ~default:m.frames) /. 60. in
  let stat i str = text (rgb 200 30 30) 2.5 str |> move (screen.left +. 210.) (screen.bottom +. 250. -. (float_of_int i *. 45.)) in
  v.shapes
  @ [ rectangle (rgb 70 70 70) screen.width bar |> move_y (screen.bottom +. (bar /. 2.));
      stat 0 (Printf.sprintf "TIME %d:%04.1f" (int_of_float time / 60) (Float.rem time 60.));
      stat 1 (Printf.sprintf "SEGS DRAWN %d / %d" (List.length v.drawn) cut_segs);
      stat 2 (Printf.sprintf "NODES VISITED %d / %d" v.visited ((2 * nodes) + 1));
      stat 3 (Printf.sprintf "SECTOR %d" (sector_at bsp m.x m.y)) ]
  @ view_minimap screen m v
  @ (match m.exited with
    | Some _ -> [ text yellow 6. "EXIT!" |> move_y 250.; text white 3. "space: again" |> move_y 170. ]
    | None -> if m.frames < 180 then [ text yellow 4. "FIND THE EXIT" |> move_y 250. ] else [])

let app = game view update initial_model

let main = Playground_platform.run_app app
