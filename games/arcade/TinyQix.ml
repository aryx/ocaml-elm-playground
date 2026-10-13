(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Qix (Randy and Sandy Pfeiffer, Taito America,
 * 1981): an empty field, a thing of lines twisting about in it (the
 * Qix), and a marker that walks the field's edge. Arrows to walk
 * along the lines; hold space and walk off a line to draw (a Stix),
 * and when the drawing comes back to a line, the side of it without
 * the Qix is yours. Hold x instead to draw slowly, for twice the
 * points. Claim 75% of the field.
 *
 * What kills: the Qix touching a Stix not yet closed; the two Sparx,
 * which run along the lines after you; and stopping in the middle of a
 * Stix, which lights a fuse at its start. The lines are the only safe
 * place from the Qix, and the only place the Sparx go: every moment is
 * one danger traded for the other.
 *
 * Qix was a game with no character and no story, drawn with lines on
 * a black screen, and it filled arcades in 1981; Volfied (Taito, 1989)
 * and Gals Panic (Kaneko, 1990), which uncovered a picture as the
 * field was claimed, are its descendants, and JezzBall (Microsoft,
 * 1992) its cousin. (Names and dates from memory, to check.)
 *
 * The new idea here is how a closed Stix knows which side to fill.
 * The geometry is awkward: the Stix and the old lines make a polygon,
 * or several, with holes; which is "inside"? The game never asks. The
 * rule says "the side without the Qix", and that is the algorithm
 * ([claim]): flood the open field from the cell the Qix is in, a cell
 * to its four neighbours, stopped by lines -- and whatever open cell
 * the flood did not reach is claimed.
 *
 *    +---------+----+       +---------+----+       +---------+----+
 *    |         |    |       |.........|    |       |         |####|
 *    |   Q     |    |  =>   |...Q.....|    |  =>   |   Q     |####|
 *    |         |    |       |.........|    |       |         |####|
 *    +---------+----+       +---------+----+       +---------+----+
 *     the Stix closed        flooded from Q         the rest claimed
 *
 * Flood fill is a paint program's bucket (Smith's "Tint Fill", 1979)
 * used backwards: fill what you do not want, and keep the complement.
 * A Stix that closes on nothing claims nothing, one that cuts the
 * field in three claims two parts at once, and no case is special.
 *
 * So the field is a grid of cells ([grid], rows of characters, as the
 * 2D games' maps), and the lines are cells too. A line is "live" while
 * it still borders open field ([live]): the marker and the Sparx walk
 * only those, so that the lines swallowed by a claim go dead without
 * any bookkeeping.
 *
 * The Qix itself is two points bouncing about the open field, each on
 * its own, the second on a leash to the first, and the last few lines
 * between them drawn in fading colours ([step_qix]).
 *
 * What it uses: Sprite ([pixels]: the whole field is one sprite, a
 * cell a pixel, drawn in runs), Scene2d, Audio's ready-made sounds,
 * Playground's random (the seed in the model: the Qix's turns). No
 * kit: TinyTron's light cycles also leave lines on a grid, but there a
 * line is only a wall.
 *
 * Exercises: the Sparx walking the dead lines back to the live ones
 * (here they jump, [nearest_live]); the Super Sparx, which follow you
 * onto a Stix; the timer that sends more Sparx; two Qix from the third
 * stage, and the bonus for splitting them apart; a picture uncovered
 * under the claimed cells (Gals Panic; Volfied's ships); a claim
 * animated, row by row.
 *)
open Playground

(*****************************************************************************)
(* The field *)
(*****************************************************************************)

let n = 81 (* cells a side: the columns and rows 0 and 80 are the edge *)
let cell = 10.

(* rows from the top; ' ' open field, '#' a line, 'f' and 's' claimed
 * (fast, slow), 'a' and 'b' a Stix being drawn (fast, slow) *)
type grid = string array

let start_grid : grid = Array.init n (fun r -> String.init n (fun c -> if r = 0 || c = 0 || r = n - 1 || c = n - 1 then '#' else ' '))

let get (g : grid) ((c, r) : int * int) : char = if c < 0 || r < 0 || c >= n || r >= n then 'f' else g.(r).[c]

let set (g : grid) ((c, r) : int * int) (ch : char) : grid =
  Array.mapi (fun i row -> if i = r then String.mapi (fun j x -> if j = c then ch else x) row else row) g

let is_stix (ch : char) : bool = ch = 'a' || ch = 'b'

(* a line that still borders open field (or a Stix), corners included *)
let live (g : grid) ((c, r) : int * int) : bool =
  get g (c, r) = '#'
  && List.exists
       (fun (dc, dr) -> let ch = get g (c + dc, r + dr) in ch = ' ' || is_stix ch)
       [ (-1, -1); (0, -1); (1, -1); (-1, 0); (1, 0); (-1, 1); (0, 1); (1, 1) ]

(* [claim g qix paint]: the Stix of [g] made a line, the open field
 * flooded from the cell [qix], and every open cell not reached painted
 * [paint]: the new grid, and how many cells were claimed. The grid is
 * copied, the copy scribbled on ('.' the flood), and a new one made. *)
let claim (g : grid) (qix : int * int) (paint : char) : grid * int =
  let b = Array.map Bytes.of_string g in
  Array.iter (fun row -> Bytes.iteri (fun c ch -> if is_stix ch then Bytes.set row c '#') row) b;
  let rec flood = function
    | [] -> ()
    | (c, r) :: rest ->
        if c >= 0 && r >= 0 && c < n && r < n && Bytes.get b.(r) c = ' ' then (
          Bytes.set b.(r) c '.';
          flood ((c + 1, r) :: (c - 1, r) :: (c, r + 1) :: (c, r - 1) :: rest))
        else flood rest
  in
  flood [ qix ];
  let claimed = ref 0 in
  let paint_row row = String.map (function ' ' -> incr claimed; paint | '.' -> ' ' | ch -> ch) (Bytes.to_string row) in
  let g = Array.map paint_row b in
  (g, !claimed)

(* how much of the inside is no longer open, in percent *)
let percent (g : grid) : int =
  let inside = (n - 2) * (n - 2) in
  let still = Array.fold_left (fun k row -> String.fold_left (fun k ch -> if ch = ' ' then k + 1 else k) k row) 0 g in
  100 * (inside - still) / inside

(* the live line nearest to a cell: where a Sparx whose line went dead
 * turns up *)
let nearest_live (g : grid) ((c, r) : int * int) : int * int =
  let best = ref (c, r) and d = ref max_int in
  for r' = 0 to n - 1 do
    for c' = 0 to n - 1 do
      let d' = abs (c' - c) + abs (r' - r) in
      if d' < !d && live g (c', r') then (best := (c', r'); d := d')
    done
  done;
  !best

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

(* a Stix being drawn: its cells, the last one first; the line cell it
 * left from; how far its fuse has burnt, from its start *)
type stix = { cells : (int * int) list; home : int * int; slow : bool; fuse : int }

(* a Sparx: where it is, and where it came from (it never turns back) *)
type sparx = { at : int * int; from : int * int }

type vec = number * number

(* the Qix: two points and their speeds, in cells, and the lines it has
 * just been, for the look *)
type qix = { p : vec; pv : vec; q : vec; qv : vec; tail : (vec * vec) list }

type game = {
  grid : grid;
  me : int * int;
  stix : stix option;
  sparx : sparx list;
  qix : qix;
  seed : seed;
  frames : int;
  dead : int; (* frames left of a death's pause *)
  score : int;
  lives : int;
  stage : int;
}

type scene = Title | Playing of game | Cleared of game | Game_over of int
type model = scene Scene2d.t

let new_game ~(stage : int) ~(score : int) ~(lives : int) : game =
  let top = (n / 2, 0) in
  { grid = start_grid; me = (n / 2, n - 1); stix = None;
    sparx = [ { at = top; from = (fst top - 1, 0) }; { at = top; from = (fst top + 1, 0) } ];
    qix = { p = (40., 40.); pv = (0.2, 0.15); q = (44., 37.); qv = (-0.15, 0.2); tail = [] };
    seed = initial_seed (1981 + stage); frames = 0; dead = 0; score; lives; stage }

let initial_model : model = Scene2d.start Title

(* a stage's numbers: the Qix a little faster each time, the Sparx a
 * cell every 4 frames, then every 3, as fast as the marker *)
let qix_speed (stage : int) : number = 0.25 +. (0.05 *. float_of_int stage)
let sparx_period (stage : int) : int = if stage < 3 then 4 else 3
let goal = 75

(*****************************************************************************)
(* The marker *)
(*****************************************************************************)

(* [close g s]: the Stix [s] back on a line: its side without the Qix
 * claimed, 4 points a cell, 8 if drawn slowly; the Sparx left on a
 * dead line put on the nearest live one *)
let close (g : game) (s : stix) : game =
  let px, py = g.qix.p in
  let grid, cells = claim g.grid (Float.to_int (Float.round px), Float.to_int (Float.round py)) (if s.slow then 's' else 'f') in
  Audio.play Audio.powerup;
  let back (x : sparx) = if live grid x.at then x else let at = nearest_live grid x.at in { at; from = at } in
  { g with grid; stix = None; score = g.score + (cells * if s.slow then 8 else 4); sparx = List.map back g.sparx }

(* [walk keys g]: the marker one cell on, if the arrows ask and it can:
 * along a live line; off it into the open field with space or x held,
 * starting a Stix; and on a Stix, further into the open field, or onto
 * a line, which closes it. Left and right are tried before up and
 * down, so that two arrows held go round a corner. *)
let walk (keys : keyboard) (g : game) : game =
  let c, r = g.me in
  let dx = Float.to_int (to_x keys) and dy = Float.to_int (to_y keys) in
  let slow = Set_.mem "x" keys.keys in
  let step (t : int * int) : game option =
    let ch = get g.grid t in
    match g.stix with
    | None ->
        if live g.grid t then Some { g with me = t }
        else if ch = ' ' && (keys.kspace || slow) then
          Some { g with me = t; grid = set g.grid t (if slow then 'b' else 'a'); stix = Some { cells = [ t ]; home = g.me; slow; fuse = 0 } }
        else None
    | Some s ->
        if ch = ' ' then Some { g with me = t; grid = set g.grid t (if s.slow then 'b' else 'a'); stix = Some { s with cells = t :: s.cells } }
        else if ch = '#' then Some (close { g with me = t } s)
        else None
  in
  let tries = (if dx <> 0 then [ (c + dx, r) ] else []) @ if dy <> 0 then [ (c, r - dy) ] else [] in
  match List.find_map step tries with Some g -> g | None -> g

(* a cell every 3 frames, every 6 on a slow Stix *)
let my_period (g : game) : int = match g.stix with Some { slow = true; _ } -> 6 | _ -> 3

(* [burn idle g]: the fuse: while no arrow is held on a Stix, it burns
 * a cell every 4 frames from the Stix's start; moving again only stops
 * it where it is *)
let burn (idle : bool) (g : game) : game =
  match g.stix with
  | Some s when idle && g.frames mod 4 = 0 -> { g with stix = Some { s with fuse = s.fuse + 1 } }
  | _ -> g

(*****************************************************************************)
(* The Sparx and the Qix *)
(*****************************************************************************)

(* along the live lines, never back: at a fork, the way nearer the
 * marker; at a dead end, back after all *)
let step_sparx (g : game) (x : sparx) : sparx =
  let c, r = x.at and mc, mr = g.me in
  let ways = List.filter (fun t -> t <> x.from && live g.grid t) [ (c + 1, r); (c - 1, r); (c, r + 1); (c, r - 1) ] in
  let far (c, r) = abs (c - mc) + abs (r - mr) in
  match List.sort (fun a b -> compare (far a) (far b)) ways with
  | t :: _ -> { at = t; from = x.at }
  | [] -> if live g.grid x.from then { at = x.from; from = x.at } else x

let cell_of ((x, y) : vec) : int * int = (Float.to_int (Float.round x), Float.to_int (Float.round y))

(* a point one frame on: bouncing off whatever is not open field, a
 * Stix let through (touching it is the point) *)
let fly (grid : grid) ((x, y) : vec) ((vx, vy) : vec) : vec * vec =
  let free p = let ch = get grid (cell_of p) in ch = ' ' || is_stix ch in
  let vx = if free (x +. vx, y) then vx else -.vx in
  let vy = if free (x +. vx, y +. vy) then vy else -.vy in
  if free (x +. vx, y +. vy) then ((x +. vx, y +. vy), (vx, vy)) else ((x, y), (vx, vy))

(* The Qix: each point turns to a new direction now and then, at
 * random; the second, when it strays more than 9 cells from the first,
 * heads back to it. *)
let step_qix (g : game) : game =
  let speed = qix_speed g.stage and k = g.qix in
  let turn (v : vec) (seed : seed) : vec * seed =
    let roll, seed = random_int 0 39 seed in
    if roll <> 0 then (v, seed) else let a, seed = random 0. (2. *. Float.pi) seed in ((speed *. cos a, speed *. sin a), seed)
  in
  let pv, seed = turn k.pv g.seed in
  let qv, seed = turn k.qv seed in
  let (px, py), (qx, qy) = (k.p, k.q) in
  let d = Float.hypot (px -. qx) (py -. qy) in
  let qv = if d > 9. then (speed *. (px -. qx) /. d, speed *. (py -. qy) /. d) else qv in
  let p, pv = fly g.grid k.p pv and q, qv = fly g.grid k.q qv in
  (* left behind by a claim on the other side of a new line *)
  let q = if get g.grid (cell_of q) = ' ' || is_stix (get g.grid (cell_of q)) then q else p in
  let tail = if g.frames mod 3 = 0 then List.filteri (fun i _ -> i < 6) ((p, q) :: k.tail) else k.tail in
  { g with seed; qix = { p; pv; q; qv; tail } }

(* the Qix's line over a Stix cell: sampled every half cell *)
let qix_on_stix (g : game) : bool =
  let (px, py), (qx, qy) = (g.qix.p, g.qix.q) in
  let steps = 1 + Float.to_int (2. *. Float.hypot (px -. qx) (py -. qy)) in
  List.exists
    (fun i ->
      let t = float_of_int i /. float_of_int steps in
      is_stix (get g.grid (cell_of (px +. (t *. (qx -. px)), py +. (t *. (qy -. py))))))
    (List.init (steps + 1) Fun.id)

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

(* a life lost: the Stix erased, the marker back where it left the line *)
let die (g : game) : game =
  Audio.play Audio.explosion;
  match g.stix with
  | None -> { g with dead = 60; lives = g.lives - 1 }
  | Some s -> { g with dead = 60; lives = g.lives - 1; stix = None; me = s.home; grid = List.fold_left (fun grid t -> set grid t ' ') g.grid s.cells }

let update_game (keys : keyboard) (g : game) : game =
  let g = { g with frames = g.frames + 1 } in
  if g.dead > 0 then
    (* the Sparx that caught him moves on before he can move again *)
    let g = { g with dead = g.dead - 1 } in
    if g.dead = 0 then { g with sparx = List.map (fun (x : sparx) -> if x.at = g.me then { at = (n / 2, 0); from = (n / 2, 0) } else x) g.sparx } else g
  else
    let g = if g.frames mod my_period g = 0 then walk keys g else g in
    let g = burn (to_x keys = 0. && to_y keys = 0.) g in
    let g = if g.frames mod sparx_period g.stage = 0 then { g with sparx = List.map (step_sparx g) g.sparx } else g in
    let g = step_qix g in
    let burnt = match g.stix with Some s -> s.fuse >= List.length s.cells | None -> false in
    if burnt || qix_on_stix g || List.exists (fun (x : sparx) -> x.at = g.me) g.sparx then die g else g

let update (computer : computer) (s : model) : model =
  let s = Scene2d.update computer s in
  let space = Scene2d.pressed (fun k -> k.kspace) s in
  match s.scene with
  | Title -> if space then Scene2d.go (Playing (new_game ~stage:1 ~score:0 ~lives:3)) s else s
  | Playing g ->
      let g = update_game computer.keyboard g in
      if percent g.grid >= goal then (Audio.play Audio.coin; Scene2d.go (Cleared g) s)
      else if g.lives <= 0 then Scene2d.go (Game_over g.score) s
      else { s with scene = Playing g }
  | Cleared g ->
      (* 1000 points for each percent over the goal, as in the original *)
      if space then Scene2d.go (Playing (new_game ~stage:(g.stage + 1) ~score:(g.score + (1000 * (percent g.grid - goal))) ~lives:g.lives)) s else s
  | Game_over _ -> if space then Scene2d.go Title s else s

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (str : string) : shape = words color str |> scale size

(* where a cell is on the screen *)
let place ((c, r) : vec) : vec = ((c -. float_of_int (n / 2)) *. cell, (float_of_int (n / 2) -. r) *. cell)
let at_cell ((c, r) : int * int) (s : shape) : shape = let x, y = place (float_of_int c, float_of_int r) in move x y s

let palette = [ ('#', white); ('f', rgb 40 90 200); ('s', rgb 190 80 40); ('a', rgb 130 180 255); ('b', rgb 255 170 90) ]

(* a line between two points of the screen: a thin rectangle, turned *)
let segment (color : color) ((x1, y1) : vec) ((x2, y2) : vec) : shape =
  rectangle color (Float.hypot (x2 -. x1) (y2 -. y1)) 3.
  |> rotate (atan2 (y2 -. y1) (x2 -. x1) *. 180. /. Float.pi)
  |> move ((x1 +. x2) /. 2.) ((y1 +. y2) /. 2.)

let view_qix (k : qix) : shape list =
  let colors = [ rgb 255 80 200; rgb 255 120 80; rgb 255 220 80; rgb 120 255 120; rgb 80 220 255; rgb 150 120 255; rgb 255 80 200 ] in
  List.mapi (fun i (p, q) -> segment (List.nth colors (i mod 7)) (place p) (place q) |> fade (1. -. (float_of_int i /. 8.))) ((k.p, k.q) :: k.tail)

let view_game (computer : computer) (g : game) : shape list =
  let screen = computer.screen in
  let spark = group [ rectangle yellow 26. 5.; rectangle yellow 5. 26.; square orange 12. |> rotate 45. ] |> rotate (spin 1. computer.time) in
  let marker = if g.dead > 0 && g.dead / 4 mod 2 = 0 then [] else [ group [ square red 20. |> rotate 45.; square white 9. |> rotate 45. ] |> at_cell g.me ] in
  let fuse = match g.stix with Some s when s.fuse > 0 -> [ circle red 6. |> at_cell (List.nth (List.rev s.cells) (min s.fuse (List.length s.cells) - 1)) ] | _ -> [] in
  [ rectangle black screen.width screen.height; Sprite.pixels cell palette (Array.to_list g.grid) ]
  @ view_qix g.qix @ fuse
  @ List.map (fun (x : sparx) -> at_cell x.at spark) g.sparx
  @ marker
  @ [ text white 2.5 (Printf.sprintf "%d" g.score) |> move (screen.left +. 150.) (screen.top -. 50.);
      text (if percent g.grid >= goal then green else yellow) 2.5 (Printf.sprintf "%d%%  of  %d%%" (percent g.grid) goal) |> move_y (screen.top -. 50.);
      text white 2.5 (Printf.sprintf "STAGE %d   LIVES %d" g.stage g.lives) |> move (screen.right -. 190.) (screen.top -. 50.) ]

let view (computer : computer) (s : model) : shape list =
  let press y = Scene2d.blink 1. s [ text yellow 3. "PRESS SPACE" |> move_y y ] in
  match s.scene with
  | Title ->
      view_game computer (new_game ~stage:1 ~score:0 ~lives:3)
      @ [ text (rgb 255 80 200) 9. "TINY QIX" |> move_y 220.;
          text white 2.3 "arrows: walk along the lines" |> move_y 140.;
          text white 2.3 "space held: draw into the field (x: slowly, for double)" |> move_y 105.;
          text white 2.3 "close a drawing: the side without the Qix is yours" |> move_y 70.;
          text white 2.3 "don't stop on a drawing; mind the sparks on the lines" |> move_y 35. ]
      @ press (-150.)
  | Playing g -> view_game computer g
  | Cleared g -> view_game computer g @ [ rectangle black 600. 200. |> fade 0.8; text green 6. "STAGE CLEAR" |> move_y 30. ] @ press (-50.)
  | Game_over score -> [ rectangle black computer.screen.width computer.screen.height; text red 6. "GAME OVER"; text white 3. (Printf.sprintf "SCORE %d" score) |> move_y (-80.) ] @ press (-160.)

let app = game view update initial_model

let main = Program.main __MODULE__ (fun () -> Playground_platform.run_app app)
