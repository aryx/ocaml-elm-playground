(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Puzzle Bobble (Taito, 1994, Bust-a-Move in America):
 * bubbles hang from the ceiling; you shoot one more from the bottom,
 * it bounces off the walls and sticks where it touches, and three or
 * more of the same color together pop -- with everything that was
 * hanging from them, which falls. Left and right (or the mouse) aim,
 * space, up or a click shoots. Every 8 shots the ceiling comes down a
 * row; a bubble below the line: game over. No bubble left: the next
 * round.
 *
 * Taito made it from Bubble Bobble (1986): the same two dinosaurs, Bub
 * and Bob, turning the crank of the launcher. Its best known clone is
 * Frozen Bubble (Guillaume Cottenceau, 2002, in Perl with SDL), for
 * years the game every Linux distribution showed off, with its
 * penguins. (Names and dates from memory, to check.)
 *
 * What's new here:
 *
 *  - A hexagonal grid, stored as a square one: every other row shifted
 *    by half a bubble ("offset coordinates"), so a cell's six
 *    neighbours depend on its row being even or odd ([neighbours]):
 *
 *        row 0:  (0,0)  (0,1)  (0,2)        a cell of an even row
 *        row 1:     (1,0)  (1,1)  (1,2)     touches (r-1, c-1), (r-1, c),
 *        row 2:  (2,0)  (2,1)  (2,2)        an odd row's (r-1, c), (r-1, c+1)
 *
 *  - Snapping a moving thing to a grid: the shot flies freely, then
 *    takes the empty cell nearest to where it touched ([snap]).
 *
 *  - Two flood fills after each shot ([stick]): the bubbles of the same
 *    color connected to the new one (3 or more pop), then the bubbles
 *    still connected to the ceiling; the others fall. The same search
 *    as a paint program's bucket, on a graph instead of pixels.
 *
 *  - The aiming guide is the shot itself, flown ahead of time with the
 *    same code ([path]), bounces included; the test's robot uses it too
 *    to choose where to aim.
 *
 * What it uses: Scene2d, Audio. No kit: the grid is too different from
 * the square ones of kits/maze/ and kits/puzzle/. Not Physics: a bounce
 * off a wall is a sign flipped, and the falling bubbles' gravity is one
 * line.
 *
 * Exercises: Frozen Bubble's two players, each popped group sending
 * bubbles to the other side; the hurry up (the bubble leaves by itself
 * after 8 seconds); the guide only in the easy mode; more rounds, and
 * a round editor; the special bubbles of the sequels (the rainbow one
 * taking the color of what it pops next to).
 *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type cell = int * int (* row, column *)

(* a bubble on the move: the shot, or a popped or falling one *)
type flying = { x : number; y : number; vx : number; vy : number; color : int }

type effect = { b : flying; age : int; falls : bool }

type game = {
  board : (cell * int) list; (* the bubbles hanging, and their color *)
  angle : number; (* the launcher's, in degrees: 90 is straight up *)
  shot : flying option;
  current : int; (* the colors of the bubble in the launcher, and the next *)
  next : int;
  effects : effect list;
  shots : int; (* since the ceiling last came down *)
  drop : int; (* rows the ceiling came down *)
  round : int;
  score : int;
  seed : int;
}

type scene = Title | Playing of game | Clear of game | Game_over of game
type model = scene Scene2d.t

let radius = 32.
let cols = 8
let row_height = 56. (* the rows fit together: about 64 * sqrt 3 / 2 *)
let left = -256. (* the walls: 8 bubbles wide *)
let right = 256.
let top = 440.
let rows = 12 (* a bubble in row 12 or more (counting the drop): game over *)
let launcher = (0., -400.)
let speed = 20.

let palette = [ rgb 230 60 60; rgb 70 190 80; rgb 60 110 230; rgb 240 210 50; rgb 170 80 210; rgb 250 140 40 ]

(* an odd row has one bubble less: it's shifted by half a bubble *)
let cols_of (r : int) : int = if r mod 2 = 0 then cols else cols -.. 1

let ceiling (g : game) : number = top - (row_height * float_of_int g.drop)

let center (g : game) ((r, c) : cell) : number * number =
  (left + radius + (2. * radius * float_of_int c) + (if r mod 2 = 1 then radius else 0.), ceiling g - radius - (row_height * float_of_int r))

(* the six around, on the board's rows and columns *)
let neighbours ((r, c) : cell) : cell list =
  let shift = if r mod 2 = 0 then -1 else 0 in
  [ (r, c -.. 1); (r, c +.. 1); (r -.. 1, c +.. shift); (r -.. 1, c +.. shift +.. 1); (r +.. 1, c +.. shift); (r +.. 1, c +.. shift +.. 1) ]
  |> List.filter (fun (r, c) -> r >= 0 && c >= 0 && c < cols_of r)

(* the rounds: a letter per bubble, its color's initial; spaces only
 * to show the odd rows' shift *)
let rounds =
  [ [ "RRYYBBGG"; " RRYYBB"; "BBGGRRYY"; " BGGRRY" ];
    [ "G......G"; " G.....G"; "BYRGBYRG"; " YRGBYR"; "...PP..." ];
    [ "RGBYRGBY"; " GBYRGB"; "BYRGBYRG"; " YRGBYR"; "PPOOPPOO"; " O.P.O." ] ]

let parse (rows : string list) : (cell * int) list =
  List.concat
    (List.mapi
       (fun r s ->
         let s = String.concat "" (String.split_on_char ' ' s) in
         List.concat
           (List.init (String.length s) (fun c ->
                match String.index_opt "RGBYPO" s.[c] with Some color -> [ ((r, c), color) ] | None -> [])))
       rows)

let roll (seed : int) (n : int) : int * int =
  let s = ((seed *.. 1103515245) +.. 12345) land 0x7fffffff in
  (s /.. 65536 mod n, s)

(* a color still on the board, so that every bubble can be of use *)
let pick (board : (cell * int) list) (seed : int) : int * int =
  match List.sort_uniq compare (List.map snd board) with
  | [] -> (0, seed)
  | colors ->
      let i, seed = roll seed (List.length colors) in
      (List.nth colors i, seed)

let new_round (round : int) (score : int) (seed : int) : game =
  let board = parse (List.nth rounds (round mod List.length rounds)) in
  let current, seed = pick board seed in
  let next, seed = pick board seed in
  { board; angle = 90.; shot = None; current; next; effects = []; shots = 0; drop = 0; round; score; seed }

let initial_model : model = Scene2d.start Title

(*****************************************************************************)
(* The shot *)
(*****************************************************************************)

let launch (g : game) (angle : number) : flying =
  let a = angle * pi / 180. in
  { x = fst launcher; y = snd launcher; vx = speed * cos a; vy = speed * sin a; color = g.current }

(* a quarter of a frame's move (so that it can't jump over a bubble),
 * bouncing off the walls *)
let fly (f : flying) : flying =
  let f = { f with x = f.x + (f.vx / 4.); y = f.y + (f.vy / 4.) } in
  if f.x < left + radius then { f with x = (2. * (left + radius)) - f.x; vx = -.f.vx }
  else if f.x > right - radius then { f with x = (2. * (right - radius)) - f.x; vx = -.f.vx }
  else f

(* touching the ceiling, or nearly a bubble: a bit less than two radii,
 * so that a shot can squeeze past one it only grazes *)
let touches (g : game) (f : flying) : bool =
  f.y + radius >= ceiling g
  || List.exists
       (fun (cell, _) ->
         let x, y = center g cell in
         Float.hypot (f.x - x) (f.y - y) < 1.75 * radius)
       g.board

(* the shot's quarter steps, from the launcher to where it touches (the
 * last one) *)
let path (g : game) (angle : number) : flying list =
  let rec go f acc = if touches g f then List.rev (f :: acc) else go (fly f) (f :: acc) in
  go (launch g angle) []

(* the empty cell nearest to where the shot touched *)
let snap (g : game) (f : flying) : cell =
  let all = List.concat (List.init (rows +.. 1) (fun r -> List.init (cols_of r) (fun c -> (r, c)))) in
  let empty = List.filter (fun cell -> not (List.mem_assoc cell g.board)) all in
  let dist cell =
    let x, y = center g cell in
    Float.hypot (f.x - x) (f.y - y)
  in
  List.fold_left (fun best cell -> if dist cell < dist best then cell else best) (List.hd empty) empty

(* the cells of [board] reached from [start] through cells where [ok] *)
let flood (board : (cell * int) list) (ok : int -> bool) (start : cell list) : cell list =
  let rec go todo seen =
    match todo with
    | [] -> seen
    | cell :: rest -> (
        match List.assoc_opt cell board with
        | Some color when ok color && not (List.mem cell seen) -> go (neighbours cell @ rest) (cell :: seen)
        | _ -> go rest seen)
  in
  go start []

(* the shot sticks: pops 3 or more of its color, then what no longer
 * hangs from the ceiling falls *)
let stick (g : game) (f : flying) : game =
  let cell = snap g f in
  let board = (cell, f.color) :: g.board in
  let before = board in
  let group = flood board (fun color -> color = f.color) [ cell ] in
  let popped = if List.length group >= 3 then group else [] in
  let board = List.filter (fun (c, _) -> not (List.mem c popped)) board in
  let hanging = flood board (fun _ -> true) (List.init cols (fun c -> (0, c))) in
  let fallen = List.filter (fun (c, _) -> not (List.mem c hanging)) board |> List.map fst in
  let as_effect falls (c : cell) =
    let x, y = center g c in
    { b = { x; y; vx = 0.; vy = (if falls then 4. else 0.); color = List.assoc c before }; age = 0; falls }
  in
  let board = List.filter (fun (c, _) -> List.mem c hanging) board in
  (* the original's scoring: 10 a popped bubble, and the fallen ones
   * doubling, 20, 40, 80... *)
  let bonus = if fallen = [] then 0 else 10 *.. (1 lsl min 17 (List.length fallen)) in
  if popped <> [] then Audio.play Audio.coin else Audio.play Audio.step;
  if fallen <> [] then Audio.play Audio.explosion;
  let shots = g.shots +.. 1 in
  let current = g.next in
  let next, seed = pick board g.seed in
  (* after a pop, the launcher's color may be gone from the board *)
  let current = if List.exists (fun (_, c) -> c = current) board || board = [] then current else next in
  { g with board; shot = None; current; next; seed;
    effects = g.effects @ List.map (as_effect false) popped @ List.map (as_effect true) fallen;
    score = g.score +.. (10 *.. List.length popped) +.. bonus;
    shots = (if shots = 8 then 0 else shots); drop = (if shots = 8 then g.drop +.. 1 else g.drop) }

let lost (g : game) : bool = List.exists (fun ((r, _), _) -> r +.. g.drop >= rows) g.board

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let update_game (computer : computer) (scenes : model) (g : game) : game =
  let k = computer.keyboard and m = computer.mouse in
  let pressed key = Scene2d.pressed key scenes in
  (* aiming: the arrows turn the launcher, the mouse points it *)
  let lx, ly = launcher in
  let angle = if m.mdx <> 0. || m.mdy <> 0. then atan2 (m.my - ly) (m.mx - lx) * 180. / pi else g.angle - (2. * to_x k) in
  let g = { g with angle = clamp 15. 165. angle } in
  let g = if g.shot = None && (pressed (fun k -> k.kspace) || pressed (fun k -> k.kup) || m.mclick) then (Audio.play Audio.jump; { g with shot = Some (launch g g.angle) }) else g in
  (* the shot: four quarter steps, until it touches *)
  let g =
    match g.shot with
    | None -> g
    | Some f ->
        let rec go f n = if touches g f then stick g f else if n = 0 then { g with shot = Some f } else go (fly f) (n -.. 1) in
        go f 4
  in
  let effects =
    List.map (fun e -> { e with age = e.age +.. 1; b = { e.b with y = e.b.y + e.b.vy; vy = (if e.falls then e.b.vy - 0.8 else 0.) } }) g.effects
    |> List.filter (fun e -> if e.falls then e.b.y > -600. else e.age < 15)
  in
  { g with effects }

let update (computer : computer) (s : model) : model =
  let s = Scene2d.update computer s in
  let space = Scene2d.pressed (fun k -> k.kspace) s in
  match s.scene with
  | Title -> if space then Scene2d.go (Playing (new_round 0 0 1)) s else s
  | Playing g ->
      let g = update_game computer s g in
      if lost g then Scene2d.go (Game_over g) s
      else if g.board = [] && g.shot = None then Scene2d.go (Clear g) s
      else { s with scene = Playing g }
  | Clear g -> if space && s.elapsed > 1. then Scene2d.go (Playing (new_round (g.round +.. 1) g.score g.seed)) s else s
  | Game_over _ -> if space && s.elapsed > 2. then Scene2d.go Title s else s

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

(* scaled before moved, so around its own center *)
let bubble ?(size = 1.) (color : int) (x : number) (y : number) : shape =
  group [ circle (List.nth palette color) (radius - 2.); circle white 8. |> move (-10.) 10. |> fade 0.6 ] |> scale size |> move x y

let view_game (g : game) : shape list =
  let lx, ly = launcher in
  let dead_line = top - radius - (row_height * float_of_int rows) + (row_height / 2.) in
  let a = g.angle * pi / 180. in
  [ rectangle (rgb 20 20 50) (right - left) 1000.;
    rectangle (rgb 120 120 140) 20. 1000. |> move_x (left - 10.);
    rectangle (rgb 120 120 140) 20. 1000. |> move_x (right + 10.);
    (* the ceiling, pushed down *)
    rectangle (rgb 120 120 140) (right - left + 40.) (500. - ceiling g) |> move_y ((500. + ceiling g) / 2.);
    rectangle (rgb 230 60 60) (right - left) 2. |> move_y dead_line ]
  @ List.map (fun (cell, color) -> let x, y = center g cell in bubble color x y) g.board
  (* the guide: every 8th quarter step, while no shot is flying *)
  @ (if g.shot = None then List.filteri (fun i _ -> i mod 8 = 4) (path g g.angle) |> List.map (fun f -> circle white 3. |> move f.x f.y) else [])
  @ [ rectangle (rgb 200 200 220) 90. 10. |> rotate g.angle |> move (lx + (45. * cos a)) (ly + (45. * sin a)) ]
  @ (match g.shot with Some f -> [ bubble f.color f.x f.y ] | None -> [ bubble g.current lx ly ])
  @ [ bubble ~size:0.7 g.next (lx - 120.) (ly - 40.); text white 2. "NEXT" |> move (lx - 120.) (ly - 90.) ]
  @ List.map (fun e -> if e.falls then bubble e.b.color e.b.x e.b.y else bubble ~size:(1. + (float_of_int e.age / 15.)) e.b.color e.b.x e.b.y |> fade (1. - (float_of_int e.age / 15.))) g.effects
  @ [ text yellow 3. (Printf.sprintf "%d" g.score) |> move (-380.) 460.; text white 2.5 (Printf.sprintf "ROUND %d" (g.round +.. 1)) |> move 380. 460. ]

let view (computer : computer) (s : model) : shape list =
  let screen = computer.screen in
  rectangle (rgb 40 30 80) screen.width screen.height
  ::
  (match s.scene with
  | Title ->
      view_game (new_round 0 0 1)
      @ [ text yellow 7. "TINY PUZZLE BOBBLE" |> move_y 60.; text white 2.5 "left right (or the mouse) aim, space shoots" |> move_y (-20.) ]
      @ Scene2d.blink 1. s [ text yellow 3. "PRESS SPACE" |> move_y (-100.) ]
  | Playing g -> view_game g
  | Clear g ->
      view_game g @ [ text yellow 6. "ROUND CLEAR!" ] @ if s.elapsed > 1. then Scene2d.blink 1. s [ text white 3. "PRESS SPACE" |> move_y (-100.) ] else []
  | Game_over g ->
      view_game g @ [ text (rgb 230 60 60) 7. "GAME OVER" ] @ if s.elapsed > 2. then Scene2d.blink 1. s [ text white 3. "PRESS SPACE" |> move_y (-100.) ] else [])

let app = game view update initial_model
let main = Playground_platform.run_app app
