(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Tetris (Alexey Pajitnov, 1984), written from
 * scratch, and juiced. Left and right to move, up to turn, down to fall
 * faster, space to drop at once. Tetris.ml, beside it, is a port of an
 * Elm version (elm-flatris), written in Elm's own architecture of
 * messages; this one is written like the other Tiny games, with the
 * rules of the versions that came after.
 *
 * Pajitnov wrote it at the Soviet Academy of Sciences' computing
 * centre in Moscow, on an Elektronika 60 that had no graphics: the
 * well was drawn in text, a cell being two brackets "[ ]". The pieces
 * are the seven tetrominoes, every way four squares can touch, and the
 * whole game is one rule -- a full line goes -- which is why it could be
 * ported to everything, and was.
 *
 * The rules, and where they come from:
 *
 *  - the scores are the Nintendo version's (1989): 40, 100, 300 and
 *    1200 points for 1 to 4 lines at once, times the level plus one --
 *    four lines together (a "Tetris") are worth three times four lines
 *    one by one, which is the game's strategy: build high, leave a
 *    well, wait for the long piece; and its speeds, frames per row
 *    ([gravity]), a level every 10 lines;
 *
 *  - the pieces come in bags ([bag]): the seven, shuffled, then the
 *    seven again. The first versions drew each piece at random, and a
 *    player could wait for the long piece for a very long time; the bag
 *    of later versions makes the drought at most 12 pieces;
 *
 *  - the ghost ([drop]): where the piece would land, drawn faintly;
 *
 *  - the lock delay: a piece that lands can still be slid for half a
 *    second ([lock_frames]) before it is part of the well;
 *
 *  - the wall kicks, simplified ([kicks]): a turn that doesn't fit is
 *    tried again one or two cells to a side, or one up.
 *
 * Juice (the juice section; the flag juice=off gives the dry game): a
 * piece that locks flashes white; one dropped at once knocks the screen
 * and makes the well bounce, squashed about its floor; each line cleared
 * bursts into pieces of its cells' colors, and shakes the screen, more
 * for each line -- a Tetris also flashes it white; the end shakes it
 * hard, and flashes it red. The juice watches the rules from outside:
 * they only say which piece locked ([locked]).
 *
 * What it uses: Scene2d (title, play, game over), Juice (squash,
 * stretch, whiten, shake, flash, burst). Not Tilemap: the well is rows of
 * cells, each row an array, a full row a row to remove. Not Physics:
 * pieces move a cell at a time.
 *
 * Left as exercises: holding a piece for later; the Super Rotation
 * System's full kick tables (they differ for the long piece), and the
 * T-spins they make possible; the delayed auto shift tuned (here 12
 * frames, then every 3); two players sending each other lines.
 *)
open Playground

(*****************************************************************************)
(* The well and the pieces *)
(*****************************************************************************)

let cols = 10
let rows = 20
(* two hidden rows above, where the pieces appear and can turn *)
let height = rows + 2

type kind = I | O | T | S | Z | J | L

let kinds = [ I; O; T; S; Z; J; L ]

let color_of (k : kind) : color =
  match k with
  | I -> rgb 0 200 220
  | O -> rgb 240 210 0
  | T -> rgb 160 60 200
  | S -> rgb 60 190 70
  | Z -> rgb 220 50 50
  | J -> rgb 40 90 220
  | L -> rgb 240 140 20

(* each piece in its box, as it appears: a turn turns the box *)
let box (k : kind) : string list =
  match k with
  | I -> [ "...."; "####"; "...."; "...." ]
  | O -> [ "##"; "##" ]
  | T -> [ ".#."; "###"; "..." ]
  | S -> [ ".##"; "##."; "..." ]
  | Z -> [ "##."; ".##"; "..." ]
  | J -> [ "#.."; "###"; "..." ]
  | L -> [ "..#"; "###"; "..." ]

(* a piece: its kind, its box's top left corner in the well (rows
 * counted from the bottom), and its quarter turns clockwise *)
type piece = { kind : kind; x : int; y : int; turns : int }

(* its four cells in the well, (column, row): a quarter turn clockwise
 * of an n x n box takes (column c, line r from the top) to
 * (n - 1 - r, c) *)
let cells (p : piece) : (int * int) list =
  let lines = box p.kind in
  let n = List.length lines in
  let in_box =
    List.concat
      (List.mapi (fun r line -> List.filter_map (fun c -> if line.[c] = '#' then Some (c, r) else None) (List.init n Fun.id)) lines)
  in
  let rec turn k cs = if k = 0 then cs else turn (k - 1) (List.map (fun (c, r) -> (n - 1 - r, c)) cs) in
  List.map (fun (c, r) -> (p.x + c, p.y - r)) (turn (p.turns mod 4) in_box)

(* the well: [height] rows from the bottom, each [cols] cells *)
type grid = color option array array

let empty_grid () : grid = Array.init height (fun _ -> Array.make cols None)

let free (g : grid) ((x, y) : int * int) : bool = x >= 0 && x < cols && y >= 0 && (y >= height || g.(y).(x) = None)
let fits (g : grid) (p : piece) : bool = List.for_all (free g) (cells p)

(* the piece made part of the well (a copy: the grid is a value) *)
let stamp (g : grid) (p : piece) : grid =
  let g = Array.map Array.copy g in
  List.iter (fun (x, y) -> if y < height then g.(y).(x) <- Some (color_of p.kind)) (cells p);
  g

let full (row : color option array) : bool = Array.for_all Option.is_some row

(* the full rows gone, the rows above them come down *)
let clear (g : grid) : grid =
  let kept = List.filter (fun row -> not (full row)) (Array.to_list g) in
  Array.of_list (kept @ List.init (height - List.length kept) (fun _ -> Array.make cols None))

(* where the piece lands if it falls straight down: the ghost *)
let rec drop (g : grid) (p : piece) : piece =
  let q = { p with y = p.y - 1 } in
  if fits g q then drop g q else p

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type game = {
  grid : grid;
  piece : piece;
  next : kind;
  bag : kind list; (* the pieces left in this bag *)
  score : int;
  lines : int;
  level : int;
  fall : int; (* frames since the piece last fell a row *)
  resting : int; (* frames the piece has been unable to fall *)
  held : int; (* frames left or right has been held *)
  (* the piece that locked at the last update, if one did -- a fact
   * about the game, which the juice section reads *)
  locked : piece option;
}

type scene = Title | Playing of game | Game_over of game

type model = {
  scenes : scene Scene2d.t;
  hi_score : int;
  (* the juice: the effects, when the last piece locked and its cells
   * (they flash), when the last one was dropped at once (the well
   * bounces) *)
  fx : Juice.t;
  lock_flash : time * (int * int) list;
  slammed : time;
}

(* seed=n (see Playground.flags), e.g. for the golden frame tests: the
 * same pieces every run *)
let () =
  match List.assoc_opt "seed" (Playground_platform.flags ()) with
  | Some n -> Random.init (int_of_string n)
  | None -> Random.self_init ()

(* a new bag: the seven, shuffled (Fisher and Yates) *)
let bag () : kind list =
  let a = Array.of_list kinds in
  for i = Array.length a - 1 downto 1 do
    let j = Random.int (i + 1) in
    let t = a.(i) in
    a.(i) <- a.(j);
    a.(j) <- t
  done;
  Array.to_list a

let take (b : kind list) : kind * kind list = match b with k :: rest -> (k, rest) | [] -> (match bag () with k :: rest -> (k, rest) | [] -> assert false)

(* a piece appears at the top, in the middle *)
let spawn (k : kind) : piece = { kind = k; x = (if k = O then 4 else 3); y = (if k = I then rows else rows - 1); turns = 0 }

let new_game () : game =
  let first, b = take (bag ()) in
  let next, b = take b in
  { grid = empty_grid (); piece = spawn first; next; bag = b; score = 0; lines = 0; level = 0; fall = 0; resting = 0; held = 0; locked = None }

let initial_model : model =
  { scenes = Scene2d.start Title; hi_score = 0; fx = Juice.none ~seed:1; lock_flash = (Time (-10.), []); slammed = Time (-10.) }

(*****************************************************************************)
(* The rules *)
(*****************************************************************************)

(* the Nintendo version's frames per row, level by level: 2 from 19 to
 * 28, then 1 (the "kill screen" speed, too fast for the delayed auto
 * shift to keep up) *)
let gravity (level : int) : int =
  let table = [| 48; 43; 38; 33; 28; 23; 18; 13; 8; 6; 5; 5; 5; 4; 4; 4; 3; 3; 3 |] in
  if level < Array.length table then table.(level) else if level < 29 then 2 else 1

let points (n : int) (level : int) : int = [| 0; 40; 100; 300; 1200 |].(n) * (level + 1)

(* half a second on the ground before a piece is part of the well *)
let lock_frames = 30

(* a turn that doesn't fit is tried moved by these, in order *)
let kicks = [ (0, 0); (-1, 0); (1, 0); (-2, 0); (2, 0); (0, 1) ]

let turn (g : game) (p : piece) : piece =
  let turned = { p with turns = p.turns + 1 } in
  match List.find_opt (fun (dx, dy) -> fits g.grid { turned with x = turned.x + dx; y = turned.y + dy }) kicks with
  | Some (dx, dy) -> { turned with x = turned.x + dx; y = turned.y + dy }
  | None -> p

(* the piece part of the well, the full lines gone and scored, the next
 * piece coming; None when it can't: the game is over *)
let lock (g : game) (p : piece) : game option =
  let grid = stamp g.grid p in
  let n = Array.fold_left (fun n row -> if full row then n + 1 else n) 0 grid in
  let lines = g.lines + n in
  let next, bag = take g.bag in
  let piece = spawn g.next in
  let g =
    { g with grid = clear grid; piece; next; bag; lines; score = g.score + points n g.level; level = lines / 10; fall = 0; resting = 0;
      locked = Some p }
  in
  if fits g.grid piece then Some g else None

let update_game (computer : computer) (scenes : scene Scene2d.t) (g : game) : game option =
  let k = computer.keyboard in
  let pressed key = Scene2d.pressed key scenes in
  let g = { g with locked = None } in
  let p = if pressed (fun k -> k.kup) then turn g g.piece else g.piece in
  (* left and right: at once, then again after 12 frames, every 3 *)
  let dir = if k.kleft then -1 else if k.kright then 1 else 0 in
  let held = if dir = 0 then 0 else g.held + 1 in
  let p =
    let q = { p with x = p.x + dir } in
    if dir <> 0 && (held = 1 || (held > 12 && (held - 12) mod 3 = 0)) && fits g.grid q then q else p
  in
  let g = { g with held } in
  if pressed (fun k -> k.kspace) then lock g (drop g.grid p)
  else
    (* gravity, 24 times faster with down held (at least 2 frames a row) *)
    let period = if k.kdown then max 1 (min 2 (gravity g.level / 24)) else gravity g.level in
    let below = { p with y = p.y - 1 } in
    let fall = g.fall + 1 in
    let p, fall = if fall >= period && fits g.grid below then (below, 0) else (p, fall) in
    let resting = if fits g.grid { p with y = p.y - 1 } then 0 else g.resting + 1 in
    if resting >= lock_frames then lock g p else Some { g with piece = p; fall; resting }

let update_rules (computer : computer) (model : model) : model =
  let scenes = Scene2d.update computer model.scenes in
  let start = Scene2d.pressed (fun k -> k.kspace) scenes in
  match scenes.scene with
  | Title -> if start then { model with scenes = Scene2d.go (Playing (new_game ())) scenes } else { model with scenes }
  | Playing g -> (
      match update_game computer scenes g with
      | Some g -> { model with scenes = { scenes with scene = Playing g }; hi_score = max model.hi_score g.score }
      | None -> { model with scenes = Scene2d.go (Game_over g) scenes })
  | Game_over _ ->
      if start || scenes.elapsed > 10. then { model with scenes = Scene2d.go Title scenes } else { model with scenes }

(*****************************************************************************)
(* The juice (juice=off: none of it) *)
(*****************************************************************************)

(* Everything the juice does is here, and the rules above don't know
 * about it: [update] runs them, then [juiced] looks at the piece they
 * locked, if they did -- where it went, which rows it filled -- and turns
 * that into effects. The view calls [flashing] and [bounced] where it
 * draws a cell and the well, and [Juice.view] around the picture. *)

let cell = 30.
let well_x = -120. (* the well's center *)
let well_bottom = -300.

(* the center of a cell, on the screen *)
let at ((x, y) : int * int) : number * number =
  (well_x +. ((float_of_int x -. (float_of_int cols /. 2.) +. 0.5) *. cell), well_bottom +. ((float_of_int y +. 0.5) *. cell))

let juiced (before : scene) (model : model) : model =
  let now = Juice.now model.fx in
  match (before, model.scenes.scene) with
  | Playing g, Playing { locked = Some p; _ } ->
      (* the rows the piece filled: the well before, and the piece in it *)
      let grid = stamp g.grid p in
      let rows_cleared = List.filter (fun y -> full grid.(y)) (List.init height Fun.id) in
      let n = List.length rows_cleared in
      (* dropped from higher than gravity takes it in a frame: a slam *)
      let slam = g.piece.y - p.y > 1 in
      let fx = model.fx in
      let fx = if slam then Juice.shake 0.3 fx else fx in
      (* every other cell of each row cleared bursts into its color *)
      let fx =
        List.fold_left
          (fun fx y ->
            List.fold_left
              (fun fx x -> match grid.(y).(x) with Some c when x mod 2 = y mod 2 -> Juice.burst ~at:(at (x, y)) (Juice.debris c) fx | _ -> fx)
              fx (List.init cols Fun.id))
          fx rows_cleared
      in
      let fx = if n = 4 then fx |> Juice.shake 0.9 |> Juice.flash white 12 else Juice.shake (0.2 *. float_of_int n) fx in
      { model with fx; lock_flash = (now, cells p); slammed = (if slam then now else model.slammed) }
  | Playing _, Game_over _ -> { model with fx = model.fx |> Juice.shake 0.8 |> Juice.flash red 20 }
  | _ -> model

let update (computer : computer) (model : model) : model =
  let model = { model with fx = Juice.step computer model.fx } in
  juiced model.scenes.scene (update_rules computer model)

(* whether a cell is one of the piece that just locked, flashing white *)
let flashing (model : model) (xy : int * int) : bool =
  let t, cs = model.lock_flash in
  Juice.during 0.07 t model.fx && List.mem xy cs

(* the well, built with its floor at (0, 0), squashed on a slam *)
let bounced (model : model) (well : shape) : shape = Juice.stretch (Juice.squash 0.06 0.25 model.slammed model.fx) well

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

(* a cell of the well, its floor at (0, 0) *)
let square_at (color : color) ((x, y) : int * int) : shape =
  square color (cell -. 2.) |> move ((float_of_int x -. (float_of_int cols /. 2.) +. 0.5) *. cell) ((float_of_int y +. 0.5) *. cell)

let view_well (model : model) (g : game) : shape =
  let settled =
    List.concat_map
      (fun y ->
        List.filter_map
          (fun x -> Option.map (fun c -> square_at (if flashing model (x, y) then white else c) (x, y)) g.grid.(y).(x))
          (List.init cols Fun.id))
      (List.init rows Fun.id)
  in
  let visible p = List.filter (fun (_, y) -> y < rows) (cells p) in
  let ghost = List.map (fun xy -> square_at (color_of g.piece.kind) xy |> fade 0.25) (visible (drop g.grid g.piece)) in
  let piece = List.map (square_at (color_of g.piece.kind)) (visible g.piece) in
  group
    ((rectangle (rgb 20 20 30) (float_of_int cols *. cell) (float_of_int rows *. cell) |> move_y (float_of_int rows *. cell /. 2.))
    :: (settled @ ghost @ piece))
  |> bounced model
  |> move well_x well_bottom

let view_panel (model : model) (g : game) : shape list =
  let px = 190. in
  let next = List.map (fun (x, y) -> square (color_of g.next) (cell -. 2.) |> move (float_of_int x *. cell) (float_of_int y *. cell)) (cells { (spawn g.next) with x = 0; y = 0 }) in
  [ text white 2.5 "NEXT" |> move px 250.; group next |> move (px -. 30.) 190.;
    text white 2.5 (Printf.sprintf "SCORE %d" g.score) |> move px 60.;
    text white 2.5 (Printf.sprintf "LINES %d" g.lines) |> move px 10.;
    text white 2.5 (Printf.sprintf "LEVEL %d" g.level) |> move px (-40.);
    text (rgb 150 150 170) 2. (Printf.sprintf "HI %d" model.hi_score) |> move px (-90.) ]

let view (computer : computer) (model : model) : shape list =
  let screen = computer.screen in
  let scenes = model.scenes in
  rectangle black screen.width screen.height
  (* the background still, everything else shaken *)
  :: Juice.view model.fx
       (match scenes.scene with
       | Title ->
           [ text white 6. "TINY TETRIS" |> move_y 150.;
             text white 2.2 "left/right: move   up: turn   down: faster   space: drop" |> move_y 40. ]
           @ Scene2d.blink 1. scenes [ text yellow 3. "PRESS SPACE" |> move_y (-100.) ]
       | Playing g -> view_well model g :: view_panel model g
       | Game_over g ->
           (view_well model g :: view_panel model g)
           @ [ rectangle black 400. 120. |> fade 0.8 |> move well_x 0.; text red 5. "GAME OVER" |> move well_x 0. ])

let app = game view update initial_model
let main = Playground_platform.run_app ~flags:(Playground_platform.flags ()) app
