(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of BlockOut (P.Z.Karen Co. Development Group --
 * Aleksander Ustaszewski and Miroslaw Zablocki -- published by
 * California Dreams, 1989): Tetris down a well. A solid falls into a
 * pit you are looking straight down into, you slide it across and turn
 * it about three axes, and a whole *layer* has to be filled before it
 * goes away. Arrows slide, z/x/c turn, space drops. (Names and dates
 * from memory, to check.)
 *
 * The rules are Tetris with one more index, and the code says so: the
 * pit is an array of cols x levels x rows instead of a grid, a layer is
 * full when its cols * rows cells are, and everything above a cleared
 * layer comes down one. If that were all of it, this file would be
 * games/Tetris.ml with a loop added. What makes BlockOut a different
 * game is the thing no rule mentions: from up here you cannot tell how
 * deep anything is.
 *
 * So most of the work below is not rules but *depth cues*, and that is
 * the interesting part of it:
 *
 *   - the camera sits just above the mouth of the pit and looks down
 *     it, so the near walls are wide and the floor is small: the
 *     perspective alone tells you roughly how far down you are looking;
 *   - a settled cube is drawn darker the deeper it lies ([shade]), so
 *     the stack reads as a relief map;
 *   - the ring of the well at the level the piece would land on is
 *     lit ([landing]), which is the exact answer to "how far down is
 *     this going". It has to be the ring, and this is the nice part:
 *     a game seen from the side can draw a drop shadow under the
 *     falling piece, and one seen from straight above cannot, because
 *     the shadow is always exactly behind the thing casting it. So the
 *     cue has to go somewhere the piece is not, and the wall of the
 *     well is the only such place. BlockOut had no cue at all and
 *     expected you to read the perspective, which is part of why it
 *     was hard;
 *   - the walls are a wireframe with a ring every level, so you can
 *     count.
 *
 * Turning a solid needs no trigonometry and no floating point. A piece
 * is a handful of integer cells that fill their bounding box, and a
 * quarter turn is that box turned: the cell (x, y, z) of a box sx x sy
 * x sz becomes (sz - 1 - z, y, x) about the vertical axis, which is the
 * "rotate a square matrix in place" exercise, done on two of the three
 * axes at a time ([turn_x], [turn_y], [turn_z]). Cells stay in their
 * box, so a piece is always normalized and a turn can never drift.
 * (A polycube has up to 24 orientations, the rotation group of the
 * cube, where a tetromino has 4 -- which is the other reason this is
 * harder than it looks.)
 *
 * What it uses: playground3d (cube, box, camera, hud) and its software
 * z-buffer. Not the kits, and nothing from games/Tetris.ml: its logic
 * came from elm-flatris and is written around a 2D grid, so the three
 * rules here are shorter re-derived than translated.
 *
 * Exercises: BlockOut's three piece sets (Flat is the tetrominoes lying
 * down, which is the easy game; Basic adds the solids below; Extended
 * is all the pentacubes); its scoring, which pays you for dropping a
 * piece far rather than for clearing; a pit that is not square; the
 * layer flashing before it goes; and the hardest one -- a camera you
 * can tilt, which BlockOut does not have, to find out whether it helps
 * or whether the fixed view down the shaft was the right answer.
 *)
open Playground
open Playground3d

(*****************************************************************************)
(* The pit *)
(*****************************************************************************)

(* across, across, and down: y = 0 is the mouth, y = levels - 1 the floor *)
let cols = 5
let rows = 5
let levels = 10
let index (x : int) (y : int) (z : int) : int = (((y * rows) + z) * cols) + x
let floor_cells : (int * int) list = List.concat_map (fun z -> List.init cols (fun x -> (x, z))) (List.init rows Fun.id)

(*****************************************************************************)
(* The pieces *)
(*****************************************************************************)

(* A piece is the cells it fills, each in its own bounding box (the
 * smallest coordinate on every axis is 0). The first five are the
 * tetrominoes lying flat, BlockOut's "Flat" set; the last three are
 * solid, from its "Basic" one. *)
type piece = { cells : (int * int * int) list; color : color }

let pieces : piece list =
  [ { cells = [ (0, 0, 0); (1, 0, 0); (2, 0, 0); (3, 0, 0) ]; color = rgb 80 200 230 };
    { cells = [ (0, 0, 0); (1, 0, 0); (0, 0, 1); (1, 0, 1) ]; color = rgb 240 210 70 };
    { cells = [ (0, 0, 0); (1, 0, 0); (2, 0, 0); (1, 0, 1) ]; color = rgb 200 110 220 };
    { cells = [ (0, 0, 0); (1, 0, 0); (2, 0, 0); (2, 0, 1) ]; color = rgb 240 150 60 };
    { cells = [ (0, 0, 0); (1, 0, 0); (1, 0, 1); (2, 0, 1) ]; color = rgb 110 220 110 };
    (* a corner: three arms from one cube, the piece that teaches you
     * that a layer is not a row *)
    { cells = [ (0, 0, 0); (1, 0, 0); (0, 0, 1); (0, 1, 0) ]; color = rgb 230 90 90 };
    (* a screw: it comes out of the wall on the way down *)
    { cells = [ (0, 0, 0); (1, 0, 0); (1, 0, 1); (1, 1, 1) ]; color = rgb 120 140 240 };
    { cells = [ (0, 0, 0); (1, 0, 0); (0, 1, 0); (1, 1, 0) ]; color = rgb 180 180 190 } ]

(* the size of a piece's bounding box *)
let extent (cells : (int * int * int) list) : int * int * int =
  let m f = 1 + List.fold_left (fun a c -> max a (f c)) 0 cells in
  (m (fun (x, _, _) -> x), m (fun (_, y, _) -> y), m (fun (_, _, z) -> z))

(* A quarter turn is the bounding box turned, which keeps every cell in
 * the box and so keeps the piece normalized: no centre to rotate
 * about, no rounding, no drift. *)
let turn_y (cells : (int * int * int) list) : (int * int * int) list =
  let _, _, sz = extent cells in
  List.map (fun (x, y, z) -> (sz - 1 - z, y, x)) cells

let turn_x (cells : (int * int * int) list) : (int * int * int) list =
  let _, _, sz = extent cells in
  List.map (fun (x, y, z) -> (x, sz - 1 - z, y)) cells

let turn_z (cells : (int * int * int) list) : (int * int * int) list =
  let _, sy, _ = extent cells in
  List.map (fun (x, y, z) -> (sy - 1 - y, x, z)) cells

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type game = {
  stack : color option array; (* cols * levels * rows, None where empty *)
  piece : piece;
  at : int * int * int; (* the piece's box, its smallest corner, in the pit *)
  next : piece;
  fall : int; (* frames before it drops one level *)
  dropped : int; (* how far this piece has been pushed down: BlockOut pays for that *)
  score : int;
  cleared : int;
  frames : int;
}

type scene = Title | Playing of game | Over of int
type model = scene Scene2d.t

let any_piece () : piece = List.nth pieces (Random.int (List.length pieces))

(* the falling speed: one level every so many frames, quicker as the
 * layers go *)
let fall_delay (cleared : int) : int = max 8 (45 - (3 * cleared))

(* a piece enters at the top, in the middle of the mouth *)
let entry (p : piece) : int * int * int =
  let sx, _, sz = extent p.cells in
  ((cols - sx) / 2, 0, (rows - sz) / 2)

let world (g : game) : (int * int * int) list =
  let ax, ay, az = g.at in
  List.map (fun (x, y, z) -> (ax + x, ay + y, az + z)) g.piece.cells

let free (g : game) (cells : (int * int * int) list) : bool =
  List.for_all
    (fun (x, y, z) -> x >= 0 && y >= 0 && z >= 0 && x < cols && y < levels && z < rows && g.stack.(index x y z) = None)
    cells

(* the piece moved, turned, or dropped, if it fits there *)
let try_at (g : game) (at : int * int * int) (cells : (int * int * int) list) : game option =
  let ax, ay, az = at in
  let there = List.map (fun (x, y, z) -> (ax + x, ay + y, az + z)) cells in
  if free g there then Some { g with at; piece = { g.piece with cells } } else None

let new_game () : game =
  { stack = Array.make (cols * levels * rows) None;
    piece = any_piece (); at = (0, 0, 0); next = any_piece (); fall = fall_delay 0; dropped = 0; score = 0; cleared = 0; frames = 0 }

let start_game () : game =
  let g = new_game () in
  { g with at = entry g.piece }

(*****************************************************************************)
(* Clearing a layer *)
(*****************************************************************************)

(* A layer is full when its cols * rows cells are -- a whole floor of
 * the well, not a line. What is left keeps its order and falls to the
 * bottom of the pit. *)
let clear_layers (stack : color option array) : color option array * int =
  let full y = List.for_all (fun (x, z) -> stack.(index x y z) <> None) floor_cells in
  let kept = List.filter (fun y -> not (full y)) (List.init levels Fun.id) in
  let gone = levels - List.length kept in
  let s = Array.make (cols * levels * rows) None in
  List.iteri
    (fun i y -> List.iter (fun (x, z) -> s.(index x (gone + i) z) <- stack.(index x y z)) floor_cells)
    kept;
  (s, gone)

(* the piece settles where it is, its layers go, and the next one enters *)
let land_piece (g : game) : game option =
  let stack = Array.copy g.stack in
  List.iter (fun (x, y, z) -> stack.(index x y z) <- Some g.piece.color) (world g);
  let stack, gone = clear_layers stack in
  let g =
    { g with stack; cleared = g.cleared + gone;
      (* BlockOut pays for the cubes and for having pushed them down,
       * and much more for a layer *)
      score = g.score + (List.length g.piece.cells * (1 + g.dropped)) + (gone * gone * 100);
      piece = g.next; next = any_piece (); dropped = 0; fall = fall_delay (g.cleared + gone) }
  in
  let g = { g with at = entry g.piece } in
  if free g (world g) then Some g else None

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

(* one level down, or settled *)
let descend (g : game) : game option =
  let ax, ay, az = g.at in
  match try_at g (ax, ay + 1, az) g.piece.cells with
  | Some g -> Some { g with fall = fall_delay g.cleared }
  | None -> land_piece g

let slide (g : game) (dx : int) (dz : int) : game =
  let ax, ay, az = g.at in
  Option.value (try_at g (ax + dx, ay, az + dz) g.piece.cells) ~default:g

(* A turn can put the piece through a wall or into the stack. BlockOut
 * refuses it then, rather than nudging the piece aside the way modern
 * Tetris does with its "kicks": in a pit you can see into, being moved
 * without asking is worse than being refused. *)
let spin (g : game) (f : (int * int * int) list -> (int * int * int) list) : game =
  Option.value (try_at g g.at (f g.piece.cells)) ~default:g

let update_game (s : model) (g : game) : game option =
  let g = { g with frames = g.frames + 1 } in
  let pressed f = Scene2d.pressed f s in
  let key c f g = if pressed (fun k -> Set_.mem c k.keys) then f g else g in
  let g = if pressed (fun k -> k.kleft) then slide g (-1) 0 else g in
  let g = if pressed (fun k -> k.kright) then slide g 1 0 else g in
  let g = if pressed (fun k -> k.kup) then slide g 0 (-1) else g in
  let g = if pressed (fun k -> k.kdown) then slide g 0 1 else g in
  let g = key "z" (fun g -> spin g turn_x) g in
  let g = key "x" (fun g -> spin g turn_y) g in
  let g = key "c" (fun g -> spin g turn_z) g in
  if pressed (fun k -> k.kspace) then
    (* a hard drop: down until it will not go *)
    let rec fall g =
      let ax, ay, az = g.at in
      match try_at g (ax, ay + 1, az) g.piece.cells with Some g -> fall { g with dropped = g.dropped + 1 } | None -> g
    in
    land_piece (fall g)
  else if g.fall > 1 then Some { g with fall = g.fall - 1 }
  else descend { g with dropped = g.dropped + 1 }

let update (computer : computer) (s : model) : model =
  let s = Scene2d.update computer s in
  let space = Scene2d.pressed (fun k -> k.kspace) s in
  match s.scene with
  | Title -> if space then Scene2d.go (Playing (start_game ())) s else s
  | Playing g -> (
      match update_game s g with Some g -> { s with scene = Playing g } | None -> Scene2d.go (Over g.score) s)
  | Over _ -> if space then Scene2d.go Title s else s

(*****************************************************************************)
(* Where the piece will land *)
(*****************************************************************************)

(* Straight down until it will not go: the shadow this draws on the
 * stack is the one thing that makes the pit readable (see the header). *)
let landing (g : game) : (int * int * int) list =
  let rec fall (ax, ay, az) =
    let below = List.map (fun (x, y, z) -> (ax + x, ay + y + 1, az + z)) g.piece.cells in
    if free g below then fall (ax, ay + 1, az) else (ax, ay, az)
  in
  let ax, ay, az = fall g.at in
  List.map (fun (x, y, z) -> (ax + x, ay + y, az + z)) g.piece.cells

(*****************************************************************************)
(* View *)
(*****************************************************************************)

(* the pit in world units: one cube a side, the mouth at y = 0, the
 * floor at y = -levels, the middle of the shaft on the y axis *)
let spot (x : int) (y : int) (z : int) : number * number * number =
  (float_of_int x -. (float_of_int (cols - 1) /. 2.), -.float_of_int y, float_of_int z -. (float_of_int (rows - 1) /. 2.))

let at_cell (s : shape3d) (x : int) (y : int) (z : int) : shape3d =
  let wx, wy, wz = spot x y z in
  move3d wx wy wz s

(* deeper is darker: the stack reads as a relief *)
let shade (y : int) (c : color) : color =
  let f = 0.85 -. (0.5 *. (float_of_int y /. float_of_int levels)) in
  let v x = int_of_float (Float.max 0. (float_of_int x *. f)) in
  match c with Rgb (r, g, b) -> rgb (v r) (v g) (v b) | _ -> c

let bar : color = rgb 90 96 120

(* the dark outside the well, so the shaft reads as a hole and not as a
 * box floating in the air: a plane far below, filling the view (the
 * camera looks straight down at it) *)
let void : color = rgb 16 16 26

let half_x = float_of_int cols /. 2.
let half_z = float_of_int rows /. 2.

(* one course of the well's brickwork, at the top of level [y] *)
let ring ?(thick = 0.05) (c : color) (y : int) : shape3d list =
  let y = -.float_of_int y +. 0.5 in
  [ box c (float_of_int cols +. thick) thick thick |> move3d 0. y (-.half_z);
    box c (float_of_int cols +. thick) thick thick |> move3d 0. y half_z;
    box c thick thick (float_of_int rows +. thick) |> move3d (-.half_x) y 0.;
    box c thick thick (float_of_int rows +. thick) |> move3d half_x y 0. ]

(* the well: its four upright edges, a ring at every level, and its
 * floor, so that depth can be counted and not only guessed *)
let pit_shape : shape3d =
  let deep = float_of_int levels in
  let upright (sx, sz) = box bar 0.08 deep 0.08 |> move3d (sx *. half_x) (-.deep /. 2. +. 0.5) (sz *. half_z) in
  group3d
    (List.map upright [ (-1., -1.); (-1., 1.); (1., -1.); (1., 1.) ]
    @ List.concat_map (fun y -> ring (if y mod 5 = 0 then rgb 130 140 175 else bar) y) (List.init (levels + 1) Fun.id)
    @ [ plane (rgb 38 40 52) (float_of_int cols) (float_of_int rows) |> move3d 0. (-.float_of_int levels +. 0.5) 0. ])

(* looking straight down the shaft, from just above its mouth: the near
 * walls are wide, the floor is small, and that is the depth cue you get
 * for free *)
let eye_height = 7.

let look : camera =
  camera ~eye:(0., eye_height, 0.) ~target:(0., -.float_of_int levels /. 2., 0.) ~up:(0., 0., -1.) ~fov:60. ()

let view_pit (g : game) : shape3d list =
  let settled =
    List.filter_map
      (fun i ->
        let x = i mod cols and z = i / cols mod rows and y = i / (cols * rows) in
        match g.stack.(index x y z) with None -> None | Some c -> Some (at_cell (cube (shade y c) 0.92) x y z))
      (List.init (cols * levels * rows) Fun.id)
  in
  (* the lit ring, at the deepest level the piece would reach *)
  let deepest = List.fold_left (fun a (_, y, _) -> max a y) 0 (landing g) in
  let shadow = ring ~thick:0.12 (rgb 245 240 160) deepest in
  (* the piece itself is never dimmed, and is a touch bigger than a
   * settled cube: at a glance, the brightest thing in the pit is the
   * one you are still holding *)
  let falling = List.map (fun (x, y, z) -> at_cell (cube g.piece.color 0.98) x y z) (world g) in
  (Camera3d.floor ~color:void ~ground:(-.float_of_int levels -. 8.) look :: pit_shape :: settled) @ shadow @ falling

(*****************************************************************************)
(* The panel *)
(*****************************************************************************)

let text (c : color) (size : number) (s : string) : shape = words c s |> scale size

(* the next piece, sketched flat: x - z across, and down for y, with the
 * nearest cubes drawn last *)
let sketch (p : piece) : shape =
  let cell (x, y, z) =
    square p.color 18.
    |> move (float_of_int (x - z) *. 14.) ((float_of_int (x + z) *. -8.) -. (float_of_int y *. 19.))
  in
  let order (x, y, z) = x + z - (3 * y) in
  group (List.map cell (List.sort (fun a b -> compare (order a) (order b)) p.cells))

let panel (screen : screen) (g : game) : shape list =
  [ text (rgb 200 210 230) 2. (Printf.sprintf "SCORE %d" g.score) |> move (screen.left +. 130.) (screen.top -. 40.);
    text (rgb 200 210 230) 2. (Printf.sprintf "LAYERS %d" g.cleared) |> move (screen.left +. 130.) (screen.top -. 80.);
    text (rgb 150 160 180) 1.6 "NEXT" |> move (screen.right -. 150.) (screen.top -. 40.);
    sketch g.next |> move (screen.right -. 150.) (screen.top -. 110.);
    text (rgb 120 130 150) 1.6 "arrows slide   z x c turn   space drops" |> move_y (screen.bottom +. 34.) ]

(*****************************************************************************)
(* Scenes *)
(*****************************************************************************)

let view (computer : computer) (s : model) : camera * shape3d list =
  let screen = computer.screen in
  match s.scene with
  | Title ->
      let g = start_game () in
      ( look,
        view_pit g
        @ List.map hud
            ([ text (rgb 120 200 240) 6. "TINY BLOCKOUT" |> move_y 300.;
               text white 2.2 "a solid falls down the well; fill a whole layer and it goes" |> move_y 240.;
               text white 2.2 "arrows slide   z x c turn about the three axes   space drops" |> move_y 205. ]
            @ Scene2d.blink 1. s [ text yellow 3. "PRESS SPACE" |> move_y 150. ]) )
  | Playing g -> (look, view_pit g @ List.map hud (panel screen g))
  | Over score ->
      ( look,
        [ pit_shape ]
        @ List.map hud
            ([ text (rgb 240 120 110) 6. "THE PIT IS FULL" |> move_y 200.;
               text white 3. (Printf.sprintf "SCORE %d" score) |> move_y 130. ]
            @ Scene2d.blink 1. s [ text white 2.5 "PRESS SPACE" |> move_y 60. ]) )

let app = game3d view update (Scene2d.start Title)

(* claude: the seed is read here and not at the top of the file, where
 * the other games read it, because reading it means parsing the command
 * line: in tests/games, which links this file and calls nothing, that
 * would see the test runner's own arguments and exit. Inside [main] it
 * only happens when the game is really run, and no piece is drawn
 * before that (the first scene is the title). seed=n (Playground.flags)
 * gives the same pieces every run, for the golden frames.
 *
 * Flat shading: every face of every cube gets its own shade, which is
 * one more thing telling you which way a cube is facing. *)
let main =
  (match List.assoc_opt "seed" (Playground_platform.flags ()) with
  | Some n -> Random.init (int_of_string n)
  | None -> Random.self_init ());
  Playground3d_platform.run_app3d ~rendering:{ default_rendering with shading = Flat } app
