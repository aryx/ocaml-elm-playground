(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Sokoban ("warehouse keeper", Hiroyuki Imabayashi,
 * Thinking Rabbit, 1982): push every box ('$') onto a goal ('.'); you
 * can push a box, one at a time, but never pull it, so a box pushed into
 * a corner is lost -- think before you push. Arrows to move, u (or
 * backspace) to undo a move, r to restart the level.
 *
 * The rules fit in a few lines (see [step]), yet the puzzles are deep:
 * deciding whether a level can be solved is PSPACE-complete (Joseph
 * Culberson, "Sokoban is PSPACE-complete", 1997), and Sokoban solvers
 * became a research topic of their own in AI (Andreas Junghanns and
 * Jonathan Schaeffer's Rolling Stone, 2001).
 *
 * The levels are strings in Sokoban's own text format, which players
 * still exchange levels in: '#' a wall, '@' the player, '$' a box, '.' a
 * goal, '*' a box on a goal, '+' the player on a goal. A level is a
 * Tilemap; the player, who moves over the goals, is kept apart. The
 * three levels here are ours (not the original's), checked by a
 * breadth-first search over the positions: their shortest solutions
 * are 7, 37 and 23 moves.
 *
 * Undo is where the Elm architecture shines: the model is a value, so
 * the history is just the list of the past boards, and undoing is taking
 * the head of the list (the puzzle kit's Undo, gamekits/puzzle/, with the
 * push itself, Push: a chain of one box at most). Scene2d gives the
 * title and the "solved" screens, and keys pressed rather than held:
 * one press, one step.
 *
 * Exercises: more levels (the classic free collections, e.g. David W.
 * Skinner's Microban, are in the same format), a solver showing a
 * solution (the breadth-first search above: a set of seen positions, a
 * queue), dead squares (corners where a box can never leave) drawn in
 * red.
 *)
open Playground

(*****************************************************************************)
(* The levels *)
(*****************************************************************************)

let levels : string list list =
  [
    [ "#######";
      "#     #";
      "# $@$ #";
      "# . . #";
      "#######" ];
    [ "  #####";
      "###   #";
      "# $ # ##";
      "# #  . #";
      "#    # #";
      "## #   #";
      " #@ $.##";
      " #######" ];
    [ "########";
      "#  .   #";
      "# $##$ #";
      "#. @   #";
      "#  ##$ #";
      "#   .  #";
      "########" ];
  ]

let tile_size = 60.

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

(* the map holds the walls, goals and boxes ('#', '.', '$', '*', ' ');
 * the player is at (col, row) *)
type board = { map : Tilemap.t; col : int; row : int; moves : int; pushes : int }

type play = {
  level : int;
  boards : board Undo.t; (* the board now, and the ones before *)
}

type scene = Title | Playing of play | Solved of play

type model = scene Scene2d.t

let load (level : int) : play =
  let map = Tilemap.of_strings tile_size (List.nth levels level) in
  let col, row = match Tilemap.find map '@' @ Tilemap.find map '+' with p :: _ -> p | [] -> (0, 0) in
  (* the player is not part of the map: what's under them stays *)
  let under = if Tilemap.get map col row = Some '+' then '.' else ' ' in
  { level; boards = { map = Tilemap.set map col row under; col; row; moves = 0; pushes = 0 } |> Undo.start }

let initial_model : model = Scene2d.start Title

(*****************************************************************************)
(* The rules *)
(*****************************************************************************)

let is_box (c : char option) = c = Some '$' || c = Some '*'
let is_free (c : char option) = c = Some ' ' || c = Some '.'

(* a box leaving a cell, arriving in one: the goals stay *)
let without_box (c : char option) = if c = Some '*' then '.' else ' '
let with_box (c : char option) = if c = Some '.' then '*' else '$'

(* One step in direction (dc, dr): into a free cell, or pushing a box
 * into the free cell behind it; otherwise (a wall, two boxes in a
 * row), nothing.
 *
 *     @$ .   ->    @$.   ->    @*      a push, then another: on the goal
 *)
let step (b : board) ((dc, dr) : int * int) : board option =
  let get (c, r) = Tilemap.get b.map c r in
  match Push.chain ~blocked:(fun p -> not (is_free (get p))) ~pushable:(fun p -> is_box (get p)) ~limit:1 (b.col, b.row) (dc, dr) with
  | None -> None
  | Some [] -> Some { b with col = b.col + dc; row = b.row + dr; moves = b.moves + 1 }
  | Some chain ->
      (* the box from the chain's one cell to the next *)
      let c1, r1 = List.hd chain in
      let c2 = c1 + dc and r2 = r1 + dr in
      let map = Tilemap.set b.map c1 r1 (without_box (get (c1, r1))) in
      let map = Tilemap.set map c2 r2 (with_box (get (c2, r2))) in
      Some { map; col = c1; row = r1; moves = b.moves + 1; pushes = b.pushes + 1 }

(* solved: no box left off a goal *)
let solved (b : board) : bool = Tilemap.find b.map '$' = []

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let pressed (key : keyboard -> bool) (s : model) = Scene2d.pressed key s
let letter (l : string) (k : keyboard) = Set_.mem l k.keys

let update_play (s : model) (p : play) : play =
  let dir =
    if pressed (fun k -> k.kup) s then Some (0, -1)
    else if pressed (fun k -> k.kdown) s then Some (0, 1)
    else if pressed (fun k -> k.kleft) s then Some (-1, 0)
    else if pressed (fun k -> k.kright) s then Some (1, 0)
    else None
  in
  match dir with
  | Some d -> (
      match step p.boards.now d with
      | Some board -> { p with boards = Undo.record board p.boards }
      | None -> p)
  | None when pressed (fun k -> k.kbackspace) s || pressed (letter "u") s -> (
      { p with boards = Undo.undo p.boards })
  | None when pressed (letter "r") s -> load p.level
  | None -> p

let update (computer : computer) (s : model) : model =
  let s = Scene2d.update computer s in
  let space = pressed (fun k -> k.kspace) s in
  match s.scene with
  | Title -> if space then Scene2d.go (Playing (load 0)) s else s
  | Playing p ->
      let p = update_play s p in
      if solved p.boards.now then Scene2d.go (Solved p) s else { s with scene = Playing p }
  | Solved p ->
      if not space then s
      else if p.level + 1 < List.length levels then Scene2d.go (Playing (load (p.level + 1))) s
      else Scene2d.go Title s

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let player =
  Sprite.pixels 6. [ ('#', rgb 250 250 250) ]
    [ "..###.."; "..###.."; "...#..."; ".#####."; "#.###.#"; "..#.#.."; ".##.##." ]

let box (color : color) = group [ square (rgb 110 70 30) 52.; square color 40. ]

let tile (c : char) : shape =
  match c with
  | '#' -> group [ square (rgb 90 90 110) 60.; square (rgb 120 120 140) 50. ]
  | '.' -> circle (rgb 240 200 40) 10.
  | '$' -> box (rgb 170 110 50)
  | '*' -> box (rgb 240 200 40)
  | _ -> group []

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

let view_board (p : play) : shape list =
  let b = p.boards.now in
  let px, py = Tilemap.center b.map b.col b.row in
  [ Tilemap.view tile b.map; player |> move px py;
    text white 3. (Printf.sprintf "LEVEL %d   MOVES %d   PUSHES %d" (p.level + 1) b.moves b.pushes) |> move_y 420.;
    text gray 2. "arrows: move   u: undo   r: restart" |> move_y (-420.) ]

let view (computer : computer) (s : model) : shape list =
  rectangle (rgb 40 40 50) computer.screen.width computer.screen.height
  ::
  (match s.scene with
  | Title ->
      [ text white 6. "TINY SOKOBAN" |> move_y 150.;
        text gray 2.5 "push every box onto a goal" |> move_y 50.;
        box (rgb 240 200 40) |> move (-60.) (-60.);
        player |> move 0. (-60.) ]
      @ Scene2d.blink 1. s [ text (rgb 240 200 40) 3. "PRESS SPACE" |> move_y (-200.) ]
  | Playing p -> view_board p
  | Solved p ->
      view_board p
      @ [ text (rgb 240 200 40) 5. "SOLVED!" |> move_y 300. ]
      @ Scene2d.blink 1. s [ text white 3. "PRESS SPACE" |> move_y (-330.) ])

let app = game view update initial_model

let main = Playground_platform.run_app app
