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
 * The levels are in Sokoban's own text format, which players still
 * exchange levels in: '#' a wall, '@' the player, '$' a box, '.' a
 * goal, '*' a box on a goal, '+' the player on a goal. They are not in
 * this file but in TinySokoban.xsb, beside it, which the level editor
 * (TinySokobanEd) writes and dune embeds in the game (Sokoban_levels,
 * see the dune file): the game carries its levels with it, and needs
 * no file at run time. A level is a Tilemap; the player, who moves over
 * the goals, is kept apart. The three levels are ours (not the
 * original's); the kit's solver finds their shortest solutions: 7, 37
 * and 23 moves.
 *
 * The rules are the puzzle kit's (gamekits/puzzle/): Sokoban's [step]
 * and [solved], shared with the editor, which plays a level to test
 * it, on Push (a chain of one box at most). Undo is where the Elm
 * architecture shines: the model is a value, so the history is just
 * the list of the past boards, and undoing is taking the head of the
 * list (the kit's Undo). Scene2d gives the title and the "solved"
 * screens, and keys pressed rather than held: one press, one step.
 *
 * Exercises: more levels (the classic free collections, e.g. David W.
 * Skinner's Microban, are in the same format: append one to
 * TinySokoban.xsb), a hint key showing the solution (Sokoban.solve),
 * dead squares (corners where a box can never leave) drawn in red.
 *)
open Playground

(*****************************************************************************)
(* The levels *)
(*****************************************************************************)

(* TinySokoban.xsb's, e.g. the first:
 *
 *     #######
 *     #     #
 *     # $@$ #
 *     # . . #
 *     #######
 *)
let levels : string list list = Sokoban.of_xsb Sokoban_levels.xsb

let tile_size = 60.

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

(* the map holds the walls, goals and boxes; the player is apart *)
type board = Sokoban.board

type play = {
  level : int;
  boards : board Undo.t; (* the board now, and the ones before *)
}

type scene = Title | Playing of play | Solved of play

type model = scene Scene2d.t

let load (level : int) : play = { level; boards = Sokoban.start tile_size (List.nth levels level) |> Undo.start }

let initial_model : model = Scene2d.start Title

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
      match Sokoban.step p.boards.now d with
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
      if Sokoban.solved p.boards.now then Scene2d.go (Solved p) s else { s with scene = Playing p }
  | Solved p ->
      if not space then s
      else if p.level + 1 < List.length levels then Scene2d.go (Playing (load (p.level + 1))) s
      else Scene2d.go Title s

(*****************************************************************************)
(* View *)
(*****************************************************************************)

(* the look is the kit's, the editor's too *)
let player = Sokoban.player

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

let view_board (p : play) : shape list =
  let b = p.boards.now in
  let px, py = Tilemap.center b.map b.col b.row in
  [ Tilemap.view Sokoban.tile b.map; player |> move px py;
    text white 3. (Printf.sprintf "LEVEL %d   MOVES %d   PUSHES %d" (p.level + 1) b.moves b.pushes) |> move_y 420.;
    text gray 2. "arrows: move   u: undo   r: restart" |> move_y (-420.) ]

let view (computer : computer) (s : model) : shape list =
  rectangle (rgb 40 40 50) computer.screen.width computer.screen.height
  ::
  (match s.scene with
  | Title ->
      [ text white 6. "TINY SOKOBAN" |> move_y 150.;
        text gray 2.5 "push every box onto a goal" |> move_y 50.;
        Sokoban.tile '*' |> move (-60.) (-60.);
        player |> move 0. (-60.) ]
      @ Scene2d.blink 1. s [ text (rgb 240 200 40) 3. "PRESS SPACE" |> move_y (-200.) ]
  | Playing p -> view_board p
  | Solved p ->
      view_board p
      @ [ text (rgb 240 200 40) 5. "SOLVED!" |> move_y 300. ]
      @ Scene2d.blink 1. s [ text white 3. "PRESS SPACE" |> move_y (-330.) ])

let app = game view update initial_model

let main = Playground_platform.run_app app
