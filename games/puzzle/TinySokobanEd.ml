(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* TinySokoban's level editor: the game's own levels, to change, add to,
 * test by playing them, check with a solver, and write back as the file
 * the game is built with.
 *
 * Type a character of Sokoban's text format and it is written at the
 * cursor, which moves on as in a text editor: '#' a wall, '$' a box, '.'
 * a goal, '@' the player (only one: the one before goes), '*' a box on
 * a goal, '+' the player on a goal, space (or '-') the floor. The
 * arrows move the cursor, the mouse paints with the last character
 * typed, or the one picked in the palette. Enter tests the level
 * (arrows, u to undo, r to restart; Escape or Enter to come back), s
 * asks the solver, Tab goes to the next level (with shift, the one
 * before), n adds a level, and e exports them all as TinySokoban.xsb:
 * natively in the current directory, in the browser as a download.
 * Copied over the game's TinySokoban.xsb, it is the game's levels at
 * the next build.
 *
 * Every game with levels needed a tool to make them, and some shipped
 * theirs: Lode Runner (Doug Smith, Broderbund, 1983) had its level
 * editor on the disk, and its players made levels by the thousand;
 * Pinball Construction Set (Bill Budge, 1983) was a game made to be
 * one; Doom's editor, DoomEd, ran on NeXTSTEP, but its level format
 * was published, so that fans wrote their own (DEU, 1994) and made
 * what came to be called mods. Sokoban's editor is the simplest of all:
 * its levels are text, and any text editor will do -- which is why
 * this one works like a text editor, only on a grid, with the checks a
 * text editor cannot do.
 *
 * How a game's tool is laid out in this repository (see
 * games/README-tools.md): the editor is a program of the game's genre
 * (games/puzzle/), because what it makes only this game reads. What the
 * two share is in the genre's kit: the rules, the look, the level
 * format and its solver (gamekits/puzzle/Sokoban). The level lives in
 * a file in the game's own format, not in OCaml: the editor writes it,
 * a text editor can too, and dune embeds it in the game at build time
 * (Sokoban_levels, see the dune file), so the game still needs no file
 * at run time, native or in the browser.
 *
 * Uses: Tilemap (the level edited), Scene2d (keys pressed), the puzzle
 * kit (Sokoban, and Undo for the tests), Playground_platform.export
 * with its capability (Cap.open_out: the only program of the genre
 * that writes a file). Not: File_menu (apps/office/'s, not a
 * game's), physics.
 *
 * Exercises: delete a level, move one up or down; Open and Save in the
 * store (Playground_platform.fetch/store) to keep work in progress;
 * undo in the editor (the kit's Undo, over the map); a solver that
 * searches pushes rather than moves, to answer for bigger levels; show
 * the solution played by itself.
 *)
open Playground

(*****************************************************************************)
(* The canvas *)
(*****************************************************************************)

let tile_size = 60.

(* the grid a level is edited on: bigger than the game's levels, so
 * that there is room to grow one *)
let canvas_cols = 12
let canvas_rows = 10

(* the canvas' center, on the screen: under the status lines *)
let canvas_y = 20.

(* [on_canvas rows]: the level in the middle of the canvas *)
let on_canvas (rows : string list) : Tilemap.t =
  let width = List.fold_left (fun acc r -> max acc (String.length r)) 0 rows in
  let cols = max canvas_cols width and nrows = max canvas_rows (List.length rows) in
  let left = (cols - width) / 2 and top = (nrows - List.length rows) / 2 in
  List.init nrows (fun r ->
      let row = if r < top then "" else Option.value (List.nth_opt rows (r - top)) ~default:"" in
      String.make left ' ' ^ row ^ String.make (cols - left - String.length row) ' ')
  |> Tilemap.of_strings tile_size

(* the level being edited, as the file will have it *)
let rows_of (map : Tilemap.t) : string list = Sokoban.trim (Tilemap.to_strings map)

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type mode = Editing | Testing of Sokoban.board Undo.t

type editor = {
  (* the file: every level, the one edited as it was when chosen *)
  levels : string list list;
  level : int;
  map : Tilemap.t; (* the level edited *)
  cursor : int * int;
  brush : char; (* what the mouse paints *)
  mode : mode;
  (* the solver's answer, if asked since the last change *)
  solution : Sokoban.solution option;
  said : string; (* what the last command did *)
}

type model = editor Scene2d.t

let choose (level : int) (levels : string list list) : editor =
  let map = on_canvas (List.nth levels level) in
  { levels; level; map; cursor = (Tilemap.cols map / 2, Tilemap.rows map / 2); brush = '#'; mode = Editing;
    solution = None; said = "" }

(* the game's own levels, to start with *)
let initial_model : model = Scene2d.start (choose 0 (Sokoban.of_xsb Sokoban_levels.xsb))

(* the level edited, put back in the file *)
let commit (e : editor) : string list list = List.mapi (fun i rows -> if i = e.level then rows_of e.map else rows) e.levels

(*****************************************************************************)
(* Editing *)
(*****************************************************************************)

(* the characters the canvas takes, the palette's order *)
let brushes = [ '#'; ' '; '$'; '.'; '@'; '*'; '+' ]

(* [paint e c]: the character [c] at the cursor. One player: painting
 * one takes away the one before (off a goal, leaving it). *)
let paint (e : editor) (c : char) : editor =
  let c = if c = '-' || c = '_' then ' ' else c in
  let col, row = e.cursor in
  let map =
    if c = '@' || c = '+' then
      Tilemap.find e.map '@' @ Tilemap.find e.map '+'
      |> List.fold_left (fun m (c', r') -> Tilemap.set m c' r' (if Tilemap.get m c' r' = Some '+' then '.' else ' ')) e.map
    else e.map
  in
  let map = Tilemap.set map col row c in
  (* the solver's answer, and what the last command said, hold until
   * the level changes *)
  if Tilemap.to_strings map = Tilemap.to_strings e.map then { e with brush = c }
  else { e with map; brush = c; solution = None; said = "" }

let move_cursor (e : editor) ((dc, dr) : int * int) : editor =
  let col, row = e.cursor in
  let clamp n hi = max 0 (min (hi - 1) n) in
  { e with cursor = (clamp (col + dc) (Tilemap.cols e.map), clamp (row + dr) (Tilemap.rows e.map)) }

(* the palette, under the canvas: each brush at its x *)
let palette_y = -330.
let palette_x (i : int) : number = (float_of_int i -. 3.) *. 80.

(* the cell under the mouse, if on the canvas *)
let cell_at (map : Tilemap.t) (x : number) (y : number) : (int * int) option =
  let col, row = Tilemap.cell map x (y -. canvas_y) in
  if Tilemap.get map col row = None then None else Some (col, row)

let update_mouse (m : mouse) (e : editor) : editor =
  match cell_at e.map m.mx m.my with
  | Some cell when m.mdown -> paint { e with cursor = cell } e.brush
  | Some _ | None ->
      if not m.mclick then e
      else
        (* a click on the palette picks a brush *)
        match List.find_opt (fun (i, _) -> abs_float (m.mx -. palette_x i) < 35. && abs_float (m.my -. palette_y) < 35.) (List.mapi (fun i b -> (i, b)) brushes) with
        | Some (_, b) -> { e with brush = b }
        | None -> e

let export (caps : < Cap.open_out >) (e : editor) : editor =
  let levels = commit e in
  let bytes = Sokoban.to_xsb levels in
  Playground_platform.export caps "TinySokoban.xsb" bytes;
  { e with levels; said = Printf.sprintf "exported TinySokoban.xsb, %d bytes: copy it over the game's" (String.length bytes) }

let solve (e : editor) : editor =
  let rows = rows_of e.map in
  match Sokoban.problems rows with
  | [] -> { e with solution = Some (Sokoban.solve rows); said = "" }
  | p :: _ -> { e with said = "cannot solve: " ^ p }

let test (e : editor) : editor =
  let rows = rows_of e.map in
  match Sokoban.problems rows with
  | [] -> { e with mode = Testing (Undo.start (Sokoban.start tile_size rows)); said = "" }
  | p :: _ -> { e with said = "cannot test: " ^ p }

(* a new level after this one: an empty room *)
let room = [ "#######"; "#     #"; "#     #"; "#     #"; "#######" ]

let update_editing (caps : < Cap.open_out >) (computer : computer) (s : model) (e : editor) : editor =
  let pressed (key : keyboard -> bool) = Scene2d.pressed key s in
  let letter (l : string) = pressed (fun k -> Set_.mem l k.keys) in
  let go_to level = { (choose level (commit e)) with said = "" } in
  let n = List.length e.levels in
  (* the characters typed, each written at the cursor, which moves on *)
  let e =
    String.fold_left
      (fun e c -> if String.contains "#@$.*+ -_" c then move_cursor (paint e c) (1, 0) else e)
      e computer.keyboard.typed
  in
  if pressed (fun k -> k.kup) then move_cursor e (0, -1)
  else if pressed (fun k -> k.kdown) then move_cursor e (0, 1)
  else if pressed (fun k -> k.kleft) then move_cursor e (-1, 0)
  else if pressed (fun k -> k.kright) then move_cursor e (1, 0)
  else if pressed (fun k -> k.kbackspace) then
    let e = move_cursor e (-1, 0) in
    { (paint e ' ') with brush = e.brush }
  else if pressed (fun k -> k.kenter) then test e
  else if letter "Tab" then go_to (if computer.keyboard.kshift then (e.level + n - 1) mod n else (e.level + 1) mod n)
  else if letter "n" then
    let levels = commit e in
    let before = List.filteri (fun i _ -> i <= e.level) levels and after = List.filteri (fun i _ -> i > e.level) levels in
    { (choose (e.level + 1) (before @ [ room ] @ after)) with said = "a new level" }
  else if letter "s" then solve e
  else if letter "e" then export caps e
  else update_mouse computer.mouse e

let update_testing (s : model) (e : editor) (boards : Sokoban.board Undo.t) : editor =
  let pressed (key : keyboard -> bool) = Scene2d.pressed key s in
  let letter (l : string) = pressed (fun k -> Set_.mem l k.keys) in
  let dir =
    if pressed (fun k -> k.kup) then Some (0, -1)
    else if pressed (fun k -> k.kdown) then Some (0, 1)
    else if pressed (fun k -> k.kleft) then Some (-1, 0)
    else if pressed (fun k -> k.kright) then Some (1, 0)
    else None
  in
  match dir with
  | _ when pressed (fun k -> k.kenter) || letter "Escape" -> { e with mode = Editing }
  | Some d when not (Sokoban.solved boards.now) -> (
      match Sokoban.step boards.now d with
      | Some b -> { e with mode = Testing (Undo.record b boards) }
      | None -> e)
  | _ when pressed (fun k -> k.kbackspace) || letter "u" -> { e with mode = Testing (Undo.undo boards) }
  | _ when letter "r" -> test e
  | _ -> e

let update (caps : < Cap.open_out >) (computer : computer) (s : model) : model =
  let s = Scene2d.update computer s in
  let e = s.scene in
  let e = match e.mode with Editing -> update_editing caps computer s e | Testing boards -> update_testing s e boards in
  { s with scene = e }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size
let yellow = rgb 240 200 40

(* a brush as the canvas shows it *)
let glyph (c : char) : shape =
  match c with
  | '@' -> Sokoban.player
  | '+' -> group [ Sokoban.tile '.'; Sokoban.player ]
  | c -> Sokoban.tile c

(* the canvas' cells, faintly, so that the floor can be seen *)
let grid (map : Tilemap.t) : shape =
  let w = float_of_int (Tilemap.cols map) *. tile_size and h = float_of_int (Tilemap.rows map) *. tile_size in
  group
    (rectangle (rgb 50 50 62) w h
    :: List.init (Tilemap.cols map + 1) (fun i -> rectangle (rgb 62 62 76) 2. h |> move_x ((float_of_int i *. tile_size) -. (w /. 2.)))
    @ List.init (Tilemap.rows map + 1) (fun i -> rectangle (rgb 62 62 76) w 2. |> move_y ((float_of_int i *. tile_size) -. (h /. 2.))))

(* what the checks say *)
let status (e : editor) : string =
  match (Sokoban.problems (rows_of e.map), e.solution) with
  | p :: ps, _ -> String.concat ", " (p :: ps)
  | [], None -> "ready: s to solve, enter to test"
  | [], Some (Sokoban.Moves m) -> Printf.sprintf "solvable in %d moves: %s" (String.length m) m
  | [], Some Sokoban.Unsolvable -> "no solution"
  | [], Some (Sokoban.Gave_up n) -> Printf.sprintf "no answer after %d positions" n

let view_editing (e : editor) : shape list =
  let col, row = e.cursor in
  let cx, cy = Tilemap.center e.map col row in
  [ group [ grid e.map; Tilemap.view glyph e.map; square yellow tile_size |> fade 0.35 |> move cx cy ] |> move_y canvas_y;
    text (if Sokoban.problems (rows_of e.map) = [] then gray else rgb 240 110 90) 2. (status e) |> move_y 385. ]
  @ List.mapi
      (fun i b ->
        let x = palette_x i in
        group
          ((if b = e.brush then [ square yellow 70. |> fade 0.5 ] else [])
          @ [ glyph b; text white 1.6 (if b = ' ' then "space" else String.make 1 b) |> move_y (-50.) ])
        |> move x palette_y)
      brushes
  @ [ text gray 1.6 "type a character, or paint with the mouse   arrows: cursor" |> move_y (-415.);
      text gray 1.6 "enter: test   s: solve   tab: next level   n: new   e: export" |> move_y (-445.) ]

let view_testing (boards : Sokoban.board Undo.t) : shape list =
  let b = boards.now in
  let px, py = Tilemap.center b.map b.col b.row in
  [ group [ Tilemap.view Sokoban.tile b.map; Sokoban.player |> move px py ] |> move_y canvas_y;
    text white 2.5 (Printf.sprintf "TESTING   MOVES %d   PUSHES %d" b.moves b.pushes) |> move_y 385. ]
  @ (if Sokoban.solved b then [ text yellow 5. "SOLVED!" |> move_y (-330.) ] else [])
  @ [ text gray 1.6 "arrows: move   u: undo   r: restart   enter: back to editing" |> move_y (-430.) ]

let view (computer : computer) (s : model) : shape list =
  let e = s.scene in
  rectangle (rgb 40 40 50) computer.screen.width computer.screen.height
  :: (text white 3. (Printf.sprintf "TINY SOKOBAN ED   LEVEL %d/%d" (e.level + 1) (List.length e.levels)) |> move_y 440.)
  :: (match e.mode with Editing -> view_editing e | Testing boards -> view_testing boards)
  @ [ text yellow 1.6 e.said |> move_y 350. ]

let app (caps : < Cap.open_out >) = game view (update caps) initial_model

let main = Cap.main (fun caps -> Playground_platform.run_app (app (caps :> < Cap.open_out >)))
