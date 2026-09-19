(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* Othello against the computer, which thinks 4 moves ahead with
 * alpha-beta (ai/Minimax.mli). You are black: click a square (or move
 * the cursor with the arrows, and space); a disk placed must trap a
 * line of white ones between it and another black one, and they all
 * turn black. No move: you pass. Neither player can move: the most
 * disks wins. "v" shows what the computer thinks of each of your moves
 * (the lower, the better for you); space after the end plays again.
 *
 * After each of its moves, the computer says how many positions it
 * looked at, and how many plain minimax would have for the same move:
 * alpha-beta's cuts, counted.
 *
 * The evaluation function, the guess at the leaves of the search, is
 * a table of what each square is worth ([weights]): the corners can't
 * be taken back, so they're worth the most; the squares next to them
 * give them away, so they're worth the least; the edges are good:
 *
 *      100 -20  10   5   5  10 -20 100
 *      -20 -50  -2  -2  -2  -2 -50 -20
 *       10  -2  -1  -1  -1  -1  -2  10
 *        5  -2  -1  -1  -1  -1  -2   5
 *        ...     (the same, upside down)
 *
 * Having many disks early is not the point, a beginner's mistake: they
 * are what the other player flips. At the end, the count is all that
 * matters.
 *
 * The game: Reversi (London, 1880s, Lewis Waterman and John Mollett
 * both claiming it), which Goro Hasegawa made into Othello in Japan,
 * 1971, with its fixed start. The computers got good early: Paul
 * Rosenbloom's IAGO (1982), then Michael Buro's Logistello beat the
 * world champion, Takeshi Murakami, 6 games to 0 in 1997, and in 2023
 * Hiroki Takizawa's computation showed that perfect play is a draw.
 * (Names and dates from memory, to check.)
 *
 * What it uses: ai/'s Minimax (minimax and alpha-beta), Scene2d (the
 * keys pressed). Not the puzzle kit: no pushing, a disk flips lines.
 *
 * Exercises: mobility in the evaluation (how many moves each player
 * has); the endgame searched to the very end (with 10 empty squares
 * left, it's quick); better move ordering, the corners tried first, and
 * count the cuts; a time limit instead of a depth (iterative deepening:
 * 1, 2, 3... moves ahead until time's up); two computers playing each
 * other, with different evaluations.
 *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The rules *)
(*****************************************************************************)

type disk = Empty | Black | White

(* the 64 squares, row by row from the top, and who's to play *)
type position = { board : disk array; turn : disk }
type move = Put of int | Pass

let other = function Black -> White | White -> Black | Empty -> Empty

let start : position =
  let b = Array.make 64 Empty in
  b.(27) <- White;
  b.(28) <- Black;
  b.(35) <- Black;
  b.(36) <- White;
  { board = b; turn = Black }

let directions = [ (-1, -1); (-1, 0); (-1, 1); (0, -1); (0, 1); (1, -1); (1, 0); (1, 1) ]

(* the disks [player] would flip by playing on square [i]: in each
 * direction, the other's disks up to one of its own *)
let flips (board : disk array) (player : disk) (i : int) : int list =
  if board.(i) <> Empty then []
  else
    List.concat_map
      (fun (dr, dc) ->
        let rec walk r c acc =
          if r < 0 || r > 7 || c < 0 || c > 7 then []
          else
            let d = board.((r *.. 8) +.. c) in
            if d = other player then walk (r +.. dr) (c +.. dc) (((r *.. 8) +.. c) :: acc)
            else if d = player then acc
            else []
        in
        walk ((i /.. 8) +.. dr) ((i mod 8) +.. dc) [])
      directions

let legal (p : position) : int list = List.filter (fun i -> flips p.board p.turn i <> []) (List.init 64 Fun.id)

(* no move: pass, unless the other can't move either: the end *)
let moves (p : position) : move list =
  match legal p with
  | [] -> if legal { p with turn = other p.turn } <> [] then [ Pass ] else []
  | squares -> List.map (fun i -> Put i) squares

let play (p : position) (m : move) : position =
  match m with
  | Pass -> { p with turn = other p.turn }
  | Put i ->
      let b = Array.copy p.board in
      List.iter (fun j -> b.(j) <- p.turn) (i :: flips p.board p.turn i);
      { board = b; turn = other p.turn }

let count (p : position) (d : disk) : int = Array.fold_left (fun n d' -> if d' = d then n +.. 1 else n) 0 p.board

(*****************************************************************************)
(* The computer *)
(*****************************************************************************)

let weights =
  let half = [ [ 100; -20; 10; 5 ]; [ -20; -50; -2; -2 ]; [ 10; -2; -1; -1 ]; [ 5; -2; -1; -1 ] ] in
  let row r = r @ List.rev r in
  Array.of_list (List.concat_map row (half @ List.rev half))

(* for white, the computer, MAX: its squares' worth minus black's; at
 * the end, only the disks count (and more than any table) *)
let score (p : position) : number =
  if moves p = [] then 1000. * float_of_int (count p White -.. count p Black)
  else
    float_of_int
      (Array.fold_left ( +.. ) 0 (Array.mapi (fun i d -> if d = White then weights.(i) else if d = Black then 0 -.. weights.(i) else 0) p.board))

let othello : (position, move) Minimax.game = { moves; play; score; max_to_play = (fun p -> p.turn = White) }

let depth = 4

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type game = {
  position : position;
  cursor : int;
  last : int option; (* the last disk placed *)
  wait : int; (* frames before the computer (or a pass) plays *)
  nodes : (int * int) option; (* the computer's last search: alpha-beta's, minimax's *)
  show_values : bool;
  values : (move * number) list option; (* your moves, as the computer sees them *)
}

type model = game Scene2d.t

let new_game () : game = { position = start; cursor = 19; last = None; wait = 0; nodes = None; show_values = false; values = None }
let initial_model : model = Scene2d.start (new_game ())

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let square_size = 100.

let square_at (x : number) (y : number) : int option =
  let c = int_of_float (Float.floor ((x + 400.) / square_size)) and r = int_of_float (Float.floor ((400. - y) / square_size)) in
  if r >= 0 && r < 8 && c >= 0 && c < 8 then Some ((r *.. 8) +.. c) else None

let after (g : game) (m : move) : game =
  let last = match m with Put i -> Some i | Pass -> g.last in
  { g with position = play g.position m; last; wait = 40; values = None }

let update_game (computer : computer) (scenes : model) (g : game) : game =
  let pressed f = Scene2d.pressed f scenes in
  let m = computer.mouse in
  let g = if pressed (fun k -> Set_.mem "v" k.keys) then { g with show_values = not g.show_values } else g in
  let g = { g with wait = max 0 (g.wait -.. 1) } in
  match (moves g.position, g.position.turn) with
  | [], _ -> g
  | [ Pass ], _ when g.wait = 0 -> after g Pass
  | _, White when g.wait = 0 ->
      (* the computer thinks: alpha-beta for its move, minimax only to
       * count what it would have visited *)
      let a = Minimax.alphabeta othello ~depth g.position in
      let mm = Minimax.minimax othello ~depth g.position in
      let g = { g with nodes = Some (a.nodes, mm.nodes) } in
      (match a.best with Some mv -> after g mv | None -> g)
  | _, Black ->
      (* the cursor: the mouse when it moves, the arrows *)
      let r, c = (g.cursor /.. 8, g.cursor mod 8) in
      let step key d = if pressed key then d else 0 in
      let r = clamp 0 7 (r +.. step (fun k -> k.kdown) 1 +.. step (fun k -> k.kup) (-1)) in
      let c = clamp 0 7 (c +.. step (fun k -> k.kright) 1 +.. step (fun k -> k.kleft) (-1)) in
      let cursor = (r *.. 8) +.. c in
      let cursor = if m.mdx <> 0. || m.mdy <> 0. then Option.value (square_at m.mx m.my) ~default:cursor else cursor in
      let g = { g with cursor } in
      let g = if g.show_values && g.values = None then { g with values = Some (Minimax.minimax othello ~depth:(depth -.. 1) g.position).children } else g in
      if (pressed (fun k -> k.kspace) || m.mclick) && List.mem (Put g.cursor) (moves g.position) then after g (Put g.cursor) else g
  | _ -> g

let update (computer : computer) (s : model) : model =
  let s = Scene2d.update computer s in
  let g = s.scene in
  if moves g.position = [] && Scene2d.pressed (fun k -> k.kspace) s then Scene2d.go (new_game ()) s
  else { s with scene = update_game computer s g }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

let square_center (i : int) : number * number =
  (-350. + (square_size * float_of_int (i mod 8)), 350. - (square_size * float_of_int (i /.. 8)))

let view_disk (d : disk) : shape =
  match d with
  | Black -> circle (rgb 20 20 20) 42.
  | White -> group [ circle (rgb 20 20 20) 42.; circle (rgb 245 245 245) 40. ]
  | Empty -> group []

let view (computer : computer) (s : model) : shape list =
  let g = s.scene and screen = computer.screen in
  let p = g.position in
  let at i shape = let x, y = square_center i in move x y shape in
  let over = moves p = [] in
  let yours = if p.turn = Black && not over && not g.show_values then legal p else [] in
  let status =
    if over then
      let b = count p Black and w = count p White in
      if b > w then "YOU WIN! (space: again)" else if w > b then "THE COMPUTER WINS (space: again)" else "A DRAW (space: again)"
    else if moves p = [ Pass ] then if p.turn = Black then "you can't move: you pass" else "the computer can't move: it passes"
    else if p.turn = Black then "your move" else "the computer thinks..."
  in
  [ rectangle (rgb 30 60 40) screen.width screen.height; rectangle (rgb 40 130 70) 800. 800. ]
  @ List.concat (List.init 9 (fun k -> let o = -400. + (square_size * float_of_int k) in [ rectangle black 800. 2. |> move_y o; rectangle black 2. 800. |> move_x o ]))
  @ (if p.turn = Black && not over then [ at g.cursor (rectangle yellow 96. 96. |> fade 0.3) ] else [])
  @ List.map (fun i -> at i (view_disk p.board.(i))) (List.init 64 Fun.id)
  @ (match g.last with Some i -> [ at i (circle (rgb 230 60 60) 6.) ] | None -> [])
  @ List.map (fun i -> at i (circle (rgb 30 80 45) 8.)) yours
  (* the computer's view of your moves: the minimax values, the lowest
   * (your best) in yellow *)
  @ (match (g.show_values, g.values) with
    | true, Some values when p.turn = Black ->
        let lowest = List.fold_left (fun m (_, v) -> Float.min m v) Float.infinity values in
        List.filter_map
          (fun (mv, v) ->
            match mv with
            | Put i -> Some (at i (text (if v = lowest then yellow else white) 3. (Printf.sprintf "%.0f" v)))
            | Pass -> None)
          values
    | _ -> [])
  @ [ text white 3. (Printf.sprintf "you (black) %d - %d computer (white)" (count p Black) (count p White)) |> move_y 450.;
      text white 2.5 status |> move_y (-440.) ]
  @ (match g.nodes with
    | Some (a, mm) -> [ text (rgb 200 220 200) 2. (Printf.sprintf "%d moves ahead: alpha-beta looked at %d positions, minimax would have looked at %d" depth a mm) |> move_y (-475.) ]
    | None -> [ text (rgb 200 220 200) 2. "v: the computer's view of your moves" |> move_y (-475.) ])

let app = game view update initial_model
let main = Playground_platform.run_app app
