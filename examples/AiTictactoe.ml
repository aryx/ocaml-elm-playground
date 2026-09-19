(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* Tic-tac-toe against the computer, which can't lose: the game is
 * small enough to search to the very end (ai/Minimax.mli), so it needs
 * no evaluation function, no guess -- every move is known to win, draw
 * or lose. You are X and play first: click a square, or move with the
 * arrows and press space. Each empty square says what playing there is
 * worth to you, against a computer that plays its best: with perfect
 * play on both sides tic-tac-toe is a draw, so they all say DRAW at the
 * start, and one says LOSS as soon as you can go wrong.
 *
 * Under the board, the positions the computer looked at, and the ones
 * plain minimax would have: alpha-beta's cuts, counted. From the empty
 * board that's the whole game tree, 549,946 positions, against 18,297
 * with alpha-beta, 3% -- Minimax.mli's worked example.
 *
 * Tic-tac-toe is the fruit fly of game AI: it was one of the first
 * video games (A. S. Douglas's OXO, 1952, on Cambridge's EDSAC, for a
 * thesis on human-computer interaction), one of the first learning
 * machines (Donald Michie's MENACE, 1961: 304 matchboxes of colored
 * beads, one box per position, the beads of losing moves removed), and
 * MIT students built a computer out of Tinkertoy sticks that played it
 * (1978). (Names and dates from memory, to check.)
 *
 * What it uses: ai/'s Minimax, Scene2d. Where games/AiOthello has to
 * guess with a table of what each square is worth, here the search
 * reaches the end of the game and the values are exact.
 *
 * Exercises: MENACE's learning instead of the search (a bag of beads
 * per position, thinned when it loses); the computer against itself;
 * misere (the one who lines up three loses); 4x4x4, where the search
 * no longer reaches the end and an evaluation function comes back.
 *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The rules *)
(*****************************************************************************)

type mark = Empty | X | O

(* the 9 squares, row by row from the top, and who's to play; X, you,
 * is MAX: a win for X scores 1, a win for O -1, a draw 0 *)
type position = { board : mark array; turn : mark }

let lines = [ [ 0; 1; 2 ]; [ 3; 4; 5 ]; [ 6; 7; 8 ]; [ 0; 3; 6 ]; [ 1; 4; 7 ]; [ 2; 5; 8 ]; [ 0; 4; 8 ]; [ 2; 4; 6 ] ]

let winning_line (p : position) : int list option =
  List.find_opt (fun l -> match List.map (fun i -> p.board.(i)) l with [ a; b; c ] -> a <> Empty && a = b && b = c | _ -> false) lines

let winner (p : position) : mark = match winning_line p with Some (i :: _) -> p.board.(i) | _ -> Empty

let moves (p : position) : int list =
  if winner p <> Empty then [] else List.filter (fun i -> p.board.(i) = Empty) (List.init 9 Fun.id)

let play (p : position) (i : int) : position =
  let b = Array.copy p.board in
  b.(i) <- p.turn;
  { board = b; turn = (if p.turn = X then O else X) }

let score (p : position) : number = match winner p with X -> 1. | O -> -1. | Empty -> 0.
let tictactoe : (position, int) Minimax.game = { moves; play; score; max_to_play = (fun p -> p.turn = X) }
let start : position = { board = Array.make 9 Empty; turn = X }

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type game = {
  position : position;
  cursor : int;
  wait : int; (* frames before the computer plays *)
  nodes : (int * int) option; (* its last search: alpha-beta's, minimax's *)
  values : (int * number) list; (* your moves, searched to the end *)
}

type model = game Scene2d.t

(* the values of the moves of whoever is to play, exact: [minimax]
 * gives them all (alpha-beta's are bounds for the moves it cut) *)
let think (p : position) : (int * number) list = (Minimax.minimax tictactoe ~depth:9 p).children

let new_game () : game = { position = start; cursor = 4; wait = 0; nodes = None; values = think start }
let initial_model : model = Scene2d.start (new_game ())

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let square_at (x : number) (y : number) : int option =
  let c = int_of_float (Float.floor ((x + 300.) / 200.)) and r = int_of_float (Float.floor ((300. - y) / 200.)) in
  if r >= 0 && r < 3 && c >= 0 && c < 3 then Some ((r *.. 3) +.. c) else None

let after (g : game) (i : int) : game =
  let position = play g.position i in
  { g with position; wait = 30; values = think position }

let update_game (computer : computer) (scenes : model) (g : game) : game =
  let pressed f = Scene2d.pressed f scenes in
  let m = computer.mouse in
  let g = { g with wait = max 0 (g.wait -.. 1) } in
  if moves g.position = [] then g
  else if g.position.turn = O then
    if g.wait > 0 then g
    else
      let a = Minimax.alphabeta tictactoe ~depth:9 g.position in
      let mm = Minimax.minimax tictactoe ~depth:9 g.position in
      let g = { g with nodes = Some (a.nodes, mm.nodes) } in
      (match a.best with Some i -> after g i | None -> g)
  else begin
    (* the cursor: the mouse when it moves, the arrows *)
    let r, c = (g.cursor /.. 3, g.cursor mod 3) in
    let step key d = if pressed key then d else 0 in
    let r = clamp 0 2 (r +.. step (fun k -> k.kdown) 1 +.. step (fun k -> k.kup) (-1)) in
    let c = clamp 0 2 (c +.. step (fun k -> k.kright) 1 +.. step (fun k -> k.kleft) (-1)) in
    let cursor = (r *.. 3) +.. c in
    let cursor = if m.mdx <> 0. || m.mdy <> 0. then Option.value (square_at m.mx m.my) ~default:cursor else cursor in
    let g = { g with cursor } in
    if (pressed (fun k -> k.kspace) || m.mclick) && List.mem g.cursor (moves g.position) then after g g.cursor else g
  end

let update (computer : computer) (s : model) : model =
  let s = Scene2d.update computer s in
  if moves s.scene.position = [] && Scene2d.pressed (fun k -> k.kspace) s then Scene2d.go (new_game ()) s
  else { s with scene = update_game computer s s.scene }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size
let square_center (i : int) : number * number = (-200. + (200. * float_of_int (i mod 3)), 200. - (200. * float_of_int (i /.. 3)))

let view_mark (m : mark) : shape list =
  match m with
  | X -> [ rectangle (rgb 80 160 240) 160. 18. |> rotate 45.; rectangle (rgb 80 160 240) 160. 18. |> rotate (-45.) ]
  | O -> [ circle (rgb 240 120 80) 70.; circle (rgb 30 30 40) 52. ]
  | Empty -> []

let view (computer : computer) (s : model) : shape list =
  let g = s.scene and screen = computer.screen in
  let p = g.position in
  let at i shape = let x, y = square_center i in move x y shape in
  let over = moves p = [] in
  let status =
    if not over then if p.turn = X then "your move" else "the computer plays..."
    else match winner p with X -> "YOU WIN! (space: again)" | O -> "THE COMPUTER WINS (space: again)" | Empty -> "A DRAW (space: again)"
  in
  [ rectangle (rgb 30 30 40) screen.width screen.height ]
  @ List.concat_map (fun o -> [ rectangle (rgb 90 90 110) 600. 8. |> move_y o; rectangle (rgb 90 90 110) 8. 600. |> move_x o ]) [ -100.; 100. ]
  @ (if p.turn = X && not over then [ at g.cursor (rectangle yellow 190. 190. |> fade 0.15) ] else [])
  @ List.concat_map (fun i -> List.map (at i) (view_mark p.board.(i))) (List.init 9 Fun.id)
  (* what each of your moves is worth, searched to the end *)
  @ (if p.turn = X && not over then
       List.filter (fun (i, _) -> p.board.(i) = Empty) g.values
       |> List.map (fun (i, v) ->
           let word, color = if v > 0. then ("WIN", rgb 120 220 120) else if v < 0. then ("LOSS", rgb 230 100 100) else ("DRAW", rgb 180 180 190) in
           at i (text color 2. word |> move_y (-70.)))
     else [])
  @ (match winning_line p with
    | Some (a :: _ as l) ->
        let x0, y0 = square_center a and x1, y1 = square_center (List.nth l 2) in
        [ rectangle (rgb 250 230 90) (Float.hypot (x1 - x0) (y1 - y0) + 100.) 10.
          |> rotate (atan2 (y1 - y0) (x1 - x0) * 180. / pi)
          |> move ((x0 + x1) / 2.) ((y0 + y1) / 2.) ]
    | _ -> [])
  @ [ text white 3. "you (X) against the computer (O)" |> move_y 440.; text white 2.5 status |> move_y (-390.) ]
  @ [ text (rgb 170 170 190) 2.
        (match g.nodes with
        | Some (a, mm) -> Printf.sprintf "searched to the end: %d positions with alpha-beta, %d with minimax" a mm
        | None -> "the computer searches to the end of the game: it can't lose")
      |> move_y (-450.) ]

let app = game view update initial_model
let main = Playground_platform.run_app app
