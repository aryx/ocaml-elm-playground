(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* Connect 4 against the computer, which is the game that needs all of
 * ai/Deepening at once (notes_ai.md section 9). You are yellow: click a
 * column, or move with the arrows and drop with space. Four in a row,
 * any direction, wins. "v" shows what the computer thinks of each
 * column (the lower, the better for you); space after the end plays
 * again.
 *
 * Between Othello and chess in size: 7 columns, so a tree of 7^d, and
 * the same position over and over by different orders of the same
 * drops -- which is exactly where the three tricks show up, and after
 * each of its moves the computer says what they saved:
 *
 *   alpha-beta alone, the columns left to right   the plain search
 *   + the middle columns first                    ordering
 *   + 1, 2, ... up to the depth                   iterative deepening
 *   + what it learned about a position kept       the table (Zobrist)
 *
 * The middle columns first is the game's own hint (ai/Deepening's
 * [order]): a piece in the middle is in more fours than one at the
 * edge -- 13 of them against 3 -- so middle moves are likelier to be
 * good, and a good move tried first is what makes alpha-beta cut.
 *
 * The evaluation, at the leaves: a four is a win; otherwise every line
 * of four squares with pieces of one colour only is worth 1, 10 or 100
 * for one, two or three of them, plus the middle column, and the other
 * player's are subtracted.
 *
 * The game: Milton Bradley's Connect Four (1974), though the game is
 * older (Captain's Mistress). Solved twice over in 1988, by James Dow
 * Allen and by Victor Allis (whose thesis is the readable one): the
 * first player wins, by starting in the middle column, and any other
 * first move draws or loses. This computer is far from that -- it
 * searches 7 moves ahead, where solving needs 42 -- but it plays the
 * middle first, which is the one thing everybody knows.
 *
 * What it uses: ai/'s Minimax (the game's rules as a [game] record),
 * Deepening (the search) and Zobrist (the keys and the table), Scene2d
 * (the keys pressed). Not the puzzle kit: nothing is pushed.
 *
 * Exercises: the endgame searched to the end (with a dozen squares
 * left it is quick); killer moves (a move that cut elsewhere, tried
 * early); thinking while you think, a frame at a time
 * (Deepening.think), instead of all at once when it is its turn.
 *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The rules *)
(*****************************************************************************)

let columns = 7
let rows = 6

type piece = Empty | You | Machine

(* the board, column by column, bottom first; [turn] is whose it is *)
type position = { board : piece array; turn : piece }

let start : position = { board = Array.make (columns *.. rows) Empty; turn = You }
let at (b : piece array) (c : int) (r : int) : piece = if c < 0 || c >= columns || r < 0 || r >= rows then Empty else b.((c *.. rows) +.. r)

(* the row a piece dropped in [c] would land on, if any *)
let landing (b : piece array) (c : int) : int option =
  List.find_opt (fun r -> at b c r = Empty) (List.init rows Fun.id)

let moves (p : position) : int list = List.filter (fun c -> landing p.board c <> None) (List.init columns Fun.id)

let play (p : position) (c : int) : position =
  let board = Array.copy p.board in
  (match landing board c with Some r -> board.((c *.. rows) +.. r) <- p.turn | None -> ());
  { board; turn = (if p.turn = You then Machine else You) }

(* every line of four squares on the board: 69 of them *)
let lines : (int * int) list list =
  let line (c, r) (dc, dr) = List.init 4 (fun i -> (c +.. (i *.. dc), r +.. (i *.. dr))) in
  List.concat_map
    (fun c ->
      List.concat_map
        (fun r ->
          List.filter_map
            (fun (dc, dr) ->
              let l = line (c, r) (dc, dr) in
              if List.for_all (fun (c, r) -> c >= 0 && c < columns && r >= 0 && r < rows) l then Some l else None)
            [ (1, 0); (0, 1); (1, 1); (1, -1) ])
        (List.init rows Fun.id))
    (List.init columns Fun.id)

let four (b : piece array) (who : piece) : bool =
  List.exists (fun l -> List.for_all (fun (c, r) -> at b c r = who) l) lines

let over (p : position) : bool = four p.board You || four p.board Machine || moves p = []

(*****************************************************************************)
(* What a position is worth *)
(*****************************************************************************)

(* a line with pieces of one colour only is worth more the more of them
 * there are; a win is worth more than any number of lines *)
let win = 100000.

let score (p : position) : number =
  if four p.board Machine then win
  else if four p.board You then -.win
  else
    let line_value l =
      let mine = List.length (List.filter (fun (c, r) -> at p.board c r = Machine) l)
      and yours = List.length (List.filter (fun (c, r) -> at p.board c r = You) l) in
      let worth n = match n with 1 -> 1. | 2 -> 10. | 3 -> 100. | _ -> 0. in
      if yours = 0 then worth mine else if mine = 0 then -.(worth yours) else 0.
    in
    let middle =
      List.init rows Fun.id
      |> List.fold_left (fun n r -> match at p.board 3 r with Machine -> n + 3. | You -> n - 3. | Empty -> n) 0.
    in
    List.fold_left (fun n l -> n + line_value l) middle lines

let connect4 : (position, int) Minimax.game =
  { moves = (fun p -> if over p then [] else moves p); play; score; max_to_play = (fun p -> p.turn = Machine) }

(*****************************************************************************)
(* The computer *)
(*****************************************************************************)

let depth = 7

(* the game's own hint: the middle columns first (see the top) *)
let middle_first (_ : position) (moves : int list) : int list =
  List.sort (fun a b -> compare (Float.abs (float_of_int a - 3.)) (Float.abs (float_of_int b - 3.))) moves

(* the keys: one number per (piece, square), and one for the turn *)
let zobrist = Zobrist.make ~pieces:2 ~squares:(columns *.. rows) ~seed:4

let key (p : position) : int64 =
  let pieces =
    List.filter_map
      (fun i -> match p.board.(i) with Empty -> None | You -> Some (0, i) | Machine -> Some (1, i))
      (List.init (columns *.. rows) Fun.id)
  in
  Int64.logxor (Zobrist.of_board zobrist pieces) (if p.turn = Machine then Zobrist.side zobrist else 0L)

(* the same rules, with the moves listed middle first: ordering is
 * nothing more than that, and this is how it is measured against the
 * plain search (Unit_games has the whole table) *)
let ordered_rules : (position, int) Minimax.game =
  { connect4 with moves = (fun p -> middle_first p (connect4.moves p)) }

(* what it played, what it thinks of your columns, and the two node
 * counts the game shows: alpha-beta as it comes, and the search with
 * every trick on *)
type counts = { plain : int; tricks : int }

let think (p : position) : int option * number list * counts =
  let plain = Minimax.alphabeta connect4 ~depth p in
  let table = Zobrist.table () in
  let tabled = Deepening.search ~order:middle_first ~key ~table connect4 ~depth p in
  (* what it thinks of each of your columns, for the "v" key: two plies
   * shallower, from your side, sharing the table it just filled *)
  let values =
    List.init columns (fun c ->
        if landing p.board c = None then Float.nan
        else (Deepening.search ~order:middle_first ~key ~table connect4 ~depth:(depth -.. 2) (play p c)).value)
  in
  (tabled.best, values, { plain = plain.nodes; tricks = tabled.nodes })

(*****************************************************************************)
(* The game *)
(*****************************************************************************)

type game = {
  position : position;
  cursor : int;
  last : int option; (* the column it dropped in *)
  wait : int; (* frames before it answers *)
  counts : counts option;
  values : number list option;
  show_values : bool;
}

type model = game Scene2d.t

let new_game () : game =
  { position = start; cursor = 3; last = None; wait = 0; counts = None; values = None; show_values = false }

let initial_model : model = Scene2d.start (new_game ())

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let cell = 100.
let left = -.(float_of_int columns * cell / 2.)
let bottom = -.(float_of_int rows * cell / 2.) - 30.
let column_at (x : number) : int option =
  let c = int_of_float (Float.floor ((x - left) / cell)) in
  if c >= 0 && c < columns then Some c else None

let drop (g : game) (c : int) : game =
  if landing g.position.board c = None || over g.position then g
  else { g with position = play g.position c; last = Some c; wait = 20; counts = None; values = None }

let update_game (computer : computer) (scenes : model) (g : game) : game =
  let m = computer.mouse in
  if over g.position then if Scene2d.pressed (fun k -> k.kspace) scenes then new_game () else g
  else if g.position.turn = Machine then
    if g.wait > 0 then { g with wait = g.wait -.. 1 }
    else
      let (best, values, counts) = think g.position in
      let g = { g with counts = Some counts; values = Some values } in
      (match best with Some c -> { (drop g c) with counts = Some counts; values = Some values } | None -> g)
  else
    let g = if Scene2d.pressed (fun k -> Set_.mem "v" k.keys) scenes then { g with show_values = not g.show_values } else g in
    let cursor =
      if Scene2d.pressed (fun k -> k.kleft) scenes then max 0 (g.cursor -.. 1)
      else if Scene2d.pressed (fun k -> k.kright) scenes then min (columns -.. 1) (g.cursor +.. 1)
      else g.cursor
    in
    let g = { g with cursor } in
    if Scene2d.pressed (fun k -> k.kspace) scenes then drop g g.cursor
    else if m.mclick then (match column_at m.mx with Some c -> drop { g with cursor = c } c | None -> g)
    else g

let update (computer : computer) (s : model) : model =
  let scenes = Scene2d.update computer s in
  { scenes with scene = update_game computer scenes scenes.scene }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

let center (c : int) (r : int) : number * number =
  (left + (float_of_int c * cell) + (cell / 2.), bottom + (float_of_int r * cell) + (cell / 2.))

let view_piece (p : piece) : shape =
  match p with
  | Empty -> circle (rgb 25 30 60) 40.
  | You -> circle (rgb 240 200 70) 40.
  | Machine -> circle (rgb 220 80 70) 40.

let view (computer : computer) (s : model) : shape list =
  let g = s.scene and screen = computer.screen in
  let board =
    List.concat_map
      (fun c ->
        List.map
          (fun r ->
            let (x, y) = center c r in
            view_piece (at g.position.board c r) |> move x y)
          (List.init rows Fun.id))
      (List.init columns Fun.id)
  in
  let hint =
    if not g.show_values then []
    else
      match g.values with
      | None -> []
      | Some values ->
          List.filteri (fun _ _ -> true) values
          |> List.mapi (fun c v ->
                 if Float.is_nan v then group []
                 else
                   let (x, _) = center c 0 in
                   text (if v < 0. then rgb 120 230 140 else rgb 230 130 120) 1.6 (Printf.sprintf "%.0f" v)
                   |> move x (bottom - 30.))
  in
  let told =
    match g.counts with
    | None -> []
    | Some c ->
        [ text (rgb 170 170 190) 1.6 (Printf.sprintf "alpha-beta, the columns in order: %d positions" c.plain)
          |> move_y (-350.);
          text (rgb 170 170 190) 1.6
            (Printf.sprintf "middle first, deepening 1 to %d, with the table: %d  (%.0f%% of it)" depth c.tricks
               (100. * float_of_int c.tricks / Float.max 1. (float_of_int c.plain)))
          |> move_y (-380.) ]
  in
  let over_text =
    if four g.position.board Machine then [ text (rgb 220 80 70) 3. "RED WINS" |> move_y 380. ]
    else if four g.position.board You then [ text (rgb 240 200 70) 3. "YOU WIN" |> move_y 380. ]
    else if over g.position then [ text white 3. "A DRAW" |> move_y 380. ]
    else []
  in
  [ rectangle (rgb 18 22 40) screen.width screen.height;
    rectangle (rgb 40 70 160) (float_of_int columns * cell) (float_of_int rows * cell) |> move_y (bottom + (float_of_int rows * cell / 2.)) ]
  @ board
  @ (if g.position.turn = You && not (over g.position) then
       [ view_piece You |> move (fst (center g.cursor rows)) (bottom + (float_of_int rows * cell) + 40.) |> fade 0.6 ]
     else [])
  @ hint @ told @ over_text
  @ [ text white 2.5 "CONNECT 4" |> move_y 440.;
      text (rgb 150 150 170) 1.6 "click a column, or the arrows and space;  v: what it thinks of yours" |> move_y (-450.) ]

let app = game view update initial_model
let main = Playground_platform.run_app app
