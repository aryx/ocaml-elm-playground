(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* See Connect4.mli *)
open Basics (* float arithmetics *)

type number = float

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
(* What the search is given *)
(*****************************************************************************)

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

(*****************************************************************************)
(* As a network reads it *)
(*****************************************************************************)

(* 42 numbers for the pieces of whoever is to play, 42 for the
 * other's: the same position seen from either side reads the same *)
let encode (p : position) : float array =
  let other = if p.turn = You then Machine else You in
  let squares = columns *.. rows in
  Array.init (2 *.. squares) (fun i ->
      if i < squares then if p.board.(i) = p.turn then 1. else 0. else if p.board.(i -.. squares) = other then 1. else 0.)

let board : (position, int) Alphazero.board =
  { game = connect4; start; inputs = 2 *.. columns *.. rows; moves = columns; encode; index = (fun c -> c) }

(*****************************************************************************)
(* A player that searches *)
(*****************************************************************************)

let alphabeta ~(depth : int) : (position, int) Arena.player =
 fun ~seed:_ p ->
  match (Deepening.search ~order:middle_first ~key ~table:(Zobrist.table ()) connect4 ~depth p).best with
  | Some c -> c
  | None -> List.hd (connect4.moves p)
