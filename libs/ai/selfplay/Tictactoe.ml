(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* See Tictactoe.mli *)

type mark = Empty | X | O
type position = { cells : mark array; turn : mark }

let start : position = { cells = Array.make 9 Empty; turn = X }

let lines = [ [ 0; 1; 2 ]; [ 3; 4; 5 ]; [ 6; 7; 8 ]; [ 0; 3; 6 ]; [ 1; 4; 7 ]; [ 2; 5; 8 ]; [ 0; 4; 8 ]; [ 2; 4; 6 ] ]

let winner (p : position) : mark =
  let owner line =
    match List.map (fun i -> p.cells.(i)) line with [ a; b; c ] when a <> Empty && a = b && b = c -> a | _ -> Empty
  in
  List.fold_left (fun w line -> if w <> Empty then w else owner line) Empty lines

let moves (p : position) : int list =
  if winner p <> Empty then [] else List.filter (fun i -> p.cells.(i) = Empty) (List.init 9 (fun i -> i))

let play (p : position) (i : int) : position =
  let cells = Array.copy p.cells in
  cells.(i) <- p.turn;
  { cells; turn = (if p.turn = X then O else X) }

let score (p : position) : float = match winner p with X -> 1. | O -> -1. | Empty -> 0.
let game : (position, int) Minimax.game = { moves; play; score; max_to_play = (fun p -> p.turn = X) }

(* nine numbers for the marks of whoever is to play, nine for the
 * other's: the same position seen by X or by O reads the same *)
let encode (p : position) : float array =
  let other = if p.turn = X then O else X in
  Array.init 18 (fun i -> if i < 9 then if p.cells.(i) = p.turn then 1. else 0. else if p.cells.(i - 9) = other then 1. else 0.)

let of_string (s : string) : position =
  let cells = Array.init 9 (fun i -> match s.[i] with 'x' -> X | 'o' -> O | _ -> Empty) in
  let count m = Array.fold_left (fun n c -> if c = m then n + 1 else n) 0 cells in
  { cells; turn = (if count X > count O then O else X) }

let to_string (p : position) : string =
  String.init 9 (fun i -> match p.cells.(i) with X -> 'x' | O -> 'o' | Empty -> '.')
