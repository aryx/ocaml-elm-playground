(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

type ('state, 'move) game = {
  moves : 'state -> 'move list;
  play : 'state -> 'move -> 'state;
  score : 'state -> float;
  max_to_play : 'state -> bool;
}

type 'move result = { value : float; best : 'move option; nodes : int; children : ('move * float) list }

(* the best of the children's values for the player to move: the first
 * one if several are equal *)
let best_of (maximizing : bool) (children : ('move * float) list) : 'move option * float =
  let better a b = if maximizing then a > b else a < b in
  List.fold_left
    (fun (best, v) (m, v') -> if best = None || better v' v then (Some m, v') else (best, v))
    (None, 0.) children

(*****************************************************************************)
(* Minimax *)
(*****************************************************************************)

let minimax (game : ('state, 'move) game) ~(depth : int) (state : 'state) : 'move result =
  let nodes = ref 0 in
  let rec value depth s =
    incr nodes;
    match game.moves s with
    | [] -> game.score s
    | _ when depth = 0 -> game.score s
    | moves ->
        let values = List.map (fun m -> value (depth - 1) (game.play s m)) moves in
        if game.max_to_play s then List.fold_left Float.max Float.neg_infinity values
        else List.fold_left Float.min Float.infinity values
  in
  incr nodes;
  match game.moves state with
  | [] -> { value = game.score state; best = None; nodes = !nodes; children = [] }
  | moves ->
      let children = List.map (fun m -> (m, value (depth - 1) (game.play state m))) moves in
      let best, v = best_of (game.max_to_play state) children in
      { value = v; best; nodes = !nodes; children }

(*****************************************************************************)
(* Alpha-beta *)
(*****************************************************************************)

(* [alpha]: what MAX is sure to get elsewhere, [beta]: what MIN is; a
 * position worth more than beta to MAX (or less than alpha to MIN)
 * won't be allowed by the other: its remaining moves are cut *)
let alphabeta ?leaf (game : ('state, 'move) game) ~(depth : int) (state : 'state) : 'move result =
  let leaf = match leaf with Some f -> f | None -> fun s ~alpha:_ ~beta:_ -> game.score s in
  let nodes = ref 0 in
  let rec value depth s alpha beta =
    incr nodes;
    (* claude: the depth first: a leaf's moves are never looked at, and
     * [score] (or [leaf]) says what an ended game is worth anyway *)
    if depth = 0 then leaf s ~alpha ~beta
    else
    match game.moves s with
    | [] -> game.score s
    | moves ->
        if game.max_to_play s then
          let rec loop v alpha = function
            | [] -> v
            | m :: rest ->
                let v = Float.max v (value (depth - 1) (game.play s m) alpha beta) in
                if v >= beta then v (* the cut: MIN won't come here *) else loop v (Float.max alpha v) rest
          in
          loop Float.neg_infinity alpha moves
        else
          let rec loop v beta = function
            | [] -> v
            | m :: rest ->
                let v = Float.min v (value (depth - 1) (game.play s m) alpha beta) in
                if v <= alpha then v (* the cut: MAX won't come here *) else loop v (Float.min beta v) rest
          in
          loop Float.infinity beta moves
  in
  incr nodes;
  match game.moves state with
  | [] -> { value = game.score state; best = None; nodes = !nodes; children = [] }
  | moves ->
      (* the root is a node like the others, whose children we keep *)
      let maximizing = game.max_to_play state in
      let _, _, children =
        List.fold_left
          (fun (alpha, beta, acc) m ->
            let v = value (depth - 1) (game.play state m) alpha beta in
            if maximizing then (Float.max alpha v, beta, (m, v) :: acc) else (alpha, Float.min beta v, (m, v) :: acc))
          (Float.neg_infinity, Float.infinity, [])
          moves
      in
      let children = List.rev children in
      let best, v = best_of maximizing children in
      { value = v; best; nodes = !nodes; children }
