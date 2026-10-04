(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* See Arena.mli *)

type ('state, 'move) player = seed:int -> 'state -> 'move
type score = { won : int; drawn : int; lost : int }

let game (g : ('state, 'move) Minimax.game) (start : 'state) ~(max : ('state, 'move) player)
    ~(min : ('state, 'move) player) ~(seed : int) : float =
  let rec go (state : 'state) (n : int) : 'state =
    match g.moves state with
    | [] -> state
    | _ ->
        let player = if g.max_to_play state then max else min in
        go (g.play state (player ~seed:((seed * 1000) + n) state)) (n + 1)
  in
  let s = g.score (go start 0) in
  if s > 0. then 1. else if s < 0. then 0. else 0.5

let play (g : ('state, 'move) Minimax.game) (start : 'state) ~(a : ('state, 'move) player)
    ~(b : ('state, 'move) player) ~(games : int) : score =
  let rec go (n : int) (s : score) : score =
    if n = games then s
    else
      (* a plays first in the even games, second in the odd ones *)
      let first = n mod 2 = 0 in
      let share = if first then game g start ~max:a ~min:b ~seed:n else 1. -. game g start ~max:b ~min:a ~seed:n in
      go (n + 1)
        (if share > 0.5 then { s with won = s.won + 1 }
         else if share < 0.5 then { s with lost = s.lost + 1 }
         else { s with drawn = s.drawn + 1 })
  in
  go 0 { won = 0; drawn = 0; lost = 0 }

let random (g : ('state, 'move) Minimax.game) : ('state, 'move) player =
 fun ~seed state ->
  let moves = g.moves state in
  List.nth moves (Lehmer.int (Lehmer.make seed) (List.length moves))
