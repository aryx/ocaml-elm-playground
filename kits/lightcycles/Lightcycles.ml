(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
open Playground

(* See Lightcycles.mli *)

(*****************************************************************************)
(* The arena *)
(*****************************************************************************)

let size = 90 (* cells, each side *)
let cell = 10. (* pixels *)

let empty_arena : Tilemap.t =
  Tilemap.of_strings cell
    (List.init size (fun r ->
         if r = 0 || r = size - 1 then String.make size '#' else "#" ^ String.make (size - 2) ' ' ^ "#"))

type dir = Up | Down | Left | Right

let delta (d : dir) : int * int = match d with Up -> (0, -1) | Down -> (0, 1) | Left -> (-1, 0) | Right -> (1, 0)
let opposite (d : dir) : dir = match d with Up -> Down | Down -> Up | Left -> Right | Right -> Left

type cycle = {
  col : int;
  row : int;
  dir : dir;
  wanted : dir; (* the last arrow pressed: a turn happens at the next step *)
  mark : char; (* its trail's character, '1' or '2' *)
  corners : (int * int) list; (* where it turned, the last first, and where it started *)
}

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type round = {
  arena : Tilemap.t;
  p1 : cycle;
  p2 : cycle;
  over : (int * int) option; (* the round's points (0 or 1 each), and... *)
  pause : int; (* ...the frames to wait before the next round *)
  frames : int;
}

type game = { round : round; score1 : int; score2 : int; computer : bool }

type scene = Title | Playing of game | Winner of game
type model = scene Scene2d.t

(* a cycle moves one cell every [step] frames *)
let step = 2
let rounds_to_win = 3

let new_round () : round =
  let p1 = { col = size / 4; row = size / 2; dir = Right; wanted = Right; mark = '1'; corners = [ (size / 4, size / 2) ] } in
  let p2 = { col = 3 * size / 4; row = size / 2; dir = Left; wanted = Left; mark = '2'; corners = [ (3 * size / 4, size / 2) ] } in
  let arena = Tilemap.set (Tilemap.set empty_arena p1.col p1.row p1.mark) p2.col p2.row p2.mark in
  { arena; p1; p2; over = None; pause = 0; frames = 0 }

let new_game (computer : bool) : game = { round = new_round (); score1 = 0; score2 = 0; computer }
let initial_model : model = Scene2d.start Title

(*****************************************************************************)
(* The computer *)
(*****************************************************************************)

let free (arena : Tilemap.t) ((c, r) : int * int) : bool = Tilemap.get arena c r = Some ' '

(* How much room from (c, r): the free cells reachable from it, counted
 * by a flood fill (a breadth-first search), up to [limit] (enough to
 * tell a dead end from open space, and cheaper) *)
let room (arena : Tilemap.t) (start : int * int) (limit : int) : int =
  let seen = Hashtbl.create 256 in
  let queue = Queue.create () in
  if free arena start then begin Hashtbl.replace seen start (); Queue.push start queue end;
  while (not (Queue.is_empty queue)) && Hashtbl.length seen < limit do
    let c, r = Queue.pop queue in
    List.iter
      (fun d ->
        let dc, dr = delta d in
        let n = (c + dc, r + dr) in
        if free arena n && not (Hashtbl.mem seen n) then begin Hashtbl.replace seen n (); Queue.push n queue end)
      [ Up; Down; Left; Right ]
  done;
  Hashtbl.length seen

(* the way with the most room, straight on when it's as good *)
let computer_turn (arena : Tilemap.t) (me : cycle) : dir =
  let ways = me.dir :: List.filter (fun d -> d <> me.dir && d <> opposite me.dir) [ Up; Down; Left; Right ] in
  let score d =
    let dc, dr = delta d in
    room arena (me.col + dc, me.row + dr) 600
  in
  List.fold_left (fun best d -> if score d > score best then d else best) me.dir ways

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let player1_wants (k : keyboard) (current : dir) : dir =
  if k.kup then Up else if k.kdown then Down else if k.kleft then Left else if k.kright then Right else current

let player2_wants (k : keyboard) (current : dir) : dir =
  if k.kw then Up else if k.ks then Down else if k.ka then Left else if k.kd then Right else current

(* a turn, unless it's straight back into its own trail; a turn is a
 * corner of the trail *)
let turn (c : cycle) : cycle =
  if c.wanted = opposite c.dir || c.wanted = c.dir then c
  else { c with dir = c.wanted; corners = (c.col, c.row) :: c.corners }

let advance (c : cycle) : cycle =
  let dc, dr = delta c.dir in
  { c with col = c.col + dc; row = c.row + dr }

(* One step: both cycles move at once; a cycle entering a cell that
 * isn't free crashes, and both entering the same cell crash head-on *)
let step_round (r : round) : round =
  let p1 = advance (turn r.p1) and p2 = advance (turn r.p2) in
  let crash1 = (not (free r.arena (p1.col, p1.row))) || (p1.col, p1.row) = (p2.col, p2.row) in
  let crash2 = (not (free r.arena (p2.col, p2.row))) || (p1.col, p1.row) = (p2.col, p2.row) in
  if crash1 || crash2 then
    { r with over = Some ((if crash2 && not crash1 then 1 else 0), if crash1 && not crash2 then 1 else 0); pause = 90 }
  else
    let arena = Tilemap.set (Tilemap.set r.arena p1.col p1.row p1.mark) p2.col p2.row p2.mark in
    { r with arena; p1; p2 }

let update_game (k : keyboard) (g : game) : game =
  let r = g.round in
  let r = { r with frames = r.frames + 1 } in
  match r.over with
  | Some _ when r.pause > 0 -> { g with round = { r with pause = r.pause - 1 } }
  | Some _ -> { g with round = new_round () }
  | None ->
      let p1 = { r.p1 with wanted = player1_wants k r.p1.wanted } in
      let p2 =
        if g.computer then { r.p2 with wanted = (if r.frames mod step = 0 then computer_turn r.arena r.p2 else r.p2.wanted) }
        else { r.p2 with wanted = player2_wants k r.p2.wanted }
      in
      let r = { r with p1; p2 } in
      let r = if r.frames mod step = 0 then step_round r else r in
      (match r.over with
      | Some (a, b) -> { g with round = r; score1 = g.score1 + a; score2 = g.score2 + b }
      | None -> { g with round = r })

let update (computer : computer) (s : model) : model =
  let s = Scene2d.update computer s in
  let key name = Scene2d.pressed (fun k -> Set_.mem name k.keys) s in
  match s.scene with
  | Title ->
      if key "1" then Scene2d.go (Playing (new_game true)) s
      else if key "2" then Scene2d.go (Playing (new_game false)) s
      else s
  | Playing g ->
      let g = update_game computer.keyboard g in
      if (g.score1 >= rounds_to_win || g.score2 >= rounds_to_win) && g.round.pause = 0 then Scene2d.go (Winner g) s
      else { s with scene = Playing g }
  | Winner _ -> if Scene2d.pressed (fun k -> k.kspace) s then Scene2d.go Title s else s
