(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Tron's light cycles (Bally Midway, 1982, after the
 * Disney film of the same year): two cycles race on a grid, each leaving
 * a wall of light behind it, and the first to crash -- into a wall, a
 * trail, its own included -- loses the round. First to 3 rounds wins.
 * Player 1 (blue) steers with the arrows; player 2 (orange) with
 * w/a/s/d, or it's the computer.
 *
 * The idea is older than the film: Blockade (Gremlin, 1976), the first
 * game of the kind, had two players leaving trails on a grid, and
 * Surround (Atari 2600, 1977) brought it home; Snake is its one-player
 * descendant (see games/Snake.ml). And it's a classic first clone: the
 * whole game is a grid, two positions, two directions, and one rule.
 *
 * The arena is a Tilemap of characters (' ' free, '#' the walls, '1'
 * and '2' the trails): a move is Tilemap.set, a crash is Tilemap.get.
 * The trails are drawn with Sprite.pixels on the map's rows: a row's
 * cells of the same color are one rectangle, so a long trail across the
 * arena costs a few shapes, not a hundred.
 *
 * The computer ([computer_turn]) looks at each way it can go, and takes
 * the one leading to the most room: the number of free cells it could
 * still reach from there (a flood fill, [room]), keeping straight on
 * when it's as good. Cutting the other player off from space is then
 * what it does without being told, and what it fears. The same idea
 * won the 2010 Google AI Challenge, whose game was Tron: its best bots
 * searched a few moves ahead (minimax), scoring positions by the cells
 * each player reaches first (a Voronoi partition of the arena) -- an
 * exercise.
 *)
open Playground

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
  let p1 = { col = size / 4; row = size / 2; dir = Right; wanted = Right; mark = '1' } in
  let p2 = { col = 3 * size / 4; row = size / 2; dir = Left; wanted = Left; mark = '2' } in
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

(* a turn, unless it's straight back into its own trail *)
let turn (c : cycle) : cycle = if c.wanted = opposite c.dir then c else { c with dir = c.wanted }

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

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let blue = rgb 60 200 255
let orange = rgb 255 150 40

let text color size str = words color str |> scale size

(* [result]: the round's result, not on the winner's screen *)
let view_round ?(result = true) (g : game) : shape list =
  let r = g.round in
  let half = float_of_int size *. cell /. 2. in
  let grid_lines =
    List.concat
      (List.init 9 (fun i ->
           let x = -.half +. (float_of_int (i + 1) *. cell *. 9.) in
           [ rectangle (rgb 20 30 60) 1. (2. *. half) |> move_x x; rectangle (rgb 20 30 60) (2. *. half) 1. |> move_y x ]))
  in
  let trails =
    Sprite.pixels cell [ ('#', rgb 40 60 120); ('1', blue); ('2', orange) ] (Tilemap.to_strings r.arena)
  in
  let head (c : cycle) color =
    let x, y = Tilemap.center r.arena c.col c.row in
    square color (cell *. 1.6) |> move x y
  in
  let result =
    if not result then []
    else
    match r.over with
    | Some (1, 0) -> [ text blue 5. "BLUE WINS THE ROUND" ]
    | Some (0, 1) -> [ text orange 5. "ORANGE WINS THE ROUND" ]
    | Some _ -> [ text white 5. "BOTH CRASH" ]
    | None -> []
  in
  grid_lines @ [ trails; head r.p1 white; head r.p2 (rgb 255 230 180) ]
  @ [ text blue 3. (Printf.sprintf "BLUE %d" g.score1) |> move (-300.) 475.;
      text orange 3. (Printf.sprintf "%s %d" (if g.computer then "COMPUTER" else "ORANGE") g.score2) |> move 300. 475. ]
  @ result

let view (computer : computer) (s : model) : shape list =
  let screen = computer.screen in
  rectangle black screen.width screen.height
  ::
  (match s.scene with
  | Title ->
      [ text blue 8. "TINY TRON" |> move_y 250.;
        text white 3. "1: against the computer" |> move_y 60.;
        text white 3. "2: two players" |> move_y 0.;
        text gray 2.5 "blue: arrows   orange: w/a/s/d" |> move_y (-80.) ]
      @ Scene2d.blink 1. s [ text orange 3. "PRESS 1 OR 2" |> move_y (-200.) ]
  | Playing g -> view_round g
  | Winner g ->
      view_round ~result:false g
      @ [ (if g.score1 > g.score2 then text blue 7. "BLUE WINS!" else text orange 7. (if g.computer then "THE COMPUTER WINS!" else "ORANGE WINS!"))
          |> move_y 150. ]
      @ Scene2d.blink 1. s [ text white 3. "PRESS SPACE" |> move_y (-150.) ])

let app = game view update initial_model

let main = Playground_platform.run_app app
