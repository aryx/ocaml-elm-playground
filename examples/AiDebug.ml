(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* What a computer player is thinking, drawn (Ai_debug.mli).
 * Click a tile to move the flag; the walker takes the way to it, rests
 * when it is tired, and waits when it is there. On the right, a game
 * of Nim: take 1, 2 or 3 sticks with the keys, and whoever takes the
 * last one wins.
 *
 * Four pictures, one per thing an AI does, and each of them is a
 * single call:
 *
 *   the way      the tiles it means to walk, joined  (Ai.way)
 *   the field    where everyone would go from every
 *                tile, one search for the lot        (Ai.flow)
 *   the mind     the modes, the one it is in lit,
 *                and what would take it out          (Ai.deciding)
 *   the opinion  what the opponent makes of each of
 *                its moves                           (Ai.thoughts)
 *
 * The point of the example is that none of this is drawn by the
 * algorithms themselves: they are lists of tiles and numbers, and
 * every one of them is unreadable until somebody draws it. A monster
 * walking into a wall is a bug you can stare at for an hour in the
 * code and see in a second on the screen -- which is why the drawing
 * belongs in the library next to the thinking.
 *
 * Keys: 1, 2, 3 take sticks; f the field, w the way, m the mind, o the
 * opinion (each off and on again); space starts the sticks over.
 *
 * What it uses: Ai (way, flow, deciding, thinking_ahead) and Ai_debug
 * (all four drawings). AiPathfinding.ml is the search itself, step by
 * step; this is what a game does with it. *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The map *)
(*****************************************************************************)

let map_rows =
  [| "............."; ".####.###.##."; ".#...........";
     ".#.###.#.###."; "...#...#...#."; ".###.###.#.#.";
     ".....#...#..."; ".#.#.#.###.#."; ".#.........#." |]

let cols = String.length map_rows.(0)
let rows = Array.length map_rows

(* the rows are written top down, and the world counts up *)
let walkable ((c, r) : int * int) : bool =
  c >= 0 && c < cols && r >= 0 && r < rows && map_rows.(rows -.. 1 -.. r).[c] = '.'

let tile = 34.
let map_left = -460.
let map_bottom = -100.
let at ((c, r) : int * int) : number * number =
  (map_left + (tile * (float_of_int c + 0.5)), map_bottom + (tile * (float_of_int r + 0.5)))

let tile_at ((x, y) : number * number) : int * int =
  (int_of_float (Float.floor ((x - map_left) / tile)), int_of_float (Float.floor ((y - map_bottom) / tile)))

let every_tile : (int * int) list =
  List.concat_map (fun r -> List.init cols (fun c -> (c, r))) (List.init rows Fun.id)

(*****************************************************************************)
(* What the walker is doing *)
(*****************************************************************************)

type mode = Walking | Resting | Waiting

(* the modes, and what takes it from one to another. The first change
 * whose test holds wins, so the order is the priority: arriving beats
 * getting tired. *)
type walker = { where_ : int * int; goal : int * int }

let changes : (mode, walker) Ai.change list =
  [
    Ai.on ~why:"arrived" Walking (fun w -> w.where_ = w.goal) Waiting;
    Ai.after ~why:"tired" 150 Walking Resting;
    Ai.after ~why:"rested" 60 Resting Walking;
    Ai.on ~why:"the flag moved" Waiting (fun w -> w.where_ <> w.goal) Walking;
  ]

(*****************************************************************************)
(* Nim, to have an opponent *)
(*****************************************************************************)

(* sticks on the table; one to three a turn; whoever takes the last one
 * wins. The machine plays perfectly, which in Nim means leaving a
 * multiple of four behind it *)
let nim : (int * bool, int) Ai.rules =
  {
    moves = (fun (n, _) -> List.filter (fun k -> k <= n) [ 1; 2; 3 ]);
    play = (fun (n, mine) k -> (n -.. k, not mine));
    score = (fun (n, mine) -> if n > 0 then 0. else if mine then -1. else 1.);
    my_turn = (fun (_, mine) -> mine);
  }

let opponent = Ai.thinking_ahead 8 nim
let sticks_at_first = 13

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type model = {
  walker : walker;
  mind : mode Ai.mind;
  field : Ai.flow; (* from the goal: rebuilt when the flag moves *)
  step_in : int; (* frames until the next step *)
  sticks : int;
  yours : bool;
  opinion : (int * number) list; (* what the machine makes of its moves *)
  said : string;
  held : string list;
  show : string list; (* which of the four pictures are on *)
}

let start_goal = (10, 0) (* the far corner of the bottom corridor *)
let start_at = (0, 8)

let initial_model : model =
  {
    walker = { where_ = start_at; goal = start_goal };
    mind = Ai.mind Walking;
    field = Ai.flow ~walkable start_goal;
    step_in = 0;
    sticks = sticks_at_first;
    yours = true;
    opinion = [];
    said = "take 1, 2 or 3 sticks";
    held = [];
    show = [ "f"; "w"; "m"; "o" ];
  }

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

(* pressed this frame and not the last: a key is an edge, not a state *)
let pressed (m : model) (computer : computer) (key : string) : bool =
  Set_.mem key computer.keyboard.keys && not (List.mem key m.held)

let steps_a_tile = 10

(* one step along the field, which is the same way [Ai.way] draws but
 * read from the tile the walker stands on: no path is kept *)
let walk (m : model) : model =
  if Ai.doing m.mind <> Walking then m
  else if m.step_in > 0 then { m with step_in = m.step_in -.. 1 }
  else
    match Ai.next_step m.field m.walker.where_ with
    | Some next -> { m with walker = { m.walker with where_ = next }; step_in = steps_a_tile }
    | None -> m

let take (m : model) (k : int) : model =
  if not m.yours || k > m.sticks then m
  else
    let m = { m with sticks = m.sticks -.. k; yours = false } in
    if m.sticks = 0 then { m with said = "you took the last one: you win"; opinion = [] }
    else
      (* it answers at once: eight moves ahead of Nim is nothing *)
      let reply = match Ai.best_move opponent (m.sticks, true) with Some k -> k | None -> 0 in
      let left = m.sticks -.. reply in
      {
        m with
        sticks = left;
        yours = true;
        opinion = Ai.thoughts opponent (left, true);
        said =
          (if left = 0 then "it took the last one: it wins"
           else Printf.sprintf "it took %d, leaving %d" reply left);
      }

let update (computer : computer) (m : model) : model =
  let mouse = computer.mouse in
  let m =
    if mouse.mclick then
      let t = tile_at (mouse.mx, mouse.my) in
      if walkable t then { m with walker = { m.walker with goal = t }; field = Ai.flow ~walkable t } else m
    else m
  in
  let m = List.fold_left (fun m (k, n) -> if pressed m computer k then take m n else m) m [ ("1", 1); ("2", 2); ("3", 3) ] in
  let m = if pressed m computer " " then { initial_model with show = m.show; held = m.held } else m in
  let m =
    List.fold_left
      (fun m k -> if pressed m computer k then { m with show = (if List.mem k m.show then List.filter (( <> ) k) m.show else k :: m.show) } else m)
      m [ "f"; "w"; "m"; "o" ]
  in
  let m = walk m in
  let held = List.filter (fun k -> Set_.mem k computer.keyboard.keys) [ "1"; "2"; "3"; " "; "f"; "w"; "m"; "o" ] in
  { m with mind = Ai.deciding changes m.walker m.mind; held }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

let view (computer : computer) (m : model) : shape list =
  let screen = computer.screen in
  let ground =
    List.map
      (fun t ->
        let (x, y) = at t in
        rectangle (if walkable t then rgb 48 56 76 else rgb 22 24 34) (tile - 2.) (tile - 2.) |> move x y)
      every_tile
  in
  let on k = List.mem k m.show in
  let field = if on "f" then Ai_debug.field ~at m.field (List.filter walkable every_tile) else [] in
  let way = if on "w" then Ai_debug.way ~color:(rgb 240 200 90) ~at (Ai.way ~walkable m.walker.where_ m.walker.goal) else [] in
  let flag =
    let (x, y) = at m.walker.goal in
    [ rectangle (rgb 220 90 80) 4. 22. |> move x (y + 4.); triangle (rgb 220 90 80) 9. |> rotate (-90.) |> move (x + 8.) (y + 12.) ]
  in
  let walker = let (x, y) = at m.walker.where_ in [ circle (rgb 240 240 235) 11. |> move x y ] in
  let mind =
    if not (on "m") then []
    else
      List.map
        (fun s -> s |> move 250. 250.)
        (Ai_debug.machine ~radius:110. ~naming:(function Walking -> "walk" | Resting -> "rest" | Waiting -> "wait")
           changes m.mind)
  in
  let sticks =
    List.init m.sticks (fun i ->
        rectangle (rgb 220 200 160) 5. 46. |> move (110. + (float_of_int i * 22.)) (-120.))
  in
  let opinion =
    if not (on "o") || m.opinion = [] then []
    else
      List.map
        (fun s -> s |> move 300. (-230.))
        (Ai_debug.thoughts ~width:110. ~naming:(fun k -> Printf.sprintf "take %d" k) m.opinion)
  in
  [ rectangle (rgb 18 20 32) screen.width screen.height ]
  @ ground @ field @ way @ flag @ walker @ mind @ sticks @ opinion
  @ [ text white 2.2 "WHAT IT IS THINKING" |> move_y 460.;
      text (rgb 150 155 175) 1.4 "click a tile to move the flag;  1 2 3 take sticks;  f w m o: the four pictures" |> move_y (-450.);
      text (rgb 200 205 220) 1.5 m.said |> move 300. (-170.);
      text (rgb 120 220 160) 1.3
        (match List.length (Ai.way ~walkable m.walker.where_ m.walker.goal) with
        | 0 -> "it is there"
        | n -> Printf.sprintf "%d tiles to the flag" n)
      |> move (-250.) (-160.) ]

let app = game view update initial_model
let main = Playground_platform.run_app app
