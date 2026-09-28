(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* The three searches of Pathfind.mli, watched while they work:
 * breadth-first ("b"), Dijkstra ("d") and A* ("a"), from the green
 * square to the red one. The cells each one takes out of its frontier
 * light up in the order it takes them, dark blue first, so the shape of
 * the search is the picture: breadth-first spreads in rings in every
 * direction, A* drives at the goal. The path found is drawn in yellow,
 * and the three counts at the bottom say how many cells each one looked
 * at -- the price of the answer.
 *
 * Draw with the mouse (or the arrows, and space): "w" walls, "m" mud
 * (which costs 5 to cross instead of 1), "e" the eraser, "s" and "g"
 * move the start and the goal, "c" clears, "r" replays.
 *
 * Mud is where breadth-first and Dijkstra part ways: breadth-first
 * counts steps, so it wades through; Dijkstra counts what the steps
 * cost, so it goes around if that's cheaper. A* is Dijkstra with a
 * guess of what's left (the Manhattan distance), which is what makes it
 * lean toward the goal.
 *
 * What it uses: ai/'s Pathfind, Scene2d. The games do this by hand for
 * now (gamekits/maze's Chase follows Pac-Man's rules, TinyZelda's and
 * TinyRogue's monsters walk toward the player and get stuck on walls).
 *
 * Exercises: diagonal steps (and the right heuristic for them: the
 * Chebyshev or octile distance, since Manhattan then overshoots and A*
 * can miss the shortest path); a greedy best-first search (the estimate
 * alone, ignoring the cost so far: fast and often wrong); the estimate
 * multiplied by 1.5, faster still and no longer sure to be right; a
 * maze generated instead of drawn; several goals at once.
 *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The grid *)
(*****************************************************************************)

type cell = Open | Wall | Mud

let cols = 24
let rows = 18
let size = 40.
let mud_cost = 5.

(* the grid is centered, with room for the text above and below *)
let left = -.(float_of_int cols * size / 2.)
let top = 360.

type grid = cell array

let index ((x, y) : int * int) : int = (y *.. cols) +.. x
let inside ((x, y) : int * int) : bool = x >= 0 && x < cols && y >= 0 && y < rows
let get (g : grid) (c : int * int) : cell = g.(index c)

let center ((x, y) : int * int) : number * number =
  (left + (size * (float_of_int x + 0.5)), top - (size * (float_of_int y + 0.5)))

let cell_at (mx : number) (my : number) : (int * int) option =
  let c = (int_of_float (Float.floor ((mx - left) / size)), int_of_float (Float.floor ((top - my) / size))) in
  if inside c then Some c else None

(* the same problem for the three searches: what it costs to step on a
 * cell, and the guess of what's left *)
let problem (g : grid) (goal : int * int) : (int * int) Pathfind.problem =
  {
    neighbors =
      (fun (x, y) ->
        List.filter_map
          (fun (dx, dy) ->
            let c = (x +.. dx, y +.. dy) in
            if not (inside c) then None else match get g c with Wall -> None | Mud -> Some (c, mud_cost) | Open -> Some (c, 1.))
          [ (1, 0); (-1, 0); (0, 1); (0, -1) ]);
    goal = (fun c -> c = goal);
    estimate = (fun c -> Pathfind.manhattan c goal);
  }

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type search = Breadth_first | Dijkstra | Astar

type model = {
  grid : grid;
  start : int * int;
  goal : int * int;
  cursor : int * int;
  brush : cell;
  search : search;
  (* all three, so their costs can be compared; recomputed whenever the
   * grid, the start or the goal change *)
  results : (search * (int * int) Pathfind.result) list;
  shown : int; (* how many of the visited cells are lit up so far *)
}

type state = model Scene2d.t

let searches = [ Breadth_first; Dijkstra; Astar ]
let name = function Breadth_first -> "breadth-first" | Dijkstra -> "Dijkstra" | Astar -> "A*"

let solve (m : model) : model =
  let p = problem m.grid m.goal in
  let run = function
    | Breadth_first -> Pathfind.breadth_first p m.start
    | Dijkstra -> Pathfind.dijkstra p m.start
    | Astar -> Pathfind.astar p m.start
  in
  { m with results = List.map (fun s -> (s, run s)) searches; shown = 0 }

(* a wall with a gap and a patch of mud: something to look at right
 * away, and the two lessons in one picture *)
let new_model () : model =
  let grid = Array.make (cols *.. rows) Open in
  for y = 0 to rows -.. 1 do
    if y <> 12 then grid.(index (14, y)) <- Wall
  done;
  for y = 4 to 13 do
    for x = 6 to 8 do
      grid.(index (x, y)) <- Mud
    done
  done;
  solve { grid; start = (2, 9); goal = (21, 9); cursor = (2, 9); brush = Wall; search = Astar; results = []; shown = 0 }

let initial_model : state = Scene2d.start (new_model ())

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let update_model (computer : computer) (scenes : state) (m : model) : model =
  let pressed key = Scene2d.pressed (fun k -> Set_.mem key k.keys) scenes in
  let mouse = computer.mouse in
  let x, y = m.cursor in
  let step key d = if Scene2d.pressed key scenes then d else 0 in
  let cursor = (clamp 0 (cols -.. 1) (x +.. step (fun k -> k.kright) 1 +.. step (fun k -> k.kleft) (-1)),
                clamp 0 (rows -.. 1) (y +.. step (fun k -> k.kdown) 1 +.. step (fun k -> k.kup) (-1))) in
  let cursor = if mouse.mdx <> 0. || mouse.mdy <> 0. then Option.value (cell_at mouse.mx mouse.my) ~default:cursor else cursor in
  let m = { m with cursor } in
  let m = if pressed "w" then { m with brush = Wall } else m in
  let m = if pressed "m" then { m with brush = Mud } else m in
  let m = if pressed "e" then { m with brush = Open } else m in
  let m = List.fold_left (fun m (key, s) -> if pressed key then { m with search = s; shown = 0 } else m) m
      [ ("b", Breadth_first); ("d", Dijkstra); ("a", Astar) ] in
  let m = if pressed "r" then { m with shown = 0 } else m in
  let m = if pressed "c" then solve { m with grid = Array.make (cols *.. rows) Open } else m in
  (* drawing: the mouse held down, or space; the start and the goal
   * can't be drawn over *)
  let paint (m : model) =
    if m.cursor = m.start || m.cursor = m.goal || get m.grid m.cursor = m.brush then m
    else begin
      let grid = Array.copy m.grid in
      grid.(index m.cursor) <- m.brush;
      solve { m with grid }
    end
  in
  let m = if mouse.mdown || Scene2d.pressed (fun k -> k.kspace) scenes then paint m else m in
  let m = if pressed "s" && m.cursor <> m.goal then solve { m with start = m.cursor } else m in
  let m = if pressed "g" && m.cursor <> m.start then solve { m with goal = m.cursor } else m in
  (* the search replayed, a few cells a frame *)
  { m with shown = m.shown +.. 6 }

let update (computer : computer) (s : state) : state =
  let s = Scene2d.update computer s in
  { s with scene = update_model computer s s.scene }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

let square (color : color) (c : int * int) : shape =
  let x, y = center c in
  rectangle color (size - 2.) (size - 2.) |> move x y

(* the order a cell was looked at, from dark blue to pale blue: the
 * search's own picture of how it spread *)
let heat (i : int) (n : int) : color =
  let t = if n <= 1 then 1. else float_of_int i / float_of_int (n -.. 1) in
  rgb (int_of_float (30. + (60. * t))) (int_of_float (50. + (110. * t))) (int_of_float (110. + (120. * t)))

let view (computer : computer) (s : state) : shape list =
  let m = s.scene and screen = computer.screen in
  let result = List.assoc m.search m.results in
  let visited = List.filteri (fun i _ -> i < m.shown) result.visited in
  let n = List.length visited in
  let done_searching = m.shown >= List.length result.visited in
  [ rectangle (rgb 25 28 35) screen.width screen.height ]
  @ List.concat_map
      (fun i ->
        let c = (i mod cols, i /.. cols) in
        match m.grid.(i) with
        | Wall -> [ square (rgb 90 95 110) c ]
        | Mud -> [ square (rgb 95 75 45) c ]
        | Open -> [ square (rgb 40 45 55) c ])
      (List.init (cols *.. rows) Fun.id)
  (* see-through, so the mud still shows under the search *)
  @ List.mapi (fun i c -> square (heat i n) c |> fade 0.6) visited
  (* the path, once the search has been replayed to its end *)
  @ (if done_searching then
       List.map (fun c -> let x, y = center c in rectangle (rgb 240 210 60) (size - 16.) (size - 16.) |> move x y) result.path
     else [])
  @ [ square (rgb 80 200 110) m.start; square (rgb 220 70 70) m.goal ]
  @ [ (let x, y = center m.cursor in rectangle white (size - 2.) (size - 2.) |> fade 0.25 |> move x y) ]
  @ [ text white 2.5
        (Printf.sprintf "%s: %d cells looked at%s" (name m.search) (List.length result.visited)
           (if result.path = [] then ", no way through" else Printf.sprintf ", a path of %d steps costing %g" (List.length result.path -.. 1) result.cost))
      |> move_y 440. ]
  @ List.mapi
      (fun i search ->
        let r = List.assoc search m.results in
        text (if search = m.search then yellow else rgb 150 155 165) 2.
          (Printf.sprintf "%s %d" (name search) (List.length r.visited))
        |> move (-300. + (300. * float_of_int i)) (-405.))
      searches
  @ [ text (rgb 150 155 165) 2. "b d a: the search   w m e: walls, mud, eraser   s g: start, goal   c: clear   r: replay" |> move_y (-450.) ]

let app = game view update initial_model
let main = Playground_platform.run_app app
