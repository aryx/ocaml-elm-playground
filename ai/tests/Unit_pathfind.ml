(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* ai/Pathfind: the .mli's worked example, and A* agreeing with Dijkstra *)

let t = Testo.create

(* a grid [w] by [h], the cells of [walls] closed, those of [mud]
 * costing 5 to step on *)
let grid ?(walls = []) ?(mud = []) ~w ~h (goal : int * int) : (int * int) Pathfind.problem =
  {
    neighbors =
      (fun (x, y) ->
        List.filter_map
          (fun (dx, dy) ->
            let ((px, py) as p) = (x + dx, y + dy) in
            if px < 0 || px >= w || py < 0 || py >= h || List.mem p walls then None
            else Some (p, if List.mem p mud then 5. else 1.))
          [ (1, 0); (-1, 0); (0, 1); (0, -1) ]);
    goal = (fun p -> p = goal);
    estimate = (fun p -> Pathfind.manhattan p goal);
  }

let steps (r : (int * int) Pathfind.result) : int = List.length r.path - 1
let looked_at (r : (int * int) Pathfind.result) : int = List.length r.visited

(* the .mli's numbers: an empty 13x9 grid, then a patch of mud between
 * the start and the goal *)
let test_worked_example () =
  let start = (1, 4) and goal = (11, 4) in
  let p = grid ~w:13 ~h:9 goal in
  let bfs = Pathfind.breadth_first p start and dij = Pathfind.dijkstra p start and a = Pathfind.astar p start in
  List.iter (fun (name, r) -> Alcotest.(check int) (name ^ ": 10 steps") 10 (steps r)) [ ("bfs", bfs); ("dijkstra", dij); ("astar", a) ];
  Alcotest.(check int) "breadth-first looks at 80 cells" 80 (looked_at bfs);
  Alcotest.(check int) "Dijkstra too" 80 (looked_at dij);
  Alcotest.(check int) "A* at 11" 11 (looked_at a);
  (* the mud: the shortest way is no longer the cheapest *)
  let mud = List.concat_map (fun x -> List.init 5 (fun k -> (x, k + 2))) [ 6; 7 ] in
  let p = grid ~mud ~w:13 ~h:9 goal in
  let bfs = Pathfind.breadth_first p start and dij = Pathfind.dijkstra p start and a = Pathfind.astar p start in
  Alcotest.(check int) "breadth-first: the same 10 steps" 10 (steps bfs);
  Alcotest.(check (float 0.)) "wading through the mud costs 18" 18. bfs.cost;
  Alcotest.(check int) "Dijkstra: 16 steps around" 16 (steps dij);
  Alcotest.(check (float 0.)) "but a cost of 16" 16. dij.cost;
  Alcotest.(check (list (pair int int))) "A*: the same way" dij.path a.path;
  Alcotest.(check int) "Dijkstra looks at 115 cells" 115 (looked_at dij);
  Alcotest.(check int) "A* at 75" 75 (looked_at a)

(* a wall with one gap: the way through it, or none at all when it's
 * closed *)
let test_wall () =
  let walls = List.init 9 (fun y -> (5, y)) in
  let open_wall = List.filter (fun c -> c <> (5, 8)) walls in
  let start = (1, 4) and goal = (9, 4) in
  let r = Pathfind.astar (grid ~walls:open_wall ~w:11 ~h:9 goal) start in
  Alcotest.(check int) "through the gap at the bottom" 16 (steps r);
  Alcotest.(check bool) "it goes through it" true (List.mem (5, 8) r.path);
  let r = Pathfind.astar (grid ~walls ~w:11 ~h:9 goal) start in
  Alcotest.(check (list (pair int int))) "walled in: no way" [] r.path;
  Alcotest.(check int) "having looked at the whole left side" 45 (looked_at r)

(* random grids of walls and mud: A* costs what Dijkstra costs (its
 * estimate never overshoots), looking at no more cells *)
let test_astar_agrees () =
  let seed = ref 7 in
  let rand n =
    seed := ((!seed * 1103515245) + 12345) land 0x7fffffff;
    !seed / 65536 mod n
  in
  let cells = List.concat_map (fun x -> List.init 12 (fun y -> (x, y))) (List.init 12 Fun.id) in
  for _ = 1 to 100 do
    let walls = List.filter (fun _ -> rand 4 = 0) cells and mud = List.filter (fun _ -> rand 3 = 0) cells in
    let goal = (11, 11) in
    let p = grid ~walls:(List.filter (fun c -> c <> (0, 0) && c <> goal) walls) ~mud ~w:12 ~h:12 goal in
    let dij = Pathfind.dijkstra p (0, 0) and a = Pathfind.astar p (0, 0) in
    Alcotest.(check (float 0.)) "the same cost" dij.cost a.cost;
    Alcotest.(check bool) "found by both, or by neither" true (dij.path = [] = (a.path = []));
    Alcotest.(check bool) "A* looks at no more cells" true (looked_at a <= looked_at dij)
  done

(* one search for a whole crowd: the field knows the cost from the goal
 * to everywhere, and following it downhill walks there from anywhere *)
let test_field () =
  let goal = (11, 4) in
  let walls = List.init 9 (fun y -> (5, y)) |> List.filter (fun c -> c <> (5, 8)) in
  let p = grid ~walls ~w:13 ~h:9 goal in
  let f = Pathfind.field p goal in
  Alcotest.(check int) "every cell but the walls" ((13 * 9) - List.length walls) (List.length f);
  Alcotest.(check (float 0.)) "nothing to do at the goal" 0. (List.assoc goal f);
  Alcotest.(check (float 0.)) "next door" 1. (List.assoc (10, 4) f);
  (* on the other side of the wall, around through the gap at (5, 8) *)
  Alcotest.(check (float 0.)) "around the wall" 15. (List.assoc (4, 4) f);
  (* three units, each following the field from where it stands *)
  List.iter
    (fun start ->
      let at = ref start and steps = ref 0 in
      while !at <> goal && !steps < 100 do
        (match Pathfind.downhill p f !at with Some next -> at := next | None -> Alcotest.fail "the field stops");
        incr steps
      done;
      Alcotest.(check (pair int int)) "it got there" goal !at;
      Alcotest.(check (float 0.)) "in as many steps as it cost" (List.assoc start f) (float_of_int !steps))
    [ (0, 0); (4, 4); (12, 8) ]

let tests =
  Testo.categorize "Pathfind"
    [ t "the worked example, and the mud" test_worked_example; t "a wall with a gap, and without" test_wall; t "A* agrees with Dijkstra" test_astar_agrees; t "a flow field for a crowd" test_field ]
