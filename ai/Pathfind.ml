(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

type 'node problem = { neighbors : 'node -> ('node * float) list; goal : 'node -> bool; estimate : 'node -> float }
type 'node result = { path : 'node list; cost : float; visited : 'node list }

let manhattan ((x1, y1) : int * int) ((x2, y2) : int * int) : float = float_of_int (abs (x1 - x2) + abs (y1 - y2))

(* the frontier, kept in order of priority; an equal priority goes last,
 * so nodes of the same cost come out oldest first (a queue) *)
let insert (node : 'node) (priority : float) (frontier : ('node * float) list) : ('node * float) list =
  let rec go = function
    | (n, p) :: rest when p <= priority -> (n, p) :: go rest
    | rest -> (node, priority) :: rest
  in
  go frontier

(* the path, read backwards from the goal through [came_from] *)
let rebuild (came_from : ('node, 'node) Hashtbl.t) (goal : 'node) : 'node list =
  let rec go node acc = match Hashtbl.find_opt came_from node with Some from -> go from (node :: acc) | None -> node :: acc in
  go goal []

(* what a path really costs, whatever the search counted *)
let path_cost (problem : 'node problem) (path : 'node list) : float =
  let rec go = function
    | a :: (b :: _ as rest) -> (match List.assoc_opt b (problem.neighbors a) with Some c -> c | None -> 0.) +. go rest
    | _ -> 0.
  in
  go path

(* the one search behind the three: [unit_steps] counts every step as 1
 * (breadth-first), [guided] adds
 * the estimate of what's left, as A* does *)
let search ~(unit_steps : bool) ~(guided : bool) (problem : 'node problem) (start : 'node) : 'node result =
  let came_from : ('node, 'node) Hashtbl.t = Hashtbl.create 97 in
  (* a node reached again more cheaply is put in the frontier a second
   * time, so skip the ones already taken out *)
  let done_with : ('node, unit) Hashtbl.t = Hashtbl.create 97 in
  let best : ('node, float) Hashtbl.t = Hashtbl.create 97 in
  Hashtbl.replace best start 0.;
  let rec loop frontier visited =
    match frontier with
    | [] -> { path = []; cost = 0.; visited = List.rev visited }
    | (node, _) :: rest when Hashtbl.mem done_with node -> loop rest visited
    | (node, _) :: rest ->
        Hashtbl.replace done_with node ();
        let visited = node :: visited in
        if problem.goal node then
          let path = rebuild came_from node in
          { path; cost = path_cost problem path; visited = List.rev visited }
        else
          let g = Hashtbl.find best node in
          let frontier =
            List.fold_left
              (fun frontier (next, step) ->
                let g' = g +. if unit_steps then 1. else step in
                match Hashtbl.find_opt best next with
                | Some old when old <= g' -> frontier
                | _ ->
                    Hashtbl.replace best next g';
                    Hashtbl.replace came_from next node;
                    insert next (g' +. if guided then problem.estimate next else 0.) frontier)
              rest (problem.neighbors node)
          in
          loop frontier visited
  in
  loop [ (start, 0.) ] []

let breadth_first (problem : 'node problem) (start : 'node) : 'node result = search ~unit_steps:true ~guided:false problem start
let dijkstra (problem : 'node problem) (start : 'node) : 'node result = search ~unit_steps:false ~guided:false problem start
let astar (problem : 'node problem) (start : 'node) : 'node result = search ~unit_steps:false ~guided:true problem start
