(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Code_units.mli *)

let centre (r : Treemap.rect) = (r.x +. (r.w /. 2.), r.y +. (r.h /. 2.))

let inside (r : Treemap.rect) u v = u >= r.x && u < r.x +. r.w && v >= r.y && v < r.y +. r.h

let is_dir (p : 'a Treemap.placed) = match p.node with Treemap.Dir _ -> true | File _ -> false

(* a directory's children: one level deeper, their centre inside it *)
let children (placed : 'a Treemap.placed array) (i : int) : int list =
  let p = placed.(i) in
  if not (is_dir p) then []
  else
    List.filter
      (fun j ->
        let q = placed.(j) in
        q.depth = p.depth + 1 && (let u, v = centre q.rect in inside p.rect u v))
      (List.init (Array.length placed) Fun.id)

let parent (placed : 'a Treemap.placed array) (i : int) : int option =
  let p = placed.(i) in
  let u, v = centre p.rect in
  let found = ref None in
  Array.iteri (fun j (q : 'a Treemap.placed) -> if q.depth = p.depth - 1 && is_dir q && inside q.rect u v then found := Some j) placed;
  !found

let child_toward (placed : 'a Treemap.placed array) (i : int) (u : float) (v : float) : int option =
  List.find_opt (fun j -> inside placed.(j).rect u v) (children placed i)

let toward (placed : 'a Treemap.placed array) (i : int) (u : float) (v : float) : int option =
  let limit = placed.(i).depth + 1 in
  (* the deepest unit under the point, no deeper than [limit] *)
  let best = ref None in
  Array.iteri
    (fun j (q : 'a Treemap.placed) ->
      if q.depth <= limit && inside q.rect u v then
        match !best with Some b when placed.(b).depth >= q.depth -> () | _ -> best := Some j)
    placed;
  match !best with Some j when j <> i -> Some j | _ -> None

type side = Left | Right | Up | Down

let sibling (placed : 'a Treemap.placed array) (i : int) (side : side) : int option =
  match parent placed i with
  | None -> None
  | Some par ->
      let x, y = centre placed.(i).rect in
      let score j =
        let x', y' = centre placed.(j).rect in
        let dx = x' -. x and dy = y' -. y in
        (* along the side, and how far across it *)
        let along, across = match side with Left -> (-.dx, dy) | Right -> (dx, dy) | Up -> (-.dy, dx) | Down -> (dy, dx) in
        if along <= 1e-9 then None else Some (along +. (2. *. Float.abs across))
      in
      List.fold_left
        (fun best j ->
          if j = i then best
          else
            match (score j, best) with
            | Some s, Some (_, b) when s >= b -> best
            | Some s, _ -> Some (j, s)
            | None, _ -> best)
        None (children placed par)
      |> Option.map fst

let ancestors (placed : 'a Treemap.placed array) (i : int) : int list =
  let rec go i acc = match parent placed i with Some p -> go p (i :: acc) | None -> i :: acc in
  go i []

let deepest (placed : 'a Treemap.placed array) (ok : 'a Treemap.placed -> bool) : int =
  let best = ref 0 in
  Array.iteri (fun j (q : 'a Treemap.placed) -> if q.depth > placed.(!best).depth && ok q then best := j) placed;
  !best
