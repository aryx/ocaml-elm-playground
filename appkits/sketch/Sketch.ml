(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Sketch.mli *)

(*****************************************************************************)
(* A sheet *)
(*****************************************************************************)

type pos = float * float
type item = Line of int * int | Circle of int * int

type constr =
  | Horizontal of int
  | Vertical of int
  | Parallel of int * int
  | Perpendicular of int * int
  | Equal of int * int
  | On_line of int * int
  | On_circle of int * int

type instance = { master : int; at : pos; size : float; angle : float }

type sheet = {
  points : (int * pos) list;
  fixed : int list;
  items : (int * item) list;
  constraints : constr list;
  instances : instance list;
  next : int;
}

let empty = { points = []; fixed = []; items = []; constraints = []; instances = []; next = 1 }
let add_point p s = ({ s with points = s.points @ [ (s.next, p) ]; next = s.next + 1 }, s.next)
let add_item it s = ({ s with items = s.items @ [ (s.next, it) ]; next = s.next + 1 }, s.next)
let constrain c s = if List.mem c s.constraints then s else { s with constraints = s.constraints @ [ c ] }
let pos s id = match List.assoc_opt id s.points with Some p -> p | None -> (0., 0.)
let set_pos id p s = { s with points = List.map (fun (i, q) -> if i = id then (i, p) else (i, q)) s.points }

let toggle_fixed id s =
  if List.mem id s.fixed then { s with fixed = List.filter (( <> ) id) s.fixed } else { s with fixed = s.fixed @ [ id ] }

let ends s id = match List.assoc_opt id s.items with Some (Line (a, b) | Circle (a, b)) -> Some (a, b) | None -> None
let two s id = match ends s id with Some (a, b) -> [ a; b ] | None -> []

let points_of s = function
  | Horizontal l | Vertical l -> two s l
  | Parallel (l, m) | Perpendicular (l, m) | Equal (l, m) -> two s l @ two s m
  | On_line (p, l) | On_circle (p, l) -> p :: two s l

(* the ids a constraint names, points and items *)
let names = function
  | Horizontal l | Vertical l -> [ l ]
  | Parallel (l, m) | Perpendicular (l, m) | Equal (l, m) | On_line (l, m) | On_circle (l, m) -> [ l; m ]

(* the items gone, and the constraints naming them *)
let without_items gone s =
  {
    s with
    items = List.filter (fun (i, _) -> not (List.mem i gone)) s.items;
    constraints = List.filter (fun c -> not (List.exists (fun n -> List.mem n gone) (names c))) s.constraints;
  }

let merge ~drop ~onto s =
  if drop = onto then s
  else
    let re i = if i = drop then onto else i in
    let items = List.map (fun (i, it) -> (i, match it with Line (a, b) -> Line (re a, re b) | Circle (a, b) -> Circle (re a, re b))) s.items in
    let constraints =
      List.map (function On_line (p, l) -> On_line (re p, l) | On_circle (p, l) -> On_circle (re p, l) | c -> c) s.constraints
    in
    let s =
      {
        s with
        items;
        constraints;
        points = List.remove_assoc drop s.points;
        fixed = List.sort_uniq compare (List.map re s.fixed);
      }
    in
    (* a line or a circle down to one point is nothing *)
    let collapsed = List.filter_map (fun (i, it) -> match it with Line (a, b) | Circle (a, b) when a = b -> Some i | _ -> None) s.items in
    (* and a point on a line it is an end of says nothing *)
    let s = without_items collapsed s in
    { s with constraints = List.filter (function On_line (p, l) | On_circle (p, l) -> not (List.mem p (two s l)) | _ -> true) s.constraints }

let delete id s =
  if List.mem_assoc id s.points then
    let gone = List.filter_map (fun (i, it) -> match it with Line (a, b) | Circle (a, b) when a = id || b = id -> Some i | _ -> None) s.items in
    let s = without_items gone s in
    {
      s with
      points = List.remove_assoc id s.points;
      fixed = List.filter (( <> ) id) s.fixed;
      constraints = List.filter (function On_line (p, _) | On_circle (p, _) -> p <> id | _ -> true) s.constraints;
    }
  else without_items [ id ] s

(*****************************************************************************)
(* Instances, and the strokes *)
(*****************************************************************************)

type t = sheet list

let sheet doc i = match List.nth_opt doc i with Some s -> s | None -> empty
let set_sheet i s doc = List.mapi (fun j s' -> if j = i then s else s') doc

let uses doc a b =
  (* the depth is bounded by the number of sheets, since place
     never lets a cycle in *)
  let rec go depth a = a = b || (depth > 0 && List.exists (fun inst -> go (depth - 1) inst.master) (sheet doc a).instances) in
  go (List.length doc) a

let place i inst doc =
  if uses doc inst.master i then None
  else
    let s = sheet doc i in
    Some (set_sheet i { s with instances = s.instances @ [ inst ] } doc)

type stroke = Seg of pos * pos | Round of pos * float

let transform inst (x, y) =
  let a = inst.angle *. Float.pi /. 180. in
  let c = Float.cos a *. inst.size and s = Float.sin a *. inst.size in
  (fst inst.at +. (c *. x) -. (s *. y), snd inst.at +. (s *. x) +. (c *. y))

let dist (ax, ay) (bx, by) = Float.hypot (bx -. ax) (by -. ay)

let map_stroke inst = function
  | Seg (a, b) -> Seg (transform inst a, transform inst b)
  | Round (c, r) -> Round (transform inst c, r *. Float.abs inst.size)

let rec strokes_depth doc depth i =
  let s = sheet doc i in
  let own =
    List.map
      (fun (_, it) -> match it with Line (a, b) -> Seg (pos s a, pos s b) | Circle (c, r) -> Round (pos s c, dist (pos s c) (pos s r)))
      s.items
  in
  if depth = 0 then own else own @ List.concat_map (instance_depth doc (depth - 1)) s.instances

and instance_depth doc depth inst = List.map (map_stroke inst) (strokes_depth doc depth inst.master)

let instance_strokes doc inst = instance_depth doc (List.length doc) inst
let strokes doc i = strokes_depth doc (List.length doc) i

(*****************************************************************************)
(* Aiming *)
(*****************************************************************************)

type aim = Nothing | At_point of int | On_item of int * pos | At_instance of int

(* the nearest place to p on the segment from a to b *)
let project (px, py) ((ax, ay) as a) (bx, by) =
  let dx = bx -. ax and dy = by -. ay in
  let l2 = (dx *. dx) +. (dy *. dy) in
  if l2 = 0. then a
  else
    let t = Float.max 0. (Float.min 1. ((((px -. ax) *. dx) +. ((py -. ay) *. dy)) /. l2)) in
    (ax +. (t *. dx), ay +. (t *. dy))

let to_segment p a b = dist p (project p a b)

let on_round ((px, py) as p) ((cx, cy) as c) r =
  let d = dist p c in
  if d = 0. then (cx +. r, cy) else (cx +. ((px -. cx) *. r /. d), cy +. ((py -. cy) *. r /. d))

let to_stroke p = function Seg (a, b) -> to_segment p a b | Round (c, r) -> Float.abs (dist p c -. r)

(* the one of [xs] nearest p by [d], if within tolerance *)
let nearest tolerance d xs =
  List.fold_left
    (fun best x ->
      let dx = d x in
      match best with Some (_, db) when db <= dx -> best | _ -> if dx <= tolerance then Some (x, dx) else best)
    None xs
  |> Option.map fst

let aim doc i ~tolerance ?except p =
  let s = sheet doc i in
  let points = List.filter (fun (id, _) -> Some id <> except) s.points in
  match nearest tolerance (fun (_, q) -> dist p q) points with
  | Some (id, _) -> At_point id
  | None -> (
      (* an item standing on the point being dragged is not aimed at *)
      let items = List.filter (fun (id, _) -> match except with Some e -> not (List.mem e (two s id)) | None -> true) s.items in
      let place (_, it) =
        match it with Line (a, b) -> project p (pos s a) (pos s b) | Circle (c, r) -> on_round p (pos s c) (dist (pos s c) (pos s r))
      in
      match nearest tolerance (fun it -> dist p (place it)) items with
      | Some ((id, _) as it) -> On_item (id, place it)
      | None -> (
          let indexed = List.mapi (fun k inst -> (k, inst)) s.instances in
          let d (_, inst) = List.fold_left (fun m st -> Float.min m (to_stroke p st)) infinity (instance_strokes doc inst) in
          match nearest tolerance d indexed with Some (k, _) -> At_instance k | None -> Nothing))
