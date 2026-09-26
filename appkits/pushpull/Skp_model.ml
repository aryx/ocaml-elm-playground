(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Skp_model.mli *)

type v3 = Vec3.t
type edge = { a : int; b : int; soft : bool; curve : bool }
type face = { id : int; outer : int list; holes : int list list }
type t = { verts : (int * v3) list; edges : edge list; faces : face list; next : int }

let empty = { verts = []; edges = []; faces = []; next = 0 }
let eps = 1e-3

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let pos t v = List.assoc v t.verts
let face t id = List.find_opt (fun f -> f.id = id) t.faces
let loops f = f.outer :: f.holes

let sides l =
  match l with
  | [] -> []
  | first :: _ ->
      let rec go = function [ x ] -> [ (x, first) ] | x :: (y :: _ as rest) -> (x, y) :: go rest | [] -> [] in
      go l

let same (a, b) (c, d) = (a = c && b = d) || (a = d && b = c)
let face_sides f = List.concat_map sides (loops f)
let uses f (a, b) = List.exists (same (a, b)) (face_sides f)
let faces_on t a b = List.filter (fun f -> uses f (a, b)) t.faces
let edge_of t a b = List.find_opt (fun e -> same (e.a, e.b) (a, b)) t.edges
let normal t f = Vec3.face_normal (List.map (pos t) f.outer)
let near p q = Vec3.length (Vec3.sub p q) < eps
let collinear p q r = Vec3.length (Vec3.cross (Vec3.sub q p) (Vec3.sub r p)) < eps *. Float.max 1. (Vec3.length (Vec3.sub r p))

(* the loop turned to start at x *)
let rotate_to x l =
  let rec go before = function [] -> l | y :: rest as all -> if y = x then all @ List.rev before else go (y :: before) rest in
  go [] l

(* a point of a plane seen along the normal's largest coordinate, which
   keeps the polygon's shape (only squeezed) *)
let flat (nx, ny, nz) (x, y, z) =
  let ax = Float.abs nx and ay = Float.abs ny and az = Float.abs nz in
  if az >= ax && az >= ay then (x, y) else if ax >= ay then (y, z) else (x, z)

(* even-odd: a ray to the right crosses the outline an odd number of
   times *)
let in_outline (px, py) pts =
  List.fold_left
    (fun inside ((x1, y1), (x2, y2)) ->
      let crosses = y1 > py <> (y2 > py) && px < x1 +. ((py -. y1) *. (x2 -. x1) /. (y2 -. y1)) in
      if crosses then not inside else inside)
    false (sides pts)

let in_loop t n l p = in_outline (flat n p) (List.map (fun v -> flat n (pos t v)) l)

let inside t f p =
  let n = normal t f in
  in_loop t n f.outer p && not (List.exists (fun h -> in_loop t n h p) f.holes)

let on_plane t f p = Float.abs (Vec3.dot (normal t f) (Vec3.sub p (pos t (List.hd f.outer)))) < eps

let hit t origin dir =
  List.fold_left
    (fun best f ->
      let n = normal t f in
      let denom = Vec3.dot dir n in
      if Float.abs denom < 1e-12 then best
      else
        let s = Vec3.dot (Vec3.sub (pos t (List.hd f.outer)) origin) n /. denom in
        if s <= 1e-9 || not (inside t f (Vec3.add origin (Vec3.scale s dir))) then best
        else match best with Some (b, _) when b <= s -> best | _ -> Some (s, f))
    None t.faces

let add_vertex t p =
  let id = t.next in
  ({ t with verts = (id, p) :: t.verts; next = id + 1 }, id)

let add_face t outer holes = { t with faces = { id = t.next; outer; holes } :: t.faces; next = t.next + 1 }

(*****************************************************************************)
(* Sticky geometry: vertices shared *)
(*****************************************************************************)

(* the point strictly between a and b, if it is on their segment *)
let on_segment a b p =
  let ab = Vec3.sub b a in
  let l2 = Vec3.dot ab ab in
  l2 > 0.
  &&
  let s = Vec3.dot (Vec3.sub p a) ab /. l2 in
  s > 0. && s < 1. && near p (Vec3.add a (Vec3.scale s ab))

(* v put between a and b wherever the loop goes from one to the other *)
let insert_between a b v l = List.concat_map (fun (x, y) -> if same (x, y) (a, b) then [ x; v ] else [ x ]) (sides l)

let split_edge t e v =
  let edges = { e with b = v } :: { e with a = v } :: List.filter (fun x -> not (same (x.a, x.b) (e.a, e.b))) t.edges in
  let faces = List.map (fun f -> { f with outer = insert_between e.a e.b v f.outer; holes = List.map (insert_between e.a e.b v) f.holes }) t.faces in
  { t with edges; faces }

let vertex_at t p =
  match List.find_opt (fun (_, q) -> near p q) t.verts with
  | Some (v, _) -> (t, v)
  | None -> (
      let t, v = add_vertex t p in
      match List.find_opt (fun e -> on_segment (pos t e.a) (pos t e.b) p) t.edges with
      | Some e -> (split_edge t e v, v)
      | None -> (t, v))

(* a vertex between exactly two edges in a straight line is no corner:
   gone, its two edges one *)
let heal t v =
  match List.filter (fun e -> e.a = v || e.b = v) t.edges with
  | [ e1; e2 ] ->
      let other e = if e.a = v then e.b else e.a in
      let p = other e1 and q = other e2 in
      if p = q || not (collinear (pos t p) (pos t v) (pos t q)) || e1.soft <> e2.soft then t
      else
        let edges = { a = p; b = q; soft = e1.soft; curve = e1.curve && e2.curve } :: List.filter (fun e -> e <> e1 && e <> e2) t.edges in
        let drop l = List.filter (( <> ) v) l in
        let faces = List.map (fun f -> { f with outer = drop f.outer; holes = List.map drop f.holes }) t.faces in
        { t with edges; faces; verts = List.remove_assoc v t.verts }
  | _ -> t

(*****************************************************************************)
(* Edges close faces *)
(*****************************************************************************)

(* a loop cut by the chord from a to b: from a to b, and from b to a
   (each closed by the chord); both turn the way the loop did *)
let split_at a b l =
  let rec upto acc = function [] -> (List.rev acc, []) | y :: rest -> if y = b then (List.rev (y :: acc), rest) else upto (y :: acc) rest in
  let first, rest = upto [] (rotate_to a l) in
  (first, (b :: rest) @ [ a ])

(* the face the new edge a-b crosses, from boundary to boundary, cut in
   two, its holes going to the half they are in *)
let split_face t a b =
  let mid = Vec3.scale 0.5 (Vec3.add (pos t a) (pos t b)) in
  let candidate f = List.mem a f.outer && List.mem b f.outer && on_plane t f mid && inside t f mid in
  match List.find_opt candidate t.faces with
  | None -> None
  | Some f ->
      let l1, l2 = split_at a b f.outer in
      if List.length l1 < 3 || List.length l2 < 3 then None
      else
        let n = normal t f in
        let h1, h2 = List.partition (fun h -> in_loop t n l1 (pos t (List.hd h))) f.holes in
        let t = { t with faces = { f with outer = l1; holes = h1 } :: List.filter (fun g -> g.id <> f.id) t.faces } in
        Some (add_face t l2 h2)

let neighbours t v = List.filter_map (fun e -> if e.a = v then Some e.b else if e.b = v then Some e.a else None) t.edges

(* the shortest path from src to dst through the vertices [ok], not by
   the edge a-b, breadth first; the path from src to dst *)
let shortest t ~ok ~avoid:(a, b) src dst =
  let rec bfs visited = function
    | [] -> None
    | (v, path) :: queue ->
        if v = dst then Some (List.rev path)
        else
          let next = List.filter (fun w -> ok w && (not (List.mem w visited)) && not (same (v, w) (a, b))) (neighbours t v) in
          bfs (next @ visited) (queue @ List.map (fun w -> (w, w :: path)) next)
  in
  bfs [ src ] [ (src, [ src ]) ]

(* which way a new face turns: against its neighbour across a shared
   edge, so that the two agree on which side is out; alone, down on the
   ground (SketchUp's rule), up above it *)
let orient t loop =
  let neighbour = List.find_map (fun (x, y) -> match faces_on t x y with g :: _ -> Some (List.mem (x, y) (face_sides g)) | [] -> None) (sides loop) in
  match neighbour with
  | Some same_way -> if same_way then List.rev loop else loop
  | None ->
      let (_, _, nz) = Vec3.face_normal (List.map (pos t) loop) in
      let (_, _, z) = pos t (List.hd loop) in
      let on_ground = Float.abs z < eps in
      if Float.abs nz < 1. -. 1e-6 then loop else if on_ground = (nz > 0.) then List.rev loop else loop

let is_face t loop = List.exists (fun f -> f.holes = [] && List.sort compare f.outer = List.sort compare loop) t.faces

(* the loop the new edge a-b closes: for each plane through a, b and a
   neighbour, the shortest way back from b to a in it *)
let close_loop t a b =
  let pa = pos t a and pb = pos t b in
  let planes =
    List.filter_map
      (fun w ->
        let n = Vec3.cross (Vec3.sub pb pa) (Vec3.sub (pos t w) pa) in
        if Vec3.length n < eps *. eps then None else Some (Vec3.normalize n))
      (List.filter (fun w -> w <> a && w <> b) (neighbours t a @ neighbours t b))
  in
  let loops =
    List.filter_map
      (fun n ->
        let ok w = Float.abs (Vec3.dot n (Vec3.sub (pos t w) pa)) < eps in
        match shortest t ~ok ~avoid:(a, b) b a with
        | Some path when List.length path >= 3 && not (is_face t path) -> Some path
        | _ -> None)
      planes
  in
  match List.sort (fun l1 l2 -> compare (List.length l1) (List.length l2)) loops with
  | loop :: _ -> add_face t (orient t loop) []
  | [] -> t

let add_edge ?(curve = false) t p q =
  let t, a = vertex_at t p in
  let t, b = vertex_at t q in
  if a = b || edge_of t a b <> None then t
  else
    let t = { t with edges = { a; b; soft = false; curve } :: t.edges } in
    match split_face t a b with Some t -> t | None -> close_loop t a b

let add_polygon ?(curve = false) t pts =
  let n = Vec3.face_normal pts in
  let clear_of f p = List.for_all (fun (x, y) -> Vec3.length (Vec3.sub p (pos t x)) > eps && not (on_segment (pos t x) (pos t y) p)) (face_sides f) in
  let host f =
    Float.abs (Vec3.dot (normal t f) n) > 1. -. 1e-6
    && List.for_all (fun p -> on_plane t f p && inside t f p && clear_of f p) pts
    && List.for_all (fun v -> not (in_outline (flat n (pos t v)) (List.map (flat n) pts))) (List.concat (loops f))
  in
  match List.find_opt host t.faces with
  | Some f ->
      (* a face inside a face: the window, and the hole it makes *)
      let t, ids = List.fold_left (fun (t, ids) p -> let t, v = add_vertex t p in (t, ids @ [ v ])) (t, []) pts in
      let loop = if Vec3.dot n (normal t f) > 0. then ids else List.rev ids in
      let t = { t with edges = List.map (fun (a, b) -> { a; b; soft = false; curve }) (sides loop) @ t.edges } in
      let t = { t with faces = List.map (fun g -> if g.id = f.id then { g with holes = List.rev loop :: g.holes } else g) t.faces } in
      add_face t loop []
  | None -> (
      match pts with
      | [] -> t
      | first :: _ ->
          let rec go t = function a :: (b :: _ as rest) -> go (add_edge ~curve t a b) rest | [ last ] -> add_edge ~curve t last first | [] -> t in
          go t pts)

(*****************************************************************************)
(* Push/pull *)
(*****************************************************************************)

let move t vs delta = { t with verts = List.map (fun (v, p) -> if List.mem v vs then (v, Vec3.add p delta) else (v, p)) t.verts }

(* two faces sharing the side x-y of their outer loops made one, with
   the holes of both, if they are in one plane and turned the same way *)
let merge t x y =
  let outer f = List.exists (same (x, y)) (sides f.outer) in
  match faces_on t x y with
  | [ f; g ] when outer f && outer g && Vec3.dot (normal t f) (normal t g) > 1. -. 1e-6 ->
      (* f goes from x to y, g from y to x: f from y round to x, then g
         from x round to y, less its ends *)
      let f, g = if List.mem (x, y) (sides f.outer) then (f, g) else (g, f) in
      let g' = rotate_to x g.outer in
      let middle = List.filteri (fun i _ -> i > 0 && i < List.length g' - 1) g' in
      let outer = rotate_to y f.outer @ middle in
      let t = { t with faces = { f with outer; holes = f.holes @ g.holes } :: List.filter (fun h -> h.id <> f.id && h.id <> g.id) t.faces } in
      let t = { t with edges = List.filter (fun e -> not (same (e.a, e.b) (x, y))) t.edges } in
      heal (heal t x) y
  | _ -> t

(* a side s = [b'; a'; a; b] lying in its neighbour g's plane but
   turned the other way (a face pushed in, next to a wall) is not a
   face but a notch cut out of g: g, which went from b to a, goes round
   it, by b' and a' *)
let notch t s g a b =
  match s.outer with
  | [ b'; a'; _; _ ] ->
      let around l = List.concat_map (fun (x, y) -> if x = b && y = a then [ x; b'; a' ] else [ x ]) (sides l) in
      let g = { g with outer = around g.outer; holes = List.map around g.holes } in
      let t = { t with faces = g :: List.filter (fun h -> h.id <> s.id && h.id <> g.id) t.faces } in
      let t = { t with edges = List.filter (fun e -> not (same (e.a, e.b) (a, b))) t.edges } in
      heal (heal t a) b
  | _ -> t

let extrude t f n d =
  let others (x, y) = List.filter (fun g -> g.id <> f.id) (faces_on t x y) in
  (* closed off: every side shared, the face was a wall of a solid *)
  let closed = List.for_all (fun s -> others s <> []) (face_sides f) in
  (* the copy faces as f did if f goes, else away from what it leaves *)
  let cap_same = closed || d > 0. in
  let vs = List.sort_uniq compare (List.concat (loops f)) in
  let t, copies = List.fold_left (fun (t, m) v -> let t, v' = add_vertex t (Vec3.add (pos t v) (Vec3.scale d n)) in (t, (v, v') :: m)) (t, []) vs in
  let copy v = List.assoc v copies in
  let orig v' = fst (List.find (fun (_, c) -> c = v') copies) in
  let turn l = if cap_same then List.map copy l else List.rev (List.map copy l) in
  let cap = { id = t.next; outer = turn f.outer; holes = List.map turn f.holes } in
  let t = { t with next = t.next + 1 } in
  let curve x y = match edge_of t x y with Some e -> e.curve | None -> false in
  (* the sides rising from a circle are its seams, not drawn *)
  let soft v = List.length (List.filter (fun (x, y) -> (x = v || y = v) && curve x y) (face_sides f)) >= 2 in
  let cap_edges = List.map (fun (a', b') -> { a = a'; b = b'; soft = false; curve = curve (orig a') (orig b') }) (face_sides cap) in
  let rising = List.map (fun v -> { a = v; b = copy v; soft = soft v; curve = false }) vs in
  (* a side turns against the cap along their shared edge *)
  let t, side_faces =
    List.fold_left
      (fun (t, acc) (a', b') -> ({ t with next = t.next + 1 }, { id = t.next; outer = [ b'; a'; orig a'; orig b' ]; holes = [] } :: acc))
      (t, []) (face_sides cap)
  in
  let rest = List.filter (fun g -> g.id <> f.id) t.faces in
  let old = if closed then [] else if cap_same then [ { f with outer = List.rev f.outer; holes = List.map List.rev f.holes } ] else [ f ] in
  let t = { t with edges = cap_edges @ rising @ t.edges; faces = (cap :: side_faces) @ old @ rest } in
  if not closed then t
  else
    (* each side against a face of the solid, in its plane: the same
       way round, one face with it; the other way, a notch in it *)
    List.fold_left
      (fun t (s : face) ->
        match s.outer with
        | [ _; _; a; b ] -> (
            match (face t s.id, List.filter (fun g -> g.id <> s.id) (faces_on t a b)) with
            | Some s, [ g ] ->
                let c = Vec3.dot (normal t s) (normal t g) in
                if c > 1. -. 1e-6 then merge t a b else if c < -1. +. 1e-6 then notch t s g a b else t
            | _ -> t)
        | _ -> t)
      t side_faces

let push_pull t id d =
  match face t id with
  | None -> t
  | Some _ when Float.abs d < eps -> t
  | Some f ->
      let n = normal t f in
      let others (x, y) = List.filter (fun g -> g.id <> f.id) (faces_on t x y) in
      let slides = f.holes = [] && List.for_all (fun s -> match others s with [ g ] -> Float.abs (Vec3.dot (normal t g) n) < 1e-6 | _ -> false) (sides f.outer) in
      if slides then move t f.outer (Vec3.scale d n) else extrude t f n d

(*****************************************************************************)
(* Erasing *)
(*****************************************************************************)

let erase_edge t a b =
  let t = { t with edges = List.filter (fun e -> not (same (e.a, e.b) (a, b))) t.edges; faces = List.filter (fun f -> not (uses f (a, b))) t.faces } in
  let alone v = not (List.exists (fun e -> e.a = v || e.b = v) t.edges) in
  let t = { t with verts = List.filter (fun (v, _) -> not (alone v)) t.verts } in
  let t = if List.mem_assoc a t.verts then heal t a else t in
  if List.mem_assoc b t.verts then heal t b else t

let erase_face t id = { t with faces = List.filter (fun f -> f.id <> id) t.faces }

let vertices_of ~edges ~faces t =
  List.sort_uniq compare
    (List.concat_map (fun (a, b) -> [ a; b ]) edges @ List.concat_map (fun id -> match face t id with Some f -> List.concat (loops f) | None -> []) faces)
