(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Map_atlas.mli *)

open Playground
open Code_map_base

(*****************************************************************************)
(* The links, and the parts they join *)
(*****************************************************************************)

(* a map's tree, from its placed nodes: a node's parent (the nearest
 * placed directory above it: fold_singletons merges some) and its depth *)
type tree = { rects : (string, Treemap.rect * int) Hashtbl.t; parent : (string, string) Hashtbl.t }

let rec up (rects : (string, Treemap.rect * int) Hashtbl.t) (path : string) : string option =
  let d = Filename.dirname path in
  if d = path || d = "." || d = "" then None else if Hashtbl.mem rects d then Some d else up rects d

let tree_of (t : t) : tree =
  let rects = Hashtbl.create 1024 and parent = Hashtbl.create 1024 in
  Array.iter (fun (p : entry Treemap.placed) -> if p.depth >= 1 then Hashtbl.replace rects p.path (p.rect, p.depth)) t.placed;
  Hashtbl.iter (fun p _ -> match up rects p with Some d -> Hashtbl.replace parent p d | None -> ()) rects;
  { rects; parent }

(* a node and the ones above it, itself first *)
let rec chain (tr : tree) (p : string) : string list = p :: (match Hashtbl.find_opt tr.parent p with Some d -> chain tr d | None -> [])

(* the part a file is in at depth [d]: its directory of that depth, or
 * the file itself when it is higher up *)
let part_of (tr : tree) (d : int) (file : string) : string =
  match List.find_opt (fun p -> match Hashtbl.find_opt tr.rects p with Some (_, depth) -> depth = d | None -> false) (chain tr file) with
  | Some p -> p
  | None -> file

(* the map's links (Code_rank.links) between the parts at depth [d],
 * added up, and each file's users (how many other files use it), for
 * the heat; kept for the last few maps, by their layout *)
type roads = { tr : tree; by_depth : (int, (string * string * int) list) Hashtbl.t; fan_in : (string, int) Hashtbl.t; max_fan_in : int }

let cache : (entry Treemap.placed array * roads) list ref = ref []

let roads_of (t : t) : roads =
  match List.find_opt (fun (p, _) -> p == t.placed) !cache with
  | Some (_, r) -> r
  | None ->
      let links = Code_rank.links (rank_of t) in
      let fan_in = Hashtbl.create 1024 in
      List.iter (fun (_, b, _) -> Hashtbl.replace fan_in b (1 + Option.value (Hashtbl.find_opt fan_in b) ~default:0)) links;
      let r = { tr = tree_of t; by_depth = Hashtbl.create 4; fan_in; max_fan_in = Hashtbl.fold (fun _ n m -> max n m) fan_in 1 } in
      cache := (t.placed, r) :: List.filteri (fun i _ -> i < 3) !cache;
      r

let roads_at (t : t) (r : roads) (d : int) : (string * string * int) list =
  match Hashtbl.find_opt r.by_depth d with
  | Some l -> l
  | None ->
      let h = Hashtbl.create 256 in
      List.iter
        (fun (a, b, n) ->
          let pa = part_of r.tr d a and pb = part_of r.tr d b in
          if pa <> pb then Hashtbl.replace h (pa, pb) (n + Option.value (Hashtbl.find_opt h (pa, pb)) ~default:0))
        (Code_rank.links (rank_of t));
      let l = Hashtbl.fold (fun (a, b) n acc -> (a, b, n) :: acc) h [] |> List.sort compare in
      Hashtbl.replace r.by_depth d l;
      l

(*****************************************************************************)
(* The heat *)
(*****************************************************************************)

(* claude: from afar, a file's colour is how much the rest of the map
 * leans on it (how many other files use its definitions): dark if none,
 * through yellow, to red for the most used, on a log scale; fading into
 * the street map's own picture as the code's colours come in *)
let heat_colour (h : float) : int * int * int =
  if h < 0.5 then mix (255, 215, 70) (h /. 0.5) (60, 60, 90) else mix (240, 70, 50) ((h -. 0.5) /. 0.5) (255, 215, 70)

let blend (img : Rgba_image.t) (x0 : int) (y0 : int) (x1 : int) (y1 : int) ((r, g, b) : int * int * int) (a : float) : unit =
  let rgba = img.rgba in
  for y = y0 to y1 - 1 do
    for x = x0 to x1 - 1 do
      let i = 4 * ((y * img.width) + x) in
      let put o v = Bigarray.Array1.unsafe_set rgba (i + o) (int_of_float ((a *. float_of_int v) +. ((1. -. a) *. float_of_int (Bigarray.Array1.unsafe_get rgba (i + o))))) in
      put 0 r;
      put 1 g;
      put 2 b
    done
  done

let paint ~(aa : bool) (t : t) (c : camera) : Rgba_image.t =
  let img = Map_streets.paint ~aa t c in
  let r = roads_of t in
  Array.iteri
    (fun i (p : entry Treemap.placed) ->
      match (p.node, t.geometry.(i), clip c p.rect) with
      | File _, Some g, Some (x0, y0, x1, y1) ->
          let lh = g.cell_h *. c.z in
          let a = 0.8 *. (1. -. Map_streets.smooth 1. Map_streets.t_colours lh) in
          if a > 0.01 then begin
            let n = Option.value (Hashtbl.find_opt r.fan_in p.path) ~default:0 in
            let h = Float.log (1. +. float_of_int n) /. Float.log (1. +. float_of_int r.max_fan_in) in
            blend img (x0 + 1) (y0 + 1) (max (x0 + 1) (x1 - 1)) (max (y0 + 1) (y1 - 1)) (heat_colour h) a
          end
      | _ -> ())
    t.placed;
  img

(*****************************************************************************)
(* The roads, bundled (Holten) *)
(*****************************************************************************)

(* claude: Holten, "Hierarchical edge bundles: visualization of adjacency
 * relations in hierarchical data" (IEEE TVCG, 2006), over a squarified
 * treemap as ours: a link from part a to part b is drawn as a B-spline
 * whose control points are the centres of the directories on the tree's
 * path from a up to their lowest common ancestor and down to b (the
 * ancestor itself left out), so links between the same two regions
 * travel together, a bundle:
 *
 *     +-------------+-------------+
 *     |  a1 .       |       . b1  |
 *     |      \  A   |   B  /      |
 *     |  a2 --*=====+=====*-- b2  |    A's and B's centres pull the
 *     |      /      |      \      |    links a_i -> b_j into one road
 *     |  a3 '       |       ' b3  |
 *     +-------------+-------------+
 *
 * straightened by a bundling strength beta (the paper's 0.85): each
 * control point pulled towards the straight line from a to b, by 1 -
 * beta. The direction without arrows (Holten and van Wijk, "A user study
 * on visualizing directed edges in graphs", CHI 2009): a colour gradient,
 * green at the user to red at the used, and a taper, wide at the user;
 * the long links faintest, drawn first, under the short. *)
let beta = 0.85

let centre (tr : tree) (p : string) : float * float =
  match Hashtbl.find_opt tr.rects p with Some (r, _) -> (r.x +. (r.w /. 2.), r.y +. (r.h /. 2.)) | None -> (0., 0.)

let control_points (tr : tree) (a : string) (b : string) : (float * float) array =
  let ca = chain tr a and cb = chain tr b in
  let common = List.find_opt (fun p -> List.mem p cb) ca in
  let below l = match common with Some lca -> List.filter (fun p -> not (List.mem p (chain tr lca))) l | None -> l in
  let pts = Array.of_list (List.map (centre tr) (below ca @ List.rev (below cb))) in
  let n = Array.length pts in
  if n < 3 then pts
  else
    let (x0, y0), (x1, y1) = (pts.(0), pts.(n - 1)) in
    Array.mapi
      (fun i (x, y) ->
        let s = float_of_int i /. float_of_int (n - 1) in
        ((beta *. x) +. ((1. -. beta) *. (x0 +. (s *. (x1 -. x0)))), (beta *. y) +. ((1. -. beta) *. (y0 +. (s *. (y1 -. y0))))))
      pts

(* a uniform cubic B-spline through the control points, its ends
 * clamped (each end point three times): [per] points a span *)
let bspline ?(per = 8) (pts : (float * float) array) : (float * float) list =
  let n = Array.length pts in
  if n < 3 then Array.to_list pts
  else
    let p = Array.concat [ [| pts.(0); pts.(0) |]; pts; [| pts.(n - 1); pts.(n - 1) |] ] in
    let out = ref [ pts.(0) ] in
    for i = 0 to Array.length p - 4 do
      let (x0, y0), (x1, y1), (x2, y2), (x3, y3) = (p.(i), p.(i + 1), p.(i + 2), p.(i + 3)) in
      for k = 1 to per do
        let s = float_of_int k /. float_of_int per in
        let s2 = s *. s and s3 = s *. s *. s in
        let b0 = (1. -. s) ** 3. /. 6. and b1 = ((3. *. s3) -. (6. *. s2) +. 4.) /. 6. and b2 = ((-3. *. s3) +. (3. *. s2) +. (3. *. s) +. 1.) /. 6. and b3 = s3 /. 6. in
        out := ((b0 *. x0) +. (b1 *. x1) +. (b2 *. x2) +. (b3 *. x3), (b0 *. y0) +. (b1 *. y1) +. (b2 *. y2) +. (b3 *. y3)) :: !out
      done
    done;
    List.rev !out

let user_end = (90, 220, 120)
let used_end = (250, 80, 70)

(* a road on the screen: its points in the map's pixels, a quad a piece,
 * [w] pixels wide at the user, a third of it at the used *)
(* a polyline cut into [k] pieces of the same length: a road between two
 * parts under the same directory is a straight line, one piece, and its
 * gradient needs many *)
let resample (k : int) (pts : (float * float) list) : (float * float) array =
  let pts = Array.of_list pts in
  let n = Array.length pts in
  if n < 2 then pts
  else
    let len = Array.make n 0. in
    for i = 1 to n - 1 do
      let (x0, y0), (x1, y1) = (pts.(i - 1), pts.(i)) in
      len.(i) <- len.(i - 1) +. Float.sqrt (((x1 -. x0) ** 2.) +. ((y1 -. y0) ** 2.))
    done;
    let total = Float.max 1e-6 len.(n - 1) in
    let j = ref 1 in
    Array.init (k + 1) (fun i ->
        let d = total *. float_of_int i /. float_of_int k in
        while !j < n - 1 && len.(!j) < d do incr j done;
        let (x0, y0), (x1, y1) = (pts.(!j - 1), pts.(!j)) in
        let s = if len.(!j) > len.(!j - 1) then (d -. len.(!j - 1)) /. (len.(!j) -. len.(!j - 1)) else 0. in
        let s = Float.min 1. (Float.max 0. s) in
        (x0 +. (s *. (x1 -. x0)), y0 +. (s *. (y1 -. y0))))

let road ?(colours = (user_end, used_end)) (a : area) (pts : (float * float) list) (w : float) (alpha : float) : shape list =
  let user_end, used_end = colours in
  (* pieces of at most 16 pixels (up to 400): the gradient smooth, and
   * the pieces off the map left out close to its edge *)
  let len = fst (List.fold_left (fun (l, (px, py)) (x, y) -> (l +. Float.sqrt (((x -. px) ** 2.) +. ((y -. py) ** 2.)), (x, y))) (0., List.hd pts) pts) in
  let pts = resample (min 400 (max 28 (int_of_float (len /. 16.)))) pts in
  let n = Array.length pts in
  (* only on the map: a piece whose middle is off it is left out *)
  List.init (max 0 (n - 1)) Fun.id
  |> List.filter (fun i -> let (x0, y0), (x1, y1) = (pts.(i), pts.(i + 1)) in on a ((x0 +. x1) /. 2.) ((y0 +. y1) /. 2.))
  |> List.map (fun i ->
      let (x0, y0), (x1, y1) = (pts.(i), pts.(i + 1)) in
      let s0 = float_of_int i /. float_of_int (n - 1) and s1 = float_of_int (i + 1) /. float_of_int (n - 1) in
      let dx = x1 -. x0 and dy = y1 -. y0 in
      let d = Float.max 1e-6 (Float.sqrt ((dx *. dx) +. (dy *. dy))) in
      let nx = -.dy /. d and ny = dx /. d in
      let h0 = w *. (1. -. (0.66 *. s0)) /. 2. and h1 = w *. (1. -. (0.66 *. s1)) /. 2. in
      let r, g, b = mix used_end ((s0 +. s1) /. 2.) user_end in
      polygon (rgb r g b)
        [
          (sx a (x0 +. (nx *. h0)), sy a (y0 +. (ny *. h0)));
          (sx a (x1 +. (nx *. h1)), sy a (y1 +. (ny *. h1)));
          (sx a (x1 -. (nx *. h1)), sy a (y1 -. (ny *. h1)));
          (sx a (x0 -. (nx *. h0)), sy a (y0 -. (ny *. h0)));
        ]
      |> fade alpha)

(* the parts at this zoom: the countries from the whole map, the regions
 * from 3 times closer, then the files (the street map's zooms, by the
 * zoom from the whole map) *)
let depth_at (z : float) : int =
  let dl = Float.log z /. Float.log 3. in
  if dl < 1. then 1 else if dl < 2. then 2 else max_int

(* at most this many roads, the busiest, when none is hovered *)
let most = 160

(* claude: fading out as the code's colours come in, a line of the
 * median file [lh] pixels high: the roads are for the organisation, the
 * code is read without them *)
let roads (t : t) (c : camera) : shape list =
  let r = roads_of t in
  let lh =
    let chs = Array.to_list t.geometry |> List.filter_map (Option.map (fun g -> g.cell_h)) |> List.sort compare in
    match chs with [] -> 1. | _ -> List.nth chs (List.length chs / 2) *. c.z
  in
  let dim = 1. -. (0.8 *. Map_streets.smooth Map_streets.t_colours (2. *. Map_streets.t_colours) lh) in
  let d = depth_at c.z in
  let all = roads_at t r d in
  let hovered =
    match t.pointer with
    | None -> None
    | Some (u, v) ->
        Array.fold_left
          (fun acc (p : entry Treemap.placed) -> match p.node with File _ when inside p.rect u v -> Some (part_of r.tr d p.path) | _ -> acc)
          None t.placed
  in
  let shown =
    match hovered with
    | Some h -> List.filter (fun (a, b, _) -> a = h || b = h) all
    | None when d = max_int -> []
    | None -> List.filteri (fun i _ -> i < most) (List.stable_sort (fun (_, _, n) (_, _, m) -> compare m n) all)
  in
  let max_n = List.fold_left (fun m (_, _, n) -> max m n) 1 shown in
  let diag = let w = float_of_int c.a.pw and h = float_of_int c.a.ph in Float.sqrt ((w *. w) +. (h *. h)) in
  let drawn =
    List.map
      (fun (a, b, n) ->
        let pts = List.map (fun (u, v) -> (to_px c u, to_py c v)) (bspline (control_points r.tr a b)) in
        let (x0, y0), (x1, y1) = (List.hd pts, List.hd (List.rev pts)) in
        let len = Float.sqrt (((x1 -. x0) ** 2.) +. ((y1 -. y0) ** 2.)) in
        (len, pts, n))
      shown
  in
  (* the longest first, under the others *)
  let drawn = List.stable_sort (fun (l, _, _) (m, _, _) -> compare m l) drawn in
  let highlight = hovered <> None in
  (* the part hovered, framed (a part is a directory or a file: the tree
   * has both) *)
  (match Option.bind hovered (Hashtbl.find_opt r.tr.rects) with
  | Some (rect, _) -> (
      match clip c rect with Some (x0, y0, x1, y1) -> frame c.a white (float_of_int x0) (float_of_int y0) (float_of_int x1) (float_of_int y1) 2. | None -> [])
  | None -> [])
  @ List.concat_map
      (fun (len, pts, n) ->
        let w = 3. +. (9. *. Float.sqrt (float_of_int n /. float_of_int max_n)) in
        let alpha = if highlight then 0.9 else 0.2 +. (0.5 *. (1. -. Float.min 1. (len /. diag))) in
        road c.a pts w (alpha *. dim))
      drawn

let labels (t : t) (c : camera) (q : float) : shape list = roads t c @ Map_streets.labels t c q

let style : style = { sname = "atlas"; paint; labels; pick = (fun _ _ _ _ _ -> None); unit_at = (fun _ _ _ _ _ -> None); units = false }
