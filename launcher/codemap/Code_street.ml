(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Code_street.mli *)

type edge = { from_line : int; target : string; target_line : int; name : string }
type panel = { path : string; ground : Code_ground.t; count : int }
type t = { focus : Code_ground.t; panels : panel list; edges : edge list; split : float }

(*****************************************************************************)
(* The uses *)
(*****************************************************************************)

let uses ~(index : Code_names.index) ~(roots : string list) ~(path : string) (f : Code_file.t) : edge list =
  let seen = Hashtbl.create 64 in
  Array.to_list f.refs
  |> List.concat_map (fun refs ->
         List.filter_map
           (fun (r : Highlight_code.reference) ->
             (* an operator's (Basics' +., /..) is noise, not an association *)
             let identifier = r.rname <> "" && (match r.rname.[0] with 'a' .. 'z' | 'A' .. 'Z' | '_' -> true | _ -> false) in
             match Code_names.find_in ~roots index ~from:path f r with
             | (c : Code_names.candidate) :: _, true when identifier && c.path <> path ->
                 let key = (r.rline, c.path, c.line) in
                 if Hashtbl.mem seen key then None
                 else begin
                   Hashtbl.replace seen key ();
                   Some { from_line = r.rline; target = c.path; target_line = c.line; name = r.rname }
                 end
             | _ -> None)
           refs)

(*****************************************************************************)
(* The layout *)
(*****************************************************************************)

(* the focus's share of the map's width *)
let share = 0.56
let most = 6

let layout ?(first = fun _ -> false) ~(focus : float array) ~(file : string -> Code_file.t option) (edges : edge list) ~(pw : int) ~(ph : int) : t =
  let split = share *. float_of_int pw in
  let ground = Code_ground.layout focus ~pw:(int_of_float split) ~ph in
  (* the files used, the most used first *)
  let counts = Hashtbl.create 16 in
  List.iter (fun e -> Hashtbl.replace counts e.target (1 + Option.value (Hashtbl.find_opt counts e.target) ~default:0)) edges;
  let used =
    Hashtbl.fold (fun p n acc -> (p, n) :: acc) counts []
    |> List.sort (fun (p, n) (q, m) ->
           (* the program's own code first (its kits), then the most used *)
           match (first p, first q) with
           | true, false -> -1
           | false, true -> 1
           | _ -> if n <> m then compare m n else compare p q)
    |> List.filteri (fun i _ -> i < most)
    |> List.filter_map (fun (p, n) -> Option.map (fun f -> (p, n, f)) (file p))
  in
  (* their panels, one under the other, as high as their uses (and a
   * title's room) *)
  let gap = 10. and title = 22. in
  let x0 = split +. 60. in
  let w = float_of_int pw -. x0 in
  (* the program's own files thrice their share: to be read, not glimpsed *)
  let wanted = List.map (fun (p, n, _) -> float_of_int (n + 3) *. if first p then 3. else 1.) used in
  let total = List.fold_left ( +. ) 0. wanted in
  let room = float_of_int ph -. (gap *. float_of_int (max 0 (List.length used - 1))) in
  let y = ref 0. in
  let panels =
    List.map2
      (fun (p, n, (f : Code_file.t)) want ->
        let h = Float.max 60. (room *. want /. Float.max 1. total) in
        let targets = List.filter_map (fun e -> if e.target = p then Some e.target_line else None) edges in
        let base = Code_ground.weights f ~important:[] in
        let weights = Array.mapi (fun l wt -> if List.mem l targets then 4. else wt *. 0.15) base in
        let g = Code_ground.layout ~x0 ~y0:(!y +. title) weights ~pw:(int_of_float w) ~ph:(int_of_float (h -. title)) in
        y := !y +. h +. gap;
        { path = p; ground = g; count = n })
      used wanted
  in
  let shown = List.map (fun p -> p.path) panels in
  { focus = ground; panels; edges = List.filter (fun e -> List.mem e.target shown) edges; split }

let scale (t : t) (q : float) : t =
  { focus = Code_ground.scale t.focus q; panels = List.map (fun p -> { p with ground = Code_ground.scale p.ground q }) t.panels; edges = t.edges; split = t.split *. q }

let line_at (t : t) ~(focus_path : string) (x : float) (y : float) : (string * int) option =
  match Code_ground.line_at t.focus x y with
  | Some l -> Some (focus_path, l)
  | None -> List.find_map (fun p -> Option.map (fun l -> (p.path, l)) (Code_ground.line_at p.ground x y)) t.panels

(*****************************************************************************)
(* The roads *)
(*****************************************************************************)

let mid (g : Code_ground.t) (l : int) : float * float * float =
  let x, y, w, h = Code_ground.box g l in
  (x, x +. w, y +. (h /. 2.))

let lit_by (hover : (string * int) option) ~(focus_path : string) (e : edge) : bool =
  match hover with Some (p, l) -> (p = focus_path && e.from_line = l) || (e.target = p && e.target_line = l) | None -> false

let roads ?hover ?(focus_path = "") (a : Code_map_base.area) (t : t) : Playground.shape list =
  let any = match hover with Some _ -> List.exists (lit_by hover ~focus_path) t.edges | None -> false in
  let road e pts =
    if lit_by hover ~focus_path e then `Lit (Map_atlas.road a pts 5. 0.95)
    else `Dim (Map_atlas.road a pts 3. (if any then 0.12 else 0.55))
  in
  let all =
  List.concat_map
    (fun (p : panel) ->
      (* the bundle's point: before the panel, at its middle *)
      let px, py, _, ph = match p.ground.places with [||] -> (0., 0., 0., 0.) | _ -> Code_ground.box p.ground 0 in
      ignore ph;
      let panel_mid = List.fold_left (fun acc e -> if e.target = p.path then let _, _, y = mid p.ground e.target_line in y :: acc else acc) [] t.edges in
      let by = match panel_mid with [] -> py | ys -> List.fold_left ( +. ) 0. ys /. float_of_int (List.length ys) in
      let hub = (px -. 40., by) in
      List.filter_map
        (fun e ->
          if e.target <> p.path || e.from_line >= Array.length t.focus.places || e.target_line >= Array.length p.ground.places then None
          else
            let _, _, y0 = mid t.focus e.from_line in
            let x1, _, y1 = mid p.ground e.target_line in
            let start = (t.split, y0) and stop = (x1 -. 6., y1) in
            let pts = Map_atlas.bspline [| start; (t.split +. 20., y0); ((t.split +. fst hub) /. 2., (y0 +. snd hub) /. 2.); hub; stop |] in
            Some (road e pts))
        t.edges)
    t.panels
  in
  (* the roads lit over the others *)
  List.concat_map (function `Dim s -> s | `Lit _ -> []) all @ List.concat_map (function `Lit s -> s | `Dim _ -> []) all

(* the two ends of the roads lit: the uses framed green, the definitions
 * red *)
let ends ?hover ~(focus_path : string) (a : Code_map_base.area) (t : t) : Playground.shape list =
  let frame (g : Code_ground.t) l color =
    if l >= Array.length g.places then []
    else
      let x, y, w, h = Code_ground.box g l in
      Code_map_base.frame a color (x -. 2.) (y -. 1.) (x +. w) (y +. Float.max h 3. +. 1.) 2.
  in
  List.concat_map
    (fun e ->
      if not (lit_by hover ~focus_path e) then []
      else
        frame t.focus e.from_line (Playground.rgb 90 220 120)
        @ match List.find_opt (fun p -> p.path = e.target) t.panels with Some p -> frame p.ground e.target_line (Playground.rgb 250 80 70) | None -> [])
    t.edges
