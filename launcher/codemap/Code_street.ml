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

type edge = { src : string; from_line : int; from_col : int; target : string; target_line : int; target_col : int; name : string }
type panel = { path : string; ground : Code_ground.t; count : int }
type mode = Uses | Users | Both
type t = {
  focus : Code_ground.t;
  left : panel list;
  right : panel list;
  uses : edge list;
  users : edge list;
  focus_path : string;
  left_more : (string * int) list;
  right_more : (string * int) list;
}

(*****************************************************************************)
(* The uses *)
(*****************************************************************************)

let uses ~(index : Code_names.index) ~(roots : string list) ~(path : string) (f : Code_file.t) : edge list =
  let seen = Hashtbl.create 64 in
  Array.to_list f.refs
  |> List.concat_map (fun refs ->
         List.filter_map
           (fun (r : Highlight_code.reference) ->
             (* an operator's (Basics' +.) is noise, not a tie *)
             let identifier = r.rname <> "" && (match r.rname.[0] with 'a' .. 'z' | 'A' .. 'Z' | '_' -> true | _ -> false) in
             match Code_names.find_in ~roots index ~from:path f r with
             | (c : Code_names.candidate) :: _, true when identifier && c.path <> path ->
                 let key = (r.rline, r.rcol, c.path, c.line) in
                 if Hashtbl.mem seen key then None
                 else begin
                   Hashtbl.replace seen key ();
                   Some { src = path; from_line = r.rline; from_col = r.rcol; target = c.path; target_line = c.line; target_col = c.col; name = r.rname }
                 end
             | _ -> None)
           refs)

(*****************************************************************************)
(* The layout *)
(*****************************************************************************)

let most = 6
let title = 22.
let gap = 10.

(* a side's panels, in a column from [x0], [w] wide: its files, the most
 * tied first (the program's own first), each as high as its ties and
 * laid out with its lines tied to the focus tall *)
let side ~(first : string -> bool) ~(file : string -> Code_file.t option) (ties : (string * int) list) ~(x0 : float) ~(w : float) ~(ph : int) :
    panel list * (string * int) list =
  let counts = Hashtbl.create 16 in
  List.iter (fun (p, _) -> Hashtbl.replace counts p (1 + Option.value (Hashtbl.find_opt counts p) ~default:0)) ties;
  let ranked =
    Hashtbl.fold (fun p n acc -> (p, n) :: acc) counts []
    |> List.sort (fun (p, n) (q, m) ->
           match (first p, first q) with true, false -> -1 | false, true -> 1 | _ -> if n <> m then compare m n else compare p q)
  in
  let more = List.filteri (fun i _ -> i >= most) ranked in
  let chosen =
    List.filteri (fun i _ -> i < most) ranked |> List.filter_map (fun (p, n) -> Option.map (fun f -> (p, n, f)) (file p))
  in
  (* the program's own files thrice their share: to be read, not glimpsed *)
  let wanted = List.map (fun (p, n, _) -> float_of_int (n + 3) *. if first p then 3. else 1.) chosen in
  let total = List.fold_left ( +. ) 0. wanted in
  (* below the map's breadcrumb *)
  let top = 30. in
  let room = float_of_int ph -. top -. (gap *. float_of_int (max 0 (List.length chosen - 1))) in
  let y = ref top in
  List.map2
    (fun (p, n, (f : Code_file.t)) want ->
      let h = Float.max 60. (room *. want /. Float.max 1. total) in
      (* the eight most tied lines tall, the others a little less *)
      let lines = List.filter_map (fun (q, l) -> if q = p then Some l else None) ties in
      let by_line = List.sort_uniq compare lines |> List.map (fun l -> (l, List.length (List.filter (( = ) l) lines))) |> List.sort (fun (_, a) (_, b) -> compare b a) in
      let tall = List.filteri (fun i _ -> i < 8) by_line |> List.map fst in
      let base = Code_ground.weights f ~important:[] in
      let weights = Array.mapi (fun l wt -> if List.mem l tall then 4. else if List.mem l lines then 1.6 else wt *. 0.15) base in
      let g = Code_ground.layout ~x0 ~y0:(!y +. title) weights ~pw:(int_of_float w) ~ph:(int_of_float (h -. title)) in
      y := !y +. h +. gap;
      { path = p; ground = g; count = n })
    chosen wanted
  , more

let layout ~(mode : mode) ?(first = fun _ -> false) ~(focus_path : string) ~(focus : float array) ~(file : string -> Code_file.t option) ~(uses : edge list) ~(users : edge list) ~(pw : int) ~(ph : int) () : t =
  let w = float_of_int pw in
  let road = 50. in
  (* the focus's column, and the sides' *)
  let fx, fw, lw, rw =
    match mode with
    | Uses -> (0.42 *. w, 0.58 *. w, (0.42 *. w) -. road, 0.)
    | Users -> (0., 0.58 *. w, 0., (0.42 *. w) -. road)
    | Both -> (0.25 *. w, 0.5 *. w, (0.25 *. w) -. road, (0.25 *. w) -. road)
  in
  let ground = Code_ground.layout ~x0:fx focus ~pw:(int_of_float fw) ~ph in
  (* room at each side's foot for the files not shown *)
  let ph' = ph - 26 in
  let left, left_more = if lw > 0. then side ~first ~file (List.map (fun e -> (e.target, e.target_line)) uses) ~x0:0. ~w:lw ~ph:ph' else ([], []) in
  let right, right_more = if rw > 0. then side ~first ~file (List.map (fun e -> (e.src, e.from_line)) users) ~x0:(fx +. fw +. road) ~w:rw ~ph:ph' else ([], []) in
  let on side p = List.exists (fun (q : panel) -> q.path = p) side in
  {
    focus = ground;
    left;
    right;
    uses = List.filter (fun e -> on left e.target) uses;
    users = List.filter (fun e -> on right e.src) users;
    focus_path;
    left_more;
    right_more;
  }

let panels (t : t) = t.left @ t.right

let scale (t : t) (q : float) : t =
  let sp = List.map (fun p -> { p with ground = Code_ground.scale p.ground q }) in
  { t with focus = Code_ground.scale t.focus q; left = sp t.left; right = sp t.right }

let ground_of (t : t) (p : string) : Code_ground.t option =
  if p = t.focus_path then Some t.focus else Option.map (fun q -> q.ground) (List.find_opt (fun q -> q.path = p) (panels t))

let line_at (t : t) (x : float) (y : float) : (string * int) option =
  match Code_ground.line_at t.focus x y with
  | Some l -> Some (t.focus_path, l)
  | None -> List.find_map (fun p -> Option.map (fun l -> (p.path, l)) (Code_ground.line_at p.ground x y)) (panels t)

(*****************************************************************************)
(* The roads *)
(*****************************************************************************)

(* where a name is: its line's box, its column's cell *)
let at (g : Code_ground.t) (l : int) (col : int) : (float * float) option =
  if l >= Array.length g.places then None
  else
    let x, y, _, h = Code_ground.box g l in
    Some (x +. (float_of_int col *. Code_ground.cell_w g l), y +. (h /. 2.))

let lit_by (hover : (string * int) option) (e : edge) : bool =
  match hover with Some (p, l) -> (e.src = p && e.from_line = l) || (e.target = p && e.target_line = l) | None -> false

(* each edge's road, from the name used to the name defined, through a
 * hub on the panel's side facing the focus, at the height of its ties:
 * the roads to one panel bundled there *)
let paths (t : t) : (edge * (float * float) list) list =
  let road hub_x (p : panel) (e : edge) =
    match (ground_of t e.src, ground_of t e.target) with
    | Some gs, Some gt -> (
        match (at gs e.from_line e.from_col, at gt e.target_line e.target_col) with
        | Some (ax, ay), Some (bx, by) ->
            (* the hub: the middle of the panel's tied lines *)
            let ties = List.filter_map (fun (x : edge) -> if x.target = p.path then at gt x.target_line 0 else if x.src = p.path then at gs x.from_line 0 else None) (t.uses @ t.users) in
            let hy = match ties with [] -> by | _ -> List.fold_left (fun acc (_, y) -> acc +. y) 0. ties /. float_of_int (List.length ties) in
            Some (e, Map_atlas.bspline [| (ax, ay); ((ax +. hub_x) /. 2., (ay +. hy) /. 2.); (hub_x, hy); (bx, by) |])
        | _ -> None)
    | _ -> None
  in
  List.filter_map
    (fun (e : edge) ->
      match List.find_opt (fun (p : panel) -> p.path = e.target) t.left with
      | Some p when Array.length p.ground.places > 0 -> let x, _, w, _ = Code_ground.box p.ground 0 in road (x +. w +. 25.) p e
      | _ -> None)
    t.uses
  @ List.filter_map
      (fun (e : edge) ->
        match List.find_opt (fun (p : panel) -> p.path = e.src) t.right with
        | Some p when Array.length p.ground.places > 0 -> let x, _, _, _ = Code_ground.box p.ground 0 in road (x -. 25.) p e
        | _ -> None)
      t.users

let roads ?hover (a : Code_map_base.area) (t : t) : Playground.shape list =
  let lit, faint = List.partition (fun (e, _) -> lit_by hover e) (paths t) in
  List.concat_map (fun (_, pts) -> Map_atlas.road a pts 2. 0.2) faint @ List.concat_map (fun (_, pts) -> Map_atlas.road a pts 5. 0.95) lit

let ends ?hover (a : Code_map_base.area) (t : t) : Playground.shape list =
  let frame (g : Code_ground.t) l color =
    if l >= Array.length g.places then []
    else
      let x, y, w, h = Code_ground.box g l in
      Code_map_base.frame a color (x -. 2.) (y -. 1.) (x +. w) (y +. Float.max h 3. +. 1.) 2.
  in
  List.concat_map
    (fun e ->
      if not (lit_by hover e) then []
      else
        (match ground_of t e.src with Some g -> frame g e.from_line (Playground.rgb 90 220 120) | None -> [])
        @ match ground_of t e.target with Some g -> frame g e.target_line (Playground.rgb 250 80 70) | None -> [])
    (t.uses @ t.users)
