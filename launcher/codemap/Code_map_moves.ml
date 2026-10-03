(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Code_map_moves.mli *)

open Playground
open Code_map_base

let clamp_cam (c : camera) : camera = { c with z = Float.max 0.5 (Float.min 400. c.z) }

(* the camera a step nearer its target: the zoom eased in its logarithm,
 * so that going in 100 times feels as steady as going in 2 *)
let ease (c : camera) (target : camera) : camera =
  let a = 0.22 in
  let z = Float.exp (Float.log c.z +. (a *. (Float.log target.z -. Float.log c.z))) in
  (* the centre moves so that the point the zoom goes to stays put: a
   * straight line in the layout at a rate scaled by the zoom's *)
  let near = Float.abs (Float.log (z /. target.z)) < 0.002 && Float.abs (c.cx -. target.cx) *. z < 0.3 && Float.abs (c.cy -. target.cy) *. z < 0.3 in
  if near then target else { c with cx = c.cx +. (a *. (target.cx -. c.cx)); cy = c.cy +. (a *. (target.cy -. c.cy)); z }

(* the directory round the view, a size bigger: where going up goes *)
let up (t : t) : camera =
  let c = t.target in
  let vw = float_of_int c.a.pw /. c.z and vh = float_of_int c.a.ph /. c.z in
  let best = ref None in
  Array.iter
    (fun (p : entry Treemap.placed) ->
      match p.node with
      | Dir _ when inside p.rect c.cx c.cy && (p.rect.w > vw *. 1.3 || p.rect.h > vh *. 1.3) -> (
          match !best with
          | Some (b : entry Treemap.placed) when b.rect.w *. b.rect.h <= p.rect.w *. p.rect.h -> ()
          | _ -> best := Some p)
      | _ -> ())
    t.placed;
  match !best with Some p when p.depth > 0 -> fit c.a p.rect | _ -> home c.a

(* claude: the stops of Codemap's tour in a file: its header, its
 * sections (the (* Model *) between rules of stars), and the places
 * saying "the trick of this game"; lexes the file *)
let stops (e : entry) : (int * string) list =
  let f = Lazy.force e.file in
  let sections = List.filter_map (fun (l, name, cat) -> if cat = Highlight_code.Comment_section && l > 0 then Some (l, name) else None) f.defs in
  let marks = List.filter_map (fun l -> if l > 0 then Some (l, Code_file.trick) else None) f.tricks in
  (0, "its header") :: List.sort_uniq (fun (a, _) (b, _) -> compare a b) (sections @ marks)

(* claude: the name under a point of the layout, bound in its file
 * (Code_file.name_at, plan_codemap_naming.md levels 1 and 2), if the file
 * is lexed: the file's index in [placed], and the occurrence *)
let name_under(t : t) (u : float) (v : float) : (int * Highlight_code.occurrence) option =
  match under t u v with
  | Some i -> (
      match (t.placed.(i).node, t.geometry.(i)) with
      | File (_, _, e), Some g when Lazy.is_val e.file ->
          let r = t.placed.(i).rect in
          let k = int_of_float ((u -. r.x) /. g.colw) in
          let col = int_of_float ((u -. r.x -. (float_of_int k *. g.colw)) /. g.cell_w) in
          Option.map (fun o -> (i, o)) (Code_file.name_at (Lazy.force e.file) (line_at g r u v) col)
      | _ -> None)
  | None -> None

(* claude: the name defined elsewhere under a point of the layout
 * (Code_file.ref_at, level 3): the file's index and its path, and the
 * reference *)
let ref_under (t : t) (u : float) (v : float) : (int * string * Highlight_code.reference) option =
  match under t u v with
  | Some i -> (
      match (t.placed.(i).node, t.geometry.(i)) with
      | File (_, _, e), Some g when Lazy.is_val e.file ->
          let r = t.placed.(i).rect in
          let k = int_of_float ((u -. r.x) /. g.colw) in
          let col = int_of_float ((u -. r.x -. (float_of_int k *. g.colw)) /. g.cell_w) in
          Option.map (fun x -> (i, e.path, x)) (Code_file.ref_at (Lazy.force e.file) (line_at g r u v) col)
      | _ -> None)
  | None -> None

(* where it goes, among the map's files (Code_names), the last search kept *)
let found (t : t) (i : int) (path : string) (r : Highlight_code.reference) : Code_names.candidate list * bool =
  let key = (path, r.rline, r.rcol) in
  match t.found with
  | Some (k, res) when k = key -> res
  | _ ->
      let f = match t.placed.(i).node with File (_, _, e) -> Lazy.force e.file | Dir _ -> assert false in
      let res = Code_names.find_in ~roots:t.roots (index_of t) ~from:path f r in
      t.found <- Some (key, res);
      res

(* claude: a candidate's place on the map, the camera moved there (b
 * coming back), its name lit: its code a readable size, 10 units a line
 * at least *)
let go_to (t : t) (target : camera) (c : Code_names.candidate) : camera =
  let at = ref None in
  Array.iteri (fun i (p : entry Treemap.placed) -> match p.node with File (_, _, e) when e.path = c.path -> at := Some i | _ -> ()) t.placed;
  match !at with
  | Some i -> (
      match t.geometry.(i) with
      | Some g ->
          t.back <- (target, t.jumped) :: t.back;
          t.jumped <- Some (i, (c.line, c.col));
          t.choices <- None;
          let x, y = name_pos t.placed.(i).rect g c.line c.col in
          { target with cx = x; cy = y +. (g.cell_h /. 2.); z = Float.max target.z (10. /. g.cell_h) }
      | None -> target)
  | None -> target

(* claude: the file under a point readable where the camera is: its code
 * read on the map itself, no glass needed *)
let readable_at (t : t) (u : float) (v : float) : bool =
  match under t u v with
  | Some i -> ( match t.geometry.(i) with Some g -> readable (at_ratio t.cam (Playground_platform.pixel_ratio ())) g | None -> false)
  | None -> false

(* claude: moving by units (Code_units, a style's [units]: Map_atlas's):
 * where a key, the wheel or a click takes the map, a directory or file at
 * a time -- in, out, beside -- or None. A click on a name goes to it
 * (style.unit_at); on a block, a level down at most; on the ground (a
 * file looked at), the clicks are the names' (update). The wheel steps
 * once a gesture: its notches add up to one, then it rests until the
 * wheel has been still a moment (a trackpad's flick is many events) *)
let unit_move (computer : computer) ~(pressed : string -> bool) ~(arrow : string option) (t : t) ~(clicked : bool) (mpx : float) (mpy : float) : int option =
  let mouse = computer.mouse in
  let (Time now) = computer.time in
  let a = t.target.a in
  (* the camera moved some other way (a search, a jump): what it frames *)
  let frames (r : Treemap.rect) =
    let c = t.target and f = fit a r in
    Float.abs (Float.log (f.z /. c.z)) < 0.15
    && Float.abs (f.cx -. c.cx) *. c.z < 0.05 *. float_of_int a.pw
    && Float.abs (f.cy -. c.cy) *. c.z < 0.05 *. float_of_int a.ph
  in
  if not (frames t.placed.(t.focus).rect) then t.focus <- Code_units.deepest t.placed (fun q -> frames q.rect);
  let i = t.focus in
  let u = to_u t.cam mpx and v = to_v t.cam mpy in
  let on_map = on a mpx mpy in
  let wheel =
    if mouse.mwheel = 0. || not on_map || t.peek <> None then 0
    else if now -. t.wheel_at < 0.3 then (t.wheel_at <- now; t.wheel_debt <- 0.; 0)
    else begin
      t.wheel_debt <- t.wheel_debt +. mouse.mwheel;
      if Float.abs t.wheel_debt < 1. then 0
      else begin
        let step = if t.wheel_debt > 0. then 1 else -1 in
        t.wheel_debt <- 0.;
        t.wheel_at <- now;
        step
      end
    end
  in
  let is_file = match t.placed.(i).node with File _ -> true | Dir _ -> false in
  let side : Code_units.side option =
    match arrow with Some "ArrowLeft" -> Some Left | Some "ArrowRight" -> Some Right | Some "ArrowUp" -> Some Up | Some "ArrowDown" -> Some Down | _ -> None
  in
  let centre () = (t.target.cx, t.target.cy) in
  match side with
  | Some side -> Code_units.sibling t.placed i side
  | None ->
      if pressed "Home" || pressed "0" then Some 0
      else if pressed "Backspace" || (mouse.mrdown && not t.before_right) || pressed "-" || wheel < 0 then Code_units.parent t.placed i
      else if pressed "=" || pressed "+" then (let cu, cv = centre () in Code_units.toward t.placed i cu cv)
      else if wheel > 0 then Code_units.toward t.placed i u v
      else if clicked then
        match t.style.unit_at t t.cam (Playground_platform.pixel_ratio ()) mpx mpy with
        | Some j -> Some j
        | None -> (
            (* a section's title (Map_atlas's, column -1) is peeked at, not flown into *)
            match t.style.pick t t.cam (Playground_platform.pixel_ratio ()) mpx mpy with
            | Some (_, _, c) when c < 0 -> None
            | _ -> if is_file then None else Code_units.toward t.placed i u v)
      else None

(* claude: a search's hit gone to (Enter, or a click on a match): a
 * directory or file framed; a definition's or a line's file, and its
 * definition peeked at *)
let search_go (t : t) (h : Code_search.hit) : camera option =
  let found = ref None in
  Array.iteri (fun i (p : entry Treemap.placed) -> if p.path = h.path then found := Some i) t.placed;
  match !found with
  (* claude: a hit beyond the map (a program's map, searching the whole
   * repository): peeked at where one is *)
  | None ->
      (if h.kind = Def || h.kind = Text then
         let file_of p = List.find_map (fun (e : entry) -> if e.path = p then Some (Lazy.force e.file) else None) (t.entries @ t.beyond) in
         Code_map_peek.open_peek t file_of (h.path, h.line));
      None
  | Some i ->
      t.focus <- i;
      t.jumped <- None;
      t.choices <- None;
      t.peek <- None;
      t.peek_stack <- [];
      (if h.kind = Def || h.kind = Text then
         let file_of p = List.find_map (fun (e : entry) -> if e.path = p then Some (Lazy.force e.file) else None) (t.entries @ t.beyond) in
         Code_map_peek.open_peek t file_of (h.path, h.line));
      Some (fit t.target.a t.placed.(i).rect)

(* claude: a config's tour (Code_guide.tours): each stop a file and an
 * anchor, flown to and its definition peeked at, the stop's words in a
 * banner (Map_atlas.tour_banner); n the next, p the one before *)
let tour_go (t : t) (tr : Code_guide.tour) (k : int) : camera option =
  match List.nth_opt tr.stops k with
  | None -> None
  | Some i ->
      t.tour_on <- Some (tr, k);
      let hit : Code_search.hit =
        match Code_guide.split i.at with
        | Some path, anchor -> (
            match Map_atlas.anchor_line t path anchor with Some line -> { kind = Def; path; line; name = Code_guide.anchor_name anchor } | None -> { kind = File; path; line = 0; name = path })
        | None, path -> { kind = File; path; line = 0; name = path }
      in
      search_go t hit

(* claude: a config's view: its files, or a file and those it uses or
 * that use it (Code_rank.links) *)
let view_set (t : t) (v : Code_guide.view) : string list =
  match v.of_ with
  | None -> v.files
  | Some f ->
      let links = Code_rank.links (rank_of t) in
      let others =
        List.filter_map
          (fun (a, b, _) ->
            match v.with_ with
            | Some "uses" -> if a = f then Some b else None
            | _ -> if b = f then Some a else None)
          links
      in
      f :: List.sort_uniq compare others @ v.files

(* claude: back from the matrix (Map_graph): a unit flown to, a
 * definition peeked at *)
let go_back_to (t : t) (path : string) (line : int option) : t =
  let is_file = List.exists (fun (e : entry) -> e.path = path) (t.entries @ t.beyond) in
  let hit : Code_search.hit =
    match line with
    | Some l -> { kind = Def; path; line = l; name = "" }
    | None -> { kind = (if is_file then File else Dir); path; line = 0; name = "" }
  in
  match search_go t hit with Some c -> { t with target = c } | None -> t
