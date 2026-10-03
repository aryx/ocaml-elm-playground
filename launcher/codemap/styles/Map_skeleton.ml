(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Map_skeleton.mli *)

open Playground
open Code_map_base

(*****************************************************************************)
(* The skeletons *)
(*****************************************************************************)

(* claude: the skeletons (Code_guide.skeleton, x), at every level: from
 * afar, a bone a dot at its line in its file's columns; at the ground and
 * the street, its definition lit in the shaded file; the joints between
 * the bones on the map, ivory roads, their direction the road's taper;
 * an end off the map, a stub to the map's edge naming it *)
let ivory = (245, 232, 200)

(* claude: the gradient of the layers and the skeleton's order: 0 green
 * (the top, the start), 0.5 yellow, 1 red (the bottom, the end) *)
let heat (x : float) : int * int * int =
  let mix (r0, g0, b0) (r1, g1, b1) k = (r0 + int_of_float (k *. float_of_int (r1 - r0)), g0 + int_of_float (k *. float_of_int (g1 - g0)), b0 + int_of_float (k *. float_of_int (b1 - b0))) in
  if x < 0.5 then mix (90, 220, 120) (245, 225, 90) (x *. 2.) else mix (245, 225, 90) (250, 80, 70) ((x -. 0.5) *. 2.)

(* the grounds on the map now, a file's path and its layout: the focus's
 * and, at the street, the panels' *)
let grounds (t : t) (e : entry) : (string * Code_ground.t) list =
  if t.street then
    let s = Map_paint.street_of t e in
    (e.path, s.focus) :: List.map (fun (p : Code_street.panel) -> (p.path, p.ground)) (Code_street.panels s)
  else [ (e.path, Map_paint.ground_of t e) ]

let entry_of_simple (t : t) (path : string) : entry option = List.find_opt (fun (x : entry) -> x.path = path) (t.entries @ t.beyond)

(* claude: opti: the entries by path in a table, one per map's entries,
 * kept: each bone of the X-ray, every frame, copied the list of all of
 * them and went through it (principia's 2,200) *)
let entries_index_cache : (entry list * entry list * (string, entry) Hashtbl.t) option ref = ref None

let entry_of_opti (t : t) (path : string) : entry option =
  let ix =
    match !entries_index_cache with
    | Some (es, bs, ix) when es == t.entries && bs == t.beyond -> ix
    | _ ->
        let ix = Hashtbl.create 1024 in
        (* the first of a path kept, as List.find_opt found it *)
        List.iter (fun (x : entry) -> if not (Hashtbl.mem ix x.path) then Hashtbl.replace ix x.path x) (t.entries @ t.beyond);
        entries_index_cache := Some (t.entries, t.beyond, ix);
        ix
  in
  Hashtbl.find_opt ix path

let entry_of (t : t) (path : string) : entry option = if !Opti.enabled then entry_of_opti t path else entry_of_simple t path

(* where a file's line is on the map now: its left, its middle's height,
 * the end of its text *)
(* claude: a unit on the map, by its path *)
let placed_of_simple (t : t) (path : string) : int option =
  let found = ref None in
  Array.iteri (fun i (p : entry Treemap.placed) -> if p.path = path then found := Some i) t.placed;
  !found

(* claude: opti: the units by path in a table, one per layout, kept, not
 * all of them gone through for one: spot and unit_spot find one for
 * every bone of the X-ray, every frame (principia's 2,200: a slow frame
 * the X-ray on) *)
let placed_index_cache : (entry Treemap.placed array * (string, int) Hashtbl.t) option ref = ref None

let placed_of_opti (t : t) (path : string) : int option =
  let ix =
    match !placed_index_cache with
    | Some (placed, ix) when placed == t.placed -> ix
    | _ ->
        let ix = Hashtbl.create (Array.length t.placed) in
        Array.iteri (fun i (p : entry Treemap.placed) -> Hashtbl.replace ix p.path i) t.placed;
        placed_index_cache := Some (t.placed, ix);
        ix
  in
  Hashtbl.find_opt ix path

let placed_of (t : t) (path : string) : int option = if !Opti.enabled then placed_of_opti t path else placed_of_simple t path

let spot (t : t) (c : camera) (path : string) (line : int) : (float * float * float) option =
  match Map_paint.at_ground t c with
  | Some e -> (
      match List.assoc_opt path (grounds t e) with
      | Some g when line < Array.length g.places ->
          let x, y, w, h = Code_ground.box g line in
          let f = Option.map (fun (x : entry) -> Lazy.force x.file) (entry_of t path) in
          let last = ref 0 in
          Option.iter (fun (f : Code_file.t) -> for k = 0 to Code_file.cols - 1 do if Bytes.get f.chars ((line * Code_file.cols) + k) <> '\000' && Bytes.get f.chars ((line * Code_file.cols) + k) <> ' ' then last := k + 1 done) f;
          Some (x, y +. (h /. 2.), Float.min (x +. w) (x +. (float_of_int !last *. Code_ground.cell_w g line)))
      | _ -> None)
  | None -> (
      let found = match placed_of t path with Some i when (match t.placed.(i).node with File _ -> true | Dir _ -> false) -> Some i | _ -> None in
      match found with
      | Some i -> (
          match (clip c t.placed.(i).rect, t.geometry.(i)) with
          | Some _, Some g ->
              let x, y = line_pos t.placed.(i).rect g line in
              let px = to_px c x and py = to_py c (y +. (g.cell_h /. 2.)) in
              Some (px, py, px)
          | _ -> None)
      | None -> None)

(* a bone's line in its file, found once; a whole file's or directory's
 * none *)
let bone_line (t : t) (b : Code_guide.bone) : int option =
  if b.banchor = "" then None else match entry_of t b.bpath with Some e -> Map_names.capital_line e b.banchor | None -> None

(* where a whole file or directory is on the map: its top left corner, a
 * little in (its name is at its centre) *)
let unit_spot (t : t) (c : camera) (path : string) : (float * float * float) option =
  match placed_of t path with
  | Some i -> (
      match clip c t.placed.(i).rect with
      | Some (x0, y0, x1, y1) when x1 - x0 > 30 && y1 - y0 > 30 -> (
          match t.placed.(i).node with
          (* claude: a directory's bone under its name, at its centre: at
           * its top left corner it seemed its first file's (the author) *)
          | Dir _ ->
              let x = float_of_int (x0 + x1) /. 2. and h = float_of_int (y1 - y0) in
              let y = (float_of_int (y0 + y1) /. 2.) +. Float.min 45. (Float.max 16. (0.18 *. h)) in
              Some (x, y, x)
          | File _ ->
              let x = float_of_int x0 +. 26. and y = float_of_int y0 +. 30. in
              Some (x, y, x))
      | _ -> None)
  | None -> None

(* a definition's lines: from its header to the next top-level one *)
let extent (f : Code_file.t) (line : int) : int * int =
  let next = List.fold_left (fun acc (l, _, (cat : Highlight_code.category)) -> if l > line && l < acc && cat <> Comment_section then l else acc) (Code_file.nlines f) f.defs in
  (line, next - 1)

(* claude: a file's skeleton when no config gives one, derived from its
 * code: its capitals and important lines (the config's words as roles),
 * else its definitions the most used within it; a joint from a to b
 * when a's definition uses b *)
let derived_file (t : t) (e : entry) : Code_guide.skeleton option =
  let f = Lazy.force e.file in
  let n = Code_file.nlines f in
  let self (l : int) (name : string) =
    if l < 0 || l >= n then None
    else List.find_opt (fun (o : Highlight_code.occurrence) -> o.bound_at = (o.line, o.col) && o.len = String.length name) f.names.(l)
  in
  let name_of at = Code_guide.anchor_name at in
  let from_config =
    match Code_guide.file_note t.guide e.path with
    | Some n ->
        List.filter_map
          (fun (it : Code_guide.item) ->
            match Code_guide.split it.at with
            | None, at when String.contains at ':' && not (Code_search.starts at "comment:" || Code_search.starts at "line:" || Code_search.starts at "section:") ->
                Option.map (fun l -> (l, name_of at, at, Option.value it.say ~default:(name_of at))) (Map_names.capital_line e at)
            | _ -> None)
          (n.capitals @ n.important)
    | None -> []
  in
  let by_use =
    List.filter_map
      (fun (l, name, (c : Highlight_code.category)) ->
        let kind = match c with Def_function | Def_value -> Some "def" | Def_type -> Some "type" | _ -> None in
        match (kind, self l name) with
        | Some k, Some o -> Some (List.length (Code_file.uses f o), (l, name, k ^ ":" ^ name, name))
        | _ -> None)
      f.defs
    |> List.filter (fun (u, _) -> u > 0)
    |> List.sort (fun (a, _) (b, _) -> compare b a)
    |> List.map snd
  in
  let seen = Hashtbl.create 16 in
  let bones =
    List.filter (fun (l, _, _, _) -> if Hashtbl.mem seen l then false else (Hashtbl.replace seen l (); true)) (from_config @ by_use)
    |> List.filteri (fun i _ -> i < 7)
  in
  if List.length bones < 2 then None
  else
    let at_of (_, _, a, _) = e.path ^ ":" ^ a in
    let joints =
      List.concat_map
        (fun ((la, _, _, _) as a) ->
          let s, en = extent f la in
          List.filter_map
            (fun ((lb, nb, _, _) as b) ->
              if lb = la then None
              else
                match self lb nb with
                | Some o when List.exists (fun (u : Highlight_code.occurrence) -> u.line >= s && u.line <= en) (Code_file.uses f o) ->
                    Some ({ jfrom = at_of a; jto = at_of b; jsay = None } : Code_guide.joint)
                | _ -> None)
            bones)
        bones
    in
    Some
      {
        sname = Filename.basename e.path ^ ": its parts, who uses whom (derived)";
        sdir = Filename.dirname e.path;
        bones = List.map (fun ((_, _, a, role) as b) -> ({ bat = at_of b; bpath = e.path; banchor = a; role } : Code_guide.bone)) bones;
        joints;
      }

(* a folder's, derived: its parts (its files and subfolders) the most
 * tied, and the uses between them (Code_rank.links) *)
let derived_dir (t : t) (here : string) : Code_guide.skeleton option =
  let under p = here = "" || Code_search.starts p (here ^ "/") in
  let part p =
    let rest = if here = "" then p else String.sub p (String.length here + 1) (String.length p - String.length here - 1) in
    let top = match String.index_opt rest '/' with Some i -> String.sub rest 0 i | None -> rest in
    if here = "" then top else here ^ "/" ^ top
  in
  let ties = Hashtbl.create 32 in
  List.iter
    (fun (a, b, n) ->
      if under a && under b then
        let pa = part a and pb = part b in
        if pa <> pb then Hashtbl.replace ties (pa, pb) (n + Option.value (Hashtbl.find_opt ties (pa, pb)) ~default:0))
    (Code_rank.links (rank_of t));
  let weight = Hashtbl.create 16 in
  Hashtbl.iter (fun (a, b) n -> List.iter (fun p -> Hashtbl.replace weight p (n + Option.value (Hashtbl.find_opt weight p) ~default:0)) [ a; b ]) ties;
  let parts = Hashtbl.fold (fun p n acc -> (p, n) :: acc) weight [] |> List.sort (fun (_, a) (_, b) -> compare b a) |> List.filteri (fun i _ -> i < 8) |> List.map fst in
  if List.length parts < 2 then None
  else
    let role p =
      let said = match Code_guide.file_note t.guide p with Some { summary = Some s; _ } -> Some s | _ -> Code_guide.dir_summary t.guide p in
      match said with Some s -> (match Map_names.wrap 40 s with l :: _ -> l | [] -> Filename.basename p) | None -> Filename.basename p
    in
    let joints =
      Hashtbl.fold (fun (a, b) n acc -> if List.mem a parts && List.mem b parts then ({ jfrom = a; jto = b; jsay = Some (Printf.sprintf "%d use%s" n (if n = 1 then "" else "s")) } : Code_guide.joint) :: acc else acc) ties []
      |> List.sort (fun (x : Code_guide.joint) (y : Code_guide.joint) -> compare x.jsay y.jsay)
    in
    Some
      {
        sname = (if here = "" then "the whole" else Filename.basename here) ^ ": its parts, who uses whom (derived)";
        sdir = here;
        bones = List.map (fun p -> ({ bat = p; bpath = p; banchor = ""; role = role p } : Code_guide.bone)) parts;
        joints;
      }

(* claude: the bones drawn last, their dot's place and their role's
 * width, for a hover (bone_card) and a click (Code_map) *)
let drawn_bones : (Code_guide.bone * float * float * float) list ref = ref []

(* claude: the last frame's, for placing the panels (least_bones): this
 * frame's are drawn after its banner is placed *)
let last_bones : (Code_guide.bone * float * float * float) list ref = ref []

(* claude: of boxes (left, top, width, height) where a panel of the X-ray
 * may go, the one over the fewest bones (the last frame's, their dots
 * and roles), the first as good (the author: "sometimes the skeleton
 * legend is on code that is actually part of the bones") *)
let least_bones (boxes : (float * float * float * float) list) : float * float * float * float =
  let covers (x0, y0, w, h) = List.length (List.filter (fun (_, x, y, bw) -> x +. bw >= x0 && x -. 8. <= x0 +. w && y +. 10. >= y0 && y -. 10. <= y0 +. h) !last_bones) in
  match boxes with
  | [] -> (0., 0., 0., 0.)
  | first :: rest -> snd (List.fold_left (fun (n, b) b' -> let n' = covers b' in if n' < n then (n', b') else (n, b)) (covers first, first) rest)

let skeleton_shapes (t : t) (c : camera) : shape list =
  last_bones := !drawn_bones;
  drawn_bones := [];
  let a = c.a in
  let ground = Map_paint.at_ground t c in
  let all = List.concat_map (fun (d : Code_guide.dir_note) -> d.skeletons) (Code_guide.dirs t.guide) in
  (* at the ground, the skeletons with a bone on the map *)
  let bone_spot (b : Code_guide.bone) =
    if b.banchor = "" then (if ground = None then Option.map (fun s -> (0, s)) (unit_spot t c b.bpath) else None)
    else Option.bind (bone_line t b) (fun l -> Option.map (fun s -> (l, s)) (spot t c b.bpath l))
  in
  (* the skeletons at hand: at the ground and the street, the file's
   * (a bone in it); from afar, the unit's config's, else the nearest
   * directory's above that has some. One at a time, x going to the next
   * and past the last turning the X-ray off; the deeper directories'
   * packed, a dot each, named: fly in to spread them *)
  (* claude: a folder laid out alone (top_kept): its root is the folder,
   * not the repository's (the author, in ~/ix's builder: the root's
   * skeletons came instead of builder's) *)
  (* claude: opti: the children of the top asked only when they matter,
   * a folder laid out alone looked at from its top: Code_units.children
   * goes through every unit, every frame of the X-ray.
   * old: match (t.focus, Code_units.children t.placed 0) with
   *      | 0, [ i ] when t.top_kept -> t.placed.(i).path | _ -> ... *)
  let here =
    match t.focus with
    | 0 when t.top_kept -> ( match Code_units.children t.placed 0 with [ i ] -> t.placed.(i).path | _ -> t.placed.(t.focus).path)
    | _ -> t.placed.(t.focus).path
  in
  let parent d = match String.rindex_opt d '/' with Some i -> String.sub d 0 i | None -> "" in
  let under d p = d = "" || p = d || (String.length p > String.length d && String.sub p 0 (String.length d + 1) = d ^ "/") in
  (* a skeleton inside one file is that file's: spread at its ground,
   * a dot from afar; a region's are the ones spanning its files *)
  (* claude: a program's: most of its bones definitions in one file (the
   * others, the library it stands on: an example's Playground.game) *)
  let one_file (s : Code_guide.skeleton) =
    match s.bones with
    | b :: _ ->
        let n = List.length s.bones in
        let here = List.length (List.filter (fun (x : Code_guide.bone) -> x.bpath = b.bpath && x.banchor <> "") s.bones) in
        b.banchor <> "" && 2 * here > n
    | [] -> false
  in
  (* claude: every file and every folder its skeleton (the author): the
   * configs' for it, else one derived from the code (derived_file,
   * derived_dir); never an enclosing folder's, whose bones are off the
   * map (the author, at ~/ix's Mkfile.ml) *)
  let candidates =
    match ground with
    | Some e -> (
        let mine (s : Code_guide.skeleton) =
          let n = List.length s.bones and here = List.length (List.filter (fun (b : Code_guide.bone) -> b.bpath = e.path) s.bones) in
          (* claude: half, a tie, only for its own config's files (the
           * author: x on Playground.mli went through every example's
           * still and scene3d, each a bone in it and one at home) *)
          here > 0 && (2 * here > n || (2 * here = n && s.sdir = Filename.dirname e.path) || (2 * here = n && s.sdir = "" && not (String.contains e.path '/')))
        in
        match List.filter mine all with [] -> Option.to_list (derived_file t e) | l -> l)
    | None -> (
        (* only a skeleton with two bones on the map: one mostly off it
         * would be stubs *)
        let seen (s : Code_guide.skeleton) = List.length (List.filter (fun b -> bone_spot b <> None) s.bones) >= 2 in
        match List.filter (fun (s : Code_guide.skeleton) -> s.sdir = here && (not (one_file s)) && seen s) all with
        | [] -> Option.to_list (derived_dir t here)
        | l -> l)
  in
  let k = List.length candidates in
  if t.xray_n >= max 1 k then begin
    t.xray <- false;
    t.xray_n <- 0
  end;
  let shown = match List.nth_opt candidates t.xray_n with Some s -> [ s ] | None -> [] in
  let deeper =
    if ground <> None then []
    (* claude: only a level down (the unit's own, or its subdirectories'):
     * with every directory described, all the levels' dots at once hid
     * the map; flying in shows the next level's *)
    else
      List.filter
        (fun (s : Code_guide.skeleton) ->
          (* a program's (inside one file) from its own directory only: a
           * level up, the examples' 91 were a flood *)
          ((one_file s && s.sdir = here) || ((not (one_file s)) && s.sdir <> here && parent s.sdir = here))
          && under here s.sdir && not (List.memq s candidates))
        all
  in
  let banner =
    match shown with
    | [ s ] ->
        let text = Printf.sprintf "X-ray: %s   (%d/%d, x: %s)" s.sname (t.xray_n + 1) k (if t.xray_n + 1 < k then "the next" else "off") in
        let tw = 0.5 *. 16. *. float_of_int (String.length text) in
        let w = tw +. 20. and h = 26. in
        let pw = float_of_int a.pw and ph = float_of_int a.ph in
        (* at the map's foot (the bones sit at the regions' top corners),
         * else where it hides fewest *)
        let x0, y0, _, _ = least_bones [ ((pw -. w) /. 2., ph -. 29., w, h); ((pw -. w) /. 2., 30., w, h); (10., ph -. 29., w, h); (pw -. w -. 10., ph -. 29., w, h) ] in
        let cx = x0 +. (w /. 2.) and cy = y0 +. (h /. 2.) in
        [
          rectangle (rgb 18 16 36) w h |> move (sx a cx) (sy a cy) |> fade 0.92;
          words (let r, g, b = ivory in rgb r g b) text |> scale (16. /. words_font_size) |> move (sx a cx) (sy a cy);
        ]
    | _ -> []
  in
  (* from afar, a file whose bones are a few pixels apart (TinyInvaders'
   * five) is one dot, its skeleton's name, the joints to other files
   * leaving from it: coming nearer spreads it *)
  let packed_files =
    if ground <> None then []
    else
      let by_file = Hashtbl.create 8 in
      List.iter
        (fun (sk : Code_guide.skeleton) ->
          List.iter
            (fun (b : Code_guide.bone) ->
              match bone_spot b with
              | Some (_, (x, y, _)) -> Hashtbl.replace by_file b.bpath ((x, y, sk.sname) :: Option.value (Hashtbl.find_opt by_file b.bpath) ~default:[])
              | None -> ())
            sk.bones)
        shown;
      Hashtbl.fold
        (fun path spots acc ->
          let xs = List.map (fun (x, _, _) -> x) spots and ys = List.map (fun (_, y, _) -> y) spots in
          let lo l = List.fold_left Float.min Float.infinity l and hi l = List.fold_left Float.max Float.neg_infinity l in
          if List.length spots >= 2 && hi xs -. lo xs +. (hi ys -. lo ys) < 60. then
            (path, ((lo xs +. hi xs) /. 2., (lo ys +. hi ys) /. 2., List.sort_uniq compare (List.map (fun (_, _, n) -> n) spots))) :: acc
          else acc)
        by_file []
  in
  let packed_at path = List.assoc_opt path packed_files in
  (* a bone's place: its file's dot when packed *)
  let bone_spot (b : Code_guide.bone) =
    match (packed_at b.bpath, bone_spot b) with Some (x, y, _), Some (l, _) -> Some (l, (x +. 12., y, x +. 12.)) | _, s -> s
  in
  let skeleton_on = List.mem Code_anatomy.Skeleton !Code_anatomy.shown in
  let r, g, b = ivory in
  let ink_i = rgb r g b in
  let bones = List.concat_map (fun (s : Code_guide.skeleton) -> s.bones) shown in
  (* claude: the order to read a skeleton in (the author: "infer an order
   * between things, a starting point, like the main then app, and color
   * the edges and bones depending on the depth, so you would know where
   * to start and what to follow"): its joints a graph, each bone's depth
   * the longest chain of joints to it from a bone nothing leads to (a
   * cycle one step, Code_rank's components); a bone with a joint
   * numbered by its depth, then top to bottom (a cycle's bones one
   * number), coloured by it -- green the start, red the end, the layers'
   * gradient; a skeleton without joints, ivory, unnumbered *)
  let order : (string, (int * int * int) * int) Hashtbl.t = Hashtbl.create 32 in
  List.iter
    (fun (sk : Code_guide.skeleton) ->
      let ids = Hashtbl.create 16 in
      List.iteri (fun i (bn : Code_guide.bone) -> if not (Hashtbl.mem ids bn.bat) then Hashtbl.replace ids bn.bat i) sk.bones;
      let n = List.length sk.bones in
      let succ = Array.make n [] and linked = Array.make n false in
      List.iter
        (fun (j : Code_guide.joint) ->
          match (Hashtbl.find_opt ids j.jfrom, Hashtbl.find_opt ids j.jto) with
          | Some a, Some b when a <> b ->
              succ.(a) <- b :: succ.(a);
              linked.(a) <- true;
              linked.(b) <- true
          | _ -> ())
        sk.joints;
      if Array.exists Fun.id linked then begin
        let ((comp, _) as comps) = Code_rank.components succ in
        let depth, _ = Code_rank.depth_height succ comps in
        let last = Array.fold_left max 0 depth in
        let spot_y (bn : Code_guide.bone) = match bone_spot bn with Some (_, (_, y, _)) -> y | None -> infinity in
        let arr = Array.of_list sk.bones in
        let ranked = List.filter (fun i -> linked.(i)) (List.init n Fun.id) |> List.stable_sort (fun i j -> compare (depth.(i), spot_y arr.(i)) (depth.(j), spot_y arr.(j))) in
        (* numbered in that order, a component's bones the first's number *)
        let numbers = Hashtbl.create 16 in
        let next = ref 1 in
        List.iter
          (fun i ->
            let k = match Hashtbl.find_opt numbers comp.(i) with Some k -> k | None -> let k = !next in incr next; Hashtbl.replace numbers comp.(i) k; k in
            let x = if last = 0 then 0. else float_of_int depth.(i) /. float_of_int last in
            Hashtbl.replace order arr.(i).bat (heat x, k))
          ranked
      end)
    shown;
  let colour_of at = match Hashtbl.find_opt order at with Some (c, _) -> c | None -> ivory in
  (* the shade: from afar the whole map; at the ground, every line but
   * the bones' definitions *)
  let shade =
    match ground with
    | None -> [ rectangle (rgb 8 6 20) (float_of_int a.pw) (float_of_int a.ph) |> move (sx a (float_of_int a.pw /. 2.)) (sy a (float_of_int a.ph /. 2.)) |> fade 0.6 ]
    | Some e ->
        List.concat_map
          (fun (path, (gr : Code_ground.t)) ->
            match entry_of t path with
            | None -> []
            | Some x ->
                let f = Lazy.force x.file in
                let lit = List.filter_map (fun (bn : Code_guide.bone) -> if bn.bpath = path then Option.map (extent f) (bone_line t bn) else None) bones in
                List.concat
                  (List.init (Array.length gr.places) (fun l ->
                       if List.exists (fun (s, e) -> l >= s && l <= e) lit then []
                       else
                         let x0, y0, w, h = Code_ground.box gr l in
                         [ rectangle (rgb 8 6 20) (w +. 16.) (h +. 0.5) |> move (sx a (x0 +. (w /. 2.))) (sy a (y0 +. (h /. 2.))) |> fade 0.72 ])))
          (grounds t e)
  in
  (* the joints, bent one way or the other so that a -> b and b -> a (a
   * loop, the model and its update) are two roads *)
  let where at = match List.find_opt (fun (bn : Code_guide.bone) -> bn.bat = at) bones with Some bn -> Option.map (fun (l, s) -> (bn, l, s)) (bone_spot bn) |> fun x -> (bn, x) |> Option.some | None -> None in
  let stubs = ref [] in
  let joints =
    List.concat_map
      (fun (s : Code_guide.skeleton) ->
        List.concat_map
          (fun (j : Code_guide.joint) ->
            let same_pack = match (where j.jfrom, where j.jto) with Some (x, _), Some (y, _) -> x.bpath = y.bpath && packed_at x.bpath <> None | _ -> false in
            match (where j.jfrom, where j.jto) with
            | _ when same_pack -> []
            | Some (_, Some (_, _, (ax0, ay, _))), Some (_, Some (_, _, (bx0, by, _))) ->
                (* from dot to dot *)
                let ax = ax0 -. 12. and bx = bx0 -. 12. in
                let dx = bx -. ax and dy = by -. ay in
                let len = Float.max 1. (Float.sqrt ((dx *. dx) +. (dy *. dy))) in
                (* the perpendicular turns with the direction: a -> b and b -> a
                 * bend to opposite sides by themselves, a loop drawn as two *)
                let bend = Float.min 70. (Float.max 30. (0.12 *. len)) in
                let mx = ((ax +. bx) /. 2.) +. (-.dy /. len *. bend) and my = ((ay +. by) /. 2.) +. (dx /. len *. bend) in
                let pts = Code_road.bspline [| (ax, ay); (mx, my); (bx, by) |] in
                (* claude: from its start's colour to its end's (ivory
                 * without an order) *)
                (if skeleton_on then Code_road.road ~colours:(colour_of j.jfrom, colour_of j.jto) a pts 6. 0.85 else [])
                @ (match j.jsay with Some w when skeleton_on -> [ words ink_i w |> scale (13. /. words_font_size) |> move (sx a mx) (sy a my) ] | _ -> [])
            | Some (_, Some (_, _, (ax0, ay, _))), Some (bn, None) | Some (bn, None), Some (_, Some (_, _, (ax0, ay, _))) ->
                (* an end off the map: a stub to its port on the edge (below) *)
                stubs := (ax0 -. 12., ay, bn) :: !stubs;
                []
            | _ -> [])
          s.joints)
      shown
  in
  (* the bones: a dot and their role *)
  let marks =
    List.concat_map
      (fun (bn : Code_guide.bone) ->
        match bone_spot bn with
        | _ when packed_at bn.bpath <> None -> []
        | None -> []
        | Some (_, (x, y, _)) ->
            let size = 14. in
            (* claude: its number in the reading order, and its colour *)
            let (cr, cg, cb), role = match Hashtbl.find_opt order bn.bat with Some (c, k) -> (c, Printf.sprintf "%d. %s" k bn.role) | None -> (ivory, bn.role) in
            let ink_b = rgb cr cg cb in
            let tw = 0.5 *. size *. float_of_int (String.length role) in
            (* above the header's start, over the shaded line before it *)
            let lx = x in
            drawn_bones := (bn, x -. 12., y, tw) :: !drawn_bones;
            [
              circle (rgb 20 16 30) 8. |> move (sx a (x -. 12.)) (sy a y);
              circle ink_b 6. |> move (sx a (x -. 12.)) (sy a y);
              rectangle (rgb 18 16 36) (tw +. 10.) (size +. 6.) |> move (sx a (lx +. (tw /. 2.))) (sy a (y -. 24.)) |> fade 0.9;
              words ink_b role |> scale (size /. words_font_size) |> move (sx a (lx +. (tw /. 2.))) (sy a (y -. 24.));
            ])
      bones
  in
  let dots =
    List.concat_map
      (fun (_, (x, y, names)) ->
        let name = String.concat ", " names in
        let tw = 0.5 *. 14. *. float_of_int (String.length name) in
        [
          circle (rgb 20 16 30) 10. |> move (sx a x) (sy a y);
          circle ink_i 8. |> move (sx a x) (sy a y);
          rectangle (rgb 18 16 36) (tw +. 10.) 20. |> move (sx a (x +. 16. +. (tw /. 2.))) (sy a (y -. 18.)) |> fade 0.9;
          words ink_i name |> scale (14. /. words_font_size) |> move (sx a (x +. 16. +. (tw /. 2.))) (sy a (y -. 18.));
        ])
      packed_files
  in
  let deep_dots =
    List.concat_map
      (fun (s : Code_guide.skeleton) ->
        let spots = List.filter_map (fun b -> Option.map snd (bone_spot b)) s.bones in
        match spots with
        | [] -> []
        | _ ->
            let n = float_of_int (List.length spots) in
            let x = List.fold_left (fun acc (x, _, _) -> acc +. x) 0. spots /. n and y = List.fold_left (fun acc (_, y, _) -> acc +. y) 0. spots /. n in
            let tw = 0.5 *. 13. *. float_of_int (String.length s.sname) in
            (* a file's skeleton a small dot, its name the file's capital's
             * business: a hundred games must not crowd the map *)
            if one_file s then [ circle (rgb 20 16 30) 6. |> move (sx a x) (sy a y); circle ink_i 4. |> move (sx a x) (sy a y) ]
            else
            [
              circle (rgb 20 16 30) 9. |> move (sx a x) (sy a y);
              circle ink_i 7. |> move (sx a x) (sy a y);
              rectangle (rgb 18 16 36) (tw +. 10.) 18. |> move (sx a (x +. 14. +. (tw /. 2.))) (sy a (y -. 16.)) |> fade 0.85;
              words ink_i s.sname |> scale (13. /. words_font_size) |> move (sx a (x +. 14. +. (tw /. 2.))) (sy a (y -. 16.));
            ])
      deeper
  in
  (* the ends off the map: a port each on the map's right edge, at the
   * height of the stubs going to it, the ports spread so that their names
   * do not overlap; each named once *)
  let ports =
    let by = Hashtbl.create 8 in
    List.iter (fun (x, y, (bn : Code_guide.bone)) -> Hashtbl.replace by bn.bat ((x, y, bn) :: Option.value (Hashtbl.find_opt by bn.bat) ~default:[])) !stubs;
    let ps = Hashtbl.fold (fun _ l acc -> let (_, _, bn) = List.hd l in (bn, l, List.fold_left (fun m (_, y, _) -> m +. y) 0. l /. float_of_int (List.length l)) :: acc) by [] in
    let ps = List.sort (fun (_, _, y) (_, _, y') -> compare y y') ps in
    let last = ref neg_infinity in
    List.map (fun (bn, l, y) -> let y = Float.max y (!last +. 24.) in last := y; (bn, l, y)) ps
  in
  let ex = float_of_int a.pw -. 20. in
  let stub_shapes =
    List.concat_map
      (fun ((bn : Code_guide.bone), l, py) ->
        (* a definition: its name and file; a whole unit: its path and
         * what it is for *)
        let text = if bn.banchor = "" then Printf.sprintf "%s: %s" bn.bpath bn.role else Printf.sprintf "%s  %s" (Code_guide.anchor_name bn.bat) bn.bpath in
        let tw = 0.5 *. 13. *. float_of_int (String.length text) in
        List.concat_map
          (fun (x, y, _) ->
            let pts = Code_road.bspline [| (x, y); ((x +. ex) /. 2., ((y +. py) /. 2.) -. 30.); (ex, py) |] in
            (if skeleton_on then Code_road.road ~colours:(ivory, (200, 170, 110)) a pts 4. 0.6 else []))
          l
        @
        if skeleton_on then
          [
            rectangle (rgb 18 16 36) (tw +. 10.) 18. |> move (sx a (ex -. 4. -. (tw /. 2.))) (sy a (py +. 13.)) |> fade 0.9;
            words ink_i text |> scale (13. /. words_font_size) |> move (sx a (ex -. 4. -. (tw /. 2.))) (sy a (py +. 13.));
          ]
        else [])
      ports
  in
  let none =
    if shown = [] && deeper = [] && skeleton_on then [ label a dim 16. (float_of_int a.pw /. 2.) 30. "(no skeleton here: the configs name none)" ] else []
  in
  shade @ joints @ stub_shapes @ (if skeleton_on then marks @ dots @ deep_dots @ banner else []) @ none
