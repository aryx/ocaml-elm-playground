(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Map_names.mli *)

open Playground
open Code_map_base

(*****************************************************************************)
(* The names *)
(*****************************************************************************)

(* a name's place: its node, its box in the map's pixels, how much it
 * matters, and how to draw it *)
(* claude: words cut into lines of at most [width] characters *)
let wrap (width : int) (text : string) : string list =
  let words = List.filter (( <> ) "") (String.split_on_char ' ' text) in
  let lines, last =
    List.fold_left
      (fun (lines, cur) w -> if cur = "" then (lines, w) else if String.length cur + 1 + String.length w > width then (cur :: lines, w) else (lines, cur ^ " " ^ w))
      ([], "") words
  in
  List.rev (if last = "" then lines else last :: lines)

type name = { node : int; nbox : float * float * float * float; nrank : float; draw : shape; said : string list option; sect : (string * int) option; cap : (string * int * string) option (* claude: a capital's file, line, name *) }

(* claude: the capitals the configs name (Code_guide.capitals): where
 * each is in its file, found once (its file lexed then) *)
let capital_lines : (string, int option) Hashtbl.t = Hashtbl.create 16

let capital_line (e : entry) (at : string) : int option =
  let key = e.path ^ "\000" ^ at in
  match Hashtbl.find_opt capital_lines key with
  | Some l -> l
  | None ->
      let l = Result.to_option (Code_guide.find (Lazy.force e.file) at) in
      Hashtbl.replace capital_lines key l;
      l

(* a capital: a dot where it is and its name, what the config says of it
 * on its card; under the names of the regions and of their
 * subdirectories (a genre's name says more from afar), above the rest *)
(* claude: the camera's no part of it: the files by path, and the
 * capitals chosen among the configs' *)
let capitals_chosen (t : t) : (string, int * entry) Hashtbl.t * (string * Code_guide.item) list =
  let where = Hashtbl.create 64 in
  Array.iteri (fun i (p : entry Treemap.placed) -> match p.node with File (_, _, e) -> Hashtbl.replace where e.path (i, e) | Dir _ -> ()) t.placed;
  (* claude: from a unit, the capitals of files at most two directories
   * below it: every directory described, all of them at once were a
   * rash of dots; flying in shows the deeper ones *)
  let top = t.placed.(t.focus).path in
  let depth p = if p = "" then 0 else List.length (String.split_on_char '/' p) in
  let near path =
    let d = Filename.dirname path in
    let d = if d = "." then "" else d in
    (top = "" && depth d <= 2) || (top <> "" && (d = top || Code_search.starts d (top ^ "/")) && depth d - depth top <= 2)
  in
  (* and a file's first only (the config's main one), fourteen a region
   * at most (the unit's immediate subdirectories) *)
  let region p =
    let rest = if top = "" then p else String.sub p (String.length top + 1) (max 0 (String.length p - String.length top - 1)) in
    match String.index_opt rest '/' with Some k -> String.sub rest 0 k | None -> rest
  in
  let seen_file = Hashtbl.create 64 and per_region = Hashtbl.create 16 in
  let fans = Lazy.force t.fan_in in
  let fan p = Code_deps.fan fans p in
  let dir_of p = match Filename.dirname p with "." -> "" | d -> d in
  let lines p = match Hashtbl.find_opt where p with Some (_, (e : entry)) -> e.nlines | None -> 0 in
  let sorted = List.stable_sort (fun (p, _) (q, _) -> compare (fan q, lines q) (fan p, lines p)) (Code_guide.capitals t.guide) in
  (* claude: a module's .ml and .mli naming the same capital: one *)
  let seen_name = Hashtbl.create 64 in
  let modname p = Filename.remove_extension p in
  let chosen =
    List.filter
      (fun (path, (it : Code_guide.item)) ->
        near path
        (* a hub's capitals all (the core's: game, computer, shape) *)
        && (fan path >= 100 || not (Hashtbl.mem seen_file path))
        && (not (Hashtbl.mem seen_name (modname path, it.at)))
        && (Hashtbl.replace seen_name (modname path, it.at) (); true)
        &&
        let r = region path in
        let n = Option.value (Hashtbl.find_opt per_region r) ~default:0 in
        Hashtbl.replace seen_file path ();
        if n >= 14 then false else (Hashtbl.replace per_region r (n + 1); true))
      (* claude: the most central first, the files the most files name
       * (Code_deps.fan_in); and from more than a level above, only a file
       * some others depend on: a program nobody names is one of the
       * project's drivers, not its core (the author: "games and apps are
       * like device drivers in a linux kernel"), its capitals shown from
       * its genre *)
      (List.filter (fun (p, _) -> fan p >= 3 || depth (dir_of p) - depth top <= 1) sorted)
  in
  (* claude: and the unit's entry point, a capital whether a config names
   * it or not (the author, at principia's rio: "why the main or
   * threadmain of rio is not a big green capital? ... it's useful to see
   * the entry point usually, and in which file it's located"): of the
   * definitions under the unit, the one whose calls reach the most files,
   * if 30 or more; weight 0, never a config's, saying so *)
  let entry =
    match rank_if_counted t with
    | None -> None
    | Some r ->
        let under p = top = "" || p = top || Code_search.starts p (top ^ "/") in
        let best =
          List.fold_left
            (fun acc (p, l, (pl : Code_rank.place)) ->
              if pl.reach >= 30 && under p && Hashtbl.mem where p then match acc with Some (_, _, r') when r' >= pl.reach -> acc | _ -> Some (p, l, pl.reach) else acc)
            None (Code_rank.places r)
        in
        match best with
        | Some (p, l, _) -> (
            let _, e = Hashtbl.find where p in
            match List.find_opt (fun (l', _, _) -> l' = l) (Lazy.force e.file).defs with
            | Some (_, name, _) -> Some (p, name)
            (* claude: OCaml's unnamed let () = ..., a program's main *)
            | None -> Some (p, Printf.sprintf "line:%d" (l + 1)))
        | None -> None
  in
  (* a config's capital already: marked so, its file said too *)
  let is_entry (q, (it : Code_guide.item)) = match entry with Some (p, name) -> q = p && Code_guide.anchor_name it.at = name | None -> false in
  let chosen = List.map (fun ((q, (it : Code_guide.item)) as x) -> if is_entry x then (q, { it with weight = 0 }) else x) chosen in
  let added =
    match entry with
    | Some (p, name) when not (List.exists is_entry chosen) ->
        let at = if Code_search.starts name "line:" then name else "def:" ^ name in
        [ (p, ({ at; say = Some "the entry point: of the code here, what reaches the most"; weight = 0 } : Code_guide.item)) ]
    | _ -> []
  in
  (where, added @ chosen)

let capitals_drawn (t : t) (c : camera) (where : (string, int * entry) Hashtbl.t) (chosen : (string * Code_guide.item) list) : name list =
  let a = c.a in
  let fans = Lazy.force t.fan_in in
  let fan p = Code_deps.fan fans p in
  List.filter_map
    (fun (path, (it : Code_guide.item)) ->
      match Hashtbl.find_opt where path with
      | Some (i, e) when not (Map_paint.outside t t.placed.(i)) -> (
          match (clip c t.placed.(i).rect, t.geometry.(i)) with
          | Some _, Some g -> (
              match capital_line e it.at with
              | Some line ->
                  let x, y = line_pos t.placed.(i).rect g line in
                  let px = to_px c x and py = to_py c (y +. (g.cell_h /. 2.)) in
                  (* claude: a syncweb chunk's name, not its marker's text *)
                  let label = Code_guide.anchor_name it.at in
                  (* claude: a name too short to say anything from afar (t), its module's with it *)
                  let label = if String.length label <= 2 then String.capitalize_ascii (Filename.remove_extension (Filename.basename path)) ^ "." ^ label else label in
                  (* claude: the entry point found (weight 0), its file said *)
                  let label = if it.weight = 0 then Printf.sprintf "%s (%s)" (if Code_search.starts it.at "line:" then "main" else label) (Filename.basename path) else label in
                  (* claude: its own uses once counted (Code_rank, not counted
                   * here: rank_if_counted), the .ml's for an .mli's *)
                  let uses =
                    match rank_if_counted t with
                    | Some rank ->
                        let name = match String.rindex_opt label '.' with Some k -> String.sub label (k + 1) (String.length label - k - 1) | None -> label in
                        let impl = Filename.remove_extension path ^ ".ml" in
                        let at_impl = if Filename.check_suffix path ".mli" then (match Hashtbl.find_opt where impl with Some (_, ei) -> Option.map (fun l -> (impl, l)) (capital_line ei it.at) | None -> None) else None in
                        let q, l = match at_impl with Some x -> x | None -> (path, line) in
                        Some (Code_rank.uses rank q l name, Code_rank.reach rank q l)
                    | None -> None
                  in
                  let many = match uses with Some (u, _) -> u.files >= 10 | None -> false in
                  (* claude: green, the counterpart: a definition whose calls
                   * reach 30 files or more, an entry point, a main loop (the
                   * author: "stuff that is the entry point that exercises lots
                   * of the code"); much used first, red *)
                  let reaching = match uses with Some (_, r) -> r >= 30 && not (many || fan path >= 30) | None -> false in
                  (* claude: as large as central: the core's the map's largest;
                   * a definition used by many files as large as a central file's *)
                  let size =
                    let by_fan = match fan path with n when n >= 100 -> 24. | n when n >= 30 -> 19. | _ -> 15. in
                    (* claude: and by its own uses and reach, so that the one
                     * that matters stands out (the author: Window, used by 15
                     * files, as small as the rest) *)
                    let by_own = match uses with Some (u, r) -> if u.files >= 20 || r >= 100 then 24. else if u.files >= 10 || r >= 30 then 22. else 15. | None -> 15. in
                    Float.max by_fan by_own
                  in
                  let tw = 0.5 *. size *. float_of_int (String.length label) in
                  let x0 = px -. 6. and x1 = px +. 10. +. tw +. 4. in
                  (* claude: a definition many use is red, the map's colour of
                   * a definition used (the street's marks: green where
                   * used, red where defined); the others yellow. claude: many, its
                   * file's module named by 30 files, or it used by 10 (the
                   * author, at principia's rio: Window, its file named by
                   * 23, used by 15, was yellow) *)
                  let colour = if fan path >= 30 || many then rgb 250 80 70 else if reaching then rgb 90 220 120 else yellow in
                  let dot = circle colour (size /. 3.) |> move (sx a px) (sy a py) in
                  let ring = circle black ((size /. 3.) +. 2.) |> move (sx a px) (sy a py) in
                  let text = words colour label |> scale (size /. words_font_size) |> move (sx a (px +. 10. +. (tw /. 2.))) (sy a py) in
                  let shadow = words black label |> scale (size /. words_font_size) |> move (sx a (px +. 11.5 +. (tw /. 2.))) (sy a (py +. 1.5)) |> fade 0.8 in
                  (* claude: how central, in the card (the author: "give an idea
                   * of how often it is used"): its module named by N files;
                   * its own uses *)
                  let used = match uses with Some (u, _) when u.others > 0 -> [ Printf.sprintf "used %d times in %d other files" u.others u.files ] | _ -> [] in
                  let reaches = match uses with Some (_, r) when r > 0 -> [ Printf.sprintf "reaches %d files through its calls" r ] | _ -> [] in
                  let central = match fan path with 0 -> [] | n -> [ Printf.sprintf "its module named by %d files%s" n (if n >= 30 then ": the core" else "") ] in
                  let said = [ "* " ^ label ^ "   " ^ path ] @ (match it.say with Some s -> wrap 48 s | None -> []) @ central @ used @ reaches @ [ "click: to its file" ] in
                  Some { node = i; nbox = (x0, py -. (size /. 2.) -. 2., x1, py +. (size /. 2.) +. 2.); nrank = (if many || reaching || it.weight = 0 then 850. else 805. +. float_of_int (min 14 (fan path / 10))); draw = group [ ring; dot; shadow; text ]; said = Some said; sect = None; cap = Some (path, line, label) }
              | None -> None)
          | _ -> None)
      | _ -> None)
    chosen

(* claude: opti: the chosen kept while the layout, the configs and the
 * unit looked at are the same, not chosen again every frame: sorting
 * and hashing principia's 2,200 files kept the map at 60% of a CPU with
 * nothing happening *)
let capitals_cache : (entry Treemap.placed array * Code_guide.t * int * bool * ((string, int * entry) Hashtbl.t * (string * Code_guide.item) list)) option ref = ref None

let capitals_chosen_opti (t : t) : (string, int * entry) Hashtbl.t * (string * Code_guide.item) list =
  match !capitals_cache with
  | Some (placed, guide, focus, counted, r) when placed == t.placed && guide == t.guide && focus = t.focus && counted = (rank_if_counted t <> None) -> r
  | _ ->
      let r = capitals_chosen t in
      capitals_cache := Some (t.placed, t.guide, t.focus, rank_if_counted t <> None, r);
      r

let capitals (t : t) (c : camera) : name list =
  let where, chosen = if !Opti.enabled then capitals_chosen_opti t else capitals_chosen t in
  capitals_drawn t c where chosen

(* the names over the map, the directories' first: a directory's centred
 * on it, as large as it fits (a region's up to 64, deeper ones smaller),
 * standing up when its block is tall and narrow; a file's smaller. Then
 * placed greedily, the most important first, none over another
 * (Code_map_base.place's way, the boxes kept for the mouse) *)
let names (t : t) (c : camera) : name list =
  let a = c.a in
  (* claude: the unit looked at and those holding it: named on the
   * breadcrumb, not over the map they fill (Code_units) *)
  let above = Code_units.ancestors t.placed t.focus in
  let crumbs =
    if t.focus = 0 then []
    else
      let x = ref 6. in
      List.mapi
        (fun k i ->
          let p = t.placed.(i) in
          let name = if i = 0 then (match String.index_opt t.title ':' with Some j -> String.sub t.title 0 j | None -> t.title) else (match p.node with Dir (n, _) | File (n, _, _) -> n) in
          let text = if k = 0 then name else "> " ^ name in
          let box, shape = tab a ~alpha:0.9 (lighter (archi t.colours p.path)) 16. !x 6. text in
          let _, _, x1, _ = box in
          x := x1 +. 4.;
          { node = i; nbox = box; nrank = 10000.; draw = shape; said = None; sect = None; cap = None })
        above
  in
  (* at the ground, the file is the map: only the breadcrumb *)
  let ground = Map_paint.at_ground t c <> None in
  let cands = ref (crumbs @ if ground then [] else capitals t c) in
  Array.iteri
    (fun i (p : entry Treemap.placed) ->
      match clip c p.rect with
      | Some (x0, y0, x1, y1) when (not ground) && p.depth > 0 && (not (List.mem i above)) && not (Map_paint.outside t p && (match p.node with File _ -> true | Dir _ -> false)) ->
          let w = float_of_int (x1 - x0) and h = float_of_int (y1 - y0) in
          let cx = (float_of_int x0 +. float_of_int x1) /. 2. and cy = (float_of_int y0 +. float_of_int y1) /. 2. in
          let is_dir, name = match p.node with Dir (n, _) -> (true, n) | File (n, _, _) -> (false, n) in
          (* claude: a directory looked at, a file big enough on the map:
           * its name on a tab at its top, its card (what the configs say
           * of it), its table of contents (its sections where they are) --
           * what tells files apart, readable *)
          let card_file =
            match p.node with
            | File (_, _, e) when (match t.placed.(t.focus).node with Dir _ -> true | File _ -> false) && w >= 105. && h >= 70. && not (Map_paint.outside t p) -> Some e
            | _ -> None
          in
          (match card_file with
          | Some e ->
              let col = archi t.colours p.path in
              let fx0 = float_of_int x0 and fy0 = float_of_int y0 in
              let box, shape = tab a (lighter col) 15. (fx0 +. 3.) (fy0 +. 3.) name in
              cands := { node = i; nbox = box; nrank = 700.; draw = shape; said = None; sect = None; cap = None } :: !cands;
              (* the card, under the tab, wrapped to the block *)
              (match Option.bind (Code_guide.file_note t.guide e.path) (fun n -> n.summary) with
              | Some said ->
                  let size = 13. in
                  let chars = max 12 (int_of_float ((w -. 16.) /. (0.5 *. size))) in
                  (* a narrow block, a line more *)
                  let lines = List.filteri (fun k _ -> k < if w < 180. then 4 else 3) (wrap chars said) in
                  let n = float_of_int (List.length lines) in
                  let top = fy0 +. 28. in
                  let lw = List.fold_left (fun m l -> Float.max m (0.5 *. size *. float_of_int (String.length l))) 0. lines in
                  let bh = n *. (size +. 3.) in
                  let draw =
                    group
                      ((rectangle (rgb 16 14 32) (lw +. 10.) (bh +. 6.) |> move (sx a (fx0 +. 6. +. (lw /. 2.))) (sy a (top +. (bh /. 2.))) |> fade 0.85)
                      :: List.mapi
                           (fun k l ->
                             let tw = 0.5 *. size *. float_of_int (String.length l) in
                             words ink l |> scale (size /. words_font_size) |> move (sx a (fx0 +. 8. +. (tw /. 2.))) (sy a (top +. ((float_of_int k +. 0.5) *. (size +. 3.)))))
                           lines)
                  in
                  cands := { node = i; nbox = (fx0 +. 4., top -. 3., fx0 +. 12. +. lw, top +. bh +. 3.); nrank = 820.; draw; said = None; sect = None; cap = None } :: !cands
              | None -> ());
              (* the sections, each where it is in the columns *)
              (match t.geometry.(i) with
              | Some g ->
                  let f = Lazy.force e.file in
                  let r, gg, b = Highlight_code.rgb Comment_section in
                  List.iter
                    (fun (l, title, (cat : Highlight_code.category)) ->
                      let telling = String.length title < 36 && title <> "" && title.[0] <> '-' && title.[0] <> '*' && not (String.contains title '/') in
                      if cat = Comment_section && l > 0 && telling then begin
                        let lx, ly = line_pos p.rect g l in
                        let px = to_px c lx +. 4. and py = to_py c ly in
                        let size = 12. in
                        let tw = 0.5 *. size *. float_of_int (String.length title + 2) in
                        let text = "* " ^ title in
                        let draw =
                          group
                            [
                              rectangle (rgb 16 14 32) (tw +. 6.) (size +. 4.) |> move (sx a (px +. (tw /. 2.))) (sy a py) |> fade 0.8;
                              words (rgb r gg b) text |> scale (size /. words_font_size) |> move (sx a (px +. (tw /. 2.) +. 2.)) (sy a py);
                            ]
                        in
                        if px +. tw < float_of_int x1 then cands := { node = i; nbox = (px, py -. (size /. 2.) -. 2., px +. tw +. 6., py +. (size /. 2.) +. 2.); nrank = 400.; draw; said = None; sect = Some (e.path, l); cap = None } :: !cands
                      end)
                    f.defs
              | None -> ())
          | None ->
          let len = float_of_int (max 1 (String.length name)) in
          let cap = if not is_dir then 16. else match p.depth with 1 -> 64. | 2 -> 36. | _ -> 24. in
          let across = Float.min (w /. (0.55 *. len)) (Float.min (h /. 2.5) cap) in
          let upright = Float.min (h /. (0.55 *. len)) (Float.min (w /. 2.5) cap) in
          let stand = is_dir && upright > across *. 1.3 in
          let size = if stand then upright else across in
          let least = if is_dir then 11. else 10. in
          if size >= least then begin
            let tw = 0.5 *. size *. len in
            let bw, bh = if stand then (size, tw) else (tw, size) in
            let colour = if is_dir then (if p.depth = 1 then mix (archi t.colours p.path) 0.3 (245, 245, 250) else mix (archi t.colours p.path) 0.55 (235, 235, 245)) else archi t.colours p.path in
            let r, g, b = colour in
            let text dx dy col alpha = words col name |> scale (size /. words_font_size) |> (if stand then rotate 90. else Fun.id) |> move (sx a (cx +. dx)) (sy a (cy +. dy)) |> fade alpha in
            let draw =
              if is_dir then (if Map_paint.outside t p then text 0. 0. (rgb r g b) 0.4 else group [ text 2. 2. black 0.75; text 0. 0. (rgb r g b) 1. ])
              else text 0. 0. (lighter (r, g, b)) 0.85
            in
            let nrank = if is_dir then 1000. -. (100. *. float_of_int p.depth) +. size else size in
            (* claude: a folder laid out alone: its own name is the title's,
             * and drawn large at its centre it hid its subfolders (the
             * author, at launcher: codemap) *)
            if not (t.top_kept && is_dir && p.depth = 1 && t.focus = 0) then
            cands := { node = i; nbox = (cx -. (bw /. 2.), cy -. (bh /. 2.), cx +. (bw /. 2.), cy +. (bh /. 2.)); nrank; draw; said = None; sect = None; cap = None } :: !cands
          end)
      | _ -> ())
    t.placed;
  let overlaps (a0, b0, a1, b1) (c0, d0, c1, d1) = a0 < c1 && c0 < a1 && b0 < d1 && d0 < b1 in
  let on_map (x0, y0, x1, y1) = x0 >= 0. && y0 >= 0. && x1 <= float_of_int a.pw && y1 <= float_of_int a.ph in
  List.fold_left
    (fun kept n ->
      let free box = on_map box && not (List.exists (fun k -> overlaps box k.nbox) kept) in
      if free n.nbox then n :: kept
      (* claude: and a capital much used or reaching far, or the entry
       * point (850: over a file's card, 820, the author: Window hidden
       * under dat.h's) *)
      else if n.said <> None && ((n.nrank >= 815. && n.nrank < 820.) || n.nrank = 850.) then begin
        (* claude: a hub's capital (fan-in 100 and more: ranked 815 and up)
         * that collides is nudged up or down a line or two, the core's
         * names shown near their place rather than not at all *)
        let x0, y0, x1, y1 = n.nbox in
        let h = y1 -. y0 in
        match List.find_opt (fun dy -> free (x0, y0 +. dy, x1, y1 +. dy)) [ h; -.h; 2. *. h; -2. *. h; 3. *. h; -3. *. h ] with
        | Some dy -> { n with nbox = (x0, y0 +. dy, x1, y1 +. dy); draw = n.draw |> move 0. (-.dy) } :: kept
        | None -> kept
      end
      else kept)
    []
    (List.stable_sort (fun a b -> compare b.nrank a.nrank) !cands)

let within (x0, y0, x1, y1) x y = x >= x0 && x < x1 && y >= y0 && y < y1

let unit_at (t : t) (c : camera) (_ : float) (px : float) (py : float) : int option =
  (* a section's title is not a unit: a click on it peeks (pick) *)
  Option.map (fun n -> n.node) (List.find_opt (fun n -> n.sect = None && within n.nbox px py) (names t c))
