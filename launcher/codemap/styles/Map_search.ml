(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Map_search.mli *)

open Playground
open Code_map_base

(*****************************************************************************)
(* The search *)
(*****************************************************************************)

(* claude: the search (/): what it looks among, the map's directories, files
 * and top-level definitions (every file lexed, once, the first query); its
 * hits, among the files shown only when [here] (the unit looked at, and
 * at the street its panels) *)
(* claude: the definitions that are types, for the search's type: *)
let types : (string * int, unit) Hashtbl.t = Hashtbl.create 256

let search_all (t : t) : Code_search.hit array =
  match t.search_all with
  | Some a -> a
  | None ->
      let dirs = Array.to_list t.placed |> List.filter_map (fun (p : entry Treemap.placed) -> match p.node with Dir _ when p.path <> "" -> Some p.path | _ -> None) in
      (* claude: the sources beyond the map too (a program's map: the rest
       * of the repository), a hit there peeked at *)
      let all_entries = t.entries @ t.beyond in
      let files = List.map (fun (e : entry) -> e.path) all_entries in
      let defs =
        List.concat_map
          (fun (e : entry) ->
            List.filter_map
              (fun (l, n, (cat : Highlight_code.category)) ->
                match cat with
                | Def_function | Def_value | Def_module -> Some (e.path, l, n)
                | Def_type -> Hashtbl.replace types (e.path, l) (); Some (e.path, l, n)
                | _ -> None)
              (Lazy.force e.file).defs)
          all_entries
      in
      let views = List.map (fun (v : Code_guide.view) -> v.vname) (Code_guide.views t.guide) in
      let tours = List.map (fun (tr : Code_guide.tour) -> tr.name) (Code_guide.tours t.guide) in
      let a = Code_search.candidates ~views ~tours ~dirs ~files ~defs () in
      t.search_all <- Some a;
      a

(* the files shown: under the unit looked at, and the street's panels *)
let shown (t : t) : string -> bool =
  let top = t.placed.(t.focus).path in
  let panels = match Map_paint.at_ground t t.cam with Some e when t.street -> List.map (fun (p : Code_street.panel) -> p.path) (Code_street.panels (Map_paint.street_of t e)) | _ -> [] in
  fun p -> top = "" || p = top || Code_search.starts p (top ^ "/") || List.mem p panels

(* claude: the files' lines as text, whole (the grid cuts them at
 * Code_file.cols), for a text search: its spans put back at their
 * columns *)
let texts : (string, string array) Hashtbl.t = Hashtbl.create 256

let text_of (e : entry) : string array =
  match Hashtbl.find_opt texts e.path with
  | Some a -> a
  | None ->
      let f = Lazy.force e.file in
      let a =
        Array.map
          (fun spans ->
            let b = Buffer.create 80 in
            List.iter (fun (sp : Highlight_code.span) -> while Buffer.length b < sp.col do Buffer.add_char b ' ' done; Buffer.add_string b sp.text) spans;
            Buffer.contents b)
          f.lines
      in
      Hashtbl.replace texts e.path a;
      a

(* a query's hits among the map's files: names, or, a text search, lines *)
(* claude: a query's prefix, as VS Code's (the author): file:, dir:,
 * def:, type:, view:, tour:, text: (the lines' text, as a double
 * quote), ref: (the code's references, as @); none, every kind *)
let prefixes = [ "file:"; "dir:"; "def:"; "type:"; "view:"; "tour:"; "text:"; "ref:"; "bone:" ]

let split_prefix (q : string) : string option * string =
  match List.find_opt (fun p -> Code_search.starts (String.lowercase_ascii q) p) prefixes with
  | Some p -> (Some p, String.sub q (String.length p) (String.length q - String.length p))
  | None -> (None, q)

(* what a query searches, said in the box *)
let query_mode (q : string) : string =
  match split_prefix q with
  | Some "text:", _ -> "the lines' text"
  | Some "ref:", _ -> "the code's references"
  | Some "bone:", _ -> "the skeletons' bones, by role or name"
  | Some p, _ -> String.sub p 0 (String.length p - 1) ^ "s by name"
  | None, _ when Code_search.text_query q <> None -> "the lines' text"
  | None, _ when Code_search.ref_query q <> None -> "the code's references"
  | None, _ -> "names: dirs, files, definitions, views, tours"

let query_hits (t : t) (q : string) : Code_search.hit list =
  let every = t.entries @ t.beyond in
  let text_search text = Code_search.text_matches (List.map (fun (e : entry) -> (e.path, text_of e)) every) text in
  let ref_search name =
    Code_search.ref_matches
      (List.map
         (fun (e : entry) ->
           let f = Lazy.force e.file in
           let refs = Array.to_list f.refs |> List.concat_map (List.map (fun (r : Highlight_code.reference) -> (r.rline, String.concat "." (r.rpath @ [ r.rname ])))) in
           (e.path, refs, text_of e))
         every)
      name
  in
  let names q =
    let top = t.placed.(t.focus).path in
    let near p = top <> "" && (p = top || Code_search.starts p (top ^ "/")) in
    Code_search.matches ~near (search_all t) q
  in
  match split_prefix q with
  | Some "text:", rest -> if String.length rest >= 2 then text_search rest else []
  | Some "ref:", rest -> if String.length rest >= 2 then ref_search rest else []
  (* claude: bone:, every skeleton's bones, by their role or their name
   * (the author) *)
  | Some "bone:", rest ->
      let q = String.lowercase_ascii (String.trim rest) in
      let has s = q = "" || Code_search.contains (String.lowercase_ascii s) q in
      List.concat_map (fun (d : Code_guide.dir_note) -> d.skeletons) (Code_guide.dirs t.guide)
      |> List.concat_map (fun (sk : Code_guide.skeleton) -> sk.bones)
      |> List.filter (fun (b : Code_guide.bone) -> has b.role || has b.banchor)
      |> List.filter_map (fun (b : Code_guide.bone) ->
             if b.banchor = "" then Some ({ kind = (if List.exists (fun (e : entry) -> e.path = b.bpath) every then File else Dir); path = b.bpath; line = 0; name = b.role } : Code_search.hit)
             else Option.map (fun line : Code_search.hit -> { kind = Def; path = b.bpath; line; name = b.role }) (match Map_skeleton.entry_of t b.bpath with Some e -> Map_names.capital_line e b.banchor | None -> None))
      |> List.sort_uniq compare
      (* the role said exactly first, then starting so, then containing *)
      |> List.stable_sort (fun (x : Code_search.hit) (y : Code_search.hit) ->
             let rank (h : Code_search.hit) = let r = String.lowercase_ascii h.name in if r = q then 0 else if Code_search.starts r q then 1 else 2 in
             compare (rank x) (rank y))
  (* claude: view: or tour: alone, all of them, to explore (the author) *)
  | Some (("view:" | "tour:") as p), rest when String.trim rest = "" ->
      Array.to_list (search_all t) |> List.filter (fun (h : Code_search.hit) -> h.kind = (if p = "view:" then View else Tour))
  | Some p, rest ->
      let keep (h : Code_search.hit) =
        match (p, h.kind) with
        | "file:", File | "dir:", Dir | "view:", View | "tour:", Tour -> true
        | "def:", Def -> not (Hashtbl.mem types (h.path, h.line))
        | "type:", Def -> Hashtbl.mem types (h.path, h.line)
        | _ -> false
      in
      List.filter keep (names rest)
  | None, _ -> (
      match (Code_search.text_query q, Code_search.ref_query q) with
      | Some text, _ -> text_search text
      | None, Some name -> ref_search name
      | None, None -> if String.length q > 0 && (q.[0] = '"' || q.[0] = '@') then [] else names q)

let search_hits (t : t) : Code_search.hit list =
  match t.search with
  | None -> []
  | Some s ->
      if fst s.hits = (s.query, s.here) then snd s.hits
      else
        let hits = query_hits t s.query in
        let hits = if s.here then (let ok = shown t in List.filter (fun (h : Code_search.hit) -> ok h.path) hits) else hits in
        s.hits <- ((s.query, s.here), hits);
        hits

let search_named (t : t) : string list =
  match t.search with Some s -> List.filter (fun p -> (not s.here) || shown t p) (Code_search.all_named (search_all t) s.query) | None -> []

(* all a search found, to see together (shift+Enter): its directories
 * and files, a file under a directory found left out; else, if it found
 * only definitions, their files *)
let search_set (t : t) : string list =
  let hits = search_hits t in
  let units = List.filter_map (fun (h : Code_search.hit) -> match h.kind with Dir | File -> Some h.path | Def | Text | View | Tour -> None) hits in
  let hits = List.filter (fun (h : Code_search.hit) -> h.kind <> View && h.kind <> Tour) hits in
  let paths = if units <> [] then units else List.map (fun (h : Code_search.hit) -> h.path) hits in
  let paths = List.sort_uniq compare paths in
  List.filter (fun p -> not (List.exists (fun d -> d <> p && Code_search.starts p (d ^ "/")) paths)) paths

(* the hits lit where they are on the map, at any level: a directory or
 * file framed, a definition's line marked (a bar at the ground, a dot
 * above it), the chosen one brighter and named *)
let search_lit ?(glow = rgb 255 225 90) ?(dot = 3.) ?(chosen : Code_search.hit option) (t : t) (c : camera) (hits : Code_search.hit list) : shape list =
  let a = c.a in
  let ground = Map_paint.at_ground t c <> None in
  let index = Hashtbl.create 64 in
  Array.iteri (fun i (p : entry Treemap.placed) -> Hashtbl.replace index p.path i) t.placed;
  List.concat
    (List.filteri (fun i _ -> i < 3000) hits
    |> List.map (fun (h : Code_search.hit) ->
           let sel = chosen == Some h || chosen = Some h in
           match h.kind with
           | Dir | File -> (
               match Hashtbl.find_opt index h.path with
               | Some i when i <> t.focus && not ground -> (
                   match clip c t.placed.(i).rect with
                   | Some (x0, y0, x1, y1) when x1 - x0 >= 2 && y1 - y0 >= 2 ->
                       let x0 = float_of_int x0 and y0 = float_of_int y0 and x1 = float_of_int x1 and y1 = float_of_int y1 in
                       [ rectangle glow (x1 -. x0) (y1 -. y0) |> move (sx a ((x0 +. x1) /. 2.)) (sy a ((y0 +. y1) /. 2.)) |> fade (if sel then 0.3 else 0.14) ]
                       @ frame a glow x0 y0 x1 y1 (if sel then 3. else 1.5)
                   | _ -> [])
               | _ -> [])
           | Def | Text -> (
               match Map_skeleton.spot t c h.path h.line with
               | Some (x, y, xe) when ground ->
                   let w = Float.max 30. (xe -. x) in
                   [ rectangle glow (w +. 8.) 12. |> move (sx a (x +. (w /. 2.))) (sy a y) |> fade (if sel then 0.55 else 0.3) ]
               (* claude: a halo round each, glowing slowly: a match, not a
                * capital (the author) *)
               | Some (x, y, _) ->
                   let pulse = 0.5 +. (0.5 *. Float.sin ((t.clock *. 3.) +. (float_of_int (h.line mod 7)))) in
                   [ circle glow ((dot *. 2.4) +. (2. *. pulse)) |> move (sx a x) (sy a y) |> fade (0.12 +. (0.12 *. pulse));
                     circle glow (if sel then 6. else dot) |> move (sx a x) (sy a y) |> fade (if sel then 1. else 0.85) ]
               | None -> [])
           | View | Tour -> []))

(* the box, under the title: the query typed, where it looks, the best
 * hits, the chosen one lit, and what the keys do *)
let search_box (t : t) (c : camera) (s : search) (hits : Code_search.hit list) : shape list =
  let a = c.a in
  let w = Float.min 760. (float_of_int a.pw -. 40.) in
  let x0 = (float_of_int a.pw -. w) /. 2. and y0 = 12. in
  (* claude: eight at a time, the chosen one among them (up and down
   * scroll the list) *)
  let first = max 0 (s.sel - 7) in
  let shown_hits = List.filteri (fun i _ -> i >= first && i < first + 8) hits in
  let row = 24. in
  let named = search_named t in
  let h = 50. +. (row *. float_of_int (max 1 (List.length shown_hits))) +. 30. in
  let left size col x y str = label a col size (x +. (text_width size str /. 2.)) y str in
  let where0 = if s.here then (match t.placed.(t.focus).path with "" -> "the files shown" | p -> "in " ^ p ^ (match t.placed.(t.focus).node with Dir _ -> "/" | File _ -> "")) else "everywhere" in
  let caret = if Float.rem t.clock 1. < 0.5 then "|" else " " in
  [ rectangle (rgb 16 14 34) w h |> move (sx a (x0 +. (w /. 2.))) (sy a (y0 +. (h /. 2.))) |> fade 0.97 ]
  @ frame a yellow x0 y0 (x0 +. w) (y0 +. h) 2.
  @ [ left 20. yellow (x0 +. 14.) (y0 +. 22.) ("/ " ^ s.query ^ caret) ]
  @ [ left 13. dim (x0 +. w -. 14. -. text_width 13. (query_mode s.query ^ ", " ^ where0 ^ "   " ^ string_of_int (List.length hits) ^ " found")) (y0 +. 22.) (query_mode s.query ^ ", " ^ where0 ^ "   " ^ string_of_int (List.length hits) ^ " found") ]
  @ (if s.query = "" then [ left 15. dim (x0 +. 20.) (y0 +. 50. +. (row /. 2.)) "a name, or a part of it; or first file: dir: def: type: view: tour: text: ref: bone:" ]
     else if hits = [] then [ left 15. dim (x0 +. 20.) (y0 +. 50. +. (row /. 2.)) "nothing of that name" ]
     else [])
  @ List.concat
      (List.mapi
         (fun i0 (hit : Code_search.hit) ->
           let i = i0 + first in
           let y = y0 +. 44. +. (float_of_int i0 *. row) +. (row /. 2.) in
           let kind = match hit.kind with Dir -> "dir" | File -> "file" | Def -> if Hashtbl.mem types (hit.path, hit.line) then "type" else "def" | Text -> "line" | View -> "view" | Tour -> "tour" in
           let cut n str = if String.length str > n then String.sub str 0 n ^ "..." else str in
           let name = match hit.kind with Dir -> hit.name ^ "/" | Text -> Printf.sprintf "%s:%d" (Code_search.basename hit.path) (hit.line + 1) | _ -> hit.name in
           let where =
             match hit.kind with
             | Def -> Printf.sprintf "%s:%d" hit.path (hit.line + 1)
             | Text -> cut 70 hit.name
             | View -> ( match List.nth_opt (Code_guide.views t.guide) hit.line with Some v -> (match v.of_ with Some o -> o ^ " and " ^ Option.value v.with_ ~default:"users" | None -> String.concat ", " v.files) | None -> "")
             | Tour -> ( match List.nth_opt (Code_guide.tours t.guide) hit.line with Some tr -> Printf.sprintf "%d stops, n next, p back" (List.length tr.stops) | None -> "")
             | Dir | File -> hit.path
           in
           let where = cut 80 where in
           let col = lighter (archi t.colours hit.path) in
           (if i = s.sel then [ rectangle (rgb 60 56 110) (w -. 12.) row |> move (sx a (x0 +. (w /. 2.))) (sy a y) ] else [])
           @ [ left 12. dim (x0 +. 16.) y kind; left 16. (if i = s.sel then yellow else col) (x0 +. 56.) y name; left 13. dim (x0 +. 70. +. text_width 16. name) y where ])
         shown_hits)
  @ [
      left 12. dim (x0 +. 14.) (y0 +. h -. 14.)
        (match named with
        | _ :: _ :: _ -> Printf.sprintf "Enter: the %d directories named so, together   Esc close" (List.length named)
        | _ when hits <> [] ->
            let n = List.length (search_set t) in
            if n = 0 then "Enter go   Tab complete   up/down choose   / first: here or all   Esc close" else
            Printf.sprintf "Enter go   shift+Enter the %d %s together   ctrl+Enter a mark   Tab complete   \"text   / first: here or all" n
              (if List.exists (fun (h : Code_search.hit) -> h.kind = Dir || h.kind = File) hits then "found" else "files of these")
        | _ -> "a name, \"text, @reference, name// directories so named   Tab complete   up/down choose   / first: here or all   Esc close");
    ]

(* claude: the marks (plan_codemap_v2.md): searches kept, ctrl+Enter,
 * each lit in its colour at any level, all at once (the author:
 * "Cap.fork, Cap.exec ... a mark with different color scheme for each
 * and get all the capabilities highlighted at the same time"); their
 * legend in the map's bottom left corner *)
let mark_colours = [ (245, 85, 85); (80, 205, 245); (120, 230, 110); (250, 165, 50); (225, 115, 235); (245, 240, 95); (110, 130, 255) ]

let mark_hits (t : t) (l : mark) : Code_search.hit list =
  match l.mhits with
  | Some h -> h
  | None ->
      let h = query_hits t l.mquery in
      l.mhits <- Some h;
      h

(* the groups of marks: those kept, then each config's, their names *)
let mark_groups (t : t) : (string * mark list) list =
  let guide =
    match t.guide_marks with
    | Some g -> g
    | None ->
        let g =
          List.map
            (fun (l : Code_guide.mark) ->
              (l.mname, List.map (fun (r : Code_guide.rule) -> { mquery = (if r.is_ref then "@" else "\"") ^ r.text; mcolour = r.colour; msay = r.rsay; mhits = None }) l.rules))
            (Code_guide.marks t.guide)
        in
        (* claude: the X-ray's nerves and lungs as marks too, derived
         * from the configs' anatomy rules (the words the X-ray guesses
         * from without them), a colour a rule (the author: "see all the
         * code doing io or using mouse or keyboard") *)
        let derived name (pick : Code_guide.dir_note -> Code_guide.rule list) words =
          let rules = List.concat_map pick (Code_guide.dirs t.guide) in
          let queries =
            if rules = [] then List.map (fun w -> ("\"" ^ w, None)) words
            else List.map (fun (r : Code_guide.rule) -> ((if r.is_ref then "@" else "\"") ^ r.text, r.rsay)) rules
          in
          let queries = List.fold_left (fun acc ((q, _) as x) -> if List.mem_assoc q acc then acc else acc @ [ x ]) [] queries in
          let n = List.length mark_colours in
          (name, List.mapi (fun i (q, say) -> { mquery = q; mcolour = List.nth mark_colours (i mod n); msay = say; mhits = None }) queries)
        in
        let g =
          g
          @ [
              derived "nerves: the inputs (the X-ray's 3)" (fun d -> d.nerves) Code_anatomy.nerve_words;
              derived "lungs: the I/O (the X-ray's 4)" (fun d -> d.lungs) Code_anatomy.lung_words;
            ]
        in
        t.guide_marks <- Some g;
        g
  in
  ("kept", t.marks) :: guide

let marks_shapes (t : t) (c : camera) : shape list =
  (* claude: -1, none (List.nth_opt raises on it) *)
  match if t.mark_group < 0 then None else List.nth_opt (mark_groups t) t.mark_group with
  | None | Some (_, []) -> []
  | Some (group, marks) ->
    let a = c.a in
    let lit = List.concat_map (fun (l : mark) -> let r, g, b = l.mcolour in search_lit ~glow:(rgb r g b) ~dot:4.5 t c (mark_hits t l)) marks in
    let row = 20. in
    let line (l : mark) =
      let q = if String.length l.mquery > 0 && (l.mquery.[0] = '"' || l.mquery.[0] = '@') then String.sub l.mquery 1 (String.length l.mquery - 1) else l.mquery in
      Printf.sprintf "%s  %d%s" q (List.length (mark_hits t l)) (match l.msay with Some s -> "   " ^ s | None -> "")
    in
    let head = Printf.sprintf "%s   (m: next)" (if group = "kept" then "marks kept" else group) in
    let n = List.length marks in
    let w = 30. +. List.fold_left (fun m l -> Float.max m (text_width 14. (line l))) (text_width 14. head) marks in
    let h = 12. +. (row *. float_of_int (n + 1)) in
    let x0 = 10. and y0 = float_of_int a.ph -. h -. 10. in
    lit
    @ [ rectangle (rgb 16 14 34) w h |> move (sx a (x0 +. (w /. 2.))) (sy a (y0 +. (h /. 2.))) |> fade 0.9 ]
    @ [ label a yellow 14. (x0 +. 12. +. (text_width 14. head /. 2.)) (y0 +. 6. +. (row /. 2.)) head ]
    @ List.concat
        (List.mapi
           (fun i (l : mark) ->
             let r, g, b = l.mcolour in
             let y = y0 +. 6. +. (float_of_int (i + 1) *. row) +. (row /. 2.) in
             let str = line l in
             [ circle (rgb r g b) 5. |> move (sx a (x0 +. 12.)) (sy a y); label a ink 14. (x0 +. 22. +. (text_width 14. str /. 2.)) y str ])
           marks)
