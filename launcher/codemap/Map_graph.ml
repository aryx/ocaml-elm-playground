(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Map_graph.mli *)

open Playground
open Code_map_base

type t = { map : Code_map_base.t; dsm : Code_dsm.t; history : (Code_dsm.t * string) list; title : string }
type action = Stay | Back | Go of string * int option

(*****************************************************************************)
(* The data, from the map *)
(*****************************************************************************)

let edges_cache : (string, Code_dsm.edge list) Hashtbl.t = Hashtbl.create 256

let make ?expand (map : Code_map_base.t) ~(title : string) (units : string list) : t =
  let every = map.entries @ map.beyond in
  let file p = List.find_map (fun (e : entry) -> if e.path = p then Some (Lazy.force e.file) else None) every in
  let tops (f : Code_file.t) =
    List.filter_map (fun (l, n, (c : Highlight_code.category)) -> match c with Def_function | Def_value | Def_type | Def_module -> Some (l, n) | _ -> None) f.defs
    |> List.sort_uniq compare
  in
  let defs p = match file p with Some f -> tops f | None -> [] in
  let edges p =
    match Hashtbl.find_opt edges_cache p with
    | Some e -> e
    | None ->
        let e =
          match file p with
          | None -> []
          | Some f ->
              let ts = List.map fst (tops f) in
              let enclosing line = List.fold_left (fun acc l -> if l <= line then Some l else acc) None ts in
              let across =
                Code_street.uses ~index:(index_of map) ~roots:map.roots ~path:p f
                |> List.map (fun (e : Code_street.edge) -> ({ sdef = enclosing e.from_line; dst = e.target; ddef = e.target_line; ename = e.name } : Code_dsm.edge))
              in
              (* claude: and within the file: each top-level definition's
               * uses (its binding's occurrences) from the others' bodies
               * (the author: "the intrafile deps between entities") *)
              let within =
                List.concat_map
                  (fun (l, name) ->
                    let own =
                      if l < Array.length f.names then
                        List.find_opt (fun (o : Highlight_code.occurrence) -> o.bound_at = (o.line, o.col) && o.len = String.length name) f.names.(l)
                      else None
                    in
                    match own with
                    | None -> []
                    | Some o ->
                        Code_file.uses f o
                        |> List.filter_map (fun (u : Highlight_code.occurrence) ->
                               match enclosing u.line with
                               | Some d when d <> l -> Some ({ sdef = Some d; dst = p; ddef = l; ename = name } : Code_dsm.edge)
                               | _ -> None))
                  (tops f)
              in
              across @ within
        in
        Hashtbl.replace edges_cache p e;
        e
  in
  let data : Code_dsm.data = { files = List.map (fun (e : entry) -> e.path) every; links = Code_rank.links (rank_of map); defs; edges } in
  let dsm = Code_dsm.make data units in
  (* claude: a unit shown open at once (the street's file) *)
  let dsm = match expand with Some p when List.length units > 1 -> Code_dsm.toggle dsm (if List.mem p data.files then File p else Dir p) | _ -> dsm in
  { map; dsm; history = []; title }

(*****************************************************************************)
(* Layout *)
(*****************************************************************************)

type layout = { n : int; cell : float; lx : float; mx0 : float; my0 : float; label_w : float }

let layout (g : t) : layout =
  let a = g.map.cam.a in
  let rows = Code_dsm.rows g.dsm in
  let n = max 1 (List.length rows) in
  let maxd = List.fold_left (fun m (r : Code_dsm.row) -> max m r.depth) 0 rows in
  let lx = 8. +. (16. *. float_of_int maxd) in
  let label_w = Float.min 380. (0.3 *. float_of_int a.pw) in
  (* claude: room above for the columns' names, written slanted (the
   * author: "the human brain does not remember the mapping number to
   * entity") *)
  let longest = List.fold_left (fun m (r : Code_dsm.row) -> Float.max m (text_width 13. (Code_dsm.name_of r.node))) 0. rows in
  let my0 = 36. +. Float.min 190. (0.72 *. longest) in
  let avail_h = float_of_int a.ph -. my0 -. 8. and avail_w = float_of_int a.pw -. lx -. label_w -. 12. in
  let cell = Float.min 44. (Float.min avail_h avail_w /. float_of_int n) in
  { n; cell; lx; mx0 = lx +. label_w; my0; label_w }

(* the row or cell under a pixel *)
type spot = Label of int | Cell of int * int | Nowhere

let spot_at (l : layout) (px : float) (py : float) : spot =
  let i = int_of_float (Float.floor ((py -. l.my0) /. l.cell)) in
  if i < 0 || i >= l.n || py < l.my0 then Nowhere
  else if px >= l.lx && px < l.mx0 then Label i
  else
    let j = int_of_float (Float.floor ((px -. l.mx0) /. l.cell)) in
    if px >= l.mx0 && j >= 0 && j < l.n then Cell (i, j) else Nowhere

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let m_ij (g : t) i j = (Code_dsm.matrix g.dsm).(i).(j)

let update (computer : computer) ~(pressed : string -> bool) (g : t) : t * action =
  let a = g.map.cam.a in
  let mouse = computer.mouse in
  let px = px_of a mouse.mx and py = py_of a mouse.my in
  let l = layout g in
  let rows = Array.of_list (Code_dsm.rows g.dsm) in
  let expand g nodes =
    let dsm = List.fold_left (fun d (r : Code_dsm.row) -> if r.expandable then Code_dsm.toggle d r.node else d) g.dsm nodes in
    if dsm == g.dsm then g else { g with dsm; history = (g.dsm, g.title) :: g.history }
  in
  let go (n : Code_dsm.node) = match n with Def (p, line, _) -> Go (p, Some line) | Dir p | File p -> Go (p, None) in
  if pressed "Escape" then (g, Back)
  else if pressed "Backspace" then match g.history with (d, title) :: rest -> ({ g with dsm = d; title; history = rest }, Stay) | [] -> (g, Back)
  else if mouse.mclick then
    let shift = Set_.mem "Shift" computer.keyboard.keys in
    match spot_at l px py with
    | Label i when shift -> (g, go rows.(i).node)
    | Label i -> ( match rows.(i).node with Def _ -> (g, go rows.(i).node) | _ -> (expand g [ rows.(i) ], Stay))
    | Cell (i, j) when i = j -> (expand g [ rows.(i) ], Stay)
    | Cell (i, j) when m_ij g i j > 0 ->
        let title = Printf.sprintf "%s uses %s: what, part by part" (Code_dsm.path_of rows.(i).node) (Code_dsm.path_of rows.(j).node) in
        ({ g with dsm = Code_dsm.focus g.dsm rows.(i).node rows.(j).node; title; history = (g.dsm, g.title) :: g.history }, Stay)
    | Cell _ -> (g, Stay)
    | Nowhere -> (g, Stay)
  else (g, Stay)

(*****************************************************************************)
(* View *)
(*****************************************************************************)

(* claude: each as the map colours it (the author): a definition as the
 * code's highlighter does its kind (a function yellow, a type green...),
 * a folder or file its region's colour, as at the earth *)
let colour_of (g : t) (n : Code_dsm.node) : color =
  match n with
  | Def (p, line, _) -> (
      let cat =
        List.find_map (fun (e : entry) -> if e.path = p then Some (Lazy.force e.file) else None) (g.map.entries @ g.map.beyond)
        |> Fun.flip Option.bind (fun (f : Code_file.t) -> List.find_map (fun (l, _, c) -> if l = line then Some c else None) f.defs)
      in
      match cat with Some c -> let r, gr, b = Highlight_code.rgb c in rgb r gr b | None -> ink)
  | _ -> lighter (archi g.map.colours (Code_dsm.path_of n))

(* a card of lines beside a pixel, kept on the area *)
let card (a : area) (x : float) (y : float) (title : string) (col : color) (lines : string list) : shape list =
  let lines = List.filteri (fun i _ -> i < 14) lines in
  let w = 24. +. List.fold_left (fun m s -> Float.max m (text_width 14. s)) (text_width 15. title) lines in
  let h = 30. +. (19. *. float_of_int (List.length lines)) in
  let x0 = Float.min (x +. 18.) (float_of_int a.pw -. w -. 4.) and y0 = Float.min (y +. 18.) (float_of_int a.ph -. h -. 4.) in
  [ rectangle (rgb 18 16 36) w h |> move (sx a (x0 +. (w /. 2.))) (sy a (y0 +. (h /. 2.))) |> fade 0.96 ]
  @ frame a col x0 y0 (x0 +. w) (y0 +. h) 1.5
  @ [ label a col 15. (x0 +. 12. +. (text_width 15. title /. 2.)) (y0 +. 14.) title ]
  @ List.mapi (fun k s -> label a ink 14. (x0 +. 12. +. (text_width 14. s /. 2.)) (y0 +. 34. +. (19. *. float_of_int k)) s) lines

(* a definition's first lines, for its card *)
let code_lines (g : t) (p : string) (line : int) : string list =
  match List.find_opt (fun (e : entry) -> e.path = p) (g.map.entries @ g.map.beyond) with
  | None -> []
  | Some e ->
      let f = Lazy.force e.file in
      let text l =
        let b = Buffer.create 80 in
        List.iter (fun (sp : Highlight_code.span) -> while Buffer.length b < sp.col do Buffer.add_char b ' ' done; Buffer.add_string b sp.text) f.lines.(l);
        let s = Buffer.contents b in
        if String.length s > 90 then String.sub s 0 90 ^ "..." else s
      in
      List.init (max 0 (min 12 (Code_file.nlines f - line))) (fun k -> text (line + k))

let view (computer : computer) (g : t) : shape list =
  let a = g.map.cam.a in
  let l = layout g in
  let rows = Array.of_list (Code_dsm.rows g.dsm) in
  let m = Code_dsm.matrix g.dsm in
  let n = Array.length rows in
  let biggest = Array.fold_left (fun acc r -> Array.fold_left max acc r) 1 m in
  let mouse = computer.mouse in
  let px = px_of a mouse.mx and py = py_of a mouse.my in
  let hover = spot_at l px py in
  (* claude: the map's convention (the author): green the user, red the
   * used. A row hovered: what it uses red, its users green; a cell
   * hovered: its row green, its column red *)
  let role k =
    match hover with
    | Label i when k <> i -> if m.(i).(k) > 0 then Some `Used else if m.(k).(i) > 0 then Some `User else None
    | Cell (i, j) when i <> j -> if k = i then Some `User else if k = j then Some `Used else None
    | _ -> None
  in
  let green = rgb 90 220 120 and red = rgb 250 80 70 in
  let named k = match role k with Some `User -> green | Some `Used -> red | None -> colour_of g rows.(k).node in
  let rect x y w h col alpha = rectangle col w h |> move (sx a (x +. (w /. 2.))) (sy a (y +. (h /. 2.))) |> fade alpha in
  let side = l.cell *. float_of_int n in
  let screen = computer.screen in
  let size = Float.max 9. (Float.min 15. (l.cell *. 0.62)) in
  (* the groups: a bar each, its name along it when it fits *)
  let groups =
    List.concat_map
      (fun (node, first, last, depth) ->
        let x = 4. +. (16. *. float_of_int depth) and y = l.my0 +. (float_of_int first *. l.cell) in
        let h = float_of_int (last - first + 1) *. l.cell in
        let col = colour_of g node in
        let name = Code_dsm.name_of node in
        [ rect x y 12. h col 0.35 ]
        @ if text_width 12. name < h -. 6. then [ words col name |> scale (12. /. words_font_size) |> rotate 90. |> move (sx a (x +. 6.)) (sy a (y +. (h /. 2.))) ] else [])
      (Code_dsm.groups g.dsm)
  in
  let labels =
    List.concat
      (List.init n (fun i ->
           let r = rows.(i) in
           let y = l.my0 +. ((float_of_int i +. 0.5) *. l.cell) in
           let str = Printf.sprintf "%d %s%s" (i + 1) (if r.expandable then "+ " else "  ") (Code_dsm.name_of r.node) in
           let str = if text_width size str > l.label_w -. 8. then String.sub str 0 (max 1 (int_of_float ((l.label_w -. 8.) /. (0.55 *. size)))) ^ "..." else str in
           [ label a (named i) size (l.lx +. 4. +. (text_width size str /. 2.)) y str ]))
  in
  let cells =
    List.concat
      (List.init n (fun i ->
           List.concat
             (List.init n (fun j ->
                  let x = l.mx0 +. (float_of_int j *. l.cell) and y = l.my0 +. (float_of_int i *. l.cell) in
                  let v = m.(i).(j) in
                  if i = j then [ rect x y l.cell l.cell (rgb 60 58 80) 0.8 ]
                  else if v = 0 then []
                  else
                    let strength = 0.25 +. (0.75 *. Float.sqrt (float_of_int v /. float_of_int biggest)) in
                    (* below the diagonal, down the layers; above, a cycle *)
                    let col =
                      match hover with
                      | Label h when h = i -> red (* what the hovered uses *)
                      | Label h when h = j -> green (* who uses the hovered *)
                      | _ -> if j < i then rgb 80 150 230 else rgb 220 70 220
                    in
                    [ rect (x +. 1.) (y +. 1.) (l.cell -. 2.) (l.cell -. 2.) col strength ]
                    @ if l.cell >= 18. then [ label a ink (Float.min 13. (l.cell *. 0.45)) (x +. (l.cell /. 2.)) (y +. (l.cell /. 2.)) (string_of_int v) ] else []))))
  in
  let numbers =
    if l.cell < 10. then []
    else
      List.concat
        (List.init n (fun j ->
             let x = l.mx0 +. ((float_of_int j +. 0.5) *. l.cell) in
             let name = Printf.sprintf "%d %s" (j + 1) (Code_dsm.name_of rows.(j).node) in
             let fs = Float.min 13. (Float.max 9. (l.cell *. 0.55)) in
             let w = text_width fs name in
             (* slanted up to the right, starting above its column *)
             let d = w /. 2. /. Float.sqrt 2. in
             [ words (named j) name |> scale (fs /. words_font_size) |> rotate 45. |> move (sx a (x +. d)) (sy a (l.my0 -. 6. -. d)) ]))
  in
  let grid =
    [ rect l.mx0 l.my0 side side (rgb 26 24 44) 1. ]
    @ List.concat (List.init (n + 1) (fun k ->
        let o = float_of_int k *. l.cell in
        [ rect (l.mx0 +. o) l.my0 1. side (rgb 50 48 70) 1.; rect l.mx0 (l.my0 +. o) side 1. (rgb 50 48 70) 1. ]))
  in
  let band i = rect l.lx (l.my0 +. (float_of_int i *. l.cell)) (l.label_w +. side) l.cell (rgb 255 255 255) 0.07 in
  let column j = rect (l.mx0 +. (float_of_int j *. l.cell)) l.my0 l.cell side (rgb 255 255 255) 0.07 in
  let hovered =
    match hover with
    | Label i ->
        let r = rows.(i) in
        let p = Code_dsm.path_of r.node in
        let said =
          match r.node with
          | Def (p, line, _) -> code_lines g p line
          | File p -> (match Code_guide.file_note g.map.guide p with Some { summary = Some s; _ } -> [ s ] | _ -> [])
          | Dir p -> Option.to_list (Code_guide.dir_summary g.map.guide p)
        in
        let uses = Array.fold_left ( + ) 0 m.(i) and used = Array.fold_left (fun acc row -> acc + row.(i)) 0 m in
        let how = match r.node with Def _ -> "click: to the map, its definition" | _ when r.expandable -> "click: its parts   shift+click: to the map" | _ -> "shift+click: to the map" in
        [ band i; column i ]
        @ card a px py p (colour_of g r.node) (said @ [ Printf.sprintf "uses %d here, used %d" uses used; how ])
    | Cell (i, j) when i <> j ->
        let ri = rows.(i) and rj = rows.(j) in
        let why = Code_dsm.explain g.dsm ri.node rj.node |> List.filteri (fun k _ -> k < 12) |> List.map (fun (f, t, k) -> Printf.sprintf "%s -> %s   %d" f t k) in
        let against = if j > i && m.(i).(j) > 0 then [ "above the diagonal: a use against the layers (a cycle)" ] else [] in
        [ band i; column j ]
        @ card a px py
            (Printf.sprintf "%s uses %s: %d" (Code_dsm.name_of ri.node) (Code_dsm.name_of rj.node) m.(i).(j))
            (if j < i then rgb 80 150 230 else rgb 220 70 220)
            (against @ why @ [ "click: this cell alone, its row's parts against its column's" ])
    | _ -> []
  in
  [ rectangle (rgb 12 10 28) screen.width screen.height ]
  @ grid @ cells @ groups @ labels @ numbers @ hovered
  @ [
      words yellow g.title |> scale (22. /. words_font_size) |> move 0. (screen.top -. 45.);
      words dim "codegraph's matrix: row i uses column j; blue below the diagonal (down the layers), magenta above (a cycle); a row hovered: what it uses red, its users green   click a row: its parts   click a cell: zoom into it   shift+click: to the map   backspace: undo   esc: the map"
      |> scale (12. /. words_font_size)
      |> move 0. (screen.bottom +. 18.);
    ]
