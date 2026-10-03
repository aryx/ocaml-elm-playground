(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Code_map.mli.
 *
 * claude: what every style of map shares -- the camera and its moves, the
 * names lit and clicked, the view round the picture, the glass -- the
 * picture and its names being the style's (t.style: Map_classic, ...),
 * drawn with Code_map_base's tools. The layout's two spaces, the
 * layout's and the screen's: Code_map_base.
 *)

open Playground

(* claude: the map's types and tools *)
include Code_map_base

type action = Stay | Open of Code_file.t * int | Close | Select of string * string list | Up | Tied of string * string list * string list | Graph of string * string list

(* claude: the styles, y going from one to the other, one setting for
 * every map (as the glass's), a flag's at the start (style=) *)
let styles = [ Map_classic.style; Map_atlas.style ]
(* claude: v2 the default, everywhere (plan_codemap_v2.md, step 11);
 * the classic behind y, zooming freely, the code painted from afar *)
let chosen = ref Map_atlas.style
let choose_style (name : string) = match List.find_opt (fun s -> s.sname = name) styles with Some s -> chosen := s | None -> ()
let style_name () = !chosen.sname

let cycle_style () =
  let rec next = function s :: (n :: _ as rest) -> if s == !chosen then n else next rest | _ -> List.hd styles in
  chosen := next styles

(* claude: the map laid out again, by another algorithm (t) *)
let laid_out (t : t) (algo : Treemap.algo) : t =
  let placed, geometry = relayout ~top_kept:t.top_kept t.cam.a algo t.entries in
  { t with algo; placed; geometry; painted = None; lens = None; focus = 0 }

(* a map in the chosen style *)
let make ?fan_in ?counted ?top_kept ?numbered ?colours ?roots ?guide ?beyond ?style ~area ~title ~marked entries : t =
  Code_map_base.make ?fan_in ?counted ?top_kept ?numbered ?colours ?roots ?guide ?beyond ~style:(match style with Some s -> s | None -> !chosen) ~area ~title ~marked entries

let street_on (t : t) : bool = t.street

let has (t : t) (path : string) : bool = Array.exists (fun (p : entry Treemap.placed) -> p.path = path) t.placed

(* claude: the map framing a unit by its path (a directory's or a
 * file's), at once *)
let focus_on (t : t) (path : string) : t =
  let found = ref None in
  Array.iteri (fun i (p : entry Treemap.placed) -> if p.path = path then found := Some i) t.placed;
  match !found with
  | Some i ->
      t.focus <- i;
      let c = fit t.target.a t.placed.(i).rect in
      { t with target = c; cam = c }
  | None -> t

let entries (t : t) : entry list = t.entries
let number (t : t) (path : string) : int option = Hashtbl.find_opt t.order path

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let update_map(computer : computer) ~(pressed : string -> bool) ~(arrow : string option) (t : t) : t * action =
  let mouse = computer.mouse in
  let a = t.target.a in
  let mpx = px_of a mouse.mx and mpy = py_of a mouse.my in
  let on_map = on a mpx mpy in
  let target = t.target in
  (* the keys *)
  let pan dx dy = { target with cx = target.cx +. (dx /. target.z); cy = target.cy +. (dy /. target.z) } in
  let target =
    match arrow with
    | _ when t.style.units -> target
    | Some "ArrowLeft" -> pan (-80.) 0.
    | Some "ArrowRight" -> pan 80. 0.
    | Some "ArrowUp" -> pan 0. (-80.)
    | Some "ArrowDown" -> pan 0. 80.
    | _ -> target
  in
  (* claude: the glass's shape, the panel's too *)
  if pressed "o" then Code_map_glass.cycle_glass ();
  (* claude: at the ground, the file with what it uses (Map_atlas) *)
  (* claude: a: what it uses, then what uses it, then both, then off *)
  (* claude: the first press, the mode that fits the file (uses and
   * users, its uses only, its users only: Map_paint.best_street_mode); then
   * the others in turn, then off *)
  if pressed "a" then begin
    let best = Map_paint.best_street_mode t in
    t.street_mode <- (if t.street_mode = 0 then best else let next = (t.street_mode mod 3) + 1 in if next = best then 0 else next);
    t.street <- t.street_mode <> 0;
    t.painted <- None
  end;
  (* claude: the skeletons, at any level (Map_atlas) *)
  (* x: the X-ray on its first skeleton, then the next, then off (Map_atlas
   * turns it off past the last) *)
  if pressed "x" then if t.xray then t.xray_n <- t.xray_n + 1 else begin t.xray <- true; t.xray_n <- 0 end;
  (* claude: in the X-ray, 1 to 6 the anatomy's plates (Code_anatomy) *)
  if t.xray && t.choices = None then List.iter (fun s -> if pressed (Code_anatomy.key s) then Code_anatomy.toggle s) Code_anatomy.all;
  (* claude: m, the marks hidden, shown (Map_atlas) *)
  (* claude: l, the layers, the map coloured by a measure, in turn, then
   * none (Map_layers.layer_shapes); the uses counted first (Code_rank) *)
  if (pressed "l" || pressed "L") && t.search = None then begin
    (* claude: shift+l, back (the author: "cycling can take time"); a
     * browser names it L *)
    let n = Map_layers.layer_count + 1 in
    t.layer <- (if pressed "L" || Set_.mem "Shift" computer.keyboard.keys then t.layer + n - 1 else t.layer + 1) mod n;
    (* the map painted again, its regions grey under a layer *)
    t.painted <- None
  end;
  (* claude: h, every key explained, again to close *)
  if pressed "h" && t.search = None then t.help <- not t.help;
  (* claude: m, the marks lit: those kept (ctrl+Enter), then each
   * config's, then none, in turn (Map_search.mark_groups); l is kept for
   * the layers, a map coloured by a measure (the author) *)
  if pressed "m" then begin
    let groups = Map_search.mark_groups t in
    let n = List.length groups in
    if n > 0 then begin
      (* the next group with marks, or none past the last *)
      let rec next k = if k >= n then -1 else if snd (List.nth groups k) <> [] then k else next (k + 1) in
      t.mark_group <- next (t.mark_group + 1)
    end
  end;
  (* claude: the style, the other one, for this map and those to come
   * (y: m went to the marks, the author: "cycling map styles is not so
   * important") *)
  let before = t.placed in
  let t =
    if pressed "y" then begin
      cycle_style ();
      { t with style = !chosen; painted = None; lens = None }
    end
    else t
  in
  let t =
    if pressed "t" then
      let algo : Treemap.algo = match t.algo with Ordered -> Squarified | Squarified -> Slice_and_dice | Slice_and_dice -> Ordered in
      laid_out t algo
    else t
  in
  (* a new layout: back to the whole map *)
  let units = t.style.units in
  let target = if ((not units) && (pressed "Home" || pressed "0")) || t.placed != before then home a else target in
  if t.placed != before then t.focus <- 0;
  let target = if (not units) && (pressed "Backspace" || (mouse.mrdown && not t.before_right)) then Code_map_moves.up { t with target } else target in
  let target =
    if units then target
    else if pressed "=" || pressed "+" then { target with z = target.z *. 1.5 }
    else if pressed "-" then { target with z = target.z /. 1.5 }
    else target
  in
  (* the wheel: zoom at the mouse, the point under it staying under it *)
  let target =
    if mouse.mwheel <> 0. && on_map && not units then
      let u = to_u target mpx and v = to_v target mpy in
      let z = (Code_map_moves.clamp_cam { target with z = target.z *. (1.25 ** mouse.mwheel) }).z in
      { target with cx = u -. ((mpx -. (float_of_int a.pw /. 2.)) /. z); cy = v -. ((mpy -. (float_of_int a.ph /. 2.)) /. z); z }
    else target
  in
  (* a drag pans, at once *)
  let t, target, cam_now =
    match t.drag with
    | Some (x0, y0, c0) when mouse.mdown ->
        let dx = mouse.mx -. x0 and dy = mouse.my -. y0 in
        let moved = t.dragged || Float.abs dx +. Float.abs dy > 5. in
        let c = if moved then { c0 with cx = c0.cx -. (dx /. c0.z); cy = c0.cy +. (dy /. c0.z) } else target in
        ({ t with dragged = moved }, c, moved)
    | None when mouse.mdown && on_map && not units -> ({ t with drag = Some (mouse.mx, mouse.my, target); dragged = false }, target, false)
    | _ -> (t, target, false)
  in
  let clicked = (mouse.mclick || mouse.mdouble) && on_map && not t.dragged in
  let t = if not mouse.mdown then { t with drag = None; dragged = (if mouse.mclick then false else t.dragged) } else t in
  (* claude: by units, a move taken, the click with it *)
  (* claude: the wheel with a peek open scrolls it *)
  if t.peek <> None && mouse.mwheel <> 0. then t.peek_scroll <- max 0 (t.peek_scroll - int_of_float (Float.round (3. *. mouse.mwheel)));
  (* claude: a click with a peek open: on a name in it, a peek of its
   * definition on top (four deep at most); elsewhere, the top one closed *)
  let clicked =
    if clicked && t.peek <> None then begin
      let file_of p = List.find_map (fun (e : entry) -> if e.path = p then Some (Lazy.force e.file) else None) (t.entries @ t.beyond) in
      (match t.style.pick t t.cam (Playground_platform.pixel_ratio ()) mpx mpy with
      | Some (p, l, col) when col >= 0 && l >= 0 -> (
          match file_of p with
          | Some f -> ( match Code_map_peek.peek_where t f p l col with Some w when List.length t.peek_stack < 3 -> Code_map_peek.open_peek t file_of w | _ -> ())
          | None -> ())
      (* inside the peek, not on a line (its title): nothing *)
      | Some _ -> ()
      | None -> Code_map_peek.close_peek t);
      false
    end
    else clicked
  in
  (* claude: shift+click on a unit's name: a view of it and all it is
   * tied to *)
  (* claude: a click on the X-ray's legend: that plate on or off *)
  let clicked =
    match (clicked && units, Map_anatomy.legend_row_at t t.cam mpx mpy) with
    | true, Some s -> Code_anatomy.toggle s; t.painted <- None; false
    | _ -> clicked
  in
  let with_ties = if clicked && units && Set_.mem "Shift" computer.keyboard.keys then Map_cards.unit_with_ties t t.cam else None in
  (* claude: ctrl+click on a unit's name, or g over it: its ties in
   * codegraph's matrix (Map_graph) *)
  let to_graph =
    if units && ((clicked && Set_.mem "Control" computer.keyboard.keys) || (pressed "g" && t.search = None)) then Map_cards.unit_with_ties t t.cam else None
  in
  let with_ties = if to_graph <> None then None else with_ties in
  let clicked = if with_ties <> None then false else clicked in
  (* claude: a click on a match (a search's, a mark's): its file, and
   * its definition peeked at, as Enter in the search *)
  let jumped_to, clicked =
    if clicked && units then
      match (Map_atlas.street_title_at t t.cam mpx mpy, Map_atlas.hovered_bone t t.cam, Map_atlas.hovered_match t t.cam) with
      (* claude: a street panel's name clicked: to that file, its street *)
      | Some path, _, _ -> (Code_map_moves.search_go t { kind = File; path; line = 0; name = path }, false)
      | None, bone, found -> (
      match (bone, found) with
      (* claude: a bone clicked (the X-ray's): its definition peeked at,
       * or its file or directory flown to *)
      | Some bn, _ ->
          let hit : Code_search.hit =
            match Map_atlas.anchor_line t bn.bpath bn.banchor with
            | Some line when bn.banchor <> "" -> { kind = Def; path = bn.bpath; line; name = bn.role }
            | _ -> { kind = File; path = bn.bpath; line = 0; name = bn.role }
          in
          (Code_map_moves.search_go t hit, false)
      | None, Some (h, _, _) -> (Code_map_moves.search_go t h, false)
      | None, None -> (None, clicked))
    else (None, clicked)
  in
  let target = match jumped_to with Some c -> c | None -> target in
  (* claude: up from the map's top (a folder laid out anew, below): back
   * to the map it was taken from (Codemap) *)
  (* claude: a lone top folder kept (top_kept, a folder laid out alone)
   * is the top too: up from it leaves, not to the root around it *)
  let at_top = t.focus = 0 || (t.top_kept && Code_units.children t.placed 0 = [ t.focus ]) in
  let up_from_top = units && at_top && t.peek = None && (pressed "Backspace" || pressed "-" || (mouse.mrdown && not t.before_right) || mouse.mwheel < 0.) in
  let moved = if units && not up_from_top then Code_map_moves.unit_move computer ~pressed ~arrow t ~clicked mpx mpy else None in
  (* claude: going in, a folder holding a single unit goes on to it (the
   * author, at Linux 0.01's init/: three clicks to reach main.c) *)
  let moved =
    match moved with
    | Some i when List.mem t.focus (Code_units.ancestors t.placed i) ->
        let rec down i = match Code_units.children t.placed i with [ j ] -> down j | _ -> i in
        Some (down i)
    | m -> m
  in
  (* claude: a folder that would leave much of the screen shaded (tall
   * and narrow, or small and wide: the author, "lots of shaded space on
   * the left and right") is laid out anew, alone, the screen its
   * (Select, as the search's selections) *)
  let wasteful i =
    match t.placed.(i).node with
    | Dir _ when i <> 0 ->
        let r = t.placed.(i).rect in
        let k = Float.min (float_of_int a.pw /. r.w) (float_of_int a.ph /. r.h) in
        r.w *. k *. r.h *. k /. float_of_int (a.pw * a.ph) < 0.65
    | _ -> false
  in
  (* only going in, never up: up from a file to a wasteful folder, laid
   * out anew, then up again back to the file, was a loop (the author was
   * stuck) *)
  let going_in i = List.mem t.focus (Code_units.ancestors t.placed i) && i <> t.focus in
  let zoom = match moved with Some i when wasteful i && going_in i -> Some t.placed.(i).path | _ -> None in
  let moved = if zoom <> None then None else moved in
  let target, clicked =
    match moved with
    | Some i ->
        t.focus <- i;
        t.jumped <- None;
        t.choices <- None;
        (fit a t.placed.(i).rect, false)
    | None -> (target, clicked)
  in
  (* a click: fly to what is under it. claude: a file stays in its
   * columns, on the map; there, a click on a name goes to its binding,
   * lit (plan_codemap_naming.md); Enter opens the file view *)
  let choice = match t.choices with Some cs -> List.find_opt (fun k -> k <= List.length cs && pressed (string_of_int k)) [ 1; 2; 3; 4; 5; 6; 7; 8; 9 ] | None -> None in
  let target, action =
    if pressed "Escape" && t.help then (t.help <- false; (target, Stay))
    else if pressed "Escape" && t.peek <> None then (Code_map_peek.close_peek t; (target, Stay))
    else if pressed "Escape" && t.choices <> None then (t.choices <- None; (target, Stay))
    else if pressed "Escape" then (target, Close)
    else if choice <> None then (Code_map_moves.go_to t target (List.nth (Option.get t.choices) (Option.get choice - 1)), Stay)
    else if pressed "b" then
      match t.back with
      | (c, j) :: rest ->
          t.back <- rest;
          t.jumped <- j;
          (c, Stay)
      | [] -> (target, Stay)
    else if clicked || pressed "Enter" then begin
      if clicked then begin
        t.jumped <- None;
        t.choices <- None;
        t.note <- ""
      end;
      let u = to_u t.cam mpx and v = to_v t.cam mpy in
      (* claude: a name clicked (Map_atlas's): to its directory or file *)
      let named = if clicked then t.style.unit_at t t.cam (Playground_platform.pixel_ratio ()) mpx mpy else None in
      (* claude: a line the style placed itself (Map_atlas's ground), not
       * the treemap's: Enter opens it there, a click stays *)
      let picked = t.style.pick t t.cam (Playground_platform.pixel_ratio ()) mpx mpy in
      let file_of path = List.find_map (fun (e : entry) -> if e.path = path then Some (Lazy.force e.file) else None) (t.entries @ t.beyond) in
      match (named, picked) with
      | Some i, _ -> (fit a t.placed.(i).rect, Stay)
      | None, Some (path, line, col) -> (
          match file_of path with
          | Some f when pressed "Enter" -> (target, Open (f, line))
          | Some f ->
              (* claude: a click shows a definition's body, readable, over
               * the map (Map_atlas's peek): the name's under the mouse, its
               * own file's or, defined elsewhere, found there; else the
               * definition the line is in *)
              if col >= 0 then Option.iter (Code_map_peek.open_peek t file_of) (Code_map_peek.peek_where t f path line col);
              (* a section's title (col -1, Map_atlas's table of contents): the
               * whole section *)
              if col < 0 then begin
                let first, last = Code_map_peek.section_extent f line in
                t.peek <- Some (path, first, last);
                t.peek_scroll <- 0
              end;
              (target, Stay)
          | None -> (target, Stay))
      | None, None ->
      match under t u v with
      | None -> (target, Stay)
      | Some i -> (
          let p = t.placed.(i) in
          match (p.node, t.geometry.(i)) with
          | File (_, _, e), Some g ->
              let there = fit a p.rect in
              let close_enough =
                readable (at_ratio t.cam (Playground_platform.pixel_ratio ())) g || Float.abs (Float.log (t.cam.z /. there.z)) < 0.1
              in
              if pressed "Enter" then (target, Open (Lazy.force e.file, line_at g p.rect u v))
              else if close_enough then
                match Code_map_moves.name_under t u v with
                | Some (_, o) ->
                    let bl, bc = o.bound_at in
                    let x, y = name_pos p.rect g bl bc in
                    t.jumped <- Some (i, o.bound_at);
                    (* the camera moved only if the binding is off the map *)
                    if on a (to_px t.cam x) (to_py t.cam y) then (target, Stay) else ({ target with cx = x; cy = y +. (g.cell_h /. 2.) }, Stay)
                | None -> (
                    (* claude: defined elsewhere: there if sure, else the
                     * places to choose from *)
                    match Code_map_moves.ref_under t u v with
                    | Some (_, path, r) -> (
                        match Code_map_moves.found t i path r with
                        | [], _ ->
                            t.note <- r.rname ^ ": not in this map";
                            (target, Stay)
                        | c :: _, true -> (Code_map_moves.go_to t target c, Stay)
                        | cs, false ->
                            t.choices <- Some cs;
                            (target, Stay))
                    | None -> (target, Stay))
              else (there, Stay)
          | Dir _, _ -> (fit a p.rect, Stay)
          | _ -> (target, Stay))
    end
    else (target, Stay)
  in
  let target = Code_map_moves.clamp_cam target in
  let cam = if cam_now then target else Code_map_moves.ease t.cam target in
  (* claude: the folder laid out anew, or back up from the top *)
  let action =
    match (with_ties, zoom, action) with
    | _, _, Stay when to_graph <> None ->
        let h, users, uses = Option.get to_graph in
        Graph (h, h :: List.sort_uniq compare (users @ uses))
    (* claude: g over nothing: where one is, its parts against each other
     * (the earth: the whole project; games/: the genres; a file: its
     * definitions) *)
    | _, _, Stay when pressed "g" && units && t.search = None ->
        let here = t.placed.(t.focus).path in
        let kids = List.map (fun i -> t.placed.(i).path) (Code_units.children t.placed t.focus) in
        (match (here, kids) with
         | "", [ one ] -> Graph ("", [ one ])
         | "", kids -> Graph ("", kids)
         (* at the street: the file and its neighbours, the file open *)
         | here, _ when t.street && Map_paint.street_files t <> [] -> Graph (here, here :: Map_paint.street_files t)
         | here, _ -> Graph (here, [ here ]))
    | Some (h, users, uses), _, Stay -> Tied (h, users, uses)
    | None, Some p, Stay -> Select (p, [ p ])
    | None, None, Stay when up_from_top -> Up
    | _ -> action
  in
  ({ t with target; cam; before_right = mouse.mrdown }, action)

(* claude: the search (/, Code_search, drawn by Map_atlas): typed letters
 * the query, a / first where it looks (the files shown, or all), Tab
 * completing, up and down the hit, Enter going there -- a directory or
 * file flown to, a definition's file and its peek; name// the
 * directories so named, together (Select). The map goes on under it,
 * the mouse too, but not the keys. *)
let searching (t : t) : bool = t.search <> None || t.tour_on <> None

let update_now (computer : computer) ~(pressed : string -> bool) ~(arrow : string option) (t : t) : t * action =
  match t.search with
  | None when pressed "/" && t.style.units ->
      t.search <- Some { query = ""; sel = 0; here = false; hits = (("", false), []) };
      update_map computer ~pressed:(fun _ -> false) ~arrow:None t
  (* claude: a tour under way: n, p, Escape are its *)
  | None when t.tour_on <> None && (pressed "n" || pressed "p" || pressed "Escape") ->
      let tr, k = Option.get t.tour_on in
      let t =
        if pressed "Escape" then begin
          t.tour_on <- None;
          t.peek <- None;
          t.peek_stack <- [];
          t
        end
        else
          let k = if pressed "n" then min (List.length tr.stops - 1) (k + 1) else max 0 (k - 1) in
          match Code_map_moves.tour_go t tr k with Some c -> { t with target = c } | None -> t
      in
      update_map computer ~pressed:(fun _ -> false) ~arrow:None t
  | None -> update_map computer ~pressed ~arrow t
  | Some s ->
      let hits = Map_search.search_hits t in
      let n = List.length hits in
      let t, action =
        if pressed "Escape" then (t.search <- None; (t, Stay))
        (* claude: ctrl+Enter, the query kept as a mark, in the next
         * colour; the same query again, the mark taken off *)
        else if pressed "Enter" && Set_.mem "Control" computer.keyboard.keys then begin
          (if List.exists (fun (l : mark) -> l.mquery = s.query) t.marks then t.marks <- List.filter (fun (l : mark) -> l.mquery <> s.query) t.marks
           else if s.query <> "" then begin
             let used = List.map (fun (l : mark) -> l.mcolour) t.marks in
             let mcolour = match List.find_opt (fun c -> not (List.mem c used)) Map_search.mark_colours with Some c -> c | None -> List.hd Map_search.mark_colours in
             t.marks <- t.marks @ [ { mquery = s.query; mcolour; msay = None; mhits = None } ];
             t.mark_group <- 0
           end);
          t.search <- None;
          t.painted <- None;
          (t, Stay)
        end
        (* claude: shift+Enter, all it found together: its directories and
         * files, else the files of its definitions *)
        else if pressed "Enter" && Set_.mem "Shift" computer.keyboard.keys then begin
          match Map_search.search_set t with
          | [] -> (t, Stay)
          | set ->
              t.search <- None;
              (t, Select (Printf.sprintf "%s: the %d found" s.query (List.length set), set))
        end
        else if pressed "Enter" then begin
          match Map_search.search_named t with
          | _ :: _ :: _ as dirs ->
              t.search <- None;
              let name = Code_search.basename (List.hd dirs) in
              (t, Select (Printf.sprintf "the %d directories named %s" (List.length dirs) name, dirs))
          | _ -> (
              match List.nth_opt hits s.sel with
              (* claude: a view: its files together; a tour: its first stop *)
              | Some { kind = View; line; _ } -> (
                  t.search <- None;
                  match List.nth_opt (Code_guide.views t.guide) line with Some v -> (t, Select (v.vname, Code_map_moves.view_set t v)) | None -> (t, Stay))
              | Some { kind = Tour; line; _ } -> (
                  t.search <- None;
                  match List.nth_opt (Code_guide.tours t.guide) line with
                  | Some tr -> ( match Code_map_moves.tour_go t tr 0 with Some c -> ({ t with target = c }, Stay) | None -> (t, Stay))
                  | None -> (t, Stay))
              | Some h ->
                  t.search <- None;
                  (match Code_map_moves.search_go t h with Some c -> ({ t with target = c }, Stay) | None -> (t, Stay))
              | None -> (t, Stay))
        end
        else begin
          if pressed "Backspace" && s.query <> "" then begin s.query <- String.sub s.query 0 (String.length s.query - 1); s.sel <- 0 end;
          if pressed "Tab" then begin s.query <- Code_search.complete hits s.query; s.sel <- 0 end;
          (match arrow with
          | Some "ArrowDown" -> s.sel <- min (max 0 (n - 1)) (s.sel + 1)
          | Some "ArrowUp" -> s.sel <- max 0 (s.sel - 1)
          | _ -> ());
          String.iter
            (fun ch ->
              if ch = '/' && s.query = "" then s.here <- not s.here
              else if ch >= ' ' && ch <> '\127' then begin s.query <- s.query ^ String.make 1 ch; s.sel <- 0 end)
            computer.keyboard.typed;
          (t, Stay)
        end
      in
      let t, a = update_map computer ~pressed:(fun _ -> false) ~arrow:None t in
      (t, if action <> Stay then action else a)

(* claude: a key needing the uses counted (l, the layers; g, the matrix
 * and a unit's ties), pressed while they are not (natively, the menu's
 * maps count them only when asked): kept, the frame drawn saying so
 * (view), counted the next frame, then the key played -- seconds, but
 * said, not a freeze (the author: "at least we should show a progress
 * bar or something") *)
let needs_uses = [ "l"; "L"; "g" ]

let update (computer : computer) ~(pressed : string -> bool) ~(arrow : string option) (t : t) : t * action =
  match t.deferred with
  | Some k ->
      t.deferred <- None;
      ignore (rank_of t);
      update_now computer ~pressed:(fun x -> x = k || pressed x) ~arrow t
  | None -> (
      match List.find_opt pressed needs_uses with
      | Some k when t.rank = None && t.search = None && t.style.units ->
          t.deferred <- Some k;
          (t, Stay)
      | _ -> update_now computer ~pressed ~arrow t)

(* claude: the parts' own, for the map's users: one module to know *)
let morph_from = Code_map_view.morph_from
let view = Code_map_view.view
let version = Code_map_view.version
let go_back_to = Code_map_moves.go_back_to
let stops = Code_map_moves.stops
let peek_extent = Code_map_peek.peek_extent
let glass = Code_map_glass.glass
let cycle_glass = Code_map_glass.cycle_glass
let glass_name = Code_map_glass.glass_name
