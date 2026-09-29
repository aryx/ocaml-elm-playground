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

(* claude: the map's types and tools, Code_map's own as before the styles
 * were apart (plan_codemap_google_maps.md, step 0) *)
include Code_map_base

type action = Stay | Open of Code_file.t * int | Close

(* claude: the styles, m going from one to the next, one setting for
 * every map (as the glass's), a flag's at the start (style=) *)
let styles = [ Map_classic.style; Map_streets.style; Map_atlas.style; Map_v2.style ]
(* claude: v2 the default, everywhere (plan_codemap_v2.md, step 11);
 * the others behind m, zooming freely *)
let chosen = ref Map_v2.style
let choose_style (name : string) = match List.find_opt (fun s -> s.sname = name) styles with Some s -> chosen := s | None -> ()
let style_name () = !chosen.sname

let cycle_style () =
  let rec next = function s :: (n :: _ as rest) -> if s == !chosen then n else next rest | _ -> List.hd styles in
  chosen := next styles

(* claude: the layout a style wants: the atlas's layered by who uses
 * whom (Code_layers), the others' by name *)
let laid_out (t : t) (style : style) (algo : Treemap.algo) : t =
  let links = if style.sname = "atlas" then Some (Code_rank.links (rank_of t)) else None in
  let placed, geometry = relayout ?links t.cam.a algo t.entries in
  { t with style; algo; placed; geometry; painted = None; lens = None; focus = 0 }

(* a map in the chosen style *)
let make ?numbered ?colours ?roots ?guide ?beyond ~area ~title ~marked entries : t =
  let t = Code_map_base.make ?numbered ?colours ?roots ?guide ?beyond ~style:!chosen ~area ~title ~marked entries in
  if !chosen.sname = "atlas" then laid_out t !chosen t.algo else t

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

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

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
  let marks = List.filter_map (fun l -> if l > 0 then Some (l, Code_file.trick) else None) f.marks in
  (0, "its header") :: List.sort_uniq (fun (a, _) (b, _) -> compare a b) (sections @ marks)

(* claude: the definition a line is in: its header, to the line before
 * the next top-level one *)
let def_extent (f : Code_file.t) (line : int) : int * int =
  let heads =
    List.filter_map (fun (l, _, (cat : Highlight_code.category)) -> match cat with Def_function | Def_value | Def_type | Def_module -> Some l | _ -> None) f.defs
    |> List.sort_uniq compare
  in
  let first = List.fold_left (fun acc l -> if l <= line then l else acc) (match heads with l :: _ when l <= line -> l | _ -> line) heads in
  let last = match List.find_opt (fun l -> l > first) heads with Some n -> n - 1 | None -> Code_file.nlines f - 1 in
  (* not the next section's banner and comment: up to its last line of code *)
  let trailer l =
    let rec first_cat c = if c >= Code_file.cols then None else match Code_file.at f l c with Some cat -> Some cat | None -> first_cat (c + 1) in
    match first_cat 0 with None -> true | Some (Comment | Comment_section) -> true | Some _ -> false
  in
  let rec trim l = if l > first && trailer l then trim (l - 1) else l in
  (first, max first (trim last))

(* claude: a section: its title's line, to the line before the next
 * section's banner *)
let section_extent (f : Code_file.t) (line : int) : int * int =
  let titles =
    List.filter_map (fun (l, _, (cat : Highlight_code.category)) -> if cat = Comment_section && l > line + 1 then Some l else None) f.defs
    |> List.sort_uniq compare
  in
  let next = match titles with l :: _ -> l - 2 | [] -> Code_file.nlines f - 1 in
  (line, max line next)

(* claude: the definition a click at a line and column of [path]'s file
 * peeks at: the name's there (its binding), or, defined elsewhere, found
 * among the sources (the map's, and those beyond it); else the line's
 * own definition *)
let peek_where (t : t) (f : Code_file.t) (path : string) (line : int) (col : int) : (string * int) option =
  match Code_file.name_at f line col with
  | Some o -> Some (path, fst o.bound_at)
  | None -> (
      match Code_file.ref_at f line col with
      | Some r -> (
          match Code_names.find_in ~roots:t.roots (index_of t) ~from:path f r with
          | c :: _, _ -> Some (c.path, c.line)
          | [], _ ->
              t.note <- r.rname ^ ": not found";
              None)
      | None -> Some (path, line))

(* the peeks: one on top of the others, four at most, each its scroll *)
let open_peek (t : t) (file_of : string -> Code_file.t option) ((p, l) : string * int) : unit =
  match file_of p with
  | Some g ->
      let first, last = def_extent g l in
      (match t.peek with Some top -> t.peek_stack <- (top, t.peek_scroll) :: t.peek_stack | None -> ());
      t.peek <- Some (p, first, last);
      t.peek_scroll <- 0
  | None -> ()

let close_peek (t : t) : unit =
  match t.peek_stack with
  | (top, scroll) :: rest ->
      t.peek <- Some top;
      t.peek_scroll <- scroll;
      t.peek_stack <- rest
  | [] -> t.peek <- None

let entries (t : t) : entry list = t.entries
let number (t : t) (path : string) : int option = Hashtbl.find_opt t.order path

(* claude: the name under a point of the layout, bound in its file
 * (Code_file.name_at, plan_codemap_naming.md levels 1 and 2), if the file
 * is lexed: the file's index in [placed], and the occurrence *)
let name_under (t : t) (u : float) (v : float) : (int * Highlight_code.occurrence) option =
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

(* claude: the magnifying glass (below): round, a reading glass (80
 * columns), or none -- none at first, o going from one to the next, one
 * setting for every map (tinybox's panel and its explorer) *)
type glass = Round | Reading | No_glass

let glass_shape = ref No_glass
let cycle_glass () = glass_shape := match !glass_shape with Round -> Reading | Reading -> No_glass | No_glass -> Round
let glass_name () = match !glass_shape with Round -> "round" | Reading -> "wide" | No_glass -> "none"

(* claude: moving by units (Code_units, a style's [units]: Map_v2's):
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
            (* a section's title (Map_v2's, column -1) is peeked at, not flown into *)
            match t.style.pick t t.cam (Playground_platform.pixel_ratio ()) mpx mpy with
            | Some (_, _, c) when c < 0 -> None
            | _ -> if is_file then None else Code_units.toward t.placed i u v)
      else None

let update (computer : computer) ~(pressed : string -> bool) ~(arrow : string option) (t : t) : t * action =
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
  if pressed "o" then cycle_glass ();
  (* claude: at the ground, the file with what it uses (Map_v2) *)
  (* claude: a: what it uses, then what uses it, then both, then off *)
  if pressed "a" then begin
    t.street_mode <- (t.street_mode + 1) mod 4;
    t.street <- t.street_mode <> 0;
    t.painted <- None
  end;
  (* claude: the skeletons, at any level (Map_v2) *)
  (* x: the X-ray on its first skeleton, then the next, then off (Map_v2
   * turns it off past the last) *)
  if pressed "x" then if t.xray then t.xray_n <- t.xray_n + 1 else begin t.xray <- true; t.xray_n <- 0 end;
  (* claude: in the X-ray, 1 to 6 the anatomy's plates (Code_anatomy) *)
  if t.xray && t.choices = None then List.iter (fun s -> if pressed (Code_anatomy.key s) then Code_anatomy.toggle s) Code_anatomy.all;
  (* claude: the style, the next one, for this map and those to come *)
  let before = t.placed in
  let t =
    if pressed "m" then begin
      cycle_style ();
      if t.style.sname = "atlas" || !chosen.sname = "atlas" then laid_out t !chosen t.algo else { t with style = !chosen; painted = None; lens = None }
    end
    else t
  in
  let t =
    if pressed "t" then
      let algo : Treemap.algo = match t.algo with Ordered -> Squarified | Squarified -> Slice_and_dice | Slice_and_dice -> Ordered in
      laid_out t t.style algo
    else t
  in
  (* a new layout: back to the whole map *)
  let units = t.style.units in
  let target = if ((not units) && (pressed "Home" || pressed "0")) || t.placed != before then home a else target in
  if t.placed != before then t.focus <- 0;
  let target = if (not units) && (pressed "Backspace" || (mouse.mrdown && not t.before_right)) then up { t with target } else target in
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
      let z = (clamp_cam { target with z = target.z *. (1.25 ** mouse.mwheel) }).z in
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
          | Some f -> ( match peek_where t f p l col with Some w when List.length t.peek_stack < 3 -> open_peek t file_of w | _ -> ())
          | None -> ())
      (* inside the peek, not on a line (its title): nothing *)
      | Some _ -> ()
      | None -> close_peek t);
      false
    end
    else clicked
  in
  let moved = if units then unit_move computer ~pressed ~arrow t ~clicked mpx mpy else None in
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
    if pressed "Escape" && t.peek <> None then (close_peek t; (target, Stay))
    else if pressed "Escape" && t.choices <> None then (t.choices <- None; (target, Stay))
    else if pressed "Escape" then (target, Close)
    else if choice <> None then (go_to t target (List.nth (Option.get t.choices) (Option.get choice - 1)), Stay)
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
      (* claude: a name clicked (Map_v2's): to its directory or file *)
      let named = if clicked then t.style.unit_at t t.cam (Playground_platform.pixel_ratio ()) mpx mpy else None in
      (* claude: a line the style placed itself (Map_v2's ground), not
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
               * the map (Map_v2's peek): the name's under the mouse, its
               * own file's or, defined elsewhere, found there; else the
               * definition the line is in *)
              if col >= 0 then Option.iter (open_peek t file_of) (peek_where t f path line col);
              (* a section's title (col -1, Map_v2's table of contents): the
               * whole section *)
              if col < 0 then begin
                let first, last = section_extent f line in
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
                match name_under t u v with
                | Some (_, o) ->
                    let bl, bc = o.bound_at in
                    let x, y = name_pos p.rect g bl bc in
                    t.jumped <- Some (i, o.bound_at);
                    (* the camera moved only if the binding is off the map *)
                    if on a (to_px t.cam x) (to_py t.cam y) then (target, Stay) else ({ target with cx = x; cy = y +. (g.cell_h /. 2.) }, Stay)
                | None -> (
                    (* claude: defined elsewhere: there if sure, else the
                     * places to choose from *)
                    match ref_under t u v with
                    | Some (_, path, r) -> (
                        match found t i path r with
                        | [], _ ->
                            t.note <- r.rname ^ ": not in this map";
                            (target, Stay)
                        | c :: _, true -> (go_to t target c, Stay)
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
  let target = clamp_cam target in
  let cam = if cam_now then target else ease t.cam target in
  ({ t with target; cam; before_right = mouse.mrdown }, action)

(*****************************************************************************)
(* View *)
(*****************************************************************************)

(* claude: on the map read up close, as in the file view (Code_view): the
 * name under the mouse, its binding framed cyan and its uses lit yellow,
 * in its file; and the binding a click went to, lit green *)
let names_lit (computer : computer) (t : t) : shape list =
  (* claude: on a file, Map_v2 lights the names itself, where its lines
   * are (Map_v2.names_glow): the treemap is not what is on the map *)
  if t.style.units && (match t.placed.(t.focus).node with File _ -> true | Dir _ -> false) then []
  else
  let c = t.cam in
  let a = c.a in
  (* [glow]: pulsing (Code_view.glow), where the eye must go *)
  let place ?(glow = false) (i : int) ((line, col) : int * int) (len : int) (color : color) (alpha : float) : shape list =
    match t.geometry.(i) with
    | Some g ->
        let x, y = name_pos t.placed.(i).rect g line col in
        let w = float_of_int len *. g.cell_w *. c.z and h = g.cell_h *. c.z in
        let px = to_px c x +. (w /. 2.) and py = to_py c y +. (h /. 2.) in
        if not (on a px py) then []
        else if glow then List.map (move (sx a px) (sy a py)) (Code_view.glow computer color w h)
        else [ rectangle color w h |> move (sx a px) (sy a py) |> fade alpha ]
    | None -> []
  in
  let file (i : int) = match t.placed.(i).node with File (_, _, e) when Lazy.is_val e.file -> Some (Lazy.force e.file) | _ -> None in
  let mouse = computer.mouse in
  let mpx = px_of a mouse.mx and mpy = py_of a mouse.my in
  let u = to_u c mpx and v = to_v c mpy in
  let hovered =
    if t.moving || not (on a mpx mpy && readable_at t u v) then []
    else
      match name_under t u v with
      | Some (i, o) -> (
          match file i with
          | Some f ->
              List.concat_map
                (fun (w : Highlight_code.occurrence) ->
                  let binding = (w.line, w.col) = o.bound_at in
                  place ~glow:binding i (w.line, w.col) w.len (if binding then rgb 0 225 255 else yellow) 0.25)
                (Code_file.uses f o)
          | None -> [])
      | None -> []
  in
  let jumped =
    match t.jumped with
    | Some (i, at) -> (
        match file i with
        | Some f -> (
            match Code_file.name_at f (fst at) (snd at) with Some o -> place ~glow:true i at o.len (rgb 90 210 120) 0.45 | None -> [])
        | None -> [])
    | None -> []
  in
  (* claude: a name defined elsewhere, framed magenta (level 3) *)
  let elsewhere =
    if hovered <> [] || t.moving || not (on a mpx mpy && readable_at t u v) then []
    else match ref_under t u v with Some (i, _, r) -> place i (r.rline, r.rcol) r.rlen (rgb 230 90 230) 0.3 | None -> []
  in
  (* the places to choose from, numbered *)
  let choices =
    match t.choices with
    | Some cs ->
        let cs = List.filteri (fun k _ -> k < 9) cs in
        let n = List.length cs in
        let row_h = 22. and w = 700. in
        let top = computer.screen.bottom +. 90. +. (float_of_int n *. row_h) in
        [ rectangle (rgb 20 18 40) w ((float_of_int n *. row_h) +. 40.) |> move 0. (top -. ((float_of_int n *. row_h) /. 2.) +. 8.) |> fade 0.92 ]
        @ [ words yellow "several places: 1 to 9 to choose, esc to close" |> scale (13. /. words_font_size) |> move 0. (top +. 14.) ]
        @ List.mapi
            (fun k (c : Code_names.candidate) ->
              words ink (Printf.sprintf "%d   %s:%d%s" (k + 1) c.path (c.line + 1) (if c.other_project then "   (another project)" else ""))
              |> scale (14. /. words_font_size)
              |> move 0. (top -. (float_of_int (k + 1) *. row_h) +. 6.))
            cs
    | None -> []
  in
  jumped @ hovered @ elsewhere @ choices

(* claude: for the status line: where the name under the mouse, defined
 * elsewhere, goes; or what the last click found (Code_map.note) *)
let where_to (computer : computer) (t : t) : string option =
  let c = t.cam in
  let a = c.a in
  let mpx = px_of a computer.mouse.mx and mpy = py_of a computer.mouse.my in
  let u = to_u c mpx and v = to_v c mpy in
  let hover =
    if t.moving || not (on a mpx mpy && readable_at t u v) then None
    else
      match ref_under t u v with
      | Some (i, path, r) -> (
          let name = String.concat "." (r.rpath @ [ r.rname ]) in
          match found t i path r with
          | [], _ -> Some (name ^ ": not in this map")
          | c :: _, true ->
              Some
                (Printf.sprintf "%s -> %s:%d%s   (click to go, b back)" name c.path (c.line + 1)
                   (if c.other_project then ", in another project" else ""))
          | cs, false -> Some (Printf.sprintf "%s -> %d places as near (click to choose)" name (List.length cs)))
      | None -> None
  in
  match hover with Some s -> Some s | None -> if t.note <> "" then Some t.note else None

let view ?(chrome = true) (computer : computer) (t : t) : shape list =
  let c = t.cam in
  let a = c.a in
  (* claude: the picture at the window's resolution (at_ratio),
   * anti-aliased, when the camera is still; while it moves, a quick one:
   * half the screen's resolution, one sample a pixel -- a zoom repaints
   * every frame, and the sharp picture (7 million pixels at 4K, 4 samples
   * each) would make it stutter; it comes the frame after the camera
   * stops *)
  let q = Float.max 0.5 (Float.min 3. (Playground_platform.pixel_ratio ())) in
  let still = t.last = Some c in
  t.last <- Some c;
  t.moving <- not still;
  let want = if still then q else Float.min q 0.5 in
  let img =
    match t.painted with
    | Some (pc, pq, img) when pc = c && pq = want -> img
    | _ ->
        let img = t.style.paint ~aa:still t (at_ratio c want) in
        t.painted <- Some (c, want, img);
        img
  in
  let mouse = computer.mouse in
  let mpx = px_of a mouse.mx and mpy = py_of a mouse.my in
  let u = to_u c mpx and v = to_v c mpy in
  let hovered = if on a mpx mpy then under t u v else None in
  t.pointer <- (if on a mpx mpy then Some (u, v) else None);
  (let (Time now) = computer.time in t.clock <- now);
  let box color th (x0, y0, x1, y1) = frame a color (float_of_int x0) (float_of_int y0) (float_of_int x1) (float_of_int y1) th in
  let marks =
    Array.to_list t.placed
    |> List.concat_map (fun (p : entry Treemap.placed) ->
           match p.node with
           (* claude: not on a file in v2 (the ground, the street): the
            * treemap is not what is on the map *)
           | File (_, _, e) when List.mem e.path t.marked && not (t.style.units && match t.placed.(t.focus).node with File _ -> true | Dir _ -> false) -> (
               match clip c p.rect with Some b -> box yellow 3. b | None -> [])
           | _ -> [])
  in
  (* claude: a style's own place under the mouse (Map_v2's ground: the
   * lines laid out anew), else the treemap's *)
  let picked = if on a mpx mpy then t.style.pick t c q mpx mpy else None in
  (* a file's line, and the definition it is in, for the status line *)
  let where (e : entry) (line : int) =
    let def =
      if Lazy.is_val e.file then List.fold_left (fun acc (l, name, _) -> if l <= line then Some name else acc) None (Lazy.force e.file).defs
      else None
    in
    Printf.sprintf "%s:%d%s   (%d lines)" e.path (line + 1) (match def with Some d -> "   " ^ d | None -> "") e.nlines
  in
  (* claude: on a file (Map_v2's ground and street) the treemap is not
   * what is on the map: no frame, and the status line the style's place
   * under the mouse (its file and line, a panel's too), else nothing *)
  let on_a_file = t.style.units && match t.placed.(t.focus).node with File _ -> true | Dir _ -> false in
  let hover, status =
    match (picked, hovered) with
    (* claude: a peek open (Map_v2's) is what is under the mouse *)
    | _ when t.peek <> None -> ([], "")
    | Some (path, line, _), _ -> ([], match List.find_opt (fun (e : entry) -> e.path = path) t.entries with Some e -> where e line | None -> "")
    | None, _ when on_a_file -> ([], "")
    | None, Some i -> (
        let p = t.placed.(i) in
        (* claude: no frame round the unit one is in *)
        let frame = match clip c p.rect with Some b when not (t.style.units && i = t.focus) -> box white 1.5 b | _ -> [] in
        match (p.node, t.geometry.(i)) with
        | File (_, _, e), Some g -> (frame, where e (line_at g p.rect u v))
        | _ -> (frame, p.path))
    | None, None -> ([], "")
  in
  let algo = match t.algo with Ordered -> "ordered" | Squarified -> "squarified" | Slice_and_dice -> "slice and dice" in
  let screen = computer.screen in
  (if chrome then [ rectangle (rgb 12 10 28) screen.width screen.height ] else [])
  @ [ bitmap (float_of_int a.pw) (float_of_int a.ph) img |> move (sx a (float_of_int a.pw /. 2.)) (sy a (float_of_int a.ph /. 2.)) ]
  @ t.style.labels t c q @ marks
  @
  if not chrome then []
  else
    hover @ names_lit computer t
    @ [
        words yellow t.title |> scale (22. /. words_font_size) |> move 0. (screen.top -. 45.);
        words ink (match where_to computer t with Some s -> s | None -> status) |> scale (14. /. words_font_size) |> move 0. (screen.bottom +. 45.);
        words dim
          (if t.style.units then
             Printf.sprintf "wheel or click: in, a directory at a time   right click, - or wheel back: out   arrows: beside   a what a file uses   x skeleton   enter the file view   m style (%s)   n tour (p back)   0 all   esc back" t.style.sname
           else
           Printf.sprintf "wheel zoom   drag pan   click fly in, a name to its definition (b back)   enter the file view   right click up   m style (%s)   t layout (%s)   n tour (p back)   o glass (%s)   0 all   esc back" t.style.sname algo (glass_name ()))
        |> scale (12. /. words_font_size)
        |> move 0. (screen.bottom +. 18.);
      ]

(*****************************************************************************)
(* The magnifying glass *)
(*****************************************************************************)

(* claude: a glass over the map, the part under the cursor closer. Not a
 * zoom of the map's picture (that would only enlarge its pixels, blurred,
 * the problem pixel_ratio solved): the part under the glass painted
 * again, by the same paint, with a camera [power] times closer, at the
 * window's resolution, anti-aliased. The power is chosen for the file
 * under the cursor, so that its lines come out about 16 units high, the
 * VGA font's own size: readable whatever the file's size. The glass's
 * shape is its picture's pixels made transparent (alpha 0, a soft edge):
 * the playground has no clipping. Painted again only when the cursor
 * moves.
 *
 * Two glasses. A round one, a glance at the code under the mouse. A
 * reading glass, the rectangular
 * kind laid over a page, where one reads: 80 columns of 8 units (640,
 * and a margin) by some 16 lines, whole lines of code rather than a
 * keyhole of them. o goes from one to the other, and to none (the
 * glass always enlarges, even over code big enough to read: none is the
 * way to be rid of it). *)

(* the file under the mouse (its rectangle and geometry), and the power
 * that makes its lines 16 units high; None when the mouse is off the map *)
let under_glass (computer : computer) (t : t) =
  let c = t.cam in
  let a = c.a in
  let mouse = computer.mouse in
  let mpx = px_of a mouse.mx and mpy = py_of a mouse.my in
  if not (on a mpx mpy) then None
  else
    let u = to_u c mpx and v = to_v c mpy in
    let file = match under t u v with Some i -> ( match t.geometry.(i) with Some g -> Some (t.placed.(i).rect, g) | None -> None) | None -> None in
    let power = match file with Some (_, g) -> float_of_int Vga_font.height /. (g.cell_h *. c.z) | None -> 4. in
    Some (u, v, file, power)

(* the part of the map [lc] sees, painted, the pixels of its picture out
 * of the glass's shape transparent: [alpha w h fx fy], w and h the
 * picture's size, fx fy a pixel's centre, gives its alpha *)
let glass_picture (t : t) (lc : camera) (alpha : float -> float -> float -> float -> int option) : Rgba_image.t * float =
  let q = Float.max 0.5 (Float.min 3. (Playground_platform.pixel_ratio ())) in
  let img =
    match t.lens with
    | Some (pc, img) when pc = lc && img.width = (at_ratio lc q).a.pw -> img
    | _ ->
        let img = t.style.paint ~aa:true t (at_ratio lc q) in
        let w = float_of_int img.width and h = float_of_int img.height in
        for y = 0 to img.height - 1 do
          for x = 0 to img.width - 1 do
            match alpha w h (float_of_int x +. 0.5) (float_of_int y +. 0.5) with
            | Some al -> Bigarray.Array1.unsafe_set img.rgba ((4 * ((y * img.width) + x)) + 3) al
            | None -> ()
          done
        done;
        t.lens <- Some (lc, img);
        img
  in
  (img, q)

(* one pixel of soft edge, [dist] from a circle's centre of radius [r] *)
let soft_edge (r : float) (dist : float) : int = if dist <= r -. 1. then 255 else if dist >= r then 0 else int_of_float ((r -. dist) *. 255.)

(* the round glass: centred on the point under the cursor *)
let lens_radius = 185.

let lens (computer : computer) (t : t) : shape list =
  match under_glass computer t with
  | None -> []
  | Some (u, v, _, power) ->
      let power = Float.max 2. (Float.min 10. power) in
      let d = int_of_float (2. *. lens_radius) in
      let lc = { cx = u; cy = v; z = t.cam.z *. power; a = { (t.cam.a) with pw = d; ph = d } } in
      let img, _ =
        glass_picture t lc (fun w _ fx fy ->
            let r = w /. 2. in
            let dx = fx -. r and dy = fy -. r in
            Some (soft_edge r (Float.sqrt ((dx *. dx) +. (dy *. dy)))))
      in
      let x = computer.mouse.mx and y = computer.mouse.my in
      let r = lens_radius in
      [
        (* the handle, down and to the right, as a magnifying glass is held *)
        rectangle (rgb 90 60 30) 22. 110. |> move 0. (-.(r +. 50.)) |> rotate 45. |> move x y;
        rectangle (rgb 150 150 160) 26. 18. |> move 0. (-.(r +. 4.)) |> rotate 45. |> move x y;
        (* the rim *)
        circle (rgb 40 40 50) (r +. 9.) |> move x y;
        circle (rgb 190 190 205) (r +. 6.) |> move x y;
        circle (rgb 12 10 28) (r +. 1.) |> move x y;
        bitmap (2. *. r) (2. *. r) img |> move x y;
        (* a glint on the glass *)
        oval white (r *. 0.5) (r *. 0.18) |> rotate 35. |> move (x -. (r *. 0.45)) (y +. (r *. 0.55)) |> fade 0.12;
        words (rgb 190 190 205) (Printf.sprintf "x%.0f" power) |> scale (12. /. words_font_size) |> move (x +. (r *. 0.62)) (y -. (r *. 0.85));
      ]

(* the reading glass. Over a file, it lines up with the start of the
 * column of lines under the mouse (a file is laid out in several), so it
 * shows whole lines from their first character, not the end of one column
 * and the start of the next; up and down, the line under the mouse is
 * drawn where the mouse is. It stays on the screen. *)
let reading_w = 660.
let reading_h = 272.
let reading_corner = 22.

(* a rectangle with rounded corners, from rectangles and circles (the
 * playground has no rounded rectangle) *)
let rounded (color : color) (w : number) (h : number) (r : number) : shape =
  group
    [
      rectangle color w (h -. (2. *. r));
      rectangle color (w -. (2. *. r)) h;
      circle color r |> move ((w /. 2.) -. r) ((h /. 2.) -. r);
      circle color r |> move (-.((w /. 2.) -. r)) ((h /. 2.) -. r);
      circle color r |> move ((w /. 2.) -. r) (-.((h /. 2.) -. r));
      circle color r |> move (-.((w /. 2.) -. r)) (-.((h /. 2.) -. r));
    ]

let reading_glass (computer : computer) (t : t) : shape list =
  match under_glass computer t with
  | None -> []
  | Some (u, v, file, power) ->
      let power = Float.max 1.5 (Float.min 10. power) in
      let c = t.cam in
      let a = c.a in
      let mouse = computer.mouse in
      let z = c.z *. power in
      let margin = 10. in
      let screen = computer.screen in
      let keep lo hi x = Float.max lo (Float.min hi x) in
      let on_screen_x x = keep (screen.left +. (reading_w /. 2.) +. 4.) (screen.right -. (reading_w /. 2.) -. 4.) x in
      let gy = keep (screen.bottom +. (reading_h /. 2.) +. 4.) (screen.top -. (reading_h /. 2.) -. 4.) mouse.my in
      (* across: the column's start at the glass's left margin, the glass
       * over the column on the map; else the point under the mouse where
       * the mouse is *)
      let gx, cx =
        match file with
        | Some (r, g) ->
            let start = r.x +. (Float.floor ((u -. r.x) /. g.colw) *. g.colw) in
            (on_screen_x (sx a (to_px c start) +. (reading_w /. 2.) -. margin), start +. (((reading_w /. 2.) -. margin) /. z))
        | None ->
            let gx = on_screen_x mouse.mx in
            (gx, u +. ((gx -. mouse.mx) /. z))
      in
      (* down: the line under the mouse drawn where the mouse is *)
      let lc = { cx; cy = v -. ((gy -. mouse.my) /. z); z; a = { a with pw = int_of_float reading_w; ph = int_of_float reading_h } } in
      let img, _ =
        glass_picture t lc (fun w h fx fy ->
            (* rounded corners: the distance to the corner's centre *)
            let r = reading_corner *. (w /. reading_w) in
            let dx = Float.max 0. (Float.max (r -. fx) (fx -. (w -. r))) and dy = Float.max 0. (Float.max (r -. fy) (fy -. (h -. r))) in
            if dx > 0. && dy > 0. then Some (soft_edge r (Float.sqrt ((dx *. dx) +. (dy *. dy)))) else None)
      in
      let w = reading_w and h = reading_h and r = reading_corner in
      [
        (* the handle, from the bottom right corner, as a reading glass is held *)
        rectangle (rgb 90 60 30) 24. 120. |> move 0. (-60.) |> rotate 45. |> move (gx +. (w /. 2.) -. 10.) (gy -. (h /. 2.) +. 10.);
        (* the rim *)
        rounded (rgb 40 40 50) (w +. 18.) (h +. 18.) (r +. 9.) |> move gx gy;
        rounded (rgb 190 190 205) (w +. 12.) (h +. 12.) (r +. 6.) |> move gx gy;
        rounded (rgb 12 10 28) (w +. 2.) (h +. 2.) (r +. 1.) |> move gx gy;
        bitmap w h img |> move gx gy;
        (* a glint on the glass *)
        oval white (w *. 0.3) (h *. 0.1) |> rotate 8. |> move (gx -. (w *. 0.28)) (gy +. (h *. 0.36)) |> fade 0.1;
        words (rgb 190 190 205) (Printf.sprintf "x%.1f" power) |> scale (12. /. words_font_size) |> move (gx +. (w /. 2.) -. 24.) (gy +. (h /. 2.) +. 1.);
      ]

(* claude: no glass while the map moves under it: its picture follows
 * the map's camera, so it would be painted anew every frame, as much
 * again as the map's own (on the web, half of a zoom's frame); it comes
 * back the frame the camera stops, as the map's sharp picture does *)
let glass (computer : computer) (t : t) : shape list =
  (* claude: none over code read on the map itself, where the names under
   * the mouse are lit (names_lit) *)
  let a = t.cam.a in
  let mpx = px_of a computer.mouse.mx and mpy = py_of a computer.mouse.my in
  if t.moving || readable_at t (to_u t.cam mpx) (to_v t.cam mpy) then []
  else match !glass_shape with Round -> lens computer t | Reading -> reading_glass computer t | No_glass -> []
