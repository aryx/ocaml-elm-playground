(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Code_map_view.mli *)

open Playground
open Code_map_base

(* claude: the animation from another map to this one (a folder laid
 * out anew, or back): each rectangle from where it was on the screen,
 * in the other map, to where it is in this one (Transition, over
 * graphics_animation); a unit only here grows from its centre *)
let morph_duration = 0.45

let morph_from ~(old : t) (t : t) ~(now : float) : unit =
  let rect (r : Treemap.rect) : Transition.rect = { x = r.x; y = r.y; w = r.w; h = r.h } in
  let where = Hashtbl.create 256 in
  Array.iter (fun (p : entry Treemap.placed) -> Hashtbl.replace where p.path p.rect) old.placed;
  (* a rectangle of the old map on the screen, in this map's units *)
  let across (r : Treemap.rect) : Transition.rect =
    let px0 = to_px old.cam r.x and py0 = to_py old.cam r.y and px1 = to_px old.cam (r.x +. r.w) and py1 = to_py old.cam (r.y +. r.h) in
    let u0 = to_u t.cam px0 and v0 = to_v t.cam py0 and u1 = to_u t.cam px1 and v1 = to_v t.cam py1 in
    { x = u0; y = v0; w = u1 -. u0; h = v1 -. v0 }
  in
  let after = Array.to_list (Array.map (fun (p : entry Treemap.placed) -> (p.path, rect p.rect)) t.placed) in
  let before = List.filter_map (fun (k, _) -> Option.map (fun r -> (k, across r)) (Hashtbl.find_opt where k)) after in
  t.morph <- Some (Transition.make ~before ~after (), now)

(* the map as it is at [now], its rectangles on their way *)
let morphed (t : t) ~(now : float) : t =
  match t.morph with
  | Some (tr, start) when now < start +. morph_duration ->
      let p = Timing.at Timing.ease_in_out ((now -. start) /. morph_duration) in
      let at = Hashtbl.create 256 in
      List.iter (fun (f : string Transition.frame) -> Hashtbl.replace at f.key f.rect) (Transition.at tr p);
      let placed =
        Array.map
          (fun (q : entry Treemap.placed) ->
            match Hashtbl.find_opt at q.path with Some (r : Transition.rect) -> { q with rect = { x = r.x; y = r.y; w = Float.max 0.01 r.w; h = Float.max 0.01 r.h } } | None -> q)
          t.placed
      in
      let geometry = Array.mapi (fun i (q : entry Treemap.placed) -> match (q.node, t.geometry.(i)) with File (_, _, e), Some _ -> Some (geometry_of q.rect (max 1 e.nlines)) | _ -> t.geometry.(i)) placed in
      { t with placed; geometry; painted = None }
  | Some _ ->
      t.morph <- None;
      t
  | None -> t

(*****************************************************************************)
(* View *)
(*****************************************************************************)

(* claude: on the map read up close, as in the file view (Code_view): the
 * name under the mouse, its binding framed cyan and its uses lit yellow,
 * in its file; and the binding a click went to, lit green *)
let names_lit (computer : computer) (t : t) : shape list =
  (* claude: on a file, Map_atlas lights the names itself, where its lines
   * are (Map_cards.names_glow): the treemap is not what is on the map *)
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
    if t.moving || not (on a mpx mpy && Code_map_moves.readable_at t u v) then []
    else
      match Code_map_moves.name_under t u v with
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
    if hovered <> [] || t.moving || not (on a mpx mpy && Code_map_moves.readable_at t u v) then []
    else match Code_map_moves.ref_under t u v with Some (i, _, r) -> place i (r.rline, r.rcol) r.rlen (rgb 230 90 230) 0.3 | None -> []
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
    if t.moving || not (on a mpx mpy && Code_map_moves.readable_at t u v) then None
    else
      match Code_map_moves.ref_under t u v with
      | Some (i, path, r) -> (
          let name = String.concat "." (r.rpath @ [ r.rname ]) in
          match Code_map_moves.found t i path r with
          | [], _ -> Some (name ^ ": not in this map")
          | c :: _, true ->
              Some
                (Printf.sprintf "%s -> %s:%d%s   (click to go, b back)" name c.path (c.line + 1)
                   (if c.other_project then ", in another project" else ""))
          | cs, false -> Some (Printf.sprintf "%s -> %d places as near (click to choose)" name (List.length cs)))
      | None -> None
  in
  match hover with Some s -> Some s | None -> if t.note <> "" then Some t.note else None

(* claude: every key, explained (h): the author, "all keys should be
 * explained at the bottom or in a hover card" *)
let keys_help = [
  ("Moving", "");
  ("click, wheel forward, +", "in: a folder, a file, a unit at a time");
  ("right click, wheel back, -, Backspace", "out: the unit around");
  ("arrows", "beside: the next unit left, right, up, down");
  ("0, Home", "the whole map");
  ("Seeing more", "");
  ("hover a name", "its card: what the configs say of it; a folder's or file's ties drawn");
  ("click a name in the code", "a peek at its definition; the wheel scrolls it; Escape closes");
  ("a", "at a file: its neighbours, what it uses, what uses it (a again: the next)");
  ("x", "the X-ray: the skeleton; x again, the next one; 1-5 the plates (hover the legend)");
  ("m", "the marks: patterns lit everywhere (the configs', and those kept)");
  ("l", "the layers, shift+l back: the map coloured by a measure (used vs using, the call depth, roles, tested, described)");
  ("Searching", "");
  ("/", "search: a name, or file: dir: def: type: view: tour: bone: text: ref:");
  ("  in the search", "Tab complete, up/down choose, Enter go, shift+Enter all found together, ctrl+Enter a mark");
  ("Dependencies", "");
  ("shift+click a name", "it and the units tied to it, together (d: users, uses, both, alone)");
  ("g, ctrl+click a name", "codegraph's matrix of it and its ties");
  ("g over nothing", "the matrix of where one is: its parts against each other");
  ("Tours and views", "");
  ("n, p", "the tour: the next stop, the one before");
  ("w", "a program's map: its own code, with what it uses, the whole repository");
  ("Enter", "the file view, the file read whole");
  ("b", "back, after a jump to a definition");
  ("y", "the other style of map (atlas, classic: the code painted from afar, the wheel zooming freely)");
  ("h", "this help; Escape, back");
]

let help_shapes (computer : computer) : shape list =
  let screen = computer.screen in
  let w = 980. and row = 22. in
  let h = 40. +. (row *. float_of_int (List.length keys_help)) in
  let top = h /. 2. in
  ignore screen;
  [ rectangle yellow (w +. 4.) (h +. 4.) |> fade 0.5; rectangle (rgb 16 14 34) w h |> fade 0.97 ]
  @ List.concat
      (List.mapi
         (fun i (k, what) ->
           let y = top -. 30. -. (float_of_int i *. row) in
           let x0 = -.(w /. 2.) +. 24. in
           if what = "" then [ words yellow k |> scale (15. /. words_font_size) |> move (x0 +. (text_width 15. k /. 2.)) y ]
           else
             [ words ink k |> scale (14. /. words_font_size) |> move (x0 +. 20. +. (text_width 14. k /. 2.)) y;
               words dim what |> scale (14. /. words_font_size) |> move (x0 +. 330. +. (text_width 14. what /. 2.)) y ])
         keys_help)

(* claude: the code map's version, at the bottom right (the author: to
 * see at once whether a page runs the latest, a browser keeping the
 * program it has for a while): 0.01, 0.02, ..., raised by hand at each
 * publish of a change to the map (make publish, make codemap-web) *)
let version = "0.07"

(* claude: a line of keys and what they do, centred at [y], the keys in
 * yellow, the rest dim; the widths Code_map_base.text_width's *)
let key_line ~(y : float) (items : (string * string) list) : shape list =
  let size = 12. and gap = "   " in
  let pieces = List.concat_map (fun (k, d) -> [ (k, yellow); (" " ^ d ^ gap, dim) ]) items in
  let width s = text_width size s in
  let total = List.fold_left (fun acc (s, _) -> acc +. width s) 0. pieces -. width gap in
  let x = ref (-.total /. 2.) in
  List.map
    (fun (s, col) ->
      let w = width s in
      let shape = words col s |> scale (size /. words_font_size) |> move (!x +. (w /. 2.)) y in
      x := !x +. w;
      shape)
    pieces

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
  (* claude: a relayout under way: the rectangles on their way, painted
   * quickly each frame *)
  let moving_layout = t.morph <> None in
  let t = let (Time now) = computer.time in morphed t ~now in
  let still = t.last = Some c && not moving_layout in
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
           (* claude: not on a file in the atlas (the ground, the street): the
            * treemap is not what is on the map *)
           | File (_, _, e) when List.mem e.path t.marked && not (t.style.units && match t.placed.(t.focus).node with File _ -> true | Dir _ -> false) -> (
               match clip c p.rect with Some b -> box yellow 3. b | None -> [])
           | _ -> [])
  in
  (* claude: a style's own place under the mouse (Map_atlas's ground: the
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
  (* claude: on a file (Map_atlas's ground and street) the treemap is not
   * what is on the map: no frame, and the status line the style's place
   * under the mouse (its file and line, a panel's too), else nothing *)
  let on_a_file = t.style.units && match t.placed.(t.focus).node with File _ -> true | Dir _ -> false in
  let hover, status =
    match (picked, hovered) with
    (* claude: a peek open (Map_atlas's) is what is under the mouse *)
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
    @ (if t.help then help_shapes computer else [])
    @ [
        (* claude: flown into a unit (the atlas), its summary, not the project's
         * (the author: "at earth level it's the summary of the project,
         * but at appkit level the summary of what appkit is") *)
        (let title =
           if t.style.units && t.focus <> 0 then
             let p = t.placed.(t.focus) in
             let said =
               match p.node with
               | File _ -> Option.bind (Code_guide.file_note t.guide p.path) (fun n -> n.summary)
               | Dir _ -> Code_guide.dir_summary t.guide p.path
             in
             match said with Some s -> p.path ^ ": " ^ s | None -> p.path
           else t.title
         in
         words yellow title |> scale (22. /. words_font_size) |> move 0. (screen.top -. 45.));
        words ink (match where_to computer t with Some s -> s | None -> status) |> scale (14. /. words_font_size) |> move 0. (screen.bottom +. 45.);
      ]
      (* claude: the keys in yellow, what they do dim (the author: "so it
       * reads better"), laid out from the centre *)
      @ [ words dim ("code map " ^ version) |> scale (12. /. words_font_size) |> move (screen.right -. 60.) (screen.bottom +. 18.) ]
      @ key_line ~y:(screen.bottom +. 18.)
          (if t.style.units then
             [ ("h", "every key"); ("click", "in"); ("right click", "out"); ("/", "search"); ("a", "a file's neighbours"); ("x", "skeleton"); ("m", "marks"); ("l", "layers"); ("g", "the matrix"); ("esc", "back") ]
           else
             [ ("wheel", "zoom"); ("drag", "pan"); ("click", "fly in, a name to its definition (b back)"); ("enter", "the file view"); ("right click", "up"); ("y", Printf.sprintf "style (%s)" t.style.sname);
               ("t", Printf.sprintf "layout (%s)" algo); ("n", "tour (p back)"); ("o", Printf.sprintf "glass (%s)" (Code_map_glass.glass_name ())); ("0", "all"); ("esc", "back") ])
