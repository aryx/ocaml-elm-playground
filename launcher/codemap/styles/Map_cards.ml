(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Map_cards.mli *)

open Playground
open Code_map_base

(* a directory's or a file's card: its path, what its config says of it
 * (Code_guide), and what it holds *)
let card (t : t) (i : int) : string * string list option =
  let p = t.placed.(i) in
  (* claude: what the configs say of it, its description; the counts were
   * not what one wants to know (the author) *)
  match p.node with
  | File (_, _, e) -> (e.path, Option.map (Map_names.wrap 52) (Option.bind (Code_guide.file_note t.guide e.path) (fun n -> n.summary)))
  | Dir _ -> (p.path ^ "/", Option.map (Map_names.wrap 52) (Code_guide.dir_summary t.guide p.path))

(* claude: a unit's ties, codegraph's at the map's granularity (the
 * author: "hovering a folder label will display the hovercard and also
 * its dependencies", "folder to folder, folder to file"): the files
 * using it and those it uses (Code_rank.links), each grouped as the unit
 * where it parts from the hovered one -- the child, under their deepest
 * common folder, on its side: a far file its region, a near one itself.
 * A road from user to used, green to red, as wide as the uses *)
(* claude: the unit whose name is under the mouse, and the files tied to
 * it, its users and what it uses (shift+click: a view of them all, the
 * author: "all the things relevant to the dir") *)
let unit_with_ties (t : t) (c : camera) : (string * string list * string list) option =
  match t.pointer with
  | None -> None
  | Some (u, v) -> (
      let mx = to_px c u and my = to_py c v in
      match List.find_opt (fun (n : Map_names.name) -> Map_names.within n.nbox mx my && n.said = None) (Map_names.names t c) with
      | Some n when n.node <> 0 ->
          let h = t.placed.(n.node).path in
          let inside p = p = h || Code_search.starts p (h ^ "/") in
          (* the units tied, as the hover's roads show them: each where it
           * parts from [h] (~/ix's version_control: lib_core,
           * lib_security, lib_compression) *)
          let parts p = String.split_on_char '/' p in
          let side q =
            let rec go acc = function x :: r, y :: r' when x = y -> go (x :: acc) (r, r') | _, y :: _ -> List.rev (y :: acc) | _ -> List.rev acc in
            String.concat "/" (go [] (parts h, parts q))
          in
          let links = Code_rank.links (rank_of t) in
          let users = List.filter_map (fun (src, dst, _) -> if inside dst && not (inside src) then Some (side src) else None) links |> List.sort_uniq compare in
          let uses = List.filter_map (fun (src, dst, _) -> if inside src && not (inside dst) then Some (side dst) else None) links |> List.sort_uniq compare in
          Some (h, users, uses)
      | _ -> None)

(* claude: a hovered unit's (or capital's) ties, the most used, and the
 * units by path *)
let ties_of (t : t) (n : Map_names.name) : (string, int) Hashtbl.t * (string * int) list * (string * int) list =
            let h = t.placed.(n.node).path in
            let inside p = p = h || Code_search.starts p (h ^ "/") in
            let parts p = String.split_on_char '/' p in
            (* the unit on [q]'s side where it parts from [h] *)
            let side q =
              let rec go acc = function x :: r, y :: r' when x = y -> go (x :: acc) (r, r') | _, y :: _ -> List.rev (y :: acc) | _ -> List.rev acc in
              String.concat "/" (go [] (parts h, parts q))
            in
            let index = Hashtbl.create 256 in
            Array.iteri (fun i (p : entry Treemap.placed) -> Hashtbl.replace index p.path i) t.placed;
            (* claude: in a view of chosen units (a selection, shift+click's),
             * the ties file to file (the author: "finer grained arrows
             * ... file-to-file instead of folder to folder") *)
            let side q = if t.top_kept && Hashtbl.mem index q then q else side q in
            let add tbl k n = Hashtbl.replace tbl k (n + Option.value (Hashtbl.find_opt tbl k) ~default:0) in
            let users = Hashtbl.create 16 and uses = Hashtbl.create 16 in
            (match n.cap with
            | None ->
                List.iter
                  (fun (src, dst, n) ->
                    if inside dst && not (inside src) then add users (side src) n
                    else if inside src && not (inside dst) then add uses (side dst) n)
                  (match rank_if_counted t with Some r -> Code_rank.links r | None -> [])
            | Some (p, line, name) ->
                (* the definition's users, file by file; and its body's
                 * uses of other files' names *)
                let short = match String.rindex_opt name '.' with Some i -> String.sub name (i + 1) (String.length name - i - 1) | None -> name in
                (* an .mli's declaration: its .ml's definition's users *)
                let rp, rl =
                  if Filename.check_suffix p ".mli" then
                    let impl = Filename.remove_extension p ^ ".ml" in
                    match List.find_opt (fun (x : entry) -> x.path = impl) (t.entries @ t.beyond) with
                    | Some x -> (match List.find_opt (fun (_, nm, _) -> nm = short) (Lazy.force x.file).defs with Some (l', _, _) -> (impl, l') | None -> (p, line))
                    | None -> (p, line)
                  else (p, line)
                in
                List.iter (fun (q, k) -> if q <> p && q <> rp then add users (side q) k) (match rank_if_counted t with Some r -> Code_rank.users r rp rl short | None -> []);
                (match List.find_opt (fun (x : entry) -> x.path = p) (t.entries @ t.beyond) with
                | Some e ->
                    let f = Lazy.force e.file in
                    (* its body: to the next top-level definition *)
                    let last = List.fold_left (fun acc (l, _, (cat : Highlight_code.category)) -> if l > line && l < acc && cat <> Comment_section then l - 1 else acc) (Code_file.nlines f - 1) f.defs in
                    List.iter
                      (fun (ed : Code_street.edge) -> if ed.from_line >= line && ed.from_line <= last && not (inside ed.target) then add uses (side ed.target) 1)
                      (Code_street.uses ~index:(index_of t) ~roots:t.roots ~path:p f)
                | None -> ()));
            let top tbl = Hashtbl.fold (fun k n acc -> (k, n) :: acc) tbl [] |> List.sort (fun (_, x) (_, y) -> compare y x) |> List.filteri (fun i _ -> i < (if t.top_kept then 30 else 12)) in
            (index, top users, top uses)

(* claude: opti: kept while the same one is hovered on the same layout,
 * not found again every frame over every link (principia's 2,200 files:
 * the map busy with the mouse resting on a name) *)
let ties_cache : (entry Treemap.placed array * int * (string * int * string) option * bool * ((string, int) Hashtbl.t * (string * int) list * (string * int) list)) option ref = ref None

let ties_of_opti (t : t) (n : Map_names.name) : (string, int) Hashtbl.t * (string * int) list * (string * int) list =
  match !ties_cache with
  | Some (placed, node, cap, counted, r) when placed == t.placed && node = n.node && cap = n.cap && counted = (rank_if_counted t <> None) -> r
  | _ ->
      let r = ties_of t n in
      ties_cache := Some (t.placed, n.node, n.cap, rank_if_counted t <> None, r);
      r

let unit_ties (t : t) (c : camera) (kept : Map_names.name list) : shape list =
  match t.pointer with
  | None -> []
  | Some (u, v) -> (
      let mx = to_px c u and my = to_py c v in
      (* claude: a unit's name, or a capital: the definition's own ties
       * (the author: "hovering over a capital can also show its deps") *)
      match List.find_opt (fun (n : Map_names.name) -> Map_names.within n.nbox mx my && (n.said = None || n.cap <> None)) kept with
      | None -> []
      | Some n ->
          if n.node = 0 then []
          else
            let a = c.a in
            let index, top_users, top_uses = if !Opti.enabled then ties_of_opti t n else ties_of t n in
            let x0, y0, x1, y1 = n.nbox in
            let hx = (x0 +. x1) /. 2. and hy = (y0 +. y1) /. 2. in
            let biggest = List.fold_left (fun m (_, n) -> max m n) 1 (top_users @ top_uses) in
            let centre k =
              match Hashtbl.find_opt index k with
              | Some i -> (match clip c t.placed.(i).rect with Some (a0, b0, a1, b1) -> Some (float_of_int (a0 + a1) /. 2., float_of_int (b0 + b1) /. 2.) | None -> None)
              | None -> None
            in
            let road (ax, ay) (bx, by) n =
              let dx = bx -. ax and dy = by -. ay in
              let len = Float.max 1. (Float.hypot dx dy) in
              let bend = Float.min 80. (0.15 *. len) in
              let pts = Code_road.bspline [| (ax, ay); (((ax +. bx) /. 2.) -. (dy /. len *. bend), ((ay +. by) /. 2.) +. (dx /. len *. bend)); (bx, by) |] in
              (* claude: as wide as its uses (the author), by area: the
               * square root of its share of the largest *)
              Code_road.road a pts (1.5 +. (12. *. Float.sqrt (float_of_int n /. float_of_int biggest))) 0.8
            in
            let label (x, y) k n col =
              let str = Printf.sprintf "%s %d" (Filename.basename k) n in
              [ rectangle (rgb 18 16 36) (text_width 13. str +. 8.) 17. |> move (sx a x) (sy a (y -. 14.)) |> fade 0.85; Code_map_base.label a col 13. x (y -. 14.) str ]
            in
            List.concat_map (fun (k, n) -> match centre k with Some p -> road p (hx, hy) n @ label p k n (rgb 90 220 120) | None -> []) top_users
            @ List.concat_map (fun (k, n) -> match centre k with Some p -> road (hx, hy) p n @ label p k n (rgb 250 80 70) | None -> []) top_uses)

(* the card of the name under the mouse, beside it, on the map: its path,
 * and its description, readable; "not described yet" where no config
 * says anything of it (the configs to write) *)
let hover_card (t : t) (c : camera) (kept : Map_names.name list) : shape list =
  match t.pointer with
  | None -> []
  | Some (u, v) -> (
      let a = c.a in
      let mx = to_px c u and my = to_py c v in
      match List.find_opt (fun (n : Map_names.name) -> Map_names.within n.nbox mx my) kept with
      | None -> []
      | Some n ->
          let title, body, described =
            match n.said with
            | Some (t0 :: rest) -> (t0, rest, true)
            | _ -> (
                match card t n.node with
                | title, Some lines -> (title, lines, true)
                | title, None -> (title, [ "not described yet" ], false))
          in
          let ts = 13. and size = 16. and gap = 6. in
          let width s z = text_width z s in
          let w = 24. +. Float.max (width title ts) (List.fold_left (fun m l -> Float.max m (width l size)) 0. body) in
          let h = 18. +. ts +. (float_of_int (List.length body) *. (size +. gap)) in
          let x0 = Float.min (mx +. 18.) (float_of_int a.pw -. w -. 4.) and y0 = Float.min (my +. 18.) (float_of_int a.ph -. h -. 4.) in
          let col = lighter (archi t.colours t.placed.(n.node).path) in
          [ rectangle (rgb 18 16 36) w h |> move (sx a (x0 +. (w /. 2.))) (sy a (y0 +. (h /. 2.))) |> fade 0.96 ]
          @ frame a col x0 y0 (x0 +. w) (y0 +. h) 1.5
          @ [ label a col ts (x0 +. 12. +. (width title ts /. 2.)) (y0 +. 6. +. (ts /. 2.)) title ]
          @ List.mapi
              (fun k l ->
                let y = y0 +. 12. +. ts +. (float_of_int k *. (size +. gap)) +. (size /. 2.) in
                (* claude: on their left, their widths estimated (text_width) *)
                label a (if described then ink else dim) size (x0 +. 12. +. (width l size /. 2.)) y l)
              body)

(* claude: at the ground, what the config says of an important line, a
 * note after its end when the column has room for it *)
let notes_on (t : t) (c : camera) (e : entry) (g : Code_ground.t) : shape list =
  let a = c.a in
  let f = Lazy.force e.file in
  List.filter_map
    (fun (l, _, say) ->
      match say with
      | None -> None
      | Some say ->
          let x0, y0, w, h = if l < Array.length g.places then Code_ground.box g l else (0., 0., 0., 0.) in
          (* the line's end: its last character *)
          let last = ref 0 in
          for k = 0 to Code_file.cols - 1 do
            let ch = Bytes.get f.chars ((l * Code_file.cols) + k) in
            if ch <> '\000' && ch <> ' ' then last := k + 1
          done;
          let cw = if h >= 7. then h /. 2. else w /. 80. in
          let size = 12. in
          let start = x0 +. (float_of_int !last *. cw) +. 12. in
          (* wrapped in the room after the line's end, three lines at most *)
          let room = int_of_float ((x0 +. w -. start) /. (0.5 *. size)) in
          (* not beside a line squeezed thin (a street panel's) *)
          if room < 16 || h < 7. then None
          else
            let lines = Map_names.wrap room ("<- " ^ say) in
            let lines = if List.length lines > 3 then List.filteri (fun k _ -> k < 3) lines else lines in
            let n = float_of_int (List.length lines) in
            let tw = 0.5 *. size *. float_of_int (List.fold_left (fun m l -> max m (String.length l)) 0 lines) in
            let top = y0 +. (h /. 2.) -. (n *. (size +. 2.) /. 2.) in
            Some
              (group
                 ((rectangle (rgb 18 16 36) (tw +. 8.) ((n *. (size +. 2.)) +. 4.)
                  |> move (sx a (start +. (tw /. 2.))) (sy a (top +. (n *. (size +. 2.) /. 2.)))
                  |> fade 0.85)
                 :: List.mapi
                      (fun k l ->
                        let lw = 0.5 *. size *. float_of_int (String.length l) in
                        words (rgb 255 215 70) l |> scale (size /. words_font_size)
                        |> move (sx a (start +. (lw /. 2.))) (sy a (top +. ((float_of_int k +. 0.5) *. (size +. 2.)))))
                      lines)))
    (Map_paint.important t e)

let notes (t : t) (c : camera) (e : entry) : shape list = notes_on t c e (Map_paint.ground_of t e)

(* claude: at the street, each panel's title, the roads *)
let street_labels (t : t) (c : camera) (e : entry) : shape list =
  let a = c.a in
  let s = Map_paint.street_of t e in
  (* the line under the mouse: its roads lit, the others dimmed *)
  let hover = match t.pointer with Some (u, v) -> Code_street.line_at s (to_px c u) (to_py c v) | None -> None in
  Code_street.roads ?hover a s
  @ Code_street.ends ?hover a s
  (* claude: the configs' notes, the focus's and the panels' *)
  @ notes_on t c e s.focus
  @ List.concat_map
      (fun (p : Code_street.panel) -> match List.find_opt (fun (x : entry) -> x.path = p.path) (t.entries @ t.beyond) with Some x -> notes_on t c x p.ground | None -> [])
      (Code_street.panels s)
  @ List.map
      (fun (p : Code_street.panel) ->
        let text = Printf.sprintf "%s   (%d tie%s)" p.path p.count (if p.count = 1 then "" else "s") in
        let tw = 0.5 *. 14. *. float_of_int (String.length text) in
        label a (lighter (archi t.colours p.path)) 14. (p.ground.ox +. 8. +. (tw /. 2.)) (p.ground.oy -. 11.) text)
      (Code_street.panels s)
  (* the files tied but not shown, at the foot of their side *)
  @ (let foot more (x0 : float) =
       match more with
       | [] -> []
       | _ ->
           let n = List.length more in
           let text =
             Printf.sprintf "and %d more: %s%s" n
               (String.concat ", " (List.map (fun (p, k) -> Printf.sprintf "%s (%d)" (Filename.basename p) k) (List.filteri (fun i _ -> i < 4) more)))
               (if n > 4 then ", ..." else "")
           in
           [ label a dim 12. (x0 +. 8. +. (0.25 *. 12. *. float_of_int (String.length text))) (float_of_int a.ph -. 10.) text ]
     in
     foot s.left_more 0.
     @ (match s.right with p :: _ -> foot s.right_more p.ground.ox | [] -> foot s.right_more (float_of_int a.pw *. 0.6)))
  @
  if Code_street.panels s = [] then
    [ label a dim 16. (float_of_int a.pw /. 2.) 24. (match Map_paint.street_mode t with Uses -> "(it uses nothing of this map's other files)" | Users -> "(nothing of this map uses it)" | Both -> "(no tie with this map's other files)") ]
  else []

(* claude: at the ground or the street, the line under the mouse framed *)
let line_lit (t : t) (c : camera) (e : entry) : shape list =
  match t.pointer with
  | None -> []
  | Some (u, v) -> (
      let mx = to_px c u and my = to_py c v in
      let grounds = if t.street then let s = Map_paint.street_of t e in s.focus :: List.map (fun (p : Code_street.panel) -> p.ground) (Code_street.panels s) else [ Map_paint.ground_of t e ] in
      match List.find_map (fun g -> Option.map (fun l -> (g, l)) (Code_ground.line_at g mx my)) grounds with
      | Some (g, l) ->
          let x, y, w, h = Code_ground.box g l in
          frame c.a (rgb 240 240 250) x y (x +. w) (y +. Float.max h 2.) 1.
      | None -> [])

(* claude: at the ground or the street, the name under the mouse bound
 * in its file: its binding pulsing cyan, its uses yellow, as on the
 * map read up close (Code_map_view.names_lit), placed where the lines are
 * laid out now (Code_ground), in the focus and in the panels; and a use
 * in a line too thin to read magnified while the mouse is there, a
 * callout (the author: "temporarily magnify the calls") -- not the
 * layout changed, which would move the line under the mouse *)
(* claude: a name defined elsewhere, hovered at the ground or the street:
 * the first lines of its definition beside the mouse, readable (the
 * author: "when we hover a use where the entity is not in the view ... a
 * simple hover should probably again show the external def"); a click
 * peeks at all of it *)
let preview_cache : (string * int * float, Rgba_image.t) Hashtbl.t = Hashtbl.create 16

(* claude: a card of code beside the mouse: [path]'s lines [first] to
 * [lastl], readable, painted once (preview_cache), [lit] a line tinted
 * in a colour (a match's) *)
let code_card ?lit (t : t) (c : camera) (path : string) (first : int) (lastl : int) (title : string) (mx : float) (my : float) : shape list =
  match List.find_opt (fun (x : entry) -> x.path = path) (t.entries @ t.beyond) with
  | None -> []
  | Some x ->
      let a = c.a in
      let g = Lazy.force x.file in
      let n = Code_file.nlines g in
      let first = max 0 first and lastl = min (n - 1) lastl in
      let lines = lastl - first + 1 in
      let iw = 560. and ih = float_of_int lines *. 15. in
      let q = Playground_platform.pixel_ratio () in
      let img =
        match Hashtbl.find_opt preview_cache (path, first, q) with
        | Some img when img.height = int_of_float (ih *. q) -> img
        | _ ->
            let weights = Array.init n (fun k -> if k >= first && k <= lastl then 1. else 0.) in
            let lay = Code_ground.layout weights ~pw:(int_of_float iw) ~ph:(int_of_float ih) in
            let img = Rgba_image.create ~width:(int_of_float (iw *. q)) ~height:(int_of_float (ih *. q)) in
            let bg = (22, 20, 38) in
            fill img 0 0 img.width img.height bg;
            Code_ground.paint img g (Code_ground.scale lay q) ~bg ~aa:true;
            Hashtbl.replace preview_cache (path, first, q) img;
            img
      in
      let bw = iw +. 16. and bh = ih +. 34. in
      let x0 = Float.min (mx +. 20.) (float_of_int a.pw -. bw -. 6.) and y0 = Float.min (my +. 20.) (float_of_int a.ph -. bh -. 6.) in
      let rr, gg, bb = archi t.colours path in
      [ rectangle (rgb 22 20 38) bw bh |> move (sx a (x0 +. (bw /. 2.))) (sy a (y0 +. (bh /. 2.))) |> fade 0.97 ]
      @ frame a (lighter (rr, gg, bb)) x0 y0 (x0 +. bw) (y0 +. bh) 1.5
      @ [
          label a (lighter (rr, gg, bb)) 13. (x0 +. 8. +. (text_width 13. title /. 2.)) (y0 +. 13.) title;
          bitmap iw ih img |> move (sx a (x0 +. 8. +. (iw /. 2.))) (sy a (y0 +. 26. +. (ih /. 2.)));
        ]
      @
      match lit with
      | Some (l, colour) when l >= first && l <= lastl ->
          let y = y0 +. 26. +. (float_of_int (l - first) *. 15.) +. 7.5 in
          [ rectangle colour iw 15. |> move (sx a (x0 +. 8. +. (iw /. 2.))) (sy a y) |> fade 0.25 ]
      | _ -> []

let preview (t : t) (c : camera) (path : string) (f : Code_file.t) (l : int) (col : int) (mx : float) (my : float) : shape list =
  match Code_file.ref_at f l col with
  | None -> []
  | Some r -> (
      match Code_names.find_in ~roots:t.roots (index_of t) ~from:path f r with
      | (cand : Code_names.candidate) :: _, _ -> (
          match List.find_opt (fun (x : entry) -> x.path = cand.path) (t.entries @ t.beyond) with
          | None -> []
          | Some x ->
              let g = Lazy.force x.file in
              let n = Code_file.nlines g in
              (* its first lines, to a blank line, eight at most *)
              let rec last k = if k >= n - 1 || k - cand.line >= 7 then k else if Bytes.for_all (fun ch -> ch = '\000' || ch = ' ') (Bytes.sub g.chars ((k + 1) * Code_file.cols) Code_file.cols) then k else last (k + 1) in
              code_card t c cand.path cand.line (last cand.line) (Printf.sprintf "%s:%d   (click: all of it)" cand.path (cand.line + 1)) mx my)
      | [], _ -> [])

let names_glow (t : t) (c : camera) (e : entry) : shape list =
  match t.pointer with
  | None -> []
  | Some _ when t.peek <> None -> []
  | Some (u, v) -> (
      let a = c.a in
      let mx = to_px c u and my = to_py c v in
      let file p = List.find_map (fun (x : entry) -> if x.path = p then Some (Lazy.force x.file) else None) (t.entries @ t.beyond) in
      let grounds =
        if t.street then
          let s = Map_paint.street_of t e in
          (e.path, s.focus) :: List.map (fun (p : Code_street.panel) -> (p.path, p.ground)) (Code_street.panels s)
        else [ (e.path, Map_paint.ground_of t e) ]
      in
      match List.find_map (fun (p, g) -> Option.map (fun l -> (p, g, l)) (Code_ground.line_at g mx my)) grounds with
      | None -> []
      | Some (p, g, l) -> (
          match file p with
          | None -> []
          | Some f -> (
              let x0, _, _, _ = Code_ground.box g l in
              let col = int_of_float ((mx -. x0) /. Code_ground.cell_w g l) in
              match Code_file.name_at f l col with
              | None -> preview t c p f l col mx my
              | Some o ->
                  let occurrences = List.filter (fun (w : Highlight_code.occurrence) -> w.line < Array.length g.places) (Code_file.uses f o) in
                  (* the binding pulsing cyan, the uses yellow *)
                  let lit =
                    List.concat_map
                      (fun (w : Highlight_code.occurrence) ->
                        let x, y, _, h = Code_ground.box g w.line in
                        let cw = Code_ground.cell_w g w.line in
                        let ww = float_of_int w.len *. cw and hh = Float.max 3. h in
                        let px = x +. (float_of_int w.col *. cw) +. (ww /. 2.) and py = y +. (h /. 2.) in
                        let binding = (w.line, w.col) = o.bound_at in
                        List.map (move (sx a px) (sy a py)) (Code_view.glow_at t.clock (if binding then rgb 0 225 255 else yellow) ww hh))
                      occurrences
                  in
                  (* a use in a line too thin to read: a callout, the line's
                   * words round it drawn readable over it, moved down when
                   * it would cover another *)
                  let size = 15. in
                  let placed = ref [] in
                  let callouts =
                    List.filter_map
                      (fun (w : Highlight_code.occurrence) ->
                        let x, y, cwidth, h = Code_ground.box g w.line in
                        if h >= 7. || (w.line, w.col) = o.bound_at then None
                        else
                          let text = String.map (fun ch -> if ch = '\000' then ' ' else ch) (Bytes.sub_string f.chars (w.line * Code_file.cols) Code_file.cols) in
                          (* a use past the grid's width (a long line, cut) shows the line's end *)
                          let from = max 0 (min (String.length text) (w.col - 24)) in
                          let snippet = String.trim (String.sub text from (max 0 (min 64 (String.length text - from)))) in
                          let snippet = (if from > 0 then "... " else "") ^ snippet in
                          let tw = (0.5 *. size *. float_of_int (String.length snippet)) +. 12. in
                          let bw = Float.min tw cwidth and bh = size +. 6. in
                          let bx = x +. 6. in
                          let overlaps by = List.exists (fun (ox, oy) -> Float.abs (oy -. by) < bh && Float.abs (ox -. bx) < bw) !placed in
                          let rec free by k = if k = 0 || not (overlaps by) then by else free (by +. bh +. 2.) (k - 1) in
                          let by = free (y +. (h /. 2.)) 8 in
                          placed := (bx, by) :: !placed;
                          let cx = bx +. (bw /. 2.) in
                          Some
                            (List.map (move (sx a cx) (sy a by)) (Code_view.glow_at t.clock yellow bw bh)
                            @ [
                                rectangle (rgb 18 16 36) bw bh |> move (sx a cx) (sy a by) |> fade 0.9;
                                words ink snippet |> scale (size /. words_font_size) |> move (sx a (bx +. 6. +. ((tw -. 12.) /. 2.))) (sy a by);
                              ]))
                      occurrences
                    |> List.concat
                  in
                  lit @ callouts)))
