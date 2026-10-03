(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Map_peek.mli *)

open Playground
open Code_map_base

(* claude: a definition's body, read over the map (a click at the ground
 * or the street, Code_map: t.peek): its lines alone laid out by
 * Code_ground, the letters as big as the box allows (18 pixels at most),
 * painted once, at the window's resolution *)
let peek_images : (string * int * int * float, Rgba_image.t) Hashtbl.t = Hashtbl.create 8

(* the peek on the map: its entry and file, its lines' layout (from the
 * box's inner corner, the window the scroll shows: 17 pixels a line),
 * the box and its inner corner, the lines asked for and those shown *)
type peek = {
  pe : entry;
  pf : Code_file.t;
  pg : Code_ground.t;
  bx : float;
  by : float;
  bw : float;
  bh : float;
  ix : float;
  iy : float;
  iw : float;
  ih : float;
  first : int;
  last : int;
  shown_first : int;
  shown_last : int;
}

(* a peek at a depth in the stack: each one shifted right and down, and
 * smaller, so that the ones under it show *)
let geom_of (t : t) (c : camera) ((path, first, last) : string * int * int) (scroll : int) (depth : int) : peek option =
  let shift = 30. *. float_of_int depth in
  (
      match Map_skeleton.entry_of t path with
      | None -> None
      | Some e ->
          let a = c.a in
          let f = Lazy.force e.file in
          let n = Code_file.nlines f in
          let first = max 0 first and last = min (n - 1) last in
          let lines = last - first + 1 in
          (* claude: as wide as its longest line, up to the screen (the
           * author: in a peek, the full line) -- 8.5 pixels a character *)
          let longest =
            List.fold_left
              (fun m l ->
                let k = ref 0 in
                for c = 0 to Code_file.cols - 1 do if Bytes.get f.grid ((l * Code_file.cols) + c) <> '\000' then k := c + 1 done;
                max m !k)
              0 (List.init (last - first + 1) (fun k -> first + k))
          in
          let bw = Float.min (float_of_int a.pw -. 80. -. shift) (Float.max 820. ((float_of_int longest *. 8.5) +. 48.)) and bh = Float.min (float_of_int a.ph -. 60. -. shift) ((float_of_int lines *. 17.) +. 56.) in
          let iw = bw -. 24. and ih = bh -. 48. in
          (* the window: as many lines as fit at 17 pixels, from the scroll *)
          let cap = max 1 (int_of_float (ih /. 17.)) in
          let shown_first = first + max 0 (min scroll (lines - cap)) in
          let shown_last = min last (shown_first + cap - 1) in
          let weights = Array.init n (fun l -> if l >= shown_first && l <= shown_last then 1. else 0.) in
          let pg = Code_ground.layout weights ~pw:(int_of_float iw) ~ph:(int_of_float ih) in
          let cx = float_of_int a.pw /. 2. and cy = float_of_int a.ph /. 2. in
          let bx = cx -. (bw /. 2.) +. shift and by = cy -. (bh /. 2.) +. (shift /. 2.) in
          Some { pe = e; pf = f; pg; bx; by; bw; bh; ix = bx +. 12.; iy = by +. 36.; iw; ih; first; last; shown_first; shown_last })

let peek_geom (t : t) (c : camera) : peek option =
  match t.peek with Some top -> geom_of t c top t.peek_scroll (List.length t.peek_stack) | None -> None

let inside_peek (pk : peek) (x : float) (y : float) = x >= pk.bx && x < pk.bx +. pk.bw && y >= pk.by && y < pk.by +. pk.bh

(* a name's occurrences glowing in a layout moved by (dx, dy): the binding
 * pulsing cyan, the uses yellow, on the lines [shown] *)
let glows (t : t) (a : area) (g : Code_ground.t) ((dx, dy) : float * float) (f : Code_file.t) (o : Highlight_code.occurrence) ~(shown : int -> bool) :
    shape list =
  List.concat_map
    (fun (w : Highlight_code.occurrence) ->
      if w.line >= Array.length g.places || not (shown w.line) || g.places.(w.line).h < 0.5 then []
      else
        let x, y, _, h = Code_ground.box g w.line in
        let cw = Code_ground.cell_w g w.line in
        let ww = float_of_int w.len *. cw and hh = Float.max 3. h in
        let px = dx +. x +. (float_of_int w.col *. cw) +. (ww /. 2.) and py = dy +. y +. (h /. 2.) in
        let binding = (w.line, w.col) = o.bound_at in
        List.map (move (sx a px) (sy a py)) (Code_view.glow_at t.clock (if binding then rgb 0 225 255 else yellow) ww hh))
    (Code_file.uses f o)

let peek_level (t : t) (c : camera) (q : float) (pk : peek) ~(top : bool) : shape list =
  let a = c.a in
  let path = pk.pe.path in
  let key = (path, pk.shown_first, pk.shown_last, q) in
  let img =
    match Hashtbl.find_opt peek_images key with
    | Some img -> img
    | None ->
        let img = Rgba_image.create ~width:(int_of_float (pk.iw *. q)) ~height:(int_of_float (pk.ih *. q)) in
        let bg = (22, 20, 38) in
        fill img 0 0 img.width img.height bg;
        Code_ground.paint img pk.pf (Code_ground.scale pk.pg q) ~bg ~aa:true;
        Hashtbl.replace peek_images key img;
        img
  in
  let cx = pk.bx +. (pk.bw /. 2.) and cy = pk.by +. (pk.bh /. 2.) in
  let name = List.fold_left (fun acc (l, nm, _) -> if l = pk.first then Some nm else acc) None pk.pf.defs in
  let more = pk.shown_first > pk.first || pk.shown_last < pk.last in
  let hint =
    (if more then Printf.sprintf "lines %d-%d of %d, the wheel scrolls; " (pk.shown_first - pk.first + 1) (pk.shown_last - pk.first + 1) (pk.last - pk.first + 1) else "")
    ^ if top then "a name: its definition; outside or Escape: back" else ""
  in
  let title = Printf.sprintf "%s:%d%s%s" path (pk.first + 1) (match name with Some nm -> "  " ^ nm | None -> "") (if hint = "" then "" else "   (" ^ hint ^ ")") in
  let r, g, b = archi t.colours path in
  [ rectangle (rgb 22 20 38) pk.bw pk.bh |> move (sx a cx) (sy a cy) ]
  @ frame a (lighter (r, g, b)) pk.bx pk.by (pk.bx +. pk.bw) (pk.by +. pk.bh) (if top then 2. else 1.)
  @ [
      label a (lighter (r, g, b)) 14. (pk.bx +. 12. +. (0.25 *. 14. *. float_of_int (String.length title))) (pk.by +. 18.) title;
      bitmap pk.iw pk.ih img |> move (sx a (pk.ix +. (pk.iw /. 2.))) (sy a (pk.iy +. (pk.ih /. 2.)));
    ]
  (* claude: the X-ray's nerves and lungs on, their lines tinted in the
   * peek too, and a bar in its margin (the author: the enclosing
   * entity's body, bigger, the nerves and lungs still lit) *)
  @ (if not t.xray then []
     else
       match Map_anatomy.facts_of t pk.pe with
       | None -> []
       | Some (fs : Code_anatomy.facts) ->
           let on s = List.mem s !Code_anatomy.shown in
           let lit s ls =
             if not (on s) then []
             else
               let r, g, b = Code_anatomy.colour s in
               List.concat_map
                 (fun l ->
                   if l < pk.shown_first || l > pk.shown_last || l >= Array.length pk.pg.places then []
                   else
                     let x0, y0, w, h = Code_ground.box pk.pg l in
                     let h = Float.max 3. h in
                     let y = pk.iy +. y0 +. (h /. 2.) in
                     [ rectangle (rgb r g b) w h |> move (sx a (pk.ix +. x0 +. (w /. 2.))) (sy a y) |> fade 0.3; rectangle (rgb r g b) 5. h |> move (sx a (pk.bx +. 5.)) (sy a y) ])
                 ls
           in
           lit Code_anatomy.Nerves fs.nerves @ lit Code_anatomy.Lungs fs.lungs)
  (* a peek under another, dimmed *)
  @ if top then [] else [ rectangle (rgb 0 0 0) pk.bw pk.bh |> move (sx a cx) (sy a cy) |> fade 0.35 ]

let peek_shapes (t : t) (c : camera) (q : float) : shape list =
  match t.peek with
  | None -> []
  | Some top ->
      let a = c.a in
      let full_w = float_of_int a.pw and full_h = float_of_int a.ph in
      let n = List.length t.peek_stack in
      let under =
        List.concat
          (List.mapi
             (fun k (pk, scroll) -> match geom_of t c pk scroll (n - 1 - k) with Some g -> peek_level t c q g ~top:false | None -> [])
             (List.rev t.peek_stack))
      in
      (rectangle (rgb 0 0 0) full_w full_h |> move (sx a (full_w /. 2.)) (sy a (full_h /. 2.)) |> fade 0.45)
      :: under
      @ match geom_of t c top t.peek_scroll n with Some g -> peek_level t c q g ~top:true | None -> []

(* claude: a name hovered in the peek: its binding and uses glowing in
 * the peek, and outside it on the map, where the same file is laid out *)
let peek_glow (t : t) (c : camera) : shape list =
  match (peek_geom t c, t.pointer) with
  | Some pk, Some (u, v) -> (
      let a = c.a in
      let mx = to_px c u and my = to_py c v in
      if not (inside_peek pk mx my) then []
      else
        match Code_ground.line_at pk.pg (mx -. pk.ix) (my -. pk.iy) with
        | None -> []
        | Some l -> (
            let x0, _, _, _ = Code_ground.box pk.pg l in
            let col = int_of_float ((mx -. pk.ix -. x0) /. Code_ground.cell_w pk.pg l) in
            match Code_file.name_at pk.pf l col with
            | None -> Map_cards.preview t c pk.pe.path pk.pf l col mx my
            | Some o ->
                let outside =
                  match Map_paint.at_ground t c with
                  | Some e ->
                      let gs = if t.street then let s = Map_paint.street_of t e in (e.path, s.focus) :: List.map (fun (p : Code_street.panel) -> (p.path, p.ground)) (Code_street.panels s) else [ (e.path, Map_paint.ground_of t e) ] in
                      List.concat_map (fun (p, g) -> if p = pk.pe.path then glows t a g (0., 0.) pk.pf o ~shown:(fun _ -> true) else []) gs
                  | None -> []
                in
                outside @ glows t a pk.pg (pk.ix, pk.iy) pk.pf o ~shown:(fun l -> l >= pk.shown_first && l <= pk.shown_last)))
  | _ -> []
