(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Cad_drawing.mli *)

module G = Cad_geom

type pt = G.pt

type entity =
  | Line of pt * pt
  | Circle of pt * float
  | Arc of pt * float * float * float
  | Insert of string * pt * float * float
  | Dimension of pt * pt * pt

type ent = { entity : entity; layer : string }
type layer = { name : string; color : int; on : bool }

type t = {
  ents : (int * ent) list;
  layers : layer list;
  current : string;
  blocks : (string * (pt * ent list)) list;
  next : int;
}

let empty = { ents = []; layers = [ { name = "0"; color = 7; on = true } ]; current = "0"; blocks = []; next = 1 }
let add e t = ({ t with ents = t.ents @ [ (t.next, { entity = e; layer = t.current }) ]; next = t.next + 1 }, t.next)
let get t id = List.assoc_opt id t.ents
let remove ids t = { t with ents = List.filter (fun (i, _) -> not (List.mem i ids)) t.ents }

let replace id es t =
  match get t id with
  | None -> t
  | Some old ->
      (* the new ones where the old one was, in depth *)
      let fresh = List.mapi (fun k e -> (t.next + k, { entity = e; layer = old.layer })) es in
      { t with ents = List.concat_map (fun (i, e) -> if i = id then fresh else [ (i, e) ]) t.ents; next = t.next + List.length es }

let ids t = List.map fst t.ents
let layer t name = match List.find_opt (fun (l : layer) -> l.name = name) t.layers with Some l -> l | None -> { name; color = 7; on = true }

let ensure_layer name t =
  if List.exists (fun (l : layer) -> l.name = name) t.layers then t else { t with layers = t.layers @ [ { name; color = 7; on = true } ] }

let set_layer (l : layer) t =
  let t = ensure_layer l.name t in
  { t with layers = List.map (fun (l' : layer) -> if l'.name = l.name then l else l') t.layers }

(*****************************************************************************)
(* Pieces *)
(*****************************************************************************)

let transform at scale rot p =
  let x, y = G.scale scale p in
  let a = rot *. Float.pi /. 180. in
  G.add at ((x *. Float.cos a) -. (y *. Float.sin a), (x *. Float.sin a) +. (y *. Float.cos a))

let move_entity d = function
  | Line (a, b) -> Line (G.add a d, G.add b d)
  | Circle (c, r) -> Circle (G.add c d, r)
  | Arc (c, r, a0, a1) -> Arc (G.add c d, r, a0, a1)
  | Insert (n, at, s, rot) -> Insert (n, G.add at d, s, rot)
  | Dimension (p1, p2, l) -> Dimension (G.add p1 d, G.add p2 d, G.add l d)

(* the dimension's sizes, in the drawing's units (DIMEXO, DIMEXE,
   DIMASZ: a drawing in millimetres) *)
let dimexo = 1.5
let dimexe = 2.
let dimasz = 3.

let horizontal (x1, y1) (x2, y2) (lx, ly) =
  let mx = (x1 +. x2) /. 2. and my = (y1 +. y2) /. 2. in
  Float.abs (ly -. my) >= Float.abs (lx -. mx)

let dimension_value ((x1, y1) as p1) ((x2, y2) as p2) l =
  if horizontal p1 p2 l then Float.abs (x2 -. x1) else Float.abs (y2 -. y1)

(* an open arrowhead at p, pointing along the direction a *)
let arrow p a = [ (p, G.polar p dimasz (a +. 160.)); (p, G.polar p dimasz (a -. 160.)) ]

let dimension_lines ((x1, y1) as p1) ((x2, y2) as p2) ((lx, ly) as l) =
  let ext (px, py) (qx, qy) =
    (* from near the point to a little past the dimension line *)
    let d = G.unit (G.sub (qx, qy) (px, py)) in
    if G.dist (px, py) (qx, qy) <= dimexo then [] else [ (G.add (px, py) (G.scale dimexo d), G.add (qx, qy) (G.scale dimexe d)) ]
  in
  let a, b = if horizontal p1 p2 l then ((x1, ly), (x2, ly)) else ((lx, y1), (lx, y2)) in
  let dir = G.angle a b in
  ext p1 a @ ext p2 b @ [ (a, b) ] @ arrow a (dir +. 180.) @ arrow b dir

let format v =
  let s = Printf.sprintf "%.2f" v in
  let rec strip s = if String.ends_with ~suffix:"0" s then strip (String.sub s 0 (String.length s - 1)) else s in
  let s = strip s in
  if String.ends_with ~suffix:"." s then String.sub s 0 (String.length s - 1) else s

let dimension_text ((x1, y1) as p1) ((x2, y2) as p2) ((lx, ly) as l) =
  let v = format (dimension_value p1 p2 l) in
  if horizontal p1 p2 l then (((x1 +. x2) /. 2., ly +. 3.5), v) else ((lx, (y1 +. y2) /. 2.), v)

let rec pieces_depth t depth (e : ent) =
  match e.entity with
  | Line (a, b) -> [ (G.Segment (a, b), e.layer) ]
  | Circle (c, r) -> [ (G.Circle (c, r), e.layer) ]
  | Arc (c, r, a0, a1) -> [ (G.Arc (c, r, a0, a1), e.layer) ]
  | Dimension (p1, p2, l) -> List.map (fun (a, b) -> (G.Segment (a, b), e.layer)) (dimension_lines p1 p2 l)
  | Insert (name, at, s, rot) when depth > 0 -> (
      match List.assoc_opt name t.blocks with
      | None -> []
      | Some (base, es) ->
          let f p = transform at s rot (G.sub p base) in
          List.concat_map
            (fun (inner : ent) ->
              let layer = if inner.layer = "0" then e.layer else inner.layer in
              List.map
                (fun (c, l) ->
                  let c =
                    match c with
                    | G.Segment (a, b) -> G.Segment (f a, f b)
                    | G.Circle (c, r) -> G.Circle (f c, r *. Float.abs s)
                    | G.Arc (c, r, a0, a1) -> G.Arc (f c, r *. Float.abs s, a0 +. rot, a1 +. rot)
                  in
                  (c, l))
                (pieces_depth t (depth - 1) { inner with layer }))
            es)
  | Insert _ -> []

(* a block inside itself goes no deeper than this *)
let pieces t e = pieces_depth t 8 e

(*****************************************************************************)
(* Boxes, picking, windows *)
(*****************************************************************************)

let box_of = function
  | G.Segment ((ax, ay), (bx, by)) -> ((Float.min ax bx, Float.min ay by), (Float.max ax bx, Float.max ay by))
  | G.Circle ((cx, cy), r) -> ((cx -. r, cy -. r), (cx +. r, cy +. r))
  | G.Arc (c, r, a0, a1) ->
      let s, e = G.arc_ends c r a0 a1 in
      let quads = List.filter (fun a -> G.within a0 a1 a) [ 0.; 90.; 180.; 270. ] in
      let ps = s :: e :: List.map (G.polar c r) quads in
      let xs = List.map fst ps and ys = List.map snd ps in
      let mn = List.fold_left Float.min infinity and mx = List.fold_left Float.max neg_infinity in
      ((mn xs, mn ys), (mx xs, mx ys))

let visible t (e : ent) = (layer t e.layer).on
let all_pieces t = List.concat_map (fun (id, e) -> if visible t e then List.map (fun (c, l) -> (id, c, l)) (pieces t e) else []) t.ents

let extents t =
  List.fold_left
    (fun acc (_, c, _) ->
      let (x0, y0), (x1, y1) = box_of c in
      match acc with
      | None -> Some ((x0, y0), (x1, y1))
      | Some ((a, b), (c, d)) -> Some ((Float.min a x0, Float.min b y0), (Float.max c x1, Float.max d y1)))
    None (all_pieces t)

let pick t tolerance p =
  List.fold_left
    (fun best (id, c, _) ->
      let d = G.distance c p in
      if d > tolerance then best else match best with Some (_, db) when db < d -> best | _ -> Some (id, d))
    None (all_pieces t)
  |> Option.map fst

let window t (ax, ay) (bx, by) ~crossing =
  let x0 = Float.min ax bx and x1 = Float.max ax bx and y0 = Float.min ay by and y1 = Float.max ay by in
  let inside c =
    let (a, b), (c, d) = box_of c in
    a >= x0 && c <= x1 && b >= y0 && d <= y1
  in
  let edges = [ G.Segment ((x0, y0), (x1, y0)); G.Segment ((x1, y0), (x1, y1)); G.Segment ((x1, y1), (x0, y1)); G.Segment ((x0, y1), (x0, y0)) ] in
  let touches c = inside c || List.exists (fun e -> G.intersections c e <> []) edges in
  List.filter_map
    (fun (id, e) ->
      if not (visible t e) then None
      else
        let ps = List.map fst (pieces t e) in
        if ps <> [] && (if crossing then List.exists touches ps else List.for_all inside ps) then Some id else None)
    t.ents
