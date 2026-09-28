(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Cad_snap.mli *)

module G = Cad_geom
module D = Cad_drawing

type kind = Endpoint | Midpoint | Center | Quadrant | Intersection | Perpendicular

let name = function
  | Endpoint -> "Endpoint"
  | Midpoint -> "Midpoint"
  | Center -> "Center"
  | Quadrant -> "Quadrant"
  | Intersection -> "Intersection"
  | Perpendicular -> "Perpendicular"

(* a piece's own points, of each kind *)
let points = function
  | G.Segment (a, b) -> [ (Endpoint, a); (Endpoint, b); (Midpoint, G.scale 0.5 (G.add a b)) ]
  | G.Circle (c, r) -> (Center, c) :: List.map (fun a -> (Quadrant, G.polar c r a)) [ 0.; 90.; 180.; 270. ]
  | G.Arc (c, r, a0, a1) ->
      let s, e = G.arc_ends c r a0 a1 in
      let mid = G.polar c r (a0 +. (G.norm_angle (a1 -. a0) /. 2.)) in
      [ (Endpoint, s); (Endpoint, e); (Midpoint, mid); (Center, c) ]
      @ List.filter_map (fun a -> if G.within a0 a1 a then Some (Quadrant, G.polar c r a) else None) [ 0.; 90.; 180.; 270. ]

let find (t : D.t) ~aperture ?from p =
  let pieces = List.concat_map (fun (_, (e : D.ent)) -> if (D.layer t e.layer).on then List.map fst (D.pieces t e) else []) t.ents in
  (* a center is caught from the rim too, as AutoCAD's was *)
  let near c = G.distance c p <= aperture || match c with G.Circle (c, _) | G.Arc (c, _, _, _) -> G.dist c p <= aperture | _ -> false in
  let close = List.filter near pieces in
  let own = List.concat_map points close in
  let rec pairs = function [] -> [] | c :: cs -> List.map (fun d -> (c, d)) cs @ pairs cs in
  let crossings = List.concat_map (fun (c, d) -> List.map (fun q -> (Intersection, q)) (G.intersections c d)) (pairs close) in
  let best cands =
    List.fold_left
      (fun acc (k, q) ->
        let d = G.dist p q in
        if d > aperture then acc else match acc with Some (_, _, db) when db <= d -> acc | _ -> Some (k, q, d))
      None cands
    |> Option.map (fun (k, q, _) -> (k, q))
  in
  let under = List.filter (fun c -> G.distance c p <= aperture) close in
  match best (own @ crossings) with
  | Some s -> Some s
  | None -> (
      (* the cursor on a circle's rim, far from any of its points: its
         center, as AutoCAD's CENter caught it *)
      match List.find_map (function G.Circle (c, _) | G.Arc (c, _, _, _) -> Some c | G.Segment _ -> None) under with
      | Some c -> Some (Center, c)
      | None -> (
          match (from, under) with
          | Some f, c :: _ ->
              let q = G.foot c f in
              if G.on_piece c q then Some (Perpendicular, q) else None
          | _ -> None))
