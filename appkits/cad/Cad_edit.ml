(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Cad_edit.mli *)

module G = Cad_geom
module D = Cad_drawing

type pt = G.pt

let curve = function
  | D.Line (a, b) -> Some (G.Segment (a, b))
  | D.Circle (c, r) -> Some (G.Circle (c, r))
  | D.Arc (c, r, a0, a1) -> Some (G.Arc (c, r, a0, a1))
  | D.Insert _ | D.Dimension _ -> None

let eps = 1e-6

(*****************************************************************************)
(* OFFSET *)
(*****************************************************************************)

let offset d e ~side =
  match e with
  | D.Line (a, b) ->
      let n = G.unit (let x, y = G.sub b a in (-.y, x)) in
      let n = if G.dot (G.sub side a) n >= 0. then n else G.scale (-1.) n in
      Ok (D.Line (G.add a (G.scale d n), G.add b (G.scale d n)))
  | D.Circle (c, r) | D.Arc (c, r, _, _) ->
      let r' = if G.dist c side < r then r -. d else r +. d in
      if r' <= 0. then Error "Cannot offset that object."
      else Ok (match e with D.Arc (_, _, a0, a1) -> D.Arc (c, r', a0, a1) | _ -> D.Circle (c, r'))
  | D.Insert _ | D.Dimension _ -> Error "Cannot offset that object."

(*****************************************************************************)
(* TRIM *)
(*****************************************************************************)

(* the cuts before and after [x] among [cuts] (all in (0, top)) *)
let around cuts x =
  let before = List.fold_left (fun m c -> if c < x then Float.max m c else m) neg_infinity cuts in
  let after = List.fold_left (fun m c -> if c > x then Float.min m c else m) infinity cuts in
  (before, after)

let trim edges e ~at =
  match curve e with
  | None -> Error "Cannot trim this object."
  | Some c -> (
      let crossings = List.concat_map (fun edge -> G.intersections c edge) edges in
      match (e, c) with
      | D.Line (a, b), _ ->
          let cuts = List.filter (fun t -> t > eps && t < 1. -. eps) (List.map (G.param a b) crossings) in
          if cuts = [] then Error "Object does not intersect an edge."
          else
            let lo, hi = around cuts (G.param a b at) in
            let point t = G.add a (G.scale t (G.sub b a)) in
            Ok ((if lo > neg_infinity then [ D.Line (a, point lo) ] else []) @ if hi < infinity then [ D.Line (point hi, b) ] else [])
      | D.Circle (center, r), _ ->
          let cuts = List.sort_uniq compare (List.map (G.angle center) crossings) in
          if List.length cuts < 2 then Error "Circle must intersect twice."
          else
            (* the angles turned to start just past the click *)
            let from = G.angle center at in
            let rel a = G.norm_angle (a -. from) in
            let next = List.fold_left (fun m a -> if rel a < rel m then a else m) (List.hd cuts) cuts in
            let prev = List.fold_left (fun m a -> if rel a > rel m then a else m) (List.hd cuts) cuts in
            (* what stays goes on from the cut after the click round to
               the one before it *)
            Ok [ D.Arc (center, r, next, prev) ]
      | D.Arc (center, r, a0, a1), _ ->
          let span = G.norm_angle (a1 -. a0) in
          let off a = G.norm_angle (a -. a0) in
          let cuts = List.filter (fun x -> x > eps && x < span -. eps) (List.map (fun p -> off (G.angle center p)) crossings) in
          if cuts = [] then Error "Object does not intersect an edge."
          else
            let lo, hi = around cuts (off (G.angle center at)) in
            Ok
              ((if lo > neg_infinity then [ D.Arc (center, r, a0, G.norm_angle (a0 +. lo)) ] else [])
              @ if hi < infinity then [ D.Arc (center, r, G.norm_angle (a0 +. hi), a1) ] else [])
      | _ -> Error "Cannot trim this object.")

(*****************************************************************************)
(* EXTEND *)
(*****************************************************************************)

let extend edges e ~at =
  match e with
  | D.Line (a, b) -> (
      (* the end nearer the click moves: from the other one, along *)
      let fixed, moving = if G.dist at a < G.dist at b then (b, a) else (a, b) in
      let carrier = G.Segment (fixed, moving) in
      let hits =
        List.concat_map
          (fun edge -> List.filter (fun p -> G.on_piece edge p) (G.carrier_intersections carrier edge))
          edges
        |> List.map (G.param fixed moving)
        |> List.filter (fun t -> t > 1. +. eps)
      in
      match hits with
      | [] -> Error "No edge in that direction."
      | t :: ts ->
          let t = List.fold_left Float.min t ts in
          let p = G.add fixed (G.scale t (G.sub moving fixed)) in
          Ok (if moving = a then D.Line (p, b) else D.Line (a, p)))
  | _ -> Error "Cannot extend this object."

(*****************************************************************************)
(* FILLET *)
(*****************************************************************************)

let fillet r (e1, q1) (e2, q2) =
  match (e1, e2) with
  | D.Line (a1, b1), D.Line (a2, b2) -> (
      match G.carrier_intersections (G.Segment (a1, b1)) (G.Segment (a2, b2)) with
      | [] -> Error "Lines are parallel."
      | p :: _ ->
          (* along each line, towards the part clicked, and that part's
             far end *)
          let side a b q =
            let u = G.unit (G.sub (G.foot (G.Segment (a, b)) q) p) in
            let far = if G.dot (G.sub a p) u >= G.dot (G.sub b p) u then a else b in
            (u, far)
          in
          let u1, f1 = side a1 b1 q1 and u2, f2 = side a2 b2 q2 in
          if r <= 0. then Ok (D.Line (f1, p), D.Line (f2, p), None)
          else
            let theta = Float.acos (Float.max (-1.) (Float.min 1. (G.dot u1 u2))) in
            let d = r /. Float.tan (theta /. 2.) in
            if d > G.dist p f1 || d > G.dist p f2 then Error "Radius is too large."
            else
              let t1 = G.add p (G.scale d u1) and t2 = G.add p (G.scale d u2) in
              let c = G.add p (G.scale (r /. Float.sin (theta /. 2.)) (G.unit (G.add u1 u2))) in
              let a1 = G.angle c t1 and a2 = G.angle c t2 in
              (* the short way round, counterclockwise *)
              let arc = if G.norm_angle (a2 -. a1) <= 180. then D.Arc (c, r, a1, a2) else D.Arc (c, r, a2, a1) in
              Ok (D.Line (f1, t1), D.Line (f2, t2), Some arc))
  | _ -> Error "Can only fillet lines."
