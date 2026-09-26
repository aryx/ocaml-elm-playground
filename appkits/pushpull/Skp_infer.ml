(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Skp_infer.mli *)

module M = Skp_model

type kind = Endpoint | Midpoint | On_axis of int | On_edge | On_face | Nowhere
type found = { point : Vec3.t; kind : kind; vertex : int option; edge : (int * int) option; face : int option }

let name = function
  | Endpoint -> "Endpoint"
  | Midpoint -> "Midpoint"
  | On_axis 0 -> "On Red Axis"
  | On_axis 1 -> "On Green Axis"
  | On_axis _ -> "On Blue Axis"
  | On_edge -> "On Edge"
  | On_face -> "On Face"
  | Nowhere -> ""

let axis i = match i with 0 -> (1., 0., 0.) | 1 -> (0., 1., 0.) | _ -> (0., 0., 1.)

(* the two nearest points of two lines, o + s r and p + u w: the one
   on the second line, as its u (Sunday's formula) *)
let along (o, r) p w =
  let w0 = Vec3.sub o p in
  let a = Vec3.dot r r and b = Vec3.dot r w and c = Vec3.dot w w in
  let d = Vec3.dot r w0 and e = Vec3.dot w w0 in
  let denom = (a *. c) -. (b *. b) in
  if Float.abs denom < 1e-12 then 0. else ((a *. e) -. (b *. d)) /. denom

let on_plane (o, r) p n =
  let denom = Vec3.dot r n in
  if Float.abs denom < 1e-9 then None
  else
    let s = Vec3.dot (Vec3.sub p o) n /. denom in
    if s <= 0. then None else Some (Vec3.add o (Vec3.scale s r))

let find ~project ~ray ?from ?(tolerance = 10.) (model : M.t) (mx, my) =
  let eye, dir = ray in
  let face = Option.map (fun (_, (f : M.face)) -> f.id) (M.hit model eye dir) in
  let on_screen p = match project p with Some (x, y) -> Some (Float.hypot (x -. mx) (y -. my)) | None -> None in
  (* nothing between the eye and p (the faces through p meet the ray at
     p itself, a whole length along it) *)
  let visible p = match M.hit model eye (Vec3.sub p eye) with Some (s, _) -> s > 1. -. 1e-4 | None -> true in
  (* the nearest on the screen of the candidates within tolerance *)
  let nearest candidates =
    List.fold_left
      (fun best (p, x) ->
        match on_screen p with
        | Some d when d <= tolerance && visible p -> ( match best with Some (_, _, bd) when bd <= d -> best | _ -> Some (p, x, d))
        | _ -> best)
      None candidates
  in
  let found point kind ?vertex ?edge () = { point; kind; vertex; edge; face } in
  let ends (e : M.edge) = (M.pos model e.a, M.pos model e.b) in
  let endpoint = nearest (List.map (fun (v, p) -> (p, v)) model.verts) in
  let midpoint = lazy (nearest (List.map (fun (e : M.edge) -> let p, q = ends e in (Vec3.scale 0.5 (Vec3.add p q), (e.a, e.b))) model.edges)) in
  let on_axis =
    lazy
      (match from with
      | None -> None
      | Some f -> nearest (List.init 3 (fun i -> (Vec3.add f (Vec3.scale (along ray f (axis i)) (axis i)), i))))
  in
  let on_edge =
    lazy
      (nearest
         (List.map
            (fun (e : M.edge) ->
              let p, q = ends e in
              let s = Float.max 0. (Float.min 1. (along ray p (Vec3.sub q p))) in
              (Vec3.add p (Vec3.scale s (Vec3.sub q p)), (e.a, e.b)))
            model.edges))
  in
  match endpoint with
  | Some (p, v, _) -> found p Endpoint ~vertex:v ()
  | None -> (
      match Lazy.force midpoint with
      | Some (p, e, _) -> found p Midpoint ~edge:e ()
      | None -> (
          match Lazy.force on_axis with
          | Some (p, i, _) -> found p (On_axis i) ()
          | None -> (
              match Lazy.force on_edge with
              | Some (p, e, _) -> found p On_edge ~edge:e ()
              | None -> (
                  match M.hit model eye dir with
                  | Some (s, _) -> found (Vec3.add eye (Vec3.scale s dir)) On_face ()
                  | None ->
                      (* the ground, or the level of the last point *)
                      let level = match from with Some f -> f | None -> (0., 0., 0.) in
                      let p = match on_plane ray level (0., 0., 1.) with Some p -> p | None -> Vec3.add eye (Vec3.scale 10. dir) in
                      found p Nowhere ()))))
