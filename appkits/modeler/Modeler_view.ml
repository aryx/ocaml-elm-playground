(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Modeler_view.mli *)

type view = Top | Front | Right | Camera_view

let name = function Top -> "Top Orthographic" | Front -> "Front Orthographic" | Right -> "Right Orthographic" | Camera_view -> "Camera Perspective"
let axes = function Top -> (0, 1) | Front -> (0, 2) | Right | Camera_view -> (1, 2)

(* looking down -z from the top, a turn counterclockwise on the screen
   is +z; from the front (looking along +y), it goes from x to z, -y;
   from the right (along -x), from y to z, +x *)
let normal = function Top | Camera_view -> (2, 1.) | Front -> (1, -1.) | Right -> (0, 1.)
let coord i (x, y, z) = match i with 0 -> x | 1 -> y | _ -> z

let project view p =
  let a, b = axes view in
  (coord a p, coord b p)

let unproject view (du, dv) =
  let a, b = axes view in
  let along i = (if a = i then du else 0.) +. if b = i then dv else 0. in
  (along 0, along 1, along 2)

let camera ?(fov = 40.) (t : Modeler.t) ~target : Camera.t =
  let eye = match List.find_opt (fun (o : Modeler.obj) -> o.kind = Modeler.Camera) t with Some o -> o.location | None -> (7.4, -6.9, 5.) in
  { eye = Modeler.to_world eye; target = Modeler.to_world target; up = (0., 1., 0.); fov; ortho = 0.; near = 0.; far = infinity }

let perspective camera ~aspect p =
  let (_, _, z) as v = Camera.view camera (Modeler.to_world p) in
  if z <= 1e-3 then None else Camera.ndc camera ~aspect v

(* the distance from p to the segment from a to b, on the screen *)
let to_segment (px, py) (ax, ay) (bx, by) =
  let dx = bx -. ax and dy = by -. ay in
  let l2 = (dx *. dx) +. (dy *. dy) in
  let t = if l2 = 0. then 0. else Float.max 0. (Float.min 1. ((((px -. ax) *. dx) +. ((py -. ay) *. dy)) /. l2)) in
  Float.hypot (px -. (ax +. (t *. dx))) (py -. (ay +. (t *. dy)))

let pick ~to_screen ~tolerance (t : Modeler.t) p =
  List.fold_left
    (fun best (o : Modeler.obj) ->
      if o.hidden then best
      else
        let d =
          List.fold_left
            (fun m (a, b) -> match (to_screen a, to_screen b) with Some a, Some b -> Float.min m (to_segment p a b) | _ -> m)
            infinity (Modeler.wires o)
        in
        if d > tolerance then best else match best with Some (_, db) when db <= d -> best | _ -> Some (o.name, d))
    None t
  |> Option.map fst
