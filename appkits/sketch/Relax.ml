(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Relax.mli *)

open Sketch

(*****************************************************************************)
(* The errors *)
(*****************************************************************************)

(* the error of [c], the points' positions read through [get] *)
let error_with (s : sheet) (get : int -> pos) (c : constr) : float =
  let vec l =
    match ends s l with
    | Some (a, b) ->
        let ax, ay = get a and bx, by = get b in
        (bx -. ax, by -. ay)
    | None -> (0., 0.)
  in
  let len (x, y) = Float.hypot x y in
  (* an angle's error as a length: the sine (or the cosine) of the angle
     between the lines, times their mean length *)
  let angular f l m =
    let ((ux, uy) as u) = vec l and ((vx, vy) as v) = vec m in
    let mean = (len u +. len v) /. 2. in
    if len u = 0. || len v = 0. then 0. else f ux uy vx vy /. (len u *. len v) *. mean
  in
  match c with
  | Horizontal l -> snd (vec l)
  | Vertical l -> fst (vec l)
  | Parallel (l, m) -> angular (fun ux uy vx vy -> (ux *. vy) -. (uy *. vx)) l m
  | Perpendicular (l, m) -> angular (fun ux uy vx vy -> (ux *. vx) +. (uy *. vy)) l m
  | Equal (l, m) -> len (vec l) -. len (vec m)
  | On_line (p, l) -> (
      match ends s l with
      | Some (a, _) ->
          let dx, dy = vec l and ax, ay = get a and px, py = get p in
          if len (dx, dy) = 0. then 0. else ((dx *. (py -. ay)) -. (dy *. (px -. ax))) /. len (dx, dy)
      | None -> 0.)
  | On_circle (p, k) -> (
      match ends s k with
      | Some (c, _) ->
          let cx, cy = get c and px, py = get p in
          Float.hypot (px -. cx) (py -. cy) -. len (vec k)
      | None -> 0.)

let error_of s c = error_with s (pos s) c
let error s = List.fold_left (fun sum c -> sum +. Float.abs (error_of s c)) 0. s.constraints

(*****************************************************************************)
(* Relaxation *)
(*****************************************************************************)

(* the trial move, for the derivatives *)
let h = 1e-3

(* the damping, a little on the diagonal *)
let lambda = 1e-6

let sweep ?(held = []) (s : sheet) : sheet =
  let here = Hashtbl.create 64 in
  List.iter (fun (id, p) -> Hashtbl.replace here id p) s.points;
  let get id = match Hashtbl.find_opt here id with Some p -> p | None -> (0., 0.) in
  (* each point's constraints, found once *)
  let on id = List.filter (fun c -> List.mem id (points_of s c)) s.constraints in
  List.iter
    (fun (id, _) ->
      if not (List.mem id s.fixed || List.mem id held) then
        match on id with
        | [] -> ()
        | cs ->
            let x, y = get id in
            let err at c =
              Hashtbl.replace here id at;
              error_with s get c
            in
            (* the sums of the normal equations *)
            let saa, sab, sbb, sae, sbe, worst =
              List.fold_left
                (fun (saa, sab, sbb, sae, sbe, worst) c ->
                  let e = err (x, y) c in
                  let a = (err (x +. h, y) c -. e) /. h and b = (err (x, y +. h) c -. e) /. h in
                  (saa +. (a *. a), sab +. (a *. b), sbb +. (b *. b), sae +. (a *. e), sbe +. (b *. e), Float.max worst (Float.abs e)))
                (0., 0., 0., 0., 0., 0.) cs
            in
            let saa = saa +. lambda and sbb = sbb +. lambda in
            let det = (saa *. sbb) -. (sab *. sab) in
            (* Cramer's rule *)
            let dx = ((-.sae *. sbb) +. (sbe *. sab)) /. det and dy = ((-.sbe *. saa) +. (sae *. sab)) /. det in
            (* two constraints nearly alike (two circles nearly tangent)
               make the equations nearly singular and the move huge: a
               move is kept no longer than the worst error it fixes *)
            let len = Float.hypot dx dy in
            let k = if len > worst then worst /. len else 1. in
            Hashtbl.replace here id (if Float.is_finite dx && Float.is_finite dy then (x +. (k *. dx), y +. (k *. dy)) else (x, y)))
    s.points;
  { s with points = List.map (fun (id, _) -> (id, get id)) s.points }

let solve ?held ~sweeps s =
  let rec go n s = if n = 0 || error s < 1e-3 then s else go (n - 1) (sweep ?held s) in
  go sweeps s
