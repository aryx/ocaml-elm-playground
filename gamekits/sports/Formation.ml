(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
open Playground
open Basics

(* See Formation.mli *)

type spot = number * number

let at ~(half_w : number) ~(half_h : number) ~(up : bool) ((fx, fy) : spot) : number * number =
  if up then (fx * half_w, fy * half_h) else (0. - (fx * half_w), 0. - (fy * half_h))

let belongs ~(pull : number) ~(home : number * number) ~(ball : number * number) : number * number =
  let hx, hy = home and bx, by = ball in
  (hx + ((bx - hx) * pull), hy + ((by - hy) * pull))

let nearest (where : 'p -> number * number) (keep : 'p -> bool) ((x, y) : number * number) (players : 'p list) : int option =
  let best =
    List.mapi (fun i p -> (i, p)) players
    |> List.filter (fun (_, p) -> keep p)
    |> List.map (fun (i, p) -> let px, py = where p in (i, Float.hypot (px - x) (py - y)))
    |> List.sort (fun (_, a) (_, b) -> compare a b)
  in
  match best with (i, _) :: _ -> Some i | [] -> None

let run_to ~(speed : number) ~(bounds : number * number) ((px, py) : number * number) ((tx, ty) : number * number) :
    (number * number) * (number * number) =
  let bw, bh = bounds in
  let dx = tx - px and dy = ty - py in
  let d = Float.hypot dx dy in
  if d < 2. then ((px, py), (0., 0.))
  else ((clamp (0. - bw) bw (px + (speed * dx / d)), clamp (0. - bh) bh (py + (speed * dy / d))), (dx / d, dy / d))
