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
open Basics (* float arithmetics *)

(* See Camera2d.mli *)

type t = { x : number; y : number; zoom : number; angle : number }

let origin = { x = 0.; y = 0.; zoom = 1.; angle = 0. }

(* (x, y) turned by [degrees], counterclockwise *)
let rotate_point (degrees : number) ((x, y) : number * number) : number * number =
  let a = degrees * pi / 180. in
  ((x * cos a) - (y * sin a), (x * sin a) + (y * cos a))

type rect = { left : number; right : number; bottom : number; top : number }

(*****************************************************************************)
(* Looking through the camera *)
(*****************************************************************************)

(* a group's transform is scale, then rotate, then move: here zoom, then
 * turn by -angle, then move the camera's point to the center *)
let view (cam : t) (shapes : shape list) : shape =
  let tx, ty = rotate_point (-.cam.angle) (cam.zoom * cam.x, cam.zoom * cam.y) in
  group shapes |> scale cam.zoom |> rotate (-.cam.angle) |> move (-.tx) (-.ty)

let to_screen (cam : t) (x : number) (y : number) : number * number =
  rotate_point (-.cam.angle) (cam.zoom * (x - cam.x), cam.zoom * (y - cam.y))

let to_world (cam : t) (x : number) (y : number) : number * number =
  let dx, dy = rotate_point cam.angle (x / cam.zoom, y / cam.zoom) in
  (dx + cam.x, dy + cam.y)

(* the screen's corners in the world, and the box around them: turned,
 * the screen shows a turned rectangle of the world, inside this box *)
let visible (screen : screen) (cam : t) : rect =
  let half_w = screen.width / (2. * cam.zoom) in
  let half_h = screen.height / (2. * cam.zoom) in
  let corners = List.map (rotate_point cam.angle) [ (-.half_w, -.half_h); (half_w, -.half_h); (half_w, half_h); (-.half_w, half_h) ] in
  let xs = List.map fst corners and ys = List.map snd corners in
  { left = cam.x + List.fold_left Float.min infinity xs; right = cam.x + List.fold_left Float.max neg_infinity xs;
    bottom = cam.y + List.fold_left Float.min infinity ys; top = cam.y + List.fold_left Float.max neg_infinity ys }

(*****************************************************************************)
(* Moving the camera *)
(*****************************************************************************)

let look_at (x : number) (y : number) (cam : t) : t = { cam with x; y }

let follow (fraction : number) (x : number) (y : number) (cam : t) : t =
  { cam with x = cam.x + (fraction * (x - cam.x)); y = cam.y + (fraction * (y - cam.y)) }

(* in one dimension: [pos], the camera's, is pushed so that [target] is
 * at most [half] away from it *)
let push (half : number) (target : number) (pos : number) : number =
  if target > pos + half then target - half
  else if target < pos - half then target + half
  else pos

let window (w : number) (h : number) (x : number) (y : number) (cam : t) : t =
  { cam with x = push (w / 2.) x cam.x; y = push (h / 2.) y cam.y }

(* in one dimension: [pos] kept in [lo + half, hi - half], so that
 * [pos - half, pos + half] (what the screen shows) stays in [lo, hi];
 * centered when [lo, hi] is smaller than the screen *)
let limit (half : number) (lo : number) (hi : number) (pos : number) : number =
  if hi - lo < 2. * half then (lo + hi) / 2.
  else clamp (lo + half) (hi - half) pos

let clamp (screen : screen) (bounds : rect) (cam : t) : t =
  let half_w = screen.width / (2. * cam.zoom) in
  let half_h = screen.height / (2. * cam.zoom) in
  { cam with
    x = limit half_w bounds.left bounds.right cam.x;
    y = limit half_h bounds.bottom bounds.top cam.y }

let turn_toward (fraction : number) (angle : number) (cam : t) : t =
  (* the difference, the short way round: between -180 and 180 *)
  let diff = Float.rem (Float.rem (angle - cam.angle + 180.) 360. + 360.) 360. - 180. in
  { cam with angle = cam.angle + (fraction * diff) }

(*****************************************************************************)
(* Parallax *)
(*****************************************************************************)

let parallax (factor : number) (cam : t) : t = { cam with x = factor * cam.x; y = factor * cam.y }
