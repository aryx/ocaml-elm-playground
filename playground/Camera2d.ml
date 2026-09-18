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

type t = { x : number; y : number; zoom : number }

let origin = { x = 0.; y = 0.; zoom = 1. }

type rect = { left : number; right : number; bottom : number; top : number }

(*****************************************************************************)
(* Looking through the camera *)
(*****************************************************************************)

let view (cam : t) (shapes : shape list) : shape =
  group shapes |> scale cam.zoom |> move (-.(cam.zoom * cam.x)) (-.(cam.zoom * cam.y))

let to_screen (cam : t) (x : number) (y : number) : number * number =
  (cam.zoom * (x - cam.x), cam.zoom * (y - cam.y))

let to_world (cam : t) (x : number) (y : number) : number * number =
  ((x / cam.zoom) + cam.x, (y / cam.zoom) + cam.y)

let visible (screen : screen) (cam : t) : rect =
  let half_w = screen.width / (2. * cam.zoom) in
  let half_h = screen.height / (2. * cam.zoom) in
  { left = cam.x - half_w; right = cam.x + half_w; bottom = cam.y - half_h; top = cam.y + half_h }

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

(*****************************************************************************)
(* Parallax *)
(*****************************************************************************)

let parallax (factor : number) (cam : t) : t = { cam with x = factor * cam.x; y = factor * cam.y }
