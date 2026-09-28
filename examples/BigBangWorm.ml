(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* How to Design Programs's worm game (its exercises 215 to 219): a worm
 * crawling on a grid, turned with the arrows, growing when it eats; it
 * dies hitting the border or itself (see Bigbang.mli). The
 * world is a record, the worm a list of segments, head first -- HtDP's
 * data definitions, in OCaml's types.
 *
 * Every big-bang clause is here: to_draw, on_tick (every 1/8 s, a
 * slower tick than the frames), on_key, stop_when, and last_picture,
 * the scene with its epitaph. The food's place is random, from a
 * generator whose state is in the world, so that a game replays. *)
open Playground
open Bigbang

let cell = 20.
let cols = 20
let rows = 20

type world = { worm : (int * int) list; (* head first *) dir : int * int; food : int * int; seed : int }

let next_seed (s : int) : int = ((s * 1103515245) + 12345) land 0x7fffffff

(* food somewhere not on the worm *)
let rec new_food (worm : (int * int) list) (seed : int) : (int * int) * int =
  let s = next_seed seed in
  let p = (s / 65536 mod cols, s / 7 mod rows) in
  if List.mem p worm then new_food worm s else (p, s)

let init : world =
  let worm = [ (10, 10); (9, 10); (8, 10) ] in
  let food, seed = new_food worm 42 in
  { worm; dir = (1, 0); food; seed }

(* a tick: the head one cell further; the tail follows, unless the
 * worm eats *)
let tick (w : world) : world =
  let hx, hy = List.hd w.worm and dx, dy = w.dir in
  let head = (hx + dx, hy + dy) in
  if head = w.food then
    let food, seed = new_food (head :: w.worm) w.seed in
    { w with worm = head :: w.worm; food; seed }
  else { w with worm = head :: List.filteri (fun i _ -> i < List.length w.worm - 1) w.worm }

(* the arrows turn it, but not back onto itself *)
let key (w : world) (k : string) : world =
  let dir = match k with "left" -> (-1, 0) | "right" -> (1, 0) | "up" -> (0, -1) | "down" -> (0, 1) | _ -> w.dir in
  if fst dir = -fst w.dir && snd dir = -snd w.dir then w else { w with dir }

let dead (w : world) : bool =
  let ((hx, hy) as head) = List.hd w.worm in
  hx < 0 || hx >= cols || hy < 0 || hy >= rows || List.mem head (List.tl w.worm)

let at ((c, r) : int * int) = ((float_of_int c +. 0.5) *. cell, (float_of_int r +. 0.5) *. cell)

let draw (w : world) : image =
  let scene = empty_scene (float_of_int cols *. cell) (float_of_int rows *. cell) in
  let place img p scene = let x, y = at p in place_image img x y scene in
  let scene = place (circle (cell /. 2.) Solid (rgb 60 170 60)) w.food scene in
  List.fold_left (fun scene p -> place (circle (cell /. 2.) Solid (rgb 200 60 60)) p scene) scene w.worm

let epitaph (w : world) : image =
  overlay (above (text "the worm hit" 24. black) (text (Printf.sprintf "its length: %d" (List.length w.worm)) 24. black)) (draw w)

let app = big_bang init ~to_draw:draw ~on_tick:tick ~tick_rate:(1. /. 8.) ~on_key:key ~stop_when:dead ~last_picture:epitaph ()
let main = Playground_platform.run_app app
