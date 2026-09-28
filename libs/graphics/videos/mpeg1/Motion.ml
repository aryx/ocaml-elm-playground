(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Motion.mli *)

type plane = { bytes : Bytes.t; stride : int; rows : int }
type search = Full | Logarithmic

(* a whole-pixel offset, the common case: a plain loop *)
let sad_whole (cur : plane) (reference : plane) ~(x : int) ~(y : int) ((dx, dy) : int * int) : int =
  let s = ref 0 in
  for j = 0 to 15 do
    let c = ((y + j) * cur.stride) + x and r = ((y + j + dy) * reference.stride) + x + dx in
    for i = 0 to 15 do
      s := !s + abs (Char.code (Bytes.unsafe_get cur.bytes (c + i)) - Char.code (Bytes.unsafe_get reference.bytes (r + i)))
    done
  done;
  !s

let sad (cur : plane) (reference : plane) ~(x : int) ~(y : int) ((vx, vy) : int * int) : int =
  if vx land 1 = 0 && vy land 1 = 0 then sad_whole cur reference ~x ~y (vx asr 1, vy asr 1)
  else
    let p = Mpeg1.prediction reference.bytes ~stride:reference.stride ~rows:reference.rows ~x ~y ~size:16 (vx, vy) in
    let s = ref 0 in
    Array.iteri (fun k v -> s := !s + abs (Char.code (Bytes.get cur.bytes (((y + (k / 16)) * cur.stride) + x + (k mod 16))) - v)) p;
    !s

(* inside the reference: the square and, for a half pixel, the pixel
 * after it *)
let inside (reference : plane) ~(x : int) ~(y : int) ((vx, vy) : int * int) : bool =
  let x0 = x + (vx asr 1) and y0 = y + (vy asr 1) in
  x0 >= 0 && y0 >= 0 && x0 + 16 + (vx land 1) <= reference.stride && y0 + 16 + (vy land 1) <= reference.rows

let estimate (search : search) ~(range : int) (cur : plane) (reference : plane) ~(x : int) ~(y : int) : (int * int) * int * int =
  let tried = ref 0 in
  let best = ref ((0, 0), sad_whole cur reference ~x ~y (0, 0)) in
  incr tried;
  (* try v (half pixels), keep it if better *)
  let consider v =
    if inside reference ~x ~y v then (
      incr tried;
      let s = sad cur reference ~x ~y v in
      if s < snd !best then best := (v, s))
  in
  (match search with
  | Full ->
      for dy = -range to range do
        for dx = -range to range do if (dx, dy) <> (0, 0) then consider (2 * dx, 2 * dy) done
      done
  | Logarithmic ->
      (* the first step: the smallest power of two whose steps, halving,
       * reach the range -- 4 for 7 (4 + 2 + 1), 8 for 12 *)
      let step = ref 1 in
      while (2 * !step) - 1 < range do step := !step * 2 done;
      while !step >= 1 do
        let cx, cy = fst !best in
        List.iter (fun (i, j) -> if (i, j) <> (0, 0) then consider (cx + (2 * i * !step), cy + (2 * j * !step))) [ (-1, -1); (0, -1); (1, -1); (-1, 0); (1, 0); (-1, 1); (0, 1); (1, 1) ];
        step := !step / 2
      done);
  (* half a pixel around the best whole one *)
  let cx, cy = fst !best in
  List.iter (fun (i, j) -> if (i, j) <> (0, 0) then consider (cx + i, cy + j)) [ (-1, -1); (0, -1); (1, -1); (-1, 0); (1, 0); (-1, 1); (0, 1); (1, 1) ];
  (fst !best, snd !best, !tried)
