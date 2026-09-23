(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Zbuffer.mli *)

(* the depths row by row, pixel (x, y) at y * width + x *)
type t = { width : int; depth : float array }

let create ~(width : int) ~(height : int) : t = { width; depth = Array.make (width * height) infinity }
let clear (t : t) : unit = Array.fill t.depth 0 (Array.length t.depth) infinity

let test_and_set (t : t) ~(x : int) ~(y : int) (z : float) : bool =
  let idx = (y * t.width) + x in
  if z < Array.unsafe_get t.depth idx then begin
    Array.unsafe_set t.depth idx z;
    true
  end
  else false
