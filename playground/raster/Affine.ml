(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Affine.mli for what the six numbers mean *)
type t = { a : float; b : float; c : float; d : float; tx : float; ty : float }

let identity = { a = 1.; b = 0.; c = 0.; d = 1.; tx = 0.; ty = 0. }

let translate dx dy = { identity with tx = dx; ty = dy }

(* The point (1, 0) goes to (cos r, sin r) and (0, 1) to (-sin r, cos r):
 * the columns of the matrix are where the x and y axes end up. *)
let rotate r =
  let cos_r = cos r and sin_r = sin r in
  { a = cos_r; b = sin_r; c = -.sin_r; d = cos_r; tx = 0.; ty = 0. }

let scale sx sy = { identity with a = sx; d = sy }

(* The 3x3 matrix product m * n, written out for the 6 entries that
 * aren't always 0 or 1 *)
let compose (m : t) (n : t) : t =
  {
    a = (m.a *. n.a) +. (m.c *. n.b);
    b = (m.b *. n.a) +. (m.d *. n.b);
    c = (m.a *. n.c) +. (m.c *. n.d);
    d = (m.b *. n.c) +. (m.d *. n.d);
    tx = (m.a *. n.tx) +. (m.c *. n.ty) +. m.tx;
    ty = (m.b *. n.tx) +. (m.d *. n.ty) +. m.ty;
  }

let apply (m : t) (x, y) = ((m.a *. x) +. (m.c *. y) +. m.tx, (m.b *. x) +. (m.d *. y) +. m.ty)
