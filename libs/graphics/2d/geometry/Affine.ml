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

(* The inverse of the 2x2 part [a c; b d] is [d -c; -b a] / determinant
 * (the determinant a*d - b*c is how much the matrix scales areas; 0
 * means it squashes the plane flat, and there's no inverse). Then the
 * translation: m moves by (tx, ty) last, so its inverse must undo that
 * first, i.e. apply the inverted 2x2 part to (-tx, -ty). *)
let invert (m : t) : t =
  let det = (m.a *. m.d) -. (m.b *. m.c) in
  let a = m.d /. det and b = -.m.b /. det and c = -.m.c /. det and d = m.a /. det in
  { a; b; c; d; tx = -.((a *. m.tx) +. (c *. m.ty)); ty = -.((b *. m.tx) +. (d *. m.ty)) }
