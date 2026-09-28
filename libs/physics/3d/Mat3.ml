(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Mat3.mli *)

type t = {
  m00 : float;
  m01 : float;
  m02 : float;
  m10 : float;
  m11 : float;
  m12 : float;
  m20 : float;
  m21 : float;
  m22 : float;
}

let zero = { m00 = 0.; m01 = 0.; m02 = 0.; m10 = 0.; m11 = 0.; m12 = 0.; m20 = 0.; m21 = 0.; m22 = 0. }
let identity = { zero with m00 = 1.; m11 = 1.; m22 = 1. }

let of_rows (a, b, c) (d, e, f) (g, h, i) =
  { m00 = a; m01 = b; m02 = c; m10 = d; m11 = e; m12 = f; m20 = g; m21 = h; m22 = i }

let diagonal a b c = { zero with m00 = a; m11 = b; m22 = c }

let add x y =
  { m00 = x.m00 +. y.m00; m01 = x.m01 +. y.m01; m02 = x.m02 +. y.m02;
    m10 = x.m10 +. y.m10; m11 = x.m11 +. y.m11; m12 = x.m12 +. y.m12;
    m20 = x.m20 +. y.m20; m21 = x.m21 +. y.m21; m22 = x.m22 +. y.m22 }

let scale s m =
  { m00 = s *. m.m00; m01 = s *. m.m01; m02 = s *. m.m02;
    m10 = s *. m.m10; m11 = s *. m.m11; m12 = s *. m.m12;
    m20 = s *. m.m20; m21 = s *. m.m21; m22 = s *. m.m22 }

let mul x y =
  { m00 = (x.m00 *. y.m00) +. (x.m01 *. y.m10) +. (x.m02 *. y.m20);
    m01 = (x.m00 *. y.m01) +. (x.m01 *. y.m11) +. (x.m02 *. y.m21);
    m02 = (x.m00 *. y.m02) +. (x.m01 *. y.m12) +. (x.m02 *. y.m22);
    m10 = (x.m10 *. y.m00) +. (x.m11 *. y.m10) +. (x.m12 *. y.m20);
    m11 = (x.m10 *. y.m01) +. (x.m11 *. y.m11) +. (x.m12 *. y.m21);
    m12 = (x.m10 *. y.m02) +. (x.m11 *. y.m12) +. (x.m12 *. y.m22);
    m20 = (x.m20 *. y.m00) +. (x.m21 *. y.m10) +. (x.m22 *. y.m20);
    m21 = (x.m20 *. y.m01) +. (x.m21 *. y.m11) +. (x.m22 *. y.m21);
    m22 = (x.m20 *. y.m02) +. (x.m21 *. y.m12) +. (x.m22 *. y.m22) }

let transpose m =
  { m00 = m.m00; m01 = m.m10; m02 = m.m20;
    m10 = m.m01; m11 = m.m11; m12 = m.m21;
    m20 = m.m02; m21 = m.m12; m22 = m.m22 }

let mul_vec m (x, y, z) =
  ( (m.m00 *. x) +. (m.m01 *. y) +. (m.m02 *. z),
    (m.m10 *. x) +. (m.m11 *. y) +. (m.m12 *. z),
    (m.m20 *. x) +. (m.m21 *. y) +. (m.m22 *. z) )

let conjugate r m = mul (mul r m) (transpose r)

let det m =
  (m.m00 *. ((m.m11 *. m.m22) -. (m.m12 *. m.m21)))
  -. (m.m01 *. ((m.m10 *. m.m22) -. (m.m12 *. m.m20)))
  +. (m.m02 *. ((m.m10 *. m.m21) -. (m.m11 *. m.m20)))

(* the adjugate over the determinant: the cofactors, transposed *)
let inverse m =
  let d = det m in
  if Float.abs d < 1e-12 then None
  else
    let s = 1. /. d in
    Some
      (scale s
         (of_rows
            ((m.m11 *. m.m22) -. (m.m12 *. m.m21), (m.m02 *. m.m21) -. (m.m01 *. m.m22), (m.m01 *. m.m12) -. (m.m02 *. m.m11))
            ((m.m12 *. m.m20) -. (m.m10 *. m.m22), (m.m00 *. m.m22) -. (m.m02 *. m.m20), (m.m02 *. m.m10) -. (m.m00 *. m.m12))
            ((m.m10 *. m.m21) -. (m.m11 *. m.m20), (m.m01 *. m.m20) -. (m.m00 *. m.m21), (m.m00 *. m.m11) -. (m.m01 *. m.m10))))

(* claude: 1/infinity = 0 falls out, which is what a body that never
 * turns about that axis wants (see Body3d.never_turns) *)
let inverse_diagonal m = diagonal (1. /. m.m00) (1. /. m.m11) (1. /. m.m22)
