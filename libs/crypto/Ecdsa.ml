(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Ecdsa.mli *)

type curve = { name : string; bytes : int; p : Bignum.modulus; n : Bignum.modulus; b : Bignum.t; gx : Bignum.t; gy : Bignum.t }

let make name bytes ~p ~n ~b ~gx ~gy =
  { name; bytes; p = Bignum.modulus (Bignum.of_hex p); n = Bignum.modulus (Bignum.of_hex n); b = Bignum.of_hex b; gx = Bignum.of_hex gx; gy = Bignum.of_hex gy }

let p256 =
  make "P-256" 32 ~p:"ffffffff00000001000000000000000000000000ffffffffffffffffffffffff"
    ~n:"ffffffff00000000ffffffffffffffffbce6faada7179e84f3b9cac2fc632551"
    ~b:"5ac635d8aa3a93e7b3ebbd55769886bc651d06b0cc53b0f63bce3c3e27d2604b"
    ~gx:"6b17d1f2e12c4247f8bce6e563a440f277037d812deb33a0f4a13945d898c296"
    ~gy:"4fe342e2fe1a7f9b8ee7eb4a7c0f9e162bce33576b315ececbb6406837bf51f5"

let p384 =
  make "P-384" 48
    ~p:"fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffeffffffff0000000000000000ffffffff"
    ~n:"ffffffffffffffffffffffffffffffffffffffffffffffffc7634d81f4372ddf581a0db248b0a77aecec196accc52973"
    ~b:"b3312fa7e23ee7e4988e056be3f82d19181d9c6efe8141120314088f5013875ac656398d8a2ed19d2a85c8edd3ec2aef"
    ~gx:"aa87ca22be8b05378eb1c71ef320ad746e1d3b628ba79b9859f741e082542a385502f25dbf55296c3a545e3872760ab7"
    ~gy:"3617de4a96262c6f5d9e98bf9292dc29f8f41dbd289a147ce9da3113b5f0b8c00a60b1ce1d7e819d7a431d7c90ea0e5f"

let size (c : curve) : int = c.bytes

(* a point, Jacobian, the field in Montgomery's form; Z = 0 is O *)
type point = { x : Bignum.mont; y : Bignum.mont; z : Bignum.mont }

let infinity (c : curve) : point = let o = Bignum.of_nat c.p Bignum.zero in { x = Bignum.mont_one c.p; y = Bignum.mont_one c.p; z = o }
let affine (c : curve) (x : Bignum.t) (y : Bignum.t) : point = { x = Bignum.of_nat c.p x; y = Bignum.of_nat c.p y; z = Bignum.mont_one c.p }

(* dbl-2001-b, for a = -3 *)
let double (c : curve) (pt : point) : point =
  if Bignum.mont_is_zero pt.z || Bignum.mont_is_zero pt.y then infinity c
  else
    let ( * ) = Bignum.mont_mul c.p and ( + ) = Bignum.mont_add c.p and ( - ) = Bignum.mont_sub c.p in
    let delta = pt.z * pt.z and gamma = pt.y * pt.y in
    let beta = pt.x * gamma in
    let t = (pt.x - delta) * (pt.x + delta) in
    let alpha = t + t + t in
    let beta4 = beta + beta + beta + beta in
    let x3 = (alpha * alpha) - (beta4 + beta4) in
    let z3 = ((pt.y + pt.z) * (pt.y + pt.z)) - gamma - delta in
    let g2 = gamma * gamma in
    let g8 = g2 + g2 + g2 + g2 + g2 + g2 + g2 + g2 in
    { x = x3; y = (alpha * (beta4 - x3)) - g8; z = z3 }

(* add-2007-bl *)
let add (c : curve) (p1 : point) (p2 : point) : point =
  if Bignum.mont_is_zero p1.z then p2
  else if Bignum.mont_is_zero p2.z then p1
  else
    let ( * ) = Bignum.mont_mul c.p and ( + ) = Bignum.mont_add c.p and ( - ) = Bignum.mont_sub c.p in
    let z1z1 = p1.z * p1.z and z2z2 = p2.z * p2.z in
    let u1 = p1.x * z2z2 and u2 = p2.x * z1z1 in
    let s1 = p1.y * p2.z * z2z2 and s2 = p2.y * p1.z * z1z1 in
    let h = u2 - u1 and r0 = s2 - s1 in
    if Bignum.mont_is_zero h then if Bignum.mont_is_zero r0 then double c p1 else infinity c
    else
      let i = (h + h) * (h + h) in
      let j = h * i in
      let r = r0 + r0 in
      let v = u1 * i in
      let x3 = (r * r) - j - (v + v) in
      let s1j = s1 * j in
      let y3 = (r * (v - x3)) - (s1j + s1j) in
      let z3 = (((p1.z + p2.z) * (p1.z + p2.z)) - z1z1 - z2z2) * h in
      { x = x3; y = y3; z = z3 }

let scalar_mult (c : curve) (k : Bignum.t) (pt : point) : point =
  let r = ref (infinity c) in
  for i = Bignum.bits k - 1 downto 0 do
    r := double c !r;
    if Bignum.bit k i then r := add c !r pt
  done;
  !r

let affine_x (c : curve) (pt : point) : Bignum.t =
  let z = Bignum.to_nat c.p pt.z in
  let zinv = Bignum.inverse_prime c.p z in
  Bignum.mul_mod c.p (Bignum.to_nat c.p pt.x) (Bignum.mul_mod c.p zinv zinv)

let on_curve (c : curve) (x : Bignum.t) (y : Bignum.t) : bool =
  let pv = Bignum.modulus_value c.p in
  Bignum.compare x pv < 0
  && Bignum.compare y pv < 0
  &&
  let m = Bignum.mul_mod c.p in
  let x3 = m x (m x x) in
  let rhs = Bignum.add_mod c.p (Bignum.sub_mod c.p x3 (Bignum.rem (Bignum.mul (Bignum.of_int 3) x) pv)) c.b in
  Bignum.equal (m y y) rhs

let generator_ok (c : curve) : bool =
  on_curve c c.gx c.gy && Bignum.mont_is_zero (scalar_mult c (Bignum.modulus_value c.n) (affine c c.gx c.gy)).z

let verify (c : curve) ~(public : string) ~(hash : string) ~(r : Bignum.t) ~(s : Bignum.t) : bool =
  let nv = Bignum.modulus_value c.n in
  let in_range v = (not (Bignum.is_zero v)) && Bignum.compare v nv < 0 in
  String.length public = 1 + (2 * c.bytes)
  && public.[0] = '\004'
  && in_range r && in_range s
  &&
  let qx = Bignum.of_bytes (String.sub public 1 c.bytes) and qy = Bignum.of_bytes (String.sub public (1 + c.bytes) c.bytes) in
  on_curve c qx qy
  &&
  (* the hash's leftmost bits, as many as n has *)
  let e = Bignum.of_bytes hash in
  let extra = (8 * String.length hash) - Bignum.bits nv in
  let e = if extra > 0 then Bignum.shift_right e extra else e in
  let e = Bignum.rem e nv in
  let w = Bignum.inverse_prime c.n s in
  let u1 = Bignum.mul_mod c.n e w and u2 = Bignum.mul_mod c.n r w in
  let pt = add c (scalar_mult c u1 (affine c c.gx c.gy)) (scalar_mult c u2 (affine c qx qy)) in
  (not (Bignum.mont_is_zero pt.z)) && Bignum.equal (Bignum.rem (affine_x c pt) nv) r
