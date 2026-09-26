(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See X25519.mli *)

let p = Bignum.sub (Bignum.shift_left Bignum.one 255) (Bignum.of_int 19)
let field = Bignum.modulus p
let le (s : string) : Bignum.t = Bignum.of_bytes (String.init (String.length s) (fun i -> s.[String.length s - 1 - i]))
let to_le (n : Bignum.t) : string = let b = Bignum.to_bytes ~len:32 n in String.init 32 (fun i -> b.[31 - i])

let scalar_mult (k : string) (u : string) : string =
  let k = Bytes.of_string k in
  Bytes.set k 0 (Char.chr (Char.code (Bytes.get k 0) land 248));
  Bytes.set k 31 (Char.chr ((Char.code (Bytes.get k 31) land 127) lor 64));
  let k = le (Bytes.to_string k) in
  (* u's top bit ignored *)
  let u = Bytes.of_string u in
  Bytes.set u 31 (Char.chr (Char.code (Bytes.get u 31) land 127));
  let ( * ) = Bignum.mont_mul field and ( + ) = Bignum.mont_add field and ( - ) = Bignum.mont_sub field in
  let x1 = Bignum.of_nat field (le (Bytes.to_string u)) in
  let a24 = Bignum.of_nat field (Bignum.of_int 121665) in
  let x2 = ref (Bignum.mont_one field) and z2 = ref (Bignum.of_nat field Bignum.zero) in
  let x3 = ref x1 and z3 = ref (Bignum.mont_one field) in
  let swap = ref false in
  let cswap b = if b then (let t = !x2 in x2 := !x3; x3 := t; let t = !z2 in z2 := !z3; z3 := t) in
  for t = 254 downto 0 do
    let kt = Bignum.bit k t in
    cswap (!swap <> kt);
    swap := kt;
    let a = !x2 + !z2 in
    let aa = a * a in
    let b = !x2 - !z2 in
    let bb = b * b in
    let e = aa - bb in
    let c = !x3 + !z3 and d = !x3 - !z3 in
    let da = d * a and cb = c * b in
    x3 := (da + cb) * (da + cb);
    z3 := x1 * ((da - cb) * (da - cb));
    x2 := aa * bb;
    z2 := e * (aa + (a24 * e))
  done;
  cswap !swap;
  let x = Bignum.to_nat field !x2 and z = Bignum.to_nat field !z2 in
  to_le (Bignum.mul_mod field x (Bignum.inverse_prime field z))

let public_key (k : string) : string = scalar_mult k ("\009" ^ String.make 31 '\000')
