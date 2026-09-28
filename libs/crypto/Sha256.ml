(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Sha256.mli *)

let ( +: ) = Int32.add
let ( ^: ) = Int32.logxor
let ( &: ) = Int32.logand
let rotr (x : int32) (n : int) : int32 = Int32.logor (Int32.shift_right_logical x n) (Int32.shift_left x (32 - n))

let k =
  [| 0x428a2f98l; 0x71374491l; 0xb5c0fbcfl; 0xe9b5dba5l; 0x3956c25bl; 0x59f111f1l; 0x923f82a4l; 0xab1c5ed5l;
     0xd807aa98l; 0x12835b01l; 0x243185bel; 0x550c7dc3l; 0x72be5d74l; 0x80deb1fel; 0x9bdc06a7l; 0xc19bf174l;
     0xe49b69c1l; 0xefbe4786l; 0x0fc19dc6l; 0x240ca1ccl; 0x2de92c6fl; 0x4a7484aal; 0x5cb0a9dcl; 0x76f988dal;
     0x983e5152l; 0xa831c66dl; 0xb00327c8l; 0xbf597fc7l; 0xc6e00bf3l; 0xd5a79147l; 0x06ca6351l; 0x14292967l;
     0x27b70a85l; 0x2e1b2138l; 0x4d2c6dfcl; 0x53380d13l; 0x650a7354l; 0x766a0abbl; 0x81c2c92el; 0x92722c85l;
     0xa2bfe8a1l; 0xa81a664bl; 0xc24b8b70l; 0xc76c51a3l; 0xd192e819l; 0xd6990624l; 0xf40e3585l; 0x106aa070l;
     0x19a4c116l; 0x1e376c08l; 0x2748774cl; 0x34b0bcb5l; 0x391c0cb3l; 0x4ed8aa4al; 0x5b9cca4fl; 0x682e6ff3l;
     0x748f82eel; 0x78a5636fl; 0x84c87814l; 0x8cc70208l; 0x90befffal; 0xa4506cebl; 0xbef9a3f7l; 0xc67178f2l |]

(* the message, a 1 bit, zeros, and its length in bits (64, big-endian) *)
let pad (s : string) : Bytes.t =
  let n = String.length s in
  let total = ((n + 8) / 64 * 64) + 64 in
  let b = Bytes.make total '\000' in
  Bytes.blit_string s 0 b 0 n;
  Bytes.set b n '\x80';
  let bits = Int64.mul (Int64.of_int n) 8L in
  for i = 0 to 7 do
    Bytes.set b (total - 1 - i) (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical bits (8 * i)) 0xffL)))
  done;
  b

let word (b : Bytes.t) (i : int) : int32 =
  let byte k = Int32.of_int (Char.code (Bytes.get b (i + k))) in
  Int32.logor (Int32.logor (Int32.shift_left (byte 0) 24) (Int32.shift_left (byte 1) 16)) (Int32.logor (Int32.shift_left (byte 2) 8) (byte 3))

let compress (h : int32 array) (b : Bytes.t) (block : int) : unit =
  let w = Array.make 64 0l in
  for i = 0 to 15 do
    w.(i) <- word b ((block * 64) + (i * 4))
  done;
  for i = 16 to 63 do
    let s0 = rotr w.(i - 15) 7 ^: rotr w.(i - 15) 18 ^: Int32.shift_right_logical w.(i - 15) 3 in
    let s1 = rotr w.(i - 2) 17 ^: rotr w.(i - 2) 19 ^: Int32.shift_right_logical w.(i - 2) 10 in
    w.(i) <- w.(i - 16) +: s0 +: w.(i - 7) +: s1
  done;
  let a = ref h.(0) and b' = ref h.(1) and c = ref h.(2) and d = ref h.(3) in
  let e = ref h.(4) and f = ref h.(5) and g = ref h.(6) and hh = ref h.(7) in
  for i = 0 to 63 do
    let s1 = rotr !e 6 ^: rotr !e 11 ^: rotr !e 25 in
    let ch = (!e &: !f) ^: (Int32.lognot !e &: !g) in
    let t1 = !hh +: s1 +: ch +: k.(i) +: w.(i) in
    let s0 = rotr !a 2 ^: rotr !a 13 ^: rotr !a 22 in
    let maj = (!a &: !b') ^: (!a &: !c) ^: (!b' &: !c) in
    let t2 = s0 +: maj in
    hh := !g;
    g := !f;
    f := !e;
    e := !d +: t1;
    d := !c;
    c := !b';
    b' := !a;
    a := t1 +: t2
  done;
  List.iteri (fun i v -> h.(i) <- h.(i) +: v) [ !a; !b'; !c; !d; !e; !f; !g; !hh ]

let digest (s : string) : string =
  let h = [| 0x6a09e667l; 0xbb67ae85l; 0x3c6ef372l; 0xa54ff53al; 0x510e527fl; 0x9b05688cl; 0x1f83d9abl; 0x5be0cd19l |] in
  let b = pad s in
  for block = 0 to (Bytes.length b / 64) - 1 do
    compress h b block
  done;
  String.init 32 (fun i -> Char.chr (Int32.to_int (Int32.logand (Int32.shift_right_logical h.(i / 4) (24 - (8 * (i mod 4)))) 0xffl)))

let hex (s : string) : string = String.concat "" (List.map (fun c -> Printf.sprintf "%02x" (Char.code c)) (List.of_seq (String.to_seq s)))
