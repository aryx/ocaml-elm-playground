(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Sha512.mli *)

let ( +: ) = Int64.add
let ( ^: ) = Int64.logxor
let ( &: ) = Int64.logand
let rotr (x : int64) (n : int) : int64 = Int64.logor (Int64.shift_right_logical x n) (Int64.shift_left x (64 - n))

let k =
  [| 0x428a2f98d728ae22L; 0x7137449123ef65cdL; 0xb5c0fbcfec4d3b2fL; 0xe9b5dba58189dbbcL; 0x3956c25bf348b538L; 0x59f111f1b605d019L;
     0x923f82a4af194f9bL; 0xab1c5ed5da6d8118L; 0xd807aa98a3030242L; 0x12835b0145706fbeL; 0x243185be4ee4b28cL; 0x550c7dc3d5ffb4e2L;
     0x72be5d74f27b896fL; 0x80deb1fe3b1696b1L; 0x9bdc06a725c71235L; 0xc19bf174cf692694L; 0xe49b69c19ef14ad2L; 0xefbe4786384f25e3L;
     0x0fc19dc68b8cd5b5L; 0x240ca1cc77ac9c65L; 0x2de92c6f592b0275L; 0x4a7484aa6ea6e483L; 0x5cb0a9dcbd41fbd4L; 0x76f988da831153b5L;
     0x983e5152ee66dfabL; 0xa831c66d2db43210L; 0xb00327c898fb213fL; 0xbf597fc7beef0ee4L; 0xc6e00bf33da88fc2L; 0xd5a79147930aa725L;
     0x06ca6351e003826fL; 0x142929670a0e6e70L; 0x27b70a8546d22ffcL; 0x2e1b21385c26c926L; 0x4d2c6dfc5ac42aedL; 0x53380d139d95b3dfL;
     0x650a73548baf63deL; 0x766a0abb3c77b2a8L; 0x81c2c92e47edaee6L; 0x92722c851482353bL; 0xa2bfe8a14cf10364L; 0xa81a664bbc423001L;
     0xc24b8b70d0f89791L; 0xc76c51a30654be30L; 0xd192e819d6ef5218L; 0xd69906245565a910L; 0xf40e35855771202aL; 0x106aa07032bbd1b8L;
     0x19a4c116b8d2d0c8L; 0x1e376c085141ab53L; 0x2748774cdf8eeb99L; 0x34b0bcb5e19b48a8L; 0x391c0cb3c5c95a63L; 0x4ed8aa4ae3418acbL;
     0x5b9cca4f7763e373L; 0x682e6ff3d6b2b8a3L; 0x748f82ee5defb2fcL; 0x78a5636f43172f60L; 0x84c87814a1f0ab72L; 0x8cc702081a6439ecL;
     0x90befffa23631e28L; 0xa4506cebde82bde9L; 0xbef9a3f7b2c67915L; 0xc67178f2e372532bL; 0xca273eceea26619cL; 0xd186b8c721c0c207L;
     0xeada7dd6cde0eb1eL; 0xf57d4f7fee6ed178L; 0x06f067aa72176fbaL; 0x0a637dc5a2c898a6L; 0x113f9804bef90daeL; 0x1b710b35131c471bL;
     0x28db77f523047d84L; 0x32caab7b40c72493L; 0x3c9ebe0a15c9bebcL; 0x431d67c49c100d4cL; 0x4cc5d4becb3e42b6L; 0x597f299cfc657e2aL;
     0x5fcb6fab3ad6faecL; 0x6c44198c4a475817L |]

(* a 1 bit, zeros, the length in bits on 128 (its high 64 bits zero
 * here: messages under 2 exabytes) *)
let pad (s : string) : Bytes.t =
  let n = String.length s in
  let total = ((n + 16) / 128 * 128) + 128 in
  let b = Bytes.make total '\000' in
  Bytes.blit_string s 0 b 0 n;
  Bytes.set b n '\x80';
  let bits = Int64.mul (Int64.of_int n) 8L in
  for i = 0 to 7 do
    Bytes.set b (total - 1 - i) (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical bits (8 * i)) 0xffL)))
  done;
  b

let word (b : Bytes.t) (i : int) : int64 =
  let r = ref 0L in
  for k = 0 to 7 do
    r := Int64.logor (Int64.shift_left !r 8) (Int64.of_int (Char.code (Bytes.get b (i + k))))
  done;
  !r

let compress (h : int64 array) (b : Bytes.t) (block : int) : unit =
  let w = Array.make 80 0L in
  for i = 0 to 15 do
    w.(i) <- word b ((block * 128) + (i * 8))
  done;
  for i = 16 to 79 do
    let s0 = rotr w.(i - 15) 1 ^: rotr w.(i - 15) 8 ^: Int64.shift_right_logical w.(i - 15) 7 in
    let s1 = rotr w.(i - 2) 19 ^: rotr w.(i - 2) 61 ^: Int64.shift_right_logical w.(i - 2) 6 in
    w.(i) <- w.(i - 16) +: s0 +: w.(i - 7) +: s1
  done;
  let a = ref h.(0) and b' = ref h.(1) and c = ref h.(2) and d = ref h.(3) in
  let e = ref h.(4) and f = ref h.(5) and g = ref h.(6) and hh = ref h.(7) in
  for i = 0 to 79 do
    let s1 = rotr !e 14 ^: rotr !e 18 ^: rotr !e 41 in
    let ch = (!e &: !f) ^: (Int64.lognot !e &: !g) in
    let t1 = !hh +: s1 +: ch +: k.(i) +: w.(i) in
    let s0 = rotr !a 28 ^: rotr !a 34 ^: rotr !a 39 in
    let maj = (!a &: !b') ^: (!a &: !c) ^: (!b' &: !c) in
    hh := !g;
    g := !f;
    f := !e;
    e := !d +: t1;
    d := !c;
    c := !b';
    b' := !a;
    a := t1 +: s0 +: maj
  done;
  List.iteri (fun i v -> h.(i) <- h.(i) +: v) [ !a; !b'; !c; !d; !e; !f; !g; !hh ]

let run (h : int64 array) (s : string) (bytes : int) : string =
  let b = pad s in
  for block = 0 to (Bytes.length b / 128) - 1 do
    compress h b block
  done;
  String.init bytes (fun i -> Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical h.(i / 8) (56 - (8 * (i mod 8)))) 0xffL)))

let digest (s : string) : string =
  run
    [| 0x6a09e667f3bcc908L; 0xbb67ae8584caa73bL; 0x3c6ef372fe94f82bL; 0xa54ff53a5f1d36f1L; 0x510e527fade682d1L; 0x9b05688c2b3e6c1fL;
       0x1f83d9abfb41bd6bL; 0x5be0cd19137e2179L |]
    s 64

let digest384 (s : string) : string =
  run
    [| 0xcbbb9d5dc1059ed8L; 0x629a292a367cd507L; 0x9159015a3070dd17L; 0x152fecd8f70e5939L; 0x67332667ffc00b31L; 0x8eb44a8768581511L;
       0xdb0c2e0d64f98fa7L; 0x47b5481dbefa4fa4L |]
    s 48
