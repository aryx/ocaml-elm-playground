(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Sha1.mli *)

let ( +: ) = Int32.add
let rotl (x : int32) (n : int) : int32 = Int32.logor (Int32.shift_left x n) (Int32.shift_right_logical x (32 - n))

(* the message, a 1 bit, zeros, and its length in bits (64, big-endian):
 * a multiple of 64 bytes *)
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
  Int32.logor
    (Int32.logor (Int32.shift_left (byte 0) 24) (Int32.shift_left (byte 1) 16))
    (Int32.logor (Int32.shift_left (byte 2) 8) (byte 3))

(* the 80 rounds on one block, mixed into the state *)
let compress (h : int32 array) (b : Bytes.t) (block : int) : unit =
  let w = Array.make 80 0l in
  for i = 0 to 15 do
    w.(i) <- word b ((block * 64) + (i * 4))
  done;
  for i = 16 to 79 do
    w.(i) <- rotl (Int32.logxor (Int32.logxor w.(i - 3) w.(i - 8)) (Int32.logxor w.(i - 14) w.(i - 16))) 1
  done;
  let a = ref h.(0) and b' = ref h.(1) and c = ref h.(2) and d = ref h.(3) and e = ref h.(4) in
  for i = 0 to 79 do
    let f, k =
      if i < 20 then (Int32.logor (Int32.logand !b' !c) (Int32.logand (Int32.lognot !b') !d), 0x5A827999l)
      else if i < 40 then (Int32.logxor (Int32.logxor !b' !c) !d, 0x6ED9EBA1l)
      else if i < 60 then
        (Int32.logor (Int32.logor (Int32.logand !b' !c) (Int32.logand !b' !d)) (Int32.logand !c !d), 0x8F1BBCDCl)
      else (Int32.logxor (Int32.logxor !b' !c) !d, 0xCA62C1D6l)
    in
    let t = rotl !a 5 +: f +: !e +: k +: w.(i) in
    e := !d;
    d := !c;
    c := rotl !b' 30;
    b' := !a;
    a := t
  done;
  h.(0) <- h.(0) +: !a;
  h.(1) <- h.(1) +: !b';
  h.(2) <- h.(2) +: !c;
  h.(3) <- h.(3) +: !d;
  h.(4) <- h.(4) +: !e

let digest (s : string) : string =
  let h = [| 0x67452301l; 0xEFCDAB89l; 0x98BADCFEl; 0x10325476l; 0xC3D2E1F0l |] in
  let b = pad s in
  for block = 0 to (Bytes.length b / 64) - 1 do
    compress h b block
  done;
  String.init 20 (fun i -> Char.chr (Int32.to_int (Int32.logand (Int32.shift_right_logical h.(i / 4) (24 - (8 * (i mod 4)))) 0xffl)))

let hex (s : string) : string = String.concat "" (List.map (fun c -> Printf.sprintf "%02x" (Char.code c)) (List.of_seq (String.to_seq (digest s))))
