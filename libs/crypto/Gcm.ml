(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Gcm.mli *)

(* a 128-bit block as two Int64, the first byte in the high bits of hi *)
let of_block (s : string) : int64 * int64 =
  let word off = List.fold_left (fun acc k -> Int64.logor (Int64.shift_left acc 8) (Int64.of_int (Char.code s.[off + k]))) 0L [ 0; 1; 2; 3; 4; 5; 6; 7 ] in
  (word 0, word 8)

let to_block ((hi, lo) : int64 * int64) : string =
  String.init 16 (fun i ->
      let w = if i < 8 then hi else lo in
      Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical w (56 - (8 * (i mod 8)))) 0xffL)))

(* X times Y in GF(2^128), GCM's bit order: bit 0 is the top bit of
   hi; shifting right is multiplying by x; R = 11100001 || 0^120 *)
let gmul ((xh, xl) : int64 * int64) (y : int64 * int64) : int64 * int64 =
  let zh = ref 0L and zl = ref 0L and v = ref y in
  for i = 0 to 127 do
    let bit = if i < 64 then Int64.logand (Int64.shift_right_logical xh (63 - i)) 1L else Int64.logand (Int64.shift_right_logical xl (127 - i)) 1L in
    let vh, vl = !v in
    if bit = 1L then (
      zh := Int64.logxor !zh vh;
      zl := Int64.logxor !zl vl);
    let lsb = Int64.logand vl 1L in
    let vl = Int64.logor (Int64.shift_right_logical vl 1) (Int64.shift_left vh 63) and vh = Int64.shift_right_logical vh 1 in
    v := if lsb = 1L then (Int64.logxor vh 0xe100000000000000L, vl) else (vh, vl)
  done;
  (!zh, !zl)

let xor_block ((a, b) : int64 * int64) ((c, d) : int64 * int64) = (Int64.logxor a c, Int64.logxor b d)

let ghash (h : int64 * int64) (data : string) : int64 * int64 =
  let n = String.length data in
  let rec go acc i = if i >= n then acc else go (gmul (xor_block acc (of_block (String.sub data i 16))) h) (i + 16) in
  go (0L, 0L) 0

let pad16 (s : string) : string = String.make ((16 - (String.length s mod 16)) mod 16) '\000'
let be64 (n : int) : string = String.init 8 (fun i -> Char.chr ((n lsr (56 - (8 * i))) land 0xff))

(* the counter block [j0 + i], its last 32 bits counting *)
let counter (j0 : string) (i : int) : string =
  let c = (Char.code j0.[12] lsl 24) lor (Char.code j0.[13] lsl 16) lor (Char.code j0.[14] lsl 8) lor Char.code j0.[15] in
  let c = (c + i) land 0xffffffff in
  String.sub j0 0 12 ^ String.init 4 (fun k -> Char.chr ((c lsr (24 - (8 * k))) land 0xff))

(* counter mode: the data xored with AES of J0+1, J0+2, ... *)
let ctr (k : Aes.key) (j0 : string) (data : string) : string =
  let n = String.length data in
  let out = Bytes.create n in
  let rec go b =
    if b * 16 < n then (
      let ks = Aes.encrypt_block k (counter j0 (b + 1)) in
      for j = 0 to min 16 (n - (b * 16)) - 1 do
        Bytes.set out ((b * 16) + j) (Char.chr (Char.code data.[(b * 16) + j] lxor Char.code ks.[j]))
      done;
      go (b + 1))
  in
  go 0;
  Bytes.to_string out

let tag (k : Aes.key) (j0 : string) ~(aad : string) (ct : string) : string =
  let h = of_block (Aes.encrypt_block k (String.make 16 '\000')) in
  let s = ghash h (aad ^ pad16 aad ^ ct ^ pad16 ct ^ be64 (8 * String.length aad) ^ be64 (8 * String.length ct)) in
  to_block (xor_block (of_block (Aes.encrypt_block k j0)) s)

let seal ~(key : string) ~(nonce : string) ~(aad : string) (plaintext : string) : string =
  let k = Aes.expand key and j0 = nonce ^ "\000\000\000\001" in
  let ct = ctr k j0 plaintext in
  ct ^ tag k j0 ~aad ct

let open_ ~(key : string) ~(nonce : string) ~(aad : string) (data : string) : string option =
  let n = String.length data in
  if n < 16 then None
  else
    let k = Aes.expand key and j0 = nonce ^ "\000\000\000\001" in
    let ct = String.sub data 0 (n - 16) in
    if Chacha20_poly1305.same (String.sub data (n - 16) 16) (tag k j0 ~aad ct) then Some (ctr k j0 ct) else None
