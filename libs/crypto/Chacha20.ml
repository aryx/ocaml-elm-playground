(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Chacha20.mli *)

let mask = 0xffffffff
let rotl (x : int) (n : int) : int = ((x lsl n) lor (x lsr (32 - n))) land mask
let le32 (s : string) (i : int) : int = Char.code s.[i] lor (Char.code s.[i + 1] lsl 8) lor (Char.code s.[i + 2] lsl 16) lor (Char.code s.[i + 3] lsl 24)

let quarter (x : int array) a b c d =
  x.(a) <- (x.(a) + x.(b)) land mask;
  x.(d) <- rotl (x.(d) lxor x.(a)) 16;
  x.(c) <- (x.(c) + x.(d)) land mask;
  x.(b) <- rotl (x.(b) lxor x.(c)) 12;
  x.(a) <- (x.(a) + x.(b)) land mask;
  x.(d) <- rotl (x.(d) lxor x.(a)) 8;
  x.(c) <- (x.(c) + x.(d)) land mask;
  x.(b) <- rotl (x.(b) lxor x.(c)) 7

let block ~(key : string) ~(nonce : string) (counter : int) : string =
  let init =
    Array.concat
      [ [| 0x61707865; 0x3320646e; 0x79622d32; 0x6b206574 |]; Array.init 8 (fun i -> le32 key (4 * i)); [| counter land mask |];
        Array.init 3 (fun i -> le32 nonce (4 * i)) ]
  in
  let x = Array.copy init in
  for _ = 1 to 10 do
    quarter x 0 4 8 12;
    quarter x 1 5 9 13;
    quarter x 2 6 10 14;
    quarter x 3 7 11 15;
    quarter x 0 5 10 15;
    quarter x 1 6 11 12;
    quarter x 2 7 8 13;
    quarter x 3 4 9 14
  done;
  String.init 64 (fun i -> Char.chr ((((x.(i / 4) + init.(i / 4)) land mask) lsr (8 * (i mod 4))) land 0xff))

let encrypt ~(key : string) ~(nonce : string) ~(counter : int) (data : string) : string =
  let n = String.length data in
  let out = Bytes.create n in
  let rec go i c =
    if i < n then (
      let ks = block ~key ~nonce c in
      for j = 0 to min 64 (n - i) - 1 do
        Bytes.set out (i + j) (Char.chr (Char.code data.[i + j] lxor Char.code ks.[j]))
      done;
      go (i + 64) (c + 1))
  in
  go 0 counter;
  Bytes.to_string out
