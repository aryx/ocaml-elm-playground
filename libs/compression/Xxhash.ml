(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Xxhash.mli *)

let ( +: ) = Int64.add
let ( *: ) = Int64.mul
let ( ^: ) = Int64.logxor
let ( >>: ) = Int64.shift_right_logical
let rotl (x : int64) (n : int) : int64 = Int64.logor (Int64.shift_left x n) (x >>: (64 - n))

(* five primes, their bits half ones, half zeros *)
let p1 = 0x9E3779B185EBCA87L
let p2 = 0xC2B2AE3D27D4EB4FL
let p3 = 0x165667B19E3779F9L
let p4 = 0x85EBCA77C2B2AE63L
let p5 = 0x27D4EB2F165667C5L

(* 8 bytes into an accumulator *)
let round (acc : int64) (input : int64) : int64 = rotl (acc +: (input *: p2)) 31 *: p1

let xxh64 (s : string) : int64 =
  let n = String.length s in
  let pos = ref 0 in
  (* the stripes of 32 bytes: four accumulators, each taking 8 bytes,
   * none depending on another *)
  let h =
    if n < 32 then p5
    else begin
      let v = [| p1 +: p2; p2; 0L; Int64.neg p1 |] in
      while !pos + 32 <= n do
        for i = 0 to 3 do
          v.(i) <- round v.(i) (String.get_int64_le s (!pos + (8 * i)))
        done;
        pos := !pos + 32
      done;
      let merge h v = ((h ^: round 0L v) *: p1) +: p4 in
      Array.fold_left merge (rotl v.(0) 1 +: rotl v.(1) 7 +: rotl v.(2) 12 +: rotl v.(3) 18) v
    end
  in
  let h = ref (h +: Int64.of_int n) in
  (* what is left, under 32 bytes: by 8, by 4, by 1 *)
  while !pos + 8 <= n do
    h := (rotl (!h ^: round 0L (String.get_int64_le s !pos)) 27 *: p1) +: p4;
    pos := !pos + 8
  done;
  if !pos + 4 <= n then begin
    let word = Int64.logand (Int64.of_int32 (String.get_int32_le s !pos)) 0xFFFFFFFFL in
    h := (rotl (!h ^: (word *: p1)) 23 *: p2) +: p3;
    pos := !pos + 4
  end;
  while !pos < n do
    h := rotl (!h ^: (Int64.of_int (Char.code s.[!pos]) *: p5)) 11 *: p1;
    incr pos
  done;
  (* the avalanche: every bit of the input to every bit of the hash *)
  let h = !h in
  let h = (h ^: (h >>: 33)) *: p2 in
  let h = (h ^: (h >>: 29)) *: p3 in
  h ^: (h >>: 32)
