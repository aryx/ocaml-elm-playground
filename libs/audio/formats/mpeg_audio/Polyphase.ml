(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Polyphase.mli *)

(* Table B.3's values times 2^16, the signs of every other block of 64
 * undone: h.(i) for i from 0 to 256, h.(512 - i) = h.(i) *)
let prototype =
  [| 0; -1; -1; -1; -1; -1; -1; -2; -2; -2;
     -2; -3; -3; -4; -4; -5; -5; -6; -7; -7;
     -8; -9; -10; -11; -13; -14; -16; -17; -19; -21;
     -24; -26; -29; -31; -35; -38; -41; -45; -49; -53;
     -58; -63; -68; -73; -79; -85; -91; -97; -104; -111;
     -117; -125; -132; -139; -147; -154; -161; -169; -176; -183;
     -190; -196; -202; -208; -213; -218; -222; -225; -227; -228;
     -228; -227; -224; -221; -215; -208; -200; -189; -177; -163;
     -146; -127; -106; -83; -57; -29; 2; 36; 72; 111;
     153; 197; 244; 294; 347; 401; 459; 519; 581; 645;
     711; 779; 848; 919; 991; 1064; 1137; 1210; 1283; 1356;
     1428; 1498; 1567; 1634; 1698; 1759; 1817; 1870; 1919; 1962;
     2001; 2032; 2057; 2075; 2085; 2087; 2080; 2063; 2037; 2000;
     1952; 1893; 1822; 1739; 1644; 1535; 1414; 1280; 1131; 970;
     794; 605; 402; 185; -45; -288; -545; -814; -1095; -1388;
     -1692; -2006; -2330; -2663; -3004; -3351; -3705; -4063; -4425; -4788;
     -5153; -5517; -5879; -6237; -6589; -6935; -7271; -7597; -7910; -8209;
     -8491; -8755; -8998; -9219; -9416; -9585; -9727; -9838; -9916; -9959;
     -9966; -9935; -9863; -9750; -9592; -9389; -9139; -8840; -8492; -8092;
     -7640; -7134; -6574; -5959; -5288; -4561; -3776; -2935; -2037; -1082;
     -70; 998; 2122; 3300; 4533; 5818; 7154; 8540; 9975; 11455;
     12980; 14548; 16155; 17799; 19478; 21189; 22929; 24694; 26482; 28289;
     30112; 31947; 33791; 35640; 37489; 39336; 41176; 43006; 44821; 46617;
     48390; 50137; 51853; 53534; 55178; 56778; 58333; 59838; 61289; 62684;
     64019; 65290; 66494; 67629; 68692; 69679; 70590; 71420; 72169; 72835;
     73415; 73908; 74313; 74630; 74856; 74992; 75038 |]

let window =
  Array.init 512 (fun i ->
      let h = prototype.(if i <= 256 then i else 512 - i) in
      let sign = if i / 64 mod 2 = 1 then -1. else 1. in
      sign *. float_of_int h /. 65536.)

(* the matrixing's cosines, N[i][k] *)
let n = Array.init 64 (fun i -> Array.init 32 (fun k -> cos (float_of_int ((16 + i) * ((2 * k) + 1)) *. Float.pi /. 64.)))

(* V as a ring: the newest 64 values at [top], older ones after it; the
 * standard shifts all 1024 each time slot instead *)
type t = { v : float array; mutable top : int }

let create () : t = { v = Array.make 1024 0.; top = 0 }

(* row i of the matrixing, N[i] . S *)
let row (i : int) (slot : float array) : float =
  let r = n.(i) and sum = ref 0. in
  for k = 0 to 31 do
    sum := !sum +. (r.(k) *. slot.(k))
  done;
  !sum

(* claude: half of the matrixing's 64 rows, the others their mirrors.
 * The standard's way, all 64 computed, is
 *
 *   for i = 0 to 63 do f.v.((f.top + i) land 1023) <- row i slot done
 *
 * but the cosines repeat: cos((16 + i)(2k + 1) pi / 64) with 16 + i
 * replaced by 64 - (16 + i) changes sign (cos((2k + 1) pi - x) = -cos x,
 * 2k + 1 odd), and by 128 - (16 + i) doesn't (cos(2 pi (2k + 1) - x) =
 * cos x): V[32 - i] = -V[i] (so V[16] = 0) and V[96 - i] = V[i]. So
 * rows 0 to 15 and 33 to 48 are computed, 1024 multiplications instead
 * of 2048 -- the matrixing was 80% of the filterbank, the filterbank
 * half of an MP3's decoding (notes_opti_ocaml.md). A fast DCT goes much
 * further (Polyphase.mli), at the price of the formula's plainness. *)
let synthesize (f : t) (slot : float array) (out : float array) (at : int) : unit =
  f.top <- (f.top + 1024 - 64) land 1023;
  let v i x = f.v.((f.top + i) land 1023) <- x in
  for i = 0 to 15 do
    let x = row i slot in
    v i x;
    v (32 - i) (-.x)
  done;
  v 16 0.;
  for i = 33 to 48 do
    let x = row i slot in
    v i x;
    v (96 - i) x (* row 48 its own mirror *)
  done;
  (* U[i * 64 + j] is V[i * 128 + j], U[i * 64 + 32 + j] is V[i * 128 +
   * 96 + j]: out[j] sums U[j + 32 k], k even from the first, odd from
   * the second *)
  for j = 0 to 31 do
    let sum = ref 0. in
    for i = 0 to 7 do
      sum := !sum +. (f.v.((f.top + (i * 128) + j) land 1023) *. window.(j + (64 * i)));
      sum := !sum +. (f.v.((f.top + (i * 128) + 96 + j) land 1023) *. window.(j + (64 * i) + 32))
    done;
    out.(at + j) <- !sum
  done
