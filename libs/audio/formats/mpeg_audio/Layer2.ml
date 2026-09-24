(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Layer2.mli *)

(*****************************************************************************)
(* The bit allocation tables (Table B.2) *)
(*****************************************************************************)

(* the levels allowed, by allocation code; its length is 2^(bits of
 * the code) *)
let ab_0_2 = [| 0; 3; 7; 15; 31; 63; 127; 255; 511; 1023; 2047; 4095; 8191; 16383; 32767; 65535 |]
let ab_3_10 = [| 0; 3; 5; 7; 9; 15; 31; 63; 127; 255; 511; 1023; 2047; 4095; 8191; 65535 |]
let ab_11_22 = [| 0; 3; 5; 7; 9; 15; 31; 65535 |]
let ab_23_29 = [| 0; 3; 5; 65535 |]
let cd_0_1 = [| 0; 3; 5; 9; 15; 31; 63; 127; 255; 511; 1023; 2047; 4095; 8191; 16383; 32767 |]
let cd_2_11 = [| 0; 3; 5; 9; 15; 31; 63; 127 |]

(* ISO/IEC 13818-3's, for MPEG-2's lower sample rates *)
let lsf_0_3 = [| 0; 3; 5; 7; 9; 15; 31; 63; 127; 255; 511; 1023; 2047; 4095; 8191; 16383 |]
let lsf_4_10 = [| 0; 3; 5; 9; 15; 31; 63; 127 |]
let lsf_11_29 = [| 0; 3; 5; 9 |]

let bands (sblimit : int) (f : int -> int array) : int array array = Array.init sblimit f

let table_a = bands 27 (fun sb -> if sb < 3 then ab_0_2 else if sb < 11 then ab_3_10 else if sb < 23 then ab_11_22 else ab_23_29)
let table_b = bands 30 (fun sb -> if sb < 3 then ab_0_2 else if sb < 11 then ab_3_10 else if sb < 23 then ab_11_22 else ab_23_29)
let table_c = bands 8 (fun sb -> if sb < 2 then cd_0_1 else cd_2_11)
let table_d = bands 12 (fun sb -> if sb < 2 then cd_0_1 else cd_2_11)
let table_lsf = bands 30 (fun sb -> if sb < 4 then lsf_0_3 else if sb < 11 then lsf_4_10 else lsf_11_29)

(* which table: by the bitrate each channel gets (2.4.3.3.1, and Table
 * B.2's headings) *)
let table (h : Mpeg_audio_header.t) : int array array =
  if h.version <> Mpeg_audio_header.Mpeg1 then table_lsf
  else
    let per_channel = h.bitrate / h.channels / 1000 in
    if per_channel <= 48 then if h.sample_rate = 32000 then table_d else table_c
    else if per_channel <= 80 then table_a
    else if h.sample_rate = 48000 then table_a
    else table_b

(*****************************************************************************)
(* Samples *)
(*****************************************************************************)

let log2 (n : int) : int =
  let rec go k = if 1 lsl k >= n then k else go (k + 1) in
  go 0

(* 3 samples of [levels] levels, as codes: one grouped number for 3, 5
 * and 9 levels (5, 7 and 10 bits), or 3 numbers *)
let read_three (b : Bits.t) (levels : int) : int * int * int =
  match levels with
  | 3 | 5 | 9 ->
      let c = Bits.read b (match levels with 3 -> 5 | 5 -> 7 | _ -> 10) in
      (c mod levels, c / levels mod levels, c / levels / levels mod levels)
  | _ ->
      let bits = log2 (levels + 1) in
      let s0 = Bits.read b bits in
      let s1 = Bits.read b bits in
      (s0, s1, Bits.read b bits)

(* [c] of [n] levels, evenly spread in ]-1, 1[ *)
let fraction (n : int) (c : int) : float = float_of_int ((2 * c) + 1 - n) /. float_of_int n

let scalefactor (i : int) : float = 2. ** (1. -. (float_of_int i /. 3.))

let decode (h : Mpeg_audio_header.t) (b : Bits.t) : float array array =
  let nch = h.channels in
  let table = table h in
  let sblimit = Array.length table in
  let bound = if h.mode = Mpeg_audio_header.Joint_stereo then min sblimit (4 + (4 * h.mode_extension)) else sblimit in
  let nbal sb = log2 (Array.length table.(sb)) in
  (* bit allocation: below the bound for each channel, above it shared *)
  let alloc = Array.make_matrix nch 32 0 in
  for sb = 0 to sblimit - 1 do
    if sb < bound then
      for ch = 0 to nch - 1 do
        alloc.(ch).(sb) <- Bits.read b (nbal sb)
      done
    else
      let a = Bits.read b (nbal sb) in
      for ch = 0 to nch - 1 do
        alloc.(ch).(sb) <- a
      done
  done;
  let scfsi = Array.make_matrix nch 32 0 in
  for sb = 0 to sblimit - 1 do
    for ch = 0 to nch - 1 do
      if alloc.(ch).(sb) <> 0 then scfsi.(ch).(sb) <- Bits.read b 2
    done
  done;
  (* the 3 parts' scalefactors, per scfsi: 3 of them, or shared *)
  let sf = Array.init nch (fun _ -> Array.make_matrix 32 3 0.) in
  for sb = 0 to sblimit - 1 do
    for ch = 0 to nch - 1 do
      if alloc.(ch).(sb) <> 0 then (
        let read () = scalefactor (Bits.read b 6) in
        let s = sf.(ch).(sb) in
        match scfsi.(ch).(sb) with
        | 0 ->
            s.(0) <- read ();
            s.(1) <- read ();
            s.(2) <- read ()
        | 1 ->
            s.(0) <- read ();
            s.(1) <- s.(0);
            s.(2) <- read ()
        | 2 ->
            s.(0) <- read ();
            s.(1) <- s.(0);
            s.(2) <- s.(0)
        | _ ->
            s.(0) <- read ();
            s.(1) <- read ();
            s.(2) <- s.(1))
    done
  done;
  let out = Array.init nch (fun _ -> Array.make (36 * 32) 0.) in
  for gr = 0 to 11 do
    for sb = 0 to sblimit - 1 do
      for ch = 0 to (if sb < bound then nch - 1 else 0) do
        let a = alloc.(ch).(sb) in
        if a <> 0 then (
          let levels = table.(sb).(a) in
          let c0, c1, c2 = read_three b levels in
          (* below the bound, this channel's; above, both channels',
           * each with its scalefactor *)
          let targets = if sb < bound then [ ch ] else List.init nch Fun.id in
          List.iter
            (fun c ->
              let scale = sf.(c).(sb).(gr / 4) in
              List.iteri (fun k code -> out.(c).((((3 * gr) + k) * 32) + sb) <- fraction levels code *. scale) [ c0; c1; c2 ])
            targets)
      done
    done
  done;
  out
