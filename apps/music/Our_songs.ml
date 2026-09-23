(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Our_songs.mli *)

let instrument name (a : float array) ~loop : Mod.instrument =
  let data = Mod.data_of_floats a in
  { name; finetune = 0; volume = 64; loop_start = 0; loop_length = (if loop then String.length data else 0); data }

let cell ?(i = 1) ?(e = 0) ?(x = 0) (n : string) : Mod.cell =
  { instrument = i; period = Option.value (Mod.period_of_name n) ~default:0; effect = e; param = x }

let soundtracker_song : Mod.song =
  let pi2 = 2. *. Float.pi in
  let lead = instrument "lead" (Array.init 32 (fun i -> if i < 8 then 0.6 else -0.2)) ~loop:true in
  let bass =
    instrument "bass" (Array.init 64 (fun i -> let u = float_of_int i /. 64. in 0.8 *. if u < 0.5 then (4. *. u) -. 1. else 3. -. (4. *. u))) ~loop:true
  in
  let kick =
    instrument "kick"
      (Array.init 2400 (fun i ->
           let t = float_of_int i /. 8287. in
           0.9 *. exp (-.t *. 18.) *. sin (pi2 *. ((60. *. t) +. (140. *. (1. -. exp (-.t *. 30.)) /. 30.)))))
      ~loop:false
  in
  let seed = ref 7 in
  let snare =
    instrument "snare"
      (Array.init 2000 (fun i ->
           seed := ((!seed * 1103515245) + 12345) land 0x7FFFFFFF;
           0.7 *. exp (-.float_of_int i /. 350.) *. ((float_of_int ((!seed lsr 8) land 0xFF) /. 128.) -. 1.)))
      ~loop:false
  in
  let drums = List.concat_map (fun r -> [ (r, 2, cell ~i:3 "C-2"); (r + 4, 3, cell ~i:4 ~e:0xC ~x:0x30 "C-2") ]) [ 0; 8; 16; 24; 32; 40; 48; 56 ] in
  let bass_line = List.mapi (fun k n -> (k * 8, 1, cell ~i:2 n)) [ "A-1"; "A-1"; "F-1"; "F-1"; "C-2"; "C-2"; "G-1"; "G-1" ] in
  let melody notes = List.mapi (fun k n -> (k * 4, 0, cell ~i:1 n)) notes in
  let first = melody [ "A-2"; "C-3"; "E-3"; "A-3"; "G-3"; "E-3"; "C-3"; "D-3"; "F-2"; "A-2"; "C-3"; "F-3"; "E-3"; "C-3"; "B-2"; "G-2" ] in
  (* the second time, chords: the arpeggio's minor and major thirds *)
  let second =
    List.mapi (fun k (n, x) -> (k * 8, 0, cell ~i:1 ~e:0 ~x n)) [ ("A-2", 0x37); ("A-2", 0x37); ("F-2", 0x47); ("F-2", 0x47); ("C-3", 0x47); ("C-3", 0x47); ("G-2", 0x47); ("G-2", 0x47) ]
  in
  let pattern cells =
    Array.init 64 (fun r -> Array.init 4 (fun c -> match List.find_opt (fun (r', c', _) -> r = r' && c = c') cells with Some (_, _, x) -> x | None -> Mod.empty_cell))
  in
  let blank = instrument "" [||] ~loop:false in
  {
    title = "tiny soundtracker";
    instruments = Array.init 31 (fun k -> match k with 0 -> lead | 1 -> bass | 2 -> kick | 3 -> snare | _ -> blank);
    restart = 127;
    positions = [| 0; 1 |];
    patterns = [| pattern (first @ bass_line @ drums); pattern (second @ bass_line @ drums) |];
    tag = "M.K.";
  }

