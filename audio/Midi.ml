(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Midi.mli *)

type note = { start : float; length : float; key : int; velocity : int; channel : int; program : int }
type score = { notes : note list; duration : float }

(*****************************************************************************)
(* Variable-length quantities *)
(*****************************************************************************)

let vlq (n : int) : string =
  (* 7 bits at a time, the lowest last; every byte but the last with
   * its high bit set *)
  let rec groups n acc = if n < 128 then n :: acc else groups (n lsr 7) ((n land 127) :: acc) in
  let gs = groups n [] in
  let last = List.length gs - 1 in
  String.concat "" (List.mapi (fun i g -> String.make 1 (Char.chr (if i < last then g lor 128 else g))) gs)

let read_vlq (s : string) (i : int) : int * int =
  let rec go i acc =
    let b = Char.code s.[i] in
    let acc = (acc lsl 7) lor (b land 127) in
    if b land 128 <> 0 then go (i + 1) acc else (acc, i + 1)
  in
  go i 0

(*****************************************************************************)
(* Reading *)
(*****************************************************************************)

let u32 s i = (Char.code s.[i] lsl 24) lor (Char.code s.[i + 1] lsl 16) lor (Char.code s.[i + 2] lsl 8) lor Char.code s.[i + 3]
let u16 s i = (Char.code s.[i] lsl 8) lor Char.code s.[i + 1]

(* a track's events at their ticks: (tick, `Tempo us | `On (ch, key, vel)
 * | `Off (ch, key) | `Program (ch, p)) *)
let track_events (s : string) (start : int) (stop : int) =
  let events = ref [] and tick = ref 0 and status = ref 0 in
  let i = ref start in
  (try
     while !i < stop do
       let (delta, j) = read_vlq s !i in
       tick := !tick + delta;
       i := j;
       let b = Char.code s.[!i] in
       (* running status: a data byte where a status was expected *)
       let st = if b land 0x80 <> 0 then (incr i; if b < 0xF0 then status := b; b) else !status in
       let data () = let d = Char.code s.[!i] in incr i; d in
       match st land 0xF0 with
       | 0x90 ->
           let k = data () in
           let v = data () in
           events := (!tick, if v = 0 then `Off (st land 15, k) else `On (st land 15, k, v)) :: !events
       | 0x80 ->
           let k = data () in
           ignore (data ());
           events := (!tick, `Off (st land 15, k)) :: !events
       | 0xC0 -> events := (!tick, `Program (st land 15, data ())) :: !events
       | 0xD0 -> ignore (data ())
       | 0xA0 | 0xB0 | 0xE0 -> ignore (data ()); ignore (data ())
       | 0xF0 when st = 0xFF ->
           (* a meta event: its type, its length, its data *)
           let typ = data () in
           let (len, j) = read_vlq s !i in
           (* the tempo: 3 bytes, microseconds per quarter note *)
           if typ = 0x51 && len = 3 then
             events := (!tick, `Tempo ((Char.code s.[j] lsl 16) lor (Char.code s.[j + 1] lsl 8) lor Char.code s.[j + 2])) :: !events;
           i := j + len;
           if typ = 0x2F then i := stop
       | 0xF0 ->
           (* a system exclusive message: its length, skipped *)
           let (len, j) = read_vlq s !i in
           i := j + len
       | _ -> i := stop
     done
   with Invalid_argument _ -> ());
  List.rev !events

let parse (s : string) : (score, string) result =
  if String.length s < 14 || String.sub s 0 4 <> "MThd" then Error "not a MIDI file (no MThd)"
  else
    let tracks = u16 s 10 and division = u16 s 12 in
    if division land 0x8000 <> 0 then Error "SMPTE time division: not supported"
    else
      (* the tracks' chunks *)
      let rec chunks i n acc =
        if n = 0 || i + 8 > String.length s then List.rev acc
        else
          let len = u32 s (i + 4) in
          let acc = if String.sub s i 4 = "MTrk" then track_events s (i + 8) (min (String.length s) (i + 8 + len)) :: acc else acc in
          chunks (i + 8 + len) (n - 1) acc
      in
      let all = List.stable_sort (fun (a, _) (b, _) -> compare a b) (List.concat (chunks (8 + u32 s 4) tracks [])) in
      (* ticks to seconds, walking the tempo map *)
      let tempo = ref 500000 and last_tick = ref 0 and last_time = ref 0. in
      let seconds tick = !last_time +. (float_of_int (tick - !last_tick) *. float_of_int !tempo /. 1e6 /. float_of_int division) in
      let programs = Array.make 16 0 and on = Hashtbl.create 64 and notes = ref [] in
      List.iter
        (fun (tick, ev) ->
          let time = seconds tick in
          match ev with
          | `Tempo us ->
              last_time := time;
              last_tick := tick;
              tempo := us
          | `Program (ch, p) -> programs.(ch) <- p
          | `On (ch, key, velocity) -> Hashtbl.add on (ch, key) (time, velocity)
          | `Off (ch, key) -> (
              match Hashtbl.find_opt on (ch, key) with
              | Some (start, velocity) ->
                  Hashtbl.remove on (ch, key);
                  notes := { start; length = time -. start; key; velocity; channel = ch; program = programs.(ch) } :: !notes
              | None -> ()))
        all;
      let notes = List.stable_sort (fun a b -> compare a.start b.start) !notes in
      let duration = List.fold_left (fun m n -> Float.max m (n.start +. n.length)) 0. notes in
      if notes = [] then Error "no notes" else Ok { notes; duration }

(*****************************************************************************)
(* Writing *)
(*****************************************************************************)

let be32 n = String.init 4 (fun i -> Char.chr ((n lsr (8 * (3 - i))) land 255))
let be16 n = String.init 2 (fun i -> Char.chr ((n lsr (8 * (1 - i))) land 255))
let chunk tag body = tag ^ be32 (String.length body) ^ body
let bytes l = String.init (List.length l) (fun i -> Char.chr (List.nth l i))

let of_tune ?(program = 80) (tune : Abc.tune) : string =
  let division = 480 in
  (* 120 beats a minute: a second is two quarters, 960 ticks *)
  let ticks seconds = int_of_float (Float.round (seconds *. 960.)) in
  let track ch (events : Abc.event list) =
    (* note on at the start, off at the end, as (tick, bytes), in time
     * order, offs before ons at the same tick *)
    let msgs =
      List.concat_map
        (fun (e : Abc.event) ->
          List.concat_map
            (fun k -> [ (ticks e.start, 1, bytes [ 0x90 lor ch; k; 100 ]); (ticks (e.start +. e.length), 0, bytes [ 0x80 lor ch; k; 64 ]) ])
            e.notes)
        events
      |> List.stable_sort compare
    in
    let (body, _) =
      List.fold_left (fun (acc, last) (tick, _, msg) -> (acc ^ vlq (tick - last) ^ msg, tick)) (vlq 0 ^ bytes [ 0xC0 lor ch; program ], 0) msgs
    in
    chunk "MTrk" (body ^ vlq 0 ^ bytes [ 0xFF; 0x2F; 0 ])
  in
  let tempo = chunk "MTrk" (vlq 0 ^ bytes [ 0xFF; 0x51; 3; 0x07; 0xA1; 0x20 ] ^ vlq 0 ^ bytes [ 0xFF; 0x2F; 0 ]) in
  (* a percussion voice goes to channel 10 (9 from 0), General MIDI's drums *)
  let tracks = List.mapi (fun i events -> track (if List.nth_opt tune.drums i = Some true then 9 else i) events) tune.voices in
  chunk "MThd" (be16 1 ^ be16 (1 + List.length tracks) ^ be16 division) ^ String.concat "" (tempo :: tracks)
