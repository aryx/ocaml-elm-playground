(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Mpeg_audio_header.mli *)

type version = Mpeg1 | Mpeg2 | Mpeg2_5
type mode = Stereo | Joint_stereo | Dual_channel | Mono

type t = {
  version : version;
  layer : int;
  crc : bool;
  bitrate : int;
  sample_rate : int;
  padding : bool;
  mode : mode;
  mode_extension : int;
  channels : int;
  length : int;
  samples : int;
}

let version_name = function Mpeg1 -> "MPEG-1" | Mpeg2 -> "MPEG-2" | Mpeg2_5 -> "MPEG-2.5"

(* kbit/s, by bitrate index (0, free format, and 15, reserved, apart) *)
let bitrates_mpeg1 =
  [| [| 32; 64; 96; 128; 160; 192; 224; 256; 288; 320; 352; 384; 416; 448 |] (* I *);
     [| 32; 48; 56; 64; 80; 96; 112; 128; 160; 192; 224; 256; 320; 384 |] (* II *);
     [| 32; 40; 48; 56; 64; 80; 96; 112; 128; 160; 192; 224; 256; 320 |] (* III *) |]

let bitrates_mpeg2 =
  [| [| 32; 48; 56; 64; 80; 96; 112; 128; 144; 160; 176; 192; 224; 256 |] (* I *);
     [| 8; 16; 24; 32; 40; 48; 56; 64; 80; 96; 112; 128; 144; 160 |] (* II and III *) |]

let parse (s : string) (at : int) : t option =
  if at < 0 || at + 4 > String.length s then None
  else
    let byte i = Char.code s.[at + i] in
    let h = (byte 0 lsl 24) lor (byte 1 lsl 16) lor (byte 2 lsl 8) lor byte 3 in
    let field shift width = (h lsr shift) land ((1 lsl width) - 1) in
    let version = match field 19 2 with 3 -> Some Mpeg1 | 2 -> Some Mpeg2 | 0 -> Some Mpeg2_5 | _ -> None in
    let layer = 4 - field 17 2 (* 01 is III *) in
    let br = field 12 4 and sr = field 10 2 in
    match version with
    | Some version when field 21 11 = 0x7FF && layer <= 3 && br <> 0 && br <> 15 && sr <> 3 ->
        let bitrate =
          1000
          * (match version with
            | Mpeg1 -> bitrates_mpeg1.(layer - 1).(br - 1)
            | Mpeg2 | Mpeg2_5 -> bitrates_mpeg2.(min 1 (layer - 1)).(br - 1))
        in
        let base = [| 44100; 48000; 32000 |].(sr) in
        let sample_rate = match version with Mpeg1 -> base | Mpeg2 -> base / 2 | Mpeg2_5 -> base / 4 in
        let padding = field 9 1 = 1 in
        let mode = [| Stereo; Joint_stereo; Dual_channel; Mono |].(field 6 2) in
        let pad = if padding then 1 else 0 in
        let samples = match (layer, version) with 1, _ -> 384 | 3, (Mpeg2 | Mpeg2_5) -> 576 | _ -> 1152 in
        (* the frame's bytes: samples / 8 bits a byte, of bitrate /
         * sample_rate bits a sample; Layer I counts in 4-byte slots *)
        let length =
          if layer = 1 then ((12 * bitrate / sample_rate) + pad) * 4 else (samples / 8 * bitrate / sample_rate) + pad
        in
        Some
          { version; layer; crc = field 16 1 = 0; bitrate; sample_rate; padding; mode; mode_extension = field 4 2;
            channels = (if mode = Mono then 1 else 2); length; samples }
    | _ -> None

(* past an ID3v2 tag: 10 bytes, its size in 4 bytes of 7 bits (no byte
 * with its top bit set, so no false sync inside), and a footer of 10
 * more if flagged *)
let skip_id3 (s : string) : int =
  if String.length s >= 10 && String.sub s 0 3 = "ID3" then
    let b i = Char.code s.[i] land 0x7F in
    let size = (b 6 lsl 21) lor (b 7 lsl 14) lor (b 8 lsl 7) lor b 9 in
    10 + size + if Char.code s.[5] land 0x10 <> 0 then 10 else 0
  else 0

let alike (a : t) (b : t) : bool = a.version = b.version && a.layer = b.layer && a.sample_rate = b.sample_rate

(* a header at [at], followed by another like it, or by the end *)
let frame_at (s : string) (at : int) : t option =
  match parse s at with
  | Some h when at + h.length >= String.length s -> Some h
  | Some h when (match parse s (at + h.length) with Some h' -> alike h h' | None -> false) -> Some h
  | _ -> None

let first_frame (s : string) : (int * t) option =
  let rec search at =
    if at + 4 > String.length s then None else match frame_at s at with Some h -> Some (at, h) | None -> search (at + 1)
  in
  search (skip_id3 s)

let at_start (s : string) : t option = frame_at s (skip_id3 s)

let frames (s : string) : (int * t) list =
  match first_frame s with
  | None -> []
  | Some (at, first) ->
      let n = String.length s in
      let rec go at acc =
        if at + 4 > n then List.rev acc
        else
          match parse s at with
          | Some h when alike h first && at + h.length <= n -> go (at + h.length) ((at, h) :: acc)
          | _ -> go (at + 1) acc
      in
      go at []
