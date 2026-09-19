(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Wav.mli *)

let to_string (samples : Signal.t) : string =
  let n = Array.length samples in
  let b = Buffer.create (44 + (2 * n)) in
  let u32 v = Buffer.add_int32_le b (Int32.of_int v) and u16 v = Buffer.add_uint16_le b v in
  Buffer.add_string b "RIFF";
  u32 (36 + (2 * n));
  Buffer.add_string b "WAVEfmt ";
  u32 16;
  u16 1;
  u16 1;
  u32 Signal.rate;
  u32 (2 * Signal.rate);
  u16 2;
  u16 16;
  Buffer.add_string b "data";
  u32 (2 * n);
  Array.iter (fun x -> Buffer.add_int16_le b (Signal.to_int16 x)) samples;
  Buffer.contents b

let of_string (s : string) : (Signal.t, string) result =
  let u16 i = String.get_uint16_le s i and u32 i = Int32.to_int (String.get_int32_le s i) in
  if String.length s < 44 || String.sub s 0 4 <> "RIFF" || String.sub s 8 8 <> "WAVEfmt " || String.sub s 36 4 <> "data"
  then Error "not a WAV file (or not a plain one)"
  else if u16 20 <> 1 || u16 22 <> 1 || u32 24 <> Signal.rate || u16 34 <> 16 then Error "not 16-bit mono PCM at 44,100 Hz"
  else
    let n = u32 40 / 2 in
    if String.length s < 44 + (2 * n) then Error "truncated"
    else Ok (Array.init n (fun i -> float_of_int (String.get_int16_le s (44 + (2 * i))) /. 32767.))

let write (path : string) (samples : Signal.t) : unit =
  Out_channel.with_open_bin path (fun oc -> Out_channel.output_string oc (to_string samples))

let read (path : string) : (Signal.t, string) result = of_string (In_channel.with_open_bin path In_channel.input_all)
