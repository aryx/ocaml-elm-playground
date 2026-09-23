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

(* [channels] interleaved: the frames, each a sample per channel *)
let to_string_channels (channels : Signal.t list) : string =
  let c = List.length channels in
  let n = match channels with [] -> 0 | s :: _ -> Array.length s in
  let b = Buffer.create (44 + (2 * c * n)) in
  let u32 v = Buffer.add_int32_le b (Int32.of_int v) and u16 v = Buffer.add_uint16_le b v in
  Buffer.add_string b "RIFF";
  u32 (36 + (2 * c * n));
  Buffer.add_string b "WAVEfmt ";
  u32 16;
  u16 1;
  u16 c;
  u32 Signal.rate;
  u32 (2 * c * Signal.rate);
  u16 (2 * c);
  u16 16;
  Buffer.add_string b "data";
  u32 (2 * c * n);
  for i = 0 to n - 1 do
    List.iter (fun s -> Buffer.add_int16_le b (Signal.to_int16 s.(i))) channels
  done;
  Buffer.contents b

let to_string (samples : Signal.t) : string = to_string_channels [ samples ]

(* the chunks after "RIFF" size "WAVE": each a 4-letter name, a size,
 * its bytes (padded to an even size); "fmt " and "data" are the two we
 * need, and a file may have others (LIST, fact, ...) before, between,
 * after *)
let chunks (s : string) : (string * int * int) list =
  let rec go i acc =
    if i + 8 > String.length s then List.rev acc
    else
      let size = Int32.to_int (String.get_int32_le s (i + 4)) in
      go (i + 8 + size + (size land 1)) ((String.sub s i 4, i + 8, size) :: acc)
  in
  go 12 []

let of_string (s : string) : (Signal.t, string) result =
  if String.length s < 12 || String.sub s 0 4 <> "RIFF" || String.sub s 8 4 <> "WAVE" then Error "not a WAV file"
  else
    let cs = chunks s in
    match (List.find_opt (fun (n, _, _) -> n = "fmt ") cs, List.find_opt (fun (n, _, _) -> n = "data") cs) with
    | Some (_, fmt, _), Some (_, data, size) ->
        let u16 i = String.get_uint16_le s (fmt + i) in
        let channels = u16 2 and rate = Int32.to_int (String.get_int32_le s (fmt + 4)) in
        if u16 0 <> 1 || u16 14 <> 16 || (channels <> 1 && channels <> 2) then Error "not 16-bit PCM, mono or stereo"
        else
          let size = min size (String.length s - data) in
          let frames = size / (2 * channels) in
          let sample i c = float_of_int (String.get_int16_le s (data + (2 * ((i * channels) + c)))) /. 32767. in
          (* stereo mixed down: a sound here is mono until panned *)
          let mono =
            Array.init frames (fun i -> if channels = 1 then sample i 0 else (sample i 0 +. sample i 1) /. 2.)
          in
          Ok (Resample.to_rate Linear rate mono)
    | _ -> Error "no fmt or no data chunk"

let write (path : string) (samples : Signal.t) : unit =
  Out_channel.with_open_bin path (fun oc -> Out_channel.output_string oc (to_string samples))

let read (path : string) : (Signal.t, string) result = of_string (In_channel.with_open_bin path In_channel.input_all)

let write_stereo (path : string) (s : Signal.stereo) : unit =
  Out_channel.with_open_bin path (fun oc -> Out_channel.output_string oc (to_string_channels [ s.left; s.right ]))
