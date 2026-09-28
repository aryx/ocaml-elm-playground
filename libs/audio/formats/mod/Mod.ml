(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Mod.mli *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type instrument = { name : string; finetune : int; volume : int; loop_start : int; loop_length : int; data : string }
type cell = { instrument : int; period : int; effect : int; param : int }

type song = {
  title : string;
  instruments : instrument array;
  restart : int;
  positions : int array;
  patterns : cell array array array;
  tag : string;
}

let empty_cell = { instrument = 0; period = 0; effect = 0; param = 0 }

let channels_of_tag (tag : string) : int option =
  let digit c = if c >= '0' && c <= '9' then Some (Char.code c - Char.code '0') else None in
  match tag with
  | "M.K." | "M!K!" | "FLT4" | "4CHN" -> Some 4
  | "FLT8" | "OCTA" -> Some 8
  | _ when String.length tag = 4 && String.sub tag 1 3 = "CHN" -> digit tag.[0]
  | _ when String.length tag = 4 && String.sub tag 2 2 = "CH" -> (
      match (digit tag.[0], digit tag.[1]) with Some a, Some b -> Some ((10 * a) + b) | _ -> None)
  | _ -> None

let channels (s : song) : int = Option.value (channels_of_tag s.tag) ~default:4
let header_size (s : song) : int = if Array.length s.instruments = 15 then 600 else 1084

(*****************************************************************************)
(* Cells *)
(*****************************************************************************)

let cell_of_bytes (s : string) (i : int) : cell =
  let b k = Char.code s.[i + k] in
  {
    instrument = (b 0 land 0xF0) lor (b 2 lsr 4);
    period = ((b 0 land 0x0F) lsl 8) lor b 1;
    effect = b 2 land 0x0F;
    param = b 3;
  }

let cell_to_bytes (c : cell) : string =
  let b = Bytes.create 4 in
  let set k v = Bytes.set b k (Char.chr (v land 0xFF)) in
  set 0 ((c.instrument land 0xF0) lor ((c.period lsr 8) land 0x0F));
  set 1 c.period;
  set 2 (((c.instrument land 0x0F) lsl 4) lor (c.effect land 0x0F));
  set 3 c.param;
  Bytes.to_string b

(*****************************************************************************)
(* Periods and notes *)
(*****************************************************************************)

let periods =
  [|
    856; 808; 762; 720; 678; 640; 604; 570; 538; 508; 480; 453;
    428; 404; 381; 360; 339; 320; 302; 285; 269; 254; 240; 226;
    214; 202; 190; 180; 170; 160; 151; 143; 135; 127; 120; 113;
  |]

let names = [| "C-"; "C#"; "D-"; "D#"; "E-"; "F-"; "F#"; "G-"; "G#"; "A-"; "A#"; "B-" |]

let note_name (period : int) : string =
  if period = 0 then "---"
  else
    let best = ref 0 in
    Array.iteri (fun i p -> if abs (p - period) < abs (periods.(!best) - period) then best := i) periods;
    names.(!best mod 12) ^ string_of_int ((!best / 12) + 1)

let period_of_name (s : string) : int option =
  if String.length s <> 3 then None
  else
    let octave = Char.code s.[2] - Char.code '0' in
    let rec find i = if i = 12 then None else if names.(i) = String.sub s 0 2 then Some i else find (i + 1) in
    match find 0 with Some n when octave >= 1 && octave <= 3 -> Some periods.(((octave - 1) * 12) + n) | _ -> None

let paula_clock = 3546895.
let rate (period : int) : float = paula_clock /. float_of_int period

(*****************************************************************************)
(* Samples *)
(*****************************************************************************)

let sample (i : instrument) (k : int) : float =
  let b = Char.code i.data.[k] in
  float_of_int (if b >= 128 then b - 256 else b) /. 128.

let data_of_floats (a : float array) : string =
  let n = Array.length a + (Array.length a land 1) in
  String.init n (fun k ->
      if k >= Array.length a then '\000'
      else
        let v = int_of_float (Float.round (a.(k) *. 127.)) in
        Char.chr (max (-128) (min 127 v) land 0xFF))

(*****************************************************************************)
(* Reading *)
(*****************************************************************************)

let u16 (s : string) (i : int) : int = (Char.code s.[i] lsl 8) lor Char.code s.[i + 1]

(* a name's bytes up to its first zero *)
let text (s : string) (i : int) (n : int) : string =
  let t = String.sub s i n in
  match String.index_opt t '\000' with Some z -> String.sub t 0 z | None -> t

let of_string (s : string) : (song, string) result =
  let len = String.length s in
  (* the tag at 1080 says the form; no tag, Ultimate Soundtracker's *)
  let tag = if len >= 1084 then String.sub s 1080 4 else "" in
  let tag, count = match channels_of_tag tag with Some _ -> (tag, 31) | None -> ("", 15) in
  let header = if count = 31 then 1084 else 600 in
  let channels = Option.value (channels_of_tag tag) ~default:4 in
  if len < header then Error (Printf.sprintf "%d bytes: too short for a module's header (%d)" len header)
  else
    let base = 20 + (count * 30) in
    let length = Char.code s.[base] and restart = Char.code s.[base + 1] in
    let order = Array.init 128 (fun i -> Char.code s.[base + 2 + i]) in
    if length < 1 || length > 128 then Error (Printf.sprintf "a song of %d positions" length)
    else
      let npatterns = Array.fold_left max 0 order + 1 in
      let pattern_size = 64 * channels * 4 in
      let samples_at = header + (npatterns * pattern_size) in
      if len < samples_at then Error (Printf.sprintf "%d patterns, and the file ends inside them" npatterns)
      else
        let patterns =
          Array.init npatterns (fun p ->
              Array.init 64 (fun r -> Array.init channels (fun c -> cell_of_bytes s (header + (p * pattern_size) + (((r * channels) + c) * 4)))))
        in
        (* the instruments, their samples one after the other *)
        let at = ref samples_at in
        let instruments =
          Array.init count (fun i ->
              let h = 20 + (i * 30) in
              let bytes = 2 * u16 s (h + 22) in
              let f = Char.code s.[h + 24] land 0x0F in
              let loop_start = if count = 15 then u16 s (h + 26) else 2 * u16 s (h + 26) in
              let loop_words = u16 s (h + 28) in
              (* a sample cut short by the file's end: what there is of it *)
              let n = max 0 (min bytes (len - !at)) in
              let data = String.sub s !at n in
              at := !at + bytes;
              {
                name = text s h 22;
                finetune = (if f >= 8 then f - 16 else f);
                volume = min 64 (Char.code s.[h + 25]);
                loop_start;
                loop_length = (if loop_words <= 1 then 0 else 2 * loop_words);
                data;
              })
        in
        Ok { title = text s 0 20; instruments; restart; positions = Array.sub order 0 length; patterns; tag }

(*****************************************************************************)
(* Writing *)
(*****************************************************************************)

let to_string (song : song) : string =
  let b = Buffer.create 4096 in
  let fixed (t : string) (n : int) =
    Buffer.add_string b (String.sub t 0 (min n (String.length t)));
    Buffer.add_string b (String.make (max 0 (n - String.length t)) '\000')
  in
  let u16 v =
    Buffer.add_char b (Char.chr ((v lsr 8) land 0xFF));
    Buffer.add_char b (Char.chr (v land 0xFF))
  in
  fixed song.title 20;
  let fifteen = Array.length song.instruments = 15 in
  Array.iter
    (fun (i : instrument) ->
      fixed i.name 22;
      u16 ((String.length i.data + 1) / 2);
      Buffer.add_char b (Char.chr (i.finetune land 0x0F));
      Buffer.add_char b (Char.chr i.volume);
      u16 (if fifteen then i.loop_start else i.loop_start / 2);
      u16 (if i.loop_length = 0 then (if fifteen then 0 else 1) else i.loop_length / 2))
    song.instruments;
  Buffer.add_char b (Char.chr (Array.length song.positions));
  Buffer.add_char b (Char.chr song.restart);
  for i = 0 to 127 do
    Buffer.add_char b (Char.chr (if i < Array.length song.positions then song.positions.(i) else 0))
  done;
  if not fifteen then fixed song.tag 4;
  Array.iter (Array.iter (Array.iter (fun c -> Buffer.add_string b (cell_to_bytes c)))) song.patterns;
  Array.iter
    (fun (i : instrument) ->
      Buffer.add_string b i.data;
      if String.length i.data land 1 = 1 then Buffer.add_char b '\000')
    song.instruments;
  Buffer.contents b
