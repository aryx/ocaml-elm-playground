(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Media.mli *)

type kind = Wav | Midi | Mod | Abc | Solfege | Png | Gif | Jpeg | Xpm

let kind_name = function
  | Wav -> "WAV"
  | Midi -> "MIDI"
  | Mod -> "MOD"
  | Abc -> "ABC"
  | Solfege -> "solfege"
  | Png -> "PNG"
  | Gif -> "GIF"
  | Jpeg -> "JPEG"
  | Xpm -> "XPM"

(*****************************************************************************)
(* What it is *)
(*****************************************************************************)

let starts (s : string) (at : int) (magic : string) : bool =
  String.length s >= at + String.length magic && String.sub s at (String.length magic) = magic

let by_bytes (s : string) : kind option =
  if starts s 0 "RIFF" && starts s 8 "WAVE" then Some Wav
  else if starts s 0 "MThd" then Some Midi
  else if String.length s >= 1084 && Mod.channels_of_tag (String.sub s 1080 4) <> None then Some Mod
  else if starts s 0 "\137PNG\r\n\026\n" then Some Png
  else if starts s 0 "GIF87a" || starts s 0 "GIF89a" then Some Gif
  else if starts s 0 "\255\216\255" then Some Jpeg
  else if starts s 0 "/* XPM */" then Some Xpm
  else if starts s 0 "X:" then Some Abc
  else None

let by_name (name : string) : kind option =
  let ends = Filename.check_suffix (String.lowercase_ascii name) in
  if ends ".mod" then Some Mod
  else if ends ".doremi" || ends ".txt" then Some Solfege
  else if ends ".abc" then Some Abc
  else None

let sniff ~(name : string) (bytes : string) : kind option =
  match by_bytes bytes with Some k -> Some k | None -> by_name name

(*****************************************************************************)
(* What it holds *)
(*****************************************************************************)

type media =
  | Sound of { samples : Signal.stereo; notes : Midi.note list }
  | Module of Mod.song
  | Picture of Rgba_image.t
  | Movie of Movie.t

(* an XPM's characters as pixels: each its palette's color, or
 * transparent ("None") *)
let xpm_picture (x : Xpm.t) : Rgba_image.t =
  let height = List.length x.rows in
  let width = List.fold_left (fun w r -> max w (String.length r)) 0 x.rows in
  let img = Rgba_image.create ~width ~height in
  List.iteri
    (fun y row ->
      String.iteri
        (fun cx ch ->
          match List.assoc_opt ch x.colors with
          | Some (Some (r, g, b)) ->
              let i = 4 * ((y * width) + cx) in
              img.rgba.{i} <- r;
              img.rgba.{i + 1} <- g;
              img.rgba.{i + 2} <- b;
              img.rgba.{i + 3} <- 255
          | _ -> ())
        row)
    x.rows;
  img

(* a GIF's frames as a movie; a delay under 0.02 s taken as 0.02 (the
 * browsers go further, making 0 and 0.01 s 0.1 s) -- a GIF of zero
 * delays would otherwise be frames of no time at all *)
let gif_movie (frames : (Rgba_image.t * float) list) : Movie.t = Movie.of_frames (List.map (fun (img, d) -> (img, Float.max 0.02 d)) frames)

(* a tune: played by the synthesizer's band, its notes read back from
 * the MIDI file it makes (Midi.of_tune), for the piano roll *)
let tune (t : Abc.tune) : media =
  let notes = match Midi.parse (Midi.of_tune t) with Ok score -> score.notes | Error _ -> [] in
  Sound { samples = Synth.render_stereo (Music.to_sound t); notes }

let open_ ~(name : string) (bytes : string) : (kind * media, string) result =
  match sniff ~name bytes with
  | None -> Error (name ^ ": not a kind of file this player knows")
  | Some kind -> (
      let media =
        match kind with
        | Wav -> Result.map (fun s -> Sound { samples = Signal.both s; notes = [] }) (Wav.of_string bytes)
        | Midi -> Result.map (fun (score : Midi.score) -> Sound { samples = Signal.both (Music.render_score score); notes = score.notes }) (Midi.parse bytes)
        | Mod -> Result.map (fun song -> Module song) (Mod.of_string bytes)
        | Abc -> Result.map tune (Abc.parse bytes)
        | Solfege -> Result.map tune (Doremi.parse bytes)
        (* the decoders raise on a broken file: caught below *)
        | Png -> Ok (Picture (Png.decode bytes))
        | Jpeg -> Ok (Picture (Jpeg.decode bytes))
        | Gif -> Ok (match Gif.animation bytes with [ (image, _) ] -> Picture image | frames -> Movie (gif_movie frames))
        | Xpm -> Ok (Picture (xpm_picture (Xpm.parse bytes)))
      in
      match media with Ok m -> Ok (kind, m) | Error e -> Error (name ^ ": " ^ e) | exception e -> Error (name ^ ": " ^ Printexc.to_string e))

let duration (m : media) : float option =
  match m with
  | Sound s -> Some (float_of_int (Array.length s.samples.left) /. float_of_int Signal.rate)
  | Movie movie -> Some movie.duration
  | Module _ | Picture _ -> None
