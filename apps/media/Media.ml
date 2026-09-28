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

type kind = Wav | Mp2 | Mp3 | Midi | Mod | Abc | Solfege | Png | Gif | Jpeg | Xpm | Y4m | Flic | Avi | Mpeg1 | Mpg

let kind_name = function
  | Wav -> "WAV"
  | Mp2 -> "MP2"
  | Mp3 -> "MP3"
  | Midi -> "MIDI"
  | Mod -> "MOD"
  | Abc -> "ABC"
  | Solfege -> "solfege"
  | Png -> "PNG"
  | Gif -> "GIF"
  | Jpeg -> "JPEG"
  | Xpm -> "XPM"
  | Y4m -> "Y4M"
  | Flic -> "FLIC"
  | Avi -> "AVI"
  | Mpeg1 -> "MPEG-1"
  | Mpg -> "MPEG-1 system"

(*****************************************************************************)
(* What it is *)
(*****************************************************************************)

let starts (s : string) (at : int) (magic : string) : bool =
  String.length s >= at + String.length magic && String.sub s at (String.length magic) = magic

let by_bytes (s : string) : kind option =
  if starts s 0 "RIFF" && starts s 8 "WAVE" then Some Wav
  else if starts s 0 "RIFF" && starts s 8 "AVI " then Some Avi
  else if starts s 0 "MThd" then Some Midi
  else if String.length s >= 1084 && Mod.channels_of_tag (String.sub s 1080 4) <> None then Some Mod
  else if starts s 0 "\137PNG\r\n\026\n" then Some Png
  else if starts s 0 "GIF87a" || starts s 0 "GIF89a" then Some Gif
  else if starts s 0 "\255\216\255" then Some Jpeg
  else if starts s 0 "/* XPM */" then Some Xpm
  else if starts s 0 "YUV4MPEG2 " then Some Y4m
  else if starts s 0 "\000\000\001\xB3" then Some Mpeg1
  else if starts s 0 "\000\000\001\xBA" then Some Mpg
  else if String.length s >= 128 && (starts s 4 "\x11\xAF" || starts s 4 "\x12\xAF") then Some Flic
  else if starts s 0 "X:" then Some Abc
  else
    match Mpeg_audio_header.at_start s with
    | Some { layer = 2; _ } -> Some Mp2
    | Some { layer = 3; _ } -> Some Mp3
    | _ -> None

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
  | Movie of { movie : Movie.t; sound : Signal.stereo option; mpeg : (Mpeg1.header * (int -> Mpeg1.info) * Movie.t Lazy.t) option }

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

(* an MP2 or MP3, at our sample rate *)
let mpeg_sound (bytes : string) : (Signal.stereo, string) result =
  Result.map
    (fun ((h : Mpeg_audio_header.t), (s : Signal.stereo)) ->
      let at_our_rate x = if h.sample_rate = Signal.rate then x else Resample.to_rate Cubic h.sample_rate x in
      { Signal.left = at_our_rate s.left; right = at_our_rate s.right })
    (Mpeg_audio.decode bytes)

(* an MPEG-1 video stream, and for the analyzer its decisions and what
 * was sent *)
let mpeg1_movie (bytes : string) ~(sound : Signal.stereo option) : media =
  let header, movie, info = Mpeg1.of_string bytes in
  let sent = lazy (let _, m, _ = Mpeg1.of_string ~residual:true bytes in m) in
  Movie { movie; sound; mpeg = Some (header, info, sent) }

(* [s] starting [seconds] later (earlier if negative): silence added
 * before it, or its start cut *)
let delayed (seconds : float) (s : Signal.t) : Signal.t =
  let n = int_of_float (Float.round (seconds *. float_of_int Signal.rate)) in
  if n >= 0 then Array.append (Array.make n 0.) s else Array.sub s (min (-n) (Array.length s)) (max 0 (Array.length s + n))

(* an .mpg: its first video stream, and its first audio stream moved so
 * that its time 0 is the first picture's (their first timestamps) --
 * the player shows the frame at the sound's position *)
let mpg (bytes : string) : (media, string) result =
  let streams = Mpeg_system.of_string bytes in
  match Mpeg_system.video streams with
  | None -> Error "no video stream"
  | Some video ->
      let sound =
        match Mpeg_system.audio streams with
        | None -> None
        | Some audio -> (
            match mpeg_sound audio.bytes with
            | Error _ -> None
            | Ok s ->
                let offset = match (audio.first_pts, video.first_pts) with Some a, Some v -> a -. v | _ -> 0. in
                Some { Signal.left = delayed offset s.left; right = delayed offset s.right })
      in
      Ok (mpeg1_movie video.bytes ~sound)

let open_ ~(name : string) (bytes : string) : (kind * media, string) result =
  match sniff ~name bytes with
  | None -> Error (name ^ ": not a kind of file this player knows")
  | Some kind -> (
      let media =
        match kind with
        | Wav -> Result.map (fun s -> Sound { samples = Signal.both s; notes = [] }) (Wav.of_string bytes)
        | Mp2 | Mp3 -> Result.map (fun samples -> Sound { samples; notes = [] }) (mpeg_sound bytes)
        | Midi -> Result.map (fun (score : Midi.score) -> Sound { samples = Signal.both (Music.render_score score); notes = score.notes }) (Midi.parse bytes)
        | Mod -> Result.map (fun song -> Module song) (Mod.of_string bytes)
        | Abc -> Result.map tune (Abc.parse bytes)
        | Solfege -> Result.map tune (Doremi.parse bytes)
        (* the decoders raise on a broken file: caught below *)
        | Png -> Ok (Picture (Png.decode bytes))
        | Jpeg -> Ok (Picture (Jpeg.decode bytes))
        | Gif -> Ok (match Gif.animation bytes with [ (image, _) ] -> Picture image | frames -> Movie { movie = gif_movie frames; sound = None; mpeg = None })
        | Xpm -> Ok (Picture (xpm_picture (Xpm.parse bytes)))
        | Y4m -> Ok (Movie { movie = snd (Y4m.of_string bytes); sound = None; mpeg = None })
        | Flic -> Ok (Movie { movie = snd (Fli.of_string bytes); sound = None; mpeg = None })
        | Avi ->
            let _, movie, sound = Avi.of_string bytes in
            Ok (Movie { movie; sound = Option.map Signal.both sound; mpeg = None })
        | Mpeg1 -> Ok (mpeg1_movie bytes ~sound:None)
        | Mpg -> mpg bytes
      in
      match media with Ok m -> Ok (kind, m) | Error e -> Error (name ^ ": " ^ e) | exception e -> Error (name ^ ": " ^ Printexc.to_string e))

let duration (m : media) : float option =
  match m with
  | Sound s -> Some (float_of_int (Array.length s.samples.left) /. float_of_int Signal.rate)
  | Movie { movie; _ } -> Some movie.duration
  | Module _ | Picture _ -> None
