(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Music.mli *)

let midi_frequency (n : int) : float = 440. *. (2. ** (float_of_int (n - 69) /. 12.))

let midi_number (name : string) : int option =
  let letter = function 'C' -> Some 0 | 'D' -> Some 2 | 'E' -> Some 4 | 'F' -> Some 5 | 'G' -> Some 7 | 'A' -> Some 9 | 'B' -> Some 11 | _ -> None in
  let n = String.length name in
  if n < 2 then None
  else
    match letter (Char.uppercase_ascii name.[0]) with
    | None -> None
    | Some semitone -> (
        let (accidental, rest) =
          match name.[1] with '#' -> (1, String.sub name 2 (n - 2)) | 'b' -> (-1, String.sub name 2 (n - 2)) | _ -> (0, String.sub name 1 (n - 1))
        in
        match int_of_string_opt rest with
        | Some octave when octave >= -1 && octave <= 9 -> Some (((octave + 1) * 12) + semitone + accidental)
        | _ -> None)

let frequency (name : string) : float = match midi_number name with Some n -> midi_frequency n | None -> 0.

let instrument ~(voice : int) ~(voices : int) : Oscillator.waveform * float =
  if voices > 1 && voice = voices - 1 then (Triangle, 0.5) else if voice = 0 then (Square, 0.3) else (Square, 0.18)

let to_sound (tune : Abc.tune) : Synth.t =
  let voices = List.length tune.voices in
  let silence seconds = Synth.voice (Wave Sine) 0. |> Synth.louder 0. |> Synth.lasting seconds in
  Synth.Together
    (List.mapi
       (fun i events ->
         let (waveform, volume) = instrument ~voice:i ~voices in
         Synth.After
           (List.map
              (fun (e : Abc.event) ->
                match e.notes with
                | [] -> silence e.length
                | notes ->
                    let sounding = e.length *. 0.9 in
                    Synth.After
                      [ Synth.Together
                          (List.map
                             (fun n -> Synth.voice (Wave waveform) (midi_frequency n) |> Synth.lasting sounding |> Synth.louder (volume /. 0.5))
                             notes);
                        silence (e.length -. sounding) ])
              events))
       tune.voices)
