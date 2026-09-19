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

(* a MIDI note's sound: its instrument, from its channel and program *)
let midi_voice (n : Midi.note) : Synth.t =
  let volume = 0.25 *. float_of_int n.velocity /. 127. in
  let v source f = Synth.Voice { source; frequency = f; slide = None; seconds = n.length; volume; fade = false } in
  let f = midi_frequency n.key in
  if n.channel = 9 then
    (* the drums: short, by key, whatever the note's length *)
    match n.key with
    | 35 | 36 -> v (Wave Triangle) 150. |> Synth.sliding 50. |> Synth.lasting 0.15 |> Synth.fading |> Synth.louder 2.
    | 38 | 40 -> v Noise 5000. |> Synth.lasting 0.15 |> Synth.fading
    | 42 | 44 | 46 -> v Noise 12000. |> Synth.lasting 0.05 |> Synth.fading |> Synth.louder 0.6
    | _ -> v Noise 3000. |> Synth.lasting 0.1 |> Synth.fading
  else
    match n.program / 8 with
    | 0 | 1 -> v (Wave Square) f |> Synth.fading |> Synth.louder 0.8
    | 3 -> v (Wave Sawtooth) f |> Synth.fading |> Synth.louder 0.7
    | 4 -> v (Wave Triangle) f |> Synth.louder 1.5
    | 5 | 6 -> v (Wave Sawtooth) f |> Synth.louder 0.5
    | 7 -> v (Wave Sawtooth) f |> Synth.louder 0.7
    | 11 -> v (Wave Triangle) f
    | _ -> v (Wave Square) f |> Synth.louder 0.7

let render_score (score : Midi.score) : Signal.t =
  (* a little after the last note, for the drums' tails *)
  let out = Array.make (Signal.samples (score.duration +. 0.2)) 0. in
  List.iter
    (fun (n : Midi.note) ->
      let at = Signal.samples n.start and s = Synth.render (midi_voice n) in
      Array.iteri (fun i x -> if at + i < Array.length out then out.(at + i) <- out.(at + i) +. x) s)
    score.notes;
  out
