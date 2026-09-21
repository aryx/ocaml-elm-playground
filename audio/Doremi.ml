(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Doremi.mli *)

(* the syllables, and their semitones above do *)
let syllables = [ ("do", 0); ("ut", 0); ("re", 2); ("ré", 2); ("mi", 4); ("fa", 5); ("sol", 7); ("la", 9); ("si", 11) ]

(* "sol#5:1/2" -> the syllable, then the rest *)
let split_syllable (word : string) : (int * string) option =
  List.find_map
    (fun (name, semitone) ->
      let n = String.length name in
      if String.length word >= n && String.lowercase_ascii (String.sub word 0 n) = name then
        Some (semitone, String.sub word n (String.length word - n))
      else None)
    (* the longest names first: "sol" before "si"... and "ré" (3
     * bytes) before "re" *)
    (List.sort (fun (a, _) (b, _) -> compare (String.length b) (String.length a)) syllables)

let fraction (s : string) : float option =
  match String.split_on_char '/' s with
  | [ a ] -> float_of_string_opt a
  | [ a; b ] -> ( match (float_of_string_opt a, float_of_string_opt b) with Some a, Some b when b <> 0. -> Some (a /. b) | _ -> None)
  | _ -> None

type voice = { mutable time : float; mutable events : Abc.event list }

let parse (text : string) : (Abc.tune, string) result =
  let beat = ref 0.5 (* seconds, at tempo 120 *) and octave = ref 4 in
  let voices = ref [] and current = ref None in
  let voice () =
    match !current with
    | Some v -> v
    | None ->
        let v = { time = 0.; events = [] } in
        voices := !voices @ [ v ];
        current := Some v;
        v
  in
  let emit notes beats =
    let v = voice () in
    let length = beats *. !beat in
    v.events <- { Abc.start = v.time; length; notes } :: v.events;
    v.time <- v.time +. length
  in
  (* "re:1/2" -> the length's beats, and the word without it *)
  let with_length (word : string) : (string * float, string) result =
    match String.index_opt word ':' with
    | None -> Ok (word, 1.)
    | Some i -> (
        match fraction (String.sub word (i + 1) (String.length word - i - 1)) with
        | Some beats -> Ok (String.sub word 0 i, beats)
        | None -> Error ("not a length: " ^ word))
  in
  let error = ref None in
  let rec words = function
    | [] -> ()
    | ("voix" | "voice") :: rest ->
        current := None;
        words rest
    | "tempo" :: bpm :: rest ->
        (match float_of_string_opt bpm with Some n when n > 0. -> beat := 60. /. n | _ -> error := Some ("not a tempo: " ^ bpm));
        words rest
    | "|" :: rest -> words rest
    | word :: rest ->
        (match with_length word with
        | Error e -> error := Some e
        | Ok ("-", beats) -> emit [] beats
        | Ok (w, beats) -> (
            match split_syllable w with
            | None -> error := Some ("not a note: " ^ word)
            | Some (semitone, tail) ->
                (* then # or b, then an octave *)
                let (accidental, tail) =
                  if tail <> "" && tail.[0] = '#' then (1, String.sub tail 1 (String.length tail - 1))
                  else if tail <> "" && tail.[0] = 'b' then (-1, String.sub tail 1 (String.length tail - 1))
                  else (0, tail)
                in
                if tail <> "" then (
                  match int_of_string_opt tail with
                  | Some o when o >= -1 && o <= 9 -> octave := o
                  | _ -> error := Some ("not an octave: " ^ word));
                emit [ ((!octave + 1) * 12) + semitone + accidental ] beats));
        if !error = None then words rest
  in
  String.split_on_char '\n' text
  |> List.iter (fun line ->
         let line = match String.index_opt line '%' with Some i -> String.sub line 0 i | None -> line in
         if !error = None then
           words (List.filter (( <> ) "") (String.split_on_char ' ' (String.map (fun c -> if c = '\t' then ' ' else c) line))));
  match !error with
  | Some e -> Error e
  | None ->
      let voices = List.map (fun v -> List.rev v.events) !voices in
      if List.for_all (( = ) []) voices then Error "no notes"
      else Ok { Abc.title = ""; voices; drums = List.map (fun _ -> false) voices }
