(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Abc.mli *)

type event = { start : float; length : float; notes : int list }
type tune = { title : string; voices : event list list }

(*****************************************************************************)
(* Keys *)
(*****************************************************************************)

(* the order sharps and flats are added in a key signature *)
let sharps_order = "FCGDAEB"
let flats_order = "BEADGCF"

(* how many sharps (> 0) or flats (< 0) a key has; None if unknown *)
let key_count (key : string) : int option =
  let majors = [ ("C", 0); ("G", 1); ("D", 2); ("A", 3); ("E", 4); ("B", 5); ("F#", 6); ("C#", 7);
                 ("F", -1); ("Bb", -2); ("Eb", -3); ("Ab", -4); ("Db", -5); ("Gb", -6); ("Cb", -7) ] in
  let minors = [ ("A", 0); ("E", 1); ("B", 2); ("F#", 3); ("C#", 4); ("G#", 5); ("D#", 6); ("A#", 7);
                 ("D", -1); ("G", -2); ("C", -3); ("F", -4); ("Bb", -5); ("Eb", -6); ("Ab", -7) ] in
  let key = String.trim key in
  let first = match String.index_opt key ' ' with Some i -> String.sub key 0 i | None -> key in
  let strip suffix s =
    let n = String.length s and k = String.length suffix in
    if n > k && String.lowercase_ascii (String.sub s (n - k) k) = suffix then Some (String.sub s 0 (n - k)) else None
  in
  match (strip "min" first, strip "m" first, strip "maj" first) with
  | Some tonic, _, _ | None, Some tonic, _ -> List.assoc_opt tonic minors
  | None, None, Some tonic -> List.assoc_opt tonic majors
  | None, None, None -> List.assoc_opt first majors

(* each letter's offset in the key: +1 for its sharps, -1 its flats *)
let key_offsets (count : int) : (char * int) list =
  if count >= 0 then List.init count (fun i -> (sharps_order.[i], 1))
  else List.init (-count) (fun i -> (flats_order.[i], -1))

(*****************************************************************************)
(* The parser *)
(*****************************************************************************)

let semitone = function 'C' -> 0 | 'D' -> 2 | 'E' -> 4 | 'F' -> 5 | 'G' -> 7 | 'A' -> 9 | _ -> 11

(* a fraction "a/b", or "a" *)
let fraction (s : string) : float option =
  match String.split_on_char '/' (String.trim s) with
  | [ a ] -> float_of_string_opt a
  | [ a; b ] -> (
      match (float_of_string_opt a, float_of_string_opt b) with Some a, Some b when b <> 0. -> Some (a /. b) | _ -> None)
  | _ -> None

(* a voice being written: its time, its events (newest first) *)
type voice = { mutable time : float; mutable events : event list }

type state = {
  mutable title : string;
  mutable unit_length : float; (* L, a fraction of a whole note *)
  mutable whole : float; (* seconds a whole note lasts, from Q *)
  mutable key : (char * int) list;
  (* the accidentals written in this bar: (letter, octave) -> offset *)
  bar : (char * int, int) Hashtbl.t;
  voices : (string, voice) Hashtbl.t;
  mutable order : string list; (* the voices, in the order they appeared *)
  mutable current : string;
  (* a triplet's notes left, and the factor they're played at *)
  mutable tuplet : int;
  (* a dotted pair's factor for the next note *)
  mutable next_factor : float;
}

let voice (st : state) : voice =
  match Hashtbl.find_opt st.voices st.current with
  | Some v -> v
  | None ->
      let v = { time = 0.; events = [] } in
      Hashtbl.replace st.voices st.current v;
      st.order <- st.order @ [ st.current ];
      v

let set_tempo (st : state) (q : string) : unit =
  (* "1/4=120": 120 quarters a minute; "120": the same *)
  match String.split_on_char '=' q with
  | [ beat; bpm ] -> (
      match (fraction beat, float_of_string_opt (String.trim bpm)) with
      | Some b, Some n when n > 0. -> st.whole <- 60. /. (n *. b)
      | _ -> ())
  | [ bpm ] -> ( match float_of_string_opt (String.trim bpm) with Some n when n > 0. -> st.whole <- 60. /. (n *. 0.25) | _ -> ())
  | _ -> ()

let field (st : state) (name : char) (value : string) : unit =
  let value = String.trim value in
  match name with
  | 'T' -> if st.title = "" then st.title <- value
  | 'L' -> Option.iter (fun l -> st.unit_length <- l) (fraction value)
  | 'Q' -> set_tempo st value
  | 'K' -> st.key <- key_offsets (Option.value (key_count value) ~default:0)
  | 'V' -> st.current <- (match String.index_opt value ' ' with Some i -> String.sub value 0 i | None -> value)
  | _ -> ()

(* an event of [units] unit lengths in the current voice *)
let emit (st : state) (notes : int list) (units : float) : unit =
  let v = voice st in
  let factor = st.next_factor *. if st.tuplet > 0 then 2. /. 3. else 1. in
  st.next_factor <- 1.;
  if st.tuplet > 0 then st.tuplet <- st.tuplet - 1;
  let length = units *. st.unit_length *. st.whole *. factor in
  v.events <- { start = v.time; length; notes } :: v.events;
  v.time <- v.time +. length

(* C>D: the last note one and a half times as long, the next half *)
let broken (st : state) (last : float) (next : float) : unit =
  let v = voice st in
  match v.events with
  | e :: rest ->
      let length = e.length *. last in
      v.time <- v.time +. (length -. e.length);
      v.events <- { e with length } :: rest;
      st.next_factor <- next
  | [] -> ()

let parse_music_line (st : state) (line : string) : unit =
  let n = String.length line in
  let peek i = if i < n then Some line.[i] else None in
  let digits i = let j = ref i in while !j < n && line.[!j] >= '0' && line.[!j] <= '9' do incr j done; !j in
  (* a length at [i]: 2, /2, /, 3/2, //; and where it ends *)
  let length i =
    let j = digits i in
    let num = if j > i then float_of_string (String.sub line i (j - i)) else 1. in
    if peek j = Some '/' then
      let k = digits (j + 1) in
      if k > j + 1 then (num /. float_of_string (String.sub line (j + 1) (k - j - 1)), k)
      else
        (* one / halves, // quarters... *)
        let s = ref j in
        while !s < n && line.[!s] = '/' do incr s done;
        (num /. (2. ** float_of_int (!s - j)), !s)
    else (num, j)
  in
  (* a note at [i]: its MIDI number, and where its length starts *)
  let note i =
    let rec accidentals i acc explicit =
      match peek i with
      | Some '^' -> accidentals (i + 1) (acc + 1) true
      | Some '_' -> accidentals (i + 1) (acc - 1) true
      | Some '=' -> accidentals (i + 1) 0 true
      | _ -> (i, acc, explicit)
    in
    let (i, acc, explicit) = accidentals i 0 false in
    match peek i with
    | Some c when (c >= 'A' && c <= 'G') || (c >= 'a' && c <= 'g') ->
        let letter = Char.uppercase_ascii c in
        let octave = ref (if c >= 'a' then 5 else 4) and j = ref (i + 1) in
        while !j < n && (line.[!j] = '\'' || line.[!j] = ',') do
          if line.[!j] = '\'' then incr octave else decr octave;
          incr j
        done;
        let offset =
          if explicit then (
            Hashtbl.replace st.bar (letter, !octave) acc;
            acc)
          else
            match Hashtbl.find_opt st.bar (letter, !octave) with
            | Some a -> a
            | None -> Option.value (List.assoc_opt letter st.key) ~default:0
        in
        Some (((!octave + 1) * 12) + semitone letter + offset, !j)
    | _ -> None
  in
  let skip_to i closing = match String.index_from_opt line i closing with Some j -> j + 1 | None -> n in
  let rec go i =
    if i >= n then ()
    else
      match line.[i] with
      | '%' -> ()
      | '|' ->
          Hashtbl.reset st.bar;
          go (i + 1)
      | '"' -> go (skip_to (i + 1) '"')
      | '!' -> go (skip_to (i + 1) '!')
      | '+' -> go (skip_to (i + 1) '+')
      | '{' -> go (skip_to (i + 1) '}')
      | '>' ->
          broken st 1.5 0.5;
          go (i + 1)
      | '<' ->
          broken st 0.5 1.5;
          go (i + 1)
      | '(' when i + 1 < n && line.[i + 1] = '3' ->
          st.tuplet <- 3;
          go (i + 2)
      | 'z' | 'x' | 'Z' ->
          let (units, j) = length (i + 1) in
          emit st [] units;
          go j
      | '[' when i + 2 < n && line.[i + 2] = ':' ->
          (* an inline field, [K:D] *)
          let j = skip_to (i + 1) ']' in
          field st line.[i + 1] (String.sub line (i + 3) (max 0 (j - i - 4)));
          go j
      | '[' when i + 1 < n && line.[i + 1] >= '0' && line.[i + 1] <= '9' -> go (digits (i + 1))
      | '[' ->
          (* a chord: its notes, the first one's length, times the one
           * after the bracket *)
          let rec notes i acc first =
            match peek i with
            | Some ']' -> (i + 1, List.rev acc, first)
            | None -> (n, List.rev acc, first)
            | _ -> (
                match note i with
                | Some (pitch, j) ->
                    let (units, k) = length j in
                    notes k (pitch :: acc) (match first with None -> Some units | f -> f)
                | None -> notes (i + 1) acc first)
          in
          let (j, pitches, first) = notes (i + 1) [] None in
          let (after, k) = length j in
          emit st pitches (Option.value first ~default:1. *. after);
          go k
      | _ -> (
          match note i with
          | Some (pitch, j) ->
              let (units, k) = length j in
              emit st [ pitch ] units;
              go k
          | None -> go (i + 1))
  in
  go 0

let is_field (line : string) : bool =
  String.length line >= 2 && line.[1] = ':' && ((line.[0] >= 'A' && line.[0] <= 'Z') || (line.[0] >= 'a' && line.[0] <= 'z'))

let parse (text : string) : (tune, string) result =
  let st =
    { title = ""; unit_length = 1. /. 8.; whole = 2.; key = []; bar = Hashtbl.create 8; voices = Hashtbl.create 4;
      order = []; current = "1"; tuplet = 0; next_factor = 1. }
  in
  let seen_x = ref false and stop = ref false in
  String.split_on_char '\n' text
  |> List.iter (fun line ->
         let line = String.trim line in
         if !stop || line = "" || line.[0] = '%' then ()
         else if is_field line then (
           if line.[0] = 'X' then if !seen_x then stop := true else seen_x := true;
           if not !stop then field st line.[0] (String.sub line 2 (String.length line - 2)))
         else parse_music_line st line);
  let voices = List.map (fun id -> List.rev (Hashtbl.find st.voices id).events) st.order in
  if List.for_all (( = ) []) voices then Error "no notes" else Ok { title = st.title; voices }

let duration (t : tune) : float =
  List.fold_left
    (fun m events -> List.fold_left (fun m (e : event) -> Float.max m (e.start +. e.length)) m events)
    0. t.voices
