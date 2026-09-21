(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* See Rhythm.mli *)

type judgement = Perfect | Great | Good | Almost | Miss

let window = function Perfect -> 0.030 | Great -> 0.060 | Good -> 0.100 | Almost -> 0.135 | Miss -> infinity

let judge (error : float) : judgement option =
  let e = Float.abs error in
  if e <= window Perfect then Some Perfect
  else if e <= window Great then Some Great
  else if e <= window Good then Some Good
  else if e <= window Almost then Some Almost
  else None

let points = function Perfect -> 100 | Great -> 70 | Good -> 40 | Almost -> 10 | Miss -> 0
let name = function Perfect -> "PERFECT" | Great -> "GREAT" | Good -> "GOOD" | Almost -> "ALMOST" | Miss -> "MISS"

let song_time ~(position : float) ~(offset : float) : float = position -. offset

type 'lane note = { at : float; lane : 'lane; length : float }

type 'lane performance = {
  judged : ('lane note * judgement option) list;
  errors : float list;
  combo : int;
  best_combo : int;
  score : int;
  last : (judgement * float) option;
  offset : float;
  now : float;
  started : float;
}

let start ~(offset : float) ~(started : float) (notes : 'lane note list) : 'lane performance =
  { judged = List.map (fun n -> (n, None)) notes; errors = []; combo = 0; best_combo = 0; score = 0; last = None;
    offset; now = 0.; started }

let press (now : float) (lane : 'lane) (p : 'lane performance) : 'lane performance =
  let candidates =
    List.filter_map (fun (n, j) -> if j = None && n.lane = lane then Some (n, now -. n.at) else None) p.judged
  in
  let nearest =
    List.fold_left
      (fun best (n, e) -> match best with Some (_, be) when Float.abs be <= Float.abs e -> best | _ -> Some (n, e))
      None candidates
  in
  match nearest with
  | Some (n, e) -> (
      match judge e with
      | Some j ->
          let combo = if j = Almost then 0 else p.combo + 1 in
          { p with
            judged = List.map (fun (n', j') -> if n' == n then (n', Some j) else (n', j')) p.judged;
            errors = e :: p.errors; combo; best_combo = max p.best_combo combo; score = p.score + points j;
            last = Some (j, now) }
      | None -> p)
  | None -> p

let misses (now : float) (p : 'lane performance) : 'lane performance =
  let late (n, j) = j = None && now -. n.at > window Almost in
  if not (List.exists late p.judged) then p
  else
    { p with judged = List.map (fun (n, j) -> if late (n, j) then (n, Some Miss) else (n, j)) p.judged; combo = 0;
             last = Some (Miss, now) }

let play (now : float) (pressed : 'lane list) (p : 'lane performance) : 'lane performance =
  misses now (List.fold_left (fun p lane -> press now lane p) { p with now } pressed)

let average_error (p : 'lane performance) : float option =
  match p.errors with [] -> None | l -> Some (List.fold_left ( +. ) 0. l /. float_of_int (List.length l))

let sounding (t : Abc.tune) (v : int) : (float * float * int list) list =
  match List.nth_opt t.voices v with
  | None -> []
  | Some events ->
      List.filter_map
        (fun (e : Abc.event) -> if e.notes = [] then None else Some (e.start, e.length, e.notes))
        events

let on_frets (t : Abc.tune) (v : int) : int note list =
  let notes = sounding t v in
  let pitches = List.concat_map (fun (_, _, ps) -> ps) notes in
  let lo = List.fold_left min max_int pitches and hi = List.fold_left max min_int pitches in
  let fret p = if hi = lo then 2 else min 4 ((p - lo) * 5 / (hi - lo + 1)) in
  List.concat_map
    (fun (at, length, ps) -> List.sort_uniq compare (List.map fret ps) |> List.map (fun lane -> { at; lane; length }))
    notes

type difficulty = Easy | Medium | Hard | Expert

let difficulties = [ Easy; Medium; Hard; Expert ]
let difficulty_name = function Easy -> "EASY" | Medium -> "MEDIUM" | Hard -> "HARD" | Expert -> "EXPERT"
let frets_at = function Easy -> 3 | Medium -> 4 | Hard | Expert -> 5
let chord_at = function Easy | Medium -> 1 | Hard -> 2 | Expert -> 5

(* the notes struck together, as groups, in time order *)
let chords (notes : 'lane note list) : 'lane note list list =
  List.fold_left
    (fun groups n -> match groups with (m :: _ as g) :: rest when m.at = n.at -> (n :: g) :: rest | _ -> [ n ] :: groups)
    [] notes
  |> List.rev_map List.rev

let reduce (level : difficulty) (chart : int note list) : int note list =
  let frets = frets_at level and keep = chord_at level in
  List.concat_map
    (fun chord ->
      let sorted = List.sort (fun a b -> compare a.lane b.lane) chord in
      let n = List.length sorted in
      let kept =
        if keep >= n then sorted
        else if keep = 1 then [ List.hd sorted ]
        else [ List.hd sorted; List.nth sorted (n - 1) ]
      in
      (* the frets folded together, and what then lands on one fret
       * struck once *)
      List.map (fun m -> { m with lane = m.lane * frets / 5 }) kept
      |> List.sort_uniq (fun a b -> compare a.lane b.lane))
    (chords (List.stable_sort (fun a b -> compare a.at b.at) chart))

let strummed ~(strum : bool) ~(held : 'lane list) : 'lane list = if strum then held else []

let sustain_min = 0.75

let sustaining (now : float) (held : 'lane list) (p : 'lane performance) : int =
  List.length
    (List.filter
       (fun (n, j) ->
         j <> None && j <> Some Miss && n.length >= sustain_min && now > n.at && now < n.at +. n.length
         && List.mem n.lane held)
       p.judged)
