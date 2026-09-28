(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

type note = { start : float; length : float; pitch : int; velocity : float }
type t = { bars : int; tracks : (int * note list) list }

let empty : t = { bars = 2; tracks = [] }
let length (t : t) : float = float_of_int (16 * t.bars)
let notes (t : t) (track : int) : note list = Option.value (List.assoc_opt track t.tracks) ~default:[]
let set_notes (t : t) (track : int) (ns : note list) : t = { t with tracks = (track, ns) :: List.remove_assoc track t.tracks }
let overlaps (a : note) (b : note) : bool = a.pitch = b.pitch && a.start < b.start +. b.length && b.start < a.start +. a.length
let add (t : t) (track : int) (n : note) : t = set_notes t track (n :: List.filter (fun m -> not (overlaps m n)) (notes t track))
let remove (t : t) (track : int) (n : note) : t = set_notes t track (List.filter (fun m -> m <> n) (notes t track))

let note_at (t : t) (track : int) (time : float) (pitch : int) : note option =
  List.find_opt (fun n -> n.pitch = pitch && n.start <= time && time < n.start +. n.length) (notes t track)

type event = On of int * float | Off of int

(* the loop unrolled: [from, until) may cross its end once, so the
 * notes are looked for in this turn and the next *)
let events (t : t) ~(from : float) ~(until : float) : (int * event) list =
  let len = length t in
  let within x = x >= from && x < until in
  let at_turn k =
    List.concat_map
      (fun (track, ns) ->
        List.concat_map
          (fun n ->
            let s = n.start +. k and e = Float.min (n.start +. n.length) len +. k in
            (if within s then [ (s, (track, On (n.pitch, n.velocity))) ] else []) @ if within e then [ (e, (track, Off n.pitch)) ] else [])
          ns)
      t.tracks
  in
  (* at the same time, a note's end before the next one's start *)
  let rank = function _, (_, Off _) -> 0 | _ -> 1 in
  List.stable_sort (fun a b -> compare (fst a, rank a) (fst b, rank b)) (at_turn 0. @ at_turn len) |> List.map snd
