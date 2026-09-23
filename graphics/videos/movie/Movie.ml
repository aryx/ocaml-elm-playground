(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Movie.mli *)

type t = {
  width : int;
  height : int;
  times : float array;
  duration : float;
  frame : int -> Rgba_image.t;
}

let frame_count (m : t) : int = Array.length m.times

let of_frames (frames : (Rgba_image.t * float) list) : t =
  let pictures = Array.of_list (List.map fst frames) in
  if pictures = [||] then invalid_arg "Movie.of_frames: no frames";
  let times = Array.make (Array.length pictures) 0. in
  let t = ref 0. in
  List.iteri
    (fun i (_, d) ->
      times.(i) <- !t;
      t := !t +. d)
    frames;
  { width = pictures.(0).width; height = pictures.(0).height; times; duration = !t; frame = (fun i -> pictures.(i)) }

let sequential ~width ~height ~(times : float array) ~(duration : float) ~(start : unit -> 's) ~(next : 's -> 's * Rgba_image.t) : t =
  (* the decoder where it last stopped: its state, the frame it gave,
   * and the frame before that one *)
  let at : ('s * int * Rgba_image.t * Rgba_image.t option) option ref = ref None in
  let rec frame i =
    match !at with
    | Some (_, j, img, _) when j = i -> img
    | Some (_, j, _, Some before) when j = i + 1 -> before
    | Some (s, j, img, _) when j < i ->
        let s, next_img = next s in
        at := Some (s, j + 1, next_img, Some img);
        frame i
    | _ ->
        (* nothing yet, or too far behind: from the start *)
        let s, img = next (start ()) in
        at := Some (s, 0, img, None);
        frame i
  in
  { width; height; times; duration; frame }

(* the last frame starting at or before [t], by bisection *)
let index_at (m : t) (t : float) : int =
  let lo = ref 0 and hi = ref (Array.length m.times - 1) in
  while !lo < !hi do
    let mid = (!lo + !hi + 1) / 2 in
    if m.times.(mid) <= t then lo := mid else hi := mid - 1
  done;
  !lo

let frame_at (m : t) (t : float) : Rgba_image.t = m.frame (index_at m t)
