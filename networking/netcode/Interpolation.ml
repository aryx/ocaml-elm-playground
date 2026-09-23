(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Interpolation.mli *)

type 'a t = { delay : float; mutable snapshots : (float * 'a) list (* the oldest first *) }

let create ~(delay : float) : 'a t = { delay; snapshots = [] }

(* kept: the ones not older than a second before the newest *)
let add (t : 'a t) ~(time : float) (x : 'a) : unit =
  t.snapshots <- List.filter (fun (at, _) -> at >= time -. 1.) (t.snapshots @ [ (time, x) ])

let sample (t : 'a t) ~(now : float) : ('a * 'a * float) option =
  let at = now -. t.delay in
  let rec around = function
    | [] -> None
    | [ (_, x) ] -> Some (x, x, 0.)
    | (t0, x0) :: ((t1, x1) :: _ as rest) ->
        if at < t0 then Some (x0, x0, 0.)
        else if at <= t1 then Some (x0, x1, if t1 > t0 then (at -. t0) /. (t1 -. t0) else 0.)
        else around rest
  in
  around t.snapshots
