(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

type ('context, 'action) option_ = { action : 'action; label : string; score : 'context -> float }

let scores (options : ('context, 'action) option_ list) (c : 'context) : ('action * float) list =
  List.map (fun o -> (o.action, o.score c)) options

let choose ?current ?(inertia = 0.) (options : ('context, 'action) option_ list) (c : 'context) : 'action option =
  let bonus a = if Some a = current then inertia else 0. in
  List.fold_left
    (fun best (a, s) ->
      let s = s +. bonus a in
      match best with Some (_, s') when s' >= s -> best | _ -> Some (a, s))
    None (scores options c)
  |> Option.map fst

let clamp01 (x : float) : float = Float.max 0. (Float.min 1. x)
let linear ~(lo : float) ~(hi : float) (x : float) : float = clamp01 ((x -. lo) /. (hi -. lo))
