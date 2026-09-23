(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Fm.mli *)

let wave ~(index : float) (carrier : float) (modulator : float) : float =
  sin ((2. *. Float.pi *. carrier) +. (index *. sin (2. *. Float.pi *. modulator)))

let render ~(carrier : float) ~(ratio : float) ~(index : float) (seconds : float) : Signal.t =
  let rate = float_of_int Signal.rate in
  Array.init (Signal.samples seconds) (fun i ->
      let t = float_of_int i /. rate in
      let phase f = let p = f *. t in p -. Float.floor p in
      wave ~index (phase carrier) (phase (carrier *. ratio)))
