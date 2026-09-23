(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Effect.mli *)

type knob = { name : string; control : Control.t; initial : float }
type t = {
  name : string;
  knobs : knob list;
  set : string -> float -> unit;
  process : Signal.stereo -> unit;
  meters : unit -> (string * float) list;
}

let ramp (last : float) (now : float) (i : int) (n : int) : float = last +. ((now -. last) *. float_of_int (i + 1) /. float_of_int n)
