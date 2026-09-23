(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Tween.mli *)

let lerp (a : float) (b : float) (p : float) : float = a +. ((b -. a) *. p)

let progress ~(start : float) ~(duration : float) (now : float) : float =
  if duration <= 0. then 1. else Float.max 0. (Float.min 1. ((now -. start) /. duration))

let value (curve : Ease.t) (a : float) (b : float) ~(start : float) ~(duration : float) (now : float) : float =
  lerp a b (curve (progress ~start ~duration now))

let finished ~(start : float) ~(duration : float) (now : float) : bool = progress ~start ~duration now >= 1.
