(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Squash.mli *)

let keep_area (k : float) : float * float = (1. /. k, k)

(* the overshoot of out_elastic is the stretch *)
let landing ~(amount : float) (p : float) : float = Tween.lerp (1. -. amount) 1. (Ease.out Ease.elastic p)
