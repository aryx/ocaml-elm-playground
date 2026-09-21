(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Frame_data.mli *)

type move = { startup : int; active : int; recovery : int; damage : int; hitstun : int; blockstun : int; hitbox : Hitbox.box }
type phase = Startup | Active | Recovery | Over

let length (m : move) : int = m.startup + m.active + m.recovery

let phase (m : move) (frame : int) : phase =
  if frame <= m.startup then Startup else if frame <= m.startup + m.active then Active else if frame <= length m then Recovery else Over

let advantage_on_hit (m : move) : int = m.hitstun - (m.active - 1 + m.recovery)
let advantage_on_block (m : move) : int = m.blockstun - (m.active - 1 + m.recovery)
