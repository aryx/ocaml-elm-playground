(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Golden_frames.mli *)

let tests =
  Testutil_golden.tests ~dir:"tests/2d" ~approve:"approve-golden2d" ~scripted:Scenes_2d.scripted ~flagged:Scenes_2d.flagged
    ~scripted_flagged:Scenes_2d.scripted_flagged Scenes_2d.scenes
