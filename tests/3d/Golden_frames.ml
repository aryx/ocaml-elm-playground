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
  Testutil_golden.tests ~dir:"tests/3d" ~approve:"approve-golden3d" ~scripted:Scenes_3d.scripted
    ~scripted_flagged:Scenes_3d.scripted_flagged Scenes_3d.scenes
