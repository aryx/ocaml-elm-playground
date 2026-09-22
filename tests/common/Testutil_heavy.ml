(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Testutil_heavy.mli *)

let tag = Testo.Tag.declare "heavy"
let skip = Sys.getenv_opt "HEAVY" = Some "skip"
let skipped = "heavy: HEAVY=skip (make test-lite)"

let t ?(tags = []) name body =
  if skip then Testo.create ~tags:(tag :: tags) ~skipped name body
  else Testo.create ~tags:(tag :: tags) name body
