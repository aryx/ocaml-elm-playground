(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* Draws a list of Playground.shape values (circle/oval/rectangle/
 * ngon/polygon/words/image/group) onto a Cairo context, using the
 * same Elm-style coordinate convention (origin at the center, y up)
 * the rest of this project's 2D rendering uses. [cr]'s current
 * transform IS the origin -- the caller is responsible for having
 * already translated it to wherever (0, 0) should be (see
 * playground/native/Playground_platform.ml's run_app, which
 * translates to the window's center every frame before calling this).
 *
 * Extracted out of Playground_platform.ml (see
 * docs/claude_notes/plan_hud.md) so it's usable from a second,
 * independent caller -- the 3D software backend's HUD overlay pass --
 * without needing to call back into elm_playground_native itself,
 * which isn't possible: a virtual module's implementation is sealed
 * to exactly its virtual .mli's signature, so
 * Playground_platform.run_app's implementation can't expose any extra
 * public function beyond run_app, no matter what its own .ml defines. *)
val render : ?smooth_images:bool -> Cairo.context -> Playground.shape list -> unit
(* claude: [smooth_images] (default true): Playground.rendering's, see
 * there *)
