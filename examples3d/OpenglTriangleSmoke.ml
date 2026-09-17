(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* Phase 2 smoke test for playground3d's new OpenGL backend (see
 * docs/claude_notes/plan_opengl.md): proves the tsdl (window/GL
 * context) + tgls (OpenGL calls) + GLSL (shader compile/link)
 * pipeline works end to end. That backend's run_app3d doesn't look at
 * shapes/camera at all yet (Phase 3 wires in the real thing) -- it
 * always draws the same hardcoded triangle, so this view is
 * intentionally a stub; whatever it returned, the picture on screen
 * would be identical. *)
open Playground3d

let view (_computer : Playground.computer) () =
  let cam = camera ~eye:(0., 0., 3.) ~target:(0., 0., 0.) () in
  (cam, [])

let update _computer () = ()

let app = game3d view update ()
let main = Playground3d_platform.run_app3d app
