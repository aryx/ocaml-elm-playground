(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)

(* The 3D analog of Playground_platform.run_app. A 3D app owns its own
 * window/event loop (it is not a shape embedded in a 2D scene), so it
 * gets its own virtual function rather than reusing Playground_platform.
 * The native implementation is a real from-scratch software rasterizer
 * on top of raw SDL; the web implementation compiles the 3D scene down
 * to ordinary Playground.shape values every frame and delegates to the
 * existing, unmodified elm_playground_web backend. *)
val run_app3d :
  ?rendering:Playground3d.rendering ->
  ?capture_mouse:bool ->
  ?flags:Playground.flags ->
  ('a, 'b) Playground3d.app3d ->
  unit
(* claude: [rendering] (default: Playground3d.default_rendering) sets how
 * to draw, see Playground3d.rendering.
 *
 * [flags] (default: none): the app's computer.flags, as for 2D's
 * Playground_platform.run_app, and read the same way, with 2D's
 * Playground_platform.flags (a 3D program is linked with a 2D backend
 * too):
 *   let main = Playground3d_platform.run_app3d ~flags:(Playground_platform.flags ()) app
 *
 * [capture_mouse] (default false), for first-person games: the mouse
 * is captured, i.e. hidden and not stopped by the window's edges, so
 * that the app turns its camera by Playground.mouse's mdx/mdy (how far
 * the mouse moved), without limit, as in most 3D games. Escape gives
 * the mouse back; a click in the window captures it again (and is only
 * that, not a click for the app). On the web (WebGL), the page must be
 * clicked first (browsers only capture after a click: the Pointer Lock
 * API); the SVG web backend ignores it. *)

(* Load (and cache) a texture src ahead of time, e.g. for all the
 * textures a game will need, so that a {!Playground3d.textured_quad}/
 * {!Playground3d.textured_cube} never has to load one lazily mid-game
 * -- mirrors {!Playground_platform.preload_image} for the 2D backend.
 * On the native backend this blocks until the texture is downloaded
 * (if given as an http(s) URL) and decoded, which is fine to do once
 * up front but would freeze the render loop if done lazily on first
 * use. The web backend doesn't actually load textures at all yet (see
 * {!Playground3d.textured_quad}'s doc comment -- it renders a flat
 * placeholder color there), so this is a no-op there for now. *)
val preload_texture : string -> unit
