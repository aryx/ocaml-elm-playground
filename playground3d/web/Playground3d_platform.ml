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

(* Web backend of the 3D playground: unlike the native backend (a real
 * software rasterizer), this one does no rendering work of its own at
 * all. It compiles the 3D scene down to an ordinary Playground.shape
 * (via Playground3d.render3d_to_2d: backface culling + a painter's
 * algorithm depth sort, then a plain 2D projection) every frame, builds
 * a standard Playground.game out of that, and hands it straight to
 * elm_playground_web's existing, unmodified run_app -- so this backend
 * gets SVG rendering, the event loop, and the requestAnimationFrame
 * timing for free. Same trick lucamug's elm-playground-3d uses. *)

(* claude: [capture_mouse] ignored here: this SVG backend is for small
 * scenes, not first-person games (see the webgl backend for one that
 * captures); mdx/mdy still work, the mouse just isn't captured *)
let run_app3d ?(rendering = Playground3d.default_rendering) ?capture_mouse:_
    (app3d : ('model, 'msg) Playground3d.app3d) : unit =
  let view2d (computer : Playground.computer) (model : 'model) : Playground.shape list =
    let (cam, shapes) = Playground3d.view3d app3d computer model in
    [ Playground3d.render3d_to_2d ~rendering cam computer.screen (Playground3d.group3d shapes) ]
  in
  let update2d (computer : Playground.computer) (model : 'model) : 'model =
    Playground3d.update3d app3d computer model
  in
  let initial = Playground3d.init3d app3d () in
  let app2d = Playground.game view2d update2d initial in
  Playground_platform.run_app app2d

(* claude: no-op -- this backend doesn't load textures at all yet (see
 * Playground3d.textured_quad's doc comment: it renders a flat
 * placeholder color instead), so there's nothing to warm up. *)
let preload_texture (_src : string) : unit = ()
