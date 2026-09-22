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
(* claude: a split screen's views (Playground3d.split3d) are projected
 * each on a screen the size of its rectangle, then moved there; but SVG
 * polygons can't be clipped to it by the playground, so each polygon
 * is cut to the rectangle here (Sutherland and Hodgman's clipping,
 * 1974: the polygon cut by each of the four edges in turn, the way
 * TinyXpilot.ml cuts its walls to its halves). The HUD shapes are
 * left whole. *)
let clip_polygon (s : Playground.screen) (points : (float * float) list) : (float * float) list =
  let cut inside at points =
    match points with
    | [] -> []
    | _ ->
        let last = List.nth points (List.length points - 1) in
        let _, out =
          List.fold_left
            (fun (prev, acc) p ->
              let acc =
                match (inside prev, inside p) with
                | true, true -> p :: acc
                | true, false -> at prev p :: acc
                | false, true -> p :: at prev p :: acc
                | false, false -> acc
              in
              (p, acc))
            (last, []) points
        in
        List.rev out
  in
  (* where the segment from a to b crosses the line x = c (or y = c) *)
  let at_x c (ax, ay) (bx, by) = (c, ay +. ((by -. ay) *. (c -. ax) /. (bx -. ax))) in
  let at_y c (ax, ay) (bx, by) = (ax +. ((bx -. ax) *. (c -. ay) /. (by -. ay)), c) in
  points
  |> cut (fun (x, _) -> x >= s.left) (at_x s.left)
  |> cut (fun (x, _) -> x <= s.right) (at_x s.right)
  |> cut (fun (_, y) -> y >= s.bottom) (at_y s.bottom)
  |> cut (fun (_, y) -> y <= s.top) (at_y s.top)

let rec clip (s : Playground.screen) (shape : Playground.shape) : Playground.shape =
  match shape.form with
  | Polygon (color, points) when shape.x = 0. && shape.y = 0. && shape.angle = 0. && shape.scale = 1. ->
      { shape with form = Polygon (color, clip_polygon s points) }
  | Group shapes -> { shape with form = Group (List.map (clip s) shapes) }
  | _ -> shape

let run_app3d ?(rendering = Playground3d.default_rendering) ?capture_mouse:_ ?flags
    (app3d : ('model, 'msg) Playground3d.app3d) : unit =
  let view2d (computer : Playground.computer) (model : 'model) : Playground.shape list =
    match Playground3d.views3d app3d computer model with
    | [ v ] when v.area = Playground3d.whole ->
        [ Playground3d.render3d_to_2d ~rendering v.camera computer.screen (Playground3d.group3d v.shapes) ]
    | views ->
        List.map
          (fun (v : Playground3d.view) ->
            let s = Playground3d.area_screen computer.screen v.area in
            let dx, dy = Playground3d.area_offset computer.screen v.area in
            Playground3d.render3d_to_2d ~rendering v.camera s (Playground3d.group3d v.shapes) |> clip s |> Playground.move dx dy)
          views
  in
  let update2d (computer : Playground.computer) (model : 'model) : 'model =
    Playground3d.update3d app3d computer model
  in
  let initial = Playground3d.init3d app3d () in
  let app2d = Playground.game view2d update2d initial in
  Playground_platform.run_app ?flags app2d

(* claude: no-op -- this backend doesn't load textures at all yet (see
 * Playground3d.textured_quad's doc comment: it renders a flat
 * placeholder color instead), so there's nothing to warm up. *)
let preload_texture (_src : string) : unit = ()
