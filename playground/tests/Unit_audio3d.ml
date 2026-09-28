(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* Audio3d: the ears agree with the eyes -- a point Playground3d.project
 * puts on the screen's right is panned right, for cameras looking every
 * way; and one straight ahead or right behind in the middle *)

let t = Testo.create

let test_right_is_right () =
  let screen = Playground.initial_computer.screen in
  List.iter
    (fun (eye, target) ->
      let camera = Playground3d.camera ~eye ~target () in
      let ears = Audio3d.listener camera in
      let (ex, ey, ez) = eye and (tx, ty, tz) = target in
      (* points around the target, a little off the line of sight *)
      List.iter
        (fun (dx, dy, dz) ->
          let p = (tx +. dx, ty +. dy, tz +. dz) in
          match Playground3d.project camera screen p with
          | Some (x, _) when Float.abs x > 20. ->
              let pan = Audio3d.pan ears p in
              if (x > 0.) <> (pan > 0.) then
                Alcotest.failf "eye (%g, %g, %g): a point drawn at x = %.0f panned %.2f" ex ey ez x pan
          | _ -> ())
        [ (3., 0., 0.); (-3., 0., 0.); (0., 0., 3.); (0., 0., -3.); (2., 1., -2.); (-2., -1., 2.) ];
      Alcotest.(check (float 1e-9)) "straight ahead: the middle" 0. (Audio3d.pan ears target))
    [
      ((0., 2., 10.), (0., 0., 0.));
      ((10., 2., 0.), (0., 0., 0.));
      ((0., 2., -10.), (0., 0., 0.));
      ((-7., 5., -7.), (1., 0., 1.));
    ]

let tests = Testo.categorize "Audio3d" [ t "a point drawn on the right is heard on the right" test_right_is_right ]
