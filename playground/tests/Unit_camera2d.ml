(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* Camera2d *)

let t = Testo.create

let point = Alcotest.(pair (float 1e-9) (float 1e-9))

let rect =
  Alcotest.testable
    (fun ppf (r : Camera2d.rect) ->
      Format.fprintf ppf "{left %g; right %g; bottom %g; top %g}" r.left r.right r.bottom r.top)
    ( = )

let cam : Camera2d.t = { x = 100.; y = 0.; zoom = 2.; angle = 0. }

let test_to_screen_world () =
  Alcotest.check point "to_screen (110, 5)" (20., 10.) (Camera2d.to_screen cam 110. 5.);
  Alcotest.check point "the camera's point at the center" (0., 0.) (Camera2d.to_screen cam 100. 0.);
  Alcotest.check point "to_world (20, 10)" (110., 5.) (Camera2d.to_world cam 20. 10.)

(* view is a group scaled then moved: the renderers apply a shape's
 * transform as translate, then scale, so a child at p ends at
 * zoom * p + (x, y) *)
let test_view () =
  let shape = Camera2d.view cam [] in
  Alcotest.(check (float 1e-9)) "scale" 2. shape.scale;
  Alcotest.check point "moved by -zoom * cam" (-200., 0.) (shape.x, shape.y)

let test_visible () =
  Alcotest.check rect "1000x800 screen"
    { left = -150.; right = 350.; bottom = -200.; top = 200. }
    (Camera2d.visible (Playground.to_screen 1000. 800.) cam)

let test_follow () =
  let xs =
    List.fold_left
      (fun (cam, xs) () ->
        let cam = Camera2d.follow 0.1 100. 0. cam in
        (cam, cam.Camera2d.x :: xs))
      (Camera2d.origin, []) [ (); (); () ]
    |> snd |> List.rev
  in
  Alcotest.(check (list (float 1e-9))) "10, 19, 27.1" [ 10.; 19.; 27.1 ] xs

let test_window () =
  let x px = (Camera2d.window 200. 200. px 0. Camera2d.origin).x in
  Alcotest.(check (float 1e-9)) "80: in the window" 0. (x 80.);
  Alcotest.(check (float 1e-9)) "150: pushed" 50. (x 150.);
  Alcotest.(check (float 1e-9)) "-150: pushed left" (-50.) (x (-150.))

let test_clamp () =
  let screen = Playground.to_screen 1000. 1000. in
  let level : Camera2d.rect = { left = 0.; right = 3000.; bottom = -250.; top = 250. } in
  let clamped x = Camera2d.clamp screen level { Camera2d.origin with x } in
  Alcotest.(check (float 1e-9)) "left edge" 500. (clamped 0.).x;
  Alcotest.(check (float 1e-9)) "inside" 1200. (clamped 1200.).x;
  Alcotest.(check (float 1e-9)) "right edge" 2500. (clamped 2900.).x;
  Alcotest.(check (float 1e-9)) "smaller than the screen: centered" 0. (clamped 0.).y

let test_parallax () =
  let c = Camera2d.parallax 0.5 { Camera2d.origin with x = 100.; y = 40. } in
  Alcotest.check point "half" (50., 20.) (c.x, c.y)

(* Camera2d.mli's examples with an angle *)
let test_turned () =
  let turned = { cam with angle = 90. } in
  Alcotest.check point "to_screen (110, 5), turned by 90" (10., -20.) (Camera2d.to_screen turned 110. 5.);
  Alcotest.check point "and back" (110., 5.) (Camera2d.to_world turned 10. (-20.));
  let shape = Camera2d.view turned [] in
  Alcotest.(check (float 1e-9)) "the group turned the other way" (-90.) shape.angle;
  (* the view's group sends the point (110, 5) where to_screen says:
   * scaled by 2, turned by -90, then moved *)
  let a = shape.angle *. Float.pi /. 180. in
  let x, y = (220. *. cos a -. (10. *. sin a) +. shape.x, (220. *. sin a) +. (10. *. cos a) +. shape.y) in
  Alcotest.check point "view agrees with to_screen" (10., -20.) (x, y);
  Alcotest.(check (float 1e-9)) "turn_toward, the short way" 360. (Camera2d.turn_toward 0.5 10. { cam with angle = 350. }).angle

(* room's and flip's worked examples *)
let test_rooms () =
  let bounds : Camera2d.rect = { left = -1000.; right = 1000.; bottom = -400.; top = 400. } in
  Alcotest.(check (pair int int)) "left" (0, 0) (Camera2d.room bounds (1000., 800.) (-500.) 0.);
  Alcotest.(check (pair int int)) "right" (1, 0) (Camera2d.room bounds (1000., 800.) 500. 0.);
  let c = Camera2d.flip bounds (1000., 800.) 700. (-100.) Camera2d.origin in
  Alcotest.check point "at the room's center" (500., 0.) (c.x, c.y)

let tests =
  Testo.categorize "Camera2d"
    [ t "turned" test_turned;
      t "to_screen and to_world" test_to_screen_world;
      t "view" test_view;
      t "visible" test_visible;
      t "follow" test_follow;
      t "window" test_window;
      t "clamp" test_clamp;
      t "parallax" test_parallax;
      t "rooms" test_rooms;
    ]
