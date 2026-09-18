(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* graphics/3d/Render *)

let t = Testo.create

(* a 2x2 square around the origin, counterclockwise seen from +z *)
let square : Render.face =
  let normal = (0., 0., 1.) in
  {
    paint = Color 0xFF0000;
    points = List.map (fun p -> (p, (0., 0.), normal)) [ (-1., -1., 0.); (1., -1., 0.); (1., 1., 0.); (-1., 1., 0.) ];
  }

(* the pixel at the center of a 20x20 frame, with the camera on +z or -z *)
let center_pixel ?(options = Render.default_options) ~eye_z () =
  let fb = Framebuffer.create ~width:20 ~height:20 in
  let zbuffer = Zbuffer.create ~width:20 ~height:20 in
  let camera : Camera.t = { eye = (0., 0., eye_z); target = (0., 0., 0.); fov = 90.; near = 0.1; far = 100. } in
  Render.render ~options:{ options with shading = Shading.Flat_color } fb zbuffer camera [ square ];
  Framebuffer.get_rgb fb ~x:10 ~y:10

let test_front_back () =
  Alcotest.(check int) "seen from the front: red" 0xFF0000 (center_pixel ~eye_z:5. ());
  Alcotest.(check int) "from behind: culled, white" 0xFFFFFF (center_pixel ~eye_z:(-5.) ());
  Alcotest.(check int) "from behind, without culling: red" 0xFF0000
    (center_pixel ~options:{ Render.default_options with backface_culling = false } ~eye_z:(-5.) ())

let test_behind_camera () =
  Alcotest.(check int) "the camera looking away: nothing" 0xFFFFFF
    (let fb = Framebuffer.create ~width:20 ~height:20 in
     let zbuffer = Zbuffer.create ~width:20 ~height:20 in
     let camera : Camera.t = { eye = (0., 0., 5.); target = (0., 0., 10.); fov = 90.; near = 0.1; far = 100. } in
     Render.render ~options:{ Render.default_options with backface_culling = false } fb zbuffer camera [ square ];
     Framebuffer.get_rgb fb ~x:10 ~y:10)

let tests =
  Testo.categorize "Render"
    [ t "a face, from the front and from behind" test_front_back; t "behind the camera" test_behind_camera ]
