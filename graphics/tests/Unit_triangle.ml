(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* graphics/3d/Triangle *)

let t = Testo.create

(* a vertex at pixel (x, y) and depth z *)
let vertex (x, y) z : Project.vertex =
  { vx = x; vy = y; z; u = 0.; v = 0.; inv_z = 1. /. z; u_over_z = 0.; v_over_z = 0.; normal = (0., 0., 1.) }

let fill fb ?zbuffer ~rgb z =
  Triangle.fill fb ~zbuffer ~interpolation:Interpolate.Perspective_correct ~shading:Shading.Flat_color
    ~color:(fun ~u:_ ~v:_ ~brightness:_ -> rgb)
    (vertex (0., 0.) z) (vertex (4., 0.) z) (vertex (0., 4.) z)

(* the framebuffer as rows of '#' (rgb) and '.' (anything else) *)
let picture (fb : Framebuffer.t) rgb =
  List.init fb.height (fun y ->
      String.init fb.width (fun x -> if Framebuffer.get_rgb fb ~x ~y = rgb then '#' else '.'))

(* Triangle.mli's example: (0, 0), (4, 0), (0, 4) covers the 10 pixels
 * with x + y <= 3 *)
let test_coverage () =
  let fb = Framebuffer.create ~width:4 ~height:4 in
  fill fb ~rgb:0xFF0000 1.;
  Alcotest.(check (list string)) "the covered pixels" [ "####"; "###."; "##.."; "#..." ] (picture fb 0xFF0000)

(* with a z-buffer, a farther triangle drawn after doesn't cover a
 * nearer one; without (the painter's algorithm), it does *)
let test_zbuffer () =
  let fb = Framebuffer.create ~width:4 ~height:4 in
  let zbuffer = Zbuffer.create ~width:4 ~height:4 in
  fill fb ~zbuffer ~rgb:0xFF0000 1.;
  fill fb ~zbuffer ~rgb:0x0000FF 2.;
  Alcotest.(check int) "z-buffer: still the nearer, red" 0xFF0000 (Framebuffer.get_rgb fb ~x:1 ~y:1);
  fill fb ~rgb:0x0000FF 2.;
  Alcotest.(check int) "no z-buffer: the last drawn, blue" 0x0000FF (Framebuffer.get_rgb fb ~x:1 ~y:1)

let tests =
  Testo.categorize "Triangle"
    [ t "the pixels of the worked example" test_coverage; t "with and without a z-buffer" test_zbuffer ]
