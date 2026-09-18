(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

let t = Testo.create

(* floating point: cos (pi/2) is 6e-17, not 0, so compare with a
 * tolerance *)
let point = Alcotest.(pair (float 1e-9) (float 1e-9))

let pi = Float.pi

let test_rotate () =
  Alcotest.check point "quarter turn of (1, 0)" (0., 1.) (Affine.apply (Affine.rotate (pi /. 2.)) (1., 0.));
  Alcotest.check point "half turn of (1, 2)" (-1., -2.) (Affine.apply (Affine.rotate pi) (1., 2.))

(* the example in Affine.mli: the order of composition matters *)
let test_compose_order () =
  let move = Affine.translate 10. 0. and turn = Affine.rotate (pi /. 2.) in
  Alcotest.check point "turn, then move" (10., 1.) (Affine.apply (Affine.compose move turn) (1., 0.));
  Alcotest.check point "move, then turn" (0., 11.) (Affine.apply (Affine.compose turn move) (1., 0.))

(* the example in Shape_render_software.screen_transform: Elm's
 * coordinates (origin at the center, y up) to the pixels of a
 * 1000x1000 window (origin at the top-left, y down) *)
let test_elm_to_pixels () =
  let screen = Affine.compose (Affine.translate 500. 500.) (Affine.scale 1. (-1.)) in
  Alcotest.check point "center" (500., 500.) (Affine.apply screen (0., 0.));
  Alcotest.check point "above center" (500., 400.) (Affine.apply screen (0., 100.));
  Alcotest.check point "top-left corner" (0., 0.) (Affine.apply screen (-500., 500.))

(* the example in Affine.mli: the inverse of "rotate, then move" is
 * "move back, then rotate back" *)
let test_invert () =
  let m = Affine.compose (Affine.translate 10. 0.) (Affine.rotate (pi /. 2.)) in
  let undo = Affine.compose (Affine.rotate (-.pi /. 2.)) (Affine.translate (-10.) 0.) in
  let p = (3., 4.) in
  Alcotest.check point "undoes m" p (Affine.apply (Affine.invert m) (Affine.apply m p));
  Alcotest.check point "= move back, rotate back" (Affine.apply undo p) (Affine.apply (Affine.invert m) p)

let tests =
  Testo.categorize "Affine"
    [
      t "rotate" test_rotate;
      t "compose order" test_compose_order;
      t "Elm coordinates to pixels" test_elm_to_pixels;
      t "invert" test_invert;
    ]
